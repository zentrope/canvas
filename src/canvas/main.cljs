(ns canvas.main
  (:require-macros
   [cljs.core.async.macros :refer [go-loop]])
  (:require
   [cljs.core.async :refer [chan <! put!]]))

(enable-console-print!)

;;-----------------------------------------------------------------------------
;; Constants
;;-----------------------------------------------------------------------------

(def SCALE-W 800)
(def SCALE-H 450)

(def KEYBOARD
  {40 :down
   38 :up
   37 :paddle-1
   39 :paddle-2})

;;-----------------------------------------------------------------------------
;; DOM
;;-----------------------------------------------------------------------------

(defn by-id
  [id]
  (.getElementById js/document id))

(defn by-tag
  [tag-name]
  (aget (.getElementsByTagName js/document tag-name) 0))

(defn attr!
  [el attr val]
  (aset el attr val)
  el)

(defn listen!
  [el type fn]
  (.addEventListener el type fn false))

;;-----------------------------------------------------------------------------

(defrecord Rect [x y width height color])
(defrecord Frame [x y width height color frame-width])
(defrecord Paddle [x y width height vy color])
(defrecord Ball [x y radius vx vy fill-color])

;;-----------------------------------------------------------------------------
;; Drawable Objects
;;-----------------------------------------------------------------------------

(defprotocol IDrawable
  (draw! [_ ctx]))

(extend-type Paddle
  IDrawable
  (draw! [paddle ctx]
    (let [{:keys [color x y width height]} paddle]
      (aset ctx "fillStyle" color)
      (.fillRect ctx x y width height))
    ctx))

(extend-type Ball
  IDrawable
  (draw! [{:keys [x y radius fill-color] :as ball} ctx]
    (aset ctx "fillStyle" fill-color)
    (.beginPath ctx)
    (.arc ctx x y radius 0 (* 2 js/Math.PI) false)
    (.fill ctx)
    (.closePath ctx)))

(extend-type Rect
  IDrawable
  (draw! [rect ctx]
    (let [{:keys [color x y width height]} rect]
      (aset ctx "lineWidth" "0.5")
      (aset ctx "fillStyle" color)
      (aset ctx "strokeStyle" "black")
      (.fillRect ctx x y width height)
      (.strokeRect ctx x y width height))
    ctx))

(extend-type Frame
  IDrawable
  (draw! [frame ctx]
    (let [{:keys [color x y width height frame-width]} frame]
      (aset ctx "lineWidth" (str frame-width))
      (aset ctx "strokeStyle" color)
      (.strokeRect ctx x y width height))
    ctx))

;;-----------------------------------------------------------------------------
;; Moveable Objects
;;-----------------------------------------------------------------------------

(defprotocol IMovable
  (move [_]))

(extend-type Ball
  IMovable
  (move [{:keys [x y vx vy radius] :as ball}]
    (let [low radius
          vx (if (< low x (- SCALE-W radius)) vx (* -1 vx))
          vy (if (< low y (- SCALE-H radius)) vy (* -1 vy))]
      (assoc ball :x (+ x vx) :y (+ y vy) :vx vx :vy vy))))

;;-----------------------------------------------------------------------------
;; Controllable Objects
;;-----------------------------------------------------------------------------

(defprotocol IControllable
  (control! [_ event]))

(extend-type Paddle
  IControllable
  (control! [{:keys [y vy height] :as paddle} event]
    (condp = event
      :up
      (if (> (- y vy) 0) (assoc paddle :y (- y vy)) paddle)

      :down
      (if (< (+ y vy height) SCALE-H ) (assoc paddle :y (+ y vy)) paddle)

      :else
      paddle)))

;;-----------------------------------------------------------------------------
;; Game State
;;-----------------------------------------------------------------------------

(def state
  ;; Order matters
  (atom {:current-paddle :paddle-1
         :background (Rect. 0 0 SCALE-W SCALE-H "black")   ;; use div behind canvas
         :border     (Frame. 0 0 SCALE-W SCALE-H "#333" 2) ;; use div behind canvas
         :paddle-1   (Paddle. 20 (- SCALE-H 120) 20 100 10 "dodgerblue")
         :paddle-2   (Paddle. (- SCALE-W 40) 20 20 100 10 "peru")
         :ball       (Ball. 400 225 13 3 2 "lime")}))

;;-----------------------------------------------------------------------------
;; Animation
;;-----------------------------------------------------------------------------

(defn draw-phase!
  [ctx]
  (.clearRect ctx 0 0 SCALE-W SCALE-H)
  (doseq [o (vals @state)]
    (when (satisfies? IDrawable o)
      (draw! o ctx))))

(defn move-phase!
  []
  (swap! state #(assoc % :ball (move (:ball %)))))

(defn animate!
  [ctx]
  (move-phase!)
  (draw-phase! ctx)
  (.requestAnimationFrame js/window #(animate! ctx)))

;;-----------------------------------------------------------------------------
;; Control
;;-----------------------------------------------------------------------------

(defn resize!
  [ctx e]
  (let [w (- (.-innerWidth js/window) 40)
        h (- (int (/ (* w 9) 16)) 40)]
    (-> (by-id "canvas")
        (attr! "width" w)
        (attr! "height" h))
    (.scale ctx (/ w SCALE-W) (/ h SCALE-H))
    (draw-phase! ctx)))

(defn- event-loop!
  [state ch]
  (go-loop []
    (when-let [event (<! ch)]
      (cond
        (contains? #{:up :down} event)
        (let [id (:current-paddle @state)
              paddle (id @state)]
          (swap! state assoc id (control! paddle event)))

        (contains? #{:paddle-1 :paddle-2} event)
        (swap! state assoc :current-paddle event)

        :else
        (println "Unhandled event:" event))
      (recur))))

(def control-stream
  (comp (map #(or (get KEYBOARD %) :unknown))
        (filter #(not= % :unknown))))

(defn- main
  []
  (println "Welcome to the Canvas Scratch App.")
  (let [ctx (.getContext (by-id "canvas") "2d")
        events (chan 1 control-stream)]
    (event-loop! state events)
    (listen! js/window "resize" (partial resize! ctx))
    (listen! js/document "keydown" #(put! events (.-keyCode %)))
    (resize! ctx nil)
    (animate! ctx)))

(set! (.-onload js/window) main)
