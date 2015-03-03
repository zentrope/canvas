(ns canvas.main
  (:require-macros
   [cljs.core.async.macros :refer [go-loop]])
  (:require
   [cljs.core.async :refer [chan <! put! sliding-buffer]]))

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

(defn sqr
  [x]
  (* x x))

(defn dist
  [x1 y1 x2 y2]
  (js/Math.sqrt (+ (sqr (- x2 x1))
                   (sqr (- y2 y1)))))

;;-----------------------------------------------------------------------------
;; DOM
;;-----------------------------------------------------------------------------

(defn by-id
  [id]
  (.getElementById js/document id))

(defn attr!
  [el attr val]
  (aset el attr val)
  el)

(defn listen!
  [el type fn]
  (.addEventListener el type fn false))

;;-----------------------------------------------------------------------------
;; Object Definitions
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
    (.closePath ctx)
    ctx))

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
         ;; use div behind canvas
         :background (Rect. 0 0 SCALE-W SCALE-H "black")
          ;; use div behind canvas
         :border     (Frame. 0 0 SCALE-W SCALE-H "#333" 2)
         :paddle-1   (Paddle. 20 (- SCALE-H 120) 20 100 10 "dodgerblue")
         :paddle-2   (Paddle. (- SCALE-W 40) 20 20 100 10 "peru")
         :ball       (Ball. 400 225 13 3 2 "lime")}))

;;-----------------------------------------------------------------------------
;; Animation
;;-----------------------------------------------------------------------------

(defn draw-phase!
  [state ctx]
  (.clearRect ctx 0 0 SCALE-W SCALE-H)
  (doseq [object (vals @state)]
    (when (satisfies? IDrawable object)
      (draw! object ctx))))

(defn move-phase!
  [{:keys [ball] :as state}]
  (merge state {:ball (move ball)}))

(defn hit?
  [ball paddle]
  (let [{x1 :x y1 :y} paddle
        x2 (+ x1 (:width paddle))
        y2 (+ y1 (:height paddle))
        {x :x y :y radius :radius} ball]
    ;; janky when the ball hits a corner, I think.
    (or (and (<= y1 y y2) (<= x1 x x2))
        ;; Need to indicate if x or y direction is reversed
        (and (<= y1 y y2) (or (>= radius (dist x2 y x y))
                              (>= radius (dist x1 y x y))))
        (and (<= x1 x x2) (or (>= radius (dist x y2 x y))
                              (>= radius (dist x y1 x y)))))))

(defn collision-phase!
  [{:keys [ball paddle-1 paddle-2] :as state}]
  (if (or (hit? ball paddle-1)
          (hit? ball paddle-2))
    (assoc state :ball (assoc ball :vx (* -1 (:vx ball))))
    state))

(defn animate-loop!
  [state ctx]
  (swap! state #(-> % move-phase! collision-phase!))
  (draw-phase! state ctx)
  (.requestAnimationFrame js/window (partial animate-loop! state ctx)))

;;-----------------------------------------------------------------------------
;; Control
;;-----------------------------------------------------------------------------

(defn resize!
  [state ctx]
  (let [w (- (.-innerWidth js/window) 40)
        h (- (int (/ (* w 9) 16)) 40)]
    (-> (by-id "canvas")
        (attr! "width" w)
        (attr! "height" h))
    (.scale ctx (/ w SCALE-W) (/ h SCALE-H))
    (draw-phase! state ctx)))

(defn- event-loop!
  [state ch]
  (go-loop []
    (when-let [event (<! ch)]
      (cond
        (contains? #{:up :down} event)
        (swap! state (fn [{id :current-paddle :as state}]
                       (assoc state id (control! (id state) event))))

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
        events (chan (sliding-buffer 10) control-stream)]
    (event-loop! state events)
    (listen! js/window   "resize"  #(resize! state ctx))
    (listen! js/document "keydown" #(put! events (.-keyCode %)))
    (resize! state ctx)
    (animate-loop! state ctx)))

(set! (.-onload js/window) main)
