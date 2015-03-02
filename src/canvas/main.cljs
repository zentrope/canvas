(ns canvas.main)

(enable-console-print!)

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
;; Canvas Context
;;-----------------------------------------------------------------------------

(def ctx (.getContext (by-id "canvas") "2d"))
(def SCALE-W 800)
(def SCALE-H 450)

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
  (control! [{:keys [y vy] :as paddle} event]
    (case event
      :up (assoc paddle :y (- y vy))
      :down (assoc paddle :y (+ y vy))
      paddle)))

;;-----------------------------------------------------------------------------
;; Game State
;;-----------------------------------------------------------------------------

(def state
  ;; Order matters
  (atom {:background (Rect. 0 0 SCALE-W SCALE-H "black")   ;; use div behind canvas
         :border     (Frame. 0 0 SCALE-W SCALE-H "#333" 2) ;; use div behind canvas
         :paddle-1   (Paddle. 20 (- SCALE-H 120) 20 100 2 "dodgerblue")
         :paddle-2   (Paddle. (- SCALE-W 40) 20 20 100 2 "peru")
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
  []
  (move-phase!)
  (draw-phase! ctx)
  (.requestAnimationFrame js/window animate!))

;;-----------------------------------------------------------------------------
;; Control
;;-----------------------------------------------------------------------------

(defn resize!
  [e]
  (let [w (- (.-innerWidth js/window) 40)
        h (- (int (/ (* w 9) 16)) 40)]
    (-> (by-id "canvas")
        (attr! "width" w)
        (attr! "height" h))
    (.scale ctx (/ w SCALE-W) (/ h SCALE-H))
    (draw-phase! ctx)))

(defn- main
  []
  (println "Welcome to the Canvas Scratch App.")
  (listen! js/window "resize" resize!)
  (resize! nil)
  (animate!))

(set! (.-onload js/window) main)
