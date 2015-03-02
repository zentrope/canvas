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

(defn style!
  [el attr val]
  (let [style (aget el "style")]
    (aset style attr val)
    el))

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

(defn ctx-prop!
  [ctx attr value]
  (aset ctx attr value)
  ctx)

(defn ctx-apply
  [ctx fname & args]
  (let [f (aget ctx fname)]
    (.apply f ctx (apply array args))
    ctx))

;;-----------------------------------------------------------------------------

(defrecord Rect [x y width height color])
(defrecord Frame [x y width height color frame-width])
(defrecord Paddle [x y width height color])

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

(def ctx (.getContext (by-id "canvas") "2d"))
(def scale-w 800)
(def scale-h 450)

(def objects
  ;; Order matters
  {:background (Rect. 0 0 scale-w scale-h "black")
   :s1         (Rect. 10 10 110 100 "rgb(200, 0, 0)")
   :s2         (Rect. 60 60 110 100 "rgba(0,0,200,0.5)")
   :paddle-1   (Paddle. 20 (- scale-h 120) 20 100 "dodgerblue")
   :paddle-2   (Paddle. (- scale-w 40) 20 20 100 "dodgerblue")
   :border     (Frame. 0 0 scale-w scale-h "dodgerblue" 2)})

;;-----------------------------------------------------------------------------

(defn draw-phase!
  [ctx]
  (.clearRect ctx 0 0 scale-w scale-h)
  (doseq [o (vals objects)]
    (when (satisfies? IDrawable o)
      (draw! o ctx))))

(defn resize!
  [e]
  ;; Maintains 16x9
  (let [w (- (.-innerWidth js/window) 40)
        h (- (int (/ (* w 9) 16)) 40)]
    (-> (by-id "canvas")
        ;; (style! "width" (str w "px"))
        ;; (style! "height" (str h "px"))
        (attr! "width" w)
        (attr! "height" h))
    (.scale ctx (/ w scale-w) (/ h scale-h))
    (draw-phase! ctx)))

(defn- main
  []
  (println "Welcome to the Canvas Scratch App.")
  (listen! js/window "resize" resize!)
  (draw-phase! ctx)
  (resize! nil)
  (draw-phase! ctx))

(set! (.-onload js/window) main)
