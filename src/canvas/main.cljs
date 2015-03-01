(ns canvas.main)

(enable-console-print!)

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

(defn ctx-prop!
  [ctx attr value]
  (aset ctx attr value)
  ctx)

(defn ctx-apply
  [ctx fname & args]
  (let [f (aget ctx fname)]
    (.apply f ctx (apply array args))
    ctx))

(def ctx (.getContext (by-id "canvas") "2d"))
(def scale-w 800)
(def scale-h 450)

(defn draw!
  [ctx]
  (let [c (by-id "canvas")]
    (-> ctx
        (ctx-apply "clearRect" 0 0 scale-w scale-h)
        (ctx-prop! "fillStyle" "green")
        (ctx-apply "fillRect" 0 0 scale-w scale-h)
        (ctx-prop! "lineWidth" "0.5")
        (ctx-prop! "strokeStyle" "black")
        (ctx-prop! "fillStyle" "rgb(200, 0, 0)")
        (ctx-apply "strokeRect" 10 10 110 100)
        (ctx-apply "fillRect" 10 10 110 100)
        (ctx-prop! "fillStyle" "rgba(0,0,200,0.5)")
        (ctx-apply "strokeRect" 60 60 110 100)
        (ctx-apply "fillRect" 60 60 110 100)
        ;;
        ;; See if the virtual bounds are what I think they are.
        (ctx-prop! "lineWidth" "4")
        (ctx-prop! "strokeStyle" "dodgerblue")
        (ctx-apply "strokeRect" 0 0 scale-w scale-h))))

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
    (draw! ctx)))

(defn- main
  []
  (println "Welcome to the Canvas Scratch App.")
  (listen! js/window "resize" resize!)
  (draw! ctx)
  (resize! nil)
  (draw! ctx))

(set! (.-onload js/window) main)
