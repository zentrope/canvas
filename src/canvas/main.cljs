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

(defn draw!
  [ctx]
  (let [c (by-id "canvas")
        w (.-width c)
        h (.-height c)]
   (-> ctx
       (ctx-prop! "fillStyle" "rgb(100, 100, 100)")
       (ctx-prop! "strokeStyle" "black")
       (ctx-apply "fillRect" 0 0 w h)
       (ctx-prop! "fillStyle" "rgb(200, 0, 0)")
       (ctx-apply "strokeRect" 10 10 55 50)
       (ctx-apply "fillRect" 10 10 55 50)
       (ctx-prop! "fillStyle" "rgba(0,0,200,0.5)")
       (ctx-apply "strokeRect" 30 30 55 50)
       (ctx-apply "fillRect" 30 30 55 50))))

(defn resize!
  [e]
  (let [w (- (.-innerWidth js/window) 40)
        h (- (.-innerHeight js/window) 40)]
    (-> (by-id "canvas")
        (style! "width" (str w "px"))
        (style! "height" (str h "px"))
        (attr! "width" w)
        (attr! "height" h))
    (draw! ctx)))

(defn- main
  []
  (println "Welcome to the Canvas Scratch App.")
  (listen! js/window "resize" resize!)
  (resize! nil)
  (draw! ctx))

(set! (.-onload js/window) main)
