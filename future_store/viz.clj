(ns future-store.viz
  (:use (future-store raw builder utils graphml))
  (:import 
    java.io.StringReader
    javax.swing.JFrame
    (prefuse Constants Display Visualization)
    (prefuse.action ActionList RepaintAction)
    (prefuse.action.assignment ColorAction DataColorAction)
    prefuse.action.layout.graph.ForceDirectedLayout
    prefuse.activity.Activity
    (prefuse.controls DragControl PanControl ZoomControl)
    prefuse.data.Graph
    (prefuse.data.io DataIOException GraphMLReader)
    (prefuse.render DefaultRendererFactory LabelRenderer)
    prefuse.util.ColorLib
    prefuse.visual.VisualItem))

(defn make-graph []
  (build-tree (root-node) 3 3 :foo)
  (in-tx 
    (doseq [n (all-nodes)]
      (set-property n "foo" "bar")
      (set-property n "name" "random"))
    (success))
  (print-gxml ["name"])
  (with-out-str (print-gxml ["name" "foo"])))

(defn draw-graph [g]
  (println "Drawing Graph: \n\n" g "\n\n")
  (let [reader (new GraphMLReader)
        is (new java.io.ByteArrayInputStream (.getBytes g "UTF-8"))
        graph  (.readGraph reader is)
        viz   (doto (new Visualization) 
                (.addGraph "graph" graph)
                (.setInteractive "graph.edges" nil false))
        display (doto (new Display viz) 
                  (.setSize 720 500)
                  (.addControlListener (new DragControl))
                  (.addControlListener (new PanControl))
                  (.addControlListener (new ZoomControl)))
        frame (doto (new JFrame "future-store basic")
                ;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
                (.add display))
        lr    (doto (new LabelRenderer "foo")
                (.setRoundedCorner 8 8))
        palette (into-array Integer/TYPE
                  [(ColorLib/rgb 255 180 180)
                   (ColorLib/rgb 190 190 255)])
        stroke (new ColorAction "graph.nodes" VisualItem/STROKECOLOR (ColorLib/rgb 30 30 150))
        ;fill (new DataColorAction "graph.nodes" "gender" Constants/NOMINAL VisualItem/FILLCOLOR palette)
        text (new ColorAction "graph.nodes" VisualItem/TEXTCOLOR (ColorLib/gray 0))
        edges (new ColorAction "graph.edges" VisualItem/STROKECOLOR (ColorLib/gray 200))
        color (doto (new ActionList) 
                (.add stroke)
                (.add text)
                (.add edges))
        layout (doto (new ActionList Activity/INFINITY)
                 (.add (new ForceDirectedLayout "graph"))
                 (.add (new RepaintAction)))]
    (.setRendererFactory viz (new DefaultRendererFactory lr))
    (.putAction viz "color" color)
    (.putAction viz "layout" layout)
    
    (.pack frame)
    (.setVisible frame true)

    (.run viz "color")
    (.run viz "layout")))

(defn goviz []
  (test-store 
    (draw-graph (make-graph))))
