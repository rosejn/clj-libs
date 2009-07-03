(ns future-store.viz
  (:use (future-store raw builder utils graphml))
  (:import 
    java.io.StringReader
    (javax.swing JFrame JPanel JLabel JTextField JButton)
    (java.awt.event ActionListener)
    (java.awt GridLayout)
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
  (with-out-str (print-gxml ["name" "foo"])))

(defn save-pdf [frame path]
  (let [width (.getWidth frame)
        height (.getHeight frame)
        doc (new com.lowagie.text.Document)
        file (new java.io.FileOutputStream path)
        writer (com.lowagie.text.pdf.PdfWriter/getInstance doc file)]
    (.open doc)
    (let [cb (.getDirectContent writer)
          tp (.createTemplate cb width height)
          g2d (.createGraphics tp width height)
          r2d (new java.awt.geom.Rectangle2D$Double 0 0 width height)]
      (.setColor g2d java.awt.Color/WHITE)
      (.fill g2d (.getBounds frame))
      (.print frame g2d)
      (.setColor g2d java.awt.Color/BLACK)
      (.draw g2d (.getBounds frame))
      (.dispose g2d)
      (.addTemplate cb tp 30 300)
      (.close doc))))

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
        panel (doto (new JPanel)
                ;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
                (.add display))
        lr    (doto (new LabelRenderer "label")
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
    (.run viz "color")
    (.run viz "layout")
    panel))

(defn goviz []
  (test-store 
    (let [graph (make-graph)
          g-panel (draw-graph graph)
          app-frame (JFrame. "Future Store - Visualization")
          pdf-button (JButton. "Save to PDF")]
      (.addActionListener pdf-button
        (proxy [ActionListener] []
          (actionPerformed [evt] (save-pdf g-panel "test.pdf"))))
    (doto app-frame
      (.setLayout (GridLayout. 2 1 3 3))
      (.add g-panel)
      (.add pdf-button)
      ;(.setSize 300 80)
      (.pack)
      (.setVisible true)))))

(defn show-graph []
  (let [xml (with-out-str (print-gxml ["label"]))
        g-panel (draw-graph xml)
        app-frame (JFrame. "Future Store - Visualization")]
    (doto app-frame
      (.setLayout (GridLayout. 2 1 3 3))
      (.add g-panel)
      (.pack)
      (.setVisible true))))

(def test-graph {:app {
                 :foo {:app-name "foo"}
                 :bar {:app-name "bar"}
                }
           :net {
                  :peer [{:peer-id 1} {:peer-id 2}]
                }
          })

(defn test-viz []
  (test-store
    (load-tree (root-node) :test test-graph)
    (show-graph)))
