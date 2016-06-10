(ns thicken.core
  (:import org.jfree.chart.ChartFrame
           org.jfree.chart.ChartUtilities
           org.jfree.chart.JFreeChart
           org.jfree.chart.ChartFactory
           org.jfree.chart.axis.ValueAxis
           org.jfree.chart.plot.FastScatterPlot
           org.jfree.chart.plot.PlotOrientation
           org.jfree.chart.renderer.xy.StandardXYBarPainter
           org.jfree.data.statistics.HistogramDataset
           java.io.File))

(def vertical PlotOrientation/VERTICAL)

(defn set-theme
  [chart]
  (let [plot (.getPlot chart)
        renderer (.getRenderer plot)]
    (doto plot
      (.setBackgroundPaint java.awt.Color/white)
      (.setRangeGridlinePaint (java.awt.Color. 200 100 200))
      (.setDomainGridlinePaint (java.awt.Color. 200 100 200)))
    (doto renderer
      (.setOutlinePaint java.awt.Color/white)
      (.setPaint java.awt.Color/gray)
      (.setDrawBarOutline true)
      (.setBarPainter (StandardXYBarPainter.)))))

(defn histogram
  [data & {:as opts}]
  (let [bins (or (:bins opts) 20)
        title (or (:title opts) "Histogram")
        dataset (HistogramDataset.)]
    (.addSeries dataset "data" (double-array data) bins)
    (let [chart (ChartFactory/createHistogram title (:x-lab opts) (:y-lab opts)
                                              dataset
                                              vertical false false false)]
      (set-theme chart)
      chart)))

(defn view
  [chart & {:as opts}]
  (let [window-title (or (:window-title opts) "Plot")
        width (or (:width opts) 500)
        height (or (:height opts) 400)
        frame (ChartFrame. window-title chart)]
    (doto frame
      (.setSize width height)
      (.setVisible true))
    frame))

(defn save
  [chart filename & {:as opts}]
  (let [width (or (:width opts) 1000)
        height (or (:height opts) 800)]
    (ChartUtilities/saveChartAsPNG (File. filename) chart width height)
    nil))
