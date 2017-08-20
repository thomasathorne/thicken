(ns thicken.core
  (:require [thicken.theme :as theme])
  (:import org.jfree.chart.ChartFrame
           org.jfree.chart.ChartUtilities
           org.jfree.chart.JFreeChart
           org.jfree.chart.ChartFactory
           org.jfree.chart.axis.ValueAxis
           org.jfree.chart.plot.FastScatterPlot
           org.jfree.chart.plot.PlotOrientation
           org.jfree.chart.renderer.xy.StandardXYBarPainter
           org.jfree.data.xy.DefaultXYDataset
           org.jfree.data.statistics.HistogramDataset
           java.io.File
           cern.jet.stat.tdouble.Probability))

(defn histogram
  [data & [{:keys [bins title x-lab y-lab] :or {bins 20 title "Histogram"}}]]
  (let [dataset (HistogramDataset.)]
    (.addSeries dataset "data" (double-array data) bins)
    (let [chart (ChartFactory/createHistogram title x-lab y-lab
                                              dataset
                                              theme/vertical false false false)]
      (theme/set-theme chart)
      chart)))

(defn scatter-plot
  [data-groups & [{:keys [title x-lab y-lab data-labels]
                   :or {title "Scatter Plot" x-lab "x" y-lab "y"}}]]
  (let [data-labels (or data-labels (map str (range (count data-groups))))
        dataset (DefaultXYDataset.)]
    (doseq [i (range (count data-groups))]
      (.addSeries dataset (nth data-labels i)
                  (into-array [(double-array (map first (nth data-groups i)))
                               (double-array (map second (nth data-groups i)))])))
    (let [chart (ChartFactory/createScatterPlot title x-lab y-lab
                                                dataset)]
      (theme/set-theme chart)
      chart)))

(defn box-and-whisker
  [& ])

(defn qq-plot
  [data & [{:keys [title x-lab y-lab]
            :or {title "QQ Plot" x-lab "Normal Quantiles" y-lab "Data Quantiles"}}]]
  (let [sorted (sort data)
        n (count data)
        plot-points (map-indexed (fn [i x]
                                   [(Probability/normalInverse (/ (inc i) (inc n))) x])
                                 sorted)]
    (scatter-plot [plot-points] {:title title :x-lab x-lab :y-lab y-lab})))

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
