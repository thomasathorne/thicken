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
  [data & {:as opts}]
  (let [bins (or (:bins opts) 20)
        title (or (:title opts) "Histogram")
        dataset (HistogramDataset.)]
    (.addSeries dataset "data" (double-array data) bins)
    (let [chart (ChartFactory/createHistogram title (:x-lab opts) (:y-lab opts)
                                              dataset
                                              theme/vertical
                                              false false false)]
      (theme/set-theme chart)
      chart)))

(defn scatter-plot
  [& args]
  (let [[data-groups [& {:as opts}]] (split-with (complement keyword?) args)
        title (or (:title opts) "Scatter Plot")
        x-lab (or (:x-lab opts) "x")
        y-lab (or (:y-lab opts) "y")
        data-labels (or (:data-labels opts) (map str (range (count data-groups))))
        dataset (DefaultXYDataset.)]
    (doseq [i (range (count data-groups))]
      (.addSeries dataset (nth data-labels i)
                  (into-array [(double-array (map first (nth data-groups i)))
                               (double-array (map second (nth data-groups i)))])))
    (let [chart (ChartFactory/createScatterPlot title x-lab y-lab
                                                dataset)]
      (theme/set-theme chart)
      chart)))

(defn qq-plot
  [data & {:as opts}]
  (let [title (or (:title opts) "QQ Plot")
        x-lab (or (:x-lab opts) "Normal Quantiles")
        y-lab (or (:y-lab opts) "Data Quantiles")
        sorted (sort data)
        n (count data)
        plot-points (map-indexed (fn [i x]
                                   [(Probability/normalInverse (/ (inc i) (inc n))) x])
                                 sorted)]
    (scatter-plot plot-points
                  :title title
                  :x-lab x-lab
                  :y-lab y-lab)))

(defn qq-plot-any
  [data dist-fn & {:as opts}]
  (let [title (or (:title opts) "QQ Plot")
        x-lab (or (:x-lab opts) "Reference Quantiles")
        y-lab (or (:y-lab opts) "Data Quantiles")
        sorted (sort data)
        n (count data)
        reference-data (sort (repeatedly n dist-fn))
        plot-points (map vector reference-data sorted)]
    (scatter-plot plot-points
                  :title title
                  :x-lab x-lab
                  :y-lab y-lab)))

(defn q-plot
  [data & {:as opts}]
  (let [title (or (:title opts) "Q Plot")
        x-lab (or (:x-lab opts) "")
        y-lab (or (:y-lab opts) "Data Quantiles")
        sorted (sort data)
        n (count data)
        plot-points (map-indexed (fn [i x] [(double (/ i n)) x]) sorted)]
    (scatter-plot plot-points
                  :title title
                  :x-lab x-lab
                  :y-lab y-lab)))

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
