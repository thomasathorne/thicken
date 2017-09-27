;;   Copyright (c) Thomas Athorne
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

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
           org.jfree.data.statistics.BoxAndWhiskerCalculator
           org.jfree.data.statistics.DefaultBoxAndWhiskerCategoryDataset
           org.jfree.data.statistics.HistogramDataset
           java.io.File
           cern.jet.stat.tdouble.Probability))

(defn histogram
  [data & [{:keys [bins title x-lab y-lab x-range] :or {bins 20 title "Histogram"}}]]
  (let [dataset (HistogramDataset.)]
    (.addSeries dataset "data" (double-array data) bins)
    (let [chart (ChartFactory/createHistogram title x-lab y-lab
                                              dataset
                                              theme/vertical false false false)]
      (theme/set-theme chart)
      (when x-range
        (.setRange (.getDomainAxis (.getPlot chart)) (first x-range) (second x-range)))
      chart)))

(defn scatter-plot
  [data-groups & [{:keys [title x-lab y-lab data-labels x-range y-range]
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
      (when x-range
        (.setRange (.getDomainAxis (.getPlot chart)) (first x-range) (second x-range)))
      (when y-range
        (.setRange (.getRangeAxis (.getPlot chart)) (first y-range) (second y-range)))
      chart)))

(defn box-and-whisker
  [categories data-series & [{:keys [title x-lab y-lab y-range]
                              :or {title "Box and Whisker Chart"}}]]
  (let [dataset (DefaultBoxAndWhiskerCategoryDataset.)]
    (mapv (fn [[c d] data]
            (let [data-item (BoxAndWhiskerCalculator/calculateBoxAndWhiskerStatistics data)]
              (.add dataset data-item d c)))
          categories data-series)
    (let [chart (ChartFactory/createBoxAndWhiskerChart title x-lab y-lab dataset true)]
      (theme/set-theme chart)
      (when y-range
        (.setRange (.getRangeAxis (.getPlot chart)) (first y-range) (second y-range)))
      chart)))

(defn qq-plot
  [data & [{:keys [title x-lab y-lab]
            :or {title "QQ Plot" x-lab "Normal Quantiles" y-lab "Data Quantiles"}}]]
  (let [sorted (sort data)
        n (count data)
        plot-points (map-indexed (fn [i x]
                                   [(Probability/normalInverse (/ (inc i) (inc n))) x])
                                 sorted)]
    (scatter-plot [plot-points] {:title title :x-lab x-lab :y-lab y-lab})))

(defn qq-plot-any
  [data reference & [{:keys [title x-lab y-lab]
                      :or {title "QQ Plot" x-lab "Reference Quantiles" y-lab "Data Quantiles"}}]]
  (let [sorted (sort data)
        n (count data)
        reference-data (sort reference)
        plot-points (map vector reference-data sorted)]
    (scatter-plot [plot-points] {:title title :x-lab x-lab :y-lab y-lab})))

(defn q-plot
  [data & [{:keys [title x-lab y-lab]
            :or {title "Q Plot" x-lab "" y-lab "Data Quantiles"}}]]
  (let [sorted (sort data)
        n (count data)
        plot-points (map-indexed (fn [i x] [(double (/ i n)) x]) sorted)]
    (scatter-plot [plot-points] {:title title :x-lab x-lab :y-lab y-lab})))

(defn view
  [chart & [{:keys [window-title width height] :or {window-title "Plot" width 500 height 400}}]]
  (let [frame (ChartFrame. window-title chart)]
    (doto frame
      (.setSize width height)
      (.setVisible true))
    frame))

(defn save
  [chart filename & [{:keys [width height] :or {width 1000 height 800}}]]
  (ChartUtilities/saveChartAsPNG (File. filename) chart width height))
