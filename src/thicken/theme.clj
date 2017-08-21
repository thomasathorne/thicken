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

(ns thicken.theme
  (:import org.jfree.chart.renderer.xy.StandardXYBarPainter
           org.jfree.chart.plot.PlotOrientation
           java.awt.Color
           java.awt.Shape))

(def vertical PlotOrientation/VERTICAL)

(defn color [r g b] (Color. r g b))

(defmulti set-theme type)

(defmethod set-theme org.jfree.chart.plot.Plot
  [plot]
  (doto plot
    (.setBackgroundPaint Color/white)
    (.setRangeGridlinePaint (color 200 100 200))
    (.setDomainGridlinePaint (color 200 100 200))))

(defmethod set-theme org.jfree.chart.renderer.xy.XYBarRenderer
  [renderer]
  (doto renderer
    (.setOutlinePaint Color/white)
    (.setPaint Color/gray)
    (.setDrawBarOutline true)
    (.setBarPainter (StandardXYBarPainter.))))

(defmethod set-theme org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
  [renderer]
  (let [point-size 4
        c (- (/ point-size 2))
        colors [Color/gray Color/green Color/red Color/blue Color/magenta
                Color/pink Color/cyan Color/orange Color/yellow]]
    (doseq [i (range 9)]
      (.setSeriesPaint renderer i (nth colors i))
      (.setSeriesShape renderer i (java.awt.geom.Ellipse2D$Double. c c point-size point-size)))))

(defmethod set-theme org.jfree.chart.JFreeChart
  [chart]
  (let [plot (.getPlot chart)
        renderer (.getRenderer plot)]
    (set-theme plot)
    (set-theme renderer)))

(defmethod set-theme org.jfree.chart.renderer.category.BoxAndWhiskerRenderer
  [renderer]
  nil)
