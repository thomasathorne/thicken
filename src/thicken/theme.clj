(ns thicken.theme
  (:import org.jfree.chart.renderer.xy.StandardXYBarPainter
           org.jfree.chart.plot.PlotOrientation
           java.awt.Color
           java.awt.Shape))

(def vertical PlotOrientation/VERTICAL)

(defmulti set-theme type)

(defmethod set-theme org.jfree.chart.plot.Plot
  [plot]
  (doto plot
    (.setBackgroundPaint Color/white)
    (.setRangeGridlinePaint (Color. 200 100 200))
    (.setDomainGridlinePaint (Color. 200 100 200))))

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
        c (- (/ point-size 2))]
    (doto renderer
      (.setPaint (Color. 0 40 87))
      (.setSeriesShape 0 (java.awt.geom.Ellipse2D$Double. c c point-size point-size)))))

(defmethod set-theme org.jfree.chart.JFreeChart
  [chart]
  (let [plot (.getPlot chart)
        renderer (.getRenderer plot)]
    (set-theme plot)
    (set-theme renderer)))
