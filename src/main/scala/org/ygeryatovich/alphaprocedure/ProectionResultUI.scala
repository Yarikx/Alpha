package org.ygeryatovich.alphaprocedure

import scala.swing.BorderPanel
import scala.swing.Label
import AlphaDefs._
import scalax.chart._
import scalax.chart.Charting._
import org.jfree.chart.ChartTheme
import org.jfree.chart.StandardChartTheme
import org.jfree.chart.renderer.xy.StandardXYItemRenderer
import org.jfree.chart.JFreeChart
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.renderer.xy.XYItemRenderer
import org.jfree.chart.renderer.xy.XYDotRenderer
import org.jfree.data.xy.XYDataset
import org.jfree.data.xy.XYSeriesCollection
import org.jfree.chart.annotations.XYShapeAnnotation
import java.awt.geom.Ellipse2D
import java.awt.geom.Rectangle2D
import java.awt.geom.Line2D
import org.jfree.chart.axis.NumberTickUnit
import scala.math._

class ProectionResultView(val surface: Surface, angle: Angle) {
  
  val range = 30

  lazy val panel = new BorderPanel {
    val label = new Label("Hello world")

    val dataset = new XYSeriesCollection()
    val c1 = surface.head._1.pClass
    val (v1,v2) = surface.partition(_._1.pClass == c1)
    for{
      vs <- Seq(v1,v2)
      pClass = vs.head._1.pClass
      series = (for{
        (point, tuple) <- vs
      } yield tuple).toXYSeries(pClass)
    } dataset.addSeries(series)
    
    val chart = {
      
      val (xs, ys) = surface.unzip._2.unzip
      val all = xs ++ ys
      val maxV = all.max
      val minV = all.min
      val minXY = min(-5, minV)
      val maxXY = max(5, maxV)
      
      val domainAxis =  {
    		  val axis = new NumberAxis("")
    		  axis.setRange(minXY, maxXY)
    		  axis.setTickUnit(new NumberTickUnit(1))
//    		  axis.setAutoRangeIncludesZero(false)
    		  axis
      }
      val rangeAxis = new NumberAxis("Points")
          		  rangeAxis.setRange(minXY, maxXY)
    		  rangeAxis.setTickUnit(new NumberTickUnit(1))

      val renderer = new XYDotRenderer()
      renderer.setDotWidth(8)
      renderer.setDotHeight(8)

      val plot = new XYPlot(dataset, domainAxis, rangeAxis, renderer)
//      plot.setOrientation(orientation)
      plot.setForegroundAlpha(0.5f)
      
      val k = if(scala.math.tan(angle) == 0)
        0.000001 else scala.math.tan(angle)
      val customFigures = {
        val maxX = 100//surface.map(_._2._1).max
        val minX = -100//surface.map(_._2._1).min
        
        val maxY = k*maxX
        val minY = k*minX
        
        new Line2D.Double(minX, minY, maxX, maxY)
      }
      plot.addAnnotation(new XYShapeAnnotation(customFigures))
      
      val kk = -1/k
      for{
        (point, (x1, y1)) <- surface
        b = y1-kk*x1
        x = b/(k-kk)
        y = k*x
        line = new Line2D.Double(x, y, x1, y1)
      } plot.addAnnotation(new XYShapeAnnotation(line))
      val chart = new JFreeChart("title", JFreeChart.DEFAULT_TITLE_FONT, plot, true)
      
      new XYChart {
        override val peer = chart
      }
    }
    

    add(chart.toPanel, BorderPanel.Position.Center)
  }
}

case class Line(x1: Int,y1: Int,x2: Int,y2: Int)

object Help{
  
}

