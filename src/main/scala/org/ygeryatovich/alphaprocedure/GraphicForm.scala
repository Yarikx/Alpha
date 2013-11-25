package org.ygeryatovich.alphaprocedure

import java.awt.Dimension
import scala.swing.BorderPanel
import scala.swing.MainFrame
import scala.swing.SimpleSwingApplication
import scala.swing.TabbedPane
import scala.swing.TabbedPane.Page

object GraphicForm extends SimpleSwingApplication {

  val ds = DatasetUtil.readDataset("/home/yarik/KPI/germany/inside/ds/crabO_MvsF.dat")
  val surface = {
    val bf = ds.bestFeature
    val scd = ds.without(bf).bestFeature
    ds.decart(bf, scd)
  }

  val solved = DatasetUtil.solveDataset(ds)

  val proections = ds.buildAllFeatures(solved.map {
    case (kind, angel, _) => (angel, kind)
  }, ds.bestFeature)
  
  val tp = new TabbedPane 
  
  val panels = for{
    (s,a) <- proections
    chartPanel = new ProectionResultView(s, a).panel
  } yield chartPanel
  
  panels.zipWithIndex.foreach{case (p, i) => tp.pages += new Page((i + 1).toString, p)}

//  val chartPanel = new ProectionResultView(surface, scala.math.Pi / 4).panel;
  
  def top = new MainFrame {
    contents = new BorderPanel {
      add(tp, BorderPanel.Position.Center)
    }
    size = new Dimension(400, 400)
  }

}