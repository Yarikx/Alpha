package org.ygeryatovich.alphaprocedure

import DatasetUtil._
import scala.util.Random

object MyDebugApp extends App {
  
  val ds = readDataset("/home/yarik/KPI/germany/inside/ds/crabO_MvsF.dat")
  
  println("******** stability test \n")
  for{
    attempt <- 1 to 15
    pointsNum = 1 + Random.nextInt(40)
    (good, bad) = removeRandomPoints(ds, pointsNum)
    result = solveDataset(good)
    (kinds,angles,ress) = result.unzip3
  } println(s"for $pointsNum - $angles")
  
  
  println("\n******** corectness test \n")
  
  val (good, bad) = removeRandomPoints(ds, 50)
  val best = good.bestFeature
  val solved = solveDataset(good)
  
  solved foreach println
  println(s"original mixed number is ${solved.last._3}")
  
  val solved2 = solved.map{case (x, y, _) => (y,x)}
  
  val bestFeature = bad.buildBestFeature(solved2, best.kind)
  
  println ("checked mixed number " +bad.howGood(bestFeature))
  
  good.buildAllFeatures(solved2, best)
  
}