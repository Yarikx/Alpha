package org.ygeryatovich.alphaprocedure

import AlphaDefs._
import scala.io.Source

object DatasetUtil {
  lazy val r = new scala.util.Random()

  def rand[T](seq: Seq[T]) = seq(r.nextInt(seq.size))

  def splitBy[T](seq: Seq[T])(f: T => Boolean) = {

    val leftClass = f(seq.head)
    val rightClass = !leftClass

    val (leftGood, other) = seq span (f(_) == leftClass)
    val (mixed, rightGood) = {
      val rightIndex = other.lastIndexWhere(f(_) == leftClass)
      other.splitAt(rightIndex + 1);
    }

    (leftGood, mixed, rightGood)
  }

  def probeAngle(f: Angle => Int) = {
    val N = 100
    (for {
      probe <- 0 to N
      angle = 2 * scala.math.Pi / N * probe
      res = f(angle)
    } yield (angle, res)).maxBy(_._2)
  }

  def readDataset(fileName: String) = {
    val file = Source.fromFile(fileName)
    val linesStream = file.getLines.toStream;
    val pointWithValues = for {
      (stringLine, index) <- linesStream.zipWithIndex
      parts = stringLine.split(" ").toSeq
      featureStrings :+ pointClass = parts
      featureValues = featureStrings.map(_.toDouble)
      point = Point(index, pointClass)
    } yield (point, featureValues)

    val mapmap = pointWithValues.foldLeft(Map[Int, Map[Point, FeatureValue]]())((map, x) => {
      val (point, values) = x
      values.zipWithIndex.foldLeft(map)((mapmap, x) => {
        val (value, index) = x
        val actualMap = mapmap.get(index).getOrElse(Map())
        mapmap + (index -> (actualMap + (point -> value)))
      })

    })

    val features = mapmap.toSeq.sortBy(_._1).map {
      case (i, map) => Feature(SimpleName(s"$i"), map)
    }

    val points = pointWithValues.map(_._1)

    Dataset(points.toSeq, features)
  }

  /**
   * Src line
   * y = tg(alpha) * x
   *
   * proected line
   * y = kx + b; b = kx - b; (k = ctg(alpha) = 1/tg(alpha)
   *
   * kx = x/k + b
   * x = b/(k-1/k)
   */
  def featureProectionValue(x: FeatureValue, y: FeatureValue, alpha: Angle) = {
    val k = scala.math.tan(alpha)
    if (k != 0) {
      //b of complementary vector
      val b = y + x / k
      val xProected = b / (k + 1 / k)
      val yProected = k * x
      val sign = if (xProected == 0) scala.math.signum(yProected) else scala.math.signum(xProected)
      sign * scala.math.sqrt(xProected * xProected + yProected * yProected)
    } else x
  }

  def proectPoints(surface: Surface, alpha: Angle): DecartResult = {
    for {
      (point, (x, y)) <- surface
      newFeatureVal = featureProectionValue(x, y, alpha)
    } yield (point, newFeatureVal)
  }

  def solveDataset(dataset: Dataset) = {
    def recur(ds: Dataset, best: Feature, acc: Seq[(FeatureKind, Angle, Int)]): Seq[(FeatureKind, Angle, Int)] = {
      if (ds.features.size >= 1) {
        val secondBest = ds.bestFeature;
        val updDS = ds.without(secondBest)
        val decart = ds.decart(best, secondBest)
        val (proectionAngle, res) = probeAngle(angle => {
          val proection = proectPoints(decart, angle).sortBy(_._2)
          -howGood(proection)
        })
        
        val resKind = ComposedName(best, secondBest)
        val newFeature = Feature(resKind, proectPoints(decart, proectionAngle).toMap)
        
        val updAcc = acc :+ (secondBest.kind, proectionAngle, -res)
        
        if(res == 0) //full separation
          updAcc
        else recur(updDS, newFeature, updAcc);
      } else acc
    }
    val best = dataset.bestFeature
    recur(dataset.without(best), best, Seq())
  }

  def howGood(line: DecartResult) = {
    val firstClass = line.head._1.pClass
    val (_, bad, _) = splitBy(line)(x => x._1.pClass == firstClass)
    bad.size
  }

  def produceFeature(kind: FeatureKind, proection: DecartResult) =
    Feature(kind, proection.toMap)
    
  def selectRandom[A](xs: Seq[A], n: Int)={
    require(xs.size > n)
    val indexes = Stream.continually(r.nextInt(xs.size)).distinct.take(n)
    for{
      (x, i) <- xs.zipWithIndex
      if indexes.contains(i)
    } yield x
  }  
    
  def removeRandomPoints(ds: Dataset, percent: Int)={
    val points = ds.points
    val removedCount = points.size * percent / 100
    
    val selected = selectRandom(points, removedCount)
    val rest = points.filterNot(selected.contains)
    val badDS = ds.filterPoints(selected.contains)
    val goodDS = ds.filterPoints(rest.contains)
    (goodDS, badDS)
  }  

    
    
}