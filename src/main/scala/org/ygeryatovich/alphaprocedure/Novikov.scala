package org.ygeryatovich.alphaprocedure

import org.ygeryatovich.alphaprocedure.AlphaDefs._
import scala.math._
import scala.annotation.tailrec

object Novikov extends App {

  def mirrorDataset(ds: Dataset) = {
    val c1 = ds.classes.head
    val Dataset(points, features) = ds
    val maxId = points.map(_.id).max + 1

    val newPoints = for {
      point <- points
      Point(id, pc) = point
      if pc == c1
      updId = maxId + id
    } yield (point, Point(updId, pc))

    val newFeatures = for {
      feature <- features
      updFeature = newPoints.foldLeft(feature) {
        case (Feature(kind, fmap), (orig, gen)) =>
          val res = fmap(orig)
          val updMap = fmap + (gen -> -res)
          Feature(kind, updMap)
      }
    } yield updFeature

    val onlyNewPoints = newPoints.map(_._2)
    Dataset(points ++ onlyNewPoints, newFeatures)
  }

  type Vector = Seq[FeatureValue]

  def findBestSupportVector(ds: Dataset) = {
    val vsize = ds.features.size
    val r = scala.util.Random

    def generateAllVectors =
      for (_ <- 1 to 100)
        yield for (_ <- 1 to vsize)
        yield r.nextDouble * 2 - 1
        
    def generateAllB(max: Double) = for(_ <- 1 to  100)
      yield r.nextDouble*2*max - max

    def testVector(v: Vector) = {
      val c1 = ds.classes.head
      val pointsVecMul =
        for (point <- ds.points.toList) yield {
          val sum = (for {
            (ai, feature) <- v zip ds.features
            featureVal = feature.featureMap(point)
          } yield ai * featureVal).sum
          if (point.pClass == c1) sum
          else -sum
        }

      val (v1, v2) = pointsVecMul.partition(_ > 0)
      val sizes = (v1.size, v2.size)
      val notdivided = min(v1.size, v2.size)
      println(s"notdivided points $sizes")
      notdivided
    }
    
    def pointVector(p: Point) = ds.features.map(f => f.featureMap(p))
    
    def ro(point: Point, v: Vector): Double = {
      val pointVec = pointVector(point)
      val up = abs(pointVec.zip(v).map { case (x, a) => x * a }.sum)
      val bottom = sqrt(v.map(x => x * x).sum)
      up / bottom
    }
    
    def far(p1: Point)= sqrt(pointVector(p1).map(x => x*x).reduce(_ + _))
    
    val max = ds.points.map(far).max

    val vectorsWithGoodness = generateAllVectors.map(v => (v, testVector(v)))
    val minGoodness = vectorsWithGoodness.map(_._2).min
    val bestVectors = vectorsWithGoodness.collect {
      case (v, g) if g <= minGoodness => v
    }
    val all = for (vector <- bestVectors) yield {
      val pointRos = for (point <- ds.points) yield ro(point, vector)
      pointRos.min
    }
    all.max
    //    if (all.isEmpty) None
    //    else Some(all.max)
  }

  def calcFeaturesRo(ds: Dataset): List[(FeatureKind, Option[Double])] = {
    val Dataset(points, features) = ds
    if (features.size == 1) List((features.head.kind, None))
    else {
      val featuresWithRo =
        features.map(f =>
          (f,
            findBestSupportVector(ds.without(f))))

      if (featuresWithRo.isEmpty) Nil
      else {
        val (worstFeature, roVal) = featuresWithRo.maxBy(_._2)
        (worstFeature.kind, Some(roVal)) :: calcFeaturesRo(ds.without(worstFeature))
      }
    }
  }

  //  val ds = DatasetUtil.readDataset("/home/yarik/KPI/germany/inside/ds/my.dat")
  val ds = DatasetUtil.readDataset("/home/yarik/KPI/germany/inside/ds/segmentation.dat")
  //  val ds2 = mirrorDataset(ds)
  //  val ro = findBestSupportVector(ds)
  val fs = calcFeaturesRo(ds)
  fs.foreach {
    case (kind, Some(ro)) => println(s"without $kind ro=$ro")
    case (kind, None) => println(s"last feature is $kind")
  }

}