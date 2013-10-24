import DatasetUtil._
import scala.math.{ sin, cos }

object AlphaDefs {
  type FeatureKind = Symbol
  type FeatureValue = Double
  type PointClass = Symbol
  type PointId = Int
  case class Point(id: PointId, pClass: PointClass)
  case class Feature(kind: FeatureKind, featureMap: Map[Point, FeatureValue])
  
  type Surface = Seq[(Point, (FeatureValue, FeatureValue))]
  type Angle = Double
  type DecartResult = Seq[(Point, FeatureValue)]

  case class Dataset(points: Seq[Point], features: Seq[Feature]) {

    val featureTypes = features.map(_.kind)
    val classes = points.map(_.pClass).toSet
    val featureMap = (for {
      Feature(kind, values) <- features
    } yield kind -> values) toMap
    
    require(classes.size == 2)

    private def pointsWithFeature(feature: Feature) =
      points.zip(points.map(feature.featureMap(_)))

    def splitByFeature(feature: Feature) = {
      require(features.contains(feature))
      val line = pointsWithFeature(feature).sortBy(_._2)
      splitByPred(line)(x => x._1.pClass == classes.head)
    }

    def howGood(feature: Feature) = {
      val (left, mix, right) = splitByFeature(feature)
      left.size + right.size
    }

    def bestFeature = features.maxBy(howGood)

    def decart(f1: Feature, f2: Feature) =
      for {
        point <- points
        left = f1.featureMap(point)
        right = f2.featureMap(point)
      } yield (point, (left, right))

  }

  def proect(surface: Surface, alpha: Angle): DecartResult = {
    for {
      (point, (x, y)) <- surface
      newFeatureVal = y / sin(alpha) + x * cos(alpha)
    } yield (point, newFeatureVal)
  }

  def howGood(line: DecartResult) = {
    val firstClass = line.head._1.pClass
    val (l, _, r) = splitByPred(line)(x => x._1.pClass == firstClass)
    l.size + r.size
  }

}