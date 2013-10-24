import DatasetUtil._
import scala.math.{ sin, cos }

object AlphaDefs {
  type FeatureKind = Symbol
  type FeatureValue = Double
  type PointClass = Symbol

  case class Point(features: Map[FeatureKind, FeatureValue], pClass: PointClass)
  type Surface = Seq[(Point, (FeatureValue, FeatureValue))]
  type Angle = Double
  type DecartResult = Seq[(Point, FeatureValue)]

  case class Dataset(points: Seq[Point]) {

    val featureTypes = for {
      point <- points
      key <- point.features.keySet
    } yield key

    val classes = points.map(_.pClass).toSet

    private def pointsWithFeature(feature: FeatureKind) =
      points.zip(points.map(_.features(feature)))

    def splitByFeature(feature: FeatureKind) = {
      require(featureTypes.contains(feature))
      val line = pointsWithFeature(feature).sortBy(_._2)
      splitByPred(line)(x => x._1.pClass == classes.head)
    }

    def howGood(feature: FeatureKind) = {
      val (left, mix, right) = splitByFeature(feature)
      left.size + right.size
    }

    def bestFeature = featureTypes.maxBy(howGood)

    def decart(f1: FeatureKind, f2: FeatureKind) =
      for {
        point <- points
        left = point.features(f1)
        right = point.features(f2)
      } yield (point, (left, right))

  }

  def proect(surface: Surface, alpha: Angle): DecartResult = {
    for {
      (point, (x, y)) <- surface
      newFeatureVal = y / sin(alpha) + x * cos(alpha)
    } yield (point, newFeatureVal)
  }

  def howGood(line: DecartResult) = {
    val (l, _, r) = splitByPred(line)(x => x._1.pClass == classes.head)
    l.size + r.size
  }

}