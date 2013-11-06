import DatasetUtil._
import scala.math._

object AlphaDefs {
  type FeatureKind = FeatureName
  type FeatureValue = Double
  type PointClass = String
  type PointId = Int
  case class Point(id: PointId, pClass: PointClass)
  case class Feature(kind: FeatureKind, featureMap: Map[Point, FeatureValue])

  type Surface = Seq[(Point, (FeatureValue, FeatureValue))]
  type Angle = Double
  type DecartResult = Seq[(Point, FeatureValue)]
  
  sealed abstract class FeatureName{
    def second: String
  }
  case class SimpleName(s: String) extends FeatureName{
    override def toString = s
    def second = s
  }
  case class ComposedName(f1: FeatureName, f2:FeatureName) extends FeatureName{
    override def toString = s"${f1.second} -> ${f2.second}"
    def second = f2.second
  }

  case class Dataset(points: Seq[Point], features: Seq[Feature]) {

    val featureTypes = features.map(_.kind)
    val classes = points.map(_.pClass).toSet
    val featureMap = (for {
      Feature(kind, values) <- features
    } yield kind -> values) toMap

    require(classes.size == 2, "number of classes must be 2")

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

    lazy val bestFeature = features.maxBy(howGood)

    def decart(f1: Feature, f2: Feature) =
      for {
        point <- points
        left = f1.featureMap(point)
        right = f2.featureMap(point)
      } yield (point, (left, right))
      
   def without(feature: Feature) = 
     copy(features = this.features.filterNot(_ == feature))

  }

  def howGood(line: DecartResult) = {
    val firstClass = line.head._1.pClass
    val (l, _, r) = splitByPred(line)(x => x._1.pClass == firstClass)
    l.size + r.size
  }

  def produceFeature(kind: FeatureKind, proection: DecartResult) =
    Feature(kind, proection.toMap)

}