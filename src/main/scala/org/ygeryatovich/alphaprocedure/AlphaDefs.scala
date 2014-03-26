package org.ygeryatovich.alphaprocedure

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

  sealed abstract class FeatureName
  case class SimpleName(s: String) extends FeatureName{
    override def toString = s
  }
  case class ComposedName(f1: Feature, f2: Feature) extends FeatureName

  case class Dataset(points: Seq[Point], features: Seq[Feature]) {
    lazy val featureTypes = features.map(_.kind)
    lazy val classes = points.map(_.pClass).toSet
    lazy val featureMap = (for {
      feature <- features
      kind = feature.kind
    } yield kind -> feature).toMap

    lazy val bestFeature = features.minBy(howBad)

    private def pointsWithFeature(feature: Feature) =
      points.zip(points.map(feature.featureMap(_)))

    def splitByFeature(feature: Feature) = {
      val line = pointsWithFeature(feature).sortBy(_._2)
      splitBy(line)(x => x._1.pClass == classes.head)
    }

    def howBad(feature: Feature) = {
      val (left, mix, right) = splitByFeature(feature)
      mix.size
    }

    def decart(f1: Feature, f2: Feature) =
      for {
        point <- points
        left = f1.featureMap(point)
        right = f2.featureMap(point)
      } yield (point, (left, right))

    def without(feature: Feature) =
      copy(features = this.features.filterNot(_ == feature))

    def filterPoints(f: Point => Boolean) = {
      val newPoints = points filter f
      val newFeatures = for {
        Feature(kind, pMap) <- features
        updMap = pMap.filter { case (p, _) => f(p) }
      } yield Feature(kind, updMap)
      Dataset(newPoints, newFeatures)
    }

    def buildBestFeature(angles: Seq[(Angle, FeatureKind)], startFeatureKind: FeatureKind) =
      angles.foldLeft(featureMap(startFeatureKind)) { (firstFeature, x) =>
        val (angle, secondKind) = x
        val secondFeature = featureMap(secondKind)
        val dec = decart(firstFeature, secondFeature)
        val proection = proectPoints(dec, angle)
        val newFeature = Feature(ComposedName(firstFeature, secondFeature), proection.toMap)
        newFeature
      }

    def buildAllFeatures(angles: Seq[(Angle, FeatureKind)], firstFeature: Feature): Seq[(Surface, Angle)] = {
      angles match {
        case x +: tail =>
          val (angle, secondKind) = x
          val secondFeature = featureMap(secondKind)
          val dec = decart(firstFeature, secondFeature)
          val proection = proectPoints(dec, angle)
          val newFeature = Feature(ComposedName(firstFeature, secondFeature), proection.toMap)
          (dec, angle) +: buildAllFeatures(tail, newFeature)
        case Seq() => Seq()
      }

    }

  }

}