import AlphaDefs._
import scala.io.Source

object DatasetUtil {
  val r = new scala.util.Random()
  val sampleFeatures = Seq("f1", "f2", "f3")
  val sampleClasses = Seq("volk", "ovca");
  val N = 10;
  val M = 10

  def rand[T](seq: Seq[T]) = seq(r.nextInt(seq.size))

  def splitByPred[T](seq: Seq[T])(f: T => Boolean) = {

    val leftClass = f(seq.head)
    val rightClass = !leftClass

    val (leftGood, other) = seq span (f(_) == leftClass)
    val (mixed, rightGood) = {
      val rightIndex = other.lastIndexWhere(f(_) == leftClass)
      other.splitAt(rightIndex + 1);
    }

    (leftGood, mixed, rightGood)
  }

  def genDataset = {
    val points = for {
      id <- 1 to N
      pclass = rand(sampleClasses)
    } yield Point(id, pclass)

    val features = for {
      kind <- sampleFeatures
      fMap = (for {
        point <- points
        value = r.nextInt(M) toDouble
      } yield point -> value) toMap
    } yield Feature(SimpleName(kind), fMap)

    Dataset(points, features)
  }

  def probeAngle(f: Angle => Int) = {
    val N = 100
    (for {
      probe <- 0 to N
      angle = 2 * scala.math.Pi / N * probe
      res = f(angle)
    } yield (angle, res)).maxBy(_._2)
  }

  def readDataset(name: String) = {
    val file = Source.fromFile(name)
    val lines = file.getLines.toStream;
    val pointWithValues = for {
      (line, number) <- lines.zipWithIndex
      parts = line.split(" ").toSeq
      featureStrings :+ pClass = parts
      featureValues = featureStrings.map(_.toDouble)
      point = Point(number, pClass)
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
      case (i, map) => Feature(SimpleName(s"feature${i+1}"), map)
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
    } else {
      x
    }
  }

  def proect(surface: Surface, alpha: Angle): DecartResult = {
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
          val proection = proect(decart, angle).sortBy(_._2)
          howGood(proection)
        })
        
        val resKind = ComposedName(best.kind, secondBest.kind)
        val newFeature = Feature(resKind, proect(decart, proectionAngle).toMap)

        recur(updDS, newFeature, acc :+ (newFeature.kind, proectionAngle, res));
      } else {
    	acc
      }
    }
    val best = dataset.bestFeature
    recur(dataset.without(best), best, Seq())
  }
}