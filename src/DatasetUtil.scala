import AlphaDefs._

object DatasetUtil {
  val r = new scala.util.Random()
  val features = Seq('f1, 'f2, 'f3)
  val classes = Seq('volk, 'ovca);
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

  def genDataset = Dataset(for {
    i <- 1 to N
    c = rand(classes)
    fs = (for {
      featureKind <- features
      fv = r.nextInt(M) toDouble
    } yield (featureKind, fv)).toMap
  } yield Point(fs, c))

//  def prettyDataset(dataset: Dataset) = {
//    implicit def symb2string(x: Symbol) = x.toString.substring(1)
//
//    import scala.math.max
//
//    val points = dataset.points;
//    val kinds = dataset.featureTypes
//    val kindsStrings = kinds map symb2string
//    val classes = dataset.classes map symb2string
//
//    val pointsSize = classes.map(_.length).max
//    val featuresWithSizes = for {
//      kind <- kinds
//      maxSize = (for {
//        point <- points
//        feature = point.features(kind)
//      } yield feature.toString.length) max
//    } yield (kind, maxSize)
//    val classSize = classes.map(_.length).max
//
//    for {
//      i <- 1 to points.size
//      Point(map, cl) = points(i)
//
//    } {}
//
//  }
}