import AlphaDefs._
import DatasetUtil._

object sheet {
  
  val ds = genDataset                             //> ds  : AlphaDefs.Dataset = Dataset(Vector(Point(1,'volk), Point(2,'ovca), Poin
                                                  //| t(3,'volk), Point(4,'volk), Point(5,'ovca), Point(6,'volk), Point(7,'ovca), P
                                                  //| oint(8,'volk), Point(9,'ovca), Point(10,'ovca)),List(Feature('f1,Map(Point(10
                                                  //| ,'ovca) -> 2.0, Point(2,'ovca) -> 9.0, Point(1,'volk) -> 1.0, Point(9,'ovca) 
                                                  //| -> 2.0, Point(3,'volk) -> 0.0, Point(8,'volk) -> 2.0, Point(5,'ovca) -> 7.0, 
                                                  //| Point(7,'ovca) -> 3.0, Point(4,'volk) -> 7.0, Point(6,'volk) -> 7.0)), Featur
                                                  //| e('f2,Map(Point(10,'ovca) -> 7.0, Point(2,'ovca) -> 9.0, Point(1,'volk) -> 1.
                                                  //| 0, Point(9,'ovca) -> 6.0, Point(3,'volk) -> 5.0, Point(8,'volk) -> 8.0, Point
                                                  //| (5,'ovca) -> 2.0, Point(7,'ovca) -> 3.0, Point(4,'volk) -> 5.0, Point(6,'volk
                                                  //| ) -> 0.0)), Feature('f3,Map(Point(10,'ovca) -> 6.0, Point(2,'ovca) -> 4.0, Po
                                                  //| int(1,'volk) -> 8.0, Point(9,'ovca) -> 5.0, Point(3,'volk) -> 8.0, Point(8,'v
                                                  //| olk) -> 0.0, Point(5,'ovca) -> 9.0, Point(7,'ovca) -> 9.0, Point(4,'volk) -> 
                                                  //| 9.0, Point(6,'volk) -> 9.
                                                  //| Output exceeds cutoff limit.
	val best = ds.bestFeature                 //> best  : AlphaDefs.Feature = Feature('f1,Map(Point(10,'ovca) -> 2.0, Point(2,
                                                  //| 'ovca) -> 9.0, Point(1,'volk) -> 1.0, Point(9,'ovca) -> 2.0, Point(3,'volk) 
                                                  //| -> 0.0, Point(8,'volk) -> 2.0, Point(5,'ovca) -> 7.0, Point(7,'ovca) -> 3.0,
                                                  //|  Point(4,'volk) -> 7.0, Point(6,'volk) -> 7.0))
                              
	val other = ds.features.filterNot(_ == best).head
                                                  //> other  : AlphaDefs.Feature = Feature('f2,Map(Point(10,'ovca) -> 7.0, Point(2
                                                  //| ,'ovca) -> 9.0, Point(1,'volk) -> 1.0, Point(9,'ovca) -> 6.0, Point(3,'volk)
                                                  //|  -> 5.0, Point(8,'volk) -> 8.0, Point(5,'ovca) -> 2.0, Point(7,'ovca) -> 3.0
                                                  //| , Point(4,'volk) -> 5.0, Point(6,'volk) -> 0.0))
	
	val surf = ds.decart(best, other)         //> surf  : Seq[(AlphaDefs.Point, (AlphaDefs.FeatureValue, AlphaDefs.FeatureValu
                                                  //| e))] = Vector((Point(1,'volk),(1.0,1.0)), (Point(2,'ovca),(9.0,9.0)), (Point
                                                  //| (3,'volk),(0.0,5.0)), (Point(4,'volk),(7.0,5.0)), (Point(5,'ovca),(7.0,2.0))
                                                  //| , (Point(6,'volk),(7.0,0.0)), (Point(7,'ovca),(3.0,3.0)), (Point(8,'volk),(2
                                                  //| .0,8.0)), (Point(9,'ovca),(2.0,6.0)), (Point(10,'ovca),(2.0,7.0)))
	
	val proect1 = proect(surf, 0.3)           //> proect1  : AlphaDefs.DecartResult = Vector((Point(1,'volk),4.339199850949729
                                                  //| ), (Point(2,'ovca),39.05279865854756), (Point(3,'volk),16.919316809120616), 
                                                  //| (Point(4,'volk),23.60667223299986), (Point(5,'ovca),13.455082147527488), (Po
                                                  //| int(6,'volk),6.687355423879242), (Point(7,'ovca),13.017599552849187), (Point
                                                  //| (8,'volk),28.981579872844197), (Point(9,'ovca),22.213853149195952), (Point(1
                                                  //| 0,'ovca),25.597716511020074))
	val sum = howGood(proect1)                //> sum  : Int = 3
	
	
	
  
  
  
  
  
  

}