import AlphaDefs._
import DatasetUtil._

object sheet {
  
  val ds = genDataset                             //> ds  : AlphaDefs.Dataset = Dataset(Vector(Point(Map('f1 -> 0.0, 'f2 -> 9.0, 'f
                                                  //| 3 -> 3.0),'ovca), Point(Map('f1 -> 0.0, 'f2 -> 0.0, 'f3 -> 0.0),'ovca), Point
                                                  //| (Map('f1 -> 2.0, 'f2 -> 1.0, 'f3 -> 5.0),'ovca), Point(Map('f1 -> 7.0, 'f2 ->
                                                  //|  0.0, 'f3 -> 5.0),'ovca), Point(Map('f1 -> 3.0, 'f2 -> 2.0, 'f3 -> 2.0),'volk
                                                  //| ), Point(Map('f1 -> 0.0, 'f2 -> 4.0, 'f3 -> 7.0),'volk), Point(Map('f1 -> 8.0
                                                  //| , 'f2 -> 2.0, 'f3 -> 1.0),'volk), Point(Map('f1 -> 8.0, 'f2 -> 2.0, 'f3 -> 4.
                                                  //| 0),'volk), Point(Map('f1 -> 7.0, 'f2 -> 2.0, 'f3 -> 5.0),'volk), Point(Map('f
                                                  //| 1 -> 5.0, 'f2 -> 4.0, 'f3 -> 2.0),'ovca)))
	val best = ds.bestFeature                 //> best  : AlphaDefs.FeatureKind = 'f1
	val other = ds.featureTypes.filterNot(_ == best).head
                                                  //> other  : AlphaDefs.FeatureKind = 'f2
	
	val surf = ds.decart(best, other)         //> surf  : Seq[(AlphaDefs.Point, (AlphaDefs.FeatureValue, AlphaDefs.FeatureValu
                                                  //| e))] = Vector((Point(Map('f1 -> 0.0, 'f2 -> 9.0, 'f3 -> 3.0),'ovca),(0.0,9.0
                                                  //| )), (Point(Map('f1 -> 0.0, 'f2 -> 0.0, 'f3 -> 0.0),'ovca),(0.0,0.0)), (Point
                                                  //| (Map('f1 -> 2.0, 'f2 -> 1.0, 'f3 -> 5.0),'ovca),(2.0,1.0)), (Point(Map('f1 -
                                                  //| > 7.0, 'f2 -> 0.0, 'f3 -> 5.0),'ovca),(7.0,0.0)), (Point(Map('f1 -> 3.0, 'f2
                                                  //|  -> 2.0, 'f3 -> 2.0),'volk),(3.0,2.0)), (Point(Map('f1 -> 0.0, 'f2 -> 4.0, '
                                                  //| f3 -> 7.0),'volk),(0.0,4.0)), (Point(Map('f1 -> 8.0, 'f2 -> 2.0, 'f3 -> 1.0)
                                                  //| ,'volk),(8.0,2.0)), (Point(Map('f1 -> 8.0, 'f2 -> 2.0, 'f3 -> 4.0),'volk),(8
                                                  //| .0,2.0)), (Point(Map('f1 -> 7.0, 'f2 -> 2.0, 'f3 -> 5.0),'volk),(7.0,2.0)), 
                                                  //| (Point(Map('f1 -> 5.0, 'f2 -> 4.0, 'f3 -> 2.0),'ovca),(5.0,4.0)))
	val proect1 = proect(surf, 0.3)           //> proect1  : AlphaDefs.DecartResult = Vector((Point(Map('f1 -> 0.0, 'f2 -> 9.0
                                                  //| , 'f3 -> 3.0),'ovca),30.454770256417106), (Point(Map('f1 -> 0.0, 'f2 -> 0.0,
                                                  //|  'f3 -> 0.0),'ovca),0.0), (Point(Map('f1 -> 2.0, 'f2 -> 1.0, 'f3 -> 5.0),'ov
                                                  //| ca),5.294536340075335), (Point(Map('f1 -> 7.0, 'f2 -> 0.0, 'f3 -> 5.0),'ovca
                                                  //| ),6.687355423879242), (Point(Map('f1 -> 3.0, 'f2 -> 2.0, 'f3 -> 2.0),'volk),
                                                  //| 9.633736191025065), (Point(Map('f1 -> 0.0, 'f2 -> 4.0, 'f3 -> 7.0),'volk),13
                                                  //| .535453447296492), (Point(Map('f1 -> 8.0, 'f2 -> 2.0, 'f3 -> 1.0),'volk),14.
                                                  //| 410418636653095), (Point(Map('f1 -> 8.0, 'f2 -> 2.0, 'f3 -> 4.0),'volk),14.4
                                                  //| 10418636653095), (Point(Map('f1 -> 7.0, 'f2 -> 2.0, 'f3 -> 5.0),'volk),13.45
                                                  //| 5082147527488), (Point(Map('f1 -> 5.0, 'f2 -> 4.0, 'f3 -> 2.0),'ovca),18.312
                                                  //| 135892924523))
	val sum = howGood(proect1)                //> sum  : Int = 4
	
	
  
  
  
  
  
  

}