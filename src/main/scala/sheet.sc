import AlphaDefs._
import DatasetUtil._
import scala.math._

object sheet {
	val ds = readDataset("/tmp/ds/crabO_MvsF.dat")
                                                  //> ds  : AlphaDefs.Dataset = Dataset(Stream(Point(0,3), Point(1,3), Point(2,3),
                                                  //|  Point(3,3), Point(4,3), Point(5,3), Point(6,3), Point(7,3), Point(8,3), Poi
                                                  //| nt(9,3), Point(10,3), Point(11,3), Point(12,3), Point(13,3), Point(14,3), Po
                                                  //| int(15,3), Point(16,3), Point(17,3), Point(18,3), Point(19,3), Point(20,3), 
                                                  //| Point(21,3), Point(22,3), Point(23,3), Point(24,3), Point(25,3), Point(26,3)
                                                  //| , Point(27,3), Point(28,3), Point(29,3), Point(30,3), Point(31,3), Point(32,
                                                  //| 3), Point(33,3), Point(34,3), Point(35,3), Point(36,3), Point(37,3), Point(3
                                                  //| 8,3), Point(39,3), Point(40,3), Point(41,3), Point(42,3), Point(43,3), Point
                                                  //| (44,3), Point(45,3), Point(46,3), Point(47,3), Point(48,3), Point(49,3), Poi
                                                  //| nt(50,4), Point(51,4), Point(52,4), Point(53,4), Point(54,4), Point(55,4), P
                                                  //| oint(56,4), Point(57,4), Point(58,4), Point(59,4), Point(60,4), Point(61,4),
                                                  //|  Point(62,4), Point(63,4), Point(64,4), Point(65,4), Point(66,4), Point(67,4
                                                  //| ), Point(68,4), Point(69
                                                  //| Output exceeds cutoff limit.
	
	val (good, bad) = removeRandomPoints(ds, 20)
                                                  //> good  : AlphaDefs.Dataset = Dataset(Stream(Point(1,3), Point(2,3), Point(4,3
                                                  //| ), Point(5,3), Point(6,3), Point(8,3), Point(9,3), Point(10,3), Point(11,3),
                                                  //|  Point(12,3), Point(14,3), Point(16,3), Point(17,3), Point(19,3), Point(20,3
                                                  //| ), Point(21,3), Point(23,3), Point(24,3), Point(25,3), Point(26,3), Point(27
                                                  //| ,3), Point(28,3), Point(29,3), Point(30,3), Point(31,3), Point(32,3), Point(
                                                  //| 33,3), Point(34,3), Point(35,3), Point(36,3), Point(37,3), Point(38,3), Poin
                                                  //| t(39,3), Point(41,3), Point(42,3), Point(43,3), Point(45,3), Point(46,3), Po
                                                  //| int(47,3), Point(48,3), Point(49,3), Point(50,4), Point(51,4), Point(52,4), 
                                                  //| Point(53,4), Point(54,4), Point(55,4), Point(56,4), Point(58,4), Point(59,4)
                                                  //| , Point(60,4), Point(61,4), Point(62,4), Point(63,4), Point(64,4), Point(65,
                                                  //| 4), Point(66,4), Point(67,4), Point(68,4), Point(69,4), Point(70,4), Point(7
                                                  //| 4,4), Point(75,4), Point(76,4), Point(77,4), Point(79,4), Point(80,4), Point
                                                  //| (81,4), Point(82,4), Poi
                                                  //| Output exceeds cutoff limit.
	
	good.points.size                          //> res0: Int = 80
	bad.points.size                           //> res1: Int = 20
  
  

}