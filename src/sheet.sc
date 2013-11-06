import AlphaDefs._
import DatasetUtil._
import scala.math._

object sheet {
	val pf: PartialFunction[Int, Int] = {case x if x %2 == 0 => x + 2}
                                                  //> pf  : PartialFunction[Int,Int] = <function1>
  

}