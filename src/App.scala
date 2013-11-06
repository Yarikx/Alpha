import scala.Int.int2double

import AlphaDefs.Dataset
import AlphaDefs.howGood
import DatasetUtil._

object MyDebugApp extends App {
  
  val ds = readDataset("/tmp/ds/crabO_MvsF.dat")
  
  val angles = solveDataset(ds)
  
  angles foreach println
  
}