package org.ygeryatovich.alphaprocedure
import AlphaDefs._
import me.iamzsx.scala.svm.SVMParameter
import me.iamzsx.scala.svm.SVMProblem
import scala.io.Source
import me.iamzsx.scala.svm.LinearKernel
import me.iamzsx.scala.svm.Solver
import me.iamzsx.scala.svm.SVMNode
import me.iamzsx.scala.svm.Instance
import me.iamzsx.scala.svm.Solution
import me.iamzsx.scala.svm.OneClassModel
import me.iamzsx.scala.svm.SVM
import libsvm.svm_problem
import libsvm.svm_parameter
import libsvm.svm_node
import libsvm.svm_model
import libsvm.svm

case class MySVM(ds: Dataset) {

  lazy val (param, problem) = {

    val param = new svm_parameter();

    // default values
    param.svm_type = svm_parameter.ONE_CLASS;
    param.kernel_type = svm_parameter.RBF;
    param.degree = 3;
    param.gamma = 0.5;
    param.coef0 = 0;
    param.nu = 0.5;
    param.cache_size = 40;
    param.C = 1;
    param.eps = 1e-3;
    param.p = 0.1;
    param.shrinking = 1;
    param.probability = 0;
    param.nr_weight = 0;
    param.weight_label = new Array[Int](0);
    param.weight = new Array[Double](0);

    val problem = new svm_problem
    problem.l = ds.points.size
    problem.y = Array.ofDim[Double](problem.l)
    problem.x = Array.ofDim[svm_node](problem.l, ds.features.size)
    for{
      i <- 0 until problem.l
      point = ds.points(i)
      _ = problem.y(i) = point.pClass.toInt
      j <- 0 until ds.features.size
      Feature(kind, map) = ds.features(j)
      fid = kind.toString().toInt
    }{
      val node = new svm_node
      node.index = fid
      node.value = map(point)
      problem.x(i)(j) = node
    }
    
    
    
    //    val param = new SVMParameter(LinearKernel, 0.9)
    //    var maxIndex = 0
    //    val instances = for {
    //      point <- ds.points
    //      Point(id, pclass) = point
    //      y = pclass.toDouble
    //      x = (for {
    //        Feature(kind, featureMap) <- ds.features
    //        index = kind.toString.toInt
    //        _ = maxIndex = maxIndex max index
    //        value = featureMap(point)
    //      } yield SVMNode(index, value)).toList
    //    } yield Instance(x, y)
    //
    //    if (param.gamma == 0 && maxIndex > 0) {
    //      param.gamma = 1.0 / maxIndex
    //    }
    //
    //    (param, new SVMProblem(instances.toArray))
    (param, problem)
  }

  def solve = {
    //    val solution: Solution = Solver.solveOneClass(problem, param)
	val model = svm.svm_train(problem, param)
	
	model.obj
  }

}

object MySVM extends App {
//  val ds = DatasetUtil.readDataset("/home/yarik/KPI/germany/inside/ds/my.dat")
  val ds = DatasetUtil.readDataset("/home/yarik/KPI/germany/inside/ds/crabO_MvsF.dat")
  new MySVM(ds).solve

}