package ftse.transformations.laresflat
import scala.collection.immutable.HashMap
import ftse.simulation.laresflat.LFReachTypes
import ftse.simulation.laresflat.LFReachTypes._

import ftse.formalism.lares.LARES_metamodel._
import ftse.formalism.lares.LaresAlgo

import ftse.formalism.tra.Tra
import ftse.transformations.AbstrTransformer

/**
 * This Trait is used to transform the result from the reachability analysis in LFSim.reach() into an Tra-Object.
 */
trait Reach2TraTransformer extends 
	AbstrTransformer[ReachResult, (Tra, Map[ComposedState, Int])] 
	with LFReachTypes with LaresAlgo {
  
  /**
   * builds a Tra-Object from the ReachStructure from the reach() Method
   * @returns Tra-Object and a Map, which contains the mapping from the composed States to the numbered States 
   */
  override def transform(obj: ReachResult) : (Tra, Map[ComposedState,Int]) = {
    
    var statemap = HashMap(
	      // unbedingt den initialstate nach vorne schieben und mit 1 beginnen lassen
	      (obj._2.toList sortWith((a,b) => if (a==obj._3) true else false) zipWithIndex) map (a => (a._1,a._2+1))   :_* 
	  ) 
	  
	  val groupedT = obj._1.map(t => {
	    t._3 match {
	      case (name,Dirac(weight))  => {
	        val evalArith = eval(t._3._2)
	        val weight : Double = evalArith.right.getOrElse(evalArith.left.get)	       
	        ftse.formalism.tra.ImmediateTransition(statemap(t._1),name, statemap(t._2),weight): ftse.formalism.tra.Transition
	      } 
	      //case e : ExponentialTransitionType => ftse.formalism.tra.MarkovianTransition(statemap(t._1),"delay" + e.delay, statemap(t._2),e.delay.toDouble) : ftse.formalism.tra.Transition
	      case (name,Exponential(rate)) => {
	        println(name)
	        val evalArith = eval(t._3._2)
	        val rate : Double = evalArith.right.getOrElse(evalArith.left.get)
	        ftse.formalism.tra.MarkovianTransition(statemap(t._1), name, statemap(t._2),rate) : ftse.formalism.tra.Transition
	      }
	    }
	  }).groupBy(_.source)
	  
	  return (ftse.formalism.tra.Tra(groupedT ,Set[Long](statemap.values.toList.map(a => a : Long):_ *)),statemap)
  }
}