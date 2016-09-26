package ftse.formalism.lares.flat

import ftse.formalism.lares._
import ftse.formalism.lares.LARES_metamodel._
import ftse.simulation.laresflat.LFReachTypes
import scala.collection.immutable.HashMap
import ftse.simulation.laresflat.LFReachTypes._
import ftse.transformations.AbstrTransformer

/** Provides an implicit serialization function to a LARESflat model */
trait Flat2Text {
	implicit def l2t(e : LaresFlat) = new Flat2T(e)
}

/**
 * Wrapper class that provides a method to serialize a LARESflat model to its text representation
 * 
 * @param e A LARESflat model
 */
class Flat2T(e : LaresFlat) extends TFlat2Text {
  /**
   * Serializes a LARESflat model
   */
  def toText : String = Flat2Text(e)
}

/**
 * Serializes a LARESflat model 
 */
trait TFlat2Text extends CLares2Text {
  /** 
   * Serializes a LARES flat model
   * 
   * @param e A LARESflat model
   * @return Textual representation of the LARESflat model 
   */ 
  def Flat2Text(e : LaresFlat) : String = e match {
    case LFA(ns, states, initial, unguarded, guarded) => "  BehaviorInstance " + ns.map(_.toText).mkString(".") + ":\n" ++ 
    											states.map(s => if (s==initial) "initial "+toText(s) else toText(s)).mkString("    ","\n    ","\n") ++
    											guarded.map(t => "Guarded " + t.from.identifier.toText + " " + toText(t)).mkString("    ","\n    ","\n") ++
    											unguarded.map(t => "Unguarded " + t.from.identifier.toText + " " + toText(t)).mkString("    ","\n    ","\n")
    											
    case LFIA(name, measures, guards, lfas) => "System " + name.toText + ":\n" ++ 
    								 measures/*.map(toText(_))*/.mkString("  ","\n  ","\n") ++
    								 guards.map(toText(_,2)).mkString("\n","\n","\n") ++
    								 lfas.map(Flat2Text(_)).mkString("\n","\n","\n")
  } 
} 

/**
 * This trait is used as a container for the reachToDot method.
 */
trait Flat2Dot extends AbstrTransformer[ReachResult, String] with LFReachTypes with LaresAlgo with Lares2Text {
  
  /**
   * generates a String that represents the model as a dot output (extracted from LFSim)
   */
  override def transform(obj: ReachResult) : String =  {
	  var statemap = HashMap(
	      // unbedingt den initialstate nach vorne schieben und mit 1 beginnen lassen
	      (obj._2.toList sortWith((a,b) => if (a==obj._3) true else false) zipWithIndex) map (a => (a._1,a._2+1))   :_* 
	  ) 
	  
	  val dotTrans = obj._1.map(e => statemap(e._1) + "->" + statemap(e._2) + "["+(e._3 match {
	    		  case (name,Exponential(rate)) => "label=\"("+name+",rate="+eval(e._3._2)+")\"" + " fontcolor=black color=black"
	    		  case (name,Dirac(weight)) => "label=\"(*"+name+",rate="+eval(e._3._2)+"*)\"" + " fontcolor=blue color=blue"
	    		      
			      case _ => ""	      
	  })+"]")
	  
	  val statesDot = statemap.map(a=> a._2 + " "+a._1.values.map(_.identifier.toText).mkString("[style=filled shape=\"record\" label=\"{","|","}\"]") )
	  val resstring = dotTrans.foldLeft(
	      "digraph g {ratio=1.33;node [height=\"2\" width=\"3\" shape = \"ellipse\"]; edge []; graph [nodesep=0.5];"
	      )((a,b)=>a+"\n  "+b)+statesDot.mkString("\n  ","\n  ","\n")+"}"
	  //println(resstring)

	  resstring    
  }
}