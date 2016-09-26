package ftse.formalism.lares.flat

import ftse.formalism.lares.LARES_metamodel._
import ftse.formalism.logical._

import scala.collection.immutable.Stack

abstract class LaresFlat 

/**
 * LFA (LARES Flat Automata)
 */
case class LFA(
    name : List[Identifier], 
    states : Set[State], 
    initial : State, 
    unguarded : List[UnguardedTransition], 
    guarded : List[GuardedTransition]
) extends LaresFlat 

/**
 * LFIA (LARES Flat Interacting Automata)
 */
case class LFIA(
    name : Identifier, 
    measures : List[Measure], 
    iacts : List[InteractionalElement], 
    lfas : List[LFA]
) extends LaresFlat

import scala.collection.immutable._


/**
 * LFA (LARES Flat Automata FAST)
 * 
 * The FAST implementation for simulation purposes
 */ 
abstract class LaresFlatFast 

import ftse.formalism.tra._
import scala.collection.mutable.{Stack => MStack, Set => MSet, LinkedList => MList}

object FastTypes {
  type LFA = Int
  type State = Int
  type GuardLabel = Int
  type Value = Double
  type FullState = (Int,State)
  type FullGuardLabel = (Int,GuardLabel)
  type GenerativePT = (Set[FullState],List[List[(FullState,Boolean)]])
  type ReactivePT = (Set[FullGuardLabel],List[List[(FullGuardLabel,Boolean)]])
}

case class LFAFast(
    name : List[Identifier], 
    states : Map[FastTypes.State,State], 
    mapStateIndex : HashMap[State, FastTypes.State], 
    guards : Map[FastTypes.GuardLabel, Identifier],
    mapGuardIndex : HashMap[Identifier, FastTypes.GuardLabel], 
    initial : FastTypes.State, 
    unguarded : Map[FastTypes.State, List[(FastTypes.State, FastTypes.State, (DistributionType, FastTypes.Value))]], 
    guarded : Map[FastTypes.State, Map[FastTypes.GuardLabel, List[(FastTypes.State, FastTypes.State, FastTypes.GuardLabel, FastTypes.Value)]]]
) extends LaresFlatFast

case class LFIAFast(
    name : Identifier, 
    measures : List[Measure], 
    tgs : List[(FastTypes.GenerativePT, FastTypes.ReactivePT, DistributionType)], 
    val lfas : Map[Int,LFAFast]
) extends LaresFlatFast


import ftse.transformations.AbstrTransformer

/** 
 *  LaresFlat Transformation to a Representation which allows a FAST execution  
 */ 
class MakeLFfast extends AbstrTransformer[LaresFlat,LFIAFast] {
    def transform(lf : LaresFlat) = lf match {
      case LFIA(name, measures, iacs, lfas) => 
        
        val fastlfas = lfas.map { lfa => 
          val mapStateIndex = HashMap(lfa.states.zipWithIndex.toSeq :_*)
          val mapIndexState = HashMap(mapStateIndex.view.map(t => (t._2,t._1)).force.toSeq :_*)
          val mapGuardIndex = HashMap(lfa.guarded.map(t => t.guard).toSet.zipWithIndex.toSeq :_*)
          val mapIndexGuard = HashMap(mapGuardIndex.view.map(t => (t._2,t._1)).force.toSeq :_*)
          
          val initial = mapStateIndex(lfa.initial)
          
          import ftse.formalism.arith.ArithExpr_metamodel._
          
          val unguarded = lfa.unguarded.map(unguarded =>
            (
                mapStateIndex(unguarded.from), 
                    mapStateIndex(unguarded.to), 
	                unguarded.distr match {
	                  case Some(Exponential(ArithAtomValue(Right(value)))) => (ExponentialType, value)
	                  case Some(Dirac(ArithAtomValue(Right(value)))) => (DiracType, value)
	                  case _ => (DiracType, 1.0)
	                }
            )
          ).groupBy(_._1)
          
          val guarded = lfa.guarded.map(guarded =>
            (
                mapStateIndex(guarded.from), 
                mapStateIndex(guarded.to), 
                mapGuardIndex(guarded.guard),
                guarded.distr match {
                  case Some(Exponential(ArithAtomValue(Right(value)))) => value
                  case _ => 1.0
                }
            )
          ).groupBy(_._1).map(g => (g._1,g._2.groupBy(_._3)))
          
          LFAFast(lfa.name, mapIndexState, mapStateIndex, mapIndexGuard, mapGuardIndex, initial, unguarded, guarded)
        }
        
        // access an LFA via its index
        val mapIndexLFA = HashMap(fastlfas.zipWithIndex.view.map(t => (t._2, t._1)).force.toSeq :_*)
        
        // retrieve the index of an LFA via its name
        val mapNameIndex = HashMap(mapIndexLFA.map(t => (t._2.name, t._1)).toSeq :_*)
        
        def makeIndex(a: ftse.formalism.logical.LE_metamodel.LExpAtom) : FastTypes.FullState = a match {
            case ResolvedReferenceCondition(m,b,s) => {
              val nameIndex = mapNameIndex(m ::: List(b))
              val lfa = mapIndexLFA(nameIndex)
              val stateIndex = lfa.mapStateIndex(State(s))
              (nameIndex, stateIndex)
            }    
            
            case ResolvedReferenceGuardLabel(m,b,t,s) => {
              val nameIndex = mapNameIndex(m ::: List(b))
              val lfa = mapIndexLFA(nameIndex)
              val stateIndex = lfa.mapGuardIndex(s)
              (nameIndex, stateIndex)
            } 
        }
        
        val interactionelements = iacs.flatMap(_ match {
          case tg : TypedGuard => 
            tg.cndDests.map(cd => 
              
              (cd.cnd.map(_ & tg.le).getOrElse(tg.le) , cd.reactive, cd.distr)) map { cd => val (g,r,dt) = cd
              
              val (lstV, lstPT) = (new LogicExprBdd(cd._1)).satMinTerms()
              val lstVmod = lstV.map(makeIndex(_))
              val lstPTmod = lstPT.map(_.map(a => (makeIndex(a._1), a._2)))
              
              val cnd : FastTypes.GenerativePT = (lstVmod.toSet, lstPTmod)
              
              val (lstV2, lstPT2) = (new LogicExprBdd(cd._2)).satMinTerms()
              val lstVmod2 = lstV2.map(makeIndex(_))
              val lstPTmod2 = lstPT2.map(_.map(a => (makeIndex(a._1), a._2)))
              
              val rct : FastTypes.ReactivePT = (lstVmod2.toSet, lstPTmod2)
              
              (cnd, rct, dt)
            } 
            
        })
		    
      	LFIAFast(name, List(), interactionelements, mapIndexLFA)
    }
} 

// LARES Reachability Structure Types
object LRSTypes {
  type ComposedState = Map[Int,ftse.formalism.lares.flat.FastTypes.State]
  type LRS = MStack[(ComposedState,ComposedState,(String,DistributionType, Double))]   
}


import ftse.formalism.lares.Lares2Text
class LRSSerializer extends AbstrTransformer[(Set[LRSTypes.ComposedState],LRSTypes.LRS, LFIAFast), String] with Lares2Text {
  def transform(rs : (Set[LRSTypes.ComposedState],LRSTypes.LRS, LFIAFast)) = {
    val (states, lrs, lrsfast) = rs 
    val x = HashMap(states.toSeq.zipWithIndex :_*)
    
    val edges = lrs map { t =>
      x(t._1) +  " -> " + x(t._2)  + "[" + "label=" + t._3._3 + "style=" + (if (t._3._2==ExponentialType) "solid" else "dashed") + "]"
    }        
    
    val legend = "legend" + " [label= "+ states.head.map{cs => 
      lrsfast.lfas(cs._1).name.map(n => n.toText).mkString(".")
    }.mkString("\"","|","\"") +" shape=\"record\"]"
      
    val nodes = x map { s => 
      s._2 + "[ label=\""+ s._1.map{ cs => 
       val lfa = lrsfast.lfas(cs._1)
       val state  = lfa.states(cs._2)
       state.identifier.toText  //cs._2
      }.mkString("|")  +"\"\n shape=\"record\"]"
    }
    
    val dot = "digraph g {\n" + legend + edges.mkString("\n","\n","\n") + nodes.mkString("\n","\n","\n") + "}" 
    
    import ftse.tools.Scripting._
    
    showDot(dot)
    ftse.tools.Scripting.pipeInputTo(dot,"gedit -")
    dot
  }
}

class DoReach extends AbstrTransformer[LaresFlatFast,(Set[LRSTypes.ComposedState],LRSTypes.LRS)] {
  import LRSTypes._
  
  /**
   * performs reachability analysis of a LaresFlatFast model
   */
  def transform(lf : LaresFlatFast) : (Set[ComposedState],LRS) = lf match {
    case LFIAFast(name, measures, iacs, lfas) =>
      
      
      val initialstate = lfas.map(lfa => (lfa._1, lfa._2.initial)).toSeq
      val currentstate = Map(initialstate :_*)

      val todo = MStack(currentstate) // insert initial state
	  val done = MSet[ComposedState]() // nothing done yet
	  
	  // create a new ReachStructure
	  val rs = new LRS()

  	  while (todo.nonEmpty) {
  	    val current = todo.pop() 
  	    done += current
  	    
  	    println("current is now "+ current)  	    
  	    
  	    // interaction induced transitions
  	    for (iac <- iacs 
  	        // first criteria: a condition product term of a conditional reactive is fulfilled by the current state 
  	        if iac._1._2.exists(pt => pt.forall(l =>
  	        	if (l._2) current(l._1._1)==l._1._2 else current(l._1._1)!=l._1._2
  	        )) 
  	    ) {
  	      
  	      
  	      // second criteria: a reactive product term of conditional reactive is fulfilled by the current state
  	      val satRPTs = iac._2._2.find(pt => pt.forall{ l =>
  	        val lfa = lfas(l._1._1) // get the LFA matching to the literal 
  	        val ccs = current(l._1._1) // determine the LFA's current component state
  	        
  	        // determine the outgoing transitions corresponding to the given literal
  	        val satLabel = lfa.guarded.get(ccs).map(_.contains(l._1._2)).getOrElse(false)
  	        if (l._2) satLabel else !satLabel 
  	      }) 
  	      
  	      // construct new current states
  	      
  	      satRPTs foreach {satRPT =>
  	        val compT = for (l <- satRPT.filter(_._2)) yield { // only the TRUE literals of a satPT are relevant
  	          val lfa = lfas(l._1._1) // get the LFA matching to the literal 
  	          val ccs = current(l._1._1) // determine the LFA's current component state
  	          lfa.guarded.get(ccs).flatMap(_.get(l._1._2)).getOrElse(List()).map(a => (l._1._1, a))
  	        }
  	        
  	        import ftse.algorithms.SetTheory._
  	        val CT = xproduct(compT)
  	        CT.foreach { ct =>
  	          val delta = ct.map(t => (t._1 -> t._2._2))
  	          val newcurrent = current ++ delta
  	          
  	          val value = ct.map(t => t._2._4).reduceLeft(_+_)
  	          rs.push((current, newcurrent, ("", iac._3, value)))
  	          
  	          if (!done.contains(newcurrent)) todo.push(newcurrent)
  	        }
  	      }
  	    }
  	    
  	    // unguarded transitions 
  	    current foreach { t => val (bi,s) = t
  	      lfas(bi).unguarded.get(s).getOrElse(List()) foreach { ugs => val (source, dest, (dt,value)) = ugs 
  	        val newcurrent = current + (bi -> dest)
  	        
  	        rs.push((current, newcurrent, ("", dt, value)))
  	        if (!done.contains(newcurrent)) todo.push(newcurrent)
  	      }  	      
  	    }
  	    
  	      	    
  	  }
      
      (done.toSet,rs)      
  }
}
