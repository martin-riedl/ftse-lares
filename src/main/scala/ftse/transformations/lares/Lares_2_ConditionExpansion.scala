package ftse.transformations.lares

import ftse.formalism.arith._
import ftse.formalism.set._

// import statements for logical expression
import ftse.formalism.logical._
import LE_metamodel._

import ftse.formalism.lares.LARESLogicExpr2Text
import ftse.formalism.lares.Lares2Text
import ftse.formalism.lares.LARES_metamodel._

import scala.collection.immutable._
import ftse.transformations._
/*
trait ImplLaresConditionExpansion {
	implicit def resolveCondition(l : LARES_Element) = new LaresConditionResolver(l)
}*/


class ClassLaresConditionExpansion(l : LARES_Element) extends LaresConditionExpansion {
	def resolveCondition : LARES_Element = transform(l)
}

/**
 * Condition Expansion Structures (PES)
 */
object CES {
    type BW = Iterable[Condition]
    type FW = Null
}

trait AbstrConditionExpansion {
  def resolveAuxConditionStatements(
      aux : List[ModuleStatement], 
      fwd : Iterable[(Identifier,(Identifier,LogicalExpr))], 
      resolvedLocal : Seq[Condition]
  ) : List[ModuleStatement]
}

class LaresConditionExpansion extends 
	LaresTraverser[CES.BW,CES.FW] with 
	LaresConditionExpansionHelpers with 
	Lares2Text with 
	ftse.algorithms.TopologicalOrderedProcessing with 
	AbstrConditionExpansion with 
	AbstrTransformer[LARES_Element,LARES_Element]
{

  import CES._
  
  def transform(e : LARES_Element) : LARES_Element = {println("LaresConditionExpansion");e match {
  	case SpecLares(_,_,ri : ResolvedInstance) => {
  	  val forwardresult = null
  	  val system_instance = traverse(ri,forwardresult)._1
  	  SpecLares(List(),List(),system_instance)
  	}
  }}

  
  def resolveAuxConditionStatements(
      aux : List[ModuleStatement], 
      fwd : Iterable[(Identifier,(Identifier,LogicalExpr))], 
      resolvedLocal : Seq[Condition]
  )  : List[ModuleStatement]= {
     aux map (_ match {
       case MeasureSteadyStateProbability(ns, l) => MeasureSteadyStateProbability(ns, LExpReduce.reduce(modifyLexp(l,fwd, resolvedLocal)) )
       case MeasureTransientStateProbability(ns, l, t) => MeasureTransientStateProbability(ns, LExpReduce.reduce(modifyLexp(l,fwd, resolvedLocal)), t)
       case sthelse => sthelse
     })
  }
  
  /**
   * backwards method implementation for resolving the condition expressions 
   * 
   * @param i the current instance node of the instance tree
   * @param fw the forward information 
   * @param bws the backward tuples, each comprising a (resolved) child node and the associated backward information 
   * @returns the resolved current instance and the associated backward information    
   */
    override def backwards(
		i : AbstrInstance,
		fw : FW,
		res : Iterable[(AbstrInstance,BW)]
    ) : (AbstrInstance,BW) = {
      val m = i.itype.right.get
      val localResolvedInstances = res map (_._1)
        
      // determine the visible Behavior instance State statements
      val visibleStates = m.mixins collect { case ri : ResInherit => (ri.assignedId, ri.resType.body.S) } flatMap (a=> a._2.map(b=>(a._1,b.identifier)))
      
      // determine the non-local backwarded resolved Conditions statements (including \State statements) 
      val backwardedConditions = res.flatMap(l => 
          l._2.map(c => (l._1.identifier,(c.identifier,c.le modify {
				case ResolvedReferenceCondition(is,beh,label) => ResolvedReferenceCondition(l._1.identifier :: is,beh,label)
				case sthelse => sthelse
		})))) ++ 
		(visibleStates map { ref => 
	    	(ref._1,(ref._2,ResolvedReferenceCondition(List(),ref._1,ref._2) : LogicalExpr))
	  })
	  
	  /**
	   * helper object to determine local dependencies of condition expressions
	   */
	  object GatherLocalConditionLabels extends Gather[Identifier] {
	    def gatherAtom(sth : LExpAtom) = sth match {
	      case ReferenceCondition(None,l) => Set(l)
	      case _ => Set()
	    }
	  }
      
      // all references to condition statement are resolved following a topological order for all condition statements  
   	  val localResolvedConditions = sortAndProcess[Identifier, Condition, Condition](
          m.body.conditions.toList, 
          _.identifier, 
          c => GatherLocalConditionLabels.gather(c.le),
          (c, processed) => Condition(c.identifier,modifyLexp(c.le,backwardedConditions,processed))
      )
      	  
      // all references to condition statement are resolved for all forward statements 
      val localResolvedForwards = m.body.forwarders.map(f => {
        val CRres = f.cndDests.collect({case cr : ConditionalReactive => cr}).map { cR => 
          ConditionalReactive(cR.cnd.map(e => modifyLexp(e, backwardedConditions, localResolvedConditions)),cR.reactive)          
        }
        Forward(f.name,CRres)
      })
      
      // all references to condition statement are resolved for all guards statements 
      val localResolvedGuards = m.body.guards.map(g => {
        val CG = modifyLexp(g.le, backwardedConditions, localResolvedConditions)
        
        val CRres = g.cndDests.collect({case cr : ConditionalReactive => cr}).map { cR => 
          ConditionalReactive(cR.cnd.map(e => modifyLexp(e, backwardedConditions, localResolvedConditions)), cR.reactive)
        }        
        
        LGuard(CG,CRres)                
      })
      
      // resolve all references to condition statements within auxiliary statements 
      val newAux = resolveAuxConditionStatements(m.body.auxStatements.toList, backwardedConditions, localResolvedConditions)
      
      //*****************************************************************
      // checking if things got resolved 
      localResolvedConditions.map{ c => assert(checkResolvedLexp(c.le)==true, c.le) }
      assert(checkResolvedTriggers(localResolvedForwards.toList)==true, "FWWWWWWWWWWWWWWWWWW")
      assert(checkResolvedTriggers(localResolvedGuards.toList)==true, "GWWWWWWWWWWWWWWWW")
      //******************************************************************
      
      val newBody = ModuleBody(
            List(),
			localResolvedGuards, 		// modified
	 		m.body.causes,
	 		m.body.behaviors,
	 		localResolvedInstances, // modified 
	 		List(), 	// modified
	 		localResolvedForwards, 	// modified
	 		List(),
	 		m.body.initials,
	 		newAux
		)
      
      val newInstance = ResolvedInstance(i.identifier,Module(m.identifier,m.parameters,m.mixins,newBody),i.init)		
     
      // return resolved instance and resolved local conditions
      (newInstance,localResolvedConditions)
    }
		 	  
    override def forwards (
 		  i : AbstrInstance,
 		  fw : FW
 	  ) : (AbstrInstance,FW) = {
      
      (i,null)
    }

}
	    
trait LaresConditionExpansionHelpers extends LogicExpr_algo {
  /**
   * checks whether a list of abstract triggers contains an unresolved condition reference 
   */
  def checkResolvedTriggers(l : List[AbstractTrigger]) : Boolean = 
    l.foldLeft(true)((assertion, t) => assertion & checkResolvedTrigger(t))
  
  /**
   * checks whether an abstract trigger contains a condition reference that has not been resolved yet
   */
  def checkResolvedTrigger(t : AbstractTrigger) : Boolean = t.cndDests.foldLeft(true)( (assertion, cR) =>
    assertion & cR.cnd.map(checkResolvedLexp(_)).getOrElse(true)
  )
  
  /**
   * checks whether a logical expression (on states) contains a condition reference that has not been resolved yet 
   */
  def checkResolvedLexp(l : LogicalExpr) : Boolean = l containsAtom {
    case r : ReferenceCondition => false
    case _ => true
  }
  
  def modifyLexp(lexp : LogicalExpr,fwd : Iterable[(Identifier,(Identifier,LogicalExpr))], resolvedLocal : Seq[Condition] = List()) : LogicalExpr = {
        val groupedResults = HashMap(fwd.map(a=>(a._1,a._2._1) -> a._2 ._2).toList :_*)
        val groupedLocal = HashMap(resolvedLocal.map(c=> c.identifier -> c.le ) :_*)
		
        val res = lexp modify (_ match {
			case ReferenceCondition(Some(ref),label) if (groupedResults.keySet contains((ref,label))) => {
			     println(ref,label +  " replaced by " + groupedResults((ref,label)))
			     groupedResults((ref,label))
			}
			
			case ReferenceCondition(Some(ref),label) if (!(groupedResults.keySet contains((ref,label)))) => {
			  println("WARNING: could not substitute " + lexp); ReferenceCondition(Some(ref),label)
			}

			case ReferenceCondition(None,label) if (groupedLocal.keySet contains(label)) => {
				groupedLocal(label) // take local condition				
			}
			
			case ReferenceCondition(None,label) if (!(groupedLocal.keySet contains(label))) => {
			  println("WARNING: could not substitute " + lexp); ReferenceCondition(None,label)
			}

			
			case ResolvedReferenceCondition(is,beh,label) => ResolvedReferenceCondition(is,beh,label) 

			case sthelse => println("Warning: no substitution for " + sthelse + (new ftse.formalism.lares.LARESLogicExpr2Text(sthelse)).toText);sthelse
		})
        
        assert(checkResolvedLexp(res)==true, (new LARESLogicExpr2Text(res)).toText + " could not be resolved by finding a valid substitution.")
        
        res 
	}
}
