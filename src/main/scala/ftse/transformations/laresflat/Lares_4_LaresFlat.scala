package ftse.transformations.laresflat


import ftse.formalism.lares.LARES_metamodel._
import ftse.formalism.lares.flat._
import ftse.formalism.arith._
import ftse.formalism.logical._
import ftse.formalism.set._

import ftse.transformations.lares._
import ftse.transformations._

import scala.collection.immutable._

/**
 * Lares Flat Expansion Structures (PES)
 */
object LFES {
	type FW = (List[Identifier],List[(List[Identifier],Int,LGuard)])
	type BW = (List[LFA],List[Guard],List[Measure])
}

/**
 * LARES to LARESflat transformation base class
 */
class L2LFGen extends 
	AbstrTransformer[LARES_Element,LFIA] with 
	LaresTraverser[LFES.BW,LFES.FW] with
	AbstrL2LF 	
{
  /**
   * root transformation method implementation
   * @param e The LARES specification root node
   * @return LARESflat Interacting Automata (LFIA)  
   */
  def transform(e : LARES_Element) : LFIA = e match {
    case SpecLares(_,_,system : ResolvedInstance) => {
      val forwardresult = (List[Identifier](),List[(List[Identifier],Int, LGuard)]())
      println("SYSTEM " + system)
      val result = traverse(system,forwardresult)
      LFIA(result._1.identifier, result._2._3, result._2._2,result._2._1)
	}
  }

  /**
   * Adapts the namespace inside generative/reactive expressions references
   * by prefixing the given parameter ns
   * 
   * @param ns Namespace to be prefixed
   * @param le Generative/Reactive expression
   * @return Modfified Generative/Reactive expression   
   */
  def modifyReactive(ns : List[Identifier], le : LogicalExpr) : LogicalExpr = le modify {
	case ResolvedReferenceGuardLabel(namespace,b,d,n) => ResolvedReferenceGuardLabel(ns ::: namespace,b,d,n)
	case r: ReferenceGuardLabel => assert(false, "found an unresolved reference to a guard label " + r); r
	case sthelse => println(ns, le);sthelse
  }


  /**
   * Processes the measure statements by prefixing the instance identifier to the current name and adapting the reactive 
   * @param ns current namespace
   * @param i An abstract instance
   * @param aux List of auxiliary statements
   * @return List of Measure statements 
   */
  def processMeasure(ns : List[Identifier], i : AbstrInstance, aux : List[ModuleStatement]) : List[Measure] = {
    aux collect {
      case MeasureSteadyStateProbability(name, l) => MeasureSteadyStateProbability(i.identifier :: name, modifyGenerative(ns,l)) 
      case MeasureTransientStateProbability(name, l, t) => MeasureTransientStateProbability(i.identifier :: name, modifyGenerative(ns,l), t) 
	}
  }
  
  /**
   * Collect all typed guards from list of auxiliary statements and adapt their namespace 
   * @param ns current namespace
   * @param i An abstract instance
   * @param aux List of auxiliary statements
   * @return List of Guards statement 
   */
  def processGuards(ns : List[Identifier], i : AbstrInstance, aux : List[ModuleStatement]) : List[Guard] = {
    aux collect {
		  case TypedGuard(le,dest) => 
		  	TypedGuard(
		  	    modifyGenerative(ns,le),
		  	    dest map { cndReact =>  
		  	      TypedConditionalReactive(
		  	          cndReact.cnd map (modifyGenerative(ns,_)),
		  	          modifyReactive(ns, cndReact.reactive), 
		  	          cndReact.distr
		  	      )
		  	    }
		  	)
		}  
  }
	
  import LFES._
  
  /**
   * forwards method implementation of the LARESflat transformation 
   * @param i the processed module instance
   * @param fw the forward information for the processed module instance 
   * @return the forward information tuple for the processed instance
   */
  override def forwards(
      i	: AbstrInstance,
      fw	: FW
  ) : (AbstrInstance,FW) = {
    val m = i.itype.right.get
    val guards = m.body.guards.toList
    
    // update namespace
    val newnamespace = fw._1 :+ i.identifier
    
    // adapt the namespaces of the guards statements 
    val newguards = guards.zipWithIndex.map(g=> 
      (newnamespace,g._2,LGuard(
          g._1.le modify {
            case ReferenceCondition(Some(beh),label) => ResolvedReferenceCondition(List(newnamespace :_*),beh,label)
            case ResolvedReferenceCondition(ns,beh,label) => ResolvedReferenceCondition(ns ::: newnamespace, beh,label)
            case sthelse => assert(true);sthelse
          },
          g._1.cndDests map { cndReact => 
            ConditionalReactive(
                cndReact.cnd map { _ modify {
	              case ReferenceCondition(Some(beh),label) => ResolvedReferenceCondition(List(newnamespace :_*),beh,label)
	              case ResolvedReferenceCondition(ns,beh,label) => ResolvedReferenceCondition(ns ::: newnamespace, beh,label)
	              case sthelse => assert(true);sthelse
	            }},
	            cndReact.reactive modify {
	              case ResolvedReferenceGuardLabel(ns,beh,distrType,label) => ResolvedReferenceGuardLabel(ns ::: newnamespace, beh,distrType,label)
	              //TODO case ReferenceGuardLabel(Some(beh),label) => ResolvedReferenceGuardLabel(Stack(newnamespace :_*), beh,label)
	              case sthelse => assert(true);sthelse
	            })
            })
      )
    )
		
    
    val fwold = fw._2 /*map(g => (g._1,g._2,LGuard(g._3.le, g._3.destinations.filter(_ match {
			case ResolvedReferenceGuardLabel(ns,beh,label) => ns startsWith newnamespace
		})))) */
    
    // construct forward information for the processed instance 
    (i,(newnamespace, fwold ::: newguards))
  }
  
  /**
   * Backwards method implementation for the LARESflat transformation
   * @param
   * @param 
   * @param
   * @return backward tuple comprising ... 
   */
  override def backwards(
		i	: AbstrInstance,
		fw	: FW,
		bws	: Iterable[(AbstrInstance,BW)]
	) : (AbstrInstance,BW) = {
        val m = i.itype.right.get
		val (ns, guards) = fw

        // collect the backwarded information from the child instances
		val child_lfas  = bws.flatMap(_._2._1).toList
		val child_guards  = bws.flatMap(_._2._2).toList
		val child_measures  = bws.flatMap(_._2._3).toList

		// perform the translation of all instantiated behaviours
		val translatedMixinBeh = m.mixins.toList.collect {
		  case r : ResInherit => t_Behavior(
		    		r.assignedId, 
					r.resType,
					fw
			)
        }
        // process the guards and measures of the processed module instance
		val translatedGuards = processGuards(ns, i, m.body.guards.toList ++ m.body.auxStatements) 
		val translatedMeasures = processMeasure(ns, i, m.body.auxStatements.toList) 
		
		
		// aggregate that information as a backward tuple 
		(i,(child_lfas ::: translatedMixinBeh, child_guards ::: translatedGuards, child_measures ::: translatedMeasures))
		
	}
	
	/**
	 * Behaviour instance transformation method  
	 * @param assignedBehId Name of the behavior instance
	 * @param b Behavior corresponding behaviour definition 
	 * @param forward information
	 * @return The LARESflat automaton 
	 */
	def t_Behavior(
	    assignedBehId : Identifier, 
	    b : LBehavior, 
	    fwd :  (List[Identifier],List[(List[Identifier],Int, LGuard)])
	) : LFA = {
		val (ns,guards) = fwd
		val transitions = b.body.T.toList
		
		// determine all states
		val states = (transitions.map(t => Set(t.from,t.to)).foldLeft(Set(b.body.S.toList :_*))(_ ++ _)).toList
		
		// determine the initial state
		val InitialState = if (b.initial.isDefined) State(b.initial.get) else states.head
		
		//  determine the guarded and unguarded transitions
		val guarded_transitions = transitions collect {case t : GuardedTransition => t}
		val unguarded_transitions = transitions collect {case t : UnguardedTransition => t}
		
		// construct the LARESflat automaton comprising a name that follows the current namespace 
		LFA(ns :+ assignedBehId, Set(states :_*), InitialState, unguarded_transitions, guarded_transitions)
	}
	

}

/**
 * Abstract method declarations and helper methods 
 */
trait AbstrL2LF  extends LogicExpr_algo  {
  def processMeasure(ns : List[Identifier], i : AbstrInstance, aux : List[ModuleStatement]) : List[Measure]
  def processGuards(ns : List[Identifier], i : AbstrInstance, aux : List[ModuleStatement]) : List[Guard]
  
  def modifyGenerative(ns : List[Identifier], le : LogicalExpr)  : LogicalExpr = le modify {
    case ResolvedReferenceCondition(namespace,b,n) => ResolvedReferenceCondition(ns ::: namespace,b,n)
    case r: ReferenceCondition => assert(false, "found an unresolved reference to a condition " + r); r
    case sthelse => sthelse
  }
  
  class ImplIdentifierInstance( a: Identifier) {
	def convertToString = a.name + "[" + "]"
  }
  
  implicit def func(a:Identifier) = new ImplIdentifierInstance(a).convertToString
  
  def modifyReactive(ns : List[Identifier], le : LogicalExpr) : LogicalExpr
  
	
  object GatherB extends Gather[ResolvedReferenceGuardLabel] {
	// import statements for logical expression
	import LE_metamodel._
	
	def gatherAtom(sth : LExpAtom )= sth match {
	    case r : ResolvedReferenceGuardLabel => Set(r)
	    case _ => Set()
	}
  }
}

