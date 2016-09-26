package ftse.simulation.laresflat

// import statements for logical expression
import ftse.formalism.arith.ArithExpr_metamodel.ArithAtomValue
import ftse.formalism.lares.LARES_metamodel._
import ftse.formalism.logical._
import LE_metamodel._

import ftse.formalism.arith._

import ftse.formalism.lares._
import LARES_metamodel._

import ftse.formalism.tra.ExtTra
import ftse.formalism.tra.{Transition => TT, _}
import ftse.formalism.lares.flat._
import scala.collection.immutable._
import scala.collection.mutable.{Stack => MStack, Set => MSet, LinkedList => MList}
import ftse.formalism.tra.TraSerializer
import java.io.FileWriter
import ftse.transformations.laresflat.Reach2TraTransformer

import ftse.formalism.lares.{IdentifierSerializer => IS}

object LF_SLTS extends LFReach {
  def eliminate = true
}

object LF_ESLTS extends LFReach {
  def eliminate = false
}

/**
 * This trait and object are needed because of some typing problems in ftse.transformation.lares.reward.RewardReachElimination.
 * It was not possible, to declare ReachExtension as an type in the trait so that transform() could use the "override" modifier.
 * This solution actually is a "quick and dirty" solution and needs to be investigated further.
 */
// TODO investigate the type problem of ReachExtension
trait LFReachSubTypes {
  abstract class ReachExtensionElement
}
object LFReachSubTypes extends LFReachSubTypes

trait LFReachTypes {
  type ComposedState = HashMap[List[Identifier],State]
  type ReachStructure = MStack[(ComposedState,ComposedState,(String,Distribution))]
  type ReachExtension = List[ReachExtensionElement]
  type ReachResult = (ReachStructure, Set[ComposedState], ComposedState)
  
  /**
   * MultiReachResult is a data container, that consists of different ReachResults.
   * Each result is accessible by its specifying EncodingEntity. The MultiReachResult
   * was introduced because a refactoring of ReachStructure or ReachResult would have been
   * very unsafe, because of the missing IDE-support. Furthermore it is one single container
   * for easy access of different sub-models. No further filtering is needed.
   * In the standard case (without any decisions specified in the model), there is only one
   * entry in the map: NoOp -> ReachResult.
   * The first entry has to be in Option, because in the standard case - without any mixins -
   * there is only the possibility to return None.
   */
  type MultiReachResult = Map[Option[LARES_Element], ReachResult]
  
  // part of the solution described in LFReachSubTypes
  import LFReachSubTypes._
  type ReachExtensionElement = LFReachSubTypes.ReachExtensionElement
}

trait LFReachTypesSerializer  {
  import LFReachTypes._
  import ftse.formalism.lares.{IdentifierSerializer => IS}
  
  def serialize(reach : ReachResult) = {
    //TODO
   val RG = reach._1; val S = reach._2; val s0 = reach._3
   val stateEnum = HashMap(S.zipWithIndex.toList : _*)
   val initState = stateEnum(s0)
   val g = 
   "digraph g {" +
		   "node [style=\"bold,rounded,filled\", fillcolor=\"#DDDDDD\", fontsize=20, shape=record]" + 
			"  {" +
			"    rankdir=LR" +
			"    legende [fillcolor = lightblue "+
			"             label=\"" + s0.map(a=>a._1.map(IS.laresType(_)).mkString("_")).mkString("|") + "\"]" +
			"  };" +
	    stateEnum.map(s => {
	      val r1 = s._2 + " [" + "label=\"" + s._1.map(si => IS.laresType(si._2.identifier)).mkString(" | ") + "\"" + "]"
	      
	      val r2 = s._2 + 
	      """ [label=<<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0"> """ + 
		  """<TR>""" +
	      s._1.map(si =>
	        "<TD BGCOLOR=\"#DDDDDD\"" + /*"\" PORT=\"" +  component */">" + IS.laresType(si._2.identifier) + "</TD>"
	      ) +
	      "<TD BGCOLOR=\"" + (if (initState==s._2) "#FF8800" else "#444444")  + "\" PORT=\"" + "statelabel" + "\">" + "<FONT color=\"#FFFFFF\">" + s._2 + "</FONT>" +  "</TD>" +
		  """ </TR></TABLE>>, shape=none]"""
	      
	      r2
	      
	    }).mkString("\n  ","\n  ","\n") + 	    
	    RG.map(a => stateEnum(a._1) + "->" + stateEnum(a._2) + " [" + 
	    	(a._3._2 match {
	    		case Dirac(w) => "label=\"" + a._3._1 + "," +
	    		    (w match {
	    		      case ArithAtomValue(Right(v)) => v
	    		      case _ => ""
	    		    }) +
	    	    "\"" + "style=\"dashed\""
	    		case Exponential(r) => "label=\"" + a._3._1 + "," + 
	    			(r match {
	    		      case ArithAtomValue(Right(v)) => v
	    		      case _ => ""
	    		    }) + "\"" 
	    	}) +
	    "]").mkString("\n  ","\n  ","\n")  +
    "}"
    
	
	ftse.tools.Scripting.showDot(g)
	
	g
  }
  
}

/**
 * companion object for trait LFReachTypes, needed to be able to access the types in definitions of other traits
 * (for example AbstractReachTranformer in ftse.transformations.laresflat
 */
object LFReachTypes extends LFReachTypes

trait AbstrClassModuleStatementHandler extends LFReachTypes {
  def process(current : ComposedState, optNextComposedState : Option[ComposedState], measures : List[Measure]) : List[ReachExtensionElement]
  def processExtTra(r: ReachStructure, s: Map[ComposedState,Int], e: ReachExtension) : List[ExtTra]
  def processMeasures(rg: ReachStructure, states: Set[ComposedState], measures: List[Measure]): ReachExtension
}

trait AbstractLFReach extends LFReachTypes {
	def reach(lfm : LFIA) : MultiReachResult 
}

/**
 * This trait provides methods for guard processing in different projects using the stackable trait pattern.
 */
trait AbstrInteractionalElementsProcessing extends LFReachTypes {
  
  /**
   * This method is used to process interactional elements in other projects, where different data types are available.
   * Therefore, the current state and its guarded transitions are taken into account.
   * The resulting transitions (as a List of them for synchronization reasons; available through the interactional elements)
   * are stored in a map, accessible by their defining LARES_Element.
   */
  def processIActs(iacts: List[InteractionalElement], current: ComposedState, 
      guardedTransitions: HashMap[List[Identifier],List[GuardedTransition]]): 
      Map[Option[LARES_Element], List[List[(List[Identifier], GuardedTransition)]]] = {
    Map()
  }
  
  /**
   * Returns the identifying LARES_Element of the current reactive.
   * standard case -> return None
   * method can be overwritten for different behavior
   */
  def getIdentifyingElement(acr: AbstrConditionalReactive): Option[LARES_Element] = {
    None
  }
  
  /**
   * returns all identifying elements
   */
  def getAllIdentifyingElements(iacts: List[InteractionalElement]): Set[Option[LARES_Element]] = {
    Set(getBaseElement())
  }
  
  /**
   * returns the base identifying element (the standard case)
   */
  def getBaseElement(): Option[LARES_Element] = {
    None
  }
}

class LFReach extends 
	Lares2Text with 
	LaresAlgo with 
	LFReachTypes with 
	AbstrClassModuleStatementHandler with 
	AbstractLFReach with
	AbstrInteractionalElementsProcessing
{  
  
  /**
   * Processes the ReachStructure and the ReachExtension (given by the reach() method) into a list
   * of ExtTra objects that contain the mapping of states and transitions to their evaluated rewards.
   * s is the Mapping from the states in r to the numbers of the corresponding TRA object.
   * For an implementation see ftse.transformation.lares.reward.RewardReachability
   */
  def processExtTra(r: ReachStructure, s: Map[ComposedState,Int], e: ReachExtension) : List[ExtTra] = {
    List()
  }
  
  /**
   * processes the measures either in one single state (if optNextComposedState is None) 
   * or in one single transition (if optNextComposedState is defined)
   */
  def process(current : ComposedState, optNextComposedState : Option[ComposedState], measures : List[Measure]) : List[ReachExtensionElement] = {
    List()
  }
  
  /**
   * Method is used as a wrapper for the process() method, so that this method can be called once after the reach() method.
   */
  def processMeasures(rg: ReachStructure, states: Set[ComposedState], measures: List[Measure]): List[ReachExtensionElement] = {
    List()
  }
  

  def serializeCS(cs : ComposedState) = {
    cs.map(b=> b._1.map(a=>a.toText).mkString(".") +"." + b._2.toText).mkString("_"*30+"\n","\n","\n"+"_"*30)
  }
  
  /**
   * determines whether a ComposedState satisfies the logical expression over states
   */ 
  def evalEnablingFunction(ef : LogicalExpr, state : ComposedState) = {
    
    // derive a logic evaluation evaluator and 
    // overriding the evalAtom function to determine if a referenced state is part of the composed state 
    object lexpeval extends LogicExprEval {
      def evalAtom(atom : LExpAtom) : Boolean = atom match {
        case ResolvedReferenceCondition(i,b,s) => {
          if (state((i :+ b)).identifier==s) true else false
        }
        
        // added BooleanAtom for CalculateEnablingFunction
        case BooleanAtom(b) => b
        
	    case sthelse => {
	      //println("Something else found: " + sthelse);
	      false
	    }
	  }
    }	
    // perform the derived evaluator
    lexpeval.eval(ef)
  }
 	  
  /**
   *  determines whether a sharp guard (i.e. satisfying the current state) 
   *  satisfies the guard label expression by its outgoing transitions    
   *  and delivers the corresponding transitions 
   *  
   *  @returns Set[List[(List[Identifier], GuardedTransition)]]
   */
  // TODO re-write method for higher readability and easier maintenance
    def determineFireable(
        guard : TypedGuard, 
        current : ComposedState,
        availableGuardedOutgoing : HashMap[List[Identifier],List[GuardedTransition]]
      ) = {
      
      	// filter conditional reactives with satisfied condition in this state
        val filteredDests = guard.cndDests.filter(reactive => {
          reactive.cnd match {
            
            // no condition is given -> this reactive should be processed
          	case None => true
          	
          	// condition is given -> only process if condition is satisfied by state 
          	case Some(cond) => evalEnablingFunction(cond, current)
          }
        })
      
          filteredDests flatMap { cndDest => 
	      		// constructing a bdd from the guard label expression of the sharp typed guard
	      		val bddLabels = new LogicExprBdd(cndDest.reactive)
	      		
	      		// deriving the variable labels and the canonical set of minterms   
			    val (labels, terms) = bddLabels.satMinTerms()
			    
			    // since the guard is typed for those referenced guard labels with unknown distribution type 
			    // implicitely the type of the guard is assumed 
			    // further we require the type to be of ResolvedReferenceGuardLabel for a literal of the minterm  
			    val canonicalMinTerms=terms.map(vterm => 
			      vterm.map(a => a match { 
			        case (ResolvedReferenceGuardLabel(ns,id,None,z), b : Boolean) => Some((ResolvedReferenceGuardLabel(ns,id,Some(cndDest.distr),z),b))
			        case (l : ResolvedReferenceGuardLabel, b : Boolean) => Some((l,b)) 
			        case _ => None
			      }).filter(_.isDefined).map(_.get) 
			    )
			    
			    val filteredCanonicalMinTerms = canonicalMinTerms filter { minterm =>
			      // determines if a minterm is fireable 
			      val fireable = minterm.foldLeft(true)((a,b) => {
			        // for each literal of the minterm construct the full reference to the 
			        // behavior instance and determine its corresponding current state
			        val fullref = b._1.instanceReferences :+ b._1.behaviorReference
			        
			        // determine all outgoing transitions
			        val localAvailableOutgoing = availableGuardedOutgoing.getOrElse(fullref,List())
			        
			        // and determine those transitions which literal mach to the current literal 
			        val matchingLocal = localAvailableOutgoing filter (t => t.guard == b._1.namedStatementRef)
			        
			        // if    the current literal is true and the set of matching transitions  
			        //       is empty the whole minterm can not be satisfied 
			        // else  if the current literal is not true and the set of matching transitions
			        //       is not empty the whole minterm can not be satisfied 
			        if (b._2) a && !matchingLocal.isEmpty else a && matchingLocal.isEmpty
			      })		     
			      
			      // use the filter to keep all minterms that are fireable 
			      fireable
			    }
	      		
	      		val mappedFCMT = filteredCanonicalMinTerms map (_.filter(_._2) map(_._1)) 
	      		
	      		val flatmappedFCMT = mappedFCMT flatMap { tLiteralsMinTerm => 
			      
			      // map each affected behavior instance to 
			      // the affected transitions (i.e. referenced by true literals)
			      val componentSortedTransitions = HashMap(tLiteralsMinTerm.map(c => {
			        val fullref = c.instanceReferences :+ c.behaviorReference		        
			        (fullref, availableGuardedOutgoing(fullref) filter (t => t.guard==c.namedStatementRef))
			      }).toList :_*)
			      	
			      
			      val res = componentSortedTransitions.foldLeft(Iterable(List[(List[Identifier], GuardedTransition)]()))((A,b) => {
			        b._2.flatMap( r=> {
			          
			          // implicitely assume the typed guard distribution if no distribution set for the transition
			          val distribution = r.distr.getOrElse(cndDest.distr match {
			            case DiracType => Dirac1
			            case ExponentialType => Exponential1
			          })
			          
			          // should not occur but just for the case
//			          assert(distribution.distrType==cndDest.distr, "distributions do not match")
			           
			          val typedTransition = GuardedTransition(r.from,r.to,r.guard, Some(distribution))
			          
			          // append 
			          A map (e => e :+ ((b._1.toList,typedTransition)) )
			        })
			      })
			      
			      res
			    }
	      		
	      		// map transitions to defining interactional element
	      		val improvedFlatmappedFCMT = flatmappedFCMT map (x => (x,getIdentifyingElement(cndDest)))
	      		improvedFlatmappedFCMT
          }
    }
    
    /**
     * calculates an reachability analysis over the given LFIA
     * returns the reachability-graph, a set of all states and the initial state, accessible by its defining action
     * or NoOp in case no action was specified
     */
	def reach(lfm : LFIA) : MultiReachResult = {
	  def determineTransitions[T <: Transition](lfm : LFIA, f : LFA => Seq[T]) = {
	    // determines that mapping for each state of a LFA to its outgoing transitions      
	    val res = //Map(lfm.lfas.flatMap(lfa => lfa.states.map((lfa.name,_)->List[T]())) :_*) ++
	    (lfm.lfas.flatMap(n => f(n).map(t => ((n.name,t.from) -> t))) groupBy(_._1) map(e => (e._1, e._2.map(_._2))))
	    
	    res
	  }
	  
	  val G = determineTransitions(lfm, n=> n.guarded)
	  //println("GuardedTransitions:")
	  //println(G.map(t => t._1 + "\n" + t._2.map(_.toText).mkString("\n")).mkString("\n"))
	
	  val U = determineTransitions(lfm, n=> n.unguarded)

	  //***********************************************************************
	  // reachability 
	  
	  var initialstate : ComposedState = HashMap(lfm.lfas.map(n => { n.name -> n.initial }) :_*) // construct initial state
	  val todo = MStack(initialstate) // insert initial state
	  val done = MSet[ComposedState]() // nothing done yet
	  
	  var multiReach = Map[Option[LARES_Element], ReachResult]() // empty MultiReachResult
	  
	  // create a reachability graph for every identifying element
		  
	  // get set of keys
	  val keySet = getAllIdentifyingElements(lfm.iacts)
		  
	  // step through every key
	  for (key <- keySet) {
		    
	    // create a new ReachStructure
	    val rs = MStack[(ComposedState,ComposedState,(String,Distribution))]()
		    
	    // create a new set of states
	    val stateSet = Set[ComposedState]()
		    
	    // add to multiReach
	    multiReach = addEntryMultiReach(multiReach, key, rs, stateSet, initialstate)
	  }
	  
	  while (todo.size>0) {
		  val current = todo.pop() 
		  done += current
		  val availableGuardedOutgoing = current.map(a => (a._1,G.getOrElse(a,List()))) 	// determine the current states available guarded transitions 
		  val availableUnguardedOutgoing = current.map(a => (a._1,U.getOrElse(a,List())))	// determine the current states available unguarded transitions
//		  val active = availableUnguardedOutgoing.iterator.map(a => a._2.map((a._1,_))).toList
		 
		  println(serializeCS(current))
//		  println("availableGuardedOutgoing")
//		  println(availableGuardedOutgoing.map(a => a._1.map(_.toText).mkString(".")+ ":   " + a._2.map(_.toText).mkString("|")).mkString("  ","\n  ","\n"))
		  
		  // determine the "sharp" guards
		  val sharpGuards = (lfm.iacts collect {case g : TypedGuard => g}).filter (g => evalEnablingFunction(g.le, current))
		  
		  println("SharpGuards")
		  println(sharpGuards.map(_.toText).mkString("\n"))
		  // determine the "fireable" guards
		  val triggered = sharpGuards flatMap {guard => determineFireable(guard, current, availableGuardedOutgoing)}
		  
		  println("TRIGGERED:")
		  println(triggered.map(e => e._1.map(e=>e._2.toText).mkString(",")).mkString("\n"))
		  
		  // store the transitions in a map, accessible by the base element
		  val transitionMap = transformListToMap(triggered)
		  
//		  // process special interactional elements in other traits in order to get new fireable transitions
		  val specialMap = processIActs(lfm.iacts, current, availableGuardedOutgoing)
		  
		  // merge both maps
		  val transSpecMap = mergeMapsWithLists(transitionMap, specialMap)
		  
//		  println(transSpecMap.map(e => e._1 + " \n"+ e._2.map(e => e._1 +" " +e._2.toText).mkString("\n")).mkString("\n"))
		   
		  println("handle effects caused by sharp guards")
		  // step through the map and store the transitions in different reachStructures for each key-element
		  for (entry <- transSpecMap) {
	    
		    // easy access to parts of entry
		    
		    // defining LARES_Element
		    val laresElem = entry._1
		    
		    // stored transitions
		    val transitionList = entry._2
		    
		    // step through every list of transitions (synchronization reasons)
		    for (transitions <- transitionList) {		    
		     
		    	// fold the associated transition and determine the target state
		    	val next = transitions.foldLeft(current)((c,t) => c.map(e => if (e._1==t._1.toList) (e._1,t._2.to) else e))
			  
		    			if (transitions.nonEmpty) {
			  
		    				val compDistr = (transitions.collect {case (_,GuardedTransition(_,_,_,Some(distr))) => distr}).reduceLeft((a,b) => compose(a,b))
			  
		    				// push the new transition in the matching reachability graph (safe, because of loop through mergedMap)
		    				val RG = multiReach.get(laresElem).get._1
		    				RG push ((current,next,("g_"+"???",compDistr))) // TODO modify string if needed
		    				println("push a new state: \n" + serializeCS(next).replace("\n", "\n  "))
		    			}
			  
		    	if (!todo.contains(next) && !done.contains(next)) {
		    		todo.push(next)
		    	}
		    }
		  }
		  
		  println("handle effects caused by unguarded transitions")
		  // construct the target states by performing the unguarded transitions emanating the current state
		  availableUnguardedOutgoing map { aU => val (compIdent, lstUnguarded) = aU
		     lstUnguarded.map { ut =>  
//		       println(ut.toText)
		       
		       // generate next state
		       val next = current.map(entry => if (entry._1==compIdent) (compIdent, ut.to) else entry)
		       
		       // process new transition
		       if (!todo.contains(next) && !done.contains(next)) {
		         todo.push(next)     
		       }
		       
		       // step through every reachability structure in order to add the unguarded transitions to every RS
		       for (entry <- multiReach) {
		    	   
		    	   // get the reachability graph
		    	   val RG = entry._2._1
		    	   
		    	   // push the state
		    	   //val label = ut.from.identifier.toText+ut.to.identifier.toText
		    	   val label = IS.plainType(ut.from.identifier)+IS.plainType(ut.to.identifier)
		    	   RG push ((current,next,(label,ut.distr.get)))
		    	   println("push a new state: \n" + serializeCS(next).replace("\n", "\n  "))
		       }
		       
		     }
		  }
	  }

	  // add the set of states (stored in "done") to every entry of multiReach
	  // done needs to be immutable, so perform ".toSet" on done
	  val result = multiReach.map(e => (e._1,(e._2._1,done.toSet,e._2._3)))
	  
	  return result
	}
	
	/* ************************************************ *
   * ****** Helping Methods ************************* *
   * ************************************************ */  
  
  /**
   * Transforms a list of pairs of lists and elements into a map. This map stores the elements
   * as keys, and merges the lists into a list of lists.
   */
  def transformListToMap[A,B](list: List[(List[A],B)]): Map[B,List[List[A]]] = {
    
    // first create a map that maps the second entry of each element to its first, than merge all created maps
    return list.foldLeft(Map[B, List[List[A]]]())   ((old, elem) => {
          
       	// create new map
       	val newMap = Map(elem._2 -> List(elem._1))
        	
       	// merge old map with current map
       	val mergedMap = mergeMapsWithLists(old, newMap)
       	mergedMap
    })
  }
  
  /**
   * This method merges maps that contain Lists as their value objects.
   * The lists are merged and duplicate elements are deleted.
   */
  def mergeMapsWithLists[A,B](m: Map[A,List[B]], n: Map[A, List[B]]): Map[A, List[B]] = {
    
    // provide access to both maps depending on their size
    var bigger: Map[A, List[B]] = Map()
    var smaller: Map[A, List[B]] = Map()
    
    // determine order of process (minimize duration of loop)
    if (m.size > n.size) {
      bigger = m
      smaller = n
    } else {
      bigger = n
      smaller = m
    }
    
    // step through the smaller map
    for (entry <- smaller) {
      
      // check whether bigger map contains entry with same key
      if (bigger.contains(entry._1)) {
        
        // bigger contains an entry with same key, so merge both lists (safe, because of previous if-clause)
        val newList = bigger.get(entry._1).get ++ entry._2
        
        // delete duplicate list-members
        val shortList = newList.distinct
        
        // add shortList to bigger map
        bigger += (entry._1 -> shortList)
        
      } else {
        
        // bigger contains no entry with same key, so add the whole entry to the bigger map
        bigger += entry
      }
    }
    
    // return result
    return bigger
  }
  
  /**
   * adds a new entry to the multiResult by merging the existing entries with new entry
   */
  def addEntryMultiReach(multiReach: MultiReachResult, key: Option[LARES_Element],
		  				rs: ReachStructure, stateSet: Set[ComposedState], initialstate: ComposedState): MultiReachResult = {
    
    // check whether there exists already an entry with this key
    multiReach.get(key) match {
      
      // no entry exists -> simply put in the given values and return
      case None => return multiReach + (key -> (rs, stateSet, initialstate))
        
      // entry exists -> step into processing  
      case Some(entry) => {
        
        // generate new ReachStructure by adding new rs to existing rs and remove duplicates
        val newRS = (entry._1 ++ rs).distinct
        
        // generate new stateSet in the same way
        val newSS = (entry._2 ++ stateSet)
        
        // initial state should be the same, so no change here
        
        // add the new entry to muliReach and return; thereby the old entry will be overwritten
        return multiReach + (key -> (newRS, newSS, initialstate))
      }
    }
  }

}