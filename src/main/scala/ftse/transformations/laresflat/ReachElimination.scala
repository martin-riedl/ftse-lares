package ftse.transformations.laresflat

import scala.collection.mutable.Stack
import ftse.formalism.lares.LARES_metamodel.{Exponential, Dirac}
import ftse.formalism.logical._


// import statements for arithmetic expression 
import ftse.formalism.arith._
import ArithExpr_metamodel._

// import statements for set expression 
import ftse.formalism.set._
import SetExpr_metamodel._

/**
 * This trait is used to eliminate the vanishing states out of the result from the reachability analysis.
 * 
 */
trait ReachElimination extends AbstractReachExtensionModifier with ArithExpr_algo {

  /**
   * eliminates vanishing states from the reach structure and the set of states
   * algorithm is identical to the algorithm in ftse.transformations.tra.TraElimination
   */
  abstract override def transform(obj: (ReachResult, ReachExtension)): (ReachResult, ReachExtension) = {

    /**
     * eliminates vanishing states and modifies the remaining markovian transitions
     * @returns new transitions, unchanged transitions, remaining states, new extension
     */
    def eliminationStep(transitions: ReachStructure, states: Set[ComposedState], initial: ComposedState, extension: ReachExtension): 
    	(ReachStructure, ReachStructure, Set[ComposedState], ReachExtension) = {
      
      // collect all states that are reached by at least one immediate transition
      val reachedByImmediates = (transitions filter (_._3._2.isInstanceOf[Dirac])) map (t => t._2)
      
      // group all transitions by their source state
      val transitionsBySource = transitions groupBy (t => t._1)
      
      // group all markovian transitions by their target state
      val markoviansByTarget = (transitions filter (_._3._2.isInstanceOf[Exponential])) groupBy (t => t._2)
      
      // get all states, that are not reached by an immediate transition, but have at least one outgoing immediate transition
      
      // get all transitions from this states
      val interestingTransitions = ((states -- reachedByImmediates) map (state => transitionsBySource.getOrElse(state, Stack()))).flatten
      
      // filter the immediate transitions and ignore the initial state (!)
      val interestingImmediates = interestingTransitions filter (t => t._3._2.isInstanceOf[Dirac] && !t._1.equals(initial))
      
      // group the immediates by their source state
      val vanishing = interestingImmediates groupBy (t => t._1)
      
      // modify transitions for every state in vanishing
      var newTransitions: ReachStructure = Stack()
      
      // store the changed extension
      var newExtension: ReachExtension = List()
      
      for (entry <- vanishing) {
        
        val state = entry._1
        val immTransitions = entry._2
        
        // calculate sum of all weights of the outgoing transitions
        
        // evaluate all values of the transition (casting possible because of previous filtering)
        val evaluatedValues = immTransitions.map(trans => trans._3._2.asInstanceOf[Dirac].weight.eval())
        
        // calculate the sum
        val sum = evaluatedValues.foldLeft(0.0)((old, value) => {
          value match {
            case Left(x) => old + x.toDouble
            case Right(x) => old + x
          }
        })
        
        // take every markovian transition, that targets this state, into account
        for (markovian <- markoviansByTarget.getOrElse(state,List())) {
          
          // generate one new markovian transition for every immediate transition out of this state
          for (immediate <- immTransitions) {
            
            // source and label unchanged, target of the immediate transition, changed value (i.value*m.value / sum)
            val source = markovian._1
            val label = markovian._3._1
            val target = immediate._2
            val value = ArithDiv(ArithMult(immediate._3._2.asInstanceOf[Dirac].weight, markovian._3._2.asInstanceOf[Exponential].rate),
            					ArithAtomValue(Right(sum)))
            val distribution = Exponential(value)
            val newMarkovian = (source, target, (label, distribution))
            
            // add the new transition to the stack
            newTransitions.push(newMarkovian)   
            
            // transform the rewards of the transitions
            newExtension = newExtension ++ extensionTransformation(Stack(markovian, immediate), extension)//extensionTransformation(transitions, extension)
          }
        }
      }
      
      // determine the remaining states (current states without the vanishing states)
      val remainingStates = states -- vanishing.map(entry => entry._1)
      
      // create the result (new transitions, remaining transitions with their states in the remaining states, remaining states)
      val result = (newTransitions, 
    		  		transitions filter (t => (remainingStates contains t._1) && (remainingStates contains t._2)),
    		  		remainingStates, newExtension)
      
      return result
    }
    
    /* *********************************** *
     * "real" working of tranform() begins *
     * *********************************** */
    
    // extract the transitions from the input
    var transitions = obj._1._1
    
    // extract the states from the input
    var states = obj._1._2
    
    // extract the initial state from the input
    val initialState = obj._1._3
    
    // provide interim results
    var newTransitions: ReachStructure = Stack()
    var oldExtension = obj._2    
    
    // perform eliminationStep() as long as there is a new transition built by eliminationStep()
    do {
      
      // perform one single elimination step
      val (modifiedTransitions, unchangedTransitions, remainingStates, newExtension) = 
        eliminationStep(transitions, states, initialState, oldExtension)
      
      // store the new (modified) transitions
      newTransitions = modifiedTransitions
      
      // store the new extension
      oldExtension = oldExtension ++ newExtension
      
      // save the actual remaining transitions
      transitions = newTransitions ++ unchangedTransitions
      
      // save the remaining states
      states = remainingStates
      
    } while (!newTransitions.isEmpty)
    
    // generate the result
    val result = ((transitions, states, initialState), oldExtension)
      
    // transform in other traits
    super.transform(result)
  }
}