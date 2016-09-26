package ftse.transformations.laresflat
import ftse.formalism.lares.LARES_metamodel.{Dirac}
import ftse.formalism.logical._

// import statements for arithmetic expression 
import ftse.formalism.arith._
import ArithExpr_metamodel._

// import statements for set expression 
import ftse.formalism.set._
import SetExpr_metamodel._

/**
 * This trait is used to add self loops (with probability "1.0") to every state of the given
 * ReachResult, that has no outgoing transition.
 */
trait ReachResultSelfLoop extends AbstractReachModifier {

  /**
   * This method performs the transformation, i. e. adding the self loops.
   */
  abstract override def transform(obj: ReachResult): ReachResult = {
    
    // provide data container for new ReachStructure
    var newRS: ReachStructure = null
    
    // step through every state
    for (state <- obj._2) {
      
      // check whether there is an outgoing transition from this state
      val hasNoOutgoing = obj._1.filter(_._1 equals state).isEmpty
      
      // add self loop, if there is no transition
      if (hasNoOutgoing) {
        
        // generate loop
        val loop = (state, state, ("generated_self_loop", Dirac(ArithAtomValue(Left(1)))))
        
        // add loop to reach structure
        newRS = obj._1 push loop
      }
    }
    
    // generate new ReachResult
    val result = (newRS, obj._2, obj._3)
    
    return super.transform(result) 
  }
}