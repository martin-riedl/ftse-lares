package ftse.transformations.lares

import ftse.formalism._

import ftse.formalism.lares.Lares2Text
import ftse.formalism.lares.LARES_metamodel._

import scala.collection.immutable._

/**
 * Abstract Lares Traverser
 * taking two parameter types to denote the type of forward rsp. backward information 
 */
trait LaresTraverser[BW,FW] extends Lares2Text 
{
    /**
     * abstract backward propagation method
     */
	def backwards(
	    ri : AbstrInstance,
	    fw : FW,
	    bw : Iterable[(AbstrInstance,BW)]
	) : (AbstrInstance,BW)
	 	  
	/** 
	 *  abstract forward propagation method
	 */
	def forwards(
	    ri : AbstrInstance,
	 	fw : FW
	) : (AbstrInstance,FW) 

	
	/**
	 * main traversal function  
	 */
	def traverse(
		i : AbstrInstance, 
		fw : FW
	) : (AbstrInstance,BW) = {
		
		val emptyspaces : String = " "*(67-i.identifier.toText.size).toInt
		println("_______________________________________________________________________________")
		println("|  FORWARD " + i.identifier.toText + emptyspaces + "|")
		
		// perform a computation which results are then propagated into the subtrees
		val forwardresult = forwards(i,fw)

		//TrafoLogger.indentInkr
		// do a recursive decent and propagate the computation results into the subtrees 
		val smd = forwardresult._1.itype.right.get.body.instances.map(subinstance => {
			traverse(subinstance,forwardresult._2)
		})
		
		//TrafoLogger.indentDekr
		println("_______________________________________________________________________________")
		println("| BACKWARD " + i.identifier.toText + emptyspaces + "|")
		//TrafoLogger.indentInkr
		
		// perform a computation which results are propagated in the direction of the root node
		val backwardresult = backwards(forwardresult._1,forwardresult._2,smd)
		//TrafoLogger.indentDekr
		
		backwardresult
	}
}



