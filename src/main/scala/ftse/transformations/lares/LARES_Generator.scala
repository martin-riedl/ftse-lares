package ftse.transformations.lares

import ftse.formalism.logical._

// import statements for arithmetic expression 
import ftse.formalism.arith._
import ArithExpr_metamodel._

// import statements for set expression 
import ftse.formalism.set._
import SetExpr_metamodel._

import ftse.formalism.lares.LARES_metamodel._

import ftse.formalism.spa._
import ftse.formalism.spa.PACT._

import scala.collection.immutable._
import ftse.tools._

import ftse.transformations.spa._

trait LaresGenerator {
	import Function._

	// an implicit conversion of transformation lares2[something] functions 
	implicit def implComposableLaresTransformators[T1](f : (LARES_Element) => T1) = new ComposableTransformators[T1](f) 
	
	// providing a compose operator ∘ to be able to compose lares2[something] transformation functions
	class ComposableTransformators[T1](f : (LARES_Element) => T1) {
		def ∘(g : (LARES_Element) => LARES_Element) = f compose g
	}	
}


/**
 * Provides an implicit conversion that allows to perform transformations  
 * from lares models into timenet petri nets
 */

/*
trait LARES2TNImpl {
	implicit def transform(e : LARES_Element) = new pn.L2TNGenerator(e)
}*/

object TrafoLogger extends Logger {
	def initial = ""
	def finish = ""
}


object ResolveIterator extends SetExpr_algo {
	
	def modifyArithExp(a:ArithExpr) : ArithExpr = a //TODO well currently there is not so much that this function does ;)

	/**
	 * Builds the cross-product base on a given iterator list
	 * @param iters
	 * @return
	 */
	def buildCrossProductMap(iters : List[Iterator]) = {
	  // -1- generate a topological sorting of the iterators 
	  val depMap = iters.flatMap(i => i.set.dependsOn().map(a => (a -> i.name))) .groupBy (_._1) map(a => (a._1,a._2.map(_._2)))
	  val L = new scala.collection.mutable.ListBuffer[String]()   // empty list that will contain the sorted nodes 
	  val S = iters.filter(i => i.set.dependsOn().isEmpty).map(_.name) // set of all nodes with no incoming edge
	  val full = HashMap(iters.map(a =>(a.name,List())) :_*) ++ depMap
	  
	  val visited = scala.collection.mutable.HashSet[String]()
	  S.foreach(n => visit(n))
	  
	  def visit(n : String) : Unit = {
	    if (!visited.contains(n)) {
	      visited.add(n)
	      full(n).foreach(m=> visit(m))
	      L+=(n)
	    }
	  }
	  
	  val Iterators = HashMap(iters.map(a=>a.name->a) :_*)
	  val topologicalSortedIterators = L.reverse.map(Iterators(_))
	  
	  // -2- put this sequence in a recursive function that performs the first loop on the first entry
	  def TG(l: List[Iterator]) : Set[List[(String,Int)]] = {
	      val lTopologicalSorted = l // TODO: topological sorting
	      rekTG(lTopologicalSorted, HashMap[String,Int]() )
	  }
	  
	  // -3- this recursive function is defined that the first value is generated and another call 
	  //     is done recursively with the remaining iterators an the already generated numbers
	  def rekTG(l: List[Iterator], gv : HashMap[String,Int]) : Set[List[(String,Int)]] = {
	    if (l.isEmpty) Set(gv.iterator.toList) else {
	      val name : String = l.head.name
	      val oldSet = l.head.set
	      println()
	      
	      
	      val substitutables = Set(gv.keys.toList :_*) & oldSet.dependsOn()
	  	  val newSetExpr = substitutables.foldLeft(oldSet)((set,s) => {
	  	    set subst( a => a match {
        	  case aai : ArithAtomIdentifier => if (aai.id==s) ArithAtomValue(Left(gv.get(s).get)) else aai 
        	  case sthelse => sthelse
	  	    })
	  	  })
	
	      println("isEvaluable: ");
	      println(newSetExpr.isEvaluable())
	        
	      
	      val values : Set[Int] = newSetExpr.eval().left.get
	      
	      val sth = values.foldLeft(Set[List[(String,Int)]]())((set,v) => {
	        
	        val newGV = gv + (name -> v)
	        set ++ rekTG(l.tail,newGV)
	      })
	      sth
	    }
	  }
	  
	  // call of -2- and -3-
	  TG(topologicalSortedIterators.toList)
	}
}
