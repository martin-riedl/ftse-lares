package ftse.transformations.deprecated

// import statements for logical expression
import ftse.formalism.logical._
import LE_metamodel._


import scala.collection.immutable.Stack
import ftse.formalism.lares._

import jdd.bdd._
import LARES_metamodel._

@deprecated
trait SatPathCalculator {
         /*
   * @param le: logical state expression representing a condition
   * @param traverser: a function traversing the bdd from the root node generating guarded processes
   * @returns a list with the generated guarded processes
   */
       
   type nodenum = Int 
   type variable = Int
  
   /** @deprecated **/
   def convert2SP(le: LogicalExpr) : List[List[(ResolvedReferenceCondition, Boolean)]] = {
          //Definition of a BDD with a maximal nodesize of 10000
      val bdd = new BDD(10000)          
      // Hashmaps to save the relationship between Atom and the Integer representing the variable
      val var_hash = new scala.collection.mutable.HashMap[variable, ResolvedReferenceCondition]
      val name_hash = new scala.collection.mutable.HashMap[ResolvedReferenceCondition, variable]
      /*
       * inner recursive function with sideeffects on var_hash and name_hash that stores a mapping 
       * node -> identifier, identifier -> node
       * @param le: logical state expression 	
       * @returns: root node  
       */
      
      val countstatespercomponent = scala.collection.mutable.HashMap[(List[Identifier], Identifier), List[Int]]()
   
      def rek_toBDD(le: LogicalExpr) : nodenum = le match {
        //case for a boolean expression
        case r : ResolvedReferenceCondition => {
          val v = name_hash.get(r)
          if (v.isDefined)
            //if the given String already exists return that variable
            return v.get
          else {
            //else create a new variable and save the relationship in a HashMap
              val v = bdd.createVar()
              
              val key = (r.instanceReferences,r.behaviorReference)
        	  countstatespercomponent(key) = v :: countstatespercomponent.getOrElse(key,List())          

              var_hash += ((v, r))
              name_hash += ((r, v))

              v 
          }
        }
        
        //case for logical or
        case Disjunct(l: LogicalExpr, r: LogicalExpr) => {
          val lBDD = rek_toBDD(l)
          val rBDD = rek_toBDD(r)
          
          val resBDD = bdd or(lBDD,rBDD)
          
          bdd.ref(resBDD)
          bdd.deref(lBDD)
          bdd.deref(rBDD)

          resBDD
        }
        //case for logical and
        case Conjunct(l: LogicalExpr, r: LogicalExpr) => {
          val lBDD = rek_toBDD(l)
          val rBDD = rek_toBDD(r)
          
          val resBDD = bdd and(lBDD,rBDD)
          
          bdd.ref(resBDD)
          bdd.deref(lBDD)
          bdd.deref(rBDD)

          resBDD
        }
        //case for logical not
        case Negate(arg: LogicalExpr) => {
          val argBDD = rek_toBDD(arg) 
          val resBDD = bdd not(argBDD)
          bdd ref resBDD
          bdd deref argBDD
          resBDD
        }
        case a => error("No Valid LogicalExpression for SatisfyablePath calculation: " + a)
      }
   
      
      val root = rek_toBDD(le)
 
      if (root!=bdd.getZero) {
    	if (root==bdd.getOne) throw new Exception(le + " has been evaluated to true")
    	val paths = GenPaths(root,bdd,var_hash,countstatespercomponent)
      	val var_hash_keys = var_hash.keys.toList.sortWith((e1,e2)=>e1<e2)
      	paths.map(a=>a.toList.map(a=>(a._2,a._3)))
      } else List()
  }
 
  def GenPaths (
    root : Int,
    bdd : BDD,
    var_hash : scala.collection.mutable.Map[variable, ResolvedReferenceCondition],
    countstatespercomponent :  scala.collection.mutable.HashMap[(List[Identifier], Identifier), List[Int]]
  ) = {
    //println("NEWBDD")
    //HashSet containing all nodes of the given BDD
    val marked = new scala.collection.mutable.HashSet[nodenum]()
    //a queue with the getHighs and getLows of the actual node
    val queue = new scala.collection.mutable.Queue[nodenum]()
    //actual node is added to the queue and the actual node is added to the marked
    queue.enqueue(root); marked += root	
    
    val symbolicTrue = bdd.getOne()
    val var_hash_keys = var_hash.keys.toList.sortWith((e1,e2)=>e1<e2)
    val parents_mapping = new scala.collection.mutable.HashMap[nodenum,List[(nodenum,ResolvedReferenceCondition,Boolean)]]()    
    //modified breadthsearch to build up the marked list
    do {
      val originalnode : Int = queue.dequeue
      var highentry : (nodenum, List[(nodenum,ResolvedReferenceCondition,Boolean)]) = null 
      var lowentry : (nodenum, List[(nodenum,ResolvedReferenceCondition,Boolean)]) = null
      val nodevar = bdd.getVar(originalnode)

      val high_node : nodenum = bdd.getHigh(originalnode)
      val high_nodevar = bdd.getVar(high_node)

      // introduce intermediate splitstates for the high-part
      var node = originalnode
      var nodeatom = var_hash(var_hash_keys(bdd.getVar(node)))
      
//          println("CURRENT NODE",originalnode,bdd.getVar(originalnode),nodeatom)
//          println("HIGH_NODE",high_node,high_nodevar)
                
      val bypassed_variable_levels_high = bdd.getVar(originalnode)+1 to bdd.getVar(high_node)-1
      
//          println("BYPASSED_LEVELS: " + bypassed_variable_levels_high.size)
      
      for (successor <- bypassed_variable_levels_high) {
        highentry = (successor+1000,((node,nodeatom,true) :: parents_mapping.getOrElse(successor+1000,List())))	
        parents_mapping += highentry
        node = successor+1000	
        nodeatom=var_hash(var_hash_keys(successor))
        
        if (bypassed_variable_levels_high.last<successor)
          lowentry = (successor+1001,((node,nodeatom,false) :: parents_mapping.getOrElse(successor+1001,List())))
        else 
          lowentry = (high_node,((node,nodeatom,false) :: parents_mapping.getOrElse(high_node,List())))
        
        parents_mapping += lowentry
      }
      
      highentry = (high_node,((node,nodeatom,true) :: parents_mapping.getOrElse(high_node,List())))	
      parents_mapping += highentry   

      // introduce intermediate splitstates for the low-part
      
      node = originalnode
      nodeatom = var_hash(var_hash_keys(bdd.getVar(node)))
      val low_node : nodenum = bdd.getLow(originalnode)
      var low_nodevar = bdd.getVar(low_node)
//          println("LOW_NODE",low_node,low_nodevar)
      
      val bypassed_variable_levels_low = bdd.getVar(originalnode)+1 to bdd.getVar(low_node)-1
      
//          println("BYPASSED_LEVELS: " + bypassed_variable_levels_low.size)
      
      for (successor <- bypassed_variable_levels_low) {
        lowentry = (successor+2000,((node,nodeatom,false) :: parents_mapping.getOrElse(successor+2000,List())))	
        parents_mapping += lowentry
        highentry = (successor+2000,((node,nodeatom,true) :: parents_mapping.getOrElse(successor+2000,List())))	
        parents_mapping += highentry
        node = successor+2000
        nodeatom=var_hash(var_hash_keys(successor))
      }       
      
      lowentry = (low_node,((node,nodeatom,false) :: parents_mapping.getOrElse(low_node,List())))
      parents_mapping += lowentry
      
//          println(parents_mapping)
      
      //the gethigh of the node 
      if (high_node>1 && !marked.contains(high_node)) {
        queue.enqueue(high_node); marked += high_node
      }
      //the getlow of the node
      if (low_node>1 && !marked.contains(low_node)) {
        queue.enqueue(low_node); marked += low_node
      }
    } while (!queue.isEmpty)
   
    val l = marked.toList
                   

    val current = symbolicTrue
    
   // println(parents_mapping)
    
    def paths(c : nodenum, path : scala.collection.immutable.Stack[(nodenum,ResolvedReferenceCondition,Boolean)]) : List[scala.collection.immutable.Stack[(nodenum,ResolvedReferenceCondition,Boolean)]] = {
      //println(c)
      if (c == root) {
        //println("test")
        List(path)
      } else {
          val parents = parents_mapping(c)
          parents flatMap(p=> paths(p._1,path).map(a=>a.push(p)))
      }
    }        
    val blub = paths(current, new scala.collection.immutable.Stack())
    //println("NUMBER OF PATHS: " + Set(blub:_*).size)
    blub
    //ps.map(a=>a.map(b=>var_hash(b._1)))
  }
   
}