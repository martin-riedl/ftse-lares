package ftse.simulation.pn

import ftse.formalism.timenet._
import ftse.formalism.lares._
import LARES_metamodel._

// import statements for logical expression
import ftse.formalism.logical._
import LE_metamodel._


import scala.collection.immutable.HashMap

object PNS_SLTS extends PNS {
  def eliminate = true
}

object PNS_ESLTS extends PNS {
  def eliminate = false
}

abstract class PNS extends Enumeration with Lares2Text with eDSPN_serializer_impl {
    implicit def serializeLExp(ef : LogicalExpr) = new LARESLogicExpr2Text(ef)
		    
	def eliminate : Boolean
  
	def simulate(nets : List[EDSPNNet]) = {
      println("Nets: "+nets.map(_.id).mkString(","))
	  
      /*
       *
        // check the obtained TimeNet models of the sequential behaviors
		import sys.process._
  	  	nets.foreach(edspn => {
			val dot = (new eDSPN_serializer(edspn)).toDot
			println(dot)
  	  	})
  	  */
      
	  def evalEnablingFunction(ef : LogicalExpr, state : HashMap[String,String]) = {
	    
	    object lexpeval extends LogicExprEval {
	      def evalAtom(atom : LExpAtom) : Boolean = atom match {
	        case ResolvedReferenceCondition(i,b,s) => {
	          val c = i.map(_.toText) .mkString("","_","_") + b.toText
	          if (state(c)==c+"_"+s.toText) true else false // if an exception occurs the boolean expression seems to be invalid (somehow)
	        }
	        case _ => false
	      }
	
	    }
	    
	    val result = lexpeval.eval(ef)
	    
	    /*
	    if (result) {
		    println(state.mkString("","\n","") + "\n " + ef.toText + "=>" + result)
	    }
	    */
	    
	    result
	  }
	  /*
	  def isEnabledImmediate(t : TransitionTypable, state : HashMap[String,String]) = {
	    t match {
	      case ImmediateTransitionTypeFTSE(_,_,_,_,_,_,gId,g,_) => evalEnablingFunction(g.le,state)
	      case _ => false
	    }
	  }*/
	  
	  def determineTransitions[T <: TransitionTypable](nets : List[EDSPNNet], f : EDSPNNet => Seq[T]) = {
	    nets.flatMap(n => { // use the net id as first key
	    	
	      
	      // create a temporary HashMap of arcs indexed by the source id
	      val arcs = n.arc.map(a=>a.fromNode -> a.toNode).groupBy(_._1) 
		  
		  // create a temporary HashMap of transitions indexed by their id
		  val T = HashMap( 
		      f(n).map(t=>t.id -> t) : _*
		  )
		  
		  // return a HashMap 
		  val hm = n.place.map(s => (n.id,s.id) -> {
		    // obtain all transition ids from the arcs having the same place id as a sort
		    val availableTransitionIDs = arcs.getOrElse(s.id,List()).map(_._2) 
		    // obtain all transitions having a connection to the source place s
		    val availableTransitions = availableTransitionIDs.map(TransitionID => T.get(TransitionID)) filter (_.isDefined) map(_.get)
		    // return all transitions with source place a with the ids of its target places
		    availableTransitions.map(t => (t,arcs(t.id).map(_._2)))
		  })
		  
		  /*
		  import sys.process._
  	  		val dot = (new eDSPN_serializer(n)).toDot
			println(dot)
			*/
		  hm 
	    }) 
	  }
	  
	  //val Itransitions = determineTransitions(nets, n => n.immediateTransition filter(_ match {case x:ImmediateTransitionTypeFTSE => true; case _ => false})) groupBy(_._1)
	  // TODO Martin->Alex
	  val Itransitions = determineTransitions(nets, n => n.immediateTransition collect {case x:ImmediateTransitionTypeFTSE => x}) groupBy(_._1)
	  
	  Itransitions.values.foreach(i => i.foreach(k=>k._2.foreach(t=>println(k._1 + "---<" + t._1.guard.toText + ">-->"))))
	  
	  // TODO Martin->Alex
	  val ItransitionsUnguarded = determineTransitions(nets, n => n.immediateTransition collect {case x: ExponentialTransitionType => x}) groupBy(_._1) 
	  
	  val Mtransitions = determineTransitions(nets, n => n.exponentialTransition) groupBy(_._1)

	  var initialstate = HashMap(nets.map(n => {
	    n.id -> {
	      (n.place.find(_.initialMarking == "1").getOrElse(n.place.head)).id
	    }
	    
	  }) :_*)
	  
	  val todo = scala.collection.mutable.Stack(initialstate)
	  val done = scala.collection.mutable.Set[HashMap[String,String]]()
	  var RG = scala.collection.mutable.Stack[(HashMap[String,String],HashMap[String,String],TransitionTypable)]()
	  
	  while (todo.size>0) {
		  val current = todo.pop()
		  done += current
		
		  val enabledITs = current flatMap (nIdsId => Itransitions(nIdsId)) flatMap 
		  					(t => t._2.map(tra=> (t._1,tra._1,tra._2) )) groupBy(t => {
		  					  val iT = t._2.asInstanceOf[ImmediateTransitionTypeFTSE]
		  					  (iT.guardId,iT.guard)
		  					}) filter(g => evalEnablingFunction(g._1._2.le,current))
		 
		  					
		  val enabledMTs = current flatMap (nIdsId => Mtransitions(nIdsId)) flatMap 
		  					(t => t._2.map(tra=> (t._1,tra._1,tra._2) ))  
		  
		  val enabledITsUnguarded = current flatMap (nIdsId => ItransitionsUnguarded(nIdsId)) flatMap 
		  					(t => t._2.map(tra=> (t._1,tra._1,tra._2) )) 
		  					
		  // if enabled immediate transitions are available then the maximal progress assumption holds 
		  //if (enabledITs.size>0 || enabledITsUnguarded.size>0) {
		    val imm = enabledITs.map(entry => {
		      println("enabled: "+entry._1._1._1.map(_.toText).mkString("","_","_")+entry._1._1._2+" "+entry._1._2.toText)
		      val (guardId,transitions) = entry
		      println("GuardEnumerator:" + guardId._1._2)
		      println(transitions.map(t=>t._1 +"->"+ t._3.mkString("\n")))
		      val componentStateSortedTransitions = transitions.groupBy(t => t._1) //.filterKeys(a => current(a._1)==a._2)
		      //val minterms = (new LogicExprBdd(guardId._2.destinations)).satMinTerms()
		      class evalGuardLabelReferences(guardLabels : Set[String]) extends LogicExprEval {
		        println("guardLabels: " + guardLabels.mkString(", "))
		        
		        override def evalAtom(atom: LExpAtom): Boolean = atom match {		          
		          case r : ResolvedReferenceGuardLabel => {
		              val stringRep = r.instanceReferences.map(_.toText).mkString(".") + "." + r.behaviorReference.toText + "." + r.namedStatementRef.toText
		              println("resolvedLabel " + stringRep)
		        	  guardLabels.contains(stringRep)
		          } 
		          case _ => false
		        }
		      }
		      //===============================================================
		      // HERE COMES THE PART WITH MAX-SYNC
		      // here we build the combinations to allow something like the label combination  X.moveC, X.moveB 
		      // so always only one combination is possible since from a certain state we can either go left or go right but not both at the same time
		      val combinations = componentStateSortedTransitions.foldLeft(Iterable(Set[((String, String), ftse.formalism.timenet.ImmediateTransitionType, Seq[String])]()))((A,b) => {
		        b._2.flatMap( b=> A map (e => e+b))
		      })
		      
		      println("number of choice combinations is " + combinations.size )
		      // handle the combinations 
		      val outcome = combinations.map(transitions => {
		        //println(transitions.map(_._2.id).mkString("\n"))
		        
		        // TODO: Warning ausbessern ...
		        val labels = transitions collect {
		          case ((net,state),i : ImmediateTransitionTypeFTSE,_) => i.fullLabelRef.map(_.toText).mkString(".")
		        }
		        
		        guardId._2.cndDests flatMap { cndDest =>
		        	val evaluation = (new evalGuardLabelReferences(labels)).eval(cndDest.reactive)
			        //println("destinations " + guardId._2.destinations.toText)
			        //println("evaluation " + evaluation)
			        if (evaluation) {
				        //println("TransitionInCombination: \n" + transitions.mkString("  ","\n  ",""))
				        val next = transitions.foldLeft(current)((c,t) => c.map(e => if (e==t._1) (e._1,t._3.head) else e))
				        //println(next.mkString("NextState:\n","\n","\n------------"))
				        //RG push ((current,next,transitions.first._2))		          
				        if (!todo.contains(next) && !done.contains(next)) todo.push(next)
				        Some((current,next,transitions.head._2))
				    } else None
		        }
		        
		      })
		      //===============================================================
		      
		      outcome
		    }) flatten (a=>a) /*filter (_.isDefined) map (_.get)*/ 
		    
		    //imm.foreach(RG push (_))
		    
		  if (imm.isEmpty) {
			  enabledMTs.map(t => {
		    val next = current.map(e => if (e==t._1) (e._1,t._3.head) else e)
		    RG push ((current,next,t._2))
		    if (!todo.contains(next) && !done.contains(next)) todo.push(next)
		  })
		  
		  }
	  }
	 
	  var statemap = HashMap(
	      // unbedingt den initialstate nach vorne schieben und mit 1 beginnen lassen
	      (done.toList sortWith((a,b) => if (a==initialstate) true else false) zipWithIndex) map (a => (a._1,a._2+1))   :_* 
	  ) 
	  
	  //---------------------------------------------------------
	  /**
	   * HERE YOU CAN SET IF TRA IS ELIMINATED
	   */
	  var modify = eliminate
	  //---------------------------------------------------------
	  
	  while (modify) {
	    val incomingImm = RG collect {case (s,d,t : ImmediateTransitionTypeFTSE) => (s,d,t)} groupBy(_._2)
	    val incomingMkv = RG collect {case (s,d,t : ExponentialTransitionType) => (s,d,t)} groupBy(_._2) 
	    val outgoingImm = RG collect {case (s,d,t : ImmediateTransitionTypeFTSE) => (s,d,t)} groupBy(_._1)
	    
	    val vanishingstates = outgoingImm.keySet  -- incomingImm.keySet
	    
	    val newTransitions = vanishingstates.flatMap(vstate => {
	      //val iM = if (incomingMkv.contains(vstate)) { incomingMkv(vstate)} else {println("warning: no incoming");new scala.collection.immutable.Stack[(scala.collection.immutable.HashMap[String,String],scala.collection.immutable.HashMap[String,String],ftse.formalism.timenet.ExponentialTransitionType)]()}
	    
	      val iM = incomingMkv(vstate) 
	      val oI = outgoingImm(vstate)
	      
	      val sumWeight = oI.foldLeft(0.0)(_ + _._3.weight.toDouble)
	      
	      for (i <- iM; o <-oI) yield {
	    	val oldM = i._3
	        val newRate = (oldM.delay.toDouble * o._3.weight.toDouble / sumWeight) 
	        
	        val newM = ExponentialTransitionType(oldM.graphics, oldM.label, oldM.id, oldM.typeValue, newRate.toString, oldM.serverType, oldM.preemptionPolicy, oldM.DTSPNpriority)
  
	        (i._1,o._2,newM) 
	      }
	    })
	    
	    RG = RG filterNot (a=> (vanishingstates contains (a._1)) | (vanishingstates contains (a._2)))
	    
	    RG = RG ++ newTransitions
	    
	    
	    if (vanishingstates.isEmpty) modify = false else modify = true
	    statemap = statemap filterNot (a=> vanishingstates contains a._1)
	    
	  } 

	  val dotTrans = RG.map(e => statemap(e._1) + "->" + statemap(e._2) + "["+(e._3 match {
			      case i : ImmediateTransitionTypeFTSE => "label=\""+i.guardId._1.map(_.toText).mkString("_") + "_" + i.guardId._2 +"\""
			      case e : ExponentialTransitionType => "label=\"("+e.id.split("_").last+",rate="+e.delay+")\"" + " fontcolor=blue color=blue"
			      case _ => ""	      
	  })+"]")
	  
	  val groupedT = RG.map(t => {
	    t._3 match {
	      case i : ImmediateTransitionTypeFTSE => ftse.formalism.tra.ImmediateTransition(statemap(t._1),i.guardId._1.map(_.toText).mkString("_") + "_" + i.guardId._2, statemap(t._2),1): ftse.formalism.tra.Transition 
	      //case e : ExponentialTransitionType => ftse.formalism.tra.MarkovianTransition(statemap(t._1),"delay" + e.delay, statemap(t._2),e.delay.toDouble) : ftse.formalism.tra.Transition
	      case e : ExponentialTransitionType => ftse.formalism.tra.MarkovianTransition(statemap(t._1), t._3.id.split("_").last, statemap(t._2),e.delay.toDouble) : ftse.formalism.tra.Transition
	    }
	  }).groupBy(_.source)
	  //println(groupedT)	  
	  val tra = ftse.formalism.tra.Tra(groupedT ,Set[Long](statemap.values.toList.map(a => a : Long):_ *))
	  
	  val statesDot = statemap.map(a=> a._2 + " "+a._1.values.mkString("[style=filled shape=\"record\" label=\"{","|","}\"]") )
	  
	  val resstring = dotTrans.foldLeft(
	      //"digraph g {ratio=1.33; node [fontsize = \"16\" shape = \"ellipse\"]; edge [fontsize = \"46\"]; node [fixedsize=true, height=\"3\", width=\"8\", fontsize = \"46\"]; graph [rankdir=LR];"
	      "digraph g {ratio=1.33;node [height=\"2\" width=\"3\" shape = \"ellipse\"]; edge []; graph [nodesep=0.5];"
	      )((a,b)=>a+"\n  "+b)+statesDot.mkString("\n  ","\n  ","\n")+"}"
	  println(resstring)
	  //resstring
	  ftse.tools.Scripting.showDot(resstring)

	  tra
	}
}
