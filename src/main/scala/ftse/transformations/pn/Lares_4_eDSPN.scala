package ftse.transformations.pn

import ftse.formalism.arith._
import ftse.formalism.set._

// import statements for logical expression
import ftse.formalism.logical._
import LE_metamodel._


import ftse.formalism.lares.Lares2Text
import ftse.formalism.lares.LARES_metamodel._

import scala.collection.immutable._
import ftse.tools._

import ftse.formalism.timenet._
import ftse.transformations.lares._
import ftse.transformations._


object L2TNLogger extends Logger {
	def initial = ""
	def finish = ""
}

object Graphics {
	var x = 0
	var y = 0
	var num = 0
	
	def next() = {
		num +=300
		x = num % 1200
		y = num / 1200
	}
	
	def get : (Int,Int) = {
		val dx : Int = (scala.math.random * 200).toInt
		val dy : Int = (scala.math.random * 200).toInt 
		(x+dx,y+dy)
	}
}

object TNLogger extends PetriNetLogger

class L2eDSPNGenerator(x : LARES_Element) extends LaresGenerator with Lares2Text {


	val p = (e:LARES_Element) => {
		L2TNLogger.log("================================================================================")
		L2TNLogger.log(e.toText)
	}

    	  def reduceTimeNet(nets : List[EDSPNNet]) : EDSPNNet = {
	    val place: Seq[PlaceType] = List()
		val exponentialTransition: Seq[ExponentialTransitionType] = List()
		val immediateTransition: Seq[ImmediateTransitionType] = List()
		val deterministicTransition: Seq[DeterministicTransitionType] = List()
		val generalTransition: Seq[GeneralTransitionType] = List() 
		val arc: Seq[ArcType] = List()
		val inhibit: Seq[InhibitType] = List()
		val measure: Seq[MeasureType] = List()
		val definition: Seq[DefinitionType] = List()
		val id : String = "lares"
		val netclass: String = "eDSPN"
		  
	    nets.foldLeft(
			EDSPNNet(
			  place, 
			  exponentialTransition, 
			  immediateTransition, 
			  deterministicTransition, 
			  generalTransition, 
			  arc, 
			  inhibit, 
			  measure, 
			  definition, 
			  id, 
			  netclass
		))((a : EDSPNNet ,b : EDSPNNet)=>{
			EDSPNNet(
			  a.place ++ b.place,
			  a.exponentialTransition ++ b.exponentialTransition,
			  a.immediateTransition ++ b.immediateTransition,
			  a.deterministicTransition ++ b.deterministicTransition,
			  List(),
			  a.arc ++ b.arc,
			  List(),
			  List(),
			  List(),
			  a.id,
			  a.netclass
			)
		})
	  }
    	  
	// define the lares2lares transformation sequence in terms of transformation functions
	val seq = 
			List(
					(e:LARES_Element) => {/*p(e);*/new ClassLaresParameterExpansion(e) transform},
					(e:LARES_Element) => {/*p(e);*/new ClassLaresConditionExpansion(e) resolveCondition},
					(e:LARES_Element) => {/*p(e);*/new ClassLaresForwardExpansion(e) resolveForward},
					(e:LARES_Element) => {p(e);e}
			)
	
	// define the lares2spa transformation function  
	val ttn = (e:LARES_Element) => (new L2EDSPNGenImpl(e)) toTimeNet
		
	def toTN = {
	  val result = (ttn /: seq.reverse) (_ ∘ _)  (x)
		//println(L2TNLogger)
		//println(TNLogger.getDot)
		result		
	}
}

/**
 * PN Expansion Structures (PES)
 */
object PNES {
	type FW = (List[Identifier],List[(List[Identifier],Int,LGuard)])
	type BW = List[EDSPNNet]
}

class L2EDSPNGenImpl(e : LARES_Element) extends L2EDSPNGenTmp {
  def toTimeNet : List[EDSPNNet] = transform(e)
	  
}

class L2EDSPNGenTmp extends 
	LaresTraverser[PNES.BW,PNES.FW] with 
	AbstrTransformer[LARES_Element,List[EDSPNNet]] with 
	LaresGenerator with 
	PNBehTransformer with 
	LogicExpr_algo
{
	
	import PNES._
  
	  class ImplIdentifierInstance( a: Identifier) {
		  def convertToString = a.name + "[" + "]"
	  }
	  
	  implicit def func(a:Identifier) = new ImplIdentifierInstance(a).convertToString

	  
	  def transform(e : LARES_Element) : List[EDSPNNet] = e match {
	 	  case SpecLares(_,_,system : ResolvedInstance) => {
	 		  val forwardresult = (List[Identifier](),List[(List[Identifier],Int, LGuard)]())
			  
			  //L2TNLogger.log(spec.toText)
			  val result = traverse(system,forwardresult)._2
			  result
	 	  }
	  }
	  
	override def forwards(
		i	: AbstrInstance,
		fw	: FW 	 		  
	) : (AbstrInstance,FW) = {
		val m = i.itype.right.get
		val guards = m.body.guards.toList
		val newnamespace = fw._1 :+ i.identifier 
		
		L2TNLogger.log("GUARDS FROM INSTANCE")
		guards.foreach(g => L2TNLogger.log(g.toText))
		L2TNLogger.log("GUARDS FROM UPPER TREE LEVELS")
		fw._2.foreach(g => L2TNLogger.log(g._3.toText))

		val newguards = guards.zipWithIndex.map(g=> 
			(newnamespace,g._2,LGuard(
					g._1.le modify {
						case ReferenceCondition(Some(beh),label) => ResolvedReferenceCondition(newnamespace,beh,label)
						case ResolvedReferenceCondition(ns,beh,label) => ResolvedReferenceCondition(newnamespace ::: ns, beh,label)		
						case sthelse => assert(true);sthelse
					}, 
					g._1.cndDests map { cndReact => 
					  ConditionalReactive(  
						cndReact.cnd map {_ modify {
							case ReferenceCondition(Some(beh),label) => ResolvedReferenceCondition(newnamespace,beh,label)
							case ResolvedReferenceCondition(ns,beh,label) => ResolvedReferenceCondition(newnamespace ::: ns, beh,label)		
							case sthelse => assert(true);sthelse
						}},
					    cndReact.reactive modify {
							case ResolvedReferenceGuardLabel(ns,beh,distrType,label) => ResolvedReferenceGuardLabel(newnamespace ::: ns, beh,distrType,label)
							//TODO case ReferenceGuardLabel(Some(beh),label) => ResolvedReferenceGuardLabel(Stack(newnamespace :_*), beh,label)
							case sthelse => assert(true);sthelse
						})
					}
				)
			)
		)
		
		val fwold = fw._2 /*map(g => (g._1,g._2,LGuard(g._3.le, g._3.destinations.filter(_ match {
			case ResolvedReferenceGuardLabel(ns,beh,label) => ns startsWith newnamespace
		})))) */


		println("newguards")
		println(newguards.mkString)
		
			
		println("altguards")
		println(fwold.mkString)
		
		L2TNLogger.log("NEW GUARDS FROM INSTANCE")
		newguards.foreach(g => L2TNLogger.log(g._3.toText))
		L2TNLogger.log("NEW GUARDS FROM UPPER TREE LEVELS")
		fwold.foreach(g => L2TNLogger.log(g._3.toText))
		
		(i,(newnamespace, fwold ::: newguards))
	}
	  
	 override def backwards(
		i	: AbstrInstance,
		fw	: FW,
		bw	: Iterable[(AbstrInstance,BW)]
	) : (AbstrInstance,BW) = {
		val m = i.itype.right.get
		
		val (ns : List[Identifier], guards) = fw
		val child_pns  = bw.flatMap(_._2)
		
		val translatedMixinBeh = m.mixins collect {
		  case r : ResInherit => t_Behavior(i.identifier, r.assignedId, r.resType, fw)
		}
		
		val result = (i,(translatedMixinBeh ++ (child_pns)).toList  )
		
		result
	}
}


trait PNBehTransformer extends Lares2Text  with ArithExprSerializerImpl {
	def t_Behavior(
	    moduleinstance :  Identifier, 
	    assignedBehId : Identifier, 
	    b : LBehavior, 
	    fwd :  (List[Identifier],List[(List[Identifier],Int, LGuard)])
	) = {
		val (ns,guards) = fwd
		
		println("incommingguards")
		println(guards.mkString)
	
		Graphics.next
		
		val place: Seq[PlaceType] = List()
		val exponentialTransition: List[ExponentialTransitionType] = List()
		val immediateTransition: List[ImmediateTransitionType] = List()
		val deterministicTransition: Seq[DeterministicTransitionType] = List()
		val generalTransition: Seq[GeneralTransitionType] = List() 
		val inhibit: Seq[InhibitType] = List()
		val measure: Seq[MeasureType] = List()
		val definition: Seq[DefinitionType] = List()
		
		val id : String = ns.map(_.toText).mkString("","_","_") +assignedBehId.toText
		
		
		val transitions = b.body.T.toList
		val states = (transitions.map(t => Set(t.from,t.to)).foldLeft(Set(b.body.S.toList :_*))(_ ++ _)).toList

		
		val InitialState = if (b.initial.isDefined) State(b.initial.get) else states.head
		
		
		
		
		val places = states.map(s => {
			val (x,y) = Graphics.get
			val gt = GraphicsType(x,y,0)
			val gtl = GraphicsType(0,0,0)
			val nodeidentifier = id+"_"+s.identifier.toText
			val lt = LabelType(gtl,Text,nodeidentifier+"_label",nodeidentifier)
			if (s == InitialState) PlaceType(gt,lt,nodeidentifier,Node,"1")
			else PlaceType(gt,lt,nodeidentifier,Node,"0")
		})
		
		object GatherB extends Gather[ResolvedReferenceGuardLabel] {
		  def gatherAtom(sth : LExpAtom )= sth match {
		    case r : ResolvedReferenceGuardLabel => Set(r)
		  }
		}
	
		    
		val groupedGuards = guards.flatMap(g => {
		        g._3.cndDests flatMap { cndReact => 
		          val sth = GatherB.gather(cndReact.reactive) filter (dest => (dest.instanceReferences.toList==ns & dest.behaviorReference==assignedBehId))
		  		  sth.map(dest => (dest.namedStatementRef,g))
		        } 		  
		}) groupBy (_._1)
		
		  
		val guarded_transitions = transitions collect {case t : GuardedTransition => t}
		val unguarded_transitions = transitions collect {case t : UnguardedTransition => t}
		
	    val guardedimmediates = guarded_transitions flatMap (t => {
	    	val identifier_source = id+"_"+t.from.identifier.toText
		    val identfier_target = id+"_"+t.to.identifier.toText
		    val guardexpressions = groupedGuards.get(t.guard) map(a=>a)
		    
		    val indexedGuardExpressions = if (guardexpressions.isDefined) {
	    		(guardexpressions.get) zipWithIndex
	    	} else List()

	    	val indexed = indexedGuardExpressions.map(ige => {
	    		val identifier_transition = id+"_"+t.from.identifier.toText+t.to.identifier.toText + "_" +ige 

	    		val gtl = GraphicsType(0,0,0)
	    		
	    		
		    	val lt = LabelType(gtl,Text,identifier_transition+ "_label",identifier_transition)
				
		    	val inscr1 = InscriptionType(gtl, InscriptionText, identifier_transition+"_ST" + "inscr","1")
		    	val inscr2 = InscriptionType(gtl, InscriptionText, identifier_transition+"_TT" + "inscr","1")
		    	
		    	
		    	val toarc = ArcType(
	    			inscr1,
	    			List(),
	    			identifier_source,
	    			identifier_transition,
	    			identifier_transition+"_ST", // SOURCE->TRANSITION
	    			Connector
		    	)
		    	
		    	
		    	
		    	val fromarc = ArcType(
	    			inscr2,
	    			List(),
	    			identifier_transition,
	    			identfier_target,
	    			identifier_transition+"_TT", // TRANSITION->TARGET
	    			Connector
		    	)
		    	
		    	//TNLogger.addTransition(id,identifier_transition,List(identifier_source),None,List(identfier_target), Some(ige._1._2.toString))
		    	
		    	val (x,y) = Graphics.get
	    		val gt = GraphicsType(x,y,0)

		    	val transition : ImmediateTransitionTypeFTSE= ImmediateTransitionTypeFTSE(
		    		gt,lt,
		 	 		identifier_transition,Node,
		 	 		"1","1",(ige._1._2._1,ige._1._2._2),ige._1._2._3, ns ++ List(assignedBehId,t.guard)
		 	 	)
		 	 	
		 	 	(toarc,transition,fromarc)
	    	})
	    	
	  //  	println("indexed")
	  //  	println(indexed.mkString("\n"))
	    	
	    	indexed
	    }) 
	    
//	    println("GI\n"+guardedimmediates.mkString("\n"))
	    
	    val unguarded = unguarded_transitions.map(t => {
	    	val identifier_source = id+"_"+t.from.identifier.toText
		    val identfier_target = id+"_"+t.to.identifier.toText
	    	
		    	
		    	val gtl = GraphicsType(0,10,0)
	    		
		    	
	    		val identifier_transition = id+"_"+t.from.identifier.toText+t.to.identifier.toText
	    	
	    		val inscr1 = InscriptionType(gtl, InscriptionText, identifier_transition+"_ST" + "inscr","1")
		    	val inscr2 = InscriptionType(gtl, InscriptionText, identifier_transition+"_TT" + "inscr","1")
		    	
		    	val lt = LabelType(gtl,Text,identifier_transition,identifier_transition)
				
		    	val toarc = ArcType(
	    			inscr1,
	    			List(),
	    			identifier_source,
	    			identifier_transition,
	    			identifier_transition+"_ST", // SOURCE->TRANSITION
	    			Connector
		    	)
		    	val fromarc = ArcType(
	    			inscr2,
	    			List(),
	    			identifier_transition,
	    			identfier_target,
	    			identifier_transition+"_TT", // TRANSITION->TARGET
	    			Connector
		    	)
		    	
		    	
		    	val (x,y) = Graphics.get
		    	val gt = GraphicsType(x,y,0)

		    	
		    	val transition = t.distr.get match {
	    			case Exponential(rate) => {
	    				ExponentialTransitionType(
				    		gt,lt,
				 	 		identifier_transition,Node,
				 	 		rate.toText,ExclusiveServer,PRD,"1"
				 	 	)
	    			}
	    			
	    			case Dirac(weight) => {
	    				ImmediateTransitionType(gt,lt,identifier_transition, Node, "1", weight.toText, "")
	    			}
	    		}
	    	
	    	
	    	
		 	 	
		 	 	TNLogger.addTransition(id,identifier_transition,List(identifier_source),Some(t.distr.get.toText),List(identfier_target),None)
		    	
		 	 	
		 	 	(toarc,transition,fromarc)
	    	
	    }).toList
	    
		val netclass: String = "type"
			
		val edspn = EDSPNNet(
			  places, 
			  exponentialTransition ::: 
				  unguarded.map(_._2) collect {case e : ExponentialTransitionType => e},
			  immediateTransition ::: 
				  guardedimmediates.map(_._2) ::: 
				  unguarded.map(_._2) collect {case e : ImmediateTransitionType => e},
			  deterministicTransition, 
			  generalTransition, 
			  guardedimmediates.map(_._3) ::: guardedimmediates.map(_._1)::: unguarded.map(_._3) ::: unguarded.map(_._1), 
			  inhibit, 
			  measure, 
			  definition, 
			  id, 
			  netclass
		) 
			
		
		edspn
	}
}

/*
object FORWARDER extends LogicExpr_algo with Lares2Text {
	type FW = (List[Identifier],List[(List[Identifier],Int,LGuard)])
	def forward(
		i	: AbstrInstance,
		fw	: FW,
		B	: HashMap[Identifier,LBehavior], 
		M	: HashMap[Identifier,ModuleDefinition]		 	 		  
	) : FW = {
		val m = M(i.typeid)
		val guards = m.body.guards.toList
		val newnamespace = i.identifier :: fw._1 
		
		/*
		L2TNLogger.log("GUARDS FROM INSTANCE")
		guards.foreach(g => L2TNLogger.log(g.toText))
		L2TNLogger.log("GUARDS FROM UPPER TREE LEVELS")
		fw._2.foreach(g => L2TNLogger.log(g._3.toText))
		*/
		
		val newguards = guards.zipWithIndex.map(g=> 
			(newnamespace,g._2,LGuard(
				g._1.le modify {
					case ReferenceCondition(Some(beh),label) => ResolvedReferenceCondition(newnamespace,beh,label)
					case ResolvedReferenceCondition(ns,beh,label) => ResolvedReferenceCondition(newnamespace ::: ns, beh,label)		
					case sthelse => assert(true);sthelse
				}, 
				g._1.destinations modify {
					case ResolvedReferenceGuardLabel(ns,beh, distrType, label) => ResolvedReferenceGuardLabel(newnamespace ::: ns, beh, distrType, label)
//					case ReferenceGuardLabel(Some(beh),label) => ResolvedReferenceGuardLabel(Stack(newnamespace :_*), beh,label)
					case sthelse => assert(true);sthelse
				})
			)
		)
		
		val fwold = fw._2 /*map(g => (g._1,g._2,LGuard(g._3.le, g._3.destinations.filter(_ match {
			case ResolvedReferenceGuardLabel(ns,beh,label) => ns startsWith newnamespace
		})))) */
		
		
		(newnamespace, fwold ::: newguards)
	}
}
*/