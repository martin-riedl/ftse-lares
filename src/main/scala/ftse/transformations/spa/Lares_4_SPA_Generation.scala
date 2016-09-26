package ftse.transformations.spa

import ftse.formalism.logical._

// import statements for arithmetic expression 
import ftse.formalism.arith._
import ArithExpr_metamodel._

// import statements for set expression 
import ftse.formalism.set._
import SetExpr_metamodel._

import ftse.formalism.lares.LARES_metamodel._
import ftse.formalism.lares.Lares2Text

import ftse.formalism.spa._
import SPA_metamodel._
import ftse.formalism.spa.PACT._

import scala.collection.immutable._
import ftse.tools._

import ftse.transformations.lares._
import ftse.transformations._

import ftse.formalism.lares.{IdentifierSerializer => IS}

/**
 * LARES to SPA Standard Transformation  
 */
class LaresSPATransformer extends 
	LaresTraverser[GEN_SPA_TYPES.BW,GEN_SPA_TYPES.FW] with  
	AbstrTransformer[LARES_Element,SPA_Element] with   
	Composer with 
	DeComposer with 
	SPABehaviorTransformation
 {
	import GEN_SPA_TYPES._
      
	/**
	 * transformation of LARES models to an SPA model
	 * @param e A LARES model
	 * @returns The SPA model   
	 */
	def transform(e : LARES_Element) : SPA_Element = e match {
	    case SpecLares(_,_,system : ResolvedInstance) => {
	      val forwardresult = (List[Identifier](),List[GenerativeLiteral](),List[ReactiveLiteral]())
		  val pact = traverse(system,forwardresult)._2
		  
		  //val dot = (new Serialize(pact)).toDot
		  decompose(pact).definitions.reverse
	    }
	  }
	  
  
	/**
	 * backwards traversal method implementation 
	 * @param i processed module instance 
	 * @param fw determined forward information for the processed module instance   
	 * @param bws backward information of each child instance
	 */
  	def backwards(
			i : AbstrInstance, 
			fw : FW,
			bws : Iterable[(AbstrInstance,BW)]
	): (AbstrInstance,BW) = {
		val (namespace,generativeLiterals,labels) = fw
		val m = i.itype.right.get
		
		//L2SPALogger.log("obtained #satpaths="+satpaths.size + " #numlabels="+labels.size)
		
		val generativeLiteralsGroupedByBeh =  generativeLiterals groupBy (l => l.literal.behaviorReference)
		val reactiveLiteralsGroupedByBeh = labels groupBy (e => e.literal.behaviorReference)
		
		// sequential process translation of all behaviour instances of a module  
		val translatedMixinBeh = m.mixins collect { case i : ResInherit => i } map { i =>
			
		  // constructs the sequential process from a given instance and the relevant  
			BehaviorTransformer.t_Behavior(
					i.assignedId, 
					i.resType,
					generativeLiteralsGroupedByBeh get(i.assignedId) getOrElse List(),
					reactiveLiteralsGroupedByBeh get(i.assignedId) getOrElse List(),
					namespace
			)
		  
		}
		
		// module translation 
		val translatedModule = t_ModuleBody(m.body,namespace, bws)
	   
		//val composed_inherits = composeN(Some(genProcessName(subnamespace)+"_inherits"), translatedMixinBeh)	
		val composed_inherits = composeN(None, translatedMixinBeh)
		
		(i,composeN(Some(genProcessName(namespace)), List(composed_inherits,translatedModule)))
	}
	
	/**
	 * forwards traversal method implementation
	 * @param i processed instance
	 * @param fw forward information obtained from parent module instance
	 * @returns the instance and the therefore calculated forward information  
	 */
	def forwards(
			i : AbstrInstance, 
			forwards : FW
	) : (AbstrInstance,FW) = {
	    // adjust the namespace 
		val namespace = forwards._1 :+ i.identifier
		
		L2SPALogger.log(namespace.map(_.toText).mkString("[","|","]"))
		L2SPALogger.log("forwarder generates " + i.identifier.toText + " for " + namespace.map(_.toText).mkString("","_",""))
				
		// determine generative/reactive literals for the current instance   
		val (generativeLiteral,reactiveLiteral) = determineGuard(i,namespace,forwards._2,forwards._3)
		
		L2SPALogger.log("#satpaths="+generativeLiteral.size + " #numlabels="+reactiveLiteral.size)
		
		// return the (untouched) instance and the associated forward information  
		(i,(namespace,generativeLiteral,reactiveLiteral))	
	}
}


object GEN_SPA_TYPES extends Lares2Text {
	type FW = (List[Identifier],List[GenerativeLiteral],List[ReactiveLiteral])
	type BW = PACT_Element
	
  	case class Encoding(namespace : List[Identifier], guardId : Int, termId : Int, combinationId : Int, distrType : DistributionType) {
  	  val actionlabel = namespace.map(_.toText).mkString("","_","") +"_"+ guardId +"_"+ termId + "_"+ combinationId
  	}
	
	abstract class AbstrLiteral (
				val namespace : List[Identifier], 
				val guardId : Int, 
				val pathId : Int,
				val combination : Int,
				val onlySelfAffecting : Boolean, 
				val distrType : DistributionType,
				val unnegated : Boolean, 
				val literal : ResolvedAbstractReference
	) {
	  val enc = Encoding(namespace,guardId,pathId,combination, distrType)
	}
	
	case class GenerativeLiteral (				
				override val namespace : List[Identifier], 
				override val guardId : Int, 
				override val pathId : Int, 
				override val combination : Int,
				override val onlySelfAffecting : Boolean, 
				override val distrType : DistributionType,
				override val unnegated : Boolean,
				override val literal : ResolvedReferenceCondition
	) extends AbstrLiteral(namespace,guardId,pathId,combination,onlySelfAffecting,distrType, unnegated, literal)
		
	case class ReactiveLiteral(
			override val namespace : List[Identifier], 
			override val guardId : Int, 
			override val pathId : Int, 
			override val combination : Int,
			override val onlySelfAffecting : Boolean,
			override val distrType : DistributionType,
			override val unnegated : Boolean,
			override val literal : ResolvedReferenceGuardLabel
	) extends AbstrLiteral(namespace,guardId,pathId,combination,onlySelfAffecting,distrType, unnegated, literal)
	
	/**
	 * filters the relevant literals for the given instance 
	 */
	def nextLayer[T<:AbstrLiteral](paths : List[T], namespace : List[Identifier]) = 
		paths filter (elem => (elem.namespace ::: elem.literal.instanceReferences) startsWith namespace)
			
	def s2s(s : List[Identifier]) : String  = s.map(e=>e.toText).mkString("[","|","]")
	
	def genProcessName(ns : List[Identifier]) = {
		ns.map(genIdentifierName(_)).mkString("","_","")
	}
  
	def genIdentifierName(a : Identifier) = a.name + a.indices.map(a => (new ArithExprSerializer(a)).toText).mkString("","_","")

	implicit def toStringGenerativeLiteral(a : GenerativeLiteral) = s2s(a.namespace) + "("+a.guardId+"|"+a.pathId+")" + s2s(a.literal.instanceReferences) + a.literal.behaviorReference .toText + "." + a.literal.namedStatementRef.toText 
	implicit def toStringReactiveLiteral(a : ReactiveLiteral) = s2s(a.namespace) + "("+a.guardId+"|"+a.pathId+")" + a.literal
}

trait SPABehaviorTransformation  extends 
	Composer with 
	DeComposer with 
	SPA with 
	LogicExpr_algo with
	ArithExpr_algo with 
	Lares2Text  with ImplNameSpace
{
  import GEN_SPA_TYPES._
		
	  /**
   * Transforms the body of a module definition 
   * @param body
   * @param B
   * @param M
   * @param 
   * @return
   */
  def t_ModuleBody(
		  body : ModuleBody,
		  namespace : List[Identifier],
		  subinst : Iterable[(AbstrInstance,PACT_Element)]
	) = {
	  val resolvedbody = body // resolve_ModuleBody(body,HashMap())
	 
	  //composeN(Some(genProcessName(namespace)+"_body"), subinst.map(_._2))
	  composeN(None,subinst.map(_._2))
  }
	
	
	/**
	 * Helper Function to Calculate the Label and Satpath Entries for a guards that have to be forwarded
	 */
	def determineGuard(
			i : AbstrInstance, 
			namespace : List[Identifier],
			generativeLiterals : List[GenerativeLiteral],
			reactiveLiterals : List[ReactiveLiteral]
	) : (List[GenerativeLiteral],List[ReactiveLiteral]) =  
	{
	  //TODO: This function needs a rewrite ... generative and reactive parts can be handled independently and combined in a subsequent step, instead of what is done here 
	  val m = i.itype.right.get
	  val body = m.body 
	  
	  /* filter the relevant literals for the given instance */
	  val relevantGenerativeLiterals  = nextLayer(generativeLiterals,namespace)
	  val relevantReactiveLiterals = nextLayer(reactiveLiterals,namespace)
	  
	  // for each guard, move the generative part into the conditional reactive
	  // and collect the modified conditional reactives
	  val condReactives = body.guards.map(guard => guard.cndDests collect{
	    case cndDst :TypedConditionalReactive => TypedConditionalReactive(
	        Some(cndDst.cnd.map(_ & guard.le).getOrElse(guard.le)), 
	        cndDst.reactive,
	        cndDst.distr
	    )  
	  }).flatten
	  
	  val guardStructure : Iterable[(
		      scala.collection.immutable.Set[ftse.transformations.spa.GEN_SPA_TYPES.GenerativeLiteral], 
		      scala.collection.immutable.Set[ftse.transformations.spa.GEN_SPA_TYPES.ReactiveLiteral]
		  )] = (condReactives zipWithIndex) flatMap(enumCR=> {
		    val (cndReactive, reactiveNumber) = enumCR
		  	
		  // check if guard is typed 
		  val distr : DistributionType = cndReactive match {
		    case TypedConditionalReactive(_,_,distr) => distr
		    //case _ => assert(false,"no distribution type determined")
		  }
		  
		  // generate the BDD for the generative expression 
 	 	  val generativeBDD = new LogicExprBdd(cndReactive.cnd.get)
 	 	  
 	 	  // if the generative expression is false, i.e. its root is the false leaf in the MTBDD
 	 	  // then no lists of satisfiable minterms shall be calculated   
 	 	  val (variables, generativeMinterms) = if (generativeBDD.root != 0) generativeBDD.satTerms() else (List(),List())
		    
		  println("VARIABLES/PATHS: " + variables.size+ "/"+generativeMinterms.size)
		  println(generativeMinterms)
		  //L2SPALogger.log("VARIABLES/PATHS: " + variables.size+ "/"+terms.size)
		   
		  // ===================================================================
		  // checking whether a minterm violates the criteria of not being 
		  // in two states at the same time within one instantiated behavior
		 
		  val checkedGenerativeMinterms = generativeMinterms.filter(path => { // Martin August 2013
		  
		    // generate all literall combinations within a path
		    val tt = for (x <- path; y <- path if x._2==true && y._2==true) yield (x._1,y._1)
		    val tf = for (x <- path; y <- path if x._2==true && y._2==false) yield (x._1,y._1)
		   
		    // check whether both entries within a tuple of all tuples 
		    // conform concerning their behavior instance  
		    // and differ concerning their given state
		    val result_tt = tt.forall(_ match {
		      case (ResolvedReferenceCondition(instx,behx,statex),ResolvedReferenceCondition(insty,behy,statey)) 
		      	if instx==insty && behx==behy && statex!=statey => /*println(statex.toText, statey.toText);*/false
		      case _ => true
		    })
		    
		    val result_tf = tf.forall(_ match {
		      case (ResolvedReferenceCondition(instx,behx,statex),ResolvedReferenceCondition(insty,behy,statey)) 
		      	if instx==insty && behx==behy && statex==statey => /*println(statex.toText, statey.toText);*/false
		      case _ => true
		    })
		    
		    
		   // if (!result) println("exclude invalid " + path.map(l => (if (l._2) "" else "!") + (new LARESLogicExpr2Text(l._1)).toText).mkString("&"))
		    // only then a path is valid
		    result_tt && result_tf
		  })
		  
		  // ===================================================================
		  
		  
		  // determine all resolved state reference literals (FAILURE if there is still an unresolved one!!!) 
		  val resolvedTerm = checkedGenerativeMinterms.map(_.map(_ match {
		    case (r : ResolvedReferenceCondition,b) => (r,b)
		  }))
		  		  
 	 	  // iterate over all enumerated resolved state reference terms  
 	 	  val transformedLabel = (resolvedTerm zipWithIndex) flatMap (enumeratedTerm => {
 	 	    val term = enumeratedTerm._1
		    val termNumber = enumeratedTerm._2
		    
		    val termAffectedBehaviorInstances = term.map(l => (l._1.instanceReferences,l._1.behaviorReference)).toSet
		    
		    // ---------------------------
		    // Alternative 1
		    //val djLabels = resolvedLabels.reduce((a : LogicalExpr,b : LogicalExpr) => Disjunct(a,b))
		    //val bddLabels = new LogicExprBdd(djLabels)
		    //TODO: Martin
		    val bddLabels = new LogicExprBdd(cndReactive.reactive)
 	 	    
 	 	    //val (vLabels, vTerms) = if (bddLabels.root != 0) bddLabels.satTerms() else (List(),List())
 	 	    //val (vLabels, vTerms) = bddLabels.satTerms()
		    //val (vLabels, vTerms) = bddLabels.satMinTerms()
		    val (vLabels, vTerms) = if (bddLabels.root != 0) bddLabels.satMinTerms() else (List(),List())
		    
		    val resolvedCombinations=vTerms.map(vterm => 
		      vterm.map(_ match { 
		        case (ResolvedReferenceGuardLabel(ns,id,None,z), b : Boolean) => Some((ResolvedReferenceGuardLabel(ns,id,Some(distr),z),b))
		        case (l : ResolvedReferenceGuardLabel, b : Boolean) => Some((l,b)) 
		        case _ => None
		      }).filter(_.isDefined).map(_.get).toSet
		    ).toSet
		    
		    (resolvedCombinations zipWithIndex) map (enumeratedCombination => {
		      val combination = enumeratedCombination._1
		      val combinationNumber = enumeratedCombination._2
		      
		      val AffectedBehaviorInstances = termAffectedBehaviorInstances ++ combination.map(l => (l._1.instanceReferences,l._1.behaviorReference))
		      val selfAffecting = AffectedBehaviorInstances.size<2
		      
		      val seperatedTermLiteral = term.map(l => 
		        GenerativeLiteral(namespace,reactiveNumber,termNumber,combinationNumber, selfAffecting, distr, l._2, l._1)
		      ).toSet
		      
		      val seperatedCombinationLiteral = combination map (l => 
		        ReactiveLiteral(namespace, reactiveNumber,termNumber, combinationNumber, selfAffecting, distr, l._2, l._1)
		      )
		      
		      /*
		      val sep1String = seperatedTermLiteral.map(a=> {
		        ""+(if (a.unnegated) "" else "!") 		+ 
		        (if (a.literal.instanceReferences.size>0) a.literal.instanceReferences.last.toText else a.literal.behaviorReference.toText)  + 
		        "." + a.literal.namedStatementRef.name
		      }).mkString("("," & ",")")
		      
		      
		      val sep2String = seperatedCombinationLiteral.map(a=> {
		        ""+ (if (a.unnegated) "" else "!") +
		        (if (a.literal.instanceReferences.size>0) a.literal.instanceReferences.last.toText else a.literal.behaviorReference.toText) + 
		        "." + a.literal.namedStatementRef.name
		      }).mkString("("," & ",")") 
		        
		      
		      println("TERM/COMBINATION: " + 
		           sep1String + sep2String +  
		          (reactiveNumber, termNumber, combinationNumber)
		      )*/
		      
		      (seperatedTermLiteral,seperatedCombinationLiteral)
		    })		    
		  }) 
		
	 	  L2SPALogger.log("TRANSFORMEDLABEL:")
	 	  L2SPALogger.log(transformedLabel.toString)
	 	  
	 	  transformedLabel
	  })
	 	  
	  val pathlist = guardStructure.flatMap(a=> a._1)
	  	  
	  val labels = guardStructure.flatMap(a=> a._2) 
	  	  
	  val newpaths = pathlist ++   relevantGenerativeLiterals
	  val newlabels = labels ++ relevantReactiveLiterals
	  (newpaths.toList,newlabels.toList)
	}

}


object BehaviorTransformer extends 
	Composer with 
	DeComposer with 
	SPA with 
	LogicExpr_algo with
	ArithExpr_algo with 
	Lares2Text with 
	ImplNameSpace with ArithExprSerializerImpl 
  {
	import GEN_SPA_TYPES._
	
	/**
	 * Creates a specific SPA action depending on a given distribution and label
	 */
	def matchDistr(label : String, distr : Option[Distribution], distrType : DistributionType) = distr match {
		case Some(Exponential(rate : ArithExpr)) => MA(label,rate.toText)
		case Some(Dirac(weight : ArithExpr)) => IA(label,weight.toText)
		case _ => distrType match {
		  case ExponentialType => MA(label,1.0)
		  case DiracType => IA(label,1.0)  
		}
	}
	
	/**
	 * Transforms a behavior instantiated within a given module instance
	 */
    def t_Behavior(
    	  assignedId : Identifier,
		  b : LBehavior,
		  generativeLiterals : List[GenerativeLiteral],
		  reactiveLiterals : List[ReactiveLiteral],
		  namespace : List[Identifier]
	) : PAT = {
		val processLabel = genProcessName(namespace :+ assignedId)
		
		// the list of transitions
		val T = b.body.T.toList
		// transitions grouped by their source node
		val TbyS = T groupBy (t=>t.from.identifier)
		
		val T_G = T.map(_ match {case t : GuardedTransition => Some(t);case t : UnguardedTransition => None}) filter (_.isDefined) map (_.get) groupBy (t=> t.guard)
		val t_G = T.map(_ match {case t : GuardedTransition => Some(t);case t : UnguardedTransition => None}) filter (_.isDefined) map (_.get) groupBy (t=>t.from.identifier)
		
		// the set of states
		val S = (T.map(t => Set(t.from.identifier,t.to.identifier)).foldLeft(Set(b.body.S.map(_.identifier).toList :_*))(_ ++ _))
		// mapping state numbers to states <State --> Int>
		val statesmap = HashMap(S.toList.zipWithIndex :_*) 
		
		// mapping all outgoing transitions of a state <State --> List[Transition]>
		val d = T.groupBy(t=>t.from)
		
		// group literals by id
		val L_g_grouped = generativeLiterals groupBy(l => l.enc)
		val L_r_grouped =  reactiveLiterals groupBy(l => l.enc)
		
		

		
		
			
		val labelstobesynchronized = (reactiveLiterals ::: generativeLiterals).filter(a=>a.onlySelfAffecting==false).map(a=> (a.enc))  
		println("L Sync : "+labelstobesynchronized.map(l=>l.actionlabel).toList.sortWith((a,b)=>a<b).mkString("|"))
		
		val labelsNOTtobesynchronized = (reactiveLiterals ::: generativeLiterals).filter(a=>a.onlySelfAffecting==true).map(a=> (a.enc))  
		println("L NOT Sync : "+labelsNOTtobesynchronized.map(l=>l.actionlabel).toList.sortWith((a,b)=>a<b).mkString("|"))
		
		val guardProcesses = (L_r_grouped.keys ++ L_g_grouped.keys).toList.flatMap(gid => {
		    println("      " + gid)
			val TermLiteral = L_g_grouped.getOrElse(gid,List()).toSet.toList
			val CombinationLiteral = L_r_grouped.getOrElse(gid,List()).toSet.toList
			
			val TermLiteral_true = TermLiteral.filter(tL => tL.unnegated==true).map(l=>l.literal.namedStatementRef)
			val TermLiteral_false = TermLiteral.filter(tL => tL.unnegated==false).map(l=>l.literal.namedStatementRef)
			val CombinationLiteral_true = CombinationLiteral.filter(_.unnegated==true).map(l=>l.literal.namedStatementRef)
			val CombinationLiteral_false = CombinationLiteral.filter(_.unnegated==false).map(l=>l.literal.namedStatementRef)

			
			def prooveLabelPreCond(s : Identifier) = {
			  CombinationLiteral_true.forall(l => t_G.getOrElse(s,List()).find(_.guard == l).isDefined) & 
			  CombinationLiteral_false.forall(l => ! t_G.getOrElse(s,List()).find(_.guard == l).isDefined) 
			}
			
			// states with prooven guard label condition
			def S_LC = S filter (prooveLabelPreCond(_))
			
			// AFFECTED BY A UNNEGATED TERM LITERAL
			// - affected by a unnegated combination literal
			val A_uu = (for {
			  s <- (TermLiteral_true.toSet & S_LC).toList
			  t <- t_G.getOrElse(s,List()).filter(CombinationLiteral_true contains _.guard) 
			} yield {
			  //println("A_uu +=  " + t.from.identifier.toText + " --" +gid.actionlabel+"--> " + t.to.identifier.toText)
			  //GuardedProcess(G("state",statesmap(t.from.identifier)), IA(gid.actionlabel,1.0) / I(processLabel,statesmap(t.to.identifier)))
			  val distr = if (t.distr.isDefined) Left(t.distr.get) else Right(gid.distrType)
			  GuardedProcess(G("state",statesmap(t.from.identifier)), matchDistr(gid.actionlabel,t.distr,gid.distrType) / I(processLabel,statesmap(t.to.identifier)))
			}) 
			
			// - affected by a negated combination literal
			val A_un = (for {
			  s <- (S_LC & TermLiteral_true.toSet).toList  if CombinationLiteral_true.isEmpty if CombinationLiteral_false.nonEmpty
			} yield {
			  //println("A_un +=  " + s.toText + " --" +gid.actionlabel+"--> " + s.toText)
			  GuardedProcess(G("state",statesmap(s)), matchDistr(gid.actionlabel,None,gid.distrType) / I(processLabel,"state"))
			})
			
			// - no combination literal 
			val A_ue = (for {
			  s <- TermLiteral_true if CombinationLiteral.isEmpty 
			} yield {
			  //println("A_ue +=  " + s.toText + " --" +gid.actionlabel+"--> " + s.toText)
			  GuardedProcess(G("state",statesmap(s)), matchDistr(gid.actionlabel,None,gid.distrType) / I(processLabel,"state"))
			})
			
			// AFFECTED BY A NEGATED TERM LITERAL
			// - affected by a unnegated combination literal
			val A_nu = (for {
			  s <- (S_LC -- TermLiteral_false).toList if TermLiteral_false.nonEmpty if TermLiteral_true.isEmpty 
			  t <- t_G.getOrElse(s,List()).filter(CombinationLiteral_true contains _.guard ) 
			} yield {
			  //println("A_nu +=  " + t.from.identifier.toText + " --" +gid.actionlabel+"--> " + t.to.identifier.toText)
			  GuardedProcess(G("state",statesmap(t.from.identifier)), matchDistr(gid.actionlabel,t.distr,gid.distrType) / I(processLabel,statesmap(t.to.identifier)))
			})
			 
			// - affected by a negated combination literal
			val A_nn = (for {
			  s_sat_cL <- (S_LC -- TermLiteral_false).toList if TermLiteral_false.nonEmpty if TermLiteral_true.isEmpty  if CombinationLiteral_true.isEmpty if CombinationLiteral_false.nonEmpty 
			} yield {
			  //println("A_nn +=  " + s_sat_cL.toText + " --" +gid.actionlabel+"--> " + s_sat_cL.toText)
			  GuardedProcess(G("state",statesmap(s_sat_cL)), matchDistr(gid.actionlabel,None,gid.distrType) / I(processLabel,"state"))
			})
			
			// - no combination literal 
			val A_ne = (for {
			  s <- (S.toSet -- TermLiteral_false).toList if TermLiteral_false.nonEmpty if TermLiteral_true.isEmpty if CombinationLiteral.isEmpty 
			} yield {
			  //println("A_ue +=  " + s.toText + " --" +gid.actionlabel+"--> " + s.toText)
			  GuardedProcess(G("state",statesmap(s)), matchDistr(gid.actionlabel,None,gid.distrType) / I(processLabel,"state"))
			})
			
			// NOT AFFECTED BY A TERM LITERAL
			// - affected by a unnegated combination literal
			val A_eu = (for {
			  s <- S_LC.toList if TermLiteral.isEmpty 
			  t <- t_G.getOrElse(s,List()).filter(CombinationLiteral_true contains _.guard ) 
			} yield {
			  println("A_en +=  " + t.from.identifier.toText + " --" +gid.actionlabel+"--> " + t.to.identifier.toText)
			  GuardedProcess(G("state",statesmap(t.from.identifier)), matchDistr(gid.actionlabel,t.distr,gid.distrType) / I(processLabel,statesmap(t.to.identifier)))
			}) 
			
			// - affected by a negated combination literal
			val A_en = (for {
			  s <- S_LC.toList  if CombinationLiteral_true.isEmpty if CombinationLiteral_false.nonEmpty  if TermLiteral.isEmpty    
			} yield {
			  //println("A_en +=  " + s.toText + " --" +gid.actionlabel+"--> " + s.toText)
			  GuardedProcess(G("state",statesmap(s)), matchDistr(gid.actionlabel,None,gid.distrType) / I(processLabel,statesmap(s)))
			}) 

			A_uu ::: A_un ::: A_nn :::  A_nu ::: A_eu ::: A_en ::: A_ue ::: A_ne
		}) ::: (S.toList.flatMap(state => {
		  // generate all markovian transitions
		  val unguardedTransitions = TbyS.getOrElse(state,List()) map(_ match {case t : GuardedTransition => None;case t : UnguardedTransition => Some(t)}) filter (_.isDefined) map (_.get)
		  
		  unguardedTransitions.flatMap(t => 
		    List(GuardedProcess(
		    	G("state",statesmap(state)),
		    	//matchDistr((slc(t.from.identifier.toText)+slc(t.to.identifier.toText)),t.distr,t.distr.get.distrType) / 
		    	matchDistr(IS.plainType(t.from.identifier)+IS.plainType(t.to.identifier),t.distr,t.distr.get.distrType) /
		    	I(processLabel ,statesmap(t.to.identifier)))
		   ))
		}))
		
		// active synch list      
		val synchset = labelstobesynchronized.map(a=>IA(a.actionlabel,1))
		
		//L2SPALogger.log("|synchset|=" + synchset )
		
		// determine transitions that are assigned to a guard && determine all unguarded transitions
		/*val (guardedT : Iterable[GuardedTransition], unguardedT : Iterable[UnguardedTransition]) = 
			T partition (_.isInstanceOf[GuardedTransition])
		*/
		val guardedT = T collect {case gt : GuardedTransition => gt}
		val unguardedT = T collect {case ut : UnguardedTransition => ut}
		
		val beh_state : PPD = ("state",S.size-1)
		
		
		/******************************************************/
		/*
		 * HACK!!!!!!
		 * CASPA can't build a BDD if there is no defined behavior for the first guarded parameter
		 * allerdings werden nun tatsächlich alle werte der domaene aus den gegebenen parameter im bdd codiert
		 * Anmerkung bzgl CASPA SPA Semantik: Axiom     stop + P = P
		 */
		
		val LARES_Behaviour_as_SPA = (processLabel , List(beh_state)) := GuardedProcess(Gall, Stop) :: guardProcesses
		
		/******************************************************/
		
		//println(LARES_Behaviour_as_SPA)
		
		val active = Set[Action]()   
		val passive_s = Set[Action]() ++ synchset
		val passive_u = Set[Action]() 
		
		val initialState = if (b.initial.isDefined) statesmap(b.initial.get) else 0
		
		val constantdefs = statesmap.map(e => ConstantDefinition(processLabel + "_" + IS.plainType(e._1), ArithAtomValue(Left(e._2)))).toList
		
		//println("CONSTANTDEFS\n" + constantdefs)
		
		if (guardProcesses.isEmpty) {
		    PAT(Stop, SynchSets(active,passive_s,passive_u), List() ++ constantdefs)
		} else {
			PAT(
				I(processLabel, initialState),
			    SynchSets(active,passive_s,passive_u), 
			    constantdefs ++ List(LARES_Behaviour_as_SPA)
			)	  
		}
	}
    
}



/**
 * Implicit text conversion trait for namespace data-structures
 */
trait ImplNameSpace {
	implicit def toText(namespace : List[Identifier]) = new NameSpace(namespace)
	class NameSpace(namespace : List[Identifier]) extends Lares2Text {
		def toText = namespace.map(_.toText).mkString("","_","")	
	}
}


/**
 * SPA logger class
 */
object L2SPALogger extends Logger {
	def initial = ""
	def finish = ""
}


