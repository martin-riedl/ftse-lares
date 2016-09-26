package ftse.formalism.lares

import scala.util.parsing.combinator._
import ftse.formalism._
import scala.collection.immutable._

import ftse.formalism.arith._
import ftse.formalism.set._
import ftse.formalism.logical._

import LARES_metamodel._

/**
 * Lares Iterator Parser
 */
trait LaresIter_Parser extends SetExpr_parser {
  def Iterator_PC : Parser[Option[Iterator]] = {
	ident ~"in"~ SetExpr_PC  ^^ {
		case id ~ _ ~ Some(expr) => Some(Iterator(id,expr))
		case _ => None
	}
  }	
}

/**
 * Lares Expand Parser
 */
trait LaresExpand_Parser extends LaresIter_Parser {
	def Range_PC : Parser[LARES_metamodel.Range] =
	  ArithExpr_PC ~ ".." ~ ArithExpr_PC ^^ {
	  	case start ~_~end => LARES_metamodel.Range(start,end)
	  }
	  
}

/**
 * Lares Identifier Parser
 */
trait LaresIdentifier_Parser extends ArithExpr_Parser {
 
 def IdentifierGuardLabel_PC : Parser[Option[Identifier]] =
	  "<" ~ Identifier_PC ~ ">"  ^^ {
	  case _ ~ id ~ _ => id
	  case sthelse => assert(false,"could not parse IdentifierGuardLabel: " +sthelse);None
  }
 
  def Identifier_PC : Parser[Option[Identifier]] =
	ident ~ opt("[" ~> rep1sep(ArithExpr_PC,",") <~ "]") ^^ {
	    case name ~ Some(indices) => Some(Identifier(name,indices )) 
	    case name ~ None => Some(Identifier(name,List()))
		case sthelse => assert(false,"could not parse Identifier: " +sthelse);None
	}	
    
  def Parameter_PC : Parser[Option[Parameter]] =
	ident ~ opt("=" ~> ArithExpr_PC) ^^ {
    	case paramId ~ defined => Some(Parameter(paramId,defined))
    	case _ => None
	}

  def Parameters_PC : Parser[List[Parameter]] = opt("(" ~> rep1sep(Parameter_PC,",") <~")") ^^ {
    case Some(params) => params filter(_.isDefined) map(_.get)
    case None => List()
  }
}

/**
 * Lares Conditional Reactive Parser
 */
trait LaresConditionalReactive_Parser extends 
  LaresReferenceIdentifier_Parser with 
  LaresReferenceCondition_Parser
{
  
    def ConditionalReactive_PC  : Parser[ConditionalReactive] =  GLR_PC ~ opt("if " ~ B_Expr_PC) ^^ {
      case dest ~ optCnd => ConditionalReactive(optCnd.map(_._2).getOrElse(None), dest)
    }
}

/**
 * Lares Reference Parser
 */
trait LaresReferenceIdentifier_Parser extends 
	ArithExpr_Parser with 
	LaresIdentifier_Parser with 
	LaresExpand_Parser
{
	
  import LE_metamodel._

   
  def DefinitionRef_PC : Parser[Option[ReferenceDefinition]] = 
	  Identifier_PC ~ opt("(" ~ rep1sep(Parameter_PC,",") ~")") ~ opt("[" ~ rep1sep(Range_PC,",")  ~ "]")  ^^ { 
    case _  => None
  }
  
    
  def GLR_PC : Parser[ReferenceGuardLabelAtom] = {
     rep1sep(ReferenceGuardLabel_PC,",") ^^ {
       case a :: Nil => a.get
       case a  => MAXSYNC(a.collect {case Some(bla) => bla})
     }
  }
  
  /**
   * parses guard label references 
   */
  def ReferenceGuardLabel_PC : Parser[Option[ReferenceGuardLabelAtom]] = 
      ("sync"|"maxsync"|"choose") ~ "(" ~ rep1sep(Iterator_PC,",") ~ ")" ~ "{" ~ rep1sep(ReferenceGuardLabel_PC,",") ~ "}" ^^ {
         case "sync" ~ _ ~ iterators ~ _  ~ _ ~ labels ~ _ =>  Some(SYNC_Expand(iterators.map(_.get),ReferenceGuardLabelBody(labels.map(_.get))))  
         case "maxsync" ~ _ ~ iterators ~ _  ~ _ ~ labels ~ _ =>  Some(MAXSYNC_Expand(iterators.map(_.get),ReferenceGuardLabelBody(labels.map(_.get))))             
         case "choose" ~ _ ~ iterators ~ _  ~ _ ~ labels ~ _ =>  Some(CHOOSE_Expand(iterators.map(_.get),ReferenceGuardLabelBody(labels.map(_.get))))
      } |
      ("sync"|"maxsync"|"choose") ~ "{" ~ rep1sep(ReferenceGuardLabel_PC,",") <~ "}" ^^ {
	    case "sync" ~ _ ~ lrgl => Some(SYNC(lrgl.collect{case Some(lexp) => lexp}))
	    case "maxsync" ~ _ ~ lrgl  => Some(MAXSYNC(lrgl.collect{case Some(lexp) => lexp}))
	    case "choose" ~ _ ~ lrgl  => Some(CHOOSE(lrgl.collect{case Some(lexp) => lexp}))
      } |
      "(" ~ rep1sep(Iterator_PC,",") ~ ")" ~ ReferenceGuardLabel_PC ^^ {
        case _ ~ iterators ~ _  ~ label => Some(ReferenceGuardLabel_Expand(iterators.map(_.get),ReferenceGuardLabelBody(List(label.get))))
        case sthelse => throw new Exception("ReferenceGuardLabel_PC: " +sthelse);None
      } |
	  // remote guard label references
      Identifier_PC ~ "." ~ IdentifierGuardLabel_PC ^^ {
	  	case inst ~ _ ~ Some(guardl) =>  Some(ReferenceGuardLabel(inst,guardl))	  
	  	case sthelse => throw new Exception("ReferenceGuardLabel_PC: " +sthelse);None
  	  } |
  	  // local guard label references
  	  IdentifierGuardLabel_PC ^^ {
	  	case Some(guardl) =>  Some(ReferenceGuardLabel(None,guardl))	  
	  	case sthelse => throw new Exception("ReferenceGuardLabel_PC: " +sthelse);None
  	  }

  /**
   * parses condition label references 
   */
  def ReferenceCondition_PC : Parser[Option[ReferenceCondition]] = {
	  Identifier_PC ~ "." ~ Identifier_PC ^^ {
	 	  case optIdInst ~ _ ~ Some(idCond) => Some(ReferenceCondition(optIdInst, idCond))
	 	  case sthelse => throw new Exception("ReferenceCondition_PC: " +sthelse);None
	  }	|   
	  opt(".") ~ Identifier_PC ^^ {
	 	  case _ ~ Some(idCond) => Some(ReferenceCondition(None, idCond))
	 	  case sthelse => throw new Exception("ReferenceCondition_PC: " +sthelse);None
	  }	  
  }
}

/**
 * Lares Behavior Parser
 */
trait LaresBehavior_Parser extends ArithExpr_Parser with LaresIdentifier_Parser with LaresExpand_Parser{
  
  def Behavior_PC : Parser[Option[LBehavior]] = 
    "Behavior" ~ Identifier_PC ~ Parameters_PC ~  BehaviorBody_PC ^^ {
      case _ ~ Some(identifier) ~ params ~ Some(body) => Some(LBehavior(identifier,params,body,None)) 
      case _ => None
  	}
  
  def BehaviorBody_PC : Parser[Option[BehaviorBody]] = 
	  "{" ~ rep(States_PC | Transitions_PC | Constants_PC | ExpandBehavior_PC) ~ "}" ^^ {
	  	case _ ~ be ~ _ => {
	  		val elements = be.flatten(a=>a)
	  		val T = elements collect {case Some(t : Transition) => t}
	  		val S = elements collect {case Some(t : State) => t}
	  		val C = elements collect {case Some(t : ConstantDef) => t}
	  		val fstmt = elements collect {case Some(t : ExpandBehaviorBody) => t}
	  		
	  		// assure that implicitely defined states are made explicit 
	  		val states = Set(S ++ T.map(t=> t.from) ++ T.map(t=> t.to) :_*)
	  		Some(BehaviorBody(states,T,C,fstmt))
	  	}
	  	
	  	case _ => None
      }

  def ExpandBehavior_PC : Parser[List[Option[Expand[BehaviorBody]]]] = {
	  "expand" ~ "(" ~ rep1sep(Iterator_PC,",") ~ ")" ~ BehaviorBody_PC ^^ {
	 	  case _ ~ _ ~ iterators ~ _ ~ Some(body) => {
	 	 	  val iters = iterators.filter(_.isDefined).map(_.get)
	 	 	  //val x = ExpandBehaviorBody(iters,body)
	 	 	  List(Some(ExpandBehaviorBody(iters,body)))
	 	  }
	 	  case sthelse => /*println(sthelse);*/List()
	  } 
  }
    
  def States_PC : Parser[List[Option[State]]] = "State" ~ repsep(Identifier_PC,",") ^^ {
    case _ ~ labels => labels.filter(_.isDefined).map(label => Some(State(label.get)))
  }
  
  def Transitions_PC : Parser[List[Option[Transition]]] = "Transitions" ~ "from" ~ Identifier_PC ~ 
    rep(opt("if" ~ "<" ~ Identifier_PC ~ ">") ~  "->" ~ Identifier_PC ~ opt("," ~ Distribution_PC)) ^^ {
      case _ ~ _ ~ Some(from) ~ to => {
        to.map(dest => dest match {
          case None ~ _ ~ Some(dest)  ~ Some(_ ~ distr)  => Some(UnguardedTransition(State(from), State(dest), distr))
          case None ~ _ ~ Some(dest)  ~ None  => Some(UnguardedTransition(State(from), State(dest), None))
          case Some(_ ~ _ ~ Some(guard) ~ _) ~ _ ~ Some(dest)  ~ Some(_ ~ distr) if guard.name=="true"  => Some(UnguardedTransition(State(from), State(dest), distr))
          case Some(_ ~ _ ~ Some(guard) ~ _) ~ _ ~ Some(dest)  ~ Some(_ ~ distr) => Some(GuardedTransition(State(from),State(dest),guard, distr))
          case Some(_ ~ _ ~ Some(guard) ~ _) ~ _ ~ Some(dest)  ~ None => Some(GuardedTransition(State(from),State(dest),guard, None))
          case _ => throw new Exception("could not parse transitions" + dest);None
          }
        )        
      }
      case _ => List()
    }

  def Constants_PC : Parser[List[Option[ConstantDef]]] = rep1sep(ConstantDef_PC,";") ^^ { 
	  case constants => constants
	  
  }
  
  def ConstantDef_PC : Parser[Option[ConstantDef ]] = "constant" ~ Identifier_PC ~ "=" ~ ArithExpr_PC ^^ {
    case _ ~ Some(id) ~ _ ~ ae => Some(ConstantDef(id,ae))
    case _ => None
  } 
   
  def Distribution_PC : Parser[Option[Distribution]] = ExpDistr_PC | DiracDistr_PC ^^ { 
    case dist => dist
  }
  
  def ExpDistr_PC : Parser[Option[Exponential]] = "delay" ~ "exponential" ~> ArithExpr_PC ^^ { 
    case ae  => Some(Exponential(ae))
  }
 	 
  def DiracDistr_PC : Parser[Option[Dirac]] = "weight" ~> ArithExpr_PC ^^ { 
    case weight  => Some(Dirac(weight))
  }

  def Inherit_PC : Parser[Inherit] = opt( Identifier_PC ~ "<-") ~ Identifier_PC ~ Parameters_PC ^^ {
	  case Some(Some(idElem) ~ _) ~ Some(idDef) ~ params => Inherit(idElem,idDef,params)
	  case None ~ Some(idDef) ~ params => Inherit(idDef,idDef,params)	  
  }

  def Mixins_PC: Parser[List[Inherit]] = repsep(Inherit_PC,",") ^^ {
	  case a => a
  }
 
}

/**
 * Lares specific extensions to the standard LogicalExpression parser
  */
trait LaresReferenceCondition_Parser extends LogicExpr_Parser with LaresIdentifier_Parser with LaresReferenceIdentifier_Parser with LaresIter_Parser{
  import LE_metamodel._
	//override def B_ATOM_PC : Parser[Option[ReferenceConditionExpression]] = 
  override def B_ATOM_PC : Parser[Option[LExpAtom]] =    
		ArithExpr_PC ~ "oo" ~ "{" ~ rep1sep(ReferenceCondition_PC, ",") ~ "}" ^^  {
		  case number ~ _ ~ _ ~ refs ~ _ => Some(OO_RefCnd(number,refs collect {case Some(e) => e} toSet))		    
		  case a =>  throw new Exception("found "+a+" instead");None
		} |
		"true" ^^ {
		  case "true" => Some(BooleanAtom(true))
		} |
		("AND" | "OR") ~ "(" ~ rep1sep(Iterator_PC,"," )~ ")" ~ ReferenceCondition_PC ^^ {
			case "AND" ~ _ ~ iters ~ _ ~ Some(refCond) => 
			  Some(AND_RefCnd_Expand(iters.map(_.get), ReferenceConditionLabelBody(List(refCond))))
			case "OR" ~ _ ~ iters ~ _ ~ Some(refCond) => 
			  Some(AND_RefCnd_Expand(iters.map(_.get), ReferenceConditionLabelBody(List(refCond))))
			case a => throw new Exception("found "+a+" instead");None
		} | 
	    ("AND"|"OR") ~ "{" ~ rep1sep(ReferenceCondition_PC,",") <~ "}" ^^ {
	    	case "AND" ~ _ ~ lrgl => Some(AND_RefCnd(lrgl.collect{case Some(lexp) => lexp}))
	    	case "OR" ~ _ ~ lrgl  => Some(OR_RefCnd(lrgl.collect{case Some(lexp) => lexp}))
	    	case a => throw new Exception("found "+a+" instead");None
        } |
		ReferenceCondition_PC ^^ { 
			case Some(cr) => Some(cr)
			case a => throw new Exception("found "+a+" instead");None
		} 
}

/**
 * Lares Module Parser
 */
trait LARES_ModuleStatement_Parser extends JavaTokenParsers {
  def ModuleStatement_PC : Parser[Option[ModuleStatement]]
  def MatchModuleStatements(statements : List[Option[LARES_Element]]) : List[ModuleStatement] 
}


/**
 * Lares Parser 
 */
class LARES_Parser extends 
	ParseComments with 
	LARES_ModuleStatement_Parser with 
	LaresBehavior_Parser with 
	ArithExpr_Parser with 
	LaresIdentifier_Parser with 
	LaresReferenceIdentifier_Parser with 
	LaresReferenceCondition_Parser  with 
	LaresConditionalReactive_Parser
{
  /* Locical State Expression Parser
   *	<b-expression>::= <b-term> [<orop> <b-term>]*
   *	<b-term>      ::= <not-factor> [AND <not-factor>]*
   *	<not-factor>  ::= [NOT] <b-factor>
   *	<b-factor>    ::= <b-literal> | <b-variable> | (<b-expression>)
  */

  def parse(system : String) = parseAll(Specification_PC,system).getOrElse(None)

  
  def ExpandModule_PC : Parser[Option[Expand[ModuleBody]]] = {
	  "expand" ~ "(" ~ rep1sep(Iterator_PC,",") ~ ")" ~ ModuleBody_PC ^^ {
	 	  case _ ~ _ ~ iterators ~ _ ~ body => {
	 	 	  val iters = iterators.filter(_.isDefined).map(_.get)
	 	 	  Some(ExpandModuleBody(iters,body))
	 	  }
	 	  case _ => None
	  }
  }

  def Specification_PC : Parser[Option[SpecLares]] = {
    rep(Behavior_PC | Module_PC) ~ SystemInstance_PC ~ rep(Behavior_PC | Module_PC) ^^ {
      case  rep1 ~ Some(inst) ~ rep2 => {
        // filter all behaviours from the statements found
        val behaviors = for (behaviour <- (for (elem <- rep1 ::: rep2) yield elem match {
          case Some(b : LBehavior)=> Some(b)
          case _ => None
        }).filter(_.isDefined)) yield behaviour.get
        
        // filter all modules from the statements found
        val modules = for (module <- (for (elem <- inst :: rep1 ::: rep2) yield elem match {
          case Some(m : Module) => /*println("module definition "+m);*/Some(m)
          case s : System => /*println("module definition "+s);*/Some(s)
          case sthelse => /*println("not a module "+sthelse);*/None
        }).filter(_.isDefined)) yield module.get
        
        // system instance
        val system = Instance(inst.instanceId,inst.identifier,List(),None)
        
        // generate LARES specification
        Some(SpecLares(behaviors,modules,system))         
      }
      case _ => assert(false,"could not parse specification");None
    }
  }
  
  def ModuleStatement_PC : Parser[Option[ModuleStatement]] = 
    Cause_PC| 
    Forward_PC | 
    LGuard_PC | 
    Initial_PC | 
    Condition_PC | 
    ProbabilityMeasure_PC | 
    Instance_PC
    
  
  def MatchModuleStatements(statements : List[Option[LARES_Element]]) : List[ModuleStatement] = {
    
    val x = (statements collect {
      case Some(m : MeasureSteadyStateProbability) => m
      case Some(m : MeasureTransientStateProbability) => m
    })
    
    x
  }
  
  def ModuleBody_PC :Parser[ModuleBody] = {
	"{" ~ 
	  rep(
          Module_PC | 
          Behavior_PC | 
	      ExpandModule_PC | 
	      ModuleStatement_PC 
	  ) ~ 
    "}" ^^ {
	 	  case _~statements~_ => {
	 		  val modules = statements collect {case Some(s : Module) => s}
	 	      val behaviors = statements collect {case Some(s : LBehavior) => s}
		      val guards = statements collect {case Some(s : LGuard) => s}
		      val instances = statements collect {case Some(s : AbstrInstance) => s}
		      val causes = statements collect {case Some(s : Cause) => s}
		      val conditions = statements collect {case Some(s : Condition) => s}
		      val forwarders = statements collect {case Some(s : Forward) => s}
		      
		      val Expand = statements collect {case Some(s : ExpandModuleBody) => s}
		      val initials = statements collect {case Some(s : Initial) => s}
      		
		      val auxStatements = MatchModuleStatements(statements)
		      ModuleBody(modules,guards,causes,behaviors,instances,conditions,forwarders,Expand,initials, auxStatements)
	 	  }
	 	  case _=> ModuleBody(List(),List(),List(),List(),List(),List(),List(),List(),List(),List())
	  }
  }
  
  def Initial_PC : Parser[Option[Initial]] = {
    "Initial" ~ Identifier_PC ~ ("initially"|"=") ~ rep1sep(ReferenceInitial_PC,",") ^^ {
      case _ ~ Some(id) ~ _ ~ inits => {/*println(id);*/Some(Initial(id,inits))}
      case sthelse => {/*println(sthelse);*/None}
    }
  }
  
  def ReferenceInitial_PC : Parser[ReferenceInitial] = 
    Identifier_PC ~ "." ~ Identifier_PC ^^ {
    case Some(ref) ~ _ ~ Some(refInit) => ReferenceInitial(ref,refInit)
  }
    
  def SystemInstance_PC : Parser[Option[System]] = 
    "System" ~ ident ~ Parameters_PC ~ opt(":" ~ Mixins_PC) ~  ModuleBody_PC ^^ {
    case _ ~ label ~ paramlist ~ Some(_ ~ mix) ~ body => {
    	mix.map(inh => inh)
    	Some(System(Identifier(label,List()),paramlist,mix, body))
    }
      
    case _ ~ label ~ paramlist  ~ None ~ body=> {    
      Some(System(Identifier(label,List()),paramlist,List(), body))
    }
    case _ => 
      None
  }
  
  //def Parameter_PC : Parser[Option[Module]]
  
  def Module_PC : Parser[Option[Module]] =
    "Module" ~ Identifier_PC ~ Parameters_PC ~ opt(":" ~ Mixins_PC)  ~ ModuleBody_PC ^^ {
	  case _ ~ Some(id) ~ paramlist ~ Some(_ ~ mix) ~ body => {
          Some(Module(id,paramlist,mix,body))
	  }
	  case _ ~ Some(id) ~ paramlist ~ None ~ body => {
          Some(Module(id,paramlist,List(),body))
	  }
	  case _ => None
  }
  
  
  def Cause_PC : Parser[Option[Cause]] = B_Expr_PC ~ "causes" ~ repsep(Identifier_PC ~ "=" ~ ArithExpr_PC,",") ^^ {
    //case Some(ls) ~ _ ~ arithexpr => Some(Cause(ls,arithexpr.map(a=>(a._1._1.get,a._2))))
    case _ => assert(false,"could not parse guard");None        	
  }
	  
	  
  def Condition_PC : Parser[Option[Condition]] = {
	  "Condition" ~ Identifier_PC ~ "=" ~  B_Expr_PC ^^ {
	 	  case _~ Some(condident) ~ _ ~ Some(conditionexpression) => Some(Condition(condident,conditionexpression))
	 	  case _ => assert(false,"condition expression problem");None
	  }
  }
  
  def Forward_PC : Parser[Option[Forward]] = {
	  "forward" ~ IdentifierGuardLabel_PC ~ "to" ~ ConditionalReactive_PC ^^ {
	    case _ ~ Some(idGL) ~ _ ~ cndDest => Some(Forward(idGL,List(cndDest)))
	 	case _ => None
	  } |  
	  "forward" ~ IdentifierGuardLabel_PC ~ "to" ~ "{" ~ rep1(ConditionalReactive_PC) ~"}" ^^ {
	    case _ ~ Some(idGL) ~ _ ~ _ ~ cndDests ~ _ => Some(Forward(idGL,cndDests))
	    case _ => None
	  } 
  }
  
    
  
  def LGuard_PC : Parser[Option[LGuard]] = B_Expr_PC ~ "guards" ~ ConditionalReactive_PC ^^ {
    case Some(ls) ~ guard ~ refguards => Some(LGuard(ls,List(refguards)))
    case _ => None
  } |
  B_Expr_PC ~ "guards" ~ "{" ~ rep1(ConditionalReactive_PC) ~"}" ^^ {
    case Some(ls) ~ guard ~ _ ~ refguards ~ _ => Some(LGuard(ls,refguards))
	case _ => None
  } 
  
  
  def Instance_PC : Parser[Option[AbstrInstance]] = "Instance" ~ Identifier_PC ~ opt("initially" ~> Identifier_PC) ~ "of" ~ Identifier_PC ~ Parameters_PC ^^ {
    case _ ~Some(instancename) ~ init ~ _ ~ Some(typeid) ~params => {
      val initstate = if (init.isDefined) init.get else None
      
      
      Some(Instance(instancename,typeid,params,initstate))
    }
    case sthelse => assert(false,"could not parse instance "+sthelse);None
  }
  

  
  /** Parse Something like: 
   * Probability x = SteadyState(BSys.sfailed)
   * or
   * Probability x = Transient(BSys.sfailed,8)
   */
 
  def ProbabilityMeasure_PC : Parser[Option[Measure]] = (TransientProbabilityMeasure_PC | SteadyStateProbabilityMeasure_PC) 
  
  def TransientProbabilityMeasure_PC : Parser[Option[Measure]] = "Probability" ~ Identifier_PC ~ "=" ~ "Transient" ~ "(" ~ B_Expr_PC ~ ","  ~ ArithExpr_PC ~ ")" ^^ {
    case _ ~ Some(name) ~ _ ~ _ ~ _ ~ Some(lexp) ~ _ ~ time ~ _ => Some(MeasureTransientStateProbability(List(name),lexp,time))
  }
  
  def SteadyStateProbabilityMeasure_PC : Parser[Option[Measure]] = "Probability" ~ Identifier_PC ~ "="  ~ "SteadyState" ~ "(" ~ B_Expr_PC ~ ")" ^^ {
    case _ ~ Some(name) ~ _ ~ _ ~ _ ~ Some(lexp) ~ _ => Some(MeasureSteadyStateProbability(List(name), lexp))
  }
}