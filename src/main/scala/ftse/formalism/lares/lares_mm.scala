package ftse.formalism.lares

import scala.collection.immutable._
import ftse.formalism._

// import statements for arithmetic expression 
import ftse.formalism.arith._
import ArithExpr_metamodel._

// import statements for set expression 
import ftse.formalism.set._
import SetExpr_metamodel._

// import statements for logical expression
import ftse.formalism.logical._
import LE_metamodel._

/**
 * Here comes the LARES MetaModel in Form of Case-Classes 
 */
object LARES_metamodel {
  
	/**
	 * common abstract LARES language element
	 */
	abstract trait LARES_Element
	
	/**
	 * Iterator:
	 * denotes an iterator on a set 
	 * @name: denotes the name of the iterator
	 * @set: denotes the set
	 */
	case class Iterator(name : String, set : SetExpr) extends LARES_Element
	
	/**
	 * Expands: 
	 * denotes an abstraction on both the ForStmtModule and ForStmtBehavior
	 * @iters: denote a vector of iterators
	 */
	abstract class Expand[T<:Body](val iters : Iterable[Iterator], val body : T) extends LARES_Element 
	
	case class ReferenceGuardLabelBody(ref : List[LogicalExpr]) extends Body
	case class ReferenceConditionLabelBody(ref : List[LogicalExpr]) extends Body
	
	
	/**
	 * Identifier: 
	 * @name: denotes an arbitrary text string as part of the identifier
	 * @indices: denotes a vector of numbers evaluated out of arithmetic expressions if used inside some expand statements  
	 */
	case class Identifier(
			val name : String,
			val indices : List[ArithExpr]
	) extends LARES_Element
	
	/**
	 * Initial Identifier:
	 * @ref: refers to either a module or a behavior instance
	 * @idInitial: refers to an initial identifier a the given instance reference
	 */
	case class ReferenceInitial(
			val ref : Identifier, 
			val idInitial : Identifier
	) extends LARES_Element
	
	/**
	 * Reference Definition:
	 * @name: refers to either a module or a behavior definition
	 * @parameters: denoting all the parameters and  
	 * @indices:  the corresponding indices evaluated out of a given range if used inside expand statements
	 */
	case class ReferenceDefinition(
			name : String,
			parameters : List[Parameter], 
			indices : List[Range]
	)
	

	/**
	 * Abstract Reference
	 */
	abstract class AbstractReference (
			val instanceRef : Option[Identifier], 
			val namedStatementRef : Identifier
	)
	
	abstract class ResolvedAbstractReference (
		val instanceReferences  : List[Identifier], 
		val behaviorReference : Identifier,
		val namedStatementReference : Identifier
	) extends AbstractReference(Some(behaviorReference),namedStatementReference)
	
	
	/**
	 * AbstractReferenceConditionAtom: 
	 * extends ReferenceConditionExpression by Atom Elements for referencing Condition statements 
	*/
	trait ConditionExpression extends LogicalExpr
	trait ReferenceConditionAtom extends ConditionExpression with LExpAtom
	
	case class ReferenceCondition(
			override val instanceRef : Option[Identifier], 
			override val namedStatementRef : Identifier
	) extends AbstractReference(instanceRef,namedStatementRef) with ReferenceConditionAtom
	
	case class ResolvedReferenceCondition(
			override val instanceReferences  : List[Identifier], 
			override val behaviorReference : Identifier, 
			override val namedStatementRef : Identifier
	) extends ResolvedAbstractReference(instanceReferences, behaviorReference, namedStatementRef) with ReferenceConditionAtom
	
	case class AND_RefCnd(lCL : List[LogicalExpr]) extends ReferenceConditionAtom
	case class OR_RefCnd(lCL : List[LogicalExpr]) extends ReferenceConditionAtom
	
	case class AND_RefCnd_Expand(override val iters: List[Iterator], override val body : ReferenceConditionLabelBody) extends 
		Expand[ReferenceConditionLabelBody](iters, body) with ReferenceConditionAtom
	case class OR_RefCnd_Expand(override val iters: List[Iterator], override val body : ReferenceConditionLabelBody) extends 
		Expand[ReferenceConditionLabelBody](iters, body) with ReferenceConditionAtom
	
	case class OO_RefCnd(n: ArithExpr, refConds : Set[ReferenceCondition]) extends ReferenceConditionAtom
	
	
	/**
	 * ReferenceLabelExpression:
	 * extends Logical Expressions by Atom Elements for Referencing Condition Expressions 
	 */
	trait ReactiveExpression extends LogicalExpr with LARES_Element
	trait ReferenceGuardLabelAtom extends ReactiveExpression with LExpAtom
		
	case class ReferenceGuardLabel(
			override val instanceRef : Option[Identifier], 
			override val namedStatementRef : Identifier
	) extends AbstractReference(instanceRef,namedStatementRef) with ReferenceGuardLabelAtom
	
	case class ResolvedReferenceGuardLabel(
			override val instanceReferences : List[Identifier], 
			override val behaviorReference : Identifier, 
			val distrType : Option[DistributionType],
			override val namedStatementRef : Identifier
	) extends ResolvedAbstractReference(instanceReferences,behaviorReference, namedStatementRef) with ReferenceGuardLabelAtom {
	  def getFullRef =  (instanceReferences,behaviorReference)
	}
	
	case class SYNC(lGL : List[LogicalExpr]) extends ReferenceGuardLabelAtom
	case class MAXSYNC(lGL : List[LogicalExpr]) extends ReferenceGuardLabelAtom
	case class CHOOSE(lGL : List[LogicalExpr]) extends ReferenceGuardLabelAtom
	
	case class SYNC_Expand(override val iters: List[Iterator], override val body : ReferenceGuardLabelBody) extends 
		Expand[ReferenceGuardLabelBody](iters,body) with ReferenceGuardLabelAtom
	
	case class MAXSYNC_Expand(override val iters: List[Iterator], override val body : ReferenceGuardLabelBody) extends 
		Expand[ReferenceGuardLabelBody](iters,body) with ReferenceGuardLabelAtom
	
	case class CHOOSE_Expand(override val iters: List[Iterator], override val body : ReferenceGuardLabelBody) extends 
		Expand[ReferenceGuardLabelBody](iters,body) with ReferenceGuardLabelAtom
	
	case class ReferenceGuardLabel_Expand(override val iters : List[Iterator], override val body : ReferenceGuardLabelBody) extends 
		Expand[ReferenceGuardLabelBody](iters,body) with ReferenceGuardLabelAtom
	
	
		
	/**
	 * Parameter: 
	 * @name: parameter identifier
	 * @defined: denotes if a arithmetic expression is given to define that parameters value 
	 */
	case class Parameter(name : String, defined : Option[ArithExpr]) extends LARES_Element
	
	/**
	 * Range:
	 * @from: denotes the lower bound value obtained from a given arithmetic expression 
	 * @to: denotes the upper bound value obtained from a given arithmetic expression
	 */
	case class Range(from : ArithExpr,to : ArithExpr) extends LARES_Element
	
	/**
	 * Statement: 
	 */
	abstract trait Statement extends LARES_Element
	abstract trait ModuleStatement extends Statement
	abstract trait BehaviorStatement extends Statement
	
	/**
	 * SpecLares: 
	 * denotes the root node of a specification and contains 
	 * module- and behavior definitions and the system instance
	 */
	case class SpecLares(
	  behaviors : Iterable[LBehavior], 
	  modules : Iterable[ModuleDefinition], 
	  system : AbstrInstance) extends LARES_Element 
	
	/**
	 * Definition: 
	 * an abstraction from both, the behavior definition and the module definition
	 */
	abstract class Definition(
			val identifier : Identifier, 
			val parameters : Iterable[Parameter],
			val body : Body
	) extends LARES_Element
	
	
	/**
	 * ModuleDefinition: 
	 * an abstraction from both, the system definition and the module 
	 */
	abstract class ModuleDefinition(
	  override val identifier : Identifier, 
	  override val parameters : Iterable[Parameter],
	  val mixins : Iterable[AbstrInherit], 
	  override val body :ModuleBody
	) extends Definition(identifier,parameters,body)
	
	/**
	 * Body: 
	 * an abstraction for both, the ModuleBody and the BehaviourBody
	 * @forstmt: a set of first level expand statements inside the body
	 */
	
	abstract class Body extends LARES_Element 
	
	abstract class RecursiveBody[T<:Body](
	    val expandables : Iterable[Expand[T]]
	) extends Body
	
	/**
	 * ModuleBody:
	 * denotes a body element inside a module definition that may contain
	 * @module: a set of module definitions
	 * @behaviors: a set of behavior definitions
	 * @guards: a set of guard statements
	 * @causes: a set of cause statements
	 * @forwarders: a set of forward statements
	 * @instances: a set of instances
	 * @initials: a set of initials
	 * @instances: a set of instances
	 * @forstmt: a set of expand statements: i.e. a set of inner-level module bodies 
	 */
	case class ModuleBody(
	  val modules : Iterable[ModuleDefinition], 
	  val guards : Iterable[Guard],  
	  val causes: Iterable[Cause], 
	  val behaviors : Iterable[LBehavior],
	  val instances : Iterable[AbstrInstance],
	  val conditions : Iterable[Condition],
	  val forwarders : Iterable[Forward],
	  override val expandables : Iterable[ExpandModuleBody],
	  val initials : Iterable[Initial],
	  val auxStatements : Iterable[ModuleStatement]
	) extends RecursiveBody[ModuleBody](expandables) 
		
	case class ExpandModuleBody(override val iters: List[Iterator], override val body : ModuleBody) extends 
		Expand[ModuleBody](iters, body)
	
	case class ExpandBehaviorBody(override val iters: List[Iterator], override val body : BehaviorBody) extends 
		Expand[BehaviorBody](iters, body)
	
	/**
	 * Module: 
	 * denotes a module and includes
	 * @identifier: the modules name
	 * @parameters: its parameters
	 * @mixins: the definitions from which the module inherits 
	 * @body: the main body of the module
	 */
	sealed case class Module(
	  override val identifier : Identifier, 
	  override val parameters : Iterable[Parameter],
	  override val mixins : Iterable[AbstrInherit], 
	  override val body : ModuleBody
	) extends ModuleDefinition(identifier,parameters,mixins,body) with Statement
	
	/**
	 * System: 
	 * denotes a system and includes
	 * @identifier: the modules name
	 * @mixins: the definitions from which the module inherits 
	 * @body: the main body of the module
	 */
	sealed case class System(
	  val instanceId : Identifier,
	  override val parameters : Iterable[Parameter],
	  
	  //TODO: why no parameters? 
	  override val mixins : Iterable[AbstrInherit], 
	  override val body : ModuleBody
	) extends ModuleDefinition(instanceId,List(),mixins,body) with Statement
	
	/**
	 * AbstrInherit:
	 * denotes an inheritage that could be either resolved or not:
	 * @assignedId : denotes the identifier used if explicitely given or equals typeId if not given
	 * @typeId : refers to a definition
	 * @params: denotes the assigned parameters of the refered definition
	 */
	abstract class AbstrInherit(
	    val assignedId : Identifier, 
	    val typeId : Identifier, 
	    val params : List[Parameter]
	) extends LARES_Element
	
	sealed case class ResInherit(
	    override val assignedId : Identifier, 
	    val resType : LBehavior
	) extends AbstrInherit(assignedId,resType.identifier,resType.parameters.toList)
	
	sealed case class Inherit(
	    override val assignedId : Identifier, 
	    override val typeId : Identifier, 
	    override val params : List[Parameter]
	) extends AbstrInherit(assignedId,typeId,params)
	
	/**
	 * LBehavior:
	 * denote a behavior definition including
	 * @identifier: its name
	 * @parameters: a set of parameters and 
	 * @body : its body
	 * @initial : an optionally defined initial state
	 */
	sealed case class LBehavior(
	  override val identifier : Identifier, 
	  override val parameters : Iterable[Parameter],
	  override val body : BehaviorBody,
	  initial : Option[Identifier]
	) extends Definition(identifier,parameters,body)
	
	/**
	 * BehaviorBody:
	 * denotes a body element inside a behavior definition that contains
	 * @S: a set of states
	 * @T: a set of transitions
	 * @C: a set of constants
	 */
	sealed case class BehaviorBody(
	  S : Iterable[State],
	  T : Iterable[Transition],
	  C : Iterable[ConstantDef],
	  override val expandables : Iterable[ExpandBehaviorBody]
	) extends RecursiveBody[BehaviorBody](expandables) 
	
	/**
	 * State:
	 * denotes a states
	 * @identifier: name of the state
	 */
	sealed case class State(
	  identifier : Identifier
	) extends LARES_Element 
	
	/**
	 *  Transition: 
	 *  a transition from a source state to a target state
	 *  @from: source state
	 *  @to: target state
	 */
	abstract class Transition( 
	  val from : State, 
	  val to : State
	) extends LARES_Element
	
	/** 
	 * UnguardedTransition: 
	 * a transition without a guard 
	 */
	sealed case class UnguardedTransition(override val from : State, override val to : State, distr : Option[Distribution]) extends Transition(from,to)
	
	/** 
	 * GuardedTransition: 
	 * a transition with a guard 
	 */
	sealed case class GuardedTransition(override val from : State, override val to : State, guard : Identifier, distr : Option[Distribution]) extends Transition(from,to)
	
	/**
	 * Distribution:
	 * an abstraction over all possible distributions that can be defined within LARES
	 */
	abstract class DistributionType 
	case object ExponentialType extends DistributionType
	case object DiracType extends DistributionType
	
	abstract class Distribution(val distrType : DistributionType) extends LARES_Element  with ArithExpr_algo
	
	/**
	 * Exponential: 
	 * denotes an exponential distribution with
	 * @rate: a given rate 
	 */
	sealed case class Exponential(rate : ArithExpr) extends Distribution(ExponentialType)
	object Exponential1 extends Exponential(ArithAtomValue(Right(1.0))) 
	
	/**
	 * Dirac:
	 * denotes the immediate distribution, i.e. the delay is zero
	 * @weight: implicitely the weight is 0 but can be explicitely set to arbitrary real values
	 */
	sealed case class Dirac(weight : ArithExpr) extends Distribution(DiracType)
	object Dirac1 extends Dirac(ArithAtomValue(Right(1.0)))
	/** 
	 * Constant:
	 * defines a constant
	 * @id: the identifier 
	 * @ae: the arithmetic expression to be evaluated  
	 */
	sealed case class ConstantDef(id : Identifier, ae : ArithExpr) extends LARES_Element
	
	/**
	 * AbstrInstance:
	 * defines an abstraction of an instance, which can be either resolved or not
	 * @identifier: name of the instance
	 * @typeid: the identifier of the module definition 
	 * @params: the parameters of the given module definition
	 * @init: the initial to be set 
	 */
	abstract class AbstrInstance (
	  val identifier : Identifier, 
	  val itype : Either[Identifier,ModuleDefinition],
	  val params : List[Parameter],
	  val init : Option[Identifier]
	) extends  ModuleStatement
	
	sealed case class Instance (
	  override val identifier : Identifier, 
	  val typeid : Identifier,
	  override val params : List[Parameter],
	  override val init : Option[Identifier]
	) extends AbstrInstance(identifier,Left(typeid),params,init)
	
	sealed case class ResolvedInstance (
	  override val identifier : Identifier, 
	  val restype : ModuleDefinition,
	  override val init : Option[Identifier]
	) extends  AbstrInstance(identifier, Right(restype),restype.parameters.toList, init)
	
	/**
	 * Condition:
	 * assigns a name to a given boolean expression, i.e. representing a "makro-state" of the associated subtree 
	 * @identifier: name of the condition
	 * @le: a boolean expression refering to either further conditions or states within behavior instances 
	 */
	sealed case class Condition(
	  identifier : Identifier, 
	  le : LogicalExpr
	) extends LARES_Element with ModuleStatement
	
	/**
	 * 
	 */
	abstract class InteractionalElement extends ModuleStatement 
	

	/**
	 * Guards statement 
	 */
	abstract class AbstractTrigger (
	  val cndDests  : List[AbstrConditionalReactive]
	) extends InteractionalElement
	
	/**
	 * Forwards statement 
	 */
	abstract class AbstractNamedTrigger(
	    val name : Identifier,
	    override val cndDests  : List[AbstrConditionalReactive]
	) extends AbstractTrigger(cndDests)
	
	abstract class AbstrConditionalReactive(val cnd : Option[LogicalExpr], val reactive : LogicalExpr) extends LARES_Element
	abstract class AbstrTypedConditionalReactive(override val cnd : Option[LogicalExpr], override val reactive : LogicalExpr, val distr : DistributionType) extends AbstrConditionalReactive(cnd,reactive)
	
	sealed case class ConditionalReactive(override val cnd : Option[LogicalExpr], override val reactive : LogicalExpr) extends AbstrConditionalReactive(cnd,reactive)
	sealed case class TypedConditionalReactive(override val cnd : Option[LogicalExpr], override val reactive : LogicalExpr, override val distr : DistributionType) extends AbstrTypedConditionalReactive(cnd,reactive,distr)
	
	/** 
	 * Forward: 
	 * propagates the information of a triggered guard label reference 
	 * into the leaves of the subtree, i.e. handles the over to the labels stated
	 * inside the forward statement
	 * @identifier: name of the forward statement, i.e. defines a new guard label
	 * @condition: if a boolean expression is defined, the forward is called conditional
	 * @destinations: references to further guard labels
	 */
	sealed case class Forward(
	  override val name : Identifier,
	  override val cndDests : List[AbstrConditionalReactive]
	) extends AbstractNamedTrigger(name, cndDests) 
	
	/**
	 * LGuard: 
	 * the root level of aggregating state information, the guard statement
	 * generates the event that triggers the guard label references 
	 * @le: boolean expression over conditions/states
	 * @destinations: a set of triggered guard label references 
	 */
	
	abstract class Guard(
	  val le : LogicalExpr, 
	  override val cndDests : List[AbstrConditionalReactive]
	) extends AbstractTrigger(cndDests)
	
	sealed case class LGuard(
	  override val le : LogicalExpr, 
	  override val cndDests : List[ConditionalReactive]
	) extends Guard(le,cndDests) 
	
	sealed case class TypedGuard(
	  override val le : LogicalExpr, 
	  override val cndDests : List[AbstrTypedConditionalReactive]
	) extends Guard(le,cndDests)
	
	/**
	 * Initial:
	 * defines a initial by refering to further initials, 
	 * i.e. beeing either an initial for oneself or a state of a behavior instance
	 * @identifier: name of the given Initial
	 * @destinations: list of further initials 
	 */
	sealed case class Initial(
	  identifier : Identifier,
	  destinations : List[ReferenceInitial]
	) extends ModuleStatement
	
	/**
	 * Cause:
	 * @deprecated
	 */
	sealed case class Cause(
	  le : LogicalExpr, 
	  ariths : List[(Seq[String],ArithExpr)]
	) extends ModuleStatement
	
	/**
	 * Measures represent the Question to be posed to the model  
	 */
	abstract class Measure(val name : Identifier) extends ModuleStatement 
	abstract class MacroStateMeasure(val ns : List[Identifier], val macroState : LogicalExpr) extends Measure(ns.last)
	case class MeasureSteadyStateProbability(override val ns : List[Identifier], override val macroState : LogicalExpr) extends MacroStateMeasure(ns, macroState)
	case class MeasureTransientStateProbability(override val ns : List[Identifier], override val macroState : LogicalExpr, time : ArithExpr) extends MacroStateMeasure(ns, macroState)
	
}

