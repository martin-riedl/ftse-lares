package ftse.transformations.lares

//TODO: add comments

// import statements for arithmetic expression 
import ftse.formalism.arith._
import ArithExpr_metamodel._
import ftse.formalism.set._
import SetExpr_metamodel._
import ftse.formalism.logical._
import LE_metamodel._
import ftse.formalism.lares.Lares2Text
import ftse.formalism.lares.LARES_metamodel._
import ftse.transformations._
import scala.collection.immutable._

/**
 * Parameter Expansion Structures (PES)
 */
object PES {
	type BW = Any
	type FW = (Map[Identifier,Identifier], HashMap[Identifier,LBehavior], HashMap[Identifier,ModuleDefinition]) // forwarding information of initial configuration
}

/**
 * wrapper class for Lares models that can be used for an implicit assignment of the transformation method   
 */
class ClassLaresParameterExpansion(l : LARES_Element) extends LaresParameterExpansion {
	def transform : LARES_Element = transform(l)
}

/**
 * Parameter Expansion 
 * 
 * root class that parameter expansion
 * therefore the methods transform, forward and backward are defined   
 */
class LaresParameterExpansion extends 
	LaresTraverser[PES.BW,PES.FW] with 
	TLaresExpandResolver with 
	AbstrTransformer[LARES_Element, LARES_Element]
{
  import PES._

  /**
   * root transformation method
   */
  def transform(e : LARES_Element) : LARES_Element = e match {
	  case s : SpecLares => {
	    val B_local = HashMap((for (b<- s.behaviors) yield b.identifier -> b).toSeq :_*)
	    val M_local = HashMap((for (m<- s.modules) yield m.identifier -> m).toSeq :_*)
		  
	    val fw = (HashMap[Identifier,Identifier](), B_local, M_local)
	    SpecLares(List(),List(),traverse(s.system,fw)._1)
	  }
  }
  
    
  /**
   * backward information method
   */
	override def backwards (
	  i: AbstrInstance,
	  fw : FW,
	  res : Iterable[(AbstrInstance,BW)]
	) : (AbstrInstance, BW) = {
      //val module = i.restype
	  val module = i.itype.right.get
	  
      val newSubInstances = res.map(_._1)
      
      
	  val resbody = ModuleBody(
	      List(),					 // module definitions
	      module.body.guards,
	      module.body.causes, 
	      List(),
	      newSubInstances,
	      module.body.conditions,
	      module.body.forwarders,
	      List(),
	      List(),
	      module.body.auxStatements
	  )
	  
	  val instance = ResolvedInstance(
	      i.identifier,
	      Module(module.identifier,module.parameters,module.mixins,resbody),
	      i.init)
	  
	  (instance,null)
	}
	
	
	/**
	 * forward information method
	 */
	override def forwards (
	  i : AbstrInstance,
	  fw : FW
    )  = {
	  object top extends ftse.algorithms.TopologicalOrderedProcessing
	  val initConf = fw._1; val B = fw._2; val M = fw._3
	  //val m = M(i.typeid)
	  val m = M(i.itype.left.get)
	  
	  // generate valid parameter for this instance 
	  val parameters = substInstantiationParameters(i.params, m.parameters.toList)
      
	  // perform evaluations
      val evaluations = HashMap(parameters.map (p => (p.name -> p.defined.get.eval())) toList :_*)
         
      // collect the integer typed evaluations
	  val iMap = evaluations collect {case (name, Left(value)) => (name,value)}
	  
	  // resolve body and append delegate instantiations 
	  val b = resolveBody(m.body,iMap,evaluations)
	  
	  // determine additional definitions "in scope"
	  val B_i = HashMap((B ++ (for (b<- b.behaviors) yield b.identifier -> b)).toSeq :_*)
	  val M_i = HashMap((M ++ (for (m<- b.modules) yield m.identifier -> m)).toSeq :_*)
	  
      
	  // partition the delegate instantiations into behavior rsp. module delegates
	  val (delegateB, delegateM) = m.mixins partition ( a=> B_i.contains(a.typeId))
	  	  
	  // append the module instance delegates
	  val body = ModuleBody(
	      b.modules,b.guards,b.causes,b.behaviors,
	      b.instances ++ delegateM.map(d => Instance(d.assignedId,d.typeId,d.params,None)), 
	      b.conditions,b.forwarders, b.expandables, b.initials, b.auxStatements)
      
	  // determine referenced initials   
	  val adressedInitials = fw._1.collect {case (iref,id) if iref == i.identifier => id}
	  assert(adressedInitials.size<=2, "multiple initials are adressed " + adressedInitials)
	  
	  // either the referenced or the default initial or else not initial selected  
	  val inits = HashMap((adressedInitials.headOption flatMap(e =>body.initials.find(_.identifier==e)) match {
	    case Some(initial) => initial.destinations
	    case None => body.initials.headOption map(_.destinations) getOrElse(List()) 
	  }) map(a => a.ref -> a.idInitial) :_*)
	  
	  
	  // construct the resolved behavior instances
	  val delegateBeh = delegateB map (resolveInherit(_, evaluations, B_i, inits)) 
	  
	  
	  // reconstruct the (resolved) module instance 
      val instance = ResolvedInstance(
          i.identifier, 
          m match {
          	case d : Module => Module(d.identifier,parameters,delegateBeh,body)
          	case d : System => System(d.identifier,parameters,delegateBeh,body)
          }, 
          None // i.init
      )
	  
      //  determine the remaining initials to be forwarded 
      val BN = HashSet(delegateBeh.map(m=>m.assignedId).toList :_*) // set of behavior instance names
      val remainingInits = inits.filterNot(b=>BN.contains(b._1))
      val instanceInits = HashMap(body.instances.filter(_.init.isDefined).map(i=> i.identifier -> i.init.get).toList :_*)
      
      // return 
      (instance, (instanceInits ++ remainingInits, B_i, M_i))
	}
	
	/**
	 * resolve and instantiate a delegate 
	 */
	def resolveInherit(
	    i : AbstrInherit, 
	    eval : HashMap[String,Either[Int,Double]], 
	    B : HashMap[Identifier,LBehavior], 
	    inits : HashMap[Identifier,Identifier]
	) : ResInherit = {
	  
	  val behavior = B(i.typeId)
	  // generate valid parameter for this instance 
	  val parameters = substInstantiationParameters(resolveParameters(i.params, eval), behavior.parameters.toList)
	  
	  // perform evaluations
	  val evaluations = HashMap(parameters.map (p => (p.name -> p.defined.get.eval())) toList :_*)
	     
	  // collect the integer typed evaluations
	  val iMap = evaluations collect {case (name, Left(value)) => (name,value)}

	  // construct resolved behavior instance
	  ResInherit(
	      i.assignedId,
	      LBehavior(
	          i.assignedId,
	          parameters,
	          resolveBody(behavior.body, iMap, evaluations),
	          inits.get(i.assignedId)
	      )
	  )
	}
	
	/**
	 * helper function for the instantiation to substitute parameters 
	 */
	private def substInstantiationParameters(priorityParameters : List[Parameter], defaultParameters : List[Parameter]) : List[Parameter]= {
	  object top extends ftse.algorithms.TopologicalOrderedProcessing
	  
	    top.sortAndProcess[String, Parameter, Parameter](
          defaultParameters.toList, 
          _.name, 
          _.defined.map(_.dependsOn {case ArithAtomIdentifier(n) => Set(n) case sthelse => Set()}) getOrElse(Set()),  
          (source, targetlist) => {
            // either a definedParameter is given by the instance statement or the parameter of the default parameters are used
            priorityParameters.find(_.name == source.name).getOrElse {
              // the default parameter might be required to be substituted referenced variables 
              Parameter(
                  source.name, 
			      source.defined.map(_.subst {
			      	case ArithAtomIdentifier(undefined) => targetlist.find(_.name==undefined).flatMap(_.defined).getOrElse(ArithAtomIdentifier(undefined)) // defined locally
			        case sthelse => sthelse
			      })
		      )  
            }
          } 
	   ) map { parameter =>
	   		Parameter(parameter.name,parameter.defined.map(ae => ArithAtomValue(ae.eval())))
	   }
	  }
	
}
 
/**
 * 
 */
trait ResolveLogicalExpr extends SubstituteExpressions {

  /**
   * 
   */
  def genModifiedIterators[T<:Body](f : Expand[T], iMap : HashMap[String,Int]) : List[Iterator] = {
    
	f.iters.map(i => {
	  val mSE = genModifiedSetExpr(i.set,iMap)
	  Iterator(i.name,mSE)
	}).toList
  }
 
  /**
   * 
   */
  def resolveExpression[T<:Body](e : Expand[T], resolve : (T,HashMap[String,Int],HashMap[String,Either[Int,Double]]) => T,  iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) : List[T] = {
    val modifiedIterators = genModifiedIterators(e,iMap)
	
	// here the new map is built based on the one of the upper layer
	for (cp <- ResolveIterator.buildCrossProductMap(modifiedIterators).toList) yield {
	  val newHM = HashMap((cp ++ iMap) :_*)
	  val newEval = HashMap(cp.map(a => a._1 -> Left(a._2)) :_*)
	  resolve(e.body,newHM, evalParam ++ newEval)
	}
  }
  
  /**
   * 
   */
  def resolve(b : ReferenceGuardLabelBody, iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) : ReferenceGuardLabelBody = {
    val resLExp = b.ref.map { lexp => 
      resolveLogicExpr(lexp,iMap, evalParam) 
    }
    ReferenceGuardLabelBody(resLExp)
  }
  
  /**
   * 
   */
  def resolve(b : ReferenceConditionLabelBody, iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) : ReferenceConditionLabelBody = {
    val resLExp = b.ref.map { lexp => 
      resolveLogicExpr(lexp,iMap, evalParam) 
    }
    ReferenceConditionLabelBody(resLExp)
  }
  
  /**
   * 
   */
  def resolveLogicExpr(lexp : LogicalExpr, iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) : LogicalExpr = lexp modify {
	  // Resolving the Generative Atomic Elements
      case OO_RefCnd(n,s) => {
    	  		println(iMap)
    	  		println(evalParam)
    	  		val value = substAndEvaluate(evalParam)(n).eval().left.get
    	  		if (value > 0) {
    	  			ftse.algorithms.SetTheory.powerSet(s).filter(_.size == value). 
			    	map(e => e.reduceLeft((a : LogicalExpr,b : LogicalExpr) => a & b)).
			    	reduceLeft((a : LogicalExpr,b : LogicalExpr) => a | b)  
    	  		} else {
    	  		  assert(value>=0,value + " oo { .. } is not valid!" )
    	  		  BooleanAtom(true)
    	  		}
    	  		
      }
      case AND_RefCnd(refs) => refs.map(resolveLogicExpr(_, iMap, evalParam)).reduceLeft(_ & _)
	  case OR_RefCnd(refs) => refs.map(resolveLogicExpr(_, iMap, evalParam)).reduceLeft(_ | _)
	  case atomicGen : AND_RefCnd_Expand => 
	    resolveExpression[ReferenceConditionLabelBody](atomicGen, resolve, iMap, HashMap()).flatten(a => a.ref).reduceLeft(_ & _)
	  case atomicGen : OR_RefCnd_Expand => 
	    resolveExpression[ReferenceConditionLabelBody](atomicGen, resolve, iMap, HashMap()).flatten(a => a.ref).reduceLeft(_ & _)
	  case atomicGen : ReferenceCondition => substReference(atomicGen,iMap)
	  
	  // Resolving the Reactive Atomic Elements
	  case SYNC(refs) => SYNC(refs.map(resolveLogicExpr(_, iMap, evalParam)))
	  case MAXSYNC(refs) => MAXSYNC(refs.map(resolveLogicExpr(_, iMap, evalParam)))
	  case CHOOSE(refs) => CHOOSE(refs.map(resolveLogicExpr(_, iMap, evalParam)))
	  
	  case atomicRct : SYNC_Expand => 
	    SYNC(resolveExpression[ReferenceGuardLabelBody](atomicRct, resolve, iMap, HashMap()).flatten(a => a.ref))
	  case atomicRct : MAXSYNC_Expand => 
	    MAXSYNC(resolveExpression[ReferenceGuardLabelBody](atomicRct, resolve, iMap, HashMap()).flatten(a => a.ref))
	  case atomicRct : CHOOSE_Expand => 
	    CHOOSE(resolveExpression[ReferenceGuardLabelBody](atomicRct, resolve, iMap, HashMap()).flatten(a => a.ref))
	  case atomicRct : ReferenceGuardLabel =>  substReference(atomicRct,iMap)
	  
	  // Default 
	  case sthelse => sthelse
  }
  
  /**
   * 
   */
  def genModifiedSetExpr(set : SetExpr, iMap : HashMap[String,Int]) : SetExpr = {
    val substitutables = Set(iMap.keys.toList :_*) & set.dependsOn()
	
    substitutables.foldLeft(set)((se,s) => {
	  se subst( a => a match {
	    case aai : ArithAtomIdentifier => if (aai.id==s) ArithAtomValue(Left(iMap.get(s).get)) else aai 
		case sthelse => sthelse
	  })
	})
  }
}

/**
 * root trait to transform the auxiliary statements (stackable trait pattern)  
 */
trait AbstractLaresParameterResolver {
  def resolveAuxStatements(e : List[ModuleStatement], iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) : List[ModuleStatement]
}

/**
 * 
 */
trait TLaresExpandResolver extends 
	AbstractLaresParameterResolver with 
	ResolveLogicalExpr with 
	Lares2Text
{
	
  /**
   * parameter-resolving of measure statements
   */
  def resolveAuxStatements(aux : List[ModuleStatement], iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) : List[ModuleStatement] = {
     aux map (_ match {
       case MeasureTransientStateProbability(ns, l, t) => MeasureTransientStateProbability(
             ns.map(substIdentifier(_,iMap)), resolveLogicExpr(l,iMap, evalParam), substAndEvaluate(evalParam)(t)
         ) 
       case MeasureSteadyStateProbability(ns,l) => MeasureSteadyStateProbability(
           ns.map(substIdentifier(_,iMap)),resolveLogicExpr(l,iMap, evalParam)
         ) 
       case sthelse => sthelse
     })
  }
  
  /**
   * parameter-resolving of a forward statement
   */
  private def resolveForwarder(e : Forward, iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) = {
	  Forward(
	      substIdentifier(e.name,iMap), 
	      e.cndDests.map(cndDest => {
	    	  ConditionalReactive(cndDest.cnd map (resolveLogicExpr(_,iMap, evalParam)), resolveLogicExpr(cndDest.reactive,iMap, evalParam))
	      })
	  )
  }  
  
  /**
   * parameter-resolving of a condition statement
   */
  private def resolveCondition(e: Condition, iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) = {
	  Condition(
	      substIdentifier(e.identifier,iMap),
	      resolveLogicExpr(e.le,iMap, evalParam)
	  )
  }
  
  /**
   * parameter-resolving of a guards statement
   */
  private def resolveGuard(g: Guard, iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) = {
      LGuard(
          resolveLogicExpr(g.le,iMap, evalParam), 
          g.cndDests.map(cndDest => {
            ConditionalReactive(cndDest.cnd map (resolveLogicExpr(_,iMap, evalParam)), resolveLogicExpr(cndDest.reactive,iMap, evalParam))
          })
      )
  }

  /**
   * parameter-resolving of an instance statement
   */  
  private def resolveInstance(e : AbstrInstance, iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) = {
	  Instance(
	      substIdentifier(e.identifier,iMap), 
	      //substIdentifier(e.typeid,iMap), 
	      substIdentifier(e.itype.left.get,iMap),
	      resolveParameters(e.params, iMap.map(p => (p._1, Left(p._2))) ++ evalParam), 
	      e.init.map(substIdentifier(_,iMap))
	  )
  }
  
  @deprecated // TODO: remove cause constructs from metamodel and this function
  def resolveCause(c: Cause, iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) = {
	  c
  }
  
  /**
   * parameter-resolving of parameter lists following a topological order
   */
  def resolveParameters(params : List[Parameter], evaluated : HashMap[String,Either[Int,Double]]) = {
    object top extends ftse.algorithms.TopologicalOrderedProcessing
	 
    val resparams = top.sortAndProcess[String, Parameter, Parameter](
          params, 
          _.name, 
          _.defined.map(_.dependsOn {
            case ArithAtomIdentifier(n) => Set(n) 
            case sthelse => Set()
          }) getOrElse(Set()),  
          (source, targetlist) => {
            Parameter( source.name, 
                //source.defined
                source.defined.map(_ subst {
	              case ArithAtomIdentifier(undefined) => {
	                // take the undefined identifier from a module parameter or from a locally defined parameter
	                targetlist.find(_.name==undefined).flatMap(_.defined).getOrElse(ArithAtomValue(evaluated(undefined))) 
	              }
	              case sthelse => sthelse
	            })
            )
          } 
	   )
	
    resparams
  }

  // TODO: parameter-resolving of a Module definition
  def resolveModule(e : ModuleDefinition, iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) = e match {
	  case m :  Module => Module(
	        m.identifier, 
	        m.parameters,
	        m.mixins,
	        resolveBody(m.body,iMap,HashMap())
	    )
	  case s : System => System(
	      s.identifier, 
	      s.parameters ,
	      s.mixins ,
	      resolveBody(s.body,iMap,HashMap())
	  )
  }
  
  /**
   * TODO: parameter-resolving of a Module body
   * @param body
   * @param iMap
   * @return
   */
  def resolveBody(body : ModuleBody, iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) : ModuleBody = {
     val resLocalBody = ModuleBody(
	 		  for (m <- body.modules) yield resolveModule(m,iMap,evalParam), // module definitions
	 		  for (g <- body.guards) yield resolveGuard(g,iMap,evalParam),
	 		  for (c <- body.causes) yield resolveCause(c,iMap,evalParam),
	 		  for (b <- body.behaviors) yield resolveBehavior(b,iMap,evalParam), // behavior definitions
	 		  for (i <- body.instances) yield resolveInstance(i,iMap,evalParam), // instances
	 		  for (c <- body.conditions) yield resolveCondition(c,iMap,evalParam), // conditions
	 		  for (f <- body.forwarders) yield resolveForwarder(f,iMap,evalParam),
	 		  List(),
	 		  for (f <- body.initials) yield resolveInitials(f,iMap,evalParam),
	 		  resolveAuxStatements(body.auxStatements.toList,iMap,evalParam)
	  )		
	  
	  //val resRekBodies = resolveRekursiveBodies[ModuleBody](body,iMap) 
	  val resRekBodies = body.expandables.flatMap(f => {
	      val modifiedIterators = genModifiedIterators(f,iMap)
	 	  // here the new map is built based on the one of the upper layer
		  ResolveIterator.buildCrossProductMap(modifiedIterators) map (cp => {
			  val newHM = HashMap((cp ++ iMap) :_*)
			  val newEval = HashMap[String,Either[Int,Double]](cp.map(a => a._1 -> Left(a._2)) :_*)
			  val n : HashMap[String,Either[Int,Double]] = evalParam ++ newEval 
			  resolveBody(f.body ,newHM, n)
		  })
	  })
	  
	  // decide wheter there are for loops to be reduced or not
	  resRekBodies.foldLeft(resLocalBody)((a,b) => ModuleBody(
	 		  a.modules ++ b.modules,
	 		  a.guards ++ b.guards,
	 		  a.causes ++ b.causes,
	 		  a.behaviors ++ b.behaviors,
	 		  a.instances ++ b.instances,
	 		  a.conditions ++ b.conditions,
	 		  a.forwarders ++ b.forwarders,
	 		  List(),
	          a.initials ++ b.initials,
	 		  a.auxStatements ++ b.auxStatements
	 	)	  
	  )  
  }
  
  /**
   * parameter-resolving of a Module body
   */
  def resolveBody(body : BehaviorBody, iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) : BehaviorBody = {
	  // resolve local body 
	  val resLocalBody = BehaviorBody(
	 		  for (s <- body.S) yield resolveState(s,iMap, evalParam), 
	 		  for (t <- body.T) yield resolveTransition(t,iMap, evalParam),
	 		  for (c <- body.C) yield resolveConstant(c,iMap, evalParam),
	 		  List()
	 )
	  
	  // obtain recursively resolved bodies 
  	  val resRekBodies = body.expandables.flatMap (f => {
	      // here the new map is built based on the one of the upper layer
		  ResolveIterator.buildCrossProductMap(
		      genModifiedIterators(f,iMap)
		  ).map(cp => 
		    resolveBody(f.body , HashMap((cp ++ iMap) :_*), evalParam)
		  )
	  })
	  
	  // merge resolved bodies 
	  resRekBodies.foldLeft(resLocalBody)((a,b) => BehaviorBody(a.S ++ b.S, a.T ++ b.T, a.C ++ b.C, List()))
  }
  
  /**
   * TODO: parameter-resolving of a behavior definition
   */
  private def resolveBehavior(b : LBehavior, iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) : LBehavior = {
	  LBehavior(
	      b.identifier,
	      b.parameters,
	      resolveBody(b.body,iMap, evalParam),
	      b.initial
	  ) 
  }
  
  /**
   * parameter-resolving of a state statement
   */
  private def resolveState(s : State, iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) = {
	  State(substIdentifier(s.identifier , iMap))
  }
  
  /**
   * parameter-resolving of a transitions statement
   */
  private def resolveTransition(t : Transition, iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) = {
    val from = resolveState(t.from,iMap,evalParam)
    val to = resolveState(t.to,iMap,evalParam)
    
    // allow the use of iMap for the calculation of weights of rates 
    val params : HashMap[String,Either[Int,Double]] = iMap.map(a => (a._1, Left(a._2))) ++ evalParam
    t match {
	  case UnguardedTransition(_, _, distr) => UnguardedTransition(from,to,resolveDistribution(distr, params))
	  case GuardedTransition(_, _, guard, distr) => GuardedTransition(from,to,guard, resolveDistribution(distr,params))
	}
  }
  
  /**
   * parameter-resolving of a distribution element
   */
  private def resolveDistribution(distr: Option[Distribution], evalParam : HashMap[String,Either[Int,Double]]) = {
    distr match {
      case Some(Exponential(rate : ArithExpr)) => Some(Exponential(substAndEvaluate(evalParam)(rate)))
      case Some(Dirac(weight : ArithExpr)) => Some(Dirac(substAndEvaluate(evalParam)(weight)))
      case a => a
    }
  }
    
  /**
   * parameter-resolving of a initial statement
   */
  private def resolveInitials(t : Initial, iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) = {
    Initial(
        substIdentifier(t.identifier , iMap), 
        t.destinations.map(ref => ReferenceInitial(
        		substIdentifier(ref.ref , iMap),
        		substIdentifier(ref.idInitial, iMap)
          )
        )
    )
  }
  
  /**
   * parameter-resolving of a constant definition
   */
  private def resolveConstant(c : ConstantDef, iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) = {
    ConstantDef(substIdentifier(c.id,iMap),substAndEvaluate(evalParam)(c.ae))
  }
  
  /**
   * parameter-resolving of a lares specification
   */
  private def resolveSpecification(spec : SpecLares, iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) : SpecLares = {
	  SpecLares(
	 		  for (b <- spec.behaviors) yield resolveBehavior(b,iMap,evalParam),
			  for (m <- spec.modules) yield resolveModule(m,iMap,evalParam),
			  resolveInstance(spec.system,iMap, evalParam)
	  )
  }
  
  
 
	/**
	 * What should be resolved?
	 * Everything that 
	 * - is a definition with an identifier having indices
	 * - that is not a definition but uses indices inside its term
	 * 
	 * @param e Lares_Element to be resolved
	 * @param iMap contains the mapping to substitute strings to its evaluations
	 * @return the expanded elements in form of an iterable structure
	 */
	def resolveFor(e :LARES_Element)  : LARES_Element = {/*println(e.toText);*/val resolvedFor=resolveFor(e, HashMap(), HashMap());/*println(resolvedFor.toText);*/resolvedFor}
	def resolveFor(e : LARES_Element,iMap : HashMap[String,Int], evalParam : HashMap[String,Either[Int,Double]]) : LARES_Element = e match {
		  case s : SpecLares => resolveSpecification(s,iMap, evalParam)
		  case m : Module => resolveModule(m,iMap, evalParam)
		  case f : Forward => resolveForwarder(f,iMap, evalParam)
		  case i : Instance => resolveInstance(i,iMap, evalParam)
		  case c : Condition => resolveCondition(c,iMap, evalParam)
		  case State(identifier) => State(substIdentifier(identifier,iMap))
		  case b : LBehavior => resolveBehavior(b,iMap, evalParam)
  }
}

/**
 * 
 */
trait SubstituteExpressions extends 	
	ArithExpr_algo with
	SetExpr_algo with 
	LogicExpr_algo 
{
  /**
   * substitute undefined variables within arithmetic expressions by parameter evaluations and 
   * subsequently evaluate the arithmetic expressions 
   * 
   * @param mapping Mapping from the variables to the corresponding values
   * @param ae The arithmetic expression to be substituted
   * @return The substituted arithmetic expression 
   */
  def substAndEvaluate(mapping : HashMap[String,Either[Int,Double]])(ae : ArithExpr) : ArithExpr = {
    ArithAtomValue(substParam(mapping)(ae).eval())
  }
  
  /**
   * Substitutes and evaluate the arithmetic expressions used in an identifier 
   * @param id
   * @param iMap
   * @return a variable resolved identifier 
   */
  def substIdentifier(id : Identifier, iMap : HashMap[String, Int]) : Identifier = {
    Identifier(id.name, id.indices.map( index => ArithAtomValue(index.eval(subst(iMap)))))
  }
   
  /**
   * Substitutes an abstract reference by the visible variable definitions  
   * @param ref
   * @param iMap
   * @return a resolved reference in terms of a logical expression 
   */
  def substReference(ref : AbstractReference, iMap : HashMap[String,Int]) : LogicalExpr = {
	  val nameRef = substIdentifier(ref.namedStatementRef,iMap)
	  val instRef = ref.instanceRef.map(substIdentifier(_,iMap))
	  ref match {
	    case e : ReferenceGuardLabel => ReferenceGuardLabel(instRef,nameRef)
	    case e : ReferenceCondition => ReferenceCondition(instRef,nameRef)
	  } 
  }
  
  /**
   * Currying function to map the variables inside am arithmetic expression elements to the corresponding values  
   * @param mapping given map
   * @param ae  arithmetic expression input
   * @return mapped arithmetic expression
   */
  private def subst(mapping : HashMap[String,Int])(ae : ArithExpr) : ArithExpr = ae subst {
 	  case ArithAtomIdentifier(id) => mapping.get(id) map(v=> ArithAtomValue(Left(v))) getOrElse {
 	    throw new Exception("cannot substitute " + id + " in " + ae ); ArithAtomIdentifier(id)
 	  }
 	  case sthelse => sthelse
  }
  
  /**
   * Currying function to map the variables inside am arithmetic expression elements to the corresponding values
   * @param mapping Mapping from the variables to the corresponding values
   * @param ae The arithmetic expression to be substituted 
   * @return a Substituted arithmetic expression
   */
  private def substParam(mapping : HashMap[String,Either[Int,Double]])(ae : ArithExpr) : ArithExpr = ae subst {
 	  case ArithAtomIdentifier(id) => mapping.get(id) map(v=> ArithAtomValue(v)) getOrElse {
 	    throw new Exception("cannot substitute " + id + " in " + ae ); ArithAtomIdentifier(id)
 	    ArithAtomIdentifier(id)
 	  }
 	  case sthelse => sthelse
  }  
}
