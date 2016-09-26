package ftse.transformations.lares


import ftse.formalism.arith._
import ftse.formalism.set._

// import statements for logical expression
import ftse.formalism.logical._
import LE_metamodel._


import ftse.formalism.lares._
import ftse.algorithms.SetTheory

import ftse.formalism.lares.LARES_metamodel._

import ftse.formalism.lares.LARES_metamodel._

import ftse.transformations._
import scala.collection.immutable._
import ftse.tools._

class ClassLaresForwardExpansion(l : LARES_Element) extends LaresForwardExpansion {
	def resolveForward : LARES_Element = transform(l)
}

/**
 * Defines the Abstract Interface for the Stackable Trait Pattern of the GuardsExpansion Transformation Step
 */
trait AbstrGuardExpansion {
  
  def obtainNamedTriggers(l : List[ModuleStatement]) : List[AbstractNamedTrigger] 
  def obtainGuards(l : List[ModuleStatement]) : List[Guard]
  
  def processNamedTrigger(
      fwd : Iterable[(Identifier,AbstractNamedTrigger)]
  )(
      f : AbstractNamedTrigger, 
      processedLocalForwards : List[AbstractNamedTrigger]
  ) : AbstractNamedTrigger
  
  def processGuards(
      guards : List[Guard], 
      fwd : Iterable[(Identifier,AbstractNamedTrigger)], 
      resolvedLocal : Seq[AbstractNamedTrigger]
  ) : List[Guard]
}

/**
 * Forward Expansion Structures (PES)
 */
object FES {
    type BW = Iterable[AbstractNamedTrigger]
  type FW = Null
}

class LaresForwardExpansion extends 
	LaresTraverser[FES.BW,FES.FW] with 
	AbstrTransformer[LARES_Element, LARES_Element] with 
	AbstrGuardExpansion with 
	LaresForwardExpansionHelpers with 
	Lares2Text with 
	LogicExpr_algo with 
	ftse.algorithms.TopologicalOrderedProcessing 
{

  import FES._
  
  def detForwards(
      allVisibleForwards : Iterable[(AbstrInstance,FES.BW)], 
      allVisibleGuardedTransitions : Iterable[(Identifier,GuardedTransition)]
  ) = {
    val allVC = allVisibleForwards.flatMap(l => 
          l._2.map(fwd => (l._1.identifier,Forward(fwd.name,
              fwd.cndDests.map { cndDst => 
	              ConditionalReactive(
	                  cndDst.cnd map(conditioncase => conditioncase modify {
						case ResolvedReferenceCondition(is,beh,label) => ResolvedReferenceCondition(l._1.identifier +: is,beh,label)
						case sthelse => sthelse
		              }),
		              cndDst.reactive modify {
						case ResolvedReferenceGuardLabel(is,beh,distrType,label) => ResolvedReferenceGuardLabel(l._1.identifier +: is,beh,distrType,label)
						case sthelse => sthelse
		          	  }
	              )
          	  }
          
          )))) ++ (
             allVisibleGuardedTransitions.map { ref => {
            	val dtype : Option[DistributionType] = ref._2.distr.map(a => a.distrType)
            	val lexp  : LogicalExpr = ResolvedReferenceGuardLabel(List(),ref._1, dtype , ref._2.guard)
            	
            	(ref._1,Forward(ref._2.guard, 
            	    //TypedConditionalReactive()
            	    List(
            	        dtype.map(dt => 
            	        	TypedConditionalReactive(None, lexp, dt)
            	        ).getOrElse(ConditionalReactive(None, lexp))
            	    )
                ))
             }}
          ) 
     allVC
  }
  
  /**
   * backwards method to resolve the forwards rsp guards statements
   * @param i the instance to be resolved
   * @param fw the forward information 
   * @param bws the backward tuples obtained from the child instances
   * 
   *   @returns a backward tuple comprising the resolved instance and the resolved forwards   
   */
    override def backwards(
		i : AbstrInstance,
		fw : FW,
		res : Iterable[(AbstrInstance,BW)]
    ) : (AbstrInstance,BW) = {
      val restype = i.itype.right.get
      val body = restype.body
      val allVisibleForwards = res
      val allNewInstances = res map (_._1)
      
      /* 
       * obtain all guarded transitions visible 
       */
      
      val allVisibleGuardedTransitions = restype.mixins collect {
        case ri : ResInherit => (ri.assignedId, ri.resType.body.T)
      } flatMap (a=> (a._2 collect {case t : GuardedTransition => t}) map(b=>(a._1,b)))
      
      /* adapt the namespace to all specific forwards*/
      val allVC = detForwards(allVisibleForwards, allVisibleGuardedTransitions)
      
      val allNamedTriggers = obtainNamedTriggers(body.forwarders.toList ++ body.auxStatements)
      
      // processing 
      val newNamedTriggers = (sortAndProcess[Identifier,AbstractNamedTrigger,AbstractNamedTrigger](
          allNamedTriggers, 
          _.name,
          _.cndDests.foldLeft(Set[Identifier]())((set,cndDst) =>
            set ++ GatherUnresolvedLocalForwardReferences.gather(cndDst.reactive)
          ),
          processNamedTrigger(allVC)
      ) toSet) toList //TODO: currently a hack, i.e. convert toSet and back toList to remove duplications
      
      val allGuards = obtainGuards(body.guards.toList ++ body.auxStatements)
      val newGuards = processGuards(allGuards, allVC, newNamedTriggers)
      
      val stdGuards = newGuards  collect {case g : TypedGuard => g}
      val stdForwards = newNamedTriggers collect {case f : Forward => f}
      

      
     assert(checkResolvedAbstractTriggers(stdForwards)==true, stdForwards)
     assert(checkResolvedAbstractTriggers(stdGuards)==true, stdGuards)
      
      val newBody = ModuleBody(
            List(),
			stdGuards, 		// modified
	 		body.causes,
	 		body.behaviors,
	 		allNewInstances, // modified 
	 		List(),
	 		stdForwards,
	 		List(),
	 		body.initials,
	 		(body.auxStatements filterNot {(allGuards ++ allNamedTriggers) contains}) ++ (newGuards filterNot {stdGuards contains})	 		
		)
		
      
	  val newInstance = ResolvedInstance(i.identifier,Module(restype.identifier,restype.parameters,restype.mixins,newBody),i.init)		
      
      (newInstance,newNamedTriggers)
    }
		 	  
    override def forwards (
 		  i : AbstrInstance,
 		  fw : FW
 	  ) : (AbstrInstance,FW) = {
      
      (i,null)
    }
  
  def obtainGuards(l : List[ModuleStatement]) : List[Guard] = {
    l collect {case g : LGuard => g}
  }
  
  
  def obtainNamedTriggers(l : List[ModuleStatement]) : List[AbstractNamedTrigger] = l collect {case f : Forward => f} // only extract the lares standard forwards

    /**
   * CHECKING GUARDS AND FORWARDS IF EVERYTHING IS RESOLVED
   */
  def checkResolvedAbstractTriggers(l : List[AbstractTrigger]) : Boolean = {
    l forall { t =>
      (t.cndDests forall { cndDest =>
        cndDest.cnd.map(c => c containsAtom {
          case r : ReferenceCondition => println(cndDest.toText + " found unresolved atom " + r);false
          case _ => true
        }).getOrElse(true)           
      }) &&         
      (t.cndDests forall { cndDest =>
        cndDest.reactive.containsAtom {
          case r : ReferenceGuardLabel => println(cndDest.toText + " found unresolved atom " + r);false
              case _ => true
            }       
      })   
    }
  } 
  
  def processGuards(
      guards : List[Guard], 
      fwd : Iterable[(Identifier,AbstractNamedTrigger)], 
      resolvedLocal : Seq[AbstractNamedTrigger]
  ) : List[Guard] = {
   
    
    
    guards.map { guard => 
        //println("processGuardLARESstandard:" + guard.toText);
	    val visibleForwards = fwd.map(e=> (Some(e._1),e._2)).toList ::: resolvedLocal.map((None,_)).toList
	    val cndDests = guard.cndDests.map { cndDst =>      
	      val modReactives = resolveConditionalReactive(cndDst, visibleForwards) collect {case a : ConditionalReactive => a}
	      
	      //println(modReactives.map(mr => mr.toText).mkString("MODIFIED REACTIVES\n","\n","END MODIFIED REACTIVES\n"))
	      val resRefs = modReactives map { modReactive =>
	      	GatherResolvedForwardReferences.gather(modReactive.reactive)
	      } flatten 
	      
	      assert(resRefs.toSet.size<2,
	          "\n" + guard.toText + "\n" + "refers to incompatible distributions types \n" + 
	          "\n" + resRefs.mkString(",") + "\n" + 
	          "in" + (modReactives.collect{
	            //case e : TypedConditionalReactive => e.toText + " " + e.distr.toString 
	            case sthelse => sthelse.toText+ " <untyped>"
	      	  }).mkString("- ","\n- ","\n")
	      )
	      
	      modReactives.map{modReactive =>
	         TypedConditionalReactive(modReactive.cnd, modReactive.reactive, resRefs.headOption.getOrElse(DiracType))  
	      }
	      
	    } flatten
	    
	    val tg = TypedGuard(guard.le, cndDests)
	    println("processedGuardLARESstandard:" + tg.toText);
	    
	    tg
    }
  }
  
  
  /**
   * Every Condition has a logical expression inside... here all references to the subtrees are resolved  
   */
  def processNamedTrigger(
      fwd : Iterable[(Identifier,AbstractNamedTrigger)]
  )(
      f : AbstractNamedTrigger, 
      processedLocalForwards : List[AbstractNamedTrigger] 
  ) : AbstractNamedTrigger = f match {
      case forward : Forward => {
        val visibleForwards = fwd.map(e=> (Some(e._1),e._2)).toList ::: processedLocalForwards.map((None,_))
          
        val newCndDests = f.cndDests flatMap { cndDst =>
          //modifyGuardLabelReferenceExpression(cndDst, fwd, processedLocalForwards)
          resolveConditionalReactive(cndDst, visibleForwards)
        } collect {case a : ConditionalReactive => a}
        Forward(f.name, newCndDests)
      }
    }

  def transform(e : LARES_Element) : LARES_Element = {println("LaresForwardExpansion \n"/*+ e.toText*/);e match {
  	case SpecLares(_,_,ri : ResolvedInstance) => {
  	  val forwardresult = null
  	  val system_instance = traverse(ri,forwardresult)._1
  	  SpecLares(List(),List(),system_instance)
  	}
  }}
    

}


trait LaresForwardExpansionHelpers extends LogicExpr_algo with Lares2Text with ftse.algorithms.TopologicalOrderedProcessing with LaresAlgo{
  
    def resolveConditionalReactive(
        cndReactive : AbstrConditionalReactive, 
        visibleForwards :  Iterable[(Option[Identifier], AbstractNamedTrigger)],
        genConditionalReactive : (
            Set[((Option[Identifier], Identifier), AbstrConditionalReactive)], 
            Option[LogicalExpr], 
            LogicalExpr
        ) => AbstrConditionalReactive = (_, newOptCnd, newReactive) => ConditionalReactive(newOptCnd,newReactive)
    ) : List[AbstrConditionalReactive] = {
      
        val groupedVisibleForwards = HashMap(visibleForwards.map(a=>(a._1,a._2.name) -> a._2).toList :_*)
        
    	// ********************************************************************
        // from a given reactive guard label reference expression, to all  
        // unresolved labels its associated forward statements are determined 
	    object GatherUnresolved extends Gather[(Option[Identifier], Identifier, List[AbstrConditionalReactive])] {
		      def gatherAtom(sth : LExpAtom) = {
		        sth match {
					case ReferenceGuardLabel(optRef,label) => {
					  val matchingFW = groupedVisibleForwards((optRef,label))
					  Set((optRef,matchingFW.name, matchingFW.cndDests))
					}
					
					case SYNC(refs) => refs.flatMap(e => GatherUnresolved.gather(e)).toSet 
					case MAXSYNC(refs) => refs.flatMap(e => GatherUnresolved.gather(e)).toSet
					case CHOOSE(refs) => refs.flatMap(e => GatherUnresolved.gather(e)).toSet
					
					case _ => Set()
		          }
		      }
		    }
	        
        
        println("______________________________________________________________")
        println("TOSUBSTITUTE CNDREACTIVE")
        println(cndReactive.toText)
        
        // determine those forwards referred by unresolved labels
        val unresolved = GatherUnresolved.gather(cndReactive.reactive)
        //println("UNRESOLVED")
        //println(unresolved.map(_._2.toText).mkString("\n"))
        //println(unresolved.mkString("\n"))

        // ********************************************************************
        // the determined forwards for the unresolved labels are partitioned 
        // and split up into their conditional and unconditional parts 
        
        val ll = unresolved.map(e => e._3 map(cndFw => ((e._1,e._2), cndFw))).toList
        
        
        //************************************************** NEW 
        val ll_conditionals = ll.map(e => e.filter(a => a._2.cnd.getOrElse(BooleanAtom(true))!=BooleanAtom(true)) )
        val ll_unconditional = ll.map(e => e.filter(a => a._2.cnd.getOrElse(BooleanAtom(true))==BooleanAtom(true)) )
        
        val ll_flattened = ll_conditionals.flatten(a=>a).toSet
        val ll_cond_all = ll_flattened.map(_._2.cnd.get)
        val B = SetTheory.powerSet(ll_flattened)
        
        B.map { e => val true_cnds = e.map(_._2.cnd.get)
          val false_cnds = ll_cond_all -- true_cnds
          
          val grouped = e.groupBy(_._1)
          
        }
        
        //************************************************** TODO
        
        
        //println("matrix")
        //println(ll.map(a=>a.map(_._2.toText).mkString("(",",",")")).mkString("\n"))
        
        val choices = ftse.algorithms.SetTheory.xproduct(ll)
        //println("xproduct")
        //println(choices.map(a=>a.map(_._2.toText).mkString("(",",",")")).mkString("\n"))
        
        //println("choices")
        //println(choices.map(_.map(_._2.toText).mkString(",")).mkString("\n"))

        val result = choices flatMap { choice =>
            //println("  choice: " + choice.map(_._2.toText).mkString(","))
            
          	// determine the conditional forwards  FC = {f ∈ F | π1(π2(f)) ≡ true}
            // take all conditions which are not explicitly true or if there is am implicit true, make it explicit 
            val conditionals = (choice filter { f => f._2.cnd.getOrElse(BooleanAtom(true))!=BooleanAtom(true) })
            
            // define F_C to be the set of conditional reactives of a single choice combination
	        val F_C = conditionals.toSet
	        
	        // determine the unconditional forwards  FC = {f ∈ F | π1(π2(f)) ≡ true} 
	        // i.e. just the complement of F_C
	        val F_U = choice filterNot(conditionals contains _) 
	        //println("  conditionals: "+ F_C)
            //println("  unconditionals: "+ F_U)
            
	        // ********************************************************************
	        
            // construct all kinds of combinations that arise due to the conditions
	        val B = SetTheory.powerSet(F_C)
	        
	        //println("  iteration through the power set")
	        
	        // for each combination of conditions assumed to be true ...
	        // example: assume x and y to be conditions
	        // then the power set of assumed true evaluations {{}, {x}, {y}, {x,y}} leads to (¬x,¬y), (x,¬y), (y,¬x), (x,y) 
	        val newCndFwd = B map { trueset => 
	          val falseset = F_C -- trueset
	          
	          // substitutes are defined as those conditionalreactives that are assumed to be true and true anyway (unconditional)
	          val substitutes = HashMap( (trueset ++ F_U).toList :_*)
	          
	          // modify the current conditional reactive such that 
	          // - the guard label is replaced by a matching substitute or else considered to be false
	          // - or remains unmodified
	          
	          def matcher(l : LogicalExpr) : LogicalExpr = l match {
	            case ReferenceGuardLabel(ref,label) => substitutes.get((ref,label)).map(a=>a.reactive).getOrElse(BooleanAtom(false))
	            case SYNC(refs) => refs.map(_ modify(matcher)).reduceLeft(_ & _)
	            case MAXSYNC(refs) => refs.map(_ modify(matcher)).reduceLeft(_ | _)
	            case CHOOSE(refs) => { val lst = refs.map(_ modify(matcher))
	              lst map (l => Conjunct(Negate(l),lst.filterNot(l==_).reduceLeft((a,b) => Disjunct(a,b))) : LogicalExpr) reduceLeft ((a,b) => Disjunct(a,b))
	            }
	            case foo => foo
	          }
	          
	          println(cndReactive)
	          
	          val newReactive = cndReactive.reactive modify matcher     	  
	          
	          
	          
	          //---------------------------------
	          // determine which conditional reactives have served as substitutes
	          object GatherSubstitutes extends Gather[(Option[Identifier], Identifier)] {
			      def gatherAtom(sth : LExpAtom) = sth match {
						case ReferenceGuardLabel(optRef,label) => Set((optRef,label))
						case _ => Set()
			      }
			    }
	          
	          // construct the new condition
	          val truecond = trueset.map(_._2.cnd.get)
	          val falsecond = falseset.map(n => Negate(n._2.cnd.get))
	          val newCond = (truecond ++ falsecond).foldLeft(cndReactive.cnd)((input,next) => {
	            Some(input.getOrElse(BooleanAtom(true)) & next)
	          })
	          
	          
	          // subst represent the set of substitutions that have been performed
	          val subst = GatherSubstitutes.gather(cndReactive.reactive).map(key => substitutes.get(key).map(value => (key, value))).collect{case Some(x)=>x}
	          //---------------------------------
	          
	          // generate the new conditional reactive 
	          // - by the generated new condition, 
	          // - by the new reactive expression and
	          // - giving information about which substitutions have been performed
	          println("NEW " + newReactive)
	          val redReactive = LExpReduce.reduce(newReactive)
	          println("RED " + redReactive)
	          val tmp = genConditionalReactive(subst, newCond,  redReactive)
	          println(tmp)
	          //val tmp = ConditionalReactive(newCond, LExpReduce.reduce(newReactive))
	          
	          tmp
            }
              
             
            newCndFwd
        
        }
        
        //println("RESULT")
        //println(result.map(_.toText).mkString("\n"))

       result.filterNot(res => (res.reactive == BooleanAtom(false)) | (res.reactive == BooleanAtom(true)))
    }
    
    
    
    object GatherUnresolvedLocalForwardReferences extends Gather[Identifier] {
	    def gatherAtom(sth : LExpAtom) = sth match {
	      case ReferenceGuardLabel(None,localReferenceLabel) => Set(localReferenceLabel)
	      case _ => Set()
	    }
	  }

      object GatherResolvedForwardReferences extends Gather[DistributionType] {
	    def gatherAtom(sth : LExpAtom) = sth match {
	      case ResolvedReferenceGuardLabel(a,b,c,d) if c.isDefined => Set(c.get)
	      case _ => Set()
	    }
	  }
     
}
