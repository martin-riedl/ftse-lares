package ftse.formalism.lares

import ftse.formalism.logical._
import ftse.formalism.arith._
import ftse.formalism.set._
import LARES_metamodel._

trait Lares2Text {
	implicit def l2t(e : LARES_Element) = new L2T(e)
}

class L2T(e : LARES_Element) extends CLares2Text {
  def toText : String = toText(e)
}

/**
  * serialize an identifier to spa
  */
object IdentifierSerializer extends ArithExprSerializerImpl {
  
    def plainType(identifier : Identifier) : String = {
      identifier.name + identifier.indices.map(a=>a.toText).mkString("_")
    }
    
    def laresType(identifier : Identifier) : String = {
      val index = if (identifier.indices.size==0) "" 
    		else (for (i <- identifier.indices) yield {
    			i.toText
    		}).mkString("[",",","]")
    		
    		identifier.name + index
    }
}

/*
trait Identifier_Serializer extends ArithExprSerializerImpl {
	def Id2Text(identifier : Identifier) : String = {
    		 
    		val index = if (identifier.indices.size==0) "" 
    		else (for (i <- identifier.indices) yield {
    			i.toText
    		}).mkString("[",",","]")
    		
    		identifier.name + index
    }    
}
*/

class Id2Text(identifier : Identifier) {
	def toText : String = IdentifierSerializer.laresType(identifier)  
}


trait LARESLogicExpr2TextImpl {
  implicit def toText(l : LogicalExpr) = (new LARESLogicExpr2Text(l))
}


class LARESLogicExpr2Text(l : LogicalExpr) extends LogicExpr_Serializer {
	def toText : String = toText(l)
}

trait LogicExpr_Serializer extends LogicExpr2Text {
	import LE_metamodel._

	implicit def implId2Text(l : Identifier) = new Id2Text(l)
	
      override def toText(elem : LExpAtom) : String = elem match {
	  	case a : AND_RefCnd_Expand => a.toString
	  	case b : OR_RefCnd_Expand => b.toString
        case ReferenceCondition(idInst, idCond) => (if (idInst.isDefined) { idInst.get.toText + "." } else {""}) + idCond.toText 
        case ResolvedReferenceCondition(instances,beh, idCond) => (if (instances.size>0) instances.map(i=>i.toText).mkString("",".",".") else "") + beh.toText + "." + idCond.toText
	    case ReferenceGuardLabel(obj,gl) => (if (obj.isDefined) (obj.get.toText + ".") else "")+ "<"+gl.toText + ">"
	    case ResolvedReferenceGuardLabel(instances,behavior,distrType,gl) => (if (instances.size>0) instances.map(_.toText).mkString("",".",".") else "") + behavior.toText + "." + "<"+gl.toText + ">"
        case _ => elem.toString
      }  
}

trait Behavior2Dot extends ArithExprSerializerImpl {
  implicit def implId2Text(l : Identifier) = new Id2Text(l)
	
  def transform(ns : List[Identifier], b : LBehavior, additionalNodeParam : String = "") : String = {
    val p = ns.map(_.toText).mkString("_")
    val resState = b.body.S.map(s => {
      '"'+ p+"_"+s.identifier.toText + '"' + "[label="+'"' + s.identifier.toText +'"' +" style=filled "+additionalNodeParam+" shape=Mrecord]";
    })
    
    def distrToText(d : Option[Distribution]) : String = d match {
      case Some(Exponential(a)) => "r:"+a.toText
      case Some(Dirac(a)) => "w:"+a.toText
      case _ => "impl"
    }  
    val resTrans = (b.body.T collect {
      case GuardedTransition(s,t,l,d) => 
        '"' + p+"_"+s.identifier.toText + '"' + " -> " +  '"' +p+"_"+t.identifier.toText + '"' + "[label=\"(" + l.toText+","+distrToText(d) + ")\", style=dashed];"
      case UnguardedTransition(s,t,d) => 
        '"' + p+"_"+s.identifier.toText + '"' + " -> " +  '"' +p+"_"+t.identifier.toText + '"' + "[label=\"("+distrToText(d) + ")\", style=solid];"
    })
    
    "  subgraph cluster_"+p.replace('[','_').replace(']','_') + "{" + "\n" +
    "    edge[dir=none,weight=10];" + "\n" +
    "    ratio = \"auto\" ;" + "\n" +
    "    mincross = 2.0 ;" + "\n" +
    "    "+additionalNodeParam + "\n" +
    "    label ="+ '"' + p + '"' + "\n" +
    (resState ++ resTrans).mkString("    ", "\n    ", "\n") + 
    "  }" 
  }
}


abstract class AbstrLares2Text {
  def toText(e : LARES_Element, depth : Int) : String
}

class CLares2Text extends AbstrLares2Text  with SetExpr2Text with ArithExprSerializerImpl {
	implicit def implLogicExprToText(l : LogicalExpr) = new LARESLogicExpr2Text(l)
	implicit def implId2Text(l : Identifier) = new Id2Text(l)
	
    def Module2Text(mdef : ModuleDefinition, name :String, depth:Int) = {
      "\n"+" "*depth+ name + mdef.identifier.toText + (mdef.parameters.map(toText(_)).mkString("(",",",")")) + 
      (if (mdef.mixins.isEmpty) "" else mdef.mixins.map(a => toText(a,depth)).mkString(" : ",", ","")) + " " +
      ModuleBody2Text(mdef.body,depth) 
    }
	

	
    def ModuleBody2Text(mdef : ModuleBody, depth : Int) = {
    	val bodytext = (if (mdef.behaviors.size == 0) "" else mdef.behaviors.map(beh => toText(beh,depth+2)).reduceLeft(_+ "\n"+_) + "\n")  +
		  (if (mdef.modules.size   == 0) "" else mdef.modules.map(mod => "\n" + toText(mod,depth+2)).reduceLeft(_ + "\n"+_) + "\n") +
		  (if (mdef.instances.size == 0) "" else mdef.instances.map(i => "\n" + toText(i,depth+2)).reduceLeft(_+_) + "\n") +
		  (if (mdef.guards.size == 0) "" else mdef.guards.map(g => "\n" + toText(g,depth+2)).reduceLeft(_+_) + "\n") +
		  (if (mdef.causes.size == 0) "" else mdef.causes.map(c => "\n" + toText(c,depth+2)).reduceLeft(_+_) + "\n") +
		  (if (mdef.forwarders.size == 0) "" else mdef.forwarders.map(f => "\n" + toText(f,depth+2)).reduceLeft(_+_) + "\n") +
		  (if (mdef.conditions.size == 0) "" else mdef.conditions.map(c => "\n" + toText(c,depth+2)).reduceLeft(_+_) + "\n") +
		  (if (mdef.expandables.size == 0) "" else mdef.expandables.map(forstmt => "\n" + toText(forstmt,depth+2)).reduceLeft(_+_) + "\n") +
		  (if (mdef.initials.size == 0) "" else mdef.initials.map(initial => "\n" + toText(initial,depth+2)).reduceLeft(_+_) + "\n") +
		  (if (mdef.auxStatements.size == 0) "" else mdef.auxStatements.map("\n" + toText(_,depth+2)).reduceLeft(_+_) + "\n") 
		  
	   if (bodytext.length>0)
    	" "*(depth)+"{" + bodytext +
    	" "*depth+"}"
	  else ""
    }
    
    def toText(e : LARES_Element) : String = toText(e,0)
    
    def toText(e : LARES_Element, depth : Int) : String = {
      e match {
   	    case MeasureSteadyStateProbability(ns,l) => "Probability " + ns.map(_.toText).mkString("","."," = SteadyState(") + l.toText + ")" 
	    case MeasureTransientStateProbability(ns,l,t) => "Probability " + ns.map(_.toText).mkString("","."," = TransientState(") + l.toText +","+ t.toText +  ")"

      	case m : ModuleBody => ModuleBody2Text(m, depth) 
    	case SpecLares(behaviors,modules,system) => {
    		val statements = 	behaviors.map(beh => toText(beh,depth)).toList ::: 
    							modules.map(beh => toText(beh,depth)).toList ::: 
    							List(toText(system,depth))
    		if (statements.size==0) "" else statements.reduceLeft(_ + "\n" +_)
		}
    	case inh : Inherit => inh.typeId.toText + (inh.params.map(toText(_)).mkString("(",",",")"))
    	
    	case inh : ResInherit => inh.typeId.toText + (inh.params.map(toText(_)).mkString("(",",",")")); toText(inh.resType,depth+2)
    	 
    	case s : State => "State " + s.identifier.toText
    	//case t : Transition => "Transition " + t.from 
      
	    case BehaviorBody(states,t,c,fstmt) => {
	      val states_text = (if (states.toSeq.size==0) "" else "\n"+" "*(depth+2) + "State " + states.map(s => s.identifier.toText).mkString("",", ",""))
	 
	    	  val transitions = scala.collection.mutable.HashMap[State,List[Transition]]()
		      for ( e<- t) yield e match {
		        case e : Transition => transitions(e.from) = e :: transitions.getOrElse(e.from,List()) 
		        case _ => 
		      }
	    
	      val transitions_text = for (key <- transitions.keys) yield ("\n"+" "*(depth+2) + "Transitions from " + key.identifier.toText + transitions(key).map("\n"+" "*(depth+4) + toText(_)).reduceLeft(_+_))
	      
	      
	      val fstmt_text = (if (fstmt.size == 0) "" else fstmt.map(forstmt => toText(forstmt,depth+2)).reduceLeft(_+_) + "\n")
	      
	      
	      val statements = (fstmt_text :: states_text :: transitions_text.toList)
	      
	      " {" +
	          (if (statements.size==0) "" else statements.reduceLeft(_+_)) +
	          //(if (body.vd.size == 0) "" else body.vd.map(c => toText(c,depth+2)).reduceLeft(_+_)) +
	      "\n"+" "*depth+"}"
	    }
	    
	    case LBehavior(label,params,body,initial) => {   
	      "\n"+" "*depth+"Behavior " + label.toText + (if (initial.isDefined) (" initial " + initial.get.toText) else "" ) +
	      //    	(if (mixins.size    == 0) "" else mixins.mkString(" : ",", ",""))  + 
	      toText(body,depth)
	    }
	
	    case mdef : Module  => Module2Text(mdef,"Module ",depth)
	    case mdef : System => Module2Text(mdef,"System ",depth)
	    case Instance(label,typeid,params,inits) => " "*depth + 
	    	"Instance " + label.toText +  
	    	(if (!inits.isEmpty) {" initially " + inits.map(_.toText).mkString("",".","")} else {""}) +
	    	" of " + typeid.toText + "(" + params.map(toText(_)).mkString("",",","") +  ")"
	    
	    case LGuard(le,cDsts) => " "*depth + le.toText + " guards {" + 
	      cDsts.map(toText(_,depth+2)).mkString("\n","\n","\n") + 
	    " "*(depth+2) + "}"
	    
	    case ConditionalReactive(cond, reactive) => " "*depth + cond.map("if " + _.toText).getOrElse("if <true>") + " " + reactive.toText
	    
	    case TypedConditionalReactive(cond,reactive, distr) => " "*depth + cond.map("if " + _.toText).getOrElse("if <true>") + " " + reactive.toText + " " + distr.toString + " TCR" 
	    
	    case TypedGuard(le,cDsts) => " "*depth + le.toText + " T_guards {" + 
	      cDsts.map(toText(_,depth+2)).mkString("\n","\n","\n") + 
	    " "*(depth) + "}"
	    
	    //case Cause(le,ariths) => " "*depth + le.toText + " causes " + ariths.map(a=> a._1.mkString("",".","=") + a._2.toText).reduceLeft(_+_)
	    
	    case Forward(l,cDsts) => " "*depth +  "forward <" + l.toText + "> to {" + 
	      cDsts.map(toText(_,depth+2)).mkString("\n","\n","\n") + 
	    " "*(depth) + "}"
	    
	    case GuardedTransition(from,to,guard,distr) => 
	      "if <"+guard.toText+">" + " -> " + to.identifier.toText + (if (distr.isDefined) toText(distr.get) else "") 
	    case UnguardedTransition(from,to,distr) => 
	        "if <true> -> " + toText(to.identifier) + (if (distr.isDefined) toText(distr.get) else "")
	        
	    case Exponential(rate) => ", delay exponential " + rate.toText
	    case Dirac(weight) => ", weight " + weight.toText
	    case ExpandModuleBody(iters,body : ModuleBody) => " "*depth + "for " + iters.map(toText(_,depth)).mkString("(",",",")") + "\n" + ModuleBody2Text(body,depth)
	    case ExpandBehaviorBody(iters,body : BehaviorBody) => " "*depth + "for " + iters.map(toText(_,depth)).mkString("(",", ",")") + toText(body,depth)
	    case Iterator(label,range) => label +" in {" + toText(range) + "}"
	    case Range(start,end) => start.toText + " .. " + end.toText
	    case Condition(l,lexp) => " "*depth + "Condition " + l.toText + " = " + lexp.toText
	    
	    case id : Identifier => id.toText
	    case Initial(name,inits) => " "*depth + "Initial " + name.toText + " initially " + inits.map(a=>ReferenceInitial2Text(a)).mkString("",", ","")
	    case ResolvedInstance(name,module, inits) => " "*depth + "Instance " + name.toText + 
	    															  (if (inits.isEmpty) "" else (" initially " + inits.map(a=>a.toText).mkString("",", ",""))) +
	    															  " of " + module.identifier.toText + "(" + module.parameters.map(toText(_)).mkString("",",","") +  ")" + toText(module,depth+2)
	    case Parameter(name, defined) => name + (if (defined.isDefined) {"=" + defined.get.toText} else "")
	    case x => {
	      println(e); "Serialisation Error: don\'t know " + e.toString 
	      //throw new Exception("Serialisation Error: don\'t know " + e.toString)} 
	    }
  }}
  
  def ReferenceInitial2Text(r : ReferenceInitial) : String = r.ref.toText + "." + r.idInitial.toText
  def DefinitionRef2Text(r : ReferenceDefinition) : String = {
	  r.name + 
	  //	if (!l.parameters.isEmpty) {"(" + ")"}  else "" + 
	    //if (!indices.isEmpty) {(for (i <- indices) yield "["+i+"]").mkString("","","")} else ""+ 
	  ""
  }
  
  
  
}

