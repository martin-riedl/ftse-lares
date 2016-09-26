package ftse.transformations.spa

import ftse.formalism.lares.LARES_metamodel.{
  MeasureSteadyStateProbability, 
  MeasureTransientStateProbability, 
  Measure, 
  ResolvedReferenceCondition, 
  MacroStateMeasure
}

import ftse.formalism.lares.Lares2Text
import ftse.transformations.AbstrTransformer

// import statements for logical expression
import ftse.formalism.logical._
import LE_metamodel._
import ftse.formalism.lares.{IdentifierSerializer => IS}

trait LaresMeasure2SPA {
	def convertMeasures(measures : List[Measure]) = measures map { _ match { 
	  case MeasureSteadyStateProbability(ns, macroState) => {
	    ns.map(IS.plainType(_)).mkString("","_","")
	  }   
	}}
}

class MeasureMacroState2SPA(prefix : List[String] = List()) extends AbstrTransformer[LogicalExpr,String] with Lares2Text {
  def Measure2SPA(m : MacroStateMeasure) = "statemeasure " + m.ns.map(IS.plainType(_)).mkString("_") + " " + transform(m.macroState)   
  def transform(l : LogicalExpr) : String = l match {
    case a : ResolvedReferenceCondition => {
      val P_NS = (prefix ::: a.instanceReferences.map(IS.plainType(_))).mkString("","_","_") + IS.plainType(a.behaviorReference) 
      
      P_NS + "(state="+P_NS + "_"+IS.plainType(a.namedStatementRef) + ")"
    }
    case Conjunct(lhs, rhs) => "(" + transform(lhs) + " & " + transform(rhs) + ")"
    case Disjunct(lhs, rhs) => "(" + transform(lhs) + " | " + transform(rhs) + ")"
    case Negate(elem) =>  "!" + "(" + transform(elem) + ")"
  }  
}
