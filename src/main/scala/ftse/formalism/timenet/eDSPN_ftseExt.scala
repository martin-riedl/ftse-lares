package ftse.formalism.timenet

// import statements for logical expression
import ftse.formalism.logical._
import LE_metamodel._

import ftse.formalism.lares._
import LARES_metamodel._

import ftse.formalism.timenet._


/**
 * LogicExpr2EnablingFunction:
 * provides a conversion routine @toEnablingFunction that converts the LARES 
 * extension to the Timenet-Formalism 
 */
object LogicExpr2EnablingFunction extends  Lares2Text{
	def toEnablingFunction(elem : LogicalExpr) : String = elem match {
		case a : LExpAtom => toEnablingFunction(a)
		case Conjunct(lhs,rhs) => "(" + toEnablingFunction(lhs) + " AND " + toEnablingFunction(rhs) + ")"
		case Disjunct(lhs,rhs) => "(" +toEnablingFunction(lhs) + " OR " + toEnablingFunction(rhs)+ ")"
		case Negate(elem) => "(NOT " + toEnablingFunction(elem)+")"
	}
	
	def toEnablingFunction(elem : LExpAtom) : String = elem match {
		//case AtomStr(value) => value
		case ResolvedReferenceCondition(ns,beh,state) => "#" + ns.map(_.toText).mkString("","_","_")+ beh.toText+"_"+state.toText+ "=1"
	}
}

//TODO: resolve case-to-case inheritance
case class ImmediateTransitionTypeFTSE(
  override val graphics: GraphicsType,
  override val label: LabelType,
  override val id: String,
  override val typeValue: Type,
  val priority: String,
  val weight: String,
  guardId: (List[Identifier],Int), 
  guard : LGuard,
  fullLabelRef : List[Identifier]
) extends AbstrImmediateTransitionType(
    graphics,label,id,typeValue,priority,weight,LogicExpr2EnablingFunction.toEnablingFunction(guard.le)
  ) 
