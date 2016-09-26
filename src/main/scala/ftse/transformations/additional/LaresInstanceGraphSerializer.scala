package ftse.transformations.additional

import ftse.formalism.lares._
import scala.collection.immutable._
import ftse.formalism.lares.Behavior2Dot

import ftse.transformations._
import ftse.transformations.lares._
import LARES_metamodel._

object IGS extends Lares2Text {
	type FW = (Int,List[Identifier])
	type BW = List[String]
}

	
class LaresInstanceGraphSerializer(e : LARES_Element) extends 
  LaresTraverser[IGS.BW,IGS.FW] with 
  AbstrTransformer[LARES_Element, String] with 
  Behavior2Dot
{
  import IGS._ 
 
	
  def transform : String = transform(e)
	
	def transform(e : LARES_Element) : String = {e match {
	  	case SpecLares(_,_,ri : ResolvedInstance) => {
	  	  val ret = traverse(ri,(1,List()))._2
	  	  
	  	  val dot : String = 
	  	    "digraph g {" +
	  	    "  compound=true;" +
	  	    "  nodesep=0.5;" +
	  	    "  size=\"6,2\"; ratio = fill;" +
	  	    "  margin=\"0,0\";" +
	  		ret.mkString("  ","\n  ","\n") +
		  "\n}"
		   
		  
		   dot
	  	}
	  }
	}
	
    def forwards(
	    ri : AbstrInstance,
	 	fw : FW
	) : (AbstrInstance,FW) = {
		 val (rank,ns) = fw
 	 	 (ri,(rank+1, ns ::: List(ri.identifier)))
	}
	
	def backwards(
	    ri : AbstrInstance,
	    fw : FW,
	    bw : Iterable[(AbstrInstance,BW)]
	) : (AbstrInstance,BW) = {
		val (rank,ns) = fw
		val m = ri.itype.right.get
		val deeper = bw.map(_._2) flatten
		val conNS = ns.map(_.toText).mkString("","_","")
		
		val instbehdep = m.mixins.map { ib =>
		      '"' + conNS + "_" + ib.assignedId.toText + '"' + "->" + '"' + conNS  + '"'  + " [fontsize=5 label=\"" + "inherits behavior instance" + "\" arrowhead=odiamond style=solid];"
		}
		
		val ts = m.mixins collect { case ib : ResInherit =>
		  "\n" + transform(ns :+ib.assignedId ,ib.resType, "rank="+(rank+1)) + "\n" + 
		  "  " + '"' + (ns :+ib.typeId :+ ib.resType.body.S.last.identifier).map(_.toText).mkString("_") + '"' +
		  " -> " +
		  '"' + conNS + "_" + ib.assignedId.toText + '"' + // 
		  "[ltail=cluster_"+(ns :+ib.typeId).map(_.toText).mkString("_").replace('[','_').replace(']','_')+ "]" +
		  "\n"
		}
		
		
		val instbehnodes = m.mixins collect { case ib : ResInherit =>
		      '"' + conNS + "_" + ib.assignedId.toText + '"'+ " [label="+ 
		      "<<table>" +
		      "<tr><td bgcolor='#FFFFCC' colspan=\"2\">"+conNS+"_" + ib.assignedId.toText +"</td></tr>" +
		      "<tr><td>gen</td><td bgcolor='#CCCCCC'>"+ ib.resType.body.S.map(s => s.identifier.toText).mkString(",") + "</td></tr>" +
		      "<tr><td>react</td><td bgcolor='#AAAAAA'>"+ (ib.resType.body.T.collect {
		        case g : GuardedTransition => g.guard.toText
		      }).mkString(",") + "</td></tr>" +
		      "</table>>" +
		      "rank=" +rank+ " shape=plaintext"+  "];"
			
		}
		
		
		
		val depchildren = m.body.instances.map(
		    '"' + conNS + "_" + _.identifier.toText + '"' + "->" + '"' + conNS + '"'  + " [fontsize=5 label=\"" + "is child of" + "\" arrowhead=odiamond style=solid];" 
		)
		 
		val node = '"' + conNS + '"' + " [label=" +
			"<<table>" +
			"<tr><td bgcolor='#CCCCFF' colspan=\"2\">"+conNS+"</td></tr>" +
			"<tr><td>gen</td><td bgcolor='#CCCCCC'>"+ m.body.conditions.map(_.identifier.toText).mkString(",") + "</td></tr>" +
			"<tr><td>react</td><td bgcolor='#AAAAAA'>"+ m.body.forwarders.map(_.name.toText).mkString(",") + "</td></tr>" +
			/*ri.restype.body.guards.map("<tr><td colspan=\"2\">"+_.toText+"</td></tr>").mkString("\n") +*/ 
			"</table>>" +
			"rank=" +rank+ " shape=plaintext"+  "];"
		    
		
		(ri,ts.toList ++ List(node) ++ depchildren ++ deeper ++ instbehdep ++ instbehnodes)
	}

}

class LaresInstanceGraphSerializerSimple(e : LARES_Element) extends 
  LaresTraverser[IGS.BW,IGS.FW] with 
  AbstrTransformer[LARES_Element, String] with 
  Behavior2Dot
{
  import IGS._ 
 
	
  def transform : String = transform(e)
	
	def transform(e : LARES_Element) : String = {e match {
	  	case SpecLares(_,_,ri : ResolvedInstance) => {
	  	  val ret = traverse(ri,(1,List()))._2
	  	  
	  	  val dot : String = 
	  	    "digraph g {" +
	  	    ret.mkString("  ","\n  ","\n") +
		  "\n}"
		   
		  
		   dot
	  	}
	  }
	}
	
    def forwards(
	    ri : AbstrInstance,
	 	fw : FW
	) : (AbstrInstance,FW) = {
		 val (rank,ns) = fw
 	 	 (ri,(rank+1, ns ::: List(ri.identifier)))
	}
	
	def backwards(
	    ri : AbstrInstance,
	    fw : FW,
	    bw : Iterable[(AbstrInstance,BW)]
	) : (AbstrInstance,BW) = {
		val (rank,ns) = fw
		val m = ri.itype.right.get
		
		val deeper = bw.map(_._2) flatten
		val conNS = ns.map(_.toText).mkString("","_","")
		
		val instbehdep = m.mixins.map { ib =>
		      '"' + conNS + "_" + ib.assignedId.toText + '"' + "--" + '"' + conNS  + '"'  + 
		      //" [fontsize=5 label=\"" + "inherits behavior instance" + "\"" +
		      " [fontsize=5 label=\"" + "" + "\"" +
		      " arrowhead=odiamond style=solid];"
		}
		
		val instbehnodes = m.mixins collect { case ib : ResInherit =>
		      '"' + conNS + "_" + ib.assignedId.toText + '"' +"[color=orange rank=" +(rank+1)+ "];"
		}
		
		val depchildren = m.body.instances.map(
		    '"' + conNS + "_" + _.identifier.toText + '"' + "--" + '"' + conNS + '"'  + 
		    	//" [fontsize=5 label=\"" + "is child of" + "\"" + 
		        " [fontsize=5 label=\"" + "\"" +
		    	" arrowhead=odiamond style=solid];" 
		)
		 
		val node = '"' + conNS + '"' + "[rank=" +rank+ "];"
		
		(ri,List(node) ++ depchildren ++ deeper ++ instbehdep ++ instbehnodes)
	}

}

