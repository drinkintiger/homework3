package hw3

import scala.collection.immutable.Nil
//test
object homework03 {
  
	import scala.io.Source
	
	def main(args: Array[String]): Unit = {
	  var inputList = Source.fromFile(args(0)).getLines.toList 
	  var outputList = List()
	  var a = -1
	  /* Various regexes used for pattern matching to parse the program */
	  val global_declarations = """int ([a-z]);""".r
	  val params = """([int [a-z]|[a-z]\,].*)""".r
	  val vars = """int ([a-z])""".r
	  val MainFun = """(int|void) main\(\) \{""".r
	  val otherFun = """(int|void) ([a-z]+)\(([int [a-z]|[a-z]\,].*)\) \{""".r
	  val assignment = """([a-z]) = ([0-9]+);""".r
	  val VoidCall = """([a-z]+)\(([[a-z]|[a-z]\,].*)\);""".r
	  val NonVoidCall = """([a-z]) = ([a-z]+)\(([int [a-z]|[a-z]\,].*)\);""".r
	  val retCall = """return ([a-z]);""".r
	  
	  /* Helper methods, to aid in parsing the given program */
	  def findGlobalDecs (in: List[String]):List[(String, Any)] = in match {
	    case Nil => List()
	    case (head::tail) => if ( (global_declarations findAllIn head.trim).nonEmpty ) { val global_declarations(varName) = head.trim; a+=1; (varName,a)::findGlobalDecs(tail)} else Nil
	  }
	  def findLocalDecs (in: List[String]):List[(String, Any)] = in match {
	    case Nil => List()
	    case (head::tail) => if ( (global_declarations findAllIn head.trim).nonEmpty ) { val global_declarations(varName) = head.trim; a+=1; (varName,a)::findLocalDecs(tail)} else findLocalDecs(tail)
	  }
	  def findMainMeth (in: List[String]): List[String] = in match {
	    case Nil => List()
	    case (head::tail) => if ( (MainFun findAllIn head.trim).nonEmpty ) { tail } else findMainMeth(tail)
	  }
	  def findOtherMeths (in: List[String]): List[String] = in match {
	    case Nil => List()
	    case (head::tail) => if ( (otherFun findAllIn head.trim).nonEmpty ) { head::findOtherMeths(tail) } else findOtherMeths(tail)
	  }
	  def findMainDecs (in: List[String]):List[(String, Any)] = in match {
	    case Nil => List()
	    case (head::tail) => if ( (MainFun findAllIn head).nonEmpty ) { findGlobalDecs(tail)} else {findMainDecs(tail)}
	  }
	  def findMethodParms (in: List[String], funName: String):List[String] = in match {
	    case Nil => List()
	    case (head::tail) => if ( (otherFun findAllIn head).nonEmpty ) { 
	    						val otherFun(funType, fun, vars) = head; 
	    						if ( funName.equals(fun) ) (vars)::findMethodParms(tail, funName)
	    						else findMethodParms(tail, funName)
	    					 }
	    					 else findMethodParms(tail, funName)
	  }
	  def findOtherMeth (in: List[String], funName: String):List[String] = in match {
	    case Nil => List()
	    case (head::tail) => if ( (otherFun findAllIn head.trim).nonEmpty ) { 
	    						val otherFun(funType, fun, vars) = head; 
	    						if ( funName.equals(fun) ) (head)::findOtherMeth(tail, funName)
	    						else findOtherMeth(tail, funName)
	    					 }
	    					 else findOtherMeth(tail, funName)
	  }
	  def procMethodDecs (in: List[String]):List[String] = in match {
	    case Nil => List()
	    case (head::tail) => if ( (params findAllIn head).nonEmpty ) { val params(vars) = head; vars::procMethodDecs(tail)} else procMethodDecs(tail)
	  }
	  def extractVars (in: List[String]):List[(String, Any)] = in match {
	    case Nil => List()
	    case (head::tail) => if ( (vars findAllIn head.trim).nonEmpty ) { val vars(varName) = head.trim; a+=1; (varName,a)::extractVars(tail)} else extractVars(tail)
	  }
	  def findAssignment (in: List[String]): List[(String,String)] = in match {
	    case Nil => List()
	    case (head::tail) => if ( (assignment findAllIn head).nonEmpty ) { val assignment(variable, value) = head.trim; (variable,value)::findAssignment(tail) } else findAssignment(tail)
	  }
	  def findMethodCalls (in: List[String]): List[String] = in match {
	    case Nil => List()
	    case (head::tail) => if ( (NonVoidCall findAllIn head).nonEmpty ) { val NonVoidCall(varName, funName, _) = head.trim; (funName)::findMethodCalls(tail) }
	    else if ( (VoidCall findAllIn head).nonEmpty ) { val VoidCall(funName, params) = head.trim; (funName)::findMethodCalls(tail)}
	    						else findMethodCalls(tail)
	  }
	  def getMethRetVal (in: List[String]): String = in match {
	    case Nil => ""
	    case (head::tail) => if ( (retCall findAllIn head).nonEmpty ) { val retCall(varName) = head.trim; varName} else getMethRetVal(tail)
	  }
	  /* If function return type is void returns true, otherwise false */
	  def isMethVoid (methName: String): Boolean = methName match {
	    case "" => false
	    case fun => if ( (otherFun findAllIn fun).nonEmpty ) { val otherFun(funType, funName, _) = fun; if (funType.equals("void")) true else false } else false
	  }
	  
	  /* This will extract the body of the method
	   * @parama funName the name of the method to extract 
	   */
	  def extractMethods(in: List[String], funName: String): List[String] = {
	    var temp = List[String]()
	    if (funName.equals("main")) temp = findMainMeth(in)
	    else temp = findOtherMeth(in, funName)
	    var newList = in.dropWhile(f => !f.equals(temp.head))
	    newList = newList.takeWhile("}" !=)
	    newList
	  }
	  
	  def muBuilder(inGamma: List[(Any, Any)], inAssign: List[(Any, Any)]): List[(Any, Any)] = {
	    var adr =""
	    var value=""
	    def findPair(s: (Any, Any), inAssign: List[(Any, Any)]): (Any, Any) ={
	      inAssign.foreach(f => if(s._1.equals(f._1.toString())) return(s._2, f._2)
	        else {adr = s._2.toString; value = "undef"}	)
	      return (adr, value)
	    }
	    
	    var ls = List[(Any, Any)]()
	    for(e<-inGamma) {
	      ls = findPair(e, inAssign)::ls
	    }
	      
	    if(!inAssign.isEmpty){
	      ls
	    }
	    else Nil	    
	  } 