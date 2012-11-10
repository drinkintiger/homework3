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
	  
	  var globalVars = findGlobalDecs(inputList)
	  var str = ""
	  globalVars.foreach(e => str+=("undef "))//
	  var globalMu = List.unzip(globalVars)._2.zip(str.split(" "))
	  
	 def sigma_global (in: List[String]): Unit = {
	    println("sigma_global")
	    print("  gamma: {")
	    globalVars.foreach( f=>print(" " +f+" ") )
	    println("}")
	    print("  mu: {")
	    globalMu.foreach(f => print(" "+f+" "))
	    println("}")
	    println("  a = "+ (a+1))
	  }
	  
	  def sigma_main_in(in: List[String]): Unit = {
	    println("sigma_main_in")
	    var tempMain = globalVars ++ findMainDecs(in)
	    var tempAlpha = a+1
	    var str = ""
	      tempMain.foreach(f => str+="undef ")
	    var tempMu = List.unzip(tempMain)._2.zip(str.split(" "))
	    print("  gamma: {")
	    tempMain.foreach( f=>print(" " +f+" ") )
	    println("}")
	    print("  mu: {")
	    tempMu.foreach(f => print(" "+f+" "))
	    println("}")
	    println("  a = " + tempAlpha)
	    if (!findMethodCalls(in).isEmpty) {	    	    	
	    	var mainBody = extractMethods(in, "main")
	    	var assignList = findAssignment(mainBody)
	    	var newList = muBuilder(tempMain, assignList)
	    	println(tempMain+ "		" + "			"+ assignList + "			"+newList)	//needed for testing remove before submitting	
	    	sigma_other_in(in, findMethodCalls(mainBody), newList)
	    }
	    sigma_main_out(tempMain, tempAlpha) 
	  }
	  
	  def sigma_other_in(in: List[String], callList: List[String], muList: List[(Any, Any)]): Unit = {
	    callList.foreach(f => fun(f))
	    def fun (inFun: String): Unit = {
	    	var methDecs = procMethodDecs(findMethodParms(in,inFun))
	        if (!methDecs.isEmpty) methDecs = methDecs.head.split(",").toList
	        var methBody = extractMethods(in, inFun)
	        println("sigma_" + inFun + "_in")
	    	var tempOther = globalVars ++ extractVars(methDecs) 
	    	var localDecs = findLocalDecs(methBody)
	    	var assignList = findAssignment(methBody)
	    	var newMu = muBuilder(tempOther, assignList)
	    	newMu = newMu ++ muList
	    	tempOther = tempOther ++ localDecs
	    	if (!isMethVoid(methBody.head)) { a+=1; var temp = List((inFun, a)); tempOther = tempOther ++ temp }	        
	    	var tempAlpha = a+1
	        print("  gamma: {")
	    	tempOther.foreach( f=>print(" " +f+" ") )
	    	println("}")
	    	println("a = " + tempAlpha)
	    	var otherBody = extractMethods(in, inFun)
	    	println(newMu)
	    	if (!findMethodCalls(otherBody).isEmpty) {sigma_other_in(in, findMethodCalls(otherBody), newMu) }
	    	other_out(tempOther, inFun, tempAlpha)
	    }
	  }
	  
	  def other_out(in: List[(String,Any)], funName: String, alpha: Int): Unit = {
	    println("sigma_" + funName + "_out")
	    print("  gamma: {")
	    in.foreach( f=>print(" " +f+" ") )
	    println("}")
	    println("a = " + alpha)
	  }
	  
	  def sigma_main_out(in: List[(String,Any)], alpha: Int): Unit = {
	    println("sigma_main_out")
	    print("  gamma: {")
	    in.foreach( f=>print(" " +f+" ") )
	    println("}")
	    println("  mu: {}")
	    println("a = " + alpha)
	  }
	  sigma_global(inputList)
	  sigma_main_in(inputList)
	}

}