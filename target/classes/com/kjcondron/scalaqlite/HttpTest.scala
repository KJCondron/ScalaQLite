
package com.kjcondron.scalaqlite

import dispatch._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Failure, Success }
import dispatch.as.String
	
case class HttpResult( 
		typ : String,
		brand : String,
		product : String,
		size  : String )
	

object HttpTest {
  
  final val PREFIX = "http://www.google.com/search?q="
  
  def getResult( upc : String ) : HttpResult = {
	
	val page = url("http://graph.facebook.com/9098498615")
	val response = dispatch.Http(page OK dispatch.as.String)
	
	response onComplete {
	  case Success(json) => println(json \ "likes")
	  case Failure(error) => println(error)
	}
	HttpResult("","","","")  
  }
  
}