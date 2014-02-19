
package com.kjcondron.scalaqlite

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Failure
import scala.util.Success

import dispatch.Http
import dispatch.implyRequestHandlerTuple
import dispatch.url
	
case class HttpResult( 
		typ : String,
		brand : String,
		product : String,
		size  : String )
	

object HttpTest {
  
  final val PREFIX = "http://www.google.com/search?q="
  
  def getResult( upc : String ) : HttpResult = {
	
	val page = url(PREFIX + upc)
	val response = Http(page OK dispatch.as.String)
	
	response onComplete {
	  case Success(string) => println("got")
	  case Failure(error) => println(error)
	}
	HttpResult("Bob","","","")  
  }
  
}