
package com.kjcondron.scalaqlite

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Failure
import scala.util.Success
import dispatch._
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import java.io.InputStreamReader
import org.xml.sax.InputSource

case class HttpResult( 
		typ : String,
		brand : String,
		product : String,
		size  : String )
	

object HttpTest {
  
  final val PREFIX = "http://www.google.com/search?q="
  
  def getResult( upc : String ) : HttpResult = {
	
	val page = url(PREFIX + upc)
	val hmm = Http(page OK ( resp => new InputSource(resp.getResponseBodyAsStream()) ))
	
	val response = Http(page OK as.String)
	
	response onComplete {
	  case Success(xml) => println("got")
	  case Failure(error) => println(error)
	}
	
	val source = hmm onComplete {
	  case Success(is) => println("got is")
	  case Failure(_) => println("falied to get is")
	}
	
	val source2 = hmm.completeOption.get
	
	val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
	val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
	val deets = adapter.loadXML(source2, parser)
	println(deets.toString)
/*	def fn(s : String) = {
		val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
		val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
		adapter.loadXML(s, parser)
	}
*/
	HttpResult("Bob","","","")  
  }
  
}