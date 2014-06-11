
package com.kjcondron.scalaqlite

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Failure
import scala.util.Success
import dispatch._
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import java.io.InputStreamReader
import org.xml.sax.InputSource
import org.ccil.cowan.tagsoup.jaxp.SAXParserImpl
import java.net.URL
import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.Attributes

case class HttpResult( 
		typ : String,
		brand : String,
		product : String,
		size  : String )
		
class MyHandler extends DefaultHandler {
  
  var inLink : Boolean = false
  
    override def startElement( uri : String, localName : String,
                      name : String, a : Attributes)
    {
    	if(localName.equalsIgnoreCase("table"))
    	  println("table")
        if(localName.equalsIgnoreCase("a")){
          if(inLink)
            println("nested link")
          else
            inLink = true
        }
    }
    
    override def characters( ch : Array[Char], start : Int, length : Int) : Unit =
    {
      if(inLink)
      {
        println(ch.foldLeft("")(_+_))
      } 
    }
    
    override def endElement( uri : String, localName : String, name : String ) =
      inLink = false
}		
	

object HttpTest {
  
  final val PREFIX = "http://www.google.com/search?q="
  
  def getResult( upc : String ) : HttpResult = {
    
 /*   SAXParserImpl.newInstance(null).parse(
	        new URL("https://www.yahoo.com/").openConnection().getInputStream(),
            new MyHandler()) 
*/	
	//val page = url("https://www.yahoo.com/")
    val page = url(PREFIX+upc)
    val hmm = Http(page OK ( resp => new InputSource(resp.getResponseBodyAsStream()) ))
//	val is = hmm.completeOption
	val hmm2 = hmm()
	
	/*val response = Http(page OK as.String)
	
	response onComplete {
	  case Success(xml) => { println("got"); println(xml) }
	  case Failure(error) => println(error)
	}*/
	
	/*var vis : Option[InputSource] = None
	hmm onComplete {
	  case Success(is) => { println("got input"); vis = Some(is) }
	  case Failure(_) =>  { println("no input"); vis = None }
	}
	
	vis.map( _=>println("really got") )*/
	
	//val source2 = hmm.completeOption.get
	
//	val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
//	val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
//	val deets = hmm.completeOption.map(s=>adapter.loadXML(s, parser))
//	println(deets.toString)
/*	def fn(s : String) = {
		val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
		val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
		adapter.loadXML(s, parser)
	}
*/
	println("tag soup parse")
	val st = System.nanoTime()
	val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
	parser.parse(hmm2, new MyHandler)
	val end = System.nanoTime()
	
	val hmm3 = Http(page OK ( resp => new InputSource(resp.getResponseBodyAsStream()) ))
	val hmm4 = hmm3()
	
	println("SAX parse")
	val st1 = System.nanoTime()
	SAXParserImpl.newInstance(null).parse(
			hmm4,
            new MyHandler)
    val end1 = System.nanoTime()
    
    println("Soup Time " + (end - st) / 1000 )
    println("Sax  Time " + (end1 - st1) / 1000 )
  
    Http.shutdown
  
	HttpResult("Bob","","","")  
  }
  
}