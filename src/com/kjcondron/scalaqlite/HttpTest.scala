
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
  
  def outputResults = words.foreach (e => if(e._2>3) println(e))
  
  var inLink : Boolean = false
  var inBody : Boolean = false
  var inResults = false
  var parseLink = false
  var done = false
  
  var words : Map[String, Int] = Map()
  
    override def startElement( uri : String, localName : String,
                      name : String, a : Attributes)
    {
    	if(localName.equalsIgnoreCase("body"))
    	    inBody = true
    	
        if(inBody && name.equals("a")) {
          if(inLink)
            println("nested link")
          else
            inLink = true
            
          if(inResults)
          {
    		inResults = !a.getValue("href").contains("advanced_search")
    		done = true
          }
    
        }
    	
    	if(!parseLink && inResults && inBody && inLink)
    	  parseLink = !a.getValue("href").contains("google")
    	
 /*   	if(parseLink)
    	{
    	  println("<elem>")
    	  println("href:" + a.getValue("href"))
    	}
  */  	
    }
    
    override def characters( ch : Array[Char], start : Int, length : Int) : Unit =
    {   
      if(parseLink)
      {
        val str = new String(ch, start, length)
    //    println("<char>")
        //println("chars: " + length + ":" + ch.size + ":" + ch.foldLeft("")(_+_))
    //    println("chars: " + str)
    //    println("</char>")
        
        val localWords = str.split(" ")
        localWords.foreach( w => {
        	val lw = w.toLowerCase()
        	if( words.keys.exists(_==lw) )
        	{
        	    val count = words(lw) + 1
        	  	words += (lw -> count)
        	}
        	else
        	  words += (lw -> 1)
        	
        })
      }
      
      if(!inResults && inBody && inLink)
    	  inResults = ch.foldLeft("")(_+_).contains("Verbatim")
    }
    
    override def endElement( uri : String, localName : String, name : String ) = {
      
      /*if(inResults && inLink && parseLink)
      {
    	  println("</elem>")
      }*/
      
      if(inLink)
        inLink = false
        
      if(parseLink)
        parseLink=false
    }
}		
	

object HttpTest {
  
  final val PREFIX = "https://www.google.com/search?q="
  
  def getResult( upc : String ) : Map[String,Int] = {
    
 /*   SAXParserImpl.newInstance(null).parse(
	        new URL("https://www.yahoo.com/").openConnection().getInputStream(),
            new MyHandler()) 
*/	
	//val page = url("https://www.yahoo.com/")
    val page = url(PREFIX+upc)
    println(PREFIX+upc)
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
	val h1 = new MyHandler
	val h2 = new MyHandler
	println("tag soup parse")
	val st = System.nanoTime()
	val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
	parser.parse(hmm2, h1)
	val end = System.nanoTime()
	
	val hmm3 = Http(page OK ( resp => new InputSource(resp.getResponseBodyAsStream()) ))
	val hmm4 = hmm3()
	
	//println("SAX parse")
	val st1 = System.nanoTime()
	SAXParserImpl.newInstance(null).parse(
			hmm4,
            h2)
    val end1 = System.nanoTime()
    
  //  println("Soup Time " + (end - st) / 1000 )
  //  println("Sax  Time " + (end1 - st1) / 1000 )
    
  //  h1.outputResults
  //  h2.outputResults
  
   // Http.shutdown
  
	h2.words.filter( { case(_,v) => v > 3 } )
  }
  
  def shutdown = Http.shutdown
  
}