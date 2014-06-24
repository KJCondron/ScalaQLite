
package com.kjcondron.scalaqlite

import java.sql.DriverManager
import java.sql.ResultSet
import types._
import com.kjcondron.scalaqlite.ResultSetIterator._
import scala.language.implicitConversions
import java.sql.Connection

sealed class Result

final case class StringResult( string : String ) extends Result {
  override def toString = string
}
final case class IntResult( int : Int ) extends Result {
  override def toString = int.toString
}
final case class DoubleResult( double : Double) extends Result {
  override def toString = double.toString
}

package object types {
  type Row = Map[String,Result] 
}

object ScalaQLiteHTTP extends App {
  
  final val MASTER_SQL = """Select * from sqlite_master"""
  final val TABLES_SQL = """Select type,tbl_name from sqlite_master"""
  final val UPC_SQL = """Select * from vProducts where ean = 1111"""  
  final val BARS_SQL = """select * from Bars"""  
  final val BAR_SQL = """select * from vInventory where bar_id=1"""
  final val DELTE_BARS_SQL = """delete from Bars where _id>1"""
  final val DELTE_INV_SQL = """delete from Inventory where bar_id>1"""
    
  //val httpRes = HttpTest.getResult("088076178656")
  
  Class.forName("org.sqlite.JDBC")
    
  val connection = DriverManager.getConnection(
      """jdbc:sqlite:C:\Users\Karl\Documents\GitHub\BarKeepUtil\Data\db\barkeep__20140216""")
  
  val stmt = connection.createStatement
  
  val res = stmt.executeQuery(MASTER_SQL)
  
  val md = res.getMetaData()
  val cols = for(i <- 1 to md.getColumnCount())
    yield md.getColumnName(i)
    
  cols.foreach(println)
  println("st")
  res.foreach( r => println(r.take(2).mkString(":")))
  println("end")
  
  val tables = stmt.executeQuery(TABLES_SQL)  
  val tl = tables.toList
  tl.foreach( t =>  println(t.mkString(":")) )
  
  val bars = stmt.executeQuery(BARS_SQL)  
  bars.foreach( t =>  println(t.mkString(":")) )
  
  val bars2 = stmt.executeQuery(BARS_SQL)
  val mdb = bars2.getMetaData()
  
  val bcols = for(i <- 1 to mdb.getColumnCount())
    yield mdb.getColumnName(i)
  
  bcols.foreach(println)
  
  val upcres = stmt.executeQuery(BAR_SQL);
  SQLiteHelper.printResultSetInfo(upcres)
  val upcRows = upcres.take(100).toList
  upcRows.foreach( r => {
    println( r.mkString("PRODROW:", ",", "") + ":" + HttpTest.getResult( r("upc").toString ).mkString("HTTPRES:", ",", "" ))
    Thread.sleep(1000)
  })
  
  HttpTest.shutdown
  
// val upcs = upcRows.map(_("upc"))
// upcs.foreach(println)
  
//  stmt.executeUpdate(DELTE_BARS_SQL)
//  stmt.executeUpdate(DELTE_INV_SQL)
  stmt.close()
  connection.close()
 //val https = upcs.map( x=> HttpTest.getResult( x(6).toString ) )
}

object ResultSetIterator {
	implicit def apply( r : ResultSet ) : Iterator[Row] = if(r.isAfterLast) Iterator.empty else new ResultSetIterator(r) 
}

class ResultSetIterator( r : ResultSet ) extends Iterator[Row]
{
    if(r.isBeforeFirst()) r.next
	private val mMetaData = r.getMetaData
	private val mColCount = mMetaData.getColumnCount
	
	// r is non functional so it is not surprising we need a var to handle it
	var hasNext = !r.isAfterLast()
	
	private def getKey(ic : Int) : String = {
      mMetaData.getColumnName(ic)
    }
	private def getResult(ic : Int) : Result = {
      val t = mMetaData.getColumnType(ic)
      t match {
        case java.sql.Types.ARRAY => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.BIGINT => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.BINARY => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.BIT => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.BLOB => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.BOOLEAN => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.CHAR => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" ) //StringResult(r.getString(ic))
		case java.sql.Types.CLOB => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.DATALINK => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.DATE => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.DECIMAL => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.DISTINCT => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.DOUBLE => DoubleResult( r.getDouble(ic) )
		case java.sql.Types.FLOAT => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.INTEGER => IntResult(r.getInt(ic))
		case java.sql.Types.JAVA_OBJECT => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.LONGNVARCHAR => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.LONGVARBINARY => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.LONGVARCHAR => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.NCHAR => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.NCLOB => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.NULL => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.NUMERIC => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.NVARCHAR => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.OTHER => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.REAL => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.REF => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.ROWID => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.SMALLINT => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.SQLXML => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.STRUCT => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.TIME => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.TIMESTAMP => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.TINYINT => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.VARBINARY => new StringResult( mMetaData.getColumnClassName(ic) + " " + t + " not supported" )
		case java.sql.Types.VARCHAR => StringResult(r.getString(ic))
		case _ => StringResult("unexpected type:" + t)
      } 
	}
	
	def next() : Row = {
			val ret = (1 to mColCount).map( x => getKey(x) -> getResult(x) )
			hasNext = r.next
			ret.toMap
	}

}

object ScalaQLite extends App {
 
  val db = """C:\Users\Karl\Documents\GitHub\BarKeepUtil\Data\db\barkeep__20140216"""
  val dbDetails = SQLiteHelper.getDBDetails(db)
  SQLiteHelper.printDBInfo(dbDetails)
}


object SQLiteHelper {
  
  final val MASTER_SQL = """Select * from sqlite_master"""
  Class.forName("org.sqlite.JDBC")
  
  type DBDetails = (IndexedSeq[String], ResultSet, Connection)  

  
  def getDBDetails( dbFile : String ) : DBDetails = {
    
	  val connection = DriverManager.getConnection("jdbc:sqlite:"+dbFile)
      val stmt = connection.createStatement
      val res= stmt.executeQuery(MASTER_SQL)
  
      val md = res.getMetaData()
      val cols = for(i <- 1 to md.getColumnCount())
    	  yield md.getColumnName(i)
  
      (cols, res, connection)
  }
  
  def getTableDetails( tableName : String, conn : Connection ) = {
    val stmt = conn.createStatement
    val res= stmt.executeQuery(MASTER_SQL)
  
  }
  
  def printDBInfo( details : DBDetails )  : Unit =
    printDBInfo(details._1, details._2)
    
  class MaxLenString( str : String ){
  	def get( len : Int ) = if(str.length > len) { 
  	  val substr = str.substring(0, len-3) + "..."
  	  substr
  	  } else str
  }
  
  implicit def toMLS( str : String ) = new MaxLenString(str)  
  
  def printDBInfo( header : IndexedSeq[String],  results : ResultSet ) : Unit = {
    println( header.mkString("", "\t\t", "") )
    results.foreach( r => {
      val values = header.map( h => r(h) match {
        case StringResult(sr) => sr.get(20)
        case _ => r(h)
      }) 
      println(values.mkString("","\t\t",""))
    })
  }
  
  def getResultSetInfo( res : ResultSet ) = {
      
      val md = res.getMetaData()
      val cols = for(i <- 1 to md.getColumnCount())
    	  yield md.getColumnName(i)
    	  
     cols
  }
  
  def printResultSetInfo( res : ResultSet ) =
    getResultSetInfo(res).foreach(println)
  
}
