
package com.kjcondron.scalaqlite

import java.sql.DriverManager
import java.sql.ResultSet
import types._
import com.kjcondron.scalaqlite.ResultSetIterator._

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
  type Row = List[Result] 
}

object ScalaQLiteApp extends App {
  
  final val MASTER_SQL = """Select * from sqlite_master"""
  final val TABLES_SQL = """Select type,tbl_name from sqlite_master"""
  final val UPC_SQL = """Select * from vProducts where ean = 1111"""  
  final val BAR_SQL = """select * from vInventory where bar_id=1"""
  
  Class.forName("org.sqlite.JDBC")
    
  val connection = DriverManager.getConnection(
      """jdbc:sqlite:C:\Users\Karl\Documents\GitHub\BarKeepUtil\Data\db\barkeep__20140216""")
  
  val stmt = connection.createStatement
  
  val res= stmt.executeQuery(MASTER_SQL)
  
  val md = res.getMetaData()
  val cols = for(i <- 1 to md.getColumnCount())
    yield md.getColumnName(i)
    
  cols.foreach(println)
  res.foreach( r => println(r.take(2).mkString(":")))
  
  val tables = stmt.executeQuery(TABLES_SQL)
    
  val tl = tables.toList
  //tl.foreach( t =>  println(t.mkString(":")) )
 
  val upcres = stmt.executeQuery(BAR_SQL);
  val upcs = upcres.take(10).toList
  upcs.foreach( r=>println(r.mkString("PRODROW:", ",", "")))
  
 val https = upcs.map( x=> HttpTest.getResult( x(6).toString ) )
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
			val ret = (1 to mColCount).map( x => getResult(x) ).toList
			hasNext = r.next
			ret
	}

}



object MasterInfo {
  
  final val MASTER_SQL = """Select * from sqlite_master"""
  Class.forName("org.sqlite.JDBC")
  
  def getDBDetails( dbFile : String ) = {
    
	  val connection = DriverManager.getConnection(dbFile)
      val stmt = connection.createStatement
      val res= stmt.executeQuery(MASTER_SQL)
  
      val md = res.getMetaData()
      val cols = for(i <- 1 to md.getColumnCount())
    	  yield md.getColumnName(i)
  
      cols
  }    
}