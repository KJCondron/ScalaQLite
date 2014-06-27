package com.kjcondron.scalaqlite

import java.sql.DriverManager
import java.sql.ResultSet
import java.sql.Connection
import ResultSetIterator._

object SQLiteHelper {
  
  final val MASTER_SQL = """Select * from sqlite_master"""
  final val TABLES_SQL = """Select * from sqlite_master where type='table'"""
  final val VIEWS_SQL = """Select * from sqlite_master where type='view'"""
    
  private def TABLE_SQL(tableName : String) = "Select * from " + tableName
  
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
  
  def getColNames( res : ResultSet ) = {
    val md = res.getMetaData
    (1 to md.getColumnCount).map(md.getColumnName)
  }
  
  def getTableContents( tableName : String, conn : Connection ) = {
    
    val res = getTableRS(tableName, conn)
    val md = res.getMetaData()
    
    val header = getColNames(res)
    val data = res.map(_.toArray).toArray
    
    (data, header)
  }
  
  def getTableRS( tableName : String, conn : Connection ) = {
    val stmt = conn.createStatement
    stmt.executeQuery(TABLE_SQL(tableName))
  }
  
  
  private def getNames( conn : Connection, sql : String ) = {
    val stmt = conn.createStatement
    val res = stmt.executeQuery(sql)
    val header = getColNames(res)
    res.map( x => { val mp = (header zip x).toMap
      		mp("name")
    		})
    }
  
  def getTableNames( conn : Connection ) =
    getNames(conn, TABLES_SQL)
  
  def getViewNames( conn : Connection ) =
    getNames(conn, VIEWS_SQL)
  
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
      val values = header.map( h => { 
        val vs = (header zip r).toMap
        val v = vs(h)
        v match {
        	case StringResult(sr) => sr.get(20)
        	case _ => v
        }
      })
      println(values.mkString("","\t\t",""))
    })
  }
  
  def printResultSetInfo( res : ResultSet ) =
    getColNames(res).foreach(println)
  
}