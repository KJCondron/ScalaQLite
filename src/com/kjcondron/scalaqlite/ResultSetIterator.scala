
package com.kjcondron.scalaqlite

import java.sql.ResultSet

package object types {
  type Row = Array[Result] 
}

import types.Row

sealed trait Result {
	def toString : String
	def getType : String 
}

final case class StringResult( string : String ) extends Result {
  override def toString = string
  def getType = "String"
}
final case class IntResult( int : Int ) extends Result {
  override def toString = int.toString
  def getType = "Int"
}
final case class DoubleResult( double : Double) extends Result {
  override def toString = double.toString
  def getType = "Double"
}

object ResultSetIterator {
	//implicit def apply( r : ResultSet ) : Iterator[Row] = if(r.isAfterLast) Iterator.empty else new ResultSetIterator(r)
	implicit def apply( r : ResultSet ) : ResultSetIterator = new ResultSetIterator(r) 
}

class ResultSetIterator( r : ResultSet ) extends Iterator[Row]
{
    if(r.isBeforeFirst()) r.next
	
    private val mMetaData = r.getMetaData
	private val mColCount = mMetaData.getColumnCount
	// TODO check these are the same
	private val mColNames = for(i <- 1 to mColCount) yield mMetaData.getColumnName(i)
	private val mColNames2 = (1 to mColCount).map(mMetaData.getColumnName)
	
	// r is non functional so it is not surprising we need a var to handle it
	var hasNext = !r.isAfterLast
	
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
	
	def next : Row = {
			if(!hasNext)
			  Iterator.empty.next // this throws the empty exception
			  
			val ret = (1 to mColCount).map(getResult)
			hasNext = r.next
			ret.toArray
	}
}
