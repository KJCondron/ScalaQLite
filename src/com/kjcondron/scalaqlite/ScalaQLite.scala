
package com.kjcondron.scalaqlite

import java.sql.DriverManager
import java.sql.ResultSet
import types._

// TODO - separate file
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

import com.kjcondron.scalaqlite.ResultSetIterator._
import scala.language.implicitConversions
import scala.swing.SimpleSwingApplication
import scala.swing.MainFrame
import java.awt.Dimension
import scala.swing.Component
import javax.swing.JTree
import javax.swing.tree.MutableTreeNode
import javax.swing.tree.DefaultMutableTreeNode
import scala.swing.ScrollPane
import scala.swing.BoxPanel
import scala.swing._
import Swing._
import javax.swing.event.TreeWillExpandListener
import javax.swing.event.TreeExpansionEvent
import javax.swing.event.TreeExpansionListener
import java.util.Enumeration
import javax.swing.tree.TreePath
import javax.swing.text.Position
import java.sql.Connection
import javax.swing.JComponent
import javax.swing.event.TreeSelectionListener
import javax.swing.event.TreeSelectionEvent

sealed trait Result {
	def toString : String
}

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
  type Row = Array[Result] 
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
  res.foreach( r => println(r.mkString(":")))
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
  /*upcRows.foreach( r => {
    println( r.mkString("PRODROW:", ",", "") + ":" + HttpTest.getResult( r("upc").toString ).mkString("HTTPRES:", ",", "" ))
    Thread.sleep(1000)
  })*/
  
  HttpTest.shutdown
  
// val upcs = upcRows.map(_("upc"))
// upcs.foreach(println)
  
//  stmt.executeUpdate(DELTE_BARS_SQL)
//  stmt.executeUpdate(DELTE_INV_SQL)
  stmt.close()
  connection.close()
 //val https = upcs.map( x=> HttpTest.getResult( x(6).toString ) )
}


object ScalaQLite extends App {
 
  val db = """C:\Users\Karl\Documents\GitHub\BarKeepUtil\Data\db\barkeep__20140216"""
  val dbDetails = SQLiteHelper.getDBDetails(db)
  SQLiteHelper.printDBInfo(dbDetails)
}


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
    val stmt = conn.createStatement
    val res = stmt.executeQuery(TABLE_SQL(tableName))
    val md = res.getMetaData()
    
    val header = getColNames(res)
    val data = res.map(_.toArray).toArray
    
    (data, header)
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

class WillExpandLister( 
    expandFunc : TreeExpansionEvent => Unit,
    collapseFunc : TreeExpansionEvent => Unit )
    extends TreeWillExpandListener {
  
  def this( expandFunc : TreeExpansionEvent => Unit ) = this( expandFunc, _=>Unit )
  
  def treeWillCollapse(event : TreeExpansionEvent) : Unit =
    collapseFunc(event)
  def treeWillExpand(event : TreeExpansionEvent) : Unit =
    expandFunc(event)
  
}

class TreeExpandedLister( 
    expandFunc : TreeExpansionEvent => Unit,
    collapseFunc : TreeExpansionEvent => Unit )
    extends TreeExpansionListener {
  
  def treeCollapsed(event : TreeExpansionEvent) : Unit =
    collapseFunc(event)
  def treeExpanded(event : TreeExpansionEvent) : Unit =
    expandFunc(event)
  
}

class SelectionListener( selectionFunc : TreeSelectionEvent => Unit)
	extends TreeSelectionListener {
  def valueChanged(evt : TreeSelectionEvent) = selectionFunc(evt)
  
}

class TreeWrapper( tree: JTree ) extends Component {
  override lazy val peer = tree
}

class DBViewer( val dbLoc : String ) extends Component {
  
  val (_,_,conn) = SQLiteHelper.getDBDetails(dbLoc)
  
  def getTopNode = {
    val headings = List("Tables", "Views")
    val topNode = new DefaultMutableTreeNode(dbLoc)
    
    val tableNode = new DefaultMutableTreeNode("TABLES")
    tableNode.add( new DefaultMutableTreeNode("dummy list"))
    topNode.add(tableNode)
    
    val viewNode = new DefaultMutableTreeNode("VIEWS")
    viewNode.add( new DefaultMutableTreeNode("dummy list"))
    topNode.add(viewNode)
    
    topNode
    
  }
    
  override lazy val peer : JComponent = {
    
    val rows : Array[Array[Any]] = Array(
      Array('r1c1, 'r1c2, 'r1c3),
      Array('r2c1, 'r2c2, 'r2c3),
      Array('r3c1, 'r3c2, 'r3c3),
      Array('r4c1, 'r4c2, 'r4c3),
      Array('r5c1, 'r5c2, 'r5c3))
      
  val header = List('c1, 'c2, 'c3)
  
  var table = new Table(rows, header)
  val tree = new JTree(getTopNode)
  
  val tableSplit = new SplitPane(Orientation.Horizontal, new ScrollPane(table), new ScrollPane()) 
  val mainSplit = new SplitPane(
        Orientation.Vertical,
        new ScrollPane( new TreeWrapper(tree) ),
        tableSplit )
    
    val tablePath = tree.getNextMatch("TABLES", 0, Position.Bias.Forward)
    val viewPath = tree.getNextMatch("VIEWS", 0, Position.Bias.Forward)
    
    
    // TODO make it so this is not called more than once
    def tableEvent(evt : TreeExpansionEvent) = {
      if( evt.getPath == tablePath ) {
    	  val tblNode = evt.getPath.getLastPathComponent().asInstanceOf[DefaultMutableTreeNode]
    	  tblNode.removeAllChildren
    	  SQLiteHelper.getTableNames(conn).foreach(t=>tblNode.add(new DefaultMutableTreeNode(t)))
      }
    }
    
    // TODO make it so this is not called more than once
    def viewEvent(evt : TreeExpansionEvent) = {
      if( evt.getPath == viewPath ) {
    	  val viewNode = evt.getPath.getLastPathComponent().asInstanceOf[DefaultMutableTreeNode]
    	  viewNode.removeAllChildren
    	  SQLiteHelper.getViewNames(conn).foreach(t=>viewNode.add(new DefaultMutableTreeNode(t)))
      }
    }
    
    def viewSelectedEvent(evt : TreeSelectionEvent) = {
      if( evt.getPath.getParentPath == viewPath ||
          evt.getPath.getParentPath == tablePath ) {
    	  val name = evt.getPath.getLastPathComponent().toString()
    	  val (data , header) = SQLiteHelper.getTableContents(name,conn)
    		val anyData = data.asInstanceOf[Array[Array[Any]]]
    	    table = new Table(anyData,header)
    	    tableSplit.topComponent_=( new ScrollPane(table) )
      }
    }
    
    tree.addTreeWillExpandListener(new WillExpandLister(
        tableEvent _))
        
    tree.addTreeWillExpandListener(new WillExpandLister(
        viewEvent _))
        
    tree.addTreeSelectionListener(new SelectionListener(
        viewSelectedEvent _))
      
    mainSplit.peer
  }
}

object SwingApp extends SimpleSwingApplication {
  
  val db = """C:\Users\Karl\Documents\GitHub\BarKeepUtil\Data\db\barkeep__20140216"""
  
  def top = new MainFrame {
    title = "MyApp"
    preferredSize = new Dimension(600,600) 
    contents = new DBViewer(db)
  }  
}
