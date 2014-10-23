
package com.kjcondron.scalaqlite

import java.awt.Dimension
import java.sql.Connection
import java.sql.DriverManager
import java.sql.ResultSet
import scala.language.implicitConversions
import scala.swing.Component
import scala.swing.MainFrame
import scala.swing.Orientation
import scala.swing.ScrollPane
import scala.swing.SimpleSwingApplication
import scala.swing.SplitPane
import scala.swing.Table
import ResultSetIterator.apply
import javax.swing.JTree
import javax.swing.event.TreeExpansionEvent
import javax.swing.event.TreeExpansionListener
import javax.swing.event.TreeSelectionEvent
import javax.swing.event.TreeSelectionListener
import javax.swing.event.TreeWillExpandListener
import javax.swing.text.Position
import javax.swing.tree.DefaultMutableTreeNode
import javax.swing.JComponent
import javax.swing.plaf.nimbus.TextPanePainter
import scala.swing.EditorPane
import scala.swing.TextComponent
import scala.swing.Button
import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position._
import javax.swing.JToolBar
import scala.swing.Publisher
import scala.swing.Reactions
import scala.swing.event.ButtonClicked
import scala.swing.Dialog

object ScalaQLite extends App {
 
  val db = """C:\Users\Karl\Documents\GitHub\BarKeepUtil\Data\db\barkeep__20140216"""
  val dbDetails = SQLiteHelper.getDBDetails(db)
  SQLiteHelper.printDBInfo(dbDetails)
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

class SQLToolBar {
  
  def +=( b : Button) = _content.add(b.peer)
  
  val _content = new JToolBar("SQL Editor")
  
  def content = new SQLToolBarWrapper(_content)
}

class SQLToolBarWrapper( toolbar : JToolBar) extends Component {
  override lazy val peer = toolbar
}

class SQLEditor( conn : Connection ) extends Publisher {
  
  final val NL = String.format("%n")
  final val REC_STR = " record(s) retreived"
  
  val toolbar = new SQLToolBar
  
  val execute = new Button {
    text = "Execute"
  }
  
  val clear= new Button {
    text = "Clear"
  }
  
  toolbar += execute
  toolbar += clear
  
  listenTo(execute)
  listenTo(clear)
   
  val editor = new EditorPane
  
  val border = new BorderPanel {
    layout(toolbar.content) = North
    layout(editor) = Center
  }
  
  def content = new ScrollPane( border )
  
  def getLastEntry = {
    val lines = editor.text.split(NL)
    val line = lines.reverse.find(!_.contains(REC_STR))
    line.getOrElse("")
  }
}

class DBViewer( val dbLoc : String, val conn : Connection ) extends Component {
  
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
    
  val entry = new SQLEditor(conn)
     
  val tableSplit = new SplitPane(Orientation.Horizontal, new ScrollPane(table), entry.content) 
  
  entry.reactions += {
    case ButtonClicked(c) if c == entry.execute => {
      val qry = entry.editor.selected
      entry.editor.text += entry.NL
      val res = SQLiteHelper.executeQuery(conn, if(qry==null)entry.getLastEntry else qry)
      res match {
        case Left(msg) => entry.editor.text += msg
        case Right(rs) => {
          val (data , header) = SQLiteHelper.getRSContents(rs)
           val anyData = data.asInstanceOf[Array[Array[Any]]]
    	    table = new Table(anyData,header)
    	    tableSplit.topComponent_=( new ScrollPane(table) )
    	    entry.editor.text += data.size + entry.REC_STR
        }
      }
      Dialog.showMessage(null, "execute")
    }
    case ButtonClicked(c) if c == entry.clear => { 
    	entry.editor.text = ""
    }
  }
    
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
  
  //val db = """C:\Users\Karl\Documents\GitHub\BarKeepUtil\Data\db\barkeep__20140216"""
  
  val db = """C:\Users\Karl\Documents\GitHub\BarKeep\assets\databases\barkeep_db"""
    
  val (_,_,conn) = SQLiteHelper.getDBDetails(db)
    
  def top = new MainFrame {
    title = "MyApp"
    preferredSize = new Dimension(600,600) 
    contents = new DBViewer(db,conn)
  }
}
