package code.comet

import net.liftweb.actor.LiftActor
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.SHtml._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqJsCmds._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import code.model.M
import scala.xml.NodeSeq
import org.joda.time.DateTime
import code.model.ChatRoom
import code.snippet.ChatBox
import org.joda.time.format.DateTimeFormatter
import org.joda.time.format.DateTimeFormat
import net.liftweb.http.js.JE.AnonFunc
import net.liftweb.http.js.jquery.JqJE.Jq
import net.liftweb.http.js.JE.JsFunc
import net.liftweb.http.js.JE.JsVar

case class NewChatRoom(cr: ChatRoom)
case class ChatMessage(message: String, timestamp: DateTime)
//case class UserJoin(user: User)

class ChatServer(cr: ChatRoom) extends LiftActor with ListenerManager with Loggable {
  val chatRoom = cr
  
  logger.info("created chat server for "+cr.id)

  private var messages = List[ChatMessage]()
  
  def createUpdate = messages

  override def lowPriority = {
    case cm: ChatMessage => {
      messages ::= cm
      sendListenersMessage(cm)
    }
  }
}

class ChatUser extends CometActor with CometListener with Loggable {

  private var _chatServer: ChatServer = null
  
  def registerWith = _chatServer

  private var messages: List[ChatMessage] = Nil
  
  override def localSetup() = {
    _chatServer = name match {
      case Full(id) ⇒ ChatRoomManager.serve(id)
      case Empty    ⇒ throw new Exception("chat user has no name for chat room")
    }
    super.localSetup()

    logger.info("localSetup for "+name)
  }

  override def lowPriority = {
    case msg: ChatMessage ⇒ {
      logger.info("got chat msg of"+msg.message+" in "+name)
      messages ::= msg
      partialUpdate(AppendHtml(uniqueId+"-message-list", renderMessage(msg)))
    }
    
    case msgs: List[ChatMessage] ⇒ {
      messages = msgs
      partialUpdate(SetHtml(uniqueId+"-message-list", renderMessages()))
    }
  }
  
  def renderMessages() = messages.reverse.map(renderMessage(_))

  def renderMessage(msg: ChatMessage) =
    <li>
      <span>{ msg.timestamp.toString(DateTimeFormat.shortTime()) }</span>
      &nbsp;
      <span float="right">{ msg.message }</span>
    </li>

  
  def render = {
    var message = ""
    // README: the below Jq(".chat-message-form :submit") needs to target a this specific form for this comet
    // this also actually overrides the ajax submit so I don't get the chat message submitted to the server 
    //S.appendJs((Jq(".chat-message-form :submit") ~> JsFunc("on", "click", AnonFunc("event", (JsVar("event") ~> JsFunc("preventDefault")).cmd & SetValueAndFocus(uniqueId+"-message-input", "")))).cmd)
    ".chat-message-form [id]" #> (uniqueId + "-chat-message-form") & // README: This does not apply an id to the form (can't modify/override lifts GUID?) 
    ".messages *" #> <ul id={ uniqueId + "-message-list" }> { renderMessages() } </ul> &
    "@message" #> SHtml.text(message, str ⇒ message = str, "id" -> (uniqueId + "-message-input")) &
    "@send-message" #> SHtml.ajaxSubmit("Send", () ⇒ {
      if (message.nonEmpty) {
        _chatServer ! ChatMessage(message, DateTime.now)
      }
      // the below is here because it's the only way that I can get the effect there should be so far
      // SetValueAndFocus(uniqueId + "-message-input", "") & // README: should be executed before ajax call is made on submit
      Noop 
    })
  }
}

case class InChat(id: String)
class ChatControls extends CometActor with CometListener with Loggable {
  
  var openChats = scala.collection.mutable.Set[String]()
 
  def render = {
    var chatName = ""
    "#chat-list li *" #> ChatRoomManager.chatServers.map(cs => openChatButton(cs.chatRoom)) &
    "#chat-name" #> text("", chatName = _) &
    "#make-chat-room" #> ajaxSubmit("make chat", () => {
      val chatId = StringHelpers.clean(chatName)
      val cr = ChatRoomManager.createChatRoom(chatId, chatName)
      openChats += chatId
      AppendHtml("open-chat-rooms", ChatBox(chatId)) &
      Focus(chatId + "-message-input")
    })
  }
  
  def newOpenChatButton(cr: ChatRoom) = AppendHtml("chat-list", <li class="chat-room">{openChatButton(cr)}</li>)
  
  def openChatButton(cr: ChatRoom) = ajaxButton("Open "+cr.name, () => {
    if (openChats.contains(cr.id)) {
      Focus(cr.id + "-message-input")
    } else {
      openChats += cr.id
      AppendHtml("open-chat-rooms", ChatBox(cr.id))
    }
  })
  
  def registerWith = ChatRoomManager
  
  override def lowPriority = {
    case m: scala.collection.mutable.Map[String, ChatServer] => logger.info("got first update")
    
    case NewChatRoom(cr) => partialUpdate(newOpenChatButton(cr))
    // should only be sent when first inited to make sure that openChats is set correctly
    case InChat(id) => {
      logger.info("adding "+id+" as our initial chat")
      openChats += id
    }
  }
  
}

object ChatRoomManager extends LiftActor with ListenerManager with Logger {
  private var _chatServers = scala.collection.mutable.Map[String, ChatServer]()
  
  def chatServers() = synchronized { _chatServers.values }
  
  def createUpdate = _chatServers
  
  def createChatRoom(id: String, name: String) = synchronized {
    info("added new ChatServer for "+id)

    val cr = ChatRoom(id, name)
    sendListenersMessage(NewChatRoom(cr))
    cr
  }

  def serve(id: String): ChatServer = synchronized {
    _chatServers.get(id) match {
      case Some(server) => server
      case None ⇒ {
        val cr = createChatRoom(id, id)
        val newServer = new ChatServer(cr)
        _chatServers.put(id, newServer)
        newServer
      }
    }
  }
}
