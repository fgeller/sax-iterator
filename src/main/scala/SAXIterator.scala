package saxiterator

import scala.util._
import scala.concurrent._
import javax.xml.parsers.SAXParserFactory
import java.io.InputStream
import scala.concurrent.duration._
import javax.xml.parsers.SAXParser
import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.Attributes
import org.xml.sax.SAXParseException

trait SAXEvent

case class Characters(content: String) extends SAXEvent
case class ElementStart(uri: String, localName: String, qName: String, attributes: Attributes) extends SAXEvent
case class ElementEnd(uri: String, localName: String, qName: String) extends SAXEvent
case object DocumentStart extends SAXEvent
case object DocumentEnd extends SAXEvent
case class ParseError(ex: SAXParseException) extends SAXEvent
case class ParseFatalError(ex: SAXParseException) extends SAXEvent
case class ParseWarning(ex: SAXParseException) extends SAXEvent

case class PrefixMappingStart(prefix: String) extends SAXEvent
case class PrefixMappingEnd(prefix: String) extends SAXEvent
case class NotationDeclaration(name: String, publicId: String, systemId: String) extends SAXEvent
case class IgnorableWhitespace(content: String) extends SAXEvent
case class ProcessingInstruction(target: String, data: String) extends SAXEvent
case class SkippedEntity(name: String) extends SAXEvent
case class UnparsedEntityDeclaration(name: String, publicId: String, systemId: String, notationName: String) extends SAXEvent

class Handler(outside: SAXEvent ⇒ Future[Unit]) extends DefaultHandler {
  val timeoutLimit = (290 * 365).days

  private def passEvent(event: SAXEvent): Unit = {
    Await.ready(outside(event), timeoutLimit)
  }

  override def characters(chars: Array[Char], start: Int, length: Int): Unit = {
    passEvent(Characters(new String(chars, start, length)))
  }

  override def startElement(uri: String, localName: String, qName: String, attributes: Attributes): Unit = {
    passEvent(ElementStart(uri, localName, qName, attributes))
  }

  override def endElement(uri: String, localName: String, qName: String): Unit = {
    passEvent(ElementEnd(uri, localName, qName))
  }

  override def startDocument(): Unit = {
    passEvent(DocumentStart)
  }

  override def endDocument(): Unit = {
    passEvent(DocumentEnd)
  }

  override def error(ex: SAXParseException): Unit = {
    passEvent(ParseError(ex))
  }

  override def fatalError(ex: SAXParseException): Unit = {
    passEvent(ParseFatalError(ex))
  }

  override def warning(ex: SAXParseException): Unit = {
    passEvent(ParseWarning(ex))
  }
}

class SAXIterator(input: InputStream) extends Iterator[SAXEvent] {

  val parser: SAXParser = SAXParserFactory.newInstance.newSAXParser()
  var depleted = false
  var started = false
  var promisedEvent = Promise[SAXEvent]
  var lock = Promise[Unit]

  def log[A](message: String, fn: () ⇒ A): A = {
    val startTime = System.nanoTime
    val result = fn()
    val endTime = System.nanoTime
    println(s"$message (${(endTime - startTime) / 1000.0 / 1000.0}ms)")
    result
  }

  def receiveEvent(event: SAXEvent): Future[Unit] = {
    println(s"received event [${event.getClass.getSimpleName}]")
    promisedEvent.success(event)
    if (event == DocumentEnd) {
      depleted = true
    }
    lock.future
  }

  def startParsing(): Future[Unit] = {
    started = true
    val handler = new Handler(receiveEvent)
    import ExecutionContext.Implicits.global
    Future { parser.parse(input, handler) }
  }

  def cycleLock(): Unit = {
    lock.complete(Try(()))
    lock = Promise[Unit]
  }

  def awaitNextEvent(): SAXEvent = {
    val timeoutLimit = 3.seconds
    val nextEvent = Await.result(promisedEvent.future, timeoutLimit)
    promisedEvent = Promise[SAXEvent]
    nextEvent
  }

  override def hasNext: Boolean = {
    !depleted
  }

  override def next(): SAXEvent = {
    if (depleted) {
      throw new NoSuchElementException("next on empty iterator")
    }

    if (!started) {
      log("start parsing", startParsing)
    } else {
      log("cycling lock", cycleLock)
    }

    log("awaiting next event", awaitNextEvent)
  }
}
