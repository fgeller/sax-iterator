package saxiterator

import scala.util._
import scala.concurrent._
import javax.xml.parsers.SAXParserFactory
import java.io.InputStream
import scala.concurrent.duration._
import javax.xml.parsers.SAXParser
import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.Attributes

trait SAXElement
case class ElementStart(uri: String, localName: String, qName: String, attributes: Attributes) extends SAXElement
case class ElementEnd(uri: String, localName: String, qName: String) extends SAXElement
case object DocumentEnd extends SAXElement

class Handler(outside: SAXElement ⇒ Future[Unit]) extends DefaultHandler {
  val timeoutLimit = (290 * 365).days

  override def startElement(uri: String, localName: String, qName: String, attributes: Attributes): Unit = {
    Await.ready(
      outside(ElementStart(uri, localName, qName, attributes)),
      timeoutLimit
    )
  }

  override def endDocument(): Unit = {
    outside(DocumentEnd)
  }
}

class SAXIterator(input: InputStream) extends Iterator[SAXElement] {

  val parser: SAXParser = SAXParserFactory.newInstance.newSAXParser()
  var depleted = false
  var started = false
  var nextElement = Promise[SAXElement]
  var lock = Promise[Unit]

  def extract(element: SAXElement): Future[Unit] = {
    nextElement.complete(Success(element))
    if (element == DocumentEnd) {
      depleted = true
    }
    lock.future
  }

  override def hasNext: Boolean = !depleted
  override def next(): SAXElement = {
    if (depleted) {
      throw new IllegalStateException() //TODO
    }

    if (!started) {
      started = true
      val handler = new Handler(extract)
      import ExecutionContext.Implicits.global
      Future { parser.parse(input, handler) }
    } else {
      lock.complete(Try(()))
      lock = Promise[Unit]
    }

    val timeoutLimit = 3.seconds
    val nextEl = Await.result(nextElement.future, timeoutLimit)
    nextElement = Promise[SAXElement]
    nextEl
  }
}
