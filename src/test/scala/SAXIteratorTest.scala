package saxiterator

import java.io.InputStream
import org.xml.sax.Attributes
import org.scalatest._

class SAXIteratorTest extends FunSpec with ShouldMatchers {

  it("passes fatal errors during parsing") {
    val input: InputStream = getClass.getResourceAsStream("/fatalError.xml")
    val target = new SAXIterator(input)

    target.hasNext shouldBe true
    target.next match {
      case DocumentStart ⇒
    }
    target.next match {
      case ParseFatalError(ex) ⇒
        ex.getMessage shouldBe "XML document structures must start and end within the same entity."
    }
  }

  it("passes basic xml parsing events") {
    val input: InputStream = getClass.getResourceAsStream("/note.xml")
    val target = new SAXIterator(input)

    target.hasNext shouldBe true

    val firstEvent = target.next
    firstEvent match {
      case DocumentStart ⇒
      case _             ⇒ fail
    }

    val secondEvent = target.next
    secondEvent match {
      case ElementStart(uri, localName, qName, attributes) ⇒
        uri shouldBe ""
        localName shouldBe ""
        qName shouldBe "note"
        attributes.getLength shouldBe 0
    }

    val thirdEvent = target.next
    thirdEvent match {
      case Characters(content) ⇒
        content shouldBe """
  """
    }

    val fourthEvent = target.next
    fourthEvent match {
      case ElementStart(uri, localName, qName, attributes) ⇒
        uri shouldBe ""
        localName shouldBe ""
        qName shouldBe "to"
        attributes.getLength shouldBe 0
    }

    val fifthEvent = target.next
    fifthEvent match {
      case Characters(content) ⇒
        content shouldBe "Tove"
    }

    val sixthEvent = target.next
    sixthEvent match {
      case ElementEnd(uri, localName, qName) ⇒
        uri shouldBe ""
        localName shouldBe ""
        qName shouldBe "to"
    }

    target.foreach(identity)
  }

}
