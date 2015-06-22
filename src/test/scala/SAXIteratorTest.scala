package saxiterator

import java.io.InputStream
import org.xml.sax.Attributes
import org.scalatest._

class SAXIteratorTest extends FunSpec with ShouldMatchers {

  it("allows deserializing nested structures") {
    val input: InputStream = getClass.getResourceAsStream("/person.xml")
    val target = new SAXIterator(input)

    case class Address(
      line1:    Option[String] = None,
      line2:    Option[String] = None,
      city:     Option[String] = None,
      postCode: Option[String] = None,
      country:  Option[String] = None
    )

    case class Person(
      firstName: Option[String]  = None,
      lastName:  Option[String]  = None,
      address:   Option[Address] = None
    )

    var person = Option.empty[Person]
    var textContent = Option.empty[String]
    var readAddress = Option.empty[Address]
    while (target.hasNext) {
      target.next match {
        case ElementStart(_, _, "Person", _) ⇒
          person = Some(Person())
          textContent = Option.empty[String]
        case ElementStart(_, _, "Address", _) ⇒
          readAddress = Some(Address())
          textContent = Option.empty[String]
        case ElementStart(_, _, _, _) ⇒
          textContent = Option.empty[String]
        case ElementEnd(_, _, "FirstName") ⇒
          person = person.map(p ⇒ p.copy(firstName = textContent))
        case ElementEnd(_, _, "LastName") ⇒
          person = person.map(p ⇒ p.copy(lastName = textContent))
        case ElementEnd(_, _, "Address") ⇒
          person = person.map(p ⇒ p.copy(address = readAddress))
          readAddress = Option.empty[Address]
        case ElementEnd(_, _, "Line1") ⇒
          readAddress = readAddress.map(a ⇒ a.copy(line1 = textContent))
        case ElementEnd(_, _, "Line2") ⇒
          readAddress = readAddress.map(a ⇒ a.copy(line2 = textContent))
        case ElementEnd(_, _, "City") ⇒
          readAddress = readAddress.map(a ⇒ a.copy(city = textContent))
        case ElementEnd(_, _, "PostCode") ⇒
          readAddress = readAddress.map(a ⇒ a.copy(postCode = textContent))
        case ElementEnd(_, _, "Country") ⇒
          readAddress = readAddress.map(a ⇒ a.copy(country = textContent))
        case Characters(content) ⇒
          textContent = Option(textContent.getOrElse("") + content)
        case _ ⇒
      }
    }

    val expected = Some(
      Person(
        firstName = Some("Hans"),
        lastName = Some("Schmitt"),
        address = Some(
          Address(
            line1 = Some("42 Main Street"),
            line2 = Some("Second floor"),
            city = Some("Timbuktu"),
            postCode = Some("33452"),
            country = Some("Algeria")
          )
        )
      )
    )

    person shouldBe expected
  }

  ignore("passes fatal errors during parsing") {
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

  // it("passes notation declarations during parsing") {}
  // it("passes ignorable whitespace during parsing") {}
  // it("passes processing instructions during parsing") {}
  // it("passes skipped entities during parsing") {}
  // it("passes unparsed entity declarations during parsing") {}

  ignore("passes prefix mappings during parsing") {
    val input: InputStream = getClass.getResourceAsStream("/prefixMapping.xml")
    val target = new SAXIterator(input)

    target.hasNext shouldBe true
    target.next match {
      case DocumentStart ⇒
    }

    target.next match {
      case PrefixMappingStart(prefix, uri) ⇒
        prefix shouldBe "test"
        uri shouldBe "http://example.com"
    }

    target.next match {
      case ElementStart(uri, localName, qName, attributes) ⇒
        qName shouldBe "html"
    }

    target.next match {
      case PrefixMappingEnd(prefix) ⇒
        prefix shouldBe "test"
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
