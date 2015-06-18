package saxiterator

import java.io.InputStream
import org.scalatest._

class SAXIteratorTest extends FunSpec {

  it("sax iterator") {
    val input: InputStream = getClass.getResourceAsStream("/note.xml")
    val target = new SAXIterator(input)

    target.foreach(println)
  }

}
