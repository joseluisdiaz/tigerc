package tiger

import java.io.{ByteArrayInputStream}
import tiger.parser.TigerParser


/**
 * User: jose
 * Date: 9/25/13
 * Time: 7:52 PM
 */
object TigerTestUtil {

  trait TigerProgram {
    def tigerProgram(): Abs.Exp
  }

  class TigerAbs(val file:String) extends TigerProgram {
    private val resource = getClass.getResourceAsStream(file)
    private val cp = new TigerParser()
    def tigerProgram()  = cp.parse(resource)
  }

  class TigerAbsFromString(val text:String) extends TigerProgram {
    private val cp = new TigerParser()
    def tigerProgram()  = cp.parse(new ByteArrayInputStream(text.getBytes("UTF-8")))
  }



  trait TigerEscapes extends TigerProgram {
    abstract override def tigerProgram(): Abs.Exp = {
      val p = super.tigerProgram()
      Escapes.findEscapes(p)
      p
    }
  }
}
