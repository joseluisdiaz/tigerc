package tiger

import java.io.{ByteArrayInputStream}
import tiger.parser.TigerParser
import tiger.EscapesComponent

import scala.xml.Utility.Escapes


/**
 * User: jose
 * Date: 9/25/13
 * Time: 7:52 PM
 */
object TigerTestUtil {

  object ComponentRegistry extends EscapesComponent {
    override val escapes: Escapes = new MyEscapes()
  }

  trait TigerProgram {
    def tigerProgram(): Abs.Exp
  }

  class TigerAbs(val file:String) extends TigerProgram {
    private val resource = getClass.getResourceAsStream(file)
    private val cp = new TigerParser()
    def tigerProgram()  = cp.parse(resource)
  }


  object TigerAbs {
    def apply(file:String) = new TigerAbs(file)
  }



  class TigerAbsFromString(val text:String) extends TigerProgram {
    private val cp = new TigerParser()
    def tigerProgram()  = cp.parse(new ByteArrayInputStream(text.getBytes("UTF-8")))
  }


  trait TigerEscapes extends TigerProgram {
    abstract override def tigerProgram(): Abs.Exp = {
      val p = super.tigerProgram()
      ComponentRegistry.escapes.findEscapes(p)
      p
    }
  }


}
