package tiger

import java.io.ByteArrayInputStream

import org.scalatest.Matchers
import tiger.TigerTestUtil.ComponentRegistry._
import tiger.Types.Ty
import tiger.parser.TigerParser

/**
 * User: jose
 * Date: 9/25/13
 * Time: 7:52 PM
 */

object TigerTestUtil {

  trait TypesToFromFile extends Matchers {

    def tigerProg(s: String) = {
      seman.transProg(new TigerAbs(s) with TigerEscapes tigerProgram()).ty
    }

    def typeTo(x: Ty) = be(x) compose tigerProg

  }

  trait TypesToFromString extends Matchers {

    def tigerProgFromString(s: String) = {
      seman.transProg(new TigerAbsFromString(s) with TigerEscapes tigerProgram()).ty
    }

    def typeTo(ty: Ty) = be(ty) compose tigerProgFromString

  }


  object ComponentRegistry extends EnvComponent with SemanComponent with EscapesComponent with TranslateComponent {
    override val escapes = new MyEscapes()
    override val translate = new MyTranslate()
    override val env = new Env()
    override val seman:Seman = new SemanImp()
  }


  trait TigerProgram {
    def tigerProgram(): Abs.Exp
  }

  class TigerAbs(val file:String) extends TigerProgram {
    private val resource = getClass.getResourceAsStream(file)
    if (resource == null) {
      throw new RuntimeException(s"Cant read file $file")
    }
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
