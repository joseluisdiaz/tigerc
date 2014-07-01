package tiger

import java.io.ByteArrayInputStream
import java.io.File
import java.io.FileInputStream
import tiger.Frame.{PROC,STRING}
import tiger.parser.{TigerScanner, TigerParser}
import sext._

object Tiger {

  object ComponentRegistry extends EnvComponent with SemanComponent with EscapesComponent with TranslateComponent with CanonComponent {
    override val escapes = new MyEscapes()
    override val translate = new MyTranslate()
    override val env = new Env()
    override val seman:Seman = new SemanImp()
    override val canon = new MyCanon()
  }

  def main(args: Array[String]) {

    val stream = if (args.length == 1) {
      new FileInputStream(new File(args(0)))
    } else {
      System.in
    }

    val cp = new TigerParser()
    val prog = cp.parse(stream)

    ComponentRegistry.escapes.findEscapes(prog)

    ComponentRegistry.seman.transProg(prog)

    println("==============\n\n")

    ComponentRegistry.translate.fragments().foreach { frag => println(frag.treeString) }

    ComponentRegistry.translate.fragments().foreach { frag =>

      frag match {
        case PROC(stm, frame) => {
          println(s"frame:$frame")
          val c = ComponentRegistry.canon.linearize(stm)
          c.foreach( x => { println("->" + x) })
        }

        case STRING(l,s) => println(s"$l: $s\n")
      }
    }
  }

}
