package tiger

import java.io.ByteArrayInputStream
import java.io.File
import java.io.FileInputStream
import tiger.Frame.{PROC,STRING}
import tiger.Tree.Stm
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

    println(prog.treeString)

    ComponentRegistry.escapes.findEscapes(prog)

    ComponentRegistry.seman.transProg(prog)

    println("======= Intermediate code ==========\n\n")

    ComponentRegistry.translate.fragments().foreach {
      case PROC(stm, frame) => println(stm.treeString)

      case f@STRING(l, s) => println(s"$l: $s\n")

    }

    println("======= Canonized intermediate code ==========\n\n")

    var strings:List[(Temp.Label, String)] = List()
    var procs:List[(List[Stm], Frame)] = List()

    ComponentRegistry.translate.fragments().foreach {
      case PROC(stm, frame) => {
        println(s"frame:$frame")
        val c = ComponentRegistry.canon.linearize(stm)
        procs ::=(c, frame)

        c.foreach(x => {
          println("->" + x)
        })
      }

      case f@STRING(l, s) => {
        strings ::= (l, s)

        println(s"$l: $s\n")
      }
    }

    val p = procs map {
      case (asm,frame) => {
        val v = CodeGen(asm)
        println(frame.name)
        v foreach println
        (v, frame)
      }
    } map {
      case (asm, frame) => RegisterAllocation(asm, frame).get()
    } foreach {
      case (asm, frame) =>
        println(frame.name + ":")
        asm foreach { x => println(x.code) }
    }



//    val eval = new Interpeter(procs, strings)
//
//    println("===== eval ===== \n\n")
//
//    if (args.length >= 2 && args(2) == "debug")
//      eval.debug = true
//
//    eval.evalFunc("_tigermain", List())
//
//    println("\n\n==== end =====\n")

  }



}
