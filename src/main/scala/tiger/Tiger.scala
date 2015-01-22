package tiger

import java.io.File
import java.io.FileInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import com.gilt.handlebars.scala.Handlebars
import com.gilt.handlebars.scala.helper.Helper
import tiger.Frame.{PROC,STRING}
import tiger.Tree.Stm
import tiger.parser.{TigerScanner, TigerParser}
import sext._

import com.gilt.handlebars.scala.binding.dynamic._
import com.gilt.handlebars.scala.Handlebars._



object Tiger {

  object ComponentRegistry extends EnvComponent with SemanComponent with EscapesComponent with TranslateComponent with CanonComponent {
    override val escapes = new MyEscapes()
    override val translate = new MyTranslate()
    override val env = new Env()
    override val seman:Seman = new SemanImp()
    override val canon = new MyCanon()
  }

  case class RoData(name:String, content:String, length:Int, size: Int, mod:Int )

    //burn it and make again :D
  def main(args: Array[String]) {

    val stream = if (args.length == 1) new FileInputStream(new File(args(0)))
    else System.in

    val cp = new TigerParser()
    val prog = cp.parse(stream)

    ComponentRegistry.escapes.findEscapes(prog)

    println(prog.treeString)

    ComponentRegistry.seman.transProg(prog)

    println("======= Intermediate code ==========\n\n")

    ComponentRegistry.translate.fragments().foreach {
      case PROC(stm, frame) => println(stm.treeString)

      case f@STRING(l, s) => println(s"$l: $s\n")

    }

    println("======= Canonized intermediate code ==========\n\n")

    // reemplazar por estrucutras mutables
    var strings:List[RoData] = List()
    var procs:List[(List[Stm], Frame)] = List()
    var frames: Map[Temp.Label, Frame] = Map.empty

    ComponentRegistry.env.baseVenv.values.map {
      case ComponentRegistry.env.FuncEntry(_,name,formals,_,_) =>
        frames += name -> Frame(name, formals map ( _ => false))
      case _ => ()
    }


    ComponentRegistry.translate.fragments().foreach {
      case PROC(stm, frame) => {
        println(s"frame:$frame")
        val c = ComponentRegistry.canon.linearize(stm)
        procs ::=(c, frame)

        frames += frame.name -> frame

        c.foreach(x => {
          println("->" + x)
        })
      }

      case f@STRING(l, s) => {
        val size = s.size + Frame.WS + 1
        val r = size % 4
        strings ::= RoData(l, s, s.size, size + r , r)
      }
    }

    val label = Temp.newLabel()

    val p = procs map {
      case (stms,frame) => {
        val (v,d) = CodeGen(frames, stms)

        println("Frame--->")
        frame.formals.foreach(x => println(x.caller(Frame.FP)))
        println(s"${frame.name}")
        v foreach { x => println(x.code) }
        (v, d, frame)
      }
    } map {
      case (asm, d, frame) => {
        println(s"------ ${frame.name} ------")

        val (a1,a2) = RegisterAllocation(frame.procEntryExit2(asm), frame).get()

        (a1, d, a2)
      }
    } map {
      case (asm, d, frame) =>

        val data = if (d.ls.isEmpty) List() else List("", ".align	2", s".${d.l}:") ++ d.ls.map ( x => s".word\t\t${{x}}" )

        Map("name" -> frame.name, "asm" -> (asm.map { _.code } ++ data) )

    }

    val data = Map( "filename" -> args(0), "functions" -> p, "strings" -> strings, "label" -> label )
    val template = Handlebars(new File("src/main/hbs/asm.hbs"))


    Files.write(Paths.get("output/test.s"), template(data).getBytes(StandardCharsets.UTF_8));

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
