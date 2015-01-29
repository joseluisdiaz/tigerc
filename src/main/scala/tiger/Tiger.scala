package tiger

import java.io.File
import java.io.FileInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import com.gilt.handlebars.scala.Handlebars
import com.gilt.handlebars.scala.helper.Helper
import tiger.Frame.{PROC, STRING}
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
    override val seman: Seman = new SemanImp()
    override val canon = new MyCanon()
  }

  case class RoData(name: String, content: String, length: Int, size: Int, mod: Int)

  //burn it and make again :D
  def main(args: Array[String]) {


    if (args.length < 1) {
      println("requieres a file name at least")
      System.exit(-1)
    }
    val filename = args(0)

    val outputFile = if (args.length >= 2) args(1) else filename.replaceAll("\\.[^.]*$", "") + ".s"

    val stream = new FileInputStream(new File(filename))

    val cp = new TigerParser()
    val prog = cp.parse(stream)

    ComponentRegistry.escapes.findEscapes(prog)

    println(prog.treeString)

    ComponentRegistry.seman.transProg(prog)

    println("======= Intermediate code ==========\n\n")

    ComponentRegistry.translate.fragments().foreach {
      case PROC(stm, frame) => {

        Files.write(Paths.get(s"${outputFile}_${frame.name}.inter"), Util.printsmt(stm).getBytes(StandardCharsets.UTF_8))
      }
      case f@STRING(l, s) => ()
    }

    println("======= Canonized intermediate code ==========\n\n")

    // reemplazar por estrucutras mutables
    var strings: List[RoData] = List()
    var procs: List[(List[Stm], Frame)] = List()
    var frames: Map[Temp.Label, Frame] = Map.empty

    ComponentRegistry.env.baseVenv.values.map {
      case ComponentRegistry.env.FuncEntry(_, name, formals, _, _) =>
        frames += name -> Frame(name, formals map (_ => false))
      case _ => ()
    }
    frames += "_allocArray" -> Frame("_allocArray", List(false, false))

    frames += "_checkIndexArray" -> Frame("_checkIndexArray", List(false, false))

    frames += "_allocRecord" -> Frame("_allocRecord", List(false, false))
    frames += "_checkNil" -> Frame("_checkNil", List(false))
    frames += "_stringCompare" -> Frame("_stringCompare", List(false))



    ComponentRegistry.translate.fragments().foreach {
      case PROC(stm, frame) => {
        val c = ComponentRegistry.canon.linearize(stm)
        procs ::=(c, frame)

        frames += frame.name -> frame
        Files.write(Paths.get(s"${outputFile}_${frame.name}.cannon"), (c mkString "\n").getBytes(StandardCharsets.UTF_8))
      }

      case f@STRING(l, s) => {
        def escapa(s: String) = s flatMap { x => if (x.isControl) f"\\$x%03o" else x.toString}

        val size = s.size + Frame.WS + 1
        val r = size % 4
        strings ::= RoData(l, escapa(s), s.size, size + r, r)
      }
    }

    val label = Temp.newLabel()

    val p = procs map {
      case (stms, frame) => {
        val (v, s, i) = CodeGen(frames, stms)

        val formals = frame.formals.map(x => x.exp(Frame.FP))

        //TODO MOVE TO ASM
        val output = (formals ++ List("", "") ++ v) mkString "\n"
        Files.write(Paths.get(s"${outputFile}_${frame.name}.code"), output.getBytes(StandardCharsets.UTF_8))

        (v, (s, i), frame)
      }
    } map {
      case (asm, (s, i), frame) => {
        println(s"------ ${frame.name} ------")


        val (a1, a2) = RegisterAllocation(frame.procEntryExit2(asm), frame).get()

        (a1, (s, i), a2)
      }
    } map {
      case (asm, (s, i), frame) =>

        val dataString = if (s.ls.isEmpty) List()
        else List("", ".align	2", s".${s.l}:") ++ s.ls.map(x => s".word\t\t${x}")

        val dataInts = if (i.ls.isEmpty) List()
        else List("", ".align	2", s".${i.l}:") ++ i.ls.map(x => s".word\t\t${x}")

        Map("name" -> frame.name, "asm" -> (asm.map {
          _.code
        } ++ dataString ++ dataInts))

    }

    val data = Map("filename" -> args(0), "functions" -> p, "strings" -> strings, "label" -> label)
    val template = Handlebars(new File("src/main/hbs/asm.hbs"))

    Files.write(Paths.get(s"$outputFile"), template(data).getBytes(StandardCharsets.UTF_8));

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
