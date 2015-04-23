package tiger

import java.io.File
import java.io.FileInputStream
import java.io.InputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import com.gilt.handlebars.scala.Handlebars
import tiger.Asm.Instr
import tiger.Frame.{PROC, STRING}
import tiger.Tree.Stm
import tiger.parser.{TigerScanner, TigerParser}

import com.gilt.handlebars.scala.binding.dynamic._


object Tiger {

  object Compiler extends EnvComponent with SemanComponent with EscapesComponent with TranslateComponent with CanonComponent {
    override val escapes = new MyEscapes()
    override val translate = new MyTranslate()
    override val env = new Env()
    override val seman: Seman = new SemanImp()
    override val canon = new MyCanon()

    case class RoData(name: String, content: String, length: Int, size: Int, mod: Int)

    var strings: List[RoData] = List()
    var procs: List[(List[Stm], Frame)] = List()

    def compile(stream: InputStream): (List[Map[String, Any]], List[RoData]) = {


      // ast generation
      val ast = getAst(stream)

      // look for variables that escapes
      escapes.findEscapes(ast)

      // Semantic analysis and intermediate code generator
      seman.transProg(ast)

      // Linearize intermediate code, and separte code fragments
      separteFragments()

      val functionFrames = procs map { case (stms, frame) => frame.name -> frame} toMap

      val frames = functionFrames ++ env.baseFrames

      val functions = procs map { case (stms, frame) =>

        val (asm, dataString, dataInt) = CodeGen(frames, stms)

        val coloredAsm = RegisterAllocation(frame.procEntryExit2(asm), frame)

        createView(frame, coloredAsm, dataString, dataInt)
      }


      (functions, strings)

    }


    def createView(frame: Frame, asm: List[Instr], s: Data, i: Data) = {

      val dataString = if (s.ls.isEmpty) List()
      else List("", ".align	2", s".${s.l}:") ++ s.ls.map(x => s".word\t\t${x}")

      val dataInts = if (i.ls.isEmpty) List()
      else List("", ".align	2", s".${i.l}:") ++ i.ls.map(x => s".word\t\t${x}")

      Map("name" -> frame.name, "asm" -> (asm.map { _.code } ++ dataString ++ dataInts))
    }


    def separteFragments(): Unit = {
      translate.fragments().foreach {
        case PROC(stm, frame) => procs ::= canon.linearize(stm) -> frame


        case f@STRING(l, s) => {
          def escapa(s: String) = s flatMap { x => if (x.isControl) f"\\$x%03o" else x.toString}

          val size = s.size + Frame.WS + 1
          val r = size % 4

          strings ::= RoData(l, escapa(s), s.size, size + r, r)
        }
      }
    }

    def getAst(stream: InputStream): Abs.Exp = new TigerParser().parse(stream)


  }


  //burn it and make again :D
  def main(args: Array[String]) {


    if (args.length < 1) {
      println("requieres a file name at least")
      System.exit(-1)
    }
    val filename = args(0)

    val outputFile = if (args.length >= 2) args(1) else filename.replaceAll("\\.[^.]*$", "") + ".s"

    val stream = new FileInputStream(new File(filename))


    val (functions, strings) = Compiler.compile(stream)

    stream.close()

    val data = Map("filename" -> args(0), "functions" -> functions, "strings" -> strings)
    val template = Handlebars(new File("src/main/hbs/asm.hbs"))

    Files.write(Paths.get(s"$outputFile"), template(data).getBytes(StandardCharsets.UTF_8))

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
