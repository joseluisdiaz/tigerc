package tiger

import java.io.File
import java.io.FileInputStream
import com.gilt.handlebars.scala.Handlebars
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

  case class Function(name:String, asm: List[String])

  def main(args: Array[String]) {

    val stream = if (args.length == 1) {

      println(s"->${args(0)}")

      new FileInputStream(new File(args(0)))
    } else {
      System.in
    }

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
    var strings:List[(Temp.Label, String)] = List()
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
        strings ::= (l, s)

        println(s"$l: $s\n")
      }
    }



    val p = procs map {
      case (stms,frame) => {
        val v = CodeGen(frames, stms)
        println("Frame--->")
        frame.formals.foreach(x => println(x.exp))
        println(s"${frame.name}")
        v foreach { x => println(x.code) }
        (v, frame)
      }
    } map {
      case (asm, frame) => {
        println(s"------ ${frame.name} ------")

        RegisterAllocation(asm, frame).get()
      }
    } map {
      case (asm, frame) =>
        Function( frame.name,  asm map { _.code } )
    }

    val data = Map( "filename" -> args(0), "functions" -> p)
    val template = Handlebars(new File("src/main/hbs/asm.hbs"))

    println(template(data))


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
