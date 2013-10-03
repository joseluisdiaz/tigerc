package tiger

import tiger.parser.{TigerScanner, TigerParser}
import sext._

object Tiger {

  def main(args: Array[String]) {

    val cp = new TigerParser()
    
    val prog = cp.parse(System.in)

    println(prog.treeString)
    println("----")
    Escapes.findEscapes(prog)
    println(prog.treeString)



  }

}
