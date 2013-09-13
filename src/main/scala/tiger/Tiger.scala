package tiger

import tiger.parser.{TigerScanner, TigerParser}
import sext._

object Tiger {

  def main(args: Array[String]) {

    val cp = new TigerParser()
    
    cp.createScanner(System.in)
    val prog = cp.parse_prog()

    println(prog.treeString)
    Escapes.findEscapes(prog)
    println("----")
    println(prog.treeString)



  }

}
