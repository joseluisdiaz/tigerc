package tiger

import tiger.parser.{TigerScanner, TigerParser}
import sext._

object Tiger {

  def main(args: Array[String]) {

    val cp = new TigerParser()
    
    cp.createScanner(System.in);

    println(cp.parse_prog().treeString)

  }

}
