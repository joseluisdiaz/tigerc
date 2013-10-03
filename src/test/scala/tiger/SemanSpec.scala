package tiger

import tiger.TigerTestUtil.{TigerEscapes, TigerAbsFromString}
import org.scalatest._
import org.scalatest.Matchers._

import org.scalatest.matchers.{MatchResult, Matcher}
import tiger.Seman._
import tiger.Types.{UNIT, STRING, INT, Ty}


/**
 * User: jose
 * Date: 9/25/13
 * Time: 7:07 PM
 */



class SemanSpec extends FlatSpec {

  /* una idea en busca de una mejor sintaxis */
  def tigerProg(s: String) = {
    trExp ( (new TigerAbsFromString(s) with TigerEscapes) tigerProgram()).ty
  }
  def typeTo(ty:Ty) = be(ty) compose tigerProg

  "a binary operation " should "type to INT" in
  {
    """ "2" > "4"  """ should typeTo( INT() )

    " 2 > 4 " should typeTo( INT() )

    " 2 + 4 "  should typeTo( INT() )

    intercept[Error] {
      """ 2 > "4"  """ should typeTo( INT() )
    }
  }

  "functions" should "type" in
    {
      """substring("stringy",1,1)""" should typeTo( STRING() )

      "cunak(1)" should typeTo ( UNIT() )

      intercept[Error] {
        """substring("hola")""" should typeTo( STRING() )
      }

      intercept[Error] {
        "substring(1,1,1)" should typeTo(STRING())
      }

    }

}
