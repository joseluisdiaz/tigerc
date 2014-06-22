package tiger

import tiger.TigerTestUtil.ComponentRegistry._
import tiger.TigerTestUtil._
import org.scalatest._
import org.scalatest.Matchers._

import tiger.Types._
import tiger.Types.Ty
import tiger.Types.STRING
import tiger.Types.UNIT

/**
 * User: jose
 * Date: 9/25/13
 * Time: 7:07 PM
 */

class SemanSpec extends FlatSpec with TypesToFromString {

  "a binary operation " should "type to INT" in {
    """ "2" > "4"  """ should typeTo(INT())

    " 2 > 4 " should typeTo(INT())

    " 2 + 4 " should typeTo(INT())

    intercept[SemanComponent#TypeError] {
      """ 2 > "4"  """ should typeTo(INT())
    }
  }

  "simple functions" should "type" in {
    """substring("stringy",1,1)""" should typeTo(STRING())

    intercept[Env#EnvError] {
      "cuanak(1)" should typeTo(UNIT())
    }

    intercept[SemanComponent#TypeError] {
      """substring("hola")""" should typeTo(STRING())
    }

    intercept[SemanComponent#TypeError] {
      "substring(1,1,1)" should typeTo(STRING())
    }

  }

}
