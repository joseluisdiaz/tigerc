package tiger

import org.scalatest.FlatSpec
import tiger.TigerTestUtil.TypesToFromString
import tiger.Types.INT

/**
 * Created by jose on 6/27/14.
 */
class TranslateSpec extends FlatSpec with TypesToFromString {

  "translate" should "awesome" in {
    " 2 > 4 " should typeTo(INT())

  }
}
