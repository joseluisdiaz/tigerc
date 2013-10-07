package tiger

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{PropSpec, FlatSpec}

import tiger.Types._



/**
 * User: jose
 * Date: 9/26/13
 * Time: 10:29 PM
 */
class TypesSpec extends PropSpec with TableDrivenPropertyChecks with ShouldMatchers {

  val r1 = RECORD(List(("i", INT(), 1)))
  val r2 = RECORD(List(("i", INT(), 1)))
  val r3 = RECORD(List(("i", STRING(), 1)))

  val a1 = ARRAY(INT())
  val a2 = ARRAY(INT())

  val f1 = FUNCTION(List(INT(), r1, a1), UNIT())
  val f2 = FUNCTION(List(INT(), r1, a1), UNIT())
  val f3 = FUNCTION(List(INT(), r2, a1), UNIT())

  val TRUE =
    Table(
      ("t1", "t2"),
      (INT(), INT()),
      (STRING(), STRING()),
      (r1,r1),
      (a2,a2),
      (ALIAS("a", Some(INT())), INT() ),
      (INT(), ALIAS("a", Some(INT())) ),

      (ALIAS("a", Some(r1)), ALIAS("b", Some(r1)) ),
      (f1,f2),
      (INT(false), INT(true))
    )

  val FALSE =
    Table(
      ("t1", "t2"),
      (INT(), STRING()),
      (STRING(), INT()),
      (a1, a2),
      (r1, r2),
      (f2, f3),
      (ALIAS("a", Some(r1)), ALIAS("b", Some(r2)))

    )


  property("equality should be true") {
    forAll (TRUE) { (t1: Ty, t2:Ty ) =>
      t1 == t2 should be (true)
    }
  }

  property("equality should be false") {
    forAll (FALSE) { (t1: Ty, t2:Ty ) =>
      t1 == t2 should be (false)
    }
  }

}
