package tiger

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks

import tiger.TigerTestUtil._
import tiger.Types.{STRING, Ty, UNIT, _}

import tiger.TigerTestUtil.ComponentRegistry._


/**
 * User: jose
 * Date: 9/25/13
 * Time: 7:07 PM
 */


class SemanSpec2 extends PropSpec with TableDrivenPropertyChecks with TypesToFromFile {

  val goodExamples = Table(
    ("sourceFile", "typ"),
    ("/good/compare-record-and-nil.tig", INT()),
    ("/good/fact.tig", INT()),
    ("/good/fun-vs-var.tig", INT()),
    ("/good/local-vs-global-type.tig", INT()),
    ("/good/merge.tig", INT()),
    ("/good/queens.tig", INT()),
    ("/good/recursive-comments.tig", INT()),
    ("/good/recursive-types.tig", INT()),
    ("/good/test01.tig", INT()),
    ("/good/test02.tig", INT()),
    ("/good/test03.tig", INT()),
    ("/good/test06.tig", INT()),
    ("/good/test07.tig", INT()),
    ("/good/test08.tig", INT()),
    ("/good/test12.tig", INT()),
    ("/good/test27.tig", INT()),
    ("/good/test30.tig", INT()),
    ("/good/test37.tig", INT()),
    ("/good/test38.tig", INT()),
    ("/good/test39.tig", INT()),
    ("/good/test42.tig", INT()),
    ("/good/test44.tig", INT()),
    ("/good/test47.tig", INT()),
    ("/good/test48.tig", INT()),
    ("/good/test50.tig", UNIT()),
    ("/good/three-name-spaces.tig", INT())
  )

  property("must have a valid type") {
    forAll(goodExamples) { (sourceFile: String, typ: Ty) =>
      sourceFile should typeTo(typ)
    }
  }

  val typeErrorExamples = Table(
    ("sourceFile", "xx"),
    ("/type/nil-equal-nil.tig", 0),
    ("/type/test09.tig", 0),
    ("/type/test10.tig", 0),
    ("/type/test11.tig", 0),
    ("/type/test13.tig", 0),
    ("/type/test14.tig", 0),
    ("/type/test15.tig", 0),
    ("/type/test21.tig", 0),
    ("/type/test22.tig", 0),
    ("/type/test23.tig", 0),
    ("/type/test24.tig", 0),
    ("/type/test25.tig", 0),
    ("/type/test26.tig", 0),
    ("/type/test28.tig", 0),
    ("/type/test29.tig", 0),
    ("/type/test31.tig", 0),
    ("/type/test32.tig", 0),
    ("/type/test34.tig", 0),
    ("/type/test35.tig", 0),
    ("/type/test36.tig", 0),
    ("/type/test40.tig", 0),
    ("/type/test43.tig", 0),
    ("/type/test45.tig", 0),
    ("/type/test52.tig", 0),

    ("/type/test16.tig", 0),

    ("/type/test17.tig", 0),
    ("/type/test18.tig", 0),
    ("/type/test33.tig", 0),

    ("/type/test19.tig", 0),
    ("/type/test20.tig", 0),

    ("/type/test51.tig", 0),
    ("/type/assign-loop-var.tig", 0)


  )

  property("must fail with error") {
    forAll(typeErrorExamples) { (sourceFile: String, xx: Int) =>
      intercept[TypeError] {
        sourceFile should typeTo(UNIT())
      }
    }
  }


}
