package tiger

import tiger.TigerTestUtil.{TigerAbs, TigerEscapes, TigerAbsFromString}
import org.scalatest._
import org.scalatest.Matchers._

import tiger.Types._
import tiger.Types.Ty
import org.scalatest.Inside._
import tiger.Abs.RecordTy
import tiger.Abs.TypeDec
import tiger.Abs.Field
import tiger.Abs.TypeDecs
import tiger.Types.STRING
import tiger.Abs.ArrayTy
import tiger.Abs.NameTy
import tiger.Types.ARRAY
import tiger.Types.UNIT


/**
 * User: jose
 * Date: 9/25/13
 * Time: 7:07 PM
 */



class SemanSpec extends FlatSpec {

  /* una idea en busca de una mejor sintaxis */
  def tigerProgFromString(s: String) = {
    Seman.transProg( ( new TigerAbsFromString(s) with TigerEscapes) tigerProgram() ).ty
  }

  def tigerProg(s: String) = {
    Seman.transProg( ( new TigerAbs(s) with TigerEscapes) tigerProgram() ).ty
  }


  def typeTo(ty:Ty) = be(ty) compose tigerProgFromString

  def typ = be(INT()) compose tigerProg

  "a binary operation " should "type to INT" in
  {
    """ "2" > "4"  """ should typeTo( INT() )

    " 2 > 4 " should typeTo( INT() )

    " 2 + 4 "  should typeTo( INT() )

    intercept[TypeError] {
      """ 2 > "4"  """ should typeTo( INT() )
    }
  }

  "functions" should "type" in
    {
      """substring("stringy",1,1)""" should typeTo( STRING() )

      intercept[TypeError] {
        "cunak(1)" should typeTo ( UNIT() )
      }

      intercept[TypeError] {
        """substring("hola")""" should typeTo( STRING() )
      }

      intercept[TypeError] {
        "substring(1,1,1)" should typeTo(STRING())
      }

    }

  "mutual recursive type declaration" should "not type" in {
    val recTy = TypeDecs(
      List(
      TypeDec("a", NameTy("b"),1),
      TypeDec("b", NameTy("a"),2)
      )
    )

    intercept[RuntimeException] {
      Seman.transDec(Seman.tabVars, Seman.tabTypes, recTy)
    }

  }

  "recursive type declaration" should "type" in {
    val recTy = TypeDecs(
      List(
        TypeDec("a", NameTy("b"), 1),
        TypeDec("b", NameTy("c"), 2),
        TypeDec("c", NameTy("int"), 2),

        TypeDec("p", ArrayTy("a"), 2),
        TypeDec("list",
          RecordTy(List(
            Field("first",false,NameTy("p")),
            Field("rest",false,NameTy("list"))
          )),3)))

    val (venv, tenv) = Seman.transDec(Seman.tabVars, Seman.tabTypes, recTy)

    tenv("a") should be(INT())
    tenv("b") should be(INT())
    tenv("c") should be(INT())

    inside(tenv("p")) {
      case ARRAY(x)  => x should be(INT())
    }

    inside(tenv("list")) {
      case RECORD(List(r1,r2)) => {
        inside(r1) {
          case (name, ALIAS(aliasName, Some(ty)), _) => {
            name should be("first")
            ty should be(tenv("p"))
          }
        }
        inside(r2) {
          case (name, ALIAS(aliasName,Some(ty)), _) => {
            name should be("rest")
            ty should be(tenv("list"))
          }
        }


      }
    }




  }

  "this list" should "have a type" in {
    "/good/compare-record-and-nil.tig" should typ
    "/good/fact.tig" should typ
    "/good/fun-vs-var.tig" should typ
    "/good/local-vs-global-type.tig" should typ
    "/good/merge.tig" should typ
    "/good/queens.tig" should typ
    "/good/recursive-comments.tig" should typ
    "/good/recursive-types.tig" should typ
    "/good/test01.tig" should typ
    "/good/test02.tig" should typ
    "/good/test03.tig" should typ
    "/good/test06.tig" should typ
    "/good/test07.tig" should typ
    "/good/test08.tig" should typ
    "/good/test12.tig" should typ
    "/good/test27.tig" should typ
    "/good/test30.tig" should typ
    "/good/test37.tig" should typ
    "/good/test38.tig" should typ
    "/good/test39.tig" should typ
    "/good/test42.tig" should typ
    "/good/test44.tig" should typ
    "/good/test47.tig" should typ
    "/good/test48.tig" should typ
//    "/good/test50.tig" should typ
    "/good/three-name-spaces.tig" should typ
  }




}
