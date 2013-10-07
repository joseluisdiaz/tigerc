package tiger

import tiger.TigerTestUtil.{TigerEscapes, TigerAbsFromString}
import org.scalatest._
import org.scalatest.Matchers._

import org.scalatest.matchers.{MatchResult, Matcher}
import tiger.Seman._
import tiger.Types._
import tiger.Abs._
import tiger.Abs.RecordTy
import tiger.Abs.TypeDec
import tiger.Abs.Field
import tiger.Types.Ty
import tiger.Abs.TypeDecs
import tiger.Types.STRING
import tiger.Abs.ArrayTy
import tiger.Abs.NameTy
import tiger.Types.UNIT
import org.scalatest.Inside._
import tiger.Abs.RecordTy
import tiger.Abs.TypeDec
import tiger.Abs.Field
import tiger.Abs.VarDec
import tiger.Abs.TypeDecs
import tiger.Types.STRING
import tiger.Abs.ArrayTy
import tiger.Abs.NameTy
import tiger.Abs.LetExp
import tiger.Types.ARRAY
import tiger.Types.UNIT


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

      intercept[Error] {
        "cunak(1)" should typeTo ( UNIT() )
      }

      intercept[Error] {
        """substring("hola")""" should typeTo( STRING() )
      }

      intercept[Error] {
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

    intercept[Error] {
      trDec(recTy, Seman.tabVars, Seman.tabTypes)
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

    val (venv, tenv) = trDec(recTy, Seman.tabVars, Seman.tabTypes)

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




}
