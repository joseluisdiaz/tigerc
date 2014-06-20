package tiger

import org.scalatest._
import org.scalatest.Matchers._
import Inside._

import tiger.Abs._
import tiger.Abs.FunctionDec
import tiger.Abs.LetExp
import tiger.Abs.VarDec
import tiger.TigerTestUtil.{TigerEscapes, TigerAbs}


class EscapesSpec extends FlatSpec {

  "EscapesComponent" should "escapes nested functions" in new TigerAbs("/aditionals/escap0.tig") with TigerEscapes {
    inside(tigerProgram()) { case LetExp(List(decX, decY, decsF) , body, _)  =>
      inside(decX) { case VarDec(name, escape, _, _, _) =>
        name should be ("x")
        escape should be (true)
      }
      inside(decY) { case VarDec(name, escape, _, _, _) =>
        name should be ("y")
        escape should be (false)
      }

      inside(decsF) { case FunctionDecs(List(decF)) =>
        inside(decF)  { case FunctionDec(fname, List(Field(arg1_name, arg1_escape,_)), _ ,_ ,_) =>
          fname should be ("f")
          arg1_name should be ("y")
          arg1_escape should be (true)
        }
      }
    }
  }

  it should "escapes for counter variable" in new TigerAbs("/type/assign-loop-var.tig") with TigerEscapes {
      inside(tigerProgram()) { case ForExp(symbol, s, hi, lo, body, _) =>
        symbol should be("i")
      }
  }


//  "escape2" should "be correct" in new TigerAbs("/aditionals/escap2.tig") with TigerEscapes {
//    inside(tigerProgram()) { case LetExp(List(decF) , body, _)  =>
//
//      inside(decsF) { case FunctionDecs(List(decF)) =>
//        inside(decF)  { case FunctionDec(fname, List(Field(arg1_name, arg1_escape,_)), _ ,body ,_) =>
//          fname should be ("f")
//          arg1_name should be ("i")
//          arg1_escape should be (false)
//
//          inside(body) { case LetExp(List(iDec, gDec))}
//
//
//        }
//      }
//    }
//  }




}