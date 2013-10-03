package tiger

import tiger.Abs._
import tiger.Types._
import tiger.Types.Ty
import tiger.Env._
import scala.Error

/**
 * User: jose
 * Date: 9/18/13
 * Time: 1:16 PM
 */
trait Seman {

  def transProg(prog: Exp): Unit

  case class ExpTy(exp: Any, ty: Ty)

  type tenv = Map[String, Ty]
  type venv = Map[String, EnvEntry]

}

object Seman extends Seman {

  def notFound(key: String) = {
    error("type error: no defined " + key)(60) //tendria que poder usar implicit aca
  }

  def error(msg: String)(implicit pos: Pos) = {
    throw new Error(msg + " @line:"  + pos )
  }


  val tabTypes = Map("int" -> INT(), "string" -> STRING()) withDefault notFound

  val tabVars = Map(
    "print" -> FuncEntry(Env.mainLevel, "print", List(STRING()), STRING(), true),
    "flush" -> FuncEntry(Env.mainLevel, "flush", Nil, UNIT(), true),
    "getchar" -> FuncEntry(Env.mainLevel, "getstr", Nil, STRING(), true),
    "ord" -> FuncEntry(Env.mainLevel, "ord", Nil, INT(), true),
    "chr" -> FuncEntry(Env.mainLevel, "chr", List(INT()), STRING(), true),
    "size" -> FuncEntry(Env.mainLevel, "size", List(STRING()), INT(), true),
    "substring" -> FuncEntry(Env.mainLevel, "substring", List(STRING(), INT(), INT()), STRING(), true),
    "concat" -> FuncEntry(Env.mainLevel, "substring", List(STRING(), STRING()), STRING(), true),
    "not" -> FuncEntry(Env.mainLevel, "not", List(INT()), INT(), true),
    "exit" -> FuncEntry(Env.mainLevel, "exit", List(INT()), UNIT(), true)
  ) withDefault notFound



  def trVar(variable: Var): ExpTy = variable match {
    case SimpleVar(id) => ExpTy((), NIL())
//    case FieldVar(leftValue, id) =>
//    case SubscriptVar(leftValue, exp) =>
  }


  def trExp(exp: Exp): ExpTy = exp match {
    case VarExp(variable, position) => trVar(variable)

    case UnitExp(position) => ExpTy((), UNIT())
    case NilExp(position) => ExpTy((), NIL())
    case IntExp(value, position) => ExpTy((), INT())
    case StringExp(value, position) => ExpTy((), STRING())

    case CallExp(func, args, position) => {
      implicit val p = position

      //
      val fdef = tabVars(func) match {
        case func@FuncEntry(_, _, _, _, _) => func
        case found@_ => error("function expected: " + found)
      }

      if (args.size != fdef.params.size) {
        throw error("type error: wrong argument size")
      }

      val evaluedArgs = for ( x <- args ) yield (x, trExp(x))

      for (
        ( (param, ExpTy(_,ty1)), ty2 ) <- evaluedArgs zip fdef.params
      ) yield {
        if (ty1 != ty2) {
          error("type error: param " + param +
                " not matches " + ty2 +
                " in function " + func)
        }
      }

      ExpTy((), fdef.result)
    }

    case OpExp(left, oper, right, position) => {
      implicit val pos = position

      val ExpTy(_, tyl) = trExp(left)
      val ExpTy(_, tyr) = trExp(right)
      val unpacked = unpack(tyl)

      if ( tyl == tyr ) oper match {
          case EqOp()  | NeqOp() if !( tyl == NIL() && tyr == NIL() ) && tyl != NIL()
            => return ExpTy((), INT())

          case DivideOp() | TimesOp() | MinusOp() | PlusOp() if unpacked == INT()
            => return ExpTy((), INT())

          case LtOp() | LeOp() | GtOp() | GeOp() if unpacked == INT() || unpacked == STRING()
            => return ExpTy((), INT())

          case _ => ()
       }

      error("type error")
    }

    case RecordExp(fields, typ, position) => {
      implicit val p = position

      val r = tabTypes(typ) match {
        case r@RECORD(_) => r
        case _ =>  error("type error: '" + typ + "' not declared")
      }

      if ( r.records.size != fields.size ) {
        throw error("type error: wrong argument size")
      }

      val tfields = for ( (sy, exp) <- fields ) yield (sy, trExp(exp))

      for (
        ( (sym1, ExpTy(e, ty1)), (sym2, ty2, _)  ) <- tfields zip r.records
      ) yield {
        if ( sym1 != sym2) error("type error: " + sym1 + "!=" + sym2)
        if ( ty1 != ty2 ) error("type error: " + ty1 + "!=" + ty1)
      }

      ExpTy(() , r)
    }

    case SeqExp(exps, position) => {
      val ty = for {
          exp <- exps
          ExpTy(_, t) = trExp(exp)
      } yield t

      ExpTy((), ty.last)
    }

    // a: = 32
    case AssignExp(SimpleVar(variable), exp, position) => ExpTy((),UNIT())

    // a[1].r := ???
    // a.r[3] := ???


    //    case IfExp(test, then, _else, position) =>
    //    case WhileExp(test, body, position) =>
    //    case ForExp(variable, escape, lo, hi, body, position) =>
    //    case LetExp(decs, body, position) =>
    //    case BreakExp(position) =>
    //    case ArrayExp(typ, size, init, position) =>
  }


  def transProg(prog: Exp) {}

}