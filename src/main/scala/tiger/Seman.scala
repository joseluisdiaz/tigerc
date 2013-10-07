package tiger

import tiger.Abs._
import tiger.Types._
import tiger.Env._
import scala.{Int, Error}
import scala.Predef._
import tiger.Abs._
import scala.Some
import tiger.Types.RECORD
import tiger.Env.FuncEntry
import tiger.Env.VarEntry
import tiger.Types.ARRAY
import tiger.Types.UNIT
import tiger.Types.NIL
import tiger.Types.ALIAS
import tiger.Types.STRING

/**
 * User: jose
 * Date: 9/18/13
 * Time: 1:16 PM
 */
trait Seman {

  def transProg(prog: Exp): Unit

  case class ExpTy(exp: Any, ty: Types.Ty)

  type tenv = Map[String, Types.Ty]
  type venv = Map[String, EnvEntry]

  def transVar(venv:venv, tenv:tenv, vr:Var): ExpTy
  def transExp(venv:venv, tenv:tenv, exp:Exp): ExpTy
  def transDec(venv:venv, tenv:tenv, dec:Dec): (venv,tenv)
  def transTy(tenv:tenv, exp:Types.Ty): Types.Ty

}

object Seman extends Seman {


  def trVar(variable: Var): ExpTy = variable match {

    case SimpleVar(id) => {
      val varTy = tabVars(id) match {
        case VarEntry(v) => v
        case found@_ => error("variable expected: " + found)
      }

      ExpTy((), varTy)
    }

    case FieldVar(leftValue, id) => {

      val recordTy = trVar(leftValue).ty match {
        case RECORD(records) => records
        case found@_ => error("type error: found " + found + " RECORD" + " required")
      }

      val ty = recordTy find { _._1 == id } match {
        case Some( (_,x,_) ) => x //Type
        case None => error("type error: field not found" + id)
      }

      ExpTy((), ty)
    }

    case SubscriptVar(leftValue, exp) => {
      val arrayTy = trVar(leftValue).ty match {
        case ARRAY(ty) => ty
        case found@_ => error("type error: found " + found + " ARRAY" + " required")
      }

      val expTy = trExp(exp)

      if ( expTy.ty != INT() ) {
        error("type error: found " + expTy.ty + " INT" + " required")
      }

      ExpTy((), arrayTy)
    }

  }

  def trDec(dec: Dec, venv:venv, tenv:tenv) = dec match {

    case VarDec(name, escape, None, init, position) => {
      val initTy = transExp(venv, tenv, init)

      (venv + (name -> initTy.ty), tenv)
    }

    case VarDec(name, escape, Some(ty), init, position) => {
      val initTy = transExp(venv, tenv, init)
      val varDecTy = venv(ty)

      // RECORD == NIL is handled on Types equality declaration
      if (initTy.ty != varDecTy) {
        error("type error: invalid declaration")
      }

      (venv + (name -> initTy.ty), tenv)
    }

    case TypeDecs(decs) => {

      def transTy(tab: tenv,ty:Abs.Ty): Types.Ty = ty match {
        case RecordTy(fields) => {

          val records = for {
            (f,i) <- fields.zipWithIndex
          } yield (f.name, transTy(tab, f.ty),i)

          val idList = records.map( _._1)

          if (idList.size != idList.toSet.size) {
            error("type error: record contain duplicated fields")
          }

          RECORD (records)
        }

        case NameTy(name) => ALIAS(name,None)
        case ArrayTy(name) => ARRAY(ALIAS(name, None))
      }

      // create an enviroment with empty functions
      val voidtenv = decs.foldLeft(tenv)(
        (acc:tenv, t:TypeDec) => acc + (t.name -> ALIAS(t.name, None))
      )

      // proccess all declaration
      val __tenv = decs.foldLeft(voidtenv)(
        (acc:tenv, t:TypeDec) => acc + (t.name -> transTy(acc, t.ty))
      )

      // look for ALIAS(None) definitions and ARRAY(ALIAS("x", None)
      def genDependencyGraph(tab:List[(String, Types.Ty)]):List[(String, String)] = tab match {
          case Nil => Nil
          case (next, ALIAS(prev, None))::xs => (prev, next)::genDependencyGraph(xs)
          case (next, ARRAY(x))::xs => genDependencyGraph(List((next, x))) ++ genDependencyGraph(xs)
        //  case (next, RECORD(r))::xs => genDependencyGraph(r.map( x => (x._1, x._2) )) ++ genDependencyGraph(xs)
          case x::xs => genDependencyGraph(xs)
      }

      val envKeyProcOrder = Util.tsort(genDependencyGraph(__tenv.toList))

      // create a pseudo-alias-free enviroment
      def unpack(typ: Types.Ty, tab:tenv):Types.Ty = typ match {
        case ARRAY(tipo) => ARRAY(unpack(tipo,tab))
        case ALIAS(name, None) => tab(name)
        case x => x
      }

      val finalEnv = envKeyProcOrder.foldLeft(__tenv)( (acc:tenv, x:String) => acc + (x -> unpack(acc(x), acc) ))

      def changeReferences(typ: Types.Ty, tab:tenv):Unit = typ match {
        case RECORD(records) => records.map( x => changeReferences( x._2, tab) )
        case ARRAY(tipo) => changeReferences(tipo,tab)
        case a@ALIAS(name, None) => a.ty = Some(tab(name))
        case x => ()
      }

      val recordsKeySet = __tenv.keySet -- tenv.keySet -- envKeyProcOrder

      recordsKeySet.map( x => changeReferences(finalEnv(x), finalEnv) )

      (venv, finalEnv)
    }
    case FunctionDecs(decs) => {
      (venv, tenv)
    }

  }


  def trExp(exp: Exp):ExpTy = {
    implicit val p = exp.position
    trExp(exp, exp.position)
  }

  def trExp(exp: Exp, position:Pos): ExpTy = exp match {
    case VarExp(variable, position) => trVar(variable)

    case UnitExp(position) => ExpTy((), UNIT())
    case NilExp(position) => ExpTy((), NIL())
    case IntExp(value, position) => ExpTy((), INT())
    case StringExp(value, position) => ExpTy((), STRING())

    case CallExp(func, args, position) => {
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

    case AssignExp(v, exp, position) => {
      val vTy = trVar(v)
      val eTy = trExp(exp)

      vTy.ty match {
        case INT(true) => error("type error: found read only integer in assign")
        case _ => ()
      }

      if (vTy.ty != eTy.ty) {
        error("type error: found " + eTy.ty + " required " + vTy.ty )
      }

      ExpTy((), UNIT())

    }

    case IfExp(test, then, None, position) => {
      val testTy = trExp(test)
      val thenTy = trExp(then)

      if ( testTy.ty != INT() || thenTy.ty == UNIT() ) {
        error("type error: if")
      }

      ExpTy((), UNIT())

    }

    case IfExp(test, then, Some(_else), position) => {
      val testTy = trExp(test)
      val thenTy = trExp(test)
      val elseTy = trExp(_else)

      if ( testTy.ty != INT() || thenTy.ty == elseTy.ty ) {
        error("type error: if")
      }

      ExpTy((), elseTy.ty)
    }

    case WhileExp(test, body, position) => {
      val testTy = trExp(test)
      val bodyTy = trExp(test)

      if (testTy.ty != INT() || bodyTy.ty != UNIT() )
        error("type error: while")


      ExpTy((), UNIT())

    }

    case ForExp(variable, escape, lo, hi, body, position) => {
      val loTy = trExp(lo)
      val hiTy = trExp(hi)

      if (loTy.ty != INT()) error("type error: for: lo must be int")
      if (hiTy.ty != INT()) error("type error: for: hi must be int")

      val bodyEnv = tabVars + (variable -> VarEntry(INT(true)))

      val bodyTy = transExp(bodyEnv, tabTypes, body)



      ExpTy((), UNIT())
    }
    case LetExp(decs, body, position) => {

      ExpTy((), UNIT())
    }

    case BreakExp(position) => ExpTy((), UNIT())
    //    case ArrayExp(typ, size, init, position) =>
  }

  def notFound(key: String) = {
    error("type error: no defined " + key) //tendria que poder usar implicit aca
  }

  def error(msg: String)/*(implicit pos: Pos)*/ = {
    throw new Error(msg + " @line:"  + 11 )
  }


  val tabTypes:tenv = Map("int" -> INT(), "string" -> STRING()) withDefault notFound

  val tabVars:venv = Map(
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


  def transProg(prog: Exp): Unit = ???

  def transVar(venv: venv, tenv: tenv, vr: Var): Seman.ExpTy = ???

  def transExp(venv: venv, tenv: tenv, exp: Exp): Seman.ExpTy = ???

  def transDec(venv: venv, tenv: tenv, dec: Dec): (venv, tenv) = ???

  def transTy(tenv: tenv, exp: Types.Ty): Types.Ty = ???
}