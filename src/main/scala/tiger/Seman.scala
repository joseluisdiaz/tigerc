package tiger

import tiger.Types._
import tiger.Env._
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
case class ExpTy(exp: Unit, ty: Types.Ty)
class TypeError(message: String) extends RuntimeException(message)
class EnvError(message: String) extends RuntimeException(message)



trait Seman {

  def transVar(venv:venv, tenv:tenv, vr:Var): ExpTy
  def transExp(venv:venv, tenv:tenv, exp:Exp): ExpTy
  def transDec(venv:venv, tenv:tenv, dec:Dec): (venv,tenv)

}

class Trans(tabVars:venv, tabTypes:tenv) {

  //TODO: may be a state monad will help
  var currentPosition:Pos = 0
  def position() = currentPosition
  def position(p:Exp):Exp = { currentPosition = p.position;p }
  def position(p:Dec):Dec = { currentPosition = p.position;p }


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
        case None => error("type error: field not found " + id)
      }

      ExpTy((), ty)
    }

    case SubscriptVar(leftValue, exp) => {
      val varEntry = unpack(trVar(leftValue).ty)
      val arrayTy = varEntry  match {
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

  def trDec(venv:venv, tenv:tenv, dec: Dec):(venv,tenv) = position(dec) match {

    case VarDec(name, escape, None, init, position) => {
      val initTy = Seman.transExp(venv, tenv, init)

      if (initTy.ty.isNil() )
        error("type error: invalid declaration")

      (venv + (name -> VarEntry(initTy.ty)), tenv )
    }

    case VarDec(name, escape, Some(ty), init, position) => {
      val initTy = Seman.transExp(venv, tenv, init)
      val varDecTy = tenv(ty)

      // RECORD == NIL is handled on Types equality declaration
      if (initTy.ty != varDecTy) {
        error("type error: invalid declaration")
      }

      (venv + (name -> VarEntry(varDecTy)), tenv )
    }

    case TypeDecs(decs) => {

      //TODO: Check if the type exists in a previous enviroment; tab makes no sense now
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

      val finalTypeEnv = envKeyProcOrder.foldLeft(__tenv)( (acc:tenv, x:String) => acc + (x -> unpack(acc(x), acc) ))

      def changeReferences(typ: Types.Ty, tab:tenv):Unit = typ match {
        case RECORD(records) => records.map( x => changeReferences( x._2, tab) )
        case ARRAY(tipo) => changeReferences(tipo,tab)
        case a@ALIAS(name, None) => a.ty = Some(tab(name))
        case _ => ()
      }

      val recordsKeySet = __tenv.keySet -- tenv.keySet //-- envKeyProcOrder
      //TODO: revisit substract envKeyProcOrder impact

      recordsKeySet.map( x => changeReferences(finalTypeEnv(x), finalTypeEnv) )

      (venv, finalTypeEnv)
    }
    case FunctionDecs(decs) => {

      def transTy(ty:Abs.Ty):Types.Ty = ty match {
        case NameTy(name) => tenv(name)
        case _ => error("type error: shouln't happend")
      }

      def functionTrans(f:FunctionDec) = {
        val params = f.params.map( x => transTy(x.ty))
        val result = f.result match {
          case None => UNIT()
          case Some(x) => tenv(x)
        }
        FuncEntry((), Temp.newLabel(), params, result, extern = false)
      }

      val venvWithFunction = decs.foldLeft(venv)((x:venv, f:FunctionDec) =>  x + (f.name -> functionTrans(f)))

      def addToVEnv(params:List[Field]) =
        params.foldLeft(venvWithFunction)((x:venv, f:Field) =>  x + (f.name -> VarEntry(transTy(f.ty))))

      val functionsTy = decs.map( x => (x.name, Seman.transExp(addToVEnv(x.params),tenv,x.body).ty ))

      functionsTy.foreach({
        case (x, y) => venvWithFunction(x) match {
          case VarEntry(ty) => false
          case FuncEntry(_, _, _, result, _) => if (result != y)
            error("return type must be: "+ result+ " found " +y)
        }
      })

      (venvWithFunction, tenv)
    }

  }

  def trExp(exp: Exp): ExpTy = position(exp) match {
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
          case EqOp()  | NeqOp() if tyl != UNIT() && !( tyl.isNil() && tyr.isNil() )
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

    case AssignExp(v, e, position) => {
      val vTy = trVar(v)
      val eTy = trExp(e)

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

      if ( testTy.ty != INT() || thenTy.ty != UNIT() ) {
        error("type error: if")
      }

      ExpTy((), UNIT())

    }

    case IfExp(test, then, Some(_else), position) => {
      val testTy = trExp(test)
      val thenTy = trExp(then)
      val elseTy = trExp(_else)

      if ( testTy.ty != INT() || thenTy.ty != elseTy.ty ) {
        error("type error: if")
      }

      ExpTy((), elseTy.ty)
    }

    case WhileExp(test, body, position) => {
      val testTy = trExp(test)
      val bodyTy = trExp(body)

      if (testTy.ty != INT() || bodyTy.ty != UNIT() )
        error("type error: while")

      ExpTy((), UNIT())

    }

    case ForExp(variable, escape, lo, hi, body, position) => {
      val loTy = trExp(lo)
      val hiTy = trExp(hi)

      if (loTy.ty != INT()) error("type error: for: lo must be int")
      if (hiTy.ty != INT()) error("type error: for: hi must be int")

      val bodyEnv = tabVars + (variable -> VarEntry(INT(ro = true)))

      val _ = Seman.transExp(bodyEnv, tabTypes, body)

      ExpTy((), UNIT())
    }

    case LetExp(decs, body, position) => {
      val (venv, tenv) = decs.foldLeft( (tabVars, tabTypes) ) (

        (x:(venv,tenv), dec:Dec ) => {
          val y = Seman.transDec(x._1, x._2, dec)

          (x._1 ++ y._1, x._2 ++ y._2)
        }

      )
      val letTy = Seman.transExp(venv, tenv, body)

      ExpTy((), letTy.ty)
    }

    case BreakExp(position) => ExpTy((), UNIT())

    case ArrayExp(typ, size, init, position) => {
      val initTy = trExp(init)
      val sizeTy = trExp(size)
      val arrayTy = unpack(tabTypes(typ))

      if ( sizeTy.ty != INT() )
        error("type error: array size should be Int")

      arrayTy match  {
        case ARRAY(ty) if ty == initTy.ty => true
        case ARRAY(x) => error("type error: found" + initTy.ty + "required" + x )
        case _ => error("type error: not an array")
      }

      ExpTy((), arrayTy)
    }
  }

  def error(msg: String) = {
    throw new TypeError(msg + " @line:"  + position() )
  }

}

object Seman extends Seman {

  val tabTypes:tenv = Map("int" -> INT(), "string" -> STRING()) withDefault error

  val tabVars:venv = Map(
    "print" -> FuncEntry(Env.mainLevel, "print", List(STRING()), UNIT(), extern = true),
    "flush" -> FuncEntry(Env.mainLevel, "flush", Nil, UNIT(), extern = true),
    "getchar" -> FuncEntry(Env.mainLevel, "getstr", Nil, STRING(), extern = true),
    "ord" -> FuncEntry(Env.mainLevel, "ord", List(STRING()), INT(), extern = true),
    "chr" -> FuncEntry(Env.mainLevel, "chr", List(INT()), STRING(), extern = true),
    "size" -> FuncEntry(Env.mainLevel, "size", List(STRING()), INT(), extern = true),
    "substring" -> FuncEntry(Env.mainLevel, "substring", List(STRING(), INT(), INT()), STRING(), extern = true),
    "concat" -> FuncEntry(Env.mainLevel, "substring", List(STRING(), STRING()), STRING(), extern = true),
    "not" -> FuncEntry(Env.mainLevel, "not", List(INT()), INT(), extern = true),
    "exit" -> FuncEntry(Env.mainLevel, "exit", List(INT()), UNIT(), extern = true)
  ) withDefault error


  def error(key: String) = {
    throw new EnvError("type error: not found " + key )
  }

  def transVar(venv: Env.venv, tenv: Env.tenv, vr: Var): ExpTy = {
    val trans = new Trans(venv, tenv)

    try {   // No se me ocurre como hacerlo mas feo.
      trans.trVar(vr)
    } catch {
      case e:EnvError => trans.error(e.getMessage)
    }

  }

  def transExp(venv: Env.venv, tenv: Env.tenv, exp: Exp): ExpTy = {
    val trans = new Trans(venv, tenv)

    try {   // No se me ocurre como hacerlo mas feo.
      return trans.trExp(exp)
    } catch {
      case e:EnvError => trans.error(e.getMessage)
    }

  }

  def transDec(venv: Env.venv, tenv: Env.tenv, dec: Dec): (Env.venv, Env.tenv) = {
    val trans = new Trans(venv, tenv)

    try {   // No se me ocurre como hacerlo mas feo.
      trans.trDec(venv, tenv,dec)
    } catch {
      case e:EnvError => trans.error(e.getMessage)
    }
  }

  def transProg(exp:Exp):ExpTy = transExp(tabVars,tabTypes,exp)


}
