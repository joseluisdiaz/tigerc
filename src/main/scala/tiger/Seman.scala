package tiger

import tiger.Types._
import tiger.Env._
import scala.Predef._
import tiger.Abs._
import scala.Some
import tiger.Types.RECORD
import tiger.Env.FuncEntry
import tiger.Env.VarEntry


/**
 * User: jose
 * Date: 9/18/13
 * Time: 1:16 PM
 */

class TypeError(message: String) extends RuntimeException(message)

trait Seman {

  case class ExpTy(exp: translate.Expression, ty: Types.Ty)

  val translate: Translate

  def transProg(tree: Exp): ExpTy

}

object Seman extends Seman {

  //TODO: perhabs a state monad will help
  var currentPosition:Pos = 0

  def position() = currentPosition
  def position(p:Exp):Exp = { currentPosition = p.position;p }
  def position(p:Dec):Dec = { currentPosition = p.position;p }


  def transVar(varsEnv:venv, typesEnv:tenv, level:translate.Level, variable: Var): ExpTy = variable match {

    case SimpleVar(id) => {
      val (access, varTy) = varsEnv(id) match {
        case v@VarEntry(_, _) => v
        case found@_ => error("variable expected: " + found)
      }

      ExpTy(translate.simpleVar(access, level), varTy)
    }

    case SubscriptVar(lv, exp) => {
      val leftValue = transVar(varsEnv, typesEnv, level, lv)

      val arrayTy = unpack(leftValue.ty) match {
        case ARRAY(ty) => ty
        case found@_ => error("type error: found " + found + " ARRAY" + " required")
      }

      val index = transExp(varsEnv, typesEnv, level, exp)

      if ( index.ty != INT() ) {
        error("type error: found " + index.ty + " INT" + " required")
      }

      ExpTy(translate.subscriptVar(leftValue.exp, index.exp), arrayTy)
    }

    case FieldVar(leftValue, id) => {
      val recordExp = transVar(varsEnv, typesEnv, level, leftValue)

      val recordTy = recordExp.ty match {
        case RECORD(records) => records
        case found@_ => error("type error: found " + found + " RECORD" + " required")
      }

      val (ty, offset) = recordTy find { _._1 == id } match {
        case Some( (_,x,y) ) => (x,y) //Type
        case None => error("type error: field not found " + id)
      }

      ExpTy(translate.fieldVar(recordExp.exp, offset), ty)
    }


  }

  def transTy(typesEnv: tenv,ty:Abs.Ty): Types.Ty = ty match {
    case RecordTy(fields) => {

      val records = for {
        (f,i) <- fields.zipWithIndex
      } yield (f.name, transTy(typesEnv, f.ty),i)

      val idList = records.map( _._1)

      if (idList.size != idList.toSet.size) {
        error("type error: record contain duplicated fields")
      }

      RECORD (records)
    }

    case NameTy(name) => ALIAS(name,None)
    case ArrayTy(name) => ARRAY(ALIAS(name, None))
  }

  def transExp(varsEnv:venv, typesEnv:tenv, level:translate.Level, exp: Exp): ExpTy = position(exp) match {
    case VarExp(v, position) => transVar(varsEnv, typesEnv, level, v)

    case UnitExp(position) => ExpTy(translate.unitExp(), UNIT)
    case NilExp(position) => ExpTy(translate.nilExp(), NIL)
    case IntExp(value, position) => ExpTy(translate.intExp(value), INT())
    case StringExp(value, position) => ExpTy(translate.stringExp(value), STRING)

    case CallExp(func, args, position) => {
      val fdef = varsEnv(func) match {
        case func@FuncEntry(_, _, _, _, _) => func
        case found@_ => error("function expected: " + found)
      }

      if (args.size != fdef.params.size) {
        throw error("type error: wrong argument size")
      }

      val evaluedArgs = for ( x <- args ) yield (x, transExp(varsEnv,typesEnv, x))

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

    case OpExp(leftExp, oper, rightExp, position) => {
      val left = transExp(varsEnv, typesEnv, level, leftExp)
      val right = transExp(varsEnv,typesEnv, level, rightExp)
      val unpacked = unpack(left.ty)


      if ( left.ty == right.ty ) oper match {
          case EqOp  | NeqOp if left.ty != UNIT && !( left.ty.isNil && right.ty.isNil )
            => return ExpTy(translate.relOpExp(oper, left.ty, left.exp, right.exp), INT())

          case DivideOp | TimesOp | MinusOp | PlusOp if unpacked == INT()
            => return ExpTy(translate.binOpExp(oper, left.exp, right.exp), INT())

          case LtOp | LeOp | GtOp | GeOp if unpacked == INT() || unpacked == STRING
            => return ExpTy(translate.relOpExp(oper, left.ty, left.exp, right.exp), INT())

          case _ => ()
       }

      error("type error")
    }

    case RecordExp(fields, typ, position) => {
      val r = typesEnv(typ) match {
        case r@RECORD(_) => r
        case _ =>  error("type error: '" + typ + "' not declared")
      }

      if ( r.records.size != fields.size ) {
        throw error("type error: wrong argument size")
      }

      val tfields = for ( (sy, exp) <- fields ) yield (sy, transExp(varsEnv, typesEnv,level, exp))

      val recordExps = for (
        ( (sym1, ExpTy(e, ty1)), (sym2, ty2, _)  ) <- tfields zip r.records
      ) yield {
        if ( sym1 != sym2) error("type error: " + sym1 + "!=" + sym2)
        if ( ty1 != ty2 ) error("type error: " + ty1 + "!=" + ty1)
        e
      }

      ExpTy(translate.recordExp(recordExps) , r)
    }


    case ArrayExp(typ, size, init, position) => {
      val initTy = transExp(varsEnv, typesEnv, level, init)
      val sizeTy = transExp(varsEnv, typesEnv, level, size)
      val arrayTy = unpack(typesEnv(typ))

      if ( sizeTy.ty != INT() )
        error("type error: array size should be Int")

      arrayTy match  {
        case ARRAY(ty) if ty == initTy.ty => true
        case ARRAY(x) => error("type error: found" + initTy.ty + "required" + x )
        case _ => error("type error: not an array")
      }

      ExpTy(translate.arrayExp(sizeTy.exp, initTy.exp), arrayTy)
    }

    case SeqExp(exps, position) => {

      val expTr = exps.map(transExp(varsEnv, typesEnv, level, _))

      val expression = translate.seqExp(expTr.map(_.exp))

     ExpTy(expression, expTr.last.ty)
    }

    case AssignExp(v, e, position) => {
      val vTr = transVar(varsEnv, typesEnv, level, v)
      val eTr = transExp(varsEnv, typesEnv, level, e)

      vTr.ty match {
        case INT(true) => error("type error: found read only integer in assign")
        case _ => ()
      }

      if (vTr.ty != eTr.ty) {
        error("type error: found " + eTr.ty + " required " + vTr.ty )
      }

      ExpTy(translate.assignExp(vTr.exp, eTr.exp), UNIT)

    }

    case IfExp(test, then, None, position) => {
      val testTy = transExp(varsEnv, typesEnv, test)
      val thenTy = transExp(varsEnv, typesEnv, then)

      if ( testTy.ty != INT() || thenTy.ty != UNIT() ) {
        error("type error: if")
      }

      ExpTy((), UNIT())
    }

    case IfExp(test, then, Some(_else), position) => {
      val testTy = transExp(variableEnv, typesEnv, test)
      val thenTy = transExp(variableEnv, typesEnv, then)
      val elseTy = transExp(variableEnv, typesEnv, _else)

      if ( testTy.ty != INT() || thenTy.ty != elseTy.ty ) {
        error("type error: if")
      }

      ExpTy((), elseTy.ty)
    }

    case WhileExp(test, body, position) => {
      translate.preWhile()
      val testTr = transExp(varsEnv, typesEnv, level, test)
      val bodyTr = transExp(varsEnv, typesEnv, level, body)

      if (testTr.ty != INT() || bodyTr.ty != UNIT )
        error("type error: while")

      val exp = translate.whileExp(testTr.exp, bodyTr.exp)

      translate.postWhile()

      ExpTy(exp, UNIT)

    }

    case ForExp(variable, escape, lo, hi, body, position) => {
      val loTy = transExp(varsEnv, typesEnv, lo)
      val hiTy = transExp(varsEnv, typesEnv, hi)

      if (loTy.ty != INT()) error("type error: for: lo must be int")
      if (hiTy.ty != INT()) error("type error: for: hi must be int")

      val bodyEnv = varsEnv + (variable -> VarEntry(INT(readOnly = true)))

      val _ = transExp(bodyEnv, typesEnv, body)

      ExpTy((), UNIT())
    }

    case LetExp(decs, body, position) => {
      val (venv, tenv) = decs.foldLeft( (varsEnv, typesEnv) ) (

        (x:(venv,tenv), dec:Dec ) => {
          val (newVenv, newTenv) = transDec(x._1, x._2, level, dec)

          (x._1 ++ newVenv, x._2 ++ newTenv)
        }

      )
      val letTy = transExp(venv, tenv, level, body)

      ExpTy((), letTy.ty)
    }

    case BreakExp(position) => ExpTy((), UNIT())

  }

  def transDec(varsEnv:venv, typesEnv:tenv, level:translate.Level, dec: Dec):(venv, tenv)= position(dec) match {

    case VarDec(name, escape, None, init, position) => {
      val initTy = transExp(varsEnv, typesEnv, level, init)

      if (initTy.ty.isNil)
        error("type error: invalid declaration")

      ( varsEnv + (name -> VarEntry( initTy.ty)), typesEnv)
    }

    case VarDec(name, escape, Some(ty), init, position) => {
      val initTy = transExp(varsEnv, typesEnv, level, init)
      val varDecTy = typesEnv(ty)

      // RECORD == NIL is handled on Types equality declaration
      if (initTy.ty != varDecTy) {
        error("type error: invalid declaration")
      }

      ( varsEnv + (name -> VarEntry(varDecTy)), typesEnv )
    }

    case TypeDecs(decs) => {

      // create an enviroment with empty functions
      val voidtenv = decs.foldLeft(typesEnv)(
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

      val recordsKeySet = __tenv.keySet -- typesEnv.keySet //-- envKeyProcOrder
      //TODO: revisit substract envKeyProcOrder impact

      recordsKeySet.map( x => changeReferences(finalTypeEnv(x), finalTypeEnv) )

      (varsEnv, finalTypeEnv)
    }

    case FunctionDecs(decs) => {

      def transTy(ty:Abs.Ty):Types.Ty = ty match {
        case NameTy(name) => typesEnv(name)
        case _ => error("type error: shouln't happend")
      }

      def functionTrans(f:FunctionDec) = {
        val params = f.params.map( x => transTy(x.ty))
        val result = f.result match {
          case None => UNIT()
          case Some(x) => typesEnv(x)
        }

        val fname = Temp.namedLabel(f.name)
        val escape = f.params map ( x => x.escape )

        val level = translate.newLevel(translate.outermost(), fname, escape)

        FuncEntry(level, fname, params, result, extern = false)
      }

      val venvWithFunction = decs.foldLeft(varsEnv)((x:venv, f:FunctionDec) =>  x + (f.name -> functionTrans(f)))

      def addToVEnv(params:List[Field]) =
        params.foldLeft(venvWithFunction)((x:venv, arg:Field) =>  x + (arg.name -> VarEntry(transTy(arg.ty))))

      // Validate return values
      val functionsTy = decs.map( x => (x.name, transExp(addToVEnv(x.params),typesEnv,x.body).ty ))

      functionsTy.foreach({
        case (x, y) => venvWithFunction(x) match {
          case VarEntry(_, _) => false
          case FuncEntry(_, _, _, result, _) => if (result != y)
            error("return type must be: "+ result+ " found " +y)
        }
      })

      (venvWithFunction, typesEnv)
    }
  }

  def error(msg: String) = {
    throw new TypeError(msg + " @line:"  + position() )
  }

  def transProg(exp:Exp):ExpTy = transExp(Env.baseVenv,Env.baseTenv, translate.outermost(), exp)

  override val translate: Translate = new MyTranslate()

}

