package tiger

import tiger.Abs._
import tiger.Types._


/**
 * User: jose
 * Date: 9/18/13
 * Time: 1:16 PM
 */


trait SemanComponent {
  this: TranslateComponent with EnvComponent =>

  class TypeError(message: String) extends RuntimeException(message)

  trait Seman {

    case class ExpTy(exp: Translate#Expression, ty: Types.Ty)

    def transProg(tree: Exp): ExpTy

  }

  class SemanImp extends Seman {

    //TODO: perhabs a state monad will help
    var currentPosition: Pos = 0

    def position() = currentPosition

    def position(p: Exp): Exp = {
      currentPosition = p.position;
      p
    }

    def position(p: Dec): Dec = {
      currentPosition = p.position;
      p
    }


    def transVar(varsEnv: Env#venv, typesEnv: Env#tenv, level: Translate#Level, variable: Var): ExpTy = variable match {

      case SimpleVar(id) => {
        val v = varsEnv(id) match {
          case v@env.VarEntry(_, _) => v
          case found@_ => error("variable expected: " + found)
        }

        ExpTy(translate.simpleVar(v.access, level), v.ty)
      }

      case SubscriptVar(lv, exp) => {
        val leftValue = transVar(varsEnv, typesEnv, level, lv)

        val arrayTy = unpack(leftValue.ty) match {
          case ARRAY(ty) => ty
          case found@_ => error(s"type error: found $found (ARRAY required)")
        }

        val index = transExp(varsEnv, typesEnv, level, exp)

        if (index.ty != INT()) {
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

        val (ty, offset) = recordTy find {
          _._1 == id
        } match {
          case Some((_, x, y)) => (x, y) //Type
          case None => error("type error: field not found " + id)
        }

        ExpTy(translate.fieldVar(recordExp.exp, offset), ty)
      }


    }

    def transTy(typesEnv: Env#tenv, ty: Abs.Ty): Types.Ty = ty match {
      case RecordTy(fields) => {

        val records = for {
          (f, i) <- fields.zipWithIndex
        } yield (f.name, transTy(typesEnv, f.ty), i)

        val idList = records.map(_._1)

        if (idList.size != idList.toSet.size) {
          error("type error: record contain duplicated fields")
        }

        RECORD(records)
      }

      case NameTy(name) => ALIAS(name, None)
      case ArrayTy(name) => ARRAY(ALIAS(name, None))
    }

    def transExp(varsEnv: Env#venv, typesEnv: Env#tenv, level: Translate#Level, exp: Exp): ExpTy = position(exp) match {
      case VarExp(v, position) => transVar(varsEnv, typesEnv, level, v)

      case UnitExp(position) => ExpTy(translate.unitExp(), UNIT)
      case NilExp(position) => ExpTy(translate.nilExp(), NIL)
      case IntExp(value, position) => ExpTy(translate.intExp(value), INT())
      case StringExp(value, position) => ExpTy(translate.stringExp(value), STRING)

      case CallExp(func, args, position) => {

        val f = varsEnv(func) match {
          case func@env.FuncEntry(_, _, _, _, _) => func
          case found@_ => error("function expected: " + found)
        }

        if (args.size != f.params.size) {
          throw error("type error: wrong argument size")
        }

        //                (↓)
        val evaluedArgs = for ((x, param) <- args zip f.params)
        yield (x, transExp(varsEnv, typesEnv, level, x), param)

        val argsExp = evaluedArgs map {
          case (param, ExpTy(paramExp, ty1), ty2) =>
            if (ty1 != ty2) {
              error(s"type error: param $param not matches $ty2 in function $func")
            }
            paramExp
        }
        // Marche un merge(↑) \o/ :D
        val isProc = f.result == UNIT

        ExpTy(translate.callExp(f.label, argsExp, f.level, isProc, f.extern), f.result)
      }

      case OpExp(leftExp, oper, rightExp, position) => {
        val left = transExp(varsEnv, typesEnv, level, leftExp)
        val right = transExp(varsEnv, typesEnv, level, rightExp)
        val unpacked = unpack(left.ty)


        if (left.ty == right.ty) oper match {
          case EqOp | NeqOp if left.ty != UNIT && !(left.ty.isNil && right.ty.isNil)
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
          case _ => error("type error: '" + typ + "' not declared")
        }

        if (r.records.size != fields.size) {
          throw error("type error: wrong argument size")
        }

        val tfields = for ((sy, exp) <- fields) yield (sy, transExp(varsEnv, typesEnv, level, exp))

        val recordExps = for (
          ((sym1, ExpTy(e, ty1)), (sym2, ty2, _)) <- tfields zip r.records
        ) yield {
          if (sym1 != sym2) error("type error: " + sym1 + "!=" + sym2)
          if (ty1 != ty2) error("type error: " + ty1 + "!=" + ty1)
          e
        }

        ExpTy(translate.recordExp(recordExps), r)
      }


      case ArrayExp(typ, size, init, position) => {
        val initTy = transExp(varsEnv, typesEnv, level, init)
        val sizeTy = transExp(varsEnv, typesEnv, level, size)
        val arrayTy = unpack(typesEnv(typ))

        if (sizeTy.ty != INT())
          error("type error: array size should be Int")

        arrayTy match {
          case ARRAY(ty) if ty == initTy.ty => true
          case ARRAY(x) => error("type error: found" + initTy.ty + "required" + x)
          case _ => error("type error: not an array")
        }

        ExpTy(translate.arrayExp(sizeTy.exp, initTy.exp), arrayTy)
      }

      case SeqExp(exps, position) => {

        val expTr = exps.map(transExp(varsEnv, typesEnv, level, _))

        val expression = translate.seqExp(expTr.map(_.exp))

        ExpTy(expression, expTr.last.ty)
      }

      case AssignExp(v, e, position, checkRo) => {
        val vTr = transVar(varsEnv, typesEnv, level, v)
        val eTr = transExp(varsEnv, typesEnv, level, e)

        // If there's a
        vTr.ty match {
          case INT(true) if checkRo => error("type error: found read only integer in assign")
          case _ => ()
        }

        if (vTr.ty != eTr.ty) {
          error("type error: found " + eTr.ty + " required " + vTr.ty)
        }

        ExpTy(translate.assignExp(vTr.exp, eTr.exp), UNIT)

      }

      case IfExp(test, then, None, position) => {
        val testTr = transExp(varsEnv, typesEnv, level, test)
        val thenTr = transExp(varsEnv, typesEnv, level, then)

        if (testTr.ty != INT() || thenTr.ty != UNIT) {
          error("type error: if")
        }

        ExpTy(translate.ifThenExp(testTr.exp, testTr.exp), UNIT)

      }

      case IfExp(test, then, Some(elsa), position) => {
        val testTr = transExp(varsEnv, typesEnv, level, test)
        val thenTr = transExp(varsEnv, typesEnv, level, then)
        val elsaTr = transExp(varsEnv, typesEnv, level, elsa)

        if (testTr.ty != INT() || thenTr.ty != elsaTr.ty) {
          error("type error: if")
        }

        ExpTy(translate.ifThenElseExp(testTr.exp, testTr.exp, elsaTr.exp), UNIT)
      }

      case WhileExp(test, body, position) => {
        level.preWhile()
        val testTr = transExp(varsEnv, typesEnv, level, test)
        val bodyTr = transExp(varsEnv, typesEnv, level, body)

        if (testTr.ty != INT() || bodyTr.ty != UNIT)
          error("type error: while")

        val exp = translate.whileExp(testTr.exp, bodyTr.exp, level)
        level.postWhile()

        ExpTy(exp, UNIT)

      }

      /*
       * ForExp => WhileExp
       *
       *                      =>        let var i:= lo
       * for i := lo to hi    =>            var limit := hi
       *   do body            =>        in while ( i <= limit )
       *                      =>          do (body; i := i + 1)
       *                      =>        end
       */
      case ForExp(variable, escape, lo, hi, body, position) => {
        val loTr = transExp(varsEnv, typesEnv, level, lo)
        val hiTr = transExp(varsEnv, typesEnv, level, hi)

        if (loTr.ty != INT()) error("type error: for: lo must be int")
        if (hiTr.ty != INT()) error("type error: for: hi must be int")

        val limit = "_limit" /* revisar con que caracter no pueden arrancar los identificadores) */

        val i = SimpleVar(variable)

        // i:= lo
        val varI = VarDec(variable, escape, Some("_int_ro"), lo, -1)

        // i:= hi
        val varLimit = VarDec(limit, escape = false, Some("_int_ro"), hi, -1)

        // i <= limit
        val test = OpExp(VarExp(i, -1), LeOp, VarExp(SimpleVar(limit), -1), -1)

        // i := i + 1 (omitiendo el checkeo de i como read only)
        val inc = AssignExp(i, OpExp(VarExp(i, -1), PlusOp, IntExp(1, -1), -1), -1, checkRO = false)
        /*
         * in while ( i <= limit )
         *   do (body; i := i + 1)
         */
        val whileAst = WhileExp(test, SeqExp(List(body, inc), -1), -1)

        // toda la transformación :D
        transExp(varsEnv, typesEnv, level, LetExp(List(varI, varLimit), whileAst, -1))
      }

      case LetExp(decs, body, position) => {

        val (venv, tenv, lexp) = decs.foldLeft((varsEnv, typesEnv, List[Translate#Expression]()))({
          case ((v, t, l), dec) =>
            val (nv, nt, nl) = transDec(v, t, level, dec)
            (v ++ nv, t ++ nt, l ++ nl)
        })

        val letTr = transExp(venv, tenv, level, body)

        ExpTy(translate.letExp(lexp, letTr.exp), letTr.ty)
      }

      case BreakExp(position) => {
        val result = translate.breakExp(level)

        if (result.isEmpty)
          error("break without a for or a while")

        ExpTy(result.get, UNIT)
      }

    }

    def transDec(varsEnv: Env#venv, typesEnv: Env#tenv, level: Translate#Level, dec: Dec): (Env#venv, Env#tenv, List[Translate#Expression]) = position(dec) match {

      case VarDec(name, escape, ty, init, position) => {
        val initTr = transExp(varsEnv, typesEnv, level, init)

        // chequeo de tipo
        val varDec = ty match {
          // RECORD == NIL is handled on Types equality declaration
          case Some(x) if typesEnv(x) == initTr.ty => initTr.ty
          case None => initTr.ty
          case _ => NIL
        }

        if (varDec.isNil) {
          error("type error: invalid declaration")
        }

        // allocación

        val access = translate.allocLocal(level, escape)
        val exp = translate.varDec(access, level, initTr.exp)

        (varsEnv + (name -> env.VarEntry(access, varDec)), typesEnv, List(exp))
      }

      case TypeDecs(decs) => {

        // create an enviroment with empty functions
        val voidtenv = decs.foldLeft(typesEnv)(
          (acc: Env#tenv, t: TypeDec) => acc + (t.name -> ALIAS(t.name, None))
        )

        // proccess all declaration
        val __tenv = decs.foldLeft(voidtenv)(
          (acc: Env#tenv, t: TypeDec) => acc + (t.name -> transTy(acc, t.ty))
        )

        // Generate dependency graph
        def genDependencyGraph(tab: List[(String, Types.Ty)]): List[(String, String)] = tab match {
          case Nil => Nil
          case (next, ALIAS(prev, None)) :: xs => (prev, next) :: genDependencyGraph(xs)
          case (next, ARRAY(x)) :: xs => genDependencyGraph(List((next, x))) ++ genDependencyGraph(xs)
          //  case (next, RECORD(r))::xs => genDependencyGraph(r.map( x => (x._1, x._2) )) ++ genDependencyGraph(xs)
          case x :: xs => genDependencyGraph(xs)
        }

        val envKeyProcOrder = Util.tsort(genDependencyGraph(__tenv.toList))

        // create a pseudo-alias-free enviroment
        def unpack(typ: Types.Ty, tab: Env#tenv): Types.Ty = typ match {
          case ARRAY(tipo) => ARRAY(unpack(tipo, tab))
          case ALIAS(name, None) => tab(name)
          case x => x
        }

        val finalTypeEnv = envKeyProcOrder.foldLeft(__tenv)((acc: Env#tenv, x: String) => acc + (x -> unpack(acc(x), acc)))

        def changeReferences(typ: Types.Ty, tab: Env#tenv): Unit = typ match {
          case RECORD(records) => records.map(x => changeReferences(x._2, tab))
          case ARRAY(tipo) => changeReferences(tipo, tab)
          case a@ALIAS(name, None) => a.ty = Some(tab(name))
          case _ => ()
        }

        val recordsKeySet = __tenv.keySet -- typesEnv.keySet //-- envKeyProcOrder
        //TODO: revisit substract envKeyProcOrder impact

        recordsKeySet.map(x => changeReferences(finalTypeEnv(x), finalTypeEnv))

        (varsEnv, finalTypeEnv, List())

      }

      case FunctionDecs(decs) => {

        def transTy(ty: Abs.Ty): Types.Ty = ty match {
          case NameTy(name) => typesEnv(name)
          case _ => error("type error: shouln't happend")
        }

        def functionTrans(f: FunctionDec) = {
          val params = f.params.map(x => transTy(x.ty))
          val result = f.result match {
            case None => UNIT
            case Some(x) => typesEnv(x)
          }

          val fname = Temp.namedLabel(f.name)

          val newlevel = translate.newLevel(Some(level), fname)
          (f.name, env.FuncEntry(newlevel, fname, params, result, extern = false))
        }

        // Agrego todas las funciones a un nuevo entorno
        val functionsEnv = decs.map(functionTrans).toMap
        val venvWithFunction = varsEnv ++ functionsEnv


        def createParamsEnv(f: FunctionDec) = {
          val funcEntry = functionsEnv(f.name)

          def paramTrans(f: Field) {
            def access = translate.allocLocal(funcEntry.level, f.escape)
            (f.name, env.VarEntry(access, transTy(f.ty)))
          }

          f.params map paramTrans
        }


        val functionsTr = decs.map(f => {
          val paramsEnv = venvWithFunction ++ createParamsEnv(f).toMap
          val funcEntry = functionsEnv(f.name)

          (f.name, funcEntry, transExp(paramsEnv, typesEnv, funcEntry.level, f.body))
        })

        // Valido tipos de retorno
        functionsTr foreach {
          case (name, fe, ExpTy(_, ty)) => if (fe.result != ty)
            error(s"type error: function $name: expected ${fe.result} and returns $ty")
        }

        // se construye la lista de inicializaciones
        val results = functionsTr map {
          case (_, fe, ExpTy(body, _)) => translate.functionDec(body, fe.level, fe.result == UNIT)
        }

        (venvWithFunction, typesEnv, results)
      }
    }

    def error(msg: String) = {
      throw new TypeError(s"$msg @line:" + position())
    }

    def transProg(exp: Exp): ExpTy = transExp(env.baseVenv, env.baseTenv, env.mainLevel, exp)

  }

}
