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

  val seman: Seman

  trait Seman {

    case class ExpTy(exp:
                     translate.Expression, ty: Types.Ty)

    def transProg(tree: Exp): ExpTy


  }

  class SemanImp extends Seman {

    def transVar(varsEnv: env.venv, typesEnv: env.tenv, level: translate.Level, variable: Var): ExpTy = variable match {

      case SimpleVar(id, _) => {
        val v = varsEnv(id) match {
          case v@env.VarEntry(_, _) => v
          case found@_ => error(s"variable expected: $found", variable)
        }

        ExpTy(translate.simpleVar(v.access, level), v.ty)
      }

      case SubscriptVar(lv, exp, _) => {
        val leftValue = transVar(varsEnv, typesEnv, level, lv)

        val arrayTy = unpack(leftValue.ty) match {
          case ARRAY(ty) => ty
          case found@_ => error(s"type error: found $found (ARRAY required)", variable)
        }

        val index = transExp(varsEnv, typesEnv, level, exp)

        if (index.ty != INT()) {
          error(s"type error: found ${index.ty} INT required", variable)
        }

        ExpTy(translate.subscriptVar(leftValue.exp, index.exp), arrayTy)
      }

      case FieldVar(leftValue, id, _) => {
        val recordExp = transVar(varsEnv, typesEnv, level, leftValue)

        val recordTy = unpack(recordExp.ty) match {
          case RECORD(records) => records
          case found@_ => error(s"type error: found $found RECORD required", variable)
        }

        val (ty, offset) = recordTy find {
          _._1 == id
        } match {
          case Some((_, x, y)) => (x, y) //Type
          case None => error(s"type error: field not found $id", variable)
        }

        ExpTy(translate.fieldVar(recordExp.exp, offset), ty)
      }


    }

    def transTy(typesEnv: env.tenv, ty: Abs.Ty): Types.Ty = ty match {
      case RecordTy(fields, _) => {

        val records = for {
          (f, i) <- fields.zipWithIndex
        } yield (f.name, transTy(typesEnv, f.ty), i)

        val idList = records.map(_._1)

        if (idList.size != idList.toSet.size) {
          error("type error: record contain duplicated fields", ty)
        }

        RECORD(records)
      }

      case NameTy(name, _) => ALIAS(name, None)
      case ArrayTy(name, _) => ARRAY(ALIAS(name, None))
    }

    def transExp(varsEnv: env.venv, typesEnv: env.tenv, level: translate.Level, exp: Exp): ExpTy = exp match {
      case VarExp(v, _) => transVar(varsEnv, typesEnv, level, v)

      case UnitExp(_) => ExpTy(translate.unitExp(), UNIT())
      case NilExp(_) => ExpTy(translate.nilExp(), NIL())
      case IntExp(value, _) => ExpTy(translate.intExp(value), INT())
      case StringExp(value, _) => ExpTy(translate.stringExp(value), STRING())

      case CallExp(func, args, position) => {

        val f = varsEnv(func) match {
          case func@env.FuncEntry(_, _, _, _, _) => func
          case found@_ => error(s"function expected: $found", exp)
        }

        if (args.size != f.params.size) {
          throw error("type error: wrong argument size", exp)
        }

        //                (↓)
        val evaluedArgs = for ((x, param) <- args zip f.params)
        yield (x, transExp(varsEnv, typesEnv, level, x), param)

        val argsExp = evaluedArgs map {
          case (param, ExpTy(paramExp, ty1), ty2) =>
            if (ty1 != ty2) {
              error(s"type error: param $param not matches $ty2 in function $func", exp)
            }
            paramExp
        }
        // Marche un merge(↑) \o/ :D
        val isProc = f.result == UNIT()

        println(s"callExp($func) - " + f.label)

        ExpTy(translate.callExp(f.label, argsExp, level, f.level, isProc, f.extern), f.result)
      }

      case OpExp(leftExp, oper, rightExp, position) => {
        val left = transExp(varsEnv, typesEnv, level, leftExp)
        val right = transExp(varsEnv, typesEnv, level, rightExp)
        val unpacked = unpack(left.ty)


        if (left.ty == right.ty) oper match {
          case EqOp | NeqOp if left.ty != UNIT() && !(left.ty.isNil && right.ty.isNil)
          => return ExpTy(translate.relOpExp(oper, left.ty, left.exp, right.exp), INT())

          case DivideOp | TimesOp | MinusOp | PlusOp if unpacked == INT()
          => return ExpTy(translate.binOpExp(oper, left.exp, right.exp), INT())

          case LtOp | LeOp | GtOp | GeOp if unpacked == INT() || unpacked == STRING()
          => return ExpTy(translate.relOpExp(oper, left.ty, left.exp, right.exp), INT())

          case _ => ()
        }

        error("type error", exp)
      }

      case RecordExp(fields, typ, position) =>
        val r = typesEnv(typ) match {
          case r@RECORD(_) => r
          case _ => error(s"type error: ${typ} not declared", exp)
        }

        if (r.records.size != fields.size) {
          throw error("type error: wrong argument size", exp)
        }

        val tfields = for ((sy, exp) <- fields) yield (sy, transExp(varsEnv, typesEnv, level, exp))

        val recordExps = for (
          ((sym1, ExpTy(e, ty1)), (sym2, ty2, _)) <- tfields zip r.records
        ) yield {
          if (sym1 != sym2) error(s"type error: $sym1 != $sym2", exp)
          if (ty1 != ty2) error(s"type error: $ty1 !=  $ty2", exp)
          e
        }

        ExpTy(translate.recordExp(recordExps), r)


      case ArrayExp(typ, size, init, position) => {
        val initTy = transExp(varsEnv, typesEnv, level, init)
        val sizeTy = transExp(varsEnv, typesEnv, level, size)
        val arrayTy = unpack(typesEnv(typ))

        if (sizeTy.ty != INT())
          error("type error: array size should be Int", exp)

        arrayTy match {
          case ARRAY(ty) if ty == initTy.ty => true
          case ARRAY(x) => error(s"type error: found ${initTy.ty} required $x", exp)
          case _ => error("type error: not an array", exp)
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
          case INT(true) if checkRo => error("type error: found read only integer in assign", exp)
          case _ => ()
        }

        if (vTr.ty != eTr.ty) {
          error(s"type error: found ${eTr.ty} required ${vTr.ty}", exp)
        }

        ExpTy(translate.assignExp(vTr.exp, eTr.exp), UNIT())

      }

      case IfExp(test, then, None, position) => {
        val testTr = transExp(varsEnv, typesEnv, level, test)
        val thenTr = transExp(varsEnv, typesEnv, level, then)

        if (testTr.ty != INT() || thenTr.ty != UNIT()) {
          error("type error: if", exp)
        }

        ExpTy(translate.ifThenExp(testTr.exp, thenTr.exp), UNIT())

      }

      case IfExp(test, then, Some(elsa), position) =>
        val testTr = transExp(varsEnv, typesEnv, level, test)
        val thenTr = transExp(varsEnv, typesEnv, level, then)
        val elsaTr = transExp(varsEnv, typesEnv, level, elsa)

        if (testTr.ty != INT() || thenTr.ty != elsaTr.ty) {
          error("type error: if", exp)
        }

        ExpTy(translate.ifThenElseExp(testTr.exp, thenTr.exp, elsaTr.exp), thenTr.ty)

      case WhileExp(test, body, position) =>
        level.preLoop()
        val testTr = transExp(varsEnv, typesEnv, level, test)
        val bodyTr = transExp(varsEnv, typesEnv, level, body)

        if (testTr.ty != INT() || bodyTr.ty != UNIT())
          error("type error: while", exp)

        val result = translate.whileExp(testTr.exp, bodyTr.exp, level)
        level.postLoop()

        ExpTy(result, UNIT())

      case ForExp(variable, escape, lo, hi, body, position) => {
        level.preLoop()
        val loTr = transExp(varsEnv, typesEnv, level, lo)
        val hiTr = transExp(varsEnv, typesEnv, level, hi)

        if (loTr.ty != INT()) error("type error: for: lo must be int", exp)
        if (hiTr.ty != INT()) error("type error: for: hi must be int", exp)

        val access = translate.allocLocal(level, escape)
        val varsEnv2 = (varsEnv + (variable -> env.VarEntry(access, INT.readOnly)))

        val bodyTr = transExp(varsEnv2, typesEnv, level, body)

        val varExp = transExp(varsEnv2, typesEnv, level, VarExp(SimpleVar(variable, position), position))

        val result = translate.forExp(loTr.exp, hiTr.exp, varExp.exp,bodyTr.exp, level)

        level.postLoop()

        ExpTy(result, UNIT())
      }

      case LetExp(decs, body, position) => {

        val (venv, tenv, lexp) = decs.foldLeft((varsEnv, typesEnv, List[translate.Expression]()))({
          case ((v, t, l), dec) =>
            val (nv, nt, nl) = transDec(v, t, level, dec)
            (v ++ nv, t ++ nt, l ++ nl)
        })

        val letTr = transExp(venv, tenv, level, body)

        ExpTy(translate.letExp(lexp, letTr.exp), letTr.ty)
      }

      case BreakExp(position) =>
        val result = translate.breakExp(level)

        if (result.isEmpty)
          error("break without a for or a while", exp)

        ExpTy(result.get, UNIT())

    }

    def transDec(varsEnv: env.venv, typesEnv: env.tenv, level: translate.Level, dec: Dec) = dec match {

      case VarDec(name, escape, ty, init, position) => {
        val initTr = transExp(varsEnv, typesEnv, level, init)

        // chequeo de tipo
        val varDec = ty match {
          // RECORD == NIL is handled on Types equality declaration
          case Some(x) if typesEnv(x) == initTr.ty => typesEnv(x)
          case None => initTr.ty
          case _ => NIL()
        }

        if (varDec.isNil) {
          error("type error: invalid declaration", dec)
        }

        // allocación

        val access = translate.allocLocal(level, escape)
        val exp = translate.varDec(access, level, initTr.exp)

        (varsEnv + (name -> env.VarEntry(access, varDec)), typesEnv, List(exp))
      }

      case TypeDecs(decs) =>

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

        if (envKeyProcOrder.isFailure) {
          error("type error: circular type", dec)
        }

        // create a pseudo-alias-free enviroment
        def unpack(typ: Types.Ty, tab: Env#tenv): Types.Ty = typ match {
          case ARRAY(tipo) => ARRAY(unpack(tipo, tab))
          case ALIAS(name, None) => tab(name)
          case x => x
        }

        val finalTypeEnv = envKeyProcOrder.get.foldLeft(__tenv)((acc: Env#tenv, x: String) => acc + (x -> unpack(acc(x), acc)))

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

      case FunctionDecs(decs) =>

        def transTy(ty: Abs.Ty): Types.Ty = ty match {
          case NameTy(name, _) => typesEnv(name)
          case _ => error("type error: shouln't happend", dec)
        }

        def functionTrans(f: FunctionDec) = {
          val params = f.params.map(x => transTy(x.ty))
          val result = f.result match {
            case None => UNIT()
            case Some(x) => typesEnv(x)
          }

          val fname = Temp.namedLabel(f.name)

          val newlevel = translate.newLevel(Some(level), fname)
          (f.name, env.FuncEntry(newlevel, fname, params, result, extern = false))
        }

        // Agrego todas las funciones a un nuevo entorno
        val functionsEnv = decs.map(functionTrans).toMap


        val venvWithFunction = varsEnv ++ functionsEnv

        def createParamsEnv(func: FunctionDec) = {
          val funcEntry = functionsEnv(func.name)

          def paramTrans(f: Field): (Abs.Symbol, env.VarEntry) = {
            def access = translate.allocFormal(funcEntry.level, f.escape)
            (f.name, env.VarEntry(access, transTy(f.ty)))
          }

          func.params map paramTrans
        }


        val functionsTr = decs.map { f =>
          val paramsEnv = venvWithFunction ++ createParamsEnv(f).toMap
          val funcEntry = functionsEnv(f.name)

          (f.name, funcEntry, transExp(paramsEnv, typesEnv, funcEntry.level, f.body))
        }

        // Valido tipos de retorno
        functionsTr foreach {
          case (name, fe, ExpTy(_, ty)) => if (fe.result != UNIT() && fe.result != ty)
            error(s"type error: function $name: expected ${fe.result} and returns $ty", dec)
        }

        // se construye la lista de inicializaciones
        val results = functionsTr map {
          case (_, fe, ExpTy(body, _)) => translate.functionDec(body, fe.level, fe.result == UNIT())
        }

        (venvWithFunction, typesEnv, results)
    }

    def error(msg: String, exp: Position) = {
      throw new TypeError(s"$msg @line ${exp.position}")
    }

    def error(msg: String) = {
      throw new TypeError(s"$msg")
    }

    def main(e: Exp) = LetExp(
      List(
        FunctionDecs(List(FunctionDec("_tigermain", List(), None, e, 0)))),
      UnitExp(0), 0)

    def transProg(exp: Exp): ExpTy = transExp(env.baseVenv withDefault error, env.baseTenv withDefault error, env.mainLevel, main(exp))

  }

}
