package tiger

import tiger.Abs._
import tiger.Abs.SimpleVar
import scala.collection.immutable.Nil

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 9/11/13
 * Time: 12:52 PM
 */

trait Escapes {
  type Depth = Int
  type EscEnv = Map[Symbol, (Depth, BooleanRef)]

  def findEscapes(prog: Exp):Unit

}

object Escapes {

  def findEscapes(prog: Exp) = {
    new DefaultEscapes().findEscapes(prog)
  }

}

class DefaultEscapes extends Escapes {

  private def travVar(v: Var, env: EscEnv, d: Depth): Unit = v match {

    case SimpleVar(id) => env.get(id) match {
      case Some((dd, ref)) if d > dd => ref.set(true)
      case Some(_) => ()
      case None => throw new Error("Escape??" + id + "not exist!")
    }

    case FieldVar(leftValue, id) => travVar(v, env, d)
    case SubscriptVar(leftValue, exp) => travVar(leftValue, env, d); travExp(exp, env, d)
  }

  private def travExp(e: Exp, env: EscEnv, d: Depth): Unit = e match {
    case VarExp(variable, _) => travVar(variable, env, d)

    case CallExp(func, args, position) => travExp(SeqExp(args, position), env, d)

    case OpExp(left, oper, right, position) => travExp(left, env, d); travExp(right, env, d)

    case SeqExp(exps, _) => exps.map(travExp(_, env, d))

    case AssignExp(variable, exp, position) =>
      travVar(variable, env, d); travExp(exp, env, d)

    /* es mas claro separando de esta manera que utilizando un
      case para el Option que desarmandolo a este nivel.
     */
    case IfExp(test, then, None, position) =>
      travExp(test, env, d); travExp(then, env, d)

    case IfExp(test, then, Some(_else), position) =>
      travExp(test, env, d); travExp(then, env, d); travExp(_else, env, d)

    case WhileExp(test, body, _) => travExp(test, env, d); travExp(body, env, d)

    case forexp@ForExp(variable, escape, lo, hi, body, position) =>
      travExp(lo, env, d); travExp(hi, env, d)
      travExp(body, env + (variable -> (d, forexp)), d)

    case LetExp(decs, body, position) => travExp(body, travDecs(decs, env, d), d)

    case ArrayExp(typ, size, init, position) => travExp(init, env, d)

    case _ => ()

  }


  private def travDec(declaration: Dec, env: EscEnv, d: Depth): EscEnv ={

    def handleFuntionDec(dec:FunctionDec):Unit = {
      val paramsEnv = dec.params.foldRight(env)((f:Field, e :EscEnv) => e + (f.name -> (d + 1, f) ))
      travExp(dec.body, paramsEnv, d + 1)
    }

    declaration match {
      case varDec@VarDec(name, escape, typ, init, position) => env + ( name -> (d, varDec))
      case FunctionDecs(decs) => decs.map(handleFuntionDec);env
      case TypeDecs(decs) => env
    }
  }

  def travDecs(decs: List[Dec], env: EscEnv, d: Depth): EscEnv = decs match {
    case hd :: tl => travDecs(tl, travDec(hd, env, d), d)
    case Nil => env
  }

  def findEscapes(prog: Exp) = travExp(prog, Map(), 0)

}
