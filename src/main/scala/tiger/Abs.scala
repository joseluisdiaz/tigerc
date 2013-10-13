package tiger

object Abs {
  case class Pos(p:Int)

  type Symbol = String

  trait BooleanRef {
    def set(bool:Boolean):Unit
  }

  /* Variables */
  sealed abstract class Var
  case class SimpleVar(id:Symbol) extends Var
  case class FieldVar(leftValue: Var, id:Symbol) extends Var
  case class SubscriptVar(leftValue: Var, exp: Exp) extends Var


  trait Position {
    def position: Pos
  }

  trait NullPosition extends Position {
    def position: Abs.Pos = Pos(0)
  }


  /* Expresiones */
  sealed abstract class Exp extends Position

  case class VarExp(variable: Var, position: Pos) extends Exp
  case class UnitExp(position: Pos) extends Exp
  case class NilExp(position: Pos) extends Exp
  case class IntExp(value: Integer, position: Pos) extends Exp
  case class StringExp(value: String, position: Pos) extends Exp
  case class CallExp(func: Symbol, args: List[Exp], position: Pos) extends Exp
  case class OpExp (left: Exp, oper: Oper, right: Exp, position: Pos) extends Exp
  case class RecordExp(fields: List[(Symbol, Exp)], typ: Symbol, position: Pos) extends Exp
  case class SeqExp(exps:List[Exp], position:Pos) extends Exp
  case class AssignExp(variable: Var, exp: Exp, position: Pos) extends Exp
  case class IfExp(test: Exp, then: Exp, _else: Option[Exp], position: Pos) extends Exp
  case class WhileExp(test: Exp, body: Exp, position: Pos) extends Exp

  /*
   * podria haber usado una "RefBoolean" y mutar el estado de escape como tal
   * y no de la referencia en la ForExp.
   * Me parecio mas interesante utilizar un trait que fuera "el aspecto" a modificar.
   * para hacerlo mas explicito.
   */
  case class ForExp(variable: Symbol, var escape: Boolean,
                    lo: Exp, hi: Exp, body: Exp, position: Pos) extends Exp with BooleanRef {

    def set(bool: Boolean) = { escape = bool }
  }

  case class LetExp(decs: List[Dec], body: Exp, position: Pos) extends Exp
  case class BreakExp(position: Pos) extends Exp
  case class ArrayExp(typ: Symbol, size: Exp, init: Exp, position: Pos) extends Exp

  /* declaraciones */
  sealed abstract class Dec extends Position

  case class VarDec(name: Symbol, var escape: Boolean, typ: Option[Symbol],
                    init: Exp, position: Pos) extends Dec with BooleanRef{

    def set(bool: Boolean) { escape = bool }
  }

  case class TypeDec(name: Symbol, ty: Ty, position: Pos)

  case class TypeDecs(decs: List[TypeDec]) extends Dec with NullPosition

  case class FunctionDec(name: Symbol, params: List[Field], result: Option[Symbol], 
                         body: Exp, Position: Pos)
  case class FunctionDecs(decs: List[FunctionDec]) extends Dec with NullPosition

  /* tipos */
  sealed abstract class Ty
  case class NameTy(name: Symbol) extends Ty
  case class RecordTy(fields: List[Field]) extends Ty
  case class ArrayTy(name: Symbol) extends Ty

  case class Field(name: Symbol, var escape: Boolean, ty: Ty) extends BooleanRef {
    def set(bool: Boolean) { escape = bool }
  }

  /* operadores */
  sealed abstract class Oper
  case class PlusOp() extends Oper
  case class MinusOp() extends Oper
  case class TimesOp() extends Oper
  case class DivideOp() extends Oper
  case class EqOp() extends Oper
  case class NeqOp() extends Oper
  case class LtOp() extends Oper
  case class LeOp() extends Oper
  case class GtOp() extends Oper
  case class GeOp() extends Oper


}

