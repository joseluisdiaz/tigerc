package tiger

import scala.collection.mutable.ArrayLike
import tiger.Tree._
import tiger.Tree.MEM
import tiger.Tree.CALL
import tiger.Tree.TEMP
import tiger.Tree.CONST
import tiger.Tree.BINOP

/**
 * |    argn        |  fp+4*(n+1)
 * |    ...         |
 * |    arg2        |  fp+16
 * |    arg1        |  fp+12
 * |  fp level(sl)  |  fp+8
 * |  retorno       |  fp+4
 * |   fp ant       |  fp
 * ------------------  fp
 * |   local1       |  fp-4
 * |   local2       |  fp-8
 * |    ...         |
 * |   localn       |  fp-4*n
 */

trait Frame {
  type Access = Frame.Access

  def name(): Temp.Label
  def formals(): List[Access]
  def allocLocal(esc: Boolean): Access
  def allocFormal(esc: Boolean): Access

  def procEntryExit1(body:Tree.Stm): Tree.Stm

  val calleeSave = List()
  val callerSave = List()

}

trait ArmConstants {
  val WS = 4 /* Word Size */
  val SL = 2*WS /* Static link location */

  val localOffsetInitial = WS
  val localIncrement = -1

  val argsOffsetInitial = 2*WS
  val argsIncrement = 1

  val regIncrement = 1
}

class ArmFrame(n: Temp.Label, f: List[Boolean]) extends Frame with ArmConstants {
  import tiger.Frame.{InReg, InFrame}

  var actualArg = 0
  var actualLocal = 0
  var actualReg = 0

  val _formals = scala.collection.mutable.ListBuffer.empty[Access]

  override def formals(): List[Access] = _formals.toList

  override def name(): Temp.Label = n

  override def allocLocal(esc: Boolean): Access = {
    if (esc) {
      val ret = InFrame((actualLocal * WS) - localOffsetInitial)
      actualLocal = actualLocal + localIncrement
      ret
    } else {
      actualReg += regIncrement
      InReg(Temp.newTemp())
    }
  }

  override def allocFormal(esc: Boolean): Access = {
    val formal = if (esc) {
      val ret = InFrame((actualArg * WS) + argsOffsetInitial)
      actualArg += argsIncrement
      ret
    } else {
      actualReg += regIncrement
      InReg(Temp.newTemp())
    }
    _formals += formal

    formal
  }

  override def procEntryExit1(body: Tree.Stm): Tree.Stm = body

  override def toString = s"[$n -> $hashCode() local: $actualLocal // args: $actualArg // reg: $actualReg]"

  f map allocFormal
}

object Frame extends ArmConstants {
  def apply(name: Temp.Label, formals: List[Boolean]) = new ArmFrame(name, formals)

  sealed class Frag
  case class PROC(body: Stm, frame: Frame) extends Frag
  case class STRING(label: Temp.Label, s: String) extends Frag

  /*
   * Da lo mismo cuale s la llamada en sí, estaría bueno reemplazarlo y tal vez hacer el calculo
   * de SL aca cuando *NO* sea una llamda a una función externa
   */
  def externalCall(name:String, args:Expr*):Tree.Expr = externalCall(name, args.toList)
  def externalCall(name:String, args:List[Expr]) = CALL(NAME(Temp.namedLabel(name)), args.toList)

  sealed abstract class Access {
    def exp(): Expr
  }

  case class InFrame(i: Int) extends Access {
    def exp() = MEM(BINOP(PLUS, TEMP(Frame.FP), CONST(i)))
  }

  case class InReg(l: Temp.Label) extends Access {
    def exp() = TEMP(l)
  }

  val FP = "FP"
  val RV = "RV"

}
