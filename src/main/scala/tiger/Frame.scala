package tiger

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

trait Constants {

  val calleeSave:List[Temp.Temp]
  val callerSave:List[Temp.Temp]
  val argsRegisters:List[Temp.Temp]
  val registers: List[Temp.Temp]

  val PC:Temp.Temp
  val FP:Temp.Temp
  val RV:Temp.Temp
  val SP:Temp.Temp
}

trait Frame extends Constants {

  type Access = Frame.Access

  def name: Temp.Label
  def formals(): List[Access]
  def allocLocal(esc: Boolean): Access
  def allocFormal(esc: Boolean): Access
  def offset(a:Access): Option[Int]

  def procEntryExit1(body:Tree.Stm): Tree.Stm
  def procEntryExit2(instructions: List[Asm.Instr]): List[Asm.Instr]
  def procEntryExit3(instructions: List[Asm.Instr]): (String, List[Asm.Instr], String)

}


trait ArmConstants extends Constants {

  case class R(r: String)

  implicit def regToTemp(x:R):Temp.Temp = Temp.namedTemp(x.r)

  val WS = 4 /* Word Size */
  val SL = 2*WS /* Static link location */

  val localOffsetInitial = WS
  val localIncrement = -1

  val argsOffsetInitial = 2*WS
  val argsIncrement = 1

  val regIncrement = 1

  override val argsRegisters = List("r0", "r1", "r2", "r3")

  override val PC = "pc"
  override val FP = "fp"
  override val RV = "r0"
  override val SP = "sp"

  override val calleeSave = List("r4", "r5", "r6", "r7", "r8" )
  override val callerSave = List()
  override val registers = List("r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", "r11", FP, SP, PC)

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

  override def procEntryExit1(body: Tree.Stm): Tree.Stm = body  //nombre de la función
  override def procEntryExit2(instructions: List[Asm.Instr]): List[Asm.Instr] = instructions // acomodar el stack/frame pointer
  override def procEntryExit3(instructions: List[Asm.Instr]): (String, List[Asm.Instr], String) = ( "; prologo\n", instructions, "; epilogo\n")

  /*
   * Una instrucción trucha al principio por cada registro callee save, defini'endolo, y otra instr. al final, us'andolos.
   */
  override def toString = s"[$n -> local: $actualLocal // args: $actualArg // reg: $actualReg]"

  override def offset(a: Access): Option[Int] = a match {
    case InFrame(i) => Some(i)
    case InReg(l) => None
  }


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

}
