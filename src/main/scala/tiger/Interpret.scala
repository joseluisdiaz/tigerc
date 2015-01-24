package tiger

import scala.collection.mutable.HashMap
import tiger.Frame.Frag
import tiger.Frame.STRING
import tiger.Tree._


class Interpeter(procs:List[(List[Stm], Frame)], string:List[(Temp.Label, String)]) {

  type registers  = HashMap[Temp.Temp,Int]
  type memory = HashMap[Temp.Temp, Int]

  val mem = new HashMap[Int, Int]
  val reg = new HashMap[Temp.Temp, Int]

  var debug = false

  reg.put(Frame.FP, 10000000)
  reg.put(Frame.RV, 0)

  def loadTemp(t:Temp.Temp) = reg(t)
  def storeTemp(t:Temp.Temp, v:Int): Unit = reg.put(t,v)
  def restoreTemps(t:registers): Unit = { reg.clear(); reg ++= t }
  def saveTemps(): registers = reg.clone()

  def loadMem(addr:Int) = mem(addr)
  def storeMem(addr:Int, v:Int):Unit = mem.put(addr,v)

  var nextFree = 0

  def malloc(n:Int) = {
    val addr = nextFree
    nextFree += n*Frame.WS
    addr
  }


  val stringsMem = scala.collection.mutable.ListBuffer.empty[String]
  val stringTable = new HashMap[Temp.Label, Int]

  def loadLabel(l:Temp.Label) = stringTable(l)

  def storeString(str:String):Int = {
    val addr = malloc(1)
    storeMem(addr, stringsMem.length)
    stringsMem += str
    addr
  }
  def loadString(addr:Int):String = stringsMem(loadMem(addr))

  /* Guardado de strings */
  string map { str => stringTable.put(str._1,storeString(str._2)) }

  /* Definicion de librería */
  type lfunc = PartialFunction[List[Int], Int]

  /* Funciones de biblioteca */
  val allocArray:lfunc = { case (size::init::rest) =>
    val mem = malloc(size)

    if (debug) {
      println(s"=== allocArray size:$size init:$init === ")
      dump()
    }

    /* mem + 1 => array size */
    val l = (mem+1, size) :: List.tabulate(size) { x:Int => ( mem + Frame.WS*x, init) }

    l foreach (storeMem _).tupled

    mem
  }

  val checkIndexArray:lfunc = { case (arr::idx::rest) =>
    val size = loadMem(arr+1)

    if (idx < 0 || idx >= size)  error("Índice fuera de rango\n");

    0
  }

  val allocRecord:lfunc = { case size::vals =>
    val mem = malloc(size)
    val addrs = List.tabulate(size) { x => mem + x*Frame.WS }

    (addrs zip vals) foreach (storeMem _).tupled

    mem
  }

  val checkNil:lfunc = { case(r::rest) => if (r == 0) error("Nil") else 0 }

  val stringCompare:lfunc = { case (strPtr1::strPtr2::rest) =>

    val str1 = loadString(strPtr1)
    val str2 = loadString(strPtr2)

    str1.compareTo(str2)
  }

  val printFun:lfunc = { case(strPtr::rest) =>
    val str = loadString(strPtr)
    print(str)

    0
  }


  val chrFun:lfunc = { case value::rest  =>
    storeString(value.toChar.toString)
  }

  val ordFun:lfunc = { case strPtr::rest  =>
    val str = loadString(strPtr)
    str(0).toInt
  }

  def dump(): Unit = {
    println("==== reg ====")
    reg.foreach { case (k,v) => println(s"$k \t\t\t $v") }

    println("==== mem ====")
    mem.foreach { case (k,v) => println(s"$k \t\t\t $v") }
  }

  val library = Map[Temp.Label, lfunc](
    "_allocArray" -> allocArray,
    "_checkIndexArray" -> checkIndexArray,
    "_allocRecord" -> allocRecord,
    "_checkNil" -> checkNil,
    "_stringCompare" -> stringCompare,
    "print" -> printFun,
    "chr" -> chrFun,
    "ord" -> ordFun
  )

  def getFrag(f:Temp.Label) = procs find { case (_, frame) => frame.name == f }

  def evalLocalFunc(f:Temp.Label)(args: List[Int]): Int = {

    val (body, frame) = getFrag(f) getOrElse { error(s"Fragmento no encontrado $f") }

    def find(s:Temp.Label): List[Stm] = body.dropWhile({
      case LABEL(l) => l != s
      case _ => true
    })

    def execute(prog: List[Stm]):Unit = prog match {
      case Nil => Unit
      case x::xs => {
        if (debug) println(s"==>$x")
        evalStm(x) match {
          case None => execute(xs)
          case Some(l) => execute(find(l))
        }
      }
    }

    if (debug) println("==== call ====")
    /* Guardar temporarios */
    val prevRegs = saveTemps()

    /* Mover fp lo suficiente */
    val fpPrev = loadTemp(Frame.FP)

    storeTemp(Frame.FP, fpPrev-1024*1024)

    /* Poner argumentos donde la función los espera */
    val formals = frame.formals.map(_.exp(Frame.FP))

    val formalsValues = formals zip args

    formalsValues map {
      case (TEMP(t),y) => storeTemp(t,y)
      case (MEM(m), y) => storeMem(evalExp(m), y)
    }

    if (debug) dump()

    /* Ejecutar la lista de instrucciones */
    execute(body)

    val rv = loadTemp(Frame.RV)
    /* Restaurar temporarios */
    restoreTemps(prevRegs)


    if (debug) println(s"==== return $rv end call ====")

    rv
  }

  def evalFunc(f:Temp.Label, args: List[Int]): Int = library.applyOrElse(f, evalLocalFunc)(args)

  def evalExp(e:Expr):Int = e match {
    case CONST(c) => c
    case TEMP(t) => loadTemp(t)

    case NAME(l) => loadLabel(l)

    case BINOP(op, e1, e2) => {
      val ee1 = evalExp(e1)
      val ee2 = evalExp(e2)

      op match {
        case PLUS => ee1 + ee2
        case MINUS => ee1 - ee2
        case MUL => ee1 * ee2
        case DIV => ee1 / ee2
        case _ => ???
      }

    }

    case MEM(e) => {
      val ee = evalExp(e)
      loadMem(ee)
    }

    case CALL(NAME(f), args) => {
      val eargs = args map evalExp
      val rv = evalFunc(f,eargs)
      storeTemp(Frame.RV, rv)
      rv
    }

    case _ => ???
  }

  def evalStm(a:Stm):Option[Temp.Label] = a match {
    case MOVE(TEMP(t), e) => storeTemp(t,evalExp(e)) ; None
    case MOVE(MEM(e1), e2) => storeMem(evalExp(e1),evalExp(e2)); None
    case MOVE(_, _) => ???

    case EXP(e) => evalExp(e); None

    case JUMP(NAME(e), ls) => Some(e)
    case JUMP(_,_) => ???

    case CJUMP(rop, e1, e2, lt, lf) => {
      val ee1 = evalExp(e1)
      val ee2 = evalExp(e2)

      val b = rop match {
        case EQ => ee1 == ee2
	case NE => ee1 != ee2
	case LT => ee1 < ee2
	case GT => ee1 > ee2
	case LE => ee1 <= ee2
	case GE => ee1 >= ee2
        case _ => ???
      }
      if (b) Some(lt) else Some(lf)
    }

    case SEQ(_,_) => ???

    case LABEL(_) => None
  }


}
