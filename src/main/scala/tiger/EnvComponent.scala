package tiger

import tiger.Types.{INT, STRING, Ty, UNIT}

trait EnvComponent {

  this: TranslateComponent =>

  val env: Env

  class Env {

    //  val env = new Env() with TranslateComponent

    sealed abstract class EnvEntry

    case class VarEntry(access: translate.Access, ty: Ty) extends EnvEntry

    case class FuncEntry(level: translate.Level,
                         label: Temp.Label,
                         params: List[Ty],
                         result: Ty,
                         extern: Boolean) extends EnvEntry


    type venv = Map[String, EnvEntry]
    type tenv = Map[String, Ty]

    class EnvError(message: String) extends RuntimeException(message)

    val mainLevel = translate.newLevel(None, Temp.namedLabel("mainLevel"))

    def error(key: String) = {
      throw new EnvError("type error: not found " + key)
    }

    val baseTenv: tenv = Map("int" -> INT(), "_int_ro" -> INT.readOnly(), "string" -> STRING()) withDefault error

    val baseVenv: venv = Map(
      "print" -> FuncEntry(mainLevel, "print", List(STRING()), UNIT(), extern = true),
      "flush" -> FuncEntry(mainLevel, "flush", Nil, UNIT(), extern = true),
      "getchar" -> FuncEntry(mainLevel, "getstr", Nil, STRING(), extern = true),
      "ord" -> FuncEntry(mainLevel, "ord", List(STRING()), INT(), extern = true),
      "chr" -> FuncEntry(mainLevel, "chr", List(INT()), STRING(), extern = true),
      "size" -> FuncEntry(mainLevel, "size", List(STRING()), INT(), extern = true),
      "substring" -> FuncEntry(mainLevel, "substring", List(STRING(), INT(), INT()), STRING(), extern = true),
      "concat" -> FuncEntry(mainLevel, "substring", List(STRING(), STRING()), STRING(), extern = true),
      "not" -> FuncEntry(mainLevel, "not", List(INT()), INT(), extern = true),
      "exit" -> FuncEntry(mainLevel, "exit", List(INT()), UNIT(), extern = true)
    ) withDefault error



    val _baseFrames:Map[String, Frame] = baseVenv.flatMap {
      case (_, FuncEntry(_, name, formals, _, _)) => Some(name -> Frame(name, formals map (_ => false)))
      case _ => None
    }

    val _innerFunctions = Map("_allocArray" -> Frame("_allocArray", List(false, false)),
      "_checkIndexArray" -> Frame("_checkIndexArray", List(false, false)),
      "_allocRecord" -> Frame("_allocRecord", List(false, false)),
      "_checkNil" -> Frame("_checkNil", List(false)),
      "_stringCompare" -> Frame("_stringCompare", List(false)))


    val baseFrames = _baseFrames ++ _innerFunctions

  }

}
