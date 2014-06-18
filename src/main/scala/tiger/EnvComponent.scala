package tiger

import tiger.Types.{INT, STRING, Ty, UNIT}

trait EnvComponent {

  this: TranslateComponent =>

  val env: Env

  class Env {

    //  val env = new Env() with TranslateComponent

    sealed abstract class EnvEntry

    case class VarEntry(access: Translate#Access, ty: Ty) extends EnvEntry

    case class FuncEntry(level: Translate#Level,
                         label: Temp.Label,
                         params: List[Ty],
                         result: Ty,
                         extern: Boolean) extends EnvEntry


    type venv = Map[String, EnvEntry]
    type tenv = Map[String, Ty]

    class EnvError(message: String) extends RuntimeException(message)

    val mainLevel = translate.newLevel(None, Temp.namedLabel("mainLevel"))


    //  case class Enviroment(vars:venv, types:tenv) {
    //
    //    def ++(e:Enviroment) = Enviroment(this.vars ++ e.vars, this.types ++ e.types)
    //
    //    def +(tv: (String, EnvEntry)) = Enviroment(this.vars + tv, this.types)
    //
    //    def +(tv: (String, Ty)) = Enviroment(this.vars, this.types + tv)
    //
    //  }


    def error(key: String) = {
      throw new EnvError("type error: not found " + key)
    }

    val baseTenv: tenv = Map("int" -> INT(), "intRo" -> INT.readOnly(), "string" -> STRING) withDefault error

    val baseVenv: venv = Map(
      "print" -> FuncEntry(mainLevel, "print", List(STRING), UNIT, extern = true),
      "flush" -> FuncEntry(mainLevel, "flush", Nil, UNIT, extern = true),
      "getchar" -> FuncEntry(mainLevel, "getstr", Nil, STRING, extern = true),
      "ord" -> FuncEntry(mainLevel, "ord", List(STRING), INT(), extern = true),
      "chr" -> FuncEntry(mainLevel, "chr", List(INT()), STRING, extern = true),
      "size" -> FuncEntry(mainLevel, "size", List(STRING), INT(), extern = true),
      "substring" -> FuncEntry(mainLevel, "substring", List(STRING, INT(), INT()), STRING, extern = true),
      "concat" -> FuncEntry(mainLevel, "substring", List(STRING, STRING), STRING, extern = true),
      "not" -> FuncEntry(mainLevel, "not", List(INT()), INT(), extern = true),
      "exit" -> FuncEntry(mainLevel, "exit", List(INT()), UNIT, extern = true)
    ) withDefault error

  }

}