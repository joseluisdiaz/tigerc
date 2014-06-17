package tiger

import tiger.Types.{UNIT, STRING, INT, Ty}

object Env {
  class EnvError(message: String) extends RuntimeException(message)

  val mainLevel:Seman.translate.Level = Nil

  sealed abstract class EnvEntry

  case class VarEntry(access: Seman.translate.Access, ty: Ty) extends EnvEntry

  case class FuncEntry(level: Seman.translate.Level,
                       label: Temp.Label,
                       params: List[Ty],
                       result: Ty,
                       extern: Boolean) extends EnvEntry

  type venv = Map[String, EnvEntry]
  type tenv = Map[String, Ty]

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
    throw new EnvError("type error: not found " + key )
  }

  val baseTenv:tenv = Map("int" -> INT(), "intRo" -> INT.readOnly(), "string" -> STRING) withDefault error

  val baseVenv:venv = Map(
    "print" -> FuncEntry(Env.mainLevel, "print", List(STRING), UNIT, extern = true),
    "flush" -> FuncEntry(Env.mainLevel, "flush", Nil, UNIT, extern = true),
    "getchar" -> FuncEntry(Env.mainLevel, "getstr", Nil, STRING, extern = true),
    "ord" -> FuncEntry(Env.mainLevel, "ord", List(STRING), INT(), extern = true),
    "chr" -> FuncEntry(Env.mainLevel, "chr", List(INT()), STRING, extern = true),
    "size" -> FuncEntry(Env.mainLevel, "size", List(STRING), INT(), extern = true),
    "substring" -> FuncEntry(Env.mainLevel, "substring", List(STRING, INT(), INT()), STRING, extern = true),
    "concat" -> FuncEntry(Env.mainLevel, "substring", List(STRING, STRING), STRING, extern = true),
    "not" -> FuncEntry(Env.mainLevel, "not", List(INT()), INT(), extern = true),
    "exit" -> FuncEntry(Env.mainLevel, "exit", List(INT()), UNIT, extern = true)
  ) withDefault error


}