package soccerstand.util.enum

import scala.reflect.ClassTag

abstract class ReflectiveEnum[+Value: ClassTag] extends ObjectFieldsEarlyInit {
  lazy val values = earlyInitObjectFields.collect {
    case v: Value => v
  }

  def valueOfOpt(name: String): Option[Value] = values.find(_.toString == name)

  def valueOf(name: String): Value = valueOfOpt(name).getOrElse {
    throw new IllegalArgumentException(s"No enum value for name $name")
  }
}

sealed trait ObjectFieldsEarlyInit { self =>
  import scala.reflect.runtime.universe._

  protected lazy val earlyInitObjectFields = {
    val mirror = runtimeMirror(self.getClass.getClassLoader)
    mirror
      .classSymbol(self.getClass)
      .toType
      .members
      .sorted // Doc: Symbols with the same owner appear in same order of their declarations
      .filter(_.isModule)
      .map(m => mirror.reflectModule(m.asModule).instance)
  }
}

