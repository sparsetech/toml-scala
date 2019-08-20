package toml

import shapeless._

import scala.meta.internal.fastparse.core.Parsed._

object Toml {
  def parse(toml: String): Either[Parse.Error, Value.Tbl] =
    Rules.root.parse(toml) match {
      case Success(v, _)    => Embed.root(v)
      case f: Failure[_, _] => Left(List() -> f.msg)
    }

  def generate(root: Root): String = Generate.generate(root)

  class CodecHelperGeneric[A] {
    def apply[D <: HList, R <: HList](table: Value.Tbl)(implicit
      generic      : LabelledGeneric.Aux[A, R],
      defaults     : Default.AsRecord.Aux[A, D],
      defaultMapper: util.RecordToMap[D],
      codec        : Codec[R]
    ): Either[Parse.Error, A] = {
      val d = defaultMapper(defaults())
      codec(table, d, 0).right.map(generic.from)
    }

    def apply[D <: HList, R <: HList](toml: String)(implicit
      generic      : LabelledGeneric.Aux[A, R],
      defaults     : Default.AsRecord.Aux[A, D],
      defaultMapper: util.RecordToMap[D],
      codec        : Codec[R]
    ): Either[Parse.Error, A] = {
      val d = defaultMapper(defaults())
      parse(toml).right.flatMap(codec(_, d, 0).right.map(generic.from))
    }
  }

  class CodecHelperValue[A] {
    def apply(value: Value)(implicit codec: Codec[A]): Either[Parse.Error, A] =
      codec(value, Map(), 0)

    def apply(toml: String)(implicit codec: Codec[A]): Either[Parse.Error, A] =
      parse(toml).right.flatMap(codec(_, Map(), 0))
  }

  def parseAs     [T]: CodecHelperGeneric[T] = new CodecHelperGeneric[T]
  def parseAsValue[T]: CodecHelperValue  [T] = new CodecHelperValue[T]
}
