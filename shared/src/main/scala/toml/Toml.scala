package toml

import shapeless._

import fastparse.core.Parsed._

object Toml {
  def parse(toml: String): Either[String, Value.Tbl] =
    Rules.root.parse(toml) match {
      case Success(v, _)    => Right(Embed.root(v))
      case f: Failure[_, _] => Left(f.msg)
    }

  def generate(root: Root): String = Generate.generate(root)

  class CodecHelperGeneric[A] {
    def apply[D <: HList, R <: HList](table: Value.Tbl)(implicit
      generic      : LabelledGeneric.Aux[A, R],
      defaults     : Default.AsRecord.Aux[A, D],
      defaultMapper: util.RecordToMap[D],
      codec        : Codec[R]
    ): Either[Codec.Error, A] = {
      val d = defaultMapper(defaults())
      codec(table, d, 0).right.map(generic.from)
    }

    def apply[D <: HList, R <: HList](toml: String)(implicit
      generic      : LabelledGeneric.Aux[A, R],
      defaults     : Default.AsRecord.Aux[A, D],
      defaultMapper: util.RecordToMap[D],
      codec        : Codec[R]
    ): Either[Codec.Error, A] = {
      val d = defaultMapper(defaults())
      parse(toml)
        .left.map((List.empty, _))
        .right.flatMap(codec(_, d, 0).right.map(generic.from))
    }
  }

  class CodecHelperValue[A] {
    def apply(value: Value)(implicit codec: Codec[A]): Either[Codec.Error, A] =
      codec(value, Map.empty, 0)

    def apply(toml: String)(implicit codec: Codec[A]): Either[Codec.Error, A] =
      parse(toml).left.map((List.empty, _)).right.flatMap(codec(_, Map.empty, 0))
  }

  def parseAs     [T]: CodecHelperGeneric[T] = new CodecHelperGeneric[T]
  def parseAsValue[T]: CodecHelperValue  [T] = new CodecHelperValue[T]
}
