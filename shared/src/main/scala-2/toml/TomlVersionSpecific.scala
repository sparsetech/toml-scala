package toml

import shapeless._

trait TomlVersionSpecific {

  class CodecHelperGeneric[A] {
    def apply[D <: HList, R <: HList](table: Value.Tbl)(implicit
        generic: LabelledGeneric.Aux[A, R],
        defaults: Default.AsRecord.Aux[A, D],
        defaultMapper: util.RecordToMap[D],
        codec: Codec[R]
    ): Either[Parse.Error, A] = {
      val d = defaultMapper(defaults())
      codec(table, d, 0).right.map(generic.from)
    }

    def apply[D <: HList, R <: HList](
        toml: String,
        extensions: Set[Extension]
    )(implicit
        generic: LabelledGeneric.Aux[A, R],
        defaults: Default.AsRecord.Aux[A, D],
        defaultMapper: util.RecordToMap[D],
        codec: Codec[R]
    ): Either[Parse.Error, A] = {
      val d = defaultMapper(defaults())
      parse(toml, extensions).right
        .flatMap(codec(_, d, 0).right.map(generic.from))
    }

    def apply[D <: HList, R <: HList](toml: String)(implicit
        generic: LabelledGeneric.Aux[A, R],
        defaults: Default.AsRecord.Aux[A, D],
        defaultMapper: util.RecordToMap[D],
        codec: Codec[R]
    ): Either[Parse.Error, A] = apply(toml, Set())
  }

  class CodecHelperValue[A] {
    def apply(value: Value)(implicit codec: Codec[A]): Either[Parse.Error, A] =
      codec(value, Map(), 0)

    def apply(toml: String, extensions: Set[Extension] = Set())(implicit
        codec: Codec[A]
    ): Either[Parse.Error, A] =
      parse(toml, extensions).right.flatMap(codec(_, Map(), 0))
  }

  def parseAs[T]: CodecHelperGeneric[T] = new CodecHelperGeneric[T]
  def parseAsValue[T]: CodecHelperValue[T] = new CodecHelperValue[T]
}
