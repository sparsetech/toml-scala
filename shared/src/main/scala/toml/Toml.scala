package toml

import shapeless._

import fastparse.all._
import fastparse.core.Parsed._

object Toml {
  def parse(toml: String): Either[String, Value.Tbl] =
    Rules.root.parse(toml) match {
      case Success(v, _)    => Right(Embed.root(v))
      case f: Failure[_, _] => Left(f.msg)
    }

  trait CodecHelperLowPrio[A] {
    def apply(value: Value)(implicit codec: Codec[A]): Either[Codec.Error, A] =
      codec(value)

    def apply(toml: String)(implicit codec: Codec[A]): Either[Codec.Error, A] =
      parse(toml).left.map((List.empty, _)).right.flatMap(codec(_))
  }

  class CodecHelper[A] extends CodecHelperLowPrio[A] {
    def apply[R <: HList](table: Value.Tbl)(implicit
      gen  : LabelledGeneric.Aux[A, R],
      codec: Codec[R]
    ): Either[Codec.Error, A] = codec(table).right.map(gen.from)

    def apply[R <: HList](toml: String)(implicit
      gen  : LabelledGeneric.Aux[A, R],
      codec: Codec[R]
    ): Either[Codec.Error, A] =
      parse(toml)
        .left.map((List.empty, _))
        .right.flatMap(codec(_).right.map(gen.from))
  }

  def parseAs[T]: CodecHelper[T] = new CodecHelper[T]
}
