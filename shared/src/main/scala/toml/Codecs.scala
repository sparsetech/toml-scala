package toml

import shapeless._
import shapeless.labelled._

import scala.annotation.implicitNotFound

@implicitNotFound("Codec[${A}] implicit not defined in scope")
trait Codec[A] {
  def apply(value: Value): Either[Codec.Error, A]
}

object Codec {
  type Address = List[String]
  type Message = String
  type Error   = (Address, Message)

  def apply[T](f: Value => Either[Error, T]): Codec[T] = new Codec[T] {
    override def apply(value: Value): Either[Error, T] = f(value)
  }
}

/**
  * Adapted from: https://stackoverflow.com/a/31641779
  */
trait LowPriorityCodecs {
  implicit def hconsFromNodeOpt[K <: Symbol, V, T <: HList](implicit
    witness: Witness.Aux[K],
    fromV: Lazy[Codec[V]],
    fromT: Lazy[Codec[T]]
  ): Codec[FieldType[K, Option[V]] :: T] = Codec {
    case value @ Value.Tbl(pairs) =>
      fromT.value(value).right.map { t =>
        val v = pairs.get(witness.value.name)
          .flatMap(fromV.value(_).right.toOption)

        field[K](v) :: t
      }

    case value => Left((List.empty, s"Table expected, $value provided"))
  }

  implicit def hconsFromNode[K <: Symbol, V, T <: HList](implicit
    witness: Witness.Aux[K],
    fromV: Lazy[Codec[V]],
    fromT: Lazy[Codec[T]]
  ): Codec[FieldType[K, V] :: T] = Codec {
    case tbl @ Value.Tbl(pairs) if pairs.contains(witness.value.name) =>
      val value = pairs(witness.value.name)
      for {
        h <- fromV.value(value)
          .left.map { case (a, m) => (witness.value.name +: a, m) }
          .right
        t <- fromT.value(tbl).right
      } yield field[K](h) :: t

    case value => Left(
      (List.empty, s"Cannot resolve `${witness.value.name}`"))
  }
}

object Codecs extends LowPriorityCodecs {
  implicit val hnilFromNode: Codec[HNil] =
    Codec[HNil](_ => Right(HNil))

  implicit def hconsFromNode1[K <: Symbol, V, R <: HList, T <: HList](implicit
    witness: Witness.Aux[K],
    gen: LabelledGeneric.Aux[V, R],
    fromH: Codec[R],
    fromT: Codec[T]
  ): Codec[FieldType[K, V] :: T] = Codec {
    case tbl @ Value.Tbl(pairs) if pairs.contains(witness.value.name) =>
      val value = pairs(witness.value.name)
      for {
        h <- fromH(value)
          .left.map { case (a, m) => (witness.value.name +: a, m) }
          .right
        t <- fromT(tbl).right
      } yield field[K](gen.from(h)) :: t

    case value => Left(
      (List.empty, s"Cannot resolve `${witness.value.name}`"))
  }

  implicit def hconsFromNode1Opt[K <: Symbol, V, R <: HList, T <: HList](implicit
    witness: Witness.Aux[K],
    gen: LabelledGeneric.Aux[V, R],
    fromH: Codec[R],
    fromT: Codec[T]
  ): Codec[FieldType[K, Option[V]] :: T] = Codec {
    case value @ Value.Tbl(pairs) =>
      fromT(value).right.map { t =>
        val v = pairs.get(witness.value.name)
          .flatMap(fromH(_).right.toOption)
          .map(gen.from)

        field[K](v) :: t
      }

    case value => Left((List.empty, s"Table expected, $value provided"))
  }

  implicit val stringCodec: Codec[String] = Codec {
    case Value.Str(value) => Right(value)
    case value            =>
      Left((List.empty, s"String expected, $value provided"))
  }

  implicit val longCodec: Codec[Long] = Codec {
    case Value.Num(value) => Right(value)
    case value            =>
      Left((List.empty, s"Long expected, $value provided"))
  }

  implicit val intCodec: Codec[Int] = Codec {
    case Value.Num(value) => Right(value.toInt)
    case value            =>
      Left((List.empty, s"Int expected, $value provided"))
  }

  implicit val doubleCodec: Codec[Double] = Codec {
    case Value.Real(value) => Right(value)
    case value             =>
      Left((List.empty, s"Double expected, $value provided"))
  }

  implicit def listCodec[T](implicit codec: Codec[T]): Codec[List[T]] = Codec {
    case Value.Arr(elems) =>
      elems.foldLeft(Right(List.empty[T]): Either[Codec.Error, List[T]]) {
        case (Right(acc), cur) => codec(cur).right.map(acc :+ _)
        case (acc       , _  ) => acc
      }

    case value => Left((List.empty, s"List expected, $value provided"))
  }

  implicit def genericCodec[A, R <: HList](implicit
    gen: LabelledGeneric.Aux[A, R],
    codec: Codec[R]
  ): Codec[A] = Codec(codec(_).right.map(gen.from))
}
