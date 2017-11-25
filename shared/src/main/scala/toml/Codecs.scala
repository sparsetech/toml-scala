package toml

import shapeless._
import shapeless.labelled._

import scala.annotation.implicitNotFound

@implicitNotFound("Codec[${A}] implicit not defined in scope")
trait Codec[A] {
  def apply(value: Value, defaults: Map[String, Any]): Either[Codec.Error, A]
}

object Codec {
  type Address = List[String]
  type Message = String
  type Error   = (Address, Message)

  def apply[T](
    f: (Value, Map[String, Any]) => Either[Error, T]
  ): Codec[T] = new Codec[T] {
    override def apply(
      value: Value, defaults: Map[String, Any]
    ): Either[Error, T] = f(value, defaults)
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
    case (value @ Value.Tbl(pairs), defaults) =>
      fromT.value(value, defaults).right.map { t =>
        val v = pairs.get(witness.value.name)
          .flatMap(fromV.value(_, defaults).right.toOption)

        field[K](v) :: t
      }

    case value => Left((List.empty, s"Table expected, $value provided"))
  }

  implicit def hconsFromNode[K <: Symbol, V, T <: HList](implicit
    witness: Witness.Aux[K],
    fromV: Lazy[Codec[V]],
    fromT: Lazy[Codec[T]]
  ): Codec[FieldType[K, V] :: T] = Codec {
    case (tbl @ Value.Tbl(pairs), defaults)
      if pairs.contains(witness.value.name) =>
      val value = pairs(witness.value.name)

      for {
        h <- fromV.value(value, defaults)
          .left.map { case (a, m) => (witness.value.name +: a, m) }
          .right
        t <- fromT.value(tbl, defaults).right
      } yield field[K](h) :: t

    case (tbl: Value.Tbl, defaults) if defaults.contains(witness.value.name) =>
      val h = defaults(witness.value.name).asInstanceOf[V]
      fromT.value(tbl, defaults).right.map(t => field[K](h) :: t)

    case (value, _) =>
      Left((List.empty, s"Cannot resolve `${witness.value.name}`"))
  }
}

object Codecs extends LowPriorityCodecs with PlatformCodecs {
  implicit val hnilFromNode: Codec[HNil] =
    Codec[HNil]((_, _) => Right(HNil))

  implicit val stringCodec: Codec[String] = Codec {
    case (Value.Str(value), _) => Right(value)
    case (value,            _) =>
      Left((List.empty, s"String expected, $value provided"))
  }

  implicit val longCodec: Codec[Long] = Codec {
    case (Value.Num(value), _) => Right(value)
    case (value           , _) =>
      Left((List.empty, s"Long expected, $value provided"))
  }

  implicit val intCodec: Codec[Int] = Codec {
    case (Value.Num(value), _) => Right(value.toInt)
    case (value           , _) =>
      Left((List.empty, s"Int expected, $value provided"))
  }

  implicit val doubleCodec: Codec[Double] = Codec {
    case (Value.Real(value), _) => Right(value)
    case (value            , _) =>
      Left((List.empty, s"Double expected, $value provided"))
  }

  implicit def listCodec[T](implicit codec: Codec[T]): Codec[List[T]] = Codec {
    case (Value.Arr(elems), _) =>
      elems.foldLeft(Right(List.empty): Either[Codec.Error, List[T]]) {
        case (Right(acc), cur) => codec(cur, Map.empty).right.map(acc :+ _)
        case (acc       , _  ) => acc
      }

    case (value, _) => Left((List.empty, s"List expected, $value provided"))
  }

  implicit def tableCodec[T](implicit codec: Codec[T]): Codec[Map[String, T]] =
    Codec {
      case (Value.Tbl(value), _) =>
        value.foldLeft(Right(Map.empty): Either[Codec.Error, Map[String, T]]) {
          case (Left(l), _) => Left(l)
          case (Right(r), (k, v)) =>
            codec(v, Map.empty) match {
              case Left(l)   => Left(l)
              case Right(v2) => Right(r + (k -> v2))
            }
        }

      case (value, _) => Left((List.empty, s"Table expected, $value provided"))
    }

  implicit def genericCodec[A, D <: HList, R <: HList](implicit
    generic      : LabelledGeneric.Aux[A, R],
    defaults     : Default.AsRecord.Aux[A, D],
    defaultMapper: util.RecordToMap[D],
    codec        : Codec[R]
  ): Codec[A] = {
    val d = defaultMapper(defaults())
    Codec((v, _) => codec(v, d).right.map(generic.from))
  }
}
