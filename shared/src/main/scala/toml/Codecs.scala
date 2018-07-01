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
  ): Codec[FieldType[K, Option[V]] :: T] = {
    import witness.value.{name => witnessName}

    def f(head: Option[Value],
          tail: Value,
          default: Option[V],
          defaults: Map[String, Any]) =
      fromT.value(tail, defaults).right.flatMap(t =>
        head match {
          case None    => Right(field[K](default) :: t)
          case Some(v) =>
            for {
              k <- fromV.value(v, defaults)
                .left.map { case (a, m) => (witnessName +: a, m) }
                .right
            } yield field[K](Some(k)) :: t
        })

    def resolve(defaults: Map[String, Any], key: String): Option[V] =
      defaults.get(key).asInstanceOf[Option[Option[V]]].flatten

    Codec {
      case (Value.Tbl(pairs), defaults) =>
        f(pairs.get(witnessName),
          Value.Tbl(pairs - witnessName),
          resolve(defaults, witnessName),
          defaults)

      case (Value.Arr(head +: tail), defaults) =>
        f(Some(head), Value.Arr(tail), resolve(defaults, witnessName), defaults)

      case (value: Value.Arr, defaults) =>
        f(None, value, resolve(defaults, witnessName), defaults)

      case value => Left((List.empty, s"Table or Array expected, $value provided"))
    }
  }

  implicit def hconsFromNode[K <: Symbol, V, T <: HList](implicit
    witness: Witness.Aux[K],
    fromV: Lazy[Codec[V]],
    fromT: Lazy[Codec[T]]
  ): Codec[FieldType[K, V] :: T] = {
    import witness.value.{name => witnessName}

    def f(head: Value, tail: Value, defaults: Map[String, Any]) =
      for {
        h <- fromV.value(head, defaults)
          .left.map { case (a, m) => (witnessName +: a, m) }
          .right
        t <- fromT.value(tail, defaults).right
      } yield field[K](h) :: t

    Codec {
      case (Value.Tbl(pairs), defaults) if pairs.contains(witnessName) =>
        f(pairs(witnessName), Value.Tbl(pairs - witnessName), defaults)

      case (Value.Arr(head +: tail), defaults) =>
        f(head, Value.Arr(tail), defaults)

      case (value, defaults) if defaults.contains(witnessName) && (
        value.isInstanceOf[Value.Tbl] || value.isInstanceOf[Value.Arr]
      ) =>
        val h = defaults(witnessName).asInstanceOf[V]
        fromT.value(value, defaults).right.map(t => field[K](h) :: t)

      case (value, _) => Left((List.empty, s"Cannot resolve `${witnessName}`"))
    }
  }
}

object Codecs extends LowPriorityCodecs with PlatformCodecs {
  implicit val hnilFromNode: Codec[HNil] =
    Codec[HNil] {
      case (Value.Tbl(pairs), defaults) if pairs.nonEmpty =>
        Left((List(pairs.keySet.head), "Unknown field"))
      case (Value.Arr(elems), defaults) if elems.nonEmpty =>
        Left((List(), s"Too many elements; remove ${elems.head}"))
      case _ => Right(HNil)
    }

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

  implicit val boolCodec: Codec[Boolean] = Codec {
    case (Value.Bool(value), _) => Right(value)
    case (value            , _) =>
      Left((List.empty, s"Bool expected, $value provided"))
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
              case Left((a, m)) => Left((k +: a, m))
              case Right(v2)    => Right(r + (k -> v2))
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
