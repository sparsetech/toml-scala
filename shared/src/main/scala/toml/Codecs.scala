package toml

import shapeless._
import shapeless.labelled._

import scala.annotation.implicitNotFound

@implicitNotFound("Codec[${A}] implicit not defined in scope")
trait Codec[A] {
  def apply(
    value:    Value,
    defaults: Map[String, Any],
    index:    Int
  ): Either[Codec.Error, A]
}

object Codec {
  type Field    = String
  type Address  = List[Field]
  type Message  = String
  type Error    = (Address, Message)
  type Defaults = Map[String, Any]
  type Index    = Int

  def apply[T](
    f: (Value, Defaults, Index) => Either[Error, T]
  ): Codec[T] = new Codec[T] {
    override def apply(
      value: Value, defaults: Defaults, index: Index
    ): Either[Error, T] = f(value, defaults, index)
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

    def f(
      head:     Option[Value],
      tail:     Value,
      mapError: Codec.Error => Codec.Error,
      default:  Option[V],
      defaults: Codec.Defaults,
      index:    Codec.Index
    ) =
      fromT.value(tail, defaults, index + 1).right.flatMap(t =>
        head match {
          case None    => Right(field[K](default) :: t)
          case Some(v) =>
            for {
              k <- fromV.value(v, defaults, index)
                .left.map(mapError).right
            } yield field[K](Some(k)) :: t
        })

    def resolve(defaults: Map[String, Any], key: String): Option[V] =
      defaults.get(key).asInstanceOf[Option[Option[V]]].flatten

    Codec {
      case (Value.Tbl(pairs), defaults, _) =>
        f(pairs.get(witnessName),
          Value.Tbl(pairs - witnessName),
          { case (a, m) => (witnessName +: a, m) },
          resolve(defaults, witnessName),
          defaults,
          0)

      case (Value.Arr(values), defaults, index) =>
        f(
          values.headOption,
          Value.Arr(values.drop(1)),
          { case (a, m) => (s"#${index + 1}" +: a, m) },
          resolve(defaults, witnessName),
          defaults,
          index)

      case (value, _, _) =>
        Left((List(), s"Table or Array expected, $value provided"))
    }
  }

  implicit def hconsFromNode[K <: Symbol, V, T <: HList](implicit
    witness: Witness.Aux[K],
    fromV: Lazy[Codec[V]],
    fromT: Lazy[Codec[T]]
  ): Codec[FieldType[K, V] :: T] = {
    import witness.value.{name => witnessName}

    def f(
      head:     Value,
      tail:     Value,
      mapError: (Codec.Error, Codec.Index) => Codec.Error,
      defaults: Codec.Defaults,
      index:    Codec.Index
    ) =
      for {
        h <- fromV.value(head, defaults, index)
          .left.map(mapError(_, index)).right
        t <- fromT.value(tail, defaults, index + 1).right
      } yield field[K](h) :: t

    Codec {
      case (Value.Tbl(pairs), defaults, _) if pairs.contains(witnessName) =>
        f(pairs(witnessName), Value.Tbl(pairs - witnessName),
          { case ((a, m), _) => (witnessName +: a, m) },
          defaults, 0)

      case (Value.Arr(head +: tail), defaults, index) =>
        f(head, Value.Arr(tail),
          { case ((a, m), index) => (s"#${index + 1}" +: a, m) },
          defaults, index)

      case (value, defaults, index) if defaults.contains(witnessName) && (
        value.isInstanceOf[Value.Tbl] || value.isInstanceOf[Value.Arr]
      ) =>
        val h = defaults(witnessName).asInstanceOf[V]
        fromT.value(value, defaults, index).right.map(t => field[K](h) :: t)

      case (_, _, _) =>
        Left((List(), s"Cannot resolve `$witnessName`"))
    }
  }
}

object Codecs extends LowPriorityCodecs with PlatformCodecs {
  implicit val hnilFromNode: Codec[HNil] =
    Codec[HNil] {
      case (Value.Tbl(pairs), defaults, _) if pairs.nonEmpty =>
        Left((List(pairs.keySet.head), "Unknown field"))
      case (Value.Arr(elems), defaults, _) if elems.nonEmpty =>
        Left((List(), s"Too many elements; remove ${elems.head}"))
      case _ => Right(HNil)
    }

  implicit val stringCodec: Codec[String] = Codec {
    case (Value.Str(value), _, _) => Right(value)
    case (value,            _, _) =>
      Left((List.empty, s"String expected, $value provided"))
  }

  implicit val longCodec: Codec[Long] = Codec {
    case (Value.Num(value), _, _) => Right(value)
    case (value           , _, _) =>
      Left((List.empty, s"Long expected, $value provided"))
  }

  implicit val intCodec: Codec[Int] = Codec {
    case (Value.Num(value), _, _) => Right(value.toInt)
    case (value           , _, _) =>
      Left((List.empty, s"Int expected, $value provided"))
  }

  implicit val doubleCodec: Codec[Double] = Codec {
    case (Value.Real(value), _, _) => Right(value)
    case (value            , _, _) =>
      Left((List.empty, s"Double expected, $value provided"))
  }

  implicit val boolCodec: Codec[Boolean] = Codec {
    case (Value.Bool(value), _, _) => Right(value)
    case (value            , _, _) =>
      Left((List.empty, s"Bool expected, $value provided"))
  }

  implicit def listCodec[T](implicit codec: Codec[T]): Codec[List[T]] = Codec {
    case (Value.Arr(elems), _, _) =>
      elems.zipWithIndex.foldLeft(Right(List.empty): Either[Codec.Error, List[T]]) {
        case (Right(acc), (cur, idx)) =>
          codec(cur, Map.empty, 0)
            .left.map { case (a, m) => (s"#${idx + 1}" +: a, m) }
            .right.map(acc :+ _)

        case (acc, _) => acc
      }

    case (value, _, _) => Left((List.empty, s"List expected, $value provided"))
  }

  implicit def tableCodec[T](implicit codec: Codec[T]): Codec[Map[String, T]] =
    Codec {
      case (Value.Tbl(value), _, _) =>
        value.foldLeft(Right(Map.empty): Either[Codec.Error, Map[String, T]]) {
          case (Left(l), _) => Left(l)
          case (Right(r), (k, v)) =>
            codec(v, Map.empty, 0) match {
              case Left((a, m)) => Left((k +: a, m))
              case Right(v2)    => Right(r + (k -> v2))
            }
        }

      case (value, _, _) => Left((List.empty, s"Table expected, $value provided"))
    }

  implicit def genericCodec[A, D <: HList, R <: HList](implicit
    generic      : LabelledGeneric.Aux[A, R],
    defaults     : Default.AsRecord.Aux[A, D],
    defaultMapper: util.RecordToMap[D],
    codec        : Codec[R]
  ): Codec[A] = {
    val d = defaultMapper(defaults())
    Codec((v, _, _) => codec(v, d, 0).right.map(generic.from))
  }
}
