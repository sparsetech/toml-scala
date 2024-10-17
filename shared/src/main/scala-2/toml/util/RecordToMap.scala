package toml.util

import shapeless.{ ::, HList, HNil, Witness }
import shapeless.labelled.FieldType

// From https://github.com/circe/circe/blob/57c0d7eac5fac98bbfc40134da81ea53d0853ed2/modules/generic-extras/src/main/scala/io/circe/generic/extras/util/RecordToMap.scala

abstract class RecordToMap[R <: HList] {
  def apply(r: R): Map[String, Any]
}

final object RecordToMap {
  implicit val hnilRecordToMap: RecordToMap[HNil] = new RecordToMap[HNil] {
    def apply(r: HNil): Map[String, Any] = Map.empty
  }

  implicit def hconsRecordToMap[K <: Symbol, V, T <: HList](implicit
    wit: Witness.Aux[K],
    rtmT: RecordToMap[T]
  ): RecordToMap[FieldType[K, V] :: T] = new RecordToMap[FieldType[K, V] :: T] {
    def apply(r: FieldType[K, V] :: T): Map[String, Any] = rtmT(r.tail) + ((wit.value.name, r.head))
  }
}