package toml

import scala.meta.internal.fastparse.all._

trait PlatformRules {
  val date = StringIn().map(_ => null.asInstanceOf[Value])
}
