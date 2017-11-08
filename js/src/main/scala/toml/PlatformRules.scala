package toml

import fastparse.all._

trait PlatformRules {
  val date = StringIn().map(_ => null.asInstanceOf[Value])
}
