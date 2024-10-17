package toml

import fastparse._

trait PlatformRules {
  def date[$: P] = StringIn().map(_ => null.asInstanceOf[Value])
}
