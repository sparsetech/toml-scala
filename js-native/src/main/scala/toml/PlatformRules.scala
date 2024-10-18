package toml

import fastparse._

trait PlatformRules {
  def date[$: P] = P(CharIn()).map(_ => null.asInstanceOf[Value])
}
