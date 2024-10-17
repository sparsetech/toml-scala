package toml

import fastparse.Parsed

object Toml extends TomlVersionSpecific {
  def parse(toml: String, extensions: Set[Extension] = Set()): Either[Parse.Error, Value.Tbl] =
    fastparse.parse(toml, new Rules(extensions).root(_)) match {
      case Parsed.Success(v, _)    => Embed.root(v)
      case f: Parsed.Failure => Left(List() -> f.msg)
    }

  def generate(root: Root): String = Generate.generate(root)
}
