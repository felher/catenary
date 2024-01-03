package org.felher.catenary

import com.raquo.laminar.api.L.*
import io.circe.*
import org.scalajs.dom

object PersistentVar:
  /**
   * This function should not be used for dynamically created values
   * since it will never be cleaned up.
   */
  def leakingPersistentVar[A](name: String, default: A)(using
      enc: Encoder[A],
      dec: Decoder[A]
  ): Var[A] =
    val key = storageKey(name)
    val v   = dom.window.sessionStorage.getItem(key) match
      case null => Var(default)
      case s    =>
        io.circe.parser.decode[A](s) match
          case Left(_)  => Var(default)
          case Right(a) => Var(a)

    v.signal.foreach(value => dom.window.sessionStorage.setItem(key, enc(value).noSpaces))(unsafeWindowOwner)

    v

  private def storageKey(suffix: String): String =
    "org.felher.catenary" + suffix
