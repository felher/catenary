package org.felher.catenary

import scala.language.implicitConversions

import com.raquo.laminar.api.L.*

sealed trait Bem extends Binder[HtmlElement] {
  override def bind(element: HtmlElement): DynamicSubscription =
    (className <-- classNames.map(_.mkString(" "))).bind(element)

  def classNames: Signal[List[String]] = this match {
    case Bem.BemBlock(block, modifiers) =>
      block
        .combineWith(modifiers)
        .map({ case (b, m) =>
          b :: m.toList.filter(_._2).map(mod => s"${b}--${mod._1}")
        })

    case Bem.BemElement(block, element, modifiers) =>
      block
        .combineWith(element, modifiers)
        .map({ case (b, e, m) =>
          val blockChild = s"${b}__${e}"
          blockChild :: m.toList.filter(_._2).map(mod => s"${blockChild}--${mod._1}")
        })
  }

  def svg = Bem.SvgBem(this)

}

object Bem {
  trait BemModifiersMaybeAsString {
    def toSignal(): Signal[Map[String, Boolean]]
  }

  object BemModifiersMaybeAsString {
    implicit def string(s: String): BemModifiersMaybeAsString         = () => Signal.fromValue(Map(s -> true))
    implicit def signal(s: Signal[String]): BemModifiersMaybeAsString = () => s.map(s => Map(s -> true))

    private def sequence(map: Map[String, Signal[Boolean]]): Signal[Map[String, Boolean]] =
      Signal.sequence(map.toList.map(entry => entry._2.map(b => (entry._1, b)))).map(_.toMap)

    implicit def map(m: Map[String, Boolean]): BemModifiersMaybeAsString           = () => Signal.fromValue(m)
    implicit def mapNS(m: Map[String, Signal[Boolean]]): BemModifiersMaybeAsString = () => sequence(m)
    implicit def mapNS(m: Signal[Map[String, Boolean]]): BemModifiersMaybeAsString = () => m
  }

  trait BemString {
    def toSignal(): Signal[String]
  }

  object BemString {
    implicit def string(s: String): BemString         = () => Signal.fromValue(s)
    implicit def signal(s: Signal[String]): BemString = () => s
  }

  trait BemModifiers {
    def toSignal(): Signal[Map[String, Boolean]]
  }

  object BemModifiers {
    private def sequence(map: Map[String, Signal[Boolean]]): Signal[Map[String, Boolean]] =
      Signal.sequence(map.toList.map(entry => entry._2.map(b => (entry._1, b)))).map(_.toMap)

    implicit def map(m: Map[String, Boolean]): BemModifiers           = () => Signal.fromValue(m)
    implicit def mapNS(m: Map[String, Signal[Boolean]]): BemModifiers = () => sequence(m)
    implicit def mapNS(m: Signal[Map[String, Boolean]]): BemModifiers = () => m
  }

  def apply(name: Bem.BemString): BemBlock =
    BemBlock(name.toSignal(), Signal.fromValue(Map.empty[String, Boolean]))

  def apply(name: Bem.BemString, modifiers: Bem.BemModifiers): BemBlock =
    BemBlock(name.toSignal(), modifiers.toSignal())

  final case class BemBlock(
      block: Signal[String],
      modifiers: Signal[Map[String, Boolean]]
  ) extends Bem {
    def apply(modifiers: Bem.BemModifiers): BemBlock =
      copy(modifiers = modifiers.toSignal())

    def apply(name: Bem.BemString): BemElement =
      BemElement(block, name.toSignal(), Signal.fromValue(Map.empty[String, Boolean]))

    def apply(name: Bem.BemString, modifiers: Bem.BemModifiersMaybeAsString): BemElement =
      BemElement(block, name.toSignal(), modifiers.toSignal())
  }

  object BemBlock {
    def apply(name: Bem.BemString): BemBlock =
      BemBlock(name.toSignal(), Signal.fromValue(Map.empty[String, Boolean]))

    def apply(name: Bem.BemString, modifiers: Bem.BemModifiers): BemBlock =
      BemBlock(name.toSignal(), modifiers.toSignal())
  }

  final case class BemElement(
      block: Signal[String],
      element: Signal[String],
      modifiers: Signal[Map[String, Boolean]]
  ) extends Bem {
    def apply(modifiers: Bem.BemModifiers): BemElement =
      copy(modifiers = modifiers.toSignal())
  }

  final case class SvgBem(parent: Bem) extends Binder[SvgElement] {
    override def bind(element: SvgElement): DynamicSubscription =
      (svg.className <-- parent.classNames.map(_.mkString(" "))).bind(element)
  }
}
