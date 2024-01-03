package org.felher.catenary

import com.raquo.laminar.api.L.*

object MiniMarkdown:
  val bem = Bem("mini-markdown")

  def render(text: String): HtmlElement =
    val blocks = text.trim().split("\n\n").toList.map(_.trim())
    div(
      bem,
      blocks.map(text => p(bem("paragraph"), renderParagraph(text)))
    )

  final private case class SpecialMode(
      start: String,
      end: String,
      render: String => HtmlElement
  )

  private object SpecialMode:
    val all = List(
      SpecialMode("**", "**", s => strong(s)),
      SpecialMode("*", "*", s => em(s)),
      SpecialMode("`", "`", s => code(s)),
      SpecialMode("![", "]", s => img(bem("formula"), src := s)),
      SpecialMode(
        "[",
        ")",
        s =>
          s.split("\\]\\(", 2) match
            case Array(text, link) => a(href := link, text)
            case _                 => throw new RuntimeException("invalid link")
      )
    )

  private def renderParagraph(text: String): List[HtmlElement] =
    def nextSpecial(i: Int): Option[(Int, SpecialMode)] =
      if i >= text.length then None
      else
        SpecialMode.all.find(sm => text.substring(i).startsWith(sm.start)) match
          case None     => nextSpecial(i + 1)
          case Some(sm) => Some((i, sm))

    def takeMode(i: Int, mode: SpecialMode): Option[(Int, HtmlElement)] =
      val textStart = i + mode.start.length
      text.substring(textStart).indexOf(mode.end) match
        case -1 => None
        case j  => Some((textStart + j + mode.end.length, mode.render(text.substring(textStart, textStart + j))))

    def go(i: Int, accu: List[HtmlElement]): List[HtmlElement] =
      if i >= text.length then accu.reverse
      else
        nextSpecial(i) match
          case None            => (span(bem("text"), text.substring(i)) :: accu).reverse
          case Some((j, mode)) =>
            val textBetween = text.substring(i, j)
            takeMode(j, mode) match
              case None            => throw new RuntimeException(s"unmatched special mode: $mode")
              case Some((k, elem)) => go(k, elem :: span(textBetween) :: accu)

    go(0, Nil)
