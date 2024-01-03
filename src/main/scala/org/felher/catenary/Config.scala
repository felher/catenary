package org.felher.catenary

import com.raquo.laminar.api.L.*

final case class Config(
    origCurve: Boolean,
    hv: Boolean,
    length: Double,
    snap: Boolean,
    offset: Point,
    shadowPoints: Boolean
) derives io.circe.Codec.AsObject

object Config:
  val bem     = Bem("config")
  val default = Config(
    origCurve = false,
    hv = false,
    length = 50,
    snap = false,
    Point(0, 0),
    shadowPoints = false
  )

  def render(config: Var[Config]): HtmlElement =
    div(
      bem,
      div(
        bem("attribute"),
        div(
          bem("label"),
          "Original Curve"
        ),
        input(
          bem("input"),
          tpe := "checkbox",
          controlled(
            checked <-- config.signal.map(_.origCurve),
            onClick --> (e => config.update(_.copy(origCurve = !config.now().origCurve)))
          )
        )
      ),
      div(
        bem("attribute"),
        div(
          bem("label"),
          "Show H/v"
        ),
        input(
          bem("input"),
          tpe := "checkbox",
          controlled(
            checked <-- config.signal.map(_.hv),
            onClick --> (e => config.update(_.copy(hv = !config.now().hv)))
          )
        )
      ),
      div(
        bem("attribute"),
        div(
          bem("label"),
          "Snap to Grid"
        ),
        input(
          bem("input"),
          tpe := "checkbox",
          controlled(
            checked <-- config.signal.map(_.snap),
            onClick --> (e => config.update(_.copy(snap = !config.now().snap)))
          )
        )
      ),
      div(
        bem("attribute"),
        div(
          bem("label"),
          "Rope Length"
        ),
        input(
          bem("input"),
          tpe("range"),
          minAttr("0"),
          maxAttr("100"),
          controlled(
            value <-- config.signal.map(_.length.toString),
            onInput.mapToValue --> (v => v.toDoubleOption.foreach(l => config.update(_.copy(length = l))))
          )
        )
      )
    )
