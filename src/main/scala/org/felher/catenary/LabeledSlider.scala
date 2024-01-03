package org.felher.catenary

import com.raquo.laminar.api.L.*
import monocle.Lens

object LabeledSlider:
  val bem = Bem("labeled-slider")

  def forObject[S](
      label: String,
      min: Double,
      max: Double,
      obj: Var[S],
      attr: Lens[S, Double],
      stepSize: Double = 1
  ): HtmlElement =
    div(
      bem,
      div(
        bem("label"),
        label
      ),
      input(
        bem("input"),
        tpe("range"),
        minAttr(min.toString),
        maxAttr(max.toString),
        stepAttr(stepSize.toString),
        controlled(
          value <-- obj.signal.map(attr.get).map(_.toString),
          onInput.mapToValue --> (v => obj.update(attr.replace(v.toDouble)))
        )
      )
    )
