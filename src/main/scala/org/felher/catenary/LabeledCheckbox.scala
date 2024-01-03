package org.felher.catenary

import com.raquo.laminar.api.L.*
import monocle.Lens

object LabeledCheckbox:
  val bem = Bem("labeled-checkbox")

  def forObject[S](label: String, obj: Var[S], attr: Lens[S, Boolean]): HtmlElement =
    div(
      bem,
      div(
        bem("label"),
        label
      ),
      input(
        bem("input"),
        tpe := "checkbox",
        controlled(
          checked <-- obj.signal.map(attr.get),
          onClick --> (e => obj.update(attr.modify(!_)))
        )
      )
    )
