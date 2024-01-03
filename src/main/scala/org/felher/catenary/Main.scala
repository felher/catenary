package org.felher.catenary

import com.raquo.laminar.api.L.*
import org.scalajs.dom

@main def hello: Unit =
  val config = PersistentVar.leakingPersistentVar("config", Config.default)
  val pAPos  = PersistentVar.leakingPersistentVar("pA", Point(15, 20))
  val pBPos  = PersistentVar.leakingPersistentVar("pB", Point(35, 40))

  val info = pAPos.signal
    .combineWith(pBPos.signal, config.signal)
    .map((pA, pB, config) => CatInfo.calculate(pA, pB, config.length))

  val bem = Bem("app")
  render(
    dom.document.getElementById("app-anchor"),
    div(
      bem,
      div(
        bem("config"),
        Config.render(config)
      ),
      Diagram.render(config.signal, info, pAPos, pBPos).amend(bem("diagram").svg),
      div(
        bem("output"),
        Tutorial.render(config, info)
      )
    )
  )

  ()
