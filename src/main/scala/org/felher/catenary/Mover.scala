package org.felher.catenary

import com.raquo.laminar.api.L.*
import org.scalajs.dom

object Mover:
  trait Helper:
    def -->(f: Point => Unit): Modifier.Base

  def apply(container: dom.Element) = new Helper:
    val running = Var(false)

    def -->(f: Point => Unit): Modifier.Base =
      List(
        running.signal --> (r => println(s"running: $r")),
        onPointerDown.mapTo(true) --> running,
        windowEvents(_.onPointerUp).mapTo(false) --> running,
        windowEvents(_.onPointerMove).filter(_ => running.now()) --> (e =>
          println("move")
          val parentRect = container.getBoundingClientRect()
          val x          = e.clientX - parentRect.left
          val y          = e.clientY - parentRect.top
          val relX       = x / parentRect.width
          val relY       = 1 - y / parentRect.height
          f(Point(relX, relY))
        )
      )
