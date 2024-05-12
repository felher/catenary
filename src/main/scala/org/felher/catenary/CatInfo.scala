package org.felher.catenary

import com.raquo.laminar.api.L.*

final case class CatInfo(
    p1: Point,
    p2: Point,
    H: Double,
    L: Double,
    l: Double,
    v: Double,
    a: Option[Double],
    x1: Option[Double],
    x2: Option[Double],
    y1: Option[Double],
    y2: Option[Double],
    xOffset: Option[Double],
    yOffset: Option[Double],
    relabel: Boolean
)

object CatInfo:
  val bem                                                 = Bem("cat-info")
  def calculate(pA: Point, pB: Point, L: Double): CatInfo =
    val left    = if pA.x < pB.x then pA else pB
    val right   = if pA.x < pB.x then pB else pA
    val l       = left.distance(right)
    val H       = right.x - left.x
    val v       = right.y - left.y
    val relabel = pA != left

    val a       = approximateCatenary(H, v, L)
    val x1      = a.flatMap(a => findXOfLeft(a, H, v))
    val x2      = x1.map(_ + H)
    val y1      = x1.flatMap(x1 => a.map(a => a * Math.cosh(x1 / a)))
    val y2      = x2.flatMap(x2 => a.map(a => a * Math.cosh(x2 / a)))
    val xOffset = x1.map(left.x - _)
    val yOffset = for
      a_  <- a
      x1_ <- x1
    yield left.y - a_ * Math.cosh(x1_ / a_)

    CatInfo(
      p1 = left,
      p2 = right,
      H = H,
      L = L,
      l = l,
      v = v,
      a = a,
      x1 = x1,
      x2 = x2,
      y1 = y1,
      y2 = y2,
      xOffset = xOffset,
      yOffset = yOffset,
      relabel = relabel
    )

  private def approximateCatenary(H: Double, v: Double, L: Double): Option[Double] =
    val target = 1 / H * Math.sqrt(L * L - v * v)
    val af     = (a: Double) => 2 * a / H * Math.sinh(H / 2 / a)
    MathUtil.approximate(
      start = 0.001,
      f = af,
      positiveSlope = false,
      targetY = target,
      stepSize = 1,
      minError = 0.00001,
      maxSteps = 1000
    )

  private def findXOfLeft(a: Double, H: Double, v: Double): Option[Double] =
    val vf = (x: Double) => a * Math.cosh((x + H) / a) - a * Math.cosh(x / a)
    MathUtil.approximate(
      start = -H / 2,
      f = vf,
      positiveSlope = true,
      targetY = v,
      stepSize = 1,
      minError = 0.001,
      maxSteps = 1000
    )

  def render(info: Signal[CatInfo]): HtmlElement =
    val elems = List(
      ("H", info.map(_.H)),
      ("L", info.map(_.L)),
      ("l", info.map(_.l)),
      ("v", info.map(_.v)),
      ("a", info.map(_.a.fold("did not converge")(_.toString))),
      ("x1", info.map(_.x1.fold("did not converge")(_.toString))),
      ("x2", info.map(_.x2.fold("did not converge")(_.toString))),
      ("xOffset", info.map(_.xOffset.fold("did not converge")(_.toString))),
      ("yOffset", info.map(_.yOffset.fold("did not converge")(_.toString)))
    )

    div(
      bem,
      table(
        tbody(
          bem("table"),
          elems.map((name, value) =>
            tr(
              bem("row"),
              td(bem("name"), name),
              td(bem("value"), child.text <-- value.map(_.toString))
            )
          )
        )
      )
    )
