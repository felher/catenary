package org.felher.catenary

import scala.util.chaining.*

import com.raquo.laminar.api.L.*

object Diagram:
  val bem = Bem("diagram")

  def render(
      config: StrictSignal[Config],
      info: Signal[CatInfo],
      pAPos: Var[Point],
      pBPos: Var[Point]
  ): SvgElement =
    val p1Pos = pAPos.signal.combineWith(pBPos.signal).map((p1, p2) => if p1.x < p2.x then p1 else p2)
    val p2Pos = pAPos.signal.combineWith(pBPos.signal).map((p1, p2) => if p1.x < p2.x then p2 else p1)

    val pA = svg.circle(
      bem("pA", Map("relabel" -> info.map(_.relabel))).svg,
      svg.cx <-- pAPos.signal.map(_.x.toString),
      svg.cy <-- pAPos.signal.map(_.y.toString),
      svg.r("2")
    )

    val pB = svg.circle(
      bem("pB", Map("relabel" -> info.map(_.relabel))).svg,
      svg.cx <-- pBPos.signal.map(_.x.toString),
      svg.cy <-- pBPos.signal.map(_.y.toString),
      svg.r("2")
    )

    val hideShadowPoints = config
      .combineWith(info)
      .map((config, info) =>
        !config.shadowPoints || info.x1.isEmpty || info.x2.isEmpty || info.y1.isEmpty || info.y2.isEmpty
      )

    val shadowP1 = svg.circle(
      bem("p1-shadow", Map("hide" -> hideShadowPoints)).svg,
      svg.cx <-- info.combineWith(config).map((i, c) => (i.x1.getOrElse(0.0) + c.offset.x).toString),
      svg.cy <-- info.combineWith(config).map((i, c) => (i.y1.getOrElse(0.0) + c.offset.y).toString),
      svg.r("2")
    )

    val shadowP2 = svg.circle(
      bem("p2-shadow", Map("hide" -> hideShadowPoints)).svg,
      svg.cx <-- info.combineWith(config).map((i, c) => (i.x2.getOrElse(0.0) + c.offset.x).toString),
      svg.cy <-- info.combineWith(config).map((i, c) => (i.y2.getOrElse(0.0) + c.offset.y).toString),
      svg.r("2")
    )

    def onMove(v: Var[Point], p: Point): Unit =
      def toAbsolute(p: Point): Point =
        if config.now().smallScreen then p * 60 - Point(10, 10)
        else p * 100 - Point(50, 50)

      def round(p: Point): Point =
        if config.now().snap then p.round
        else p

      val newP = p
        .pipe(toAbsolute)
        .pipe(round)
      v.set(newP)

    svg.svg(
      bem("diagram").svg,
      svg.className("diagram"),
      svg.viewBox <-- config.map(c => if c.smallScreen then "-10 -10 60 60" else "-50 -50 100 100"),
      // all the formulas are given in math coordinates, so it is easier to just flip the y axis
      svg.transform("scale(1, -1)"),
      renderGrid(),
      renderOrigCatenary(info, config).amend(bem("orig-catenary", Map("show" -> config.map(_.origCurve))).svg),
      renderMovedCatenary(info),
      renderHv(info).amend(bem("hv-main", Map("show" -> config.map(_.hv))).svg),
      inContext(ctx =>
        List(
          pA.amend(Mover(ctx.ref) --> (p => onMove(pAPos, p))),
          pB.amend(Mover(ctx.ref) --> (p => onMove(pBPos, p)))
        )
      ),
      List(
        "P1" -> p1Pos,
        "P2" -> p2Pos
      ).map((name, p) =>
        invertedAt(
          p,
          svg.text(
            bem("p1p2-label").svg,
            svg.fontSize("2.0"),
            svg.textAnchor("middle"),
            svg.dominantBaseline("middle"),
            name
          )
        )
      ),
      shadowP1,
      shadowP2
    )

  private def renderGrid(): SvgElement =
    svg.g(
      bem("grid").svg,
      (-50 to 50 by 10)
        .map(x =>
          List(
            svg.line(bem("grid-line-x").svg, svg.x1(x.toString), svg.x2(x.toString), svg.y1("-50"), svg.y2("50")),
            svg.line(bem("grid-line-y").svg, svg.x1("-50"), svg.x2("50"), svg.y1(x.toString), svg.y2(x.toString))
          )
        ),
      (-40 to 40 by 10)
        .filter(_ != 0)
        .map(x =>
          List(
            invertedAt(
              Point(x, -2.5),
              svg.text(
                bem("grid-label-x").svg,
                svg.fontSize("2"),
                svg.textAnchor("middle"),
                svg.dominantBaseline("middle"),
                x.toString
              )
            ),
            invertedAt(
              Point(-1, x),
              svg.text(
                bem("grid-label-y").svg,
                svg.fontSize("2"),
                svg.textAnchor("end"),
                svg.dominantBaseline("middle"),
                x.toString
              )
            )
          )
        ),
      (-50 to 50 by 1).map(x =>
        val large    = x % 5 == 0
        val (y1, y2) =
          if large then ("-1", "1")
          else ("-0.5", "0.5")

        List(
          svg.line(
            bem("grid-tick", Map("large" -> large)).svg,
            svg.x1(x.toString),
            svg.x2(x.toString),
            svg.y1(y1),
            svg.y2(y2)
          ),
          svg.line(
            bem("grid-tick", Map("large" -> large)).svg,
            svg.x1(y1),
            svg.x2(y2),
            svg.y1(x.toString),
            svg.y2(x.toString)
          )
        )
      ),
      svg.line(bem("global-x-axis").svg, svg.x1("-50"), svg.x2("50"), svg.y1("0"), svg.y2("0")),
      svg.line(bem("global-y-axis").svg, svg.x1("0"), svg.x2("0"), svg.y1("-50"), svg.y2("50"))
    )

  private def renderHv(info: Signal[CatInfo]): SvgElement =
    final case class HvInfo(
        p1: Point,
        p2: Point
    ):
      def left: Double       = Math.min(p1.x, p2.x)
      def right: Double      = Math.max(p1.x, p2.x)
      def hMiddle: Double    = (left + right) / 2
      def top: Double        = Math.max(p1.y, p2.y)
      def bottom: Double     = Math.min(p1.y, p2.y)
      def vMiddle: Double    = (top + bottom) / 2
      def topPoint: Point    = if p1.y > p2.y then p1 else p2
      def bottomPoint: Point = if p1.y > p2.y then p2 else p1

    val i = info.map(i => HvInfo(i.p1, i.p2))

    svg.g(
      svg.path(
        bem("hv").svg,
        svg.d <-- i.map(i => s"""
            M ${i.left} ${i.top + 5}
            m 0 2
            l 0 -4
            m 0 2
            l ${i.right - i.left} 0
            m 0 2
            l 0 -4
          """)
      ),
      svg.path(
        bem("hv", Map("dashed" -> true)).svg,
        svg.d <-- i.map(i => s"""
            M ${i.left} ${i.top + 3}
            L ${i.p1.x} ${i.p1.y}
          """)
      ),
      svg.path(
        bem("hv", Map("dashed" -> true)).svg,
        svg.d <-- i.map(i => s"""
            M ${i.right} ${i.top + 3}
            L ${i.p2.x} ${i.p2.y}
          """)
      ),
      invertedAt(
        i.map(i => Point(i.hMiddle, i.top + 6)),
        svg.text(
          bem("hv-label").svg,
          svg.fontSize("2.0"),
          svg.textAnchor("middle"),
          svg.dominantBaseline("middle"),
          "H"
        )
      ),
      svg.path(
        bem("hv").svg,
        svg.d <-- i.map(i => s"""
            M ${i.right + 5} ${i.top}
            m 2 0
            l -4 0
            m 2 0
            l 0 ${i.bottom - i.top}
            m 2 0
            l -4 0
          """)
      ),
      svg.path(
        bem("hv", Map("dashed" -> true)).svg,
        svg.d <-- i.map(i => s"""
            M ${i.right + 3} ${i.top}
            L ${i.topPoint.x} ${i.topPoint.y}
          """)
      ),
      svg.path(
        bem("hv", Map("dashed" -> true)).svg,
        svg.d <-- i.map(i => s"""
            M ${i.right + 3} ${i.bottom}
            L ${i.bottomPoint.x} ${i.bottomPoint.y}
          """)
      ),
      invertedAt(
        i.map(i => Point(i.right + 6, i.vMiddle)),
        svg.text(
          bem("hv-label").svg,
          svg.fontSize("2.0"),
          svg.textAnchor("middle"),
          svg.dominantBaseline("middle"),
          "v"
        )
      )
    )

  private def renderMovedCatenary(info: Signal[CatInfo]): SvgElement =
    val path = info.map(info => pointPath(movedCatenary(info)))

    svg.path(
      bem("moved-catenary").svg,
      svg.d <-- path
    )

  private def renderOrigCatenary(info: Signal[CatInfo], config: Signal[Config]): SvgElement =
    val path = info.combineWith(config).map((info, config) => pointPath(origCatenary(info, config)))

    svg.path(
      bem("orig-catenary").svg,
      svg.d <-- path
    )

  private def pointPath(points: List[Point]): String =
    points
      .filter(p => p.x > -100 && p.x < 100 && p.y > -100 && p.y < 100)
      .map(p => f"${p.x}%.3f ${p.y}%.3f")
      .mkString("M ", " L", "")

  private def movedCatenary(i: CatInfo): List[Point] =
    (
      for
        x1      <- i.x1
        a       <- i.a
        xOffset <- i.xOffset
        yOffset <- i.yOffset
      yield (0 until 1000).toList
        .map(r =>
          val xx = x1 + r / 1000.0 * i.H
          val y  = a * Math.cosh((xx) / a)
          Point(xx + xOffset, y + yOffset)
        )
    ).getOrElse(Nil)

  private def origCatenary(i: CatInfo, config: Config): List[Point] =
    (
      for
        x1 <- i.x1
        a  <- i.a
      yield (-500 until 500).toList
        .map(_ / 10.0)
        .map(x => Point(x, a * Math.cosh((x) / a)) + config.offset)
    ).getOrElse(Nil)

  private def invertedAt(p: Point, elem: SvgElement): SvgElement =
    invertedAt(Signal.fromValue(p), elem)

  private def invertedAt(p: Signal[Point], elem: SvgElement): SvgElement =
    svg.g(
      svg.transform <-- p.map(p => s"translate(${p.x}, ${p.y})"),
      svg.g(
        svg.transform("scale(1, -1)"),
        elem
      )
    )
