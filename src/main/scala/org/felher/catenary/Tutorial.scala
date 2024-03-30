package org.felher.catenary

import com.raquo.laminar.api.L.*
import monocle.Lens
import monocle.macros.*

object Tutorial:
  val bem = Bem("tutorial")

  def render(
      config: Var[Config],
      catInfo: Signal[CatInfo]
  ): HtmlElement =
    div(
      bem,
      div(
        LabeledCheckbox
          .forObject("I have a small screen", config, GenLens[Config](_.smallScreen))
          .amend(bem("control")),
        bem("step"),
        div(
          bem("label"),
          "Problem Statement"
        ),
        div(
          bem("description"),
          MiniMarkdown.render(
            """
              If you have taken a look at some rope hanging between
              two points, you might have thought that it looks like
              a parabola. But it is not. It is a catenary. And its
              shape is described by the following formula:
              ![images/main-func.svg]
              where *cosh* is the
              [hyperbolic cosine function](https://en.wikipedia.org/wiki/Hyperbolic_functions).

              The problem we want to solve here is that we have
              two points *P1* and *P2* in a 2D plane and
              we want to find the curve that describes how a
              rope of length *L* would hang between these two points.

              This tutorial is more on the getting-things-done side.
              If you want to have fun with math, go to 
              [Wikipedia](https://en.wikipedia.org/wiki/Catenary).
              This is also where I stole the formulas from.
            """
          )
        )
      ),
      div(
        bem("step"),
        div(
          bem("label"),
          "Step 1 - Getting the point across"
        ),
        div(
          bem("description"),
          MiniMarkdown.render(
            """
              The first thing we do is relabel our two points *P1* and *P2*.
              If you drag around the points in the diagram,
              you will see that we always call the left point *P1*
              and the right point *P2*. If you want to set a discrete
              position, you can enable the "Snap to grid" option:
            """
          ),
          LabeledCheckbox.forObject("Snap to Grid", config, GenLens[Config](_.snap)).amend(bem("control")),
          MiniMarkdown.render(
            """
              Given our renaming, we define *(x1, y1)* as the coordinates
              of *P1* and *(x2, y2)* as the coordinates of *P2*. In our case,
              we have:
            """
          ),
          div(bem("calculation"), (child.text <-- catInfo.map(i => f"(x1, y1) = (${i.p1.x}%.2f, ${i.p1.y}%.2f)"))),
          " and ",
          div(bem("calculation"), (child.text <-- catInfo.map(i => f"(x2, y2) = (${i.p2.x}%.2f, ${i.p2.y}%.2f)")))
        )
      ),
      div(
        bem("step"),
        div(
          bem("label"),
          "Step 2 - Shaping up"
        ),
        div(
          bem("description"),
          MiniMarkdown.render(
            """
              Now that we got our coordinates sorted out, we can
              try to find the shape of the curve. As a quick reminder,
              the formula for the curve is:
              ![images/main-func.svg]
              It only depends on *a*. So what we want to do is
              calculate *a* from the coordinates of *P1* and *P2*.
              *a* can be seen as a measure of the tension of the curve.
              The larger *a* is, the straighter the curve connecting
              our two points. The lower *a* is, the more the curve
              will sag. *a* is always positive.
            """
          ),
          MiniMarkdown.render(
            """
              To calculate *a* we need 3 parameters. The
              first one is the length of the rope *L*. The
              length is an input to the algorithm and not
              deduced from the coordinates. If you want to fiddle around with
              it a bit, here is a slider for you:
            """
          ),
          LabeledSlider.forObject("Length", 0, 100, config, GenLens[Config](_.length)).amend(bem("control")),
          MiniMarkdown.render(
            """
              The next parameter is the horizontal distance between
              the two points. We call it *H*. The other parameter is
              the vertical distance between the two points. We call
              it *v*.
            """
          ),
          LabeledCheckbox.forObject("Show H and v", config, GenLens[Config](_.hv)).amend(bem("control")),
          MiniMarkdown.render(
            """
              Let's calculate *H* and *v* from the coordinates of *P1* and *P2*:
            """
          ),
          div(
            bem("calculation"),
            (child.text <-- catInfo.map(i => f"H = x2 - x1 = ${i.p2.x}%.2f -  ${i.p1.x}%.2f = ${i.H}%.2f"))
          ),
          " and ",
          br(),
          div(
            bem("calculation"),
            (child.text <-- catInfo.map(i => f"v = y2 - y1 = ${i.p2.y}%.2f -  ${i.p1.y}%.2f = ${i.v}%.2f"))
          )
        ),
        MiniMarkdown.render(
          """
            Armed with that we can take a look at this nice equation we got from Wikipedia:
            ![images/a-equation.svg]
            It would be nice if we could just solve for *a* and be done with it.
            Alas, there is no algebraic solution for it and we need to solve it
            numerically. Which in our case means that we just try various
            values for *a* so that the left side is a close to the right side as
            possible. The good thing is that *sinh(x)/x* is strictly monotonic
            for *x > 0*, so there is only one solution for *a*. It might not be
            immediately obvious that our function is of that form, but if you
            set *x = H/2a*, then *1/x = 2a/H* and you get exactly the right
            side of the equation.


            There are many ways to find a numerical solution. A common one is just
            getting everything on one side and zero on the other side and
            then using something like the
            [Newton-Raphson method](https://en.wikipedia.org/wiki/Newton%27s_method).

            The solution implemented here is a bit dumber. It just steps through
            the search space, doubling or halving the step size and changing the step direction
            depending on whether we over- or undershot the target.
            It's not very efficient, but it's easy to understand and
            implement. And it's fast enough for our purposes.

            If you want to take a look at it you can head over to
            [GitHub](https://github.com/felher/catenary).

            If we run an approximation, we arrive at a value for *a* that is roughly:
          """
        ),
        singleValue("a", catInfo.map(_.a)),
        MiniMarkdown.render(
          """
            Now that we have *a*, we can draw the shape of the curve. Let's take
            a look at the equation for *a* again:
            ![images/a-equation.svg]
            The values besides *a* are the three parameters *L*, *H* and *v*.
            None of those depend on the exact location of our points *P1* and *P2*.
            They only depend on how far the points are apart and the length of
            the rope. This means that when we draw the curve using the 
            function
            ![images/main-func.svg]
            we can't get a curve that goes through *P1* and *P2*. Or if we do, only
            because we got lucky. In fact, the curve we get is centered around the
            y-axis. 

          """
        ),
        LabeledCheckbox.forObject("Show curve", config, GenLens[Config](_.origCurve)).amend(bem("control")),
        MiniMarkdown.render(
          """
            But if you squint your eyes a bit, you can see that the curve is
            the same shape as the rope hanging between *P1* and *P2*. It's just
            shifted horizontally and vertically. In fact, here are two
            sliders for you to change the offset of the curve:
          """
        ),
        LabeledSlider.forObject("X-Offset", -50, 50, config, GenLens[Config](_.offset.x), 0.1).amend(bem("control")),
        LabeledSlider.forObject("Y-Offset", -50, 50, config, GenLens[Config](_.offset.y), 0.1).amend(bem("control")),
        div(bem("clickable"), "Reset", onClick --> (_ => config.update(_.copy(offset = Point(0, 0))))),
        MiniMarkdown.render(
          """
            So if we want to draw the curve between *P1* and *P2*,
            we need to find the horizontal and vertical offset.
          """
        )
      ),
      div(
        bem("step"),
        div(
          bem("label"),
          "Step 3 - Last Shift "
        ),
        div(
          bem("description"),
          MiniMarkdown.render(
            """
              The final problem is to shift the curve so it matches with our points.
              For this, we use another useful formula. Since we have
              ![images/main-func.svg]
              which gives us the *y*-coordinate to our *x* we know that to get *v*
              we need to subtract *y* at *x1* from *y* at *x2*, which leaves
              us with
              ![images/v-equation.svg]
              Here, the hat symbol means that we are using the *x1* and *x2*
              on the curve, not the ones from our points. It doesn't matter,
              because the *v* and *H* are the same, no matter where exactly our
              points are.
              We also know the horizontal distance between *x1* and *x2*, which
              is *H*. So we can rewrite the formula as
              ![images/v-equation-2.svg]
              This is neat because we can now numerically approximate 
              *x̂1*. Doing that gives us:
            """
          )
        ),
        singleValue("x̂1", catInfo.map(_.x1)),
        MiniMarkdown.render(
          """
            Now, getting the *y* value is as easy as just using
            ![images/main-func.svg]
            which yields:
          """
        ),
        singleValue("ŷ1", catInfo.map(_.y1)),
        MiniMarkdown.render(
          """
            We don't need it, but the same can be done for *x̂2* and *ŷ2*.
          """
        ),
        singleValue("x̂2", catInfo.map(_.x2)),
        singleValue("ŷ2", catInfo.map(_.y2)),
        MiniMarkdown.render(
          """
            If we plot those values we can see the shadow points for
            *P1* and *P2*, respectively, on the original curve:
          """
        ),
        LabeledCheckbox.forObject("Show shadow points", config, GenLens[Config](_.shadowPoints)).amend(bem("control")),
        MiniMarkdown.render(
          """
            Now we are nearly done. We just subtract (*x̂1*, *ŷ1*) from
            (*x1*, *y1*) to get our offset, which in our case is:
          """
        ),
        singleValue("xOffset", catInfo.map(_.xOffset)),
        singleValue("yOffset", catInfo.map(_.yOffset)),
        MiniMarkdown.render(
          """
            And we are done. If we want to draw the curve between *P1* and *P2*,
            we just sample the original curve between *x̂1* and *x̂2* and shift
            the resulting points by (*xOffset*, *yOffset*).

            If you want to see the code that does all that, you can find it
            at [GitHub](https://github.com/felher/catenary).
          """
        )
      )
    )

  private def singleValue(label: String, value: Signal[Option[Double]]): HtmlElement =
    div(
      bem("calculation"),
      child.text <-- value.map({
        case None    => "N/A - Maybe you need to move the points around a bit?"
        case Some(d) => f"$label%s = $d%.2f"
      })
    )
