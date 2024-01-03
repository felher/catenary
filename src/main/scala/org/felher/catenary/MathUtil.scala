package org.felher.catenary

object MathUtil:
  def approximate(
      start: Double,
      f: Double => Double,
      positiveSlope: Boolean,
      targetY: Double,
      stepSize: Double,
      minError: Double,
      maxSteps: Int
  ): Option[Double] =
    sealed trait Dir derives CanEqual
    sealed trait SingleDir extends Dir derives CanEqual
    case object DirNone    extends Dir
    case object DirBoth    extends Dir
    case object DirLeft    extends SingleDir
    case object DirRight   extends SingleDir

    def go(x: Double, stepSize: Double, previousDirs: Dir, curStep: Int): Option[Double] =
      if curStep == maxSteps then None
      else
        val error = f(x) - targetY

        if Math.abs(error) < minError then Some(x)
        else
          val dir             = if (error > 0) == positiveSlope then DirLeft else DirRight
          val newPreviousDirs =
            if previousDirs == DirNone then dir
            else if previousDirs == dir then dir
            else DirBoth

          val factor      = if dir == DirLeft then -1 else 1
          val newStepSize = if newPreviousDirs == DirBoth then stepSize / 2 else stepSize * 2
          val newX        = x + factor * newStepSize
          go(newX, newStepSize, newPreviousDirs, curStep + 1)

    go(start, stepSize, DirNone, 0)
