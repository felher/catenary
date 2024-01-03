package org.felher.catenary

final case class Point(x: Double, y: Double) derives CanEqual, io.circe.Codec.AsObject:
  def +(v: Point): Point          = Point(x + v.x, y + v.y)
  def -(v: Point): Point          = Point(x - v.x, y - v.y)
  def *(d: Double): Point         = Point(x * d, y * d)
  def /(d: Double): Point         = Point(x / d, y / d)
  def length: Double              = Math.sqrt(x * x + y * y)
  def distance(p2: Point): Double = (this - p2).length
  def toSvg: String               = s"${x} ${y}"
  def round: Point                = Point(x.round, y.round)
