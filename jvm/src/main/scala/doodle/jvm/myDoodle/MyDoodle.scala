package doodle.jvm.myDoodle

import doodle.core._
import doodle.backend._
import doodle.jvm.Java2DCanvas

// Assumptions
// x & y in elements are positive
// Starting point of Path is relative to left bottom corner of bounding box
// Path is drawn inside bounding box and starts in left bottom corner

sealed trait Element

final case class MoveTo(x: Double, y: Double) extends Element

final case class LineTo(x: Double, y: Double) extends Element

final case class BezierCurveTo(cp1x: Double, cp1y: Double, cp2x: Double, cp2y: Double, endX: Double, endY: Double) extends Element


case class Path(elements: Seq[Element]) {
  private val right = elements.collect {
    case MoveTo(x, _) => x
    case LineTo(x, _) => x
    case BezierCurveTo(x1, _, x2, _, x3, _) => Seq(x1, x2, x3).max
  }.max
  private val top = elements.collect {
    case MoveTo(_, y) => y
    case LineTo(_, y) => y
    case BezierCurveTo(_, y1, _, y2, _, y3) => Seq(y1, y2, y3).max
  }.max
  private val left = elements.collect {
    case MoveTo(x, _) => x
    case LineTo(x, _) => x
    case BezierCurveTo(x1, _, x2, _, x3, _) => Seq(x1, x2, x3).max
  }.min
  private val bottom = elements.collect {
    case MoveTo(_, y) => y
    case LineTo(_, y) => y
    case BezierCurveTo(_, y1, _, y2, _, y3) => Seq(y1, y2, y3).max
  }.min

  def center = ((right - left) / 2, (top - bottom) / 2)

  def height = top - bottom

  def width = right - left

}


case class Image(path: Path, strokeColor: Color, fillColor: Color) {
  //  def leftOf(other: Image): Image = ???
  //
  //  def inFrontOf(other: Image): Image = ???
  //
  //  def strokeColor(color: Color): Image = ???
  //
  //  def fillColor(color: Color): Image = ???
  //
  def draw(canvas: Canvas): Unit = {
    canvas.beginPath()
    for (element <- path.elements) {
      element match {
        case MoveTo(x, y) => canvas.moveTo(x, y)
        case LineTo(x, y) => canvas.lineTo(x, y)
        case BezierCurveTo(x1, y1, x2, y2, x3, y3) => canvas.bezierCurveTo(x1, y1, x2, y2, x3, y3)
      }
    }
    canvas.endPath()
    canvas.setStroke(Stroke(3.0, strokeColor, Line.Cap.Round, Line.Join.Round))
    canvas.stroke()
    canvas.setFill(fillColor)
    canvas.fill()
  }
}

object SimpleShapes {
  def circle(radius: Double, up: Double = 0, left: Double = 0): Path = {
    val c = 0.551915024494
    val cR = c * radius
    val centerX = radius - left
    val centerY = radius + up
    Path(Seq(
      MoveTo(centerX, centerY + radius),
      BezierCurveTo(centerX + cR, centerY + radius,
        centerX + radius, centerY + cR,
        centerX + radius, centerY),
      BezierCurveTo(centerX + radius, centerY - cR,
        centerX + cR, centerY - radius,
        centerX, centerY - radius),
      BezierCurveTo(centerX - cR, centerY - radius,
        centerX - radius, centerY - cR,
        centerX - radius, centerY),
      BezierCurveTo(centerX - radius, centerY + cR,
        centerX - cR, centerY + radius,
        centerX, centerY + radius)
    ))
  }

  def rectangle(width: Double, height: Double): Path = {
    val left = 0
    val top = height
    Path(Seq(
      MoveTo(left, top),
      LineTo(left + width, top),
      LineTo(left + width, top - height),
      LineTo(left, top - height),
      LineTo(left, top)
    ))
  }
}

object MyDoodle extends App {
  val canvas = Java2DCanvas.canvas
  val sampleImage = Image(SimpleShapes.circle(100, 50, 50), Color.red, Color.black)
  sampleImage.draw(canvas)
  val sampleImage2 = Image(SimpleShapes.rectangle(100, 200), Color.red, Color.black)
  sampleImage2.draw(canvas)
}
