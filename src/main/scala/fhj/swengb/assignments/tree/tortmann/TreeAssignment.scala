package fhj.swengb.assignments.tree.tortmann

import javafx.scene.paint.Color

import scala.math.BigDecimal.RoundingMode
import scala.util.Random

object Graph {

  val counter = 1
  val colorMap =
    Map[Int, Color](
      0 -> Color.DARKBLUE,
      1 -> Color.MIDNIGHTBLUE,
      2 -> Color.MEDIUMBLUE,
      3 -> Color.DODGERBLUE,
      4 -> Color.STEELBLUE,
      5 -> Color.LIGHTBLUE,
      6 -> Color.SILVER,
      7 -> Color.WHITE,
      8 -> Color.ANTIQUEWHITE
    )

  def randomTree(pt: Pt2D): Tree[L2D] =
    mkGraph(pt, Random.nextInt(360), Random.nextDouble() * 150, Random.nextInt(7))

  def traverse[A, B](tree: Tree[A])(convert: A => B): Seq[B] = tree match{
        case Node(value) => Seq(convert(value))
        case Branch(left,right) => traverse(left)(convert) ++ traverse(right)(convert)
    }

  def mkGraph(start: Pt2D,
              initialAngle: AngleInDegrees,
              length: Double,
              treeDepth: Int,
              factor: Double = 0.75,
              angle: Double = 35.0,
              colorMap: Map[Int, Color] = Graph.colorMap): Tree[L2D] = {
    assert(treeDepth <= colorMap.size, s"Treedepth higher than color mappings - bailing out ...")

    /** REMINDER: required parameters for L2D
      * def apply(start: Pt2D, angle: AngleInDegrees, length: Double, color: Color): L2D = {}
      * val counter is initialized with the value 1
      * */

    def constructTree(root : L2D, counter: Int): Tree[L2D] = counter match {
      case rootOnly if treeDepth == 0 => Node(root)
      case nodesExist if counter == treeDepth =>
        Branch(Node(root),                                                   /**constructing the root Node of the respective branch*/
          Branch(
              Node(root.left(factor,angle,colorMap(counter-1))),             /**constructing the left side of the respective branch*/
              Node(root.right(factor,angle,colorMap(counter-1)))             /**constructing the right side of the respective branch*/
          )
        )
      case _ =>
        Branch(Node(root),
          Branch(
            constructTree(root.left(factor,angle,colorMap(counter-1)),counter+1),
            constructTree(root.right(factor,angle,colorMap(counter-1)),counter+1)
          )
        )
    }
    constructTree(L2D(start,initialAngle,length,colorMap(counter-1)),counter)
  }
}

object MathUtil {

  /**using the method BidDecimal from the scala.math package object
    *  BigDecimal(input).setScale(precision,define rounding mode up/down) -> convert to a Double (...).toDouble
  */

  def round(value: Double): Double = {
    val roundedValue = BigDecimal(value).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
    return roundedValue
  }

  /** using the method toRadians(x: Double): Double from the scala.math package object
    * to convert degrees to radiant
  */

  def toRadians(angle: AngleInDegrees): AngleInRadiants = {
    val convertedAngle = angle.toRadians
    return convertedAngle
  }
}

object L2D {

  import MathUtil._

  def apply(start: Pt2D, angle: AngleInDegrees, length: Double, color: Color): L2D = {
    val coordinateX = start.x + round(math.cos(toRadians(angle))*length)
    val coordinateY = start.y + round(math.sin(toRadians(angle))*length)
    val end = Pt2D(coordinateX,coordinateY)
    return L2D(start,end,color)
  }
}

case class L2D(start: Pt2D, end: Pt2D, color: Color) {

  lazy val xDist = end.x - start.x
  lazy val yDist = end.y - start.y

  lazy val angle = {
    assert(!((xDist == 0) && (yDist == 0)))
    (xDist, yDist) match {
      case (x, 0) if x > 0 => 0
      case (0, y) if y > 0 => 90
      case (0, y) if y < 0 => 270
      case (x, 0) if x < 0 => 180
      case (x, y) if x < 0 && y < 0 => Math.atan(y / x) * 180 / Math.PI + 180
      case (x, y) if x < 0 && y > 0 => Math.atan(y / x) * 180 / Math.PI + 180
      case (x, y) if x > 0 && y < 0 => Math.atan(y / x) * 180 / Math.PI + 360
      case (x, y) => Math.atan(y / x) * 180 / Math.PI
    }
  }

  lazy val length: Double = {
    Math.sqrt(xDist * xDist + yDist * yDist)
  }

  def left(factor: Double, deltaAngle: AngleInDegrees, c: Color): L2D = {
    L2D(end, angle - deltaAngle, length * factor, c)
  }

  def right(factor: Double, deltaAngle: AngleInDegrees, c: Color): L2D = {
    L2D(end, angle + deltaAngle, length * factor, c)
  }
}

