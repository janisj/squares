package janis.squares

sealed trait Color {
  def print: String
}
case object Red   extends Color {
  def print = "x"
}
case object Black extends Color {
  def print = "o"
}
case object Empt  extends Color {
  def print = "-"
}

trait Square {
  def rotate90: Square
  def mirrorH: Square
  def mirrorV: Square
  def toString2: String
}

case class Square3x3(x1: Color, x2: Color, x3: Color, x4: Color, x5: Color, x6: Color, x7: Color, x8: Color, x9: Color)
    extends Square { self =>
  override def rotate90: Square = Square3x3(x7, x4, x1, x8, x5, x2, x9, x6, x3)
  override def mirrorH: Square  = Square3x3(x7, x8, x9, x4, x5, x6, x1, x2, x3)
  override def mirrorV: Square  = Square3x3(x3, x2, x1, x6, x5, x4, x9, x8, x7)
  def toString2                 = s"${self.x1.print} ${self.x2.print} ${self.x3.print}\n" +
    s"${self.x4.print} ${self.x5.print} ${self.x6.print}\n" +
    s"${self.x7.print} ${self.x8.print} ${self.x9.print}\n"
}
object Square3x3   {
  def apply(a: Array[Color]): Square3x3 = new Square3x3(a(0), a(1), a(2), a(3), a(4), a(5), a(6), a(7), a(8))
  def apply(redInd1: Int, redInd2: Int, blackInd1: Int, blackInd2: Int): Square3x3 = {
    val coloredArray: Array[Color] = Range(1, 10)
      .map(_ => Empt)
      .toArray
    coloredArray(redInd1 - 1) = Red
    coloredArray(redInd2 - 1) = Red
    coloredArray(blackInd1 - 1) = Black
    coloredArray(blackInd2 - 1) = Black
    Square3x3(coloredArray)
  }
  val allSquares: List[Square3x3] = {
    val indexCombinations = for {
      i <- 1 until 10
      j <- i + 1 until 10
    } yield (i, j)

    val redsWithAllBlacks = indexCombinations.map(aa =>
      (aa, indexCombinations.filterNot(bb => bb._1 == aa._1 || bb._1 == aa._2 || bb._2 == aa._1 || bb._2 == aa._2)))
    val allCombinations   = redsWithAllBlacks.flatMap(t => t._2.map(x => (t._1, x)))
    allCombinations.map(t => Square3x3(t._1._1, t._1._2, t._2._1, t._2._2)).toList
  }
}
object BFUniqueSquares extends App {

  def isEquals(square: Square, square1: Square): Boolean =
    square == square1 ||
      square == square1.rotate90 ||
      square == square1.rotate90.rotate90 ||
      square == square1.rotate90.rotate90.rotate90 ||
      square == square1.mirrorH ||
      square == square1.mirrorH.rotate90 ||
      square == square1.mirrorH.rotate90.rotate90 ||
      square == square1.mirrorH.rotate90.rotate90.rotate90 ||
      square == square1.mirrorV ||
      square == square1.mirrorV.rotate90 ||
      square == square1.mirrorV.rotate90.rotate90 ||
      square == square1.mirrorV.rotate90.rotate90.rotate90

  def unique(item: Square, others: List[Square]): List[Square] =
    others match {
      case h :: Nil  => if (isEquals(item, h)) Nil else List(item)
      case h :: tail => if (isEquals(item, h)) Nil else unique(item, tail)
      case Nil       => Nil
    }

  def findUniques(l: List[Square]): List[Square] =
    l match {
      case h :: Nil  => List(h)
      case h :: tail => unique(h, tail) ++ findUniques(tail)
      case Nil       => Nil
    }

  val uniqueSquares = findUniques(Square3x3.allSquares)
  println(s"Total ${uniqueSquares.size} 3x3 configurations found:")
  uniqueSquares.reverse.foreach(s => println(s.toString2))
}
