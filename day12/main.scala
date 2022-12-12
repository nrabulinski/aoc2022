import scala.collection.immutable.List
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math.abs

case class Point(x: Int, y: Int):
  def dist(other: Point): Int =
    abs(x - other.x)
    + abs(y - other.y)
end Point

def canMove(
  board: Array[Array[Int]],
  from: Point,
  to: Point,
): Boolean =
  val Point(fromX, fromY) = from
  val Point(toX, toY) = to
  val fromH = board(fromY)(fromX)
  val toH = board(toY)(toX)
  (toH - fromH) < 2

def processNeighbor(
  current: Point,
  neighbor: Point,
  end: Point,
  currentG: Int,
  open: ArrayBuffer[Point],
  closed: ArrayBuffer[(Point, Point)],
  gScores: ArrayBuffer[(Point, Int)],
  fScores: ArrayBuffer[(Point, Int)],
): Unit =
  val gScore = currentG + 1
  val gIdx = gScores
    .indexWhere { case (p, _) => p == neighbor }
  if gIdx == -1 || gScore < gScores(gIdx)._2 then
    val cIdx = closed
      .indexWhere { case (p, _) => p == neighbor }

    if cIdx == -1 then
      closed.addOne((neighbor, current))
    else
      closed.update(cIdx, (neighbor, current))

    if gIdx == -1 then
      gScores.addOne((neighbor, gScore))
    else
      gScores.update(gIdx, (neighbor, gScore))
    
    val fIdx = fScores
      .indexWhere { case (p, _) => p == neighbor }
    val fScore = gScore + neighbor.dist(end)
    if fIdx == -1 then
      fScores.addOne((neighbor, fScore))
    else
      fScores.update(fIdx, (neighbor, fScore))
    
    if !open.contains(neighbor) then
      open.addOne(neighbor)

def pathLength(
  end: Point,
  closed: ArrayBuffer[(Point, Point)],
): Int =
  var res = 0
  var current = end
  while true do
    closed
      .find { case (p, _) => p == current } match
    case Some((_, newCurrent)) =>
      res += 1
      current = newCurrent
    case None => return res
  0

def aStar(
  board: Array[Array[Int]],
  start: Point,
  end: Point,
): Int =
  val maxY = board.length - 1
  val maxX = board(0).length - 1
  var open = ArrayBuffer(start)
  var closed = ArrayBuffer.empty[(Point, Point)]
  var gScores = ArrayBuffer((start, 0))
  var fScores = ArrayBuffer((start, start.dist(end)))
  while !open.isEmpty do
    val (current, _) = fScores
      .filter { case (p, _) => open.contains(p) }
      .minBy { case (_, s) => s }
    val Point(x, y) = current
    val (_, currentG) = gScores
      .find { case (p, _) => p == current }
      .get

    if current == end then
      return pathLength(current, closed)
    
    open.remove(open.indexOf(current))
  
    // Neighbor up
    if y > 0 && canMove(board, current, Point(x, y - 1)) then
      val neighbor = Point(x, y - 1)
      processNeighbor(
        current,
        neighbor,
        end,
        currentG,
        open,
        closed,
        gScores,
        fScores,
      )

    // Neighbor down
    if y < maxY && canMove(board, current, Point(x, y + 1)) then
      val neighbor = Point(x, y + 1)
      processNeighbor(
        current,
        neighbor,
        end,
        currentG,
        open,
        closed,
        gScores,
        fScores,
      )

    // Neighbor left
    if x > 0 && canMove(board, current, Point(x - 1, y)) then
      val neighbor = Point(x - 1, y)
      processNeighbor(
        current,
        neighbor,
        end,
        currentG,
        open,
        closed,
        gScores,
        fScores,
      )

    // Neighbor right
    if x < maxX && canMove(board, current, Point(x + 1, y)) then
      val neighbor = Point(x + 1, y)
      processNeighbor(
        current,
        neighbor,
        end,
        currentG,
        open,
        closed,
        gScores,
        fScores,
      )

  0

@main
def main() =
  var start = Point(0, 0)
  var end = Point(0, 0)
  val board = Source
    .fromFile("assets/day12")
    .getLines
    .zipWithIndex
    .map { (line, y) =>
      line
        .split("")
        .zipWithIndex
        .map { (field, x) =>
          if field == "S" then
            start = Point(x, y)
            'a'.toInt
          else if field == "E" then
            end = Point(x, y)
            'z'.toInt
          else
            field(0).toInt
        }
        .toArray
    }
    .toArray
  val part1 = aStar(board, start, end)
  println(s"Part 1: $part1")

  // TODO: Optimize this, lol
  // Actually takes like over a minute to compute
  val part2 = board
    .zipWithIndex
    .flatMap { (line, y) =>
      line
        .zipWithIndex
        .map((e, x) => (e, Point(x, y)))
    }
    .filter { case (elevation, _) => elevation == 'a'.toInt }
    .map { case (_, start) => aStar(board, start, end) }
    .filter(_ != 0)
    .min
  println(s"Part 2: $part2")
  
  // board
  //   .zipWithIndex
  //   .foreach { (line, y) =>
  //     line
  //       .zipWithIndex
  //       .foreach { (_, x) =>
  //         val curr = Point(x, y)
  //         print(if part1.contains(curr) then '#' else '.')
  //       }
  //     println()
  //   }