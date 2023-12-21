import scala.compiletime.ops.boolean

import scala.io.Source
import scala.util.matching.Regex

val TEST = false
val DEBUG = false

val cwd = System.getProperty("user.dir")
println(s"cwd: $cwd")

val INPUTFILE = if (TEST) "day18/test.txt" else "day18/input.txt"

val RIGHT = 'R'
val LEFT = 'L'
val UP = 'U'
val DOWN = 'D'

val fileInput = Source.fromFile(INPUTFILE).getLines.toList

case class Move(direction: Char, amount: Int, colour: String) {

    def expand: Array[Move] = {
        for (i <- 0 until amount) yield Move(direction, 1, colour)
    }.toArray

}


case class Directions(moves: Array[Move])
case class Point(x: Long, y: Long, dir: Char) {

    def move(dir: Char, amount: Long): Point = {
        dir match {
            case RIGHT => Point(x + amount, y, dir)
            case LEFT => Point(x - amount, y, dir)
            case UP => Point(x, y - amount, dir)
            case DOWN => Point(x, y + amount, dir)
            case _ => this
        }
    }

    def move(m: Move): Point = {
        if (m.amount < 1) {
            this
        } else {
            move(m.direction, m.amount)
        }
    }

    def topLeft = Point(x, y, UP)
    def topRight = Point(x+1, y, RIGHT)
    def bottomLeft = Point(x, y+1, LEFT)
    def bottomRight = Point(x+1, y+1, DOWN)

}

def parseFile(lines: List[String]): Directions = {
    Directions(lines.map(s => {
        val parts = s.split(" ")
        val dir: Char = parts(0).toCharArray.head
        val count: Int = parts(1).toInt
        val colour: String = parts(2)
        Move(dir, count, colour)
    }).toArray)
}

def parseFile2(lines: List[String]): Directions = {
    Directions(lines.map(s => {
        val parts = s.split(" ")
        val colour: String = parts(2).drop(2).dropRight(1)
        val dir = colour.last match {
            case '0' => RIGHT
            case '1' => DOWN
            case '2' => LEFT
            case '3' => UP
            case _ => RIGHT
        }
        val amount = Integer.parseInt(colour.dropRight(1), 16)
        if (DEBUG) println(s"$s => colour: $colour, dir: $dir, amount: $amount")
        Move(dir, amount, colour)
    }).toArray)
}

def polygon(start: Point, moves: Array[Move]): Array[Point] = {

    val pairs = moves.sliding(2,1).toArray

    pairs.foldLeft((start, Array(start)))((acc, next) => {

        val (currSquare, outerEdge) = acc

        val f = next(0)
        val t = next(1)

        val newSquare = currSquare.move(f)
        val newEdgePoint = f.direction match {
            case RIGHT => if (t.direction == UP) newSquare.topLeft else newSquare.topRight
            case LEFT => if (t.direction == DOWN) newSquare.bottomRight else newSquare.bottomLeft
            case UP => if (t.direction == RIGHT) newSquare.topLeft else newSquare.bottomLeft
            case DOWN => if (t.direction == LEFT) newSquare.bottomRight else newSquare.topRight
        }
        (newSquare, outerEdge :+ newEdgePoint)

    })._2
}


def shoelace(poly: Array[Point]): Long = {

    val xx = poly.map( p => p.x )
    val yy = poly.map( p => p.y )
    val overlace = xx zip yy.drop(1) ++ yy.take(1)
    val underlace = yy zip xx.drop(1) ++ xx.take(1)

    (overlace.map( t => t._1 * t._2 ).sum - underlace.map( t => t._1 * t._2 ).sum).abs / 2
}


def plot(from: Point, moves: Array[Move]): Array[Point] = {

    def makeMoves(f: Point, moves: Array[Move]): Array[Point] = {
        if (moves.length > 0) {
            val newPos = f.move(moves.head)
            Array(newPos) ++ makeMoves(newPos, moves.tail)
        } else {
            Array.empty[Point]
        }
    }

    if (moves.length > 0) {

        val move = moves.head
        val newPoints = makeMoves(from, move.expand)
        val endPos = newPoints.last
        newPoints ++ plot(endPos, moves.tail)

    } else {
        Array.empty[Point]
    }
}

def fill(lines: Array[String]): Array[String] = {
    val numLines = lines.length

    lines.zipWithIndex.map((l,i) => {
        val l1 ="R\\.+L".r.replaceAllIn(l, m => {
            val lineOK = (i > 0) && (i < numLines - 1)
            val dUp = ((lines(i-1)(m.start) == UP) && (lines(i+1)(m.end - 1) == DOWN))
            if (lineOK && dUp) {
                "#" * (m.end - m.start)
            } else {
                m.matched.toString
            }
        })
        val l2 ="U\\.+".r.replaceAllIn(l1, m => "#" * (m.end - m.start))
        val l3 ="\\.+D".r.replaceAllIn(l2, m => "#" * (m.end - m.start))
        l3.replaceAll("L", "#").replaceAll("R", "#").replaceAll("U", "#").replaceAll("D", "#")
    })
}

def toStringArray(points: Array[Point]): Array[String] = {

    val minX = points.map(_.x).min
    val minY = points.map(_.y).min
    val maxX = points.map(_.x).max
    val maxY = points.map(_.y).max

    println(s"minX: $minX, minY: $minY")
    println(s"maxX: $maxX, maxY: $maxY")

    (for (y <- minY to maxY) yield {
        (for (x <- minX to maxX) yield {

            val pt = points.find(p => p.x == x && p.y == y)

            pt match {
                case Some(p) => p.dir
                case None => '.'
            }
        }).mkString
    }).toArray
}

def part1(data: Directions): Unit = {

    println("Part 1")

    val firstMove = data.moves.head.direction

    val pts = plot(Point(0,0, firstMove), data.moves)
    if (DEBUG) pts.foreach(println)

    val lines = toStringArray(pts)
    lines.foreach(println)

    val filled = fill(lines)
    filled.foreach(println)

    val total = filled.map(l => l.count(_ == '#')).sum
    println(s"Total: $total")

}

def part2(data: Directions): Unit = {

    println("Part 2")

    val startPoint = Point(0,0, RIGHT)
    val poly = polygon(startPoint, data.moves) :+ startPoint // close polygon
    if (DEBUG) poly.foreach(println)

    val area = shoelace(poly.dropRight(1))
    println(s"Area: $area")

}


val startTime = java.time.LocalTime.now

println("Start time: " + startTime)

val directions = parseFile(fileInput)
if (DEBUG) directions.moves.foreach(println)

part1(directions)

val directions2 = parseFile2(fileInput)
if (DEBUG) directions2.moves.foreach(println)

part2(directions2)

val endTime = java.time.LocalTime.now
println("End time: " + endTime)
println("Elapsed time: " + java.time.Duration.between(startTime, endTime))


// Part 1 : Your puzzle answer was 36807.
// Part 2 :