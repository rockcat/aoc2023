
import scala.io.Source
import scala.util.matching.Regex

val TEST = false
val DEBUG = false

val INPUTFILE = if (TEST) "day16\\test.txt" else "day16\\input.txt"

val fileInput = Source.fromFile(INPUTFILE).getLines.toList

val SPLITV = '|'
val SPLITH = '-'
val TURN1 = '\\'
val TURN2 = '/'

type Grid = Array[Array[Char]]

trait Direction {
    override def toString: String = this match {
        case Right => "Right"
        case Left => "Left"
        case Up => "Up"
        case Down => "Down"
    }
}
case object Right extends Direction
case object Left extends Direction
case object Up extends Direction
case object Down extends Direction

case class Point(x: Int, y: Int) {
    override def toString: String = s"($x, $y)"
}

case class Visit(point: Point, direction: Direction) {
    override def toString: String = s"Visit($point, $direction)"
}

case class Cavern(tiles: Grid) {

    def charAt(point: Point): Char = tiles(point.y)(point.x)

    def visitNext(visit: Visit): Option[Visit] = {

        val dir = visit.direction
        val from = visit.point

        dir match {
            case Right => if (from.x < tiles(from.y).length - 1) Some(Visit(Point(from.x + 1, from.y),dir)) else None
            case Left => if (from.x > 0) Some(Visit(Point(from.x - 1, from.y),dir)) else None
            case Up => if (from.y > 0) Some(Visit(Point(from.x, from.y - 1),dir)) else None
            case Down => if (from.y < tiles.length - 1) Some(Visit(Point(from.x, from.y + 1),dir)) else None
        }
    }

    def nextDir(char: Char, currDir: Direction): Array[Direction] = {
        char match {
            case SPLITV if (currDir == Right || currDir == Left) => Array(Down, Up)
            case SPLITH if (currDir == Down || currDir == Up) => Array(Right, Left)
            case TURN1 => currDir match {
                case Right => Array(Down)
                case Down => Array(Right)
                case Left => Array(Up)
                case Up => Array(Left)
            }
            case TURN2 => currDir match {
                case Right => Array(Up)
                case Up => Array(Right)
                case Left => Array(Down)
                case Down => Array(Left)
            }
            case _ => Array(currDir)
        }
    }

    def traceBeams(latest: Array[Visit], previous: Array[Visit]): Array[Visit] = {
        if (DEBUG) println(s"visitCavern($latest, $previous)")

        if (latest.isEmpty) {
            previous
        } else {

            val nextVisits = for {
                v <- latest
                c = charAt(v.point)
                nd = nextDir(c, v.direction)
                nv <- nd.map(nv => visitNext(Visit(v.point, nv))).flatten
                if (!previous.exists(p => p.point == nv.point && p.direction == nv.direction))
            } yield nv
            
            traceBeams(nextVisits, previous ++ latest)
        }
    }
}


def plotVisit(grid: Grid, visit: Visit, asBeam: Boolean): Grid = {

    val plotChar = if (asBeam) {
       visit.direction match {
            case Right => '>'
            case Left => '<'
            case Up => '^'
            case Down => 'v'
        }
    } else '#'

    val newRow = grid(visit.point.y).updated(visit.point.x, plotChar)
    grid.updated(visit.point.y, newRow)
}

def plotVisits(grid: Grid, visits: Array[Visit], asBeam: Boolean): Grid = {
    visits.foldLeft(grid)((g, v) => plotVisit(g, v, asBeam))
}


def parseFile(lines: List[String]): Cavern = {
    val tiles = lines.map(_.toCharArray).toArray
    Cavern(tiles)
}

def printGrid(grid: Grid): Unit = {
    grid.foreach(row => println(row.mkString))
}

def countEnergised(grid: Grid): Int = {
    grid.map(row => row.count(c => c == '#')).sum
}

def part1(cavern: Cavern): Unit = {

    println("Part 1")

    val startVisit = Visit(Point(0, 0), Right)

    val visits: Array[Visit] = cavern.traceBeams(Array(startVisit), Array.empty[Visit])
    println(s"Visited ${visits.length} points")
    if (DEBUG) visits.foreach(println)

    val grid = plotVisits(cavern.tiles, visits, true)
    printGrid(grid)
    println()

    val energised = plotVisits(cavern.tiles, visits, false)
    printGrid(energised)
    println()

    val count = countEnergised(energised)
    println(s"Count: $count")


}

def part2(cavern: Cavern): Unit = {

    println("Part 2")

    def startDir(row: Int, col: Int): Direction = {
        if (row == 0) Down else if (row == cavern.tiles.length - 1) Up else if (col == 0) Right else Left
    }

    def calcTrace(v: Visit): Int = {
        val visits: Array[Visit] = cavern.traceBeams(Array(v), Array.empty[Visit])
        val energised = plotVisits(cavern.tiles, visits, false)
        val count = countEnergised(energised)
        if (DEBUG) {
            print("Starting from " + v)
            print(s" Visited ${visits.length} points")
            println(s" Count: $count")
        }
        count
    }
    
    val totalX = for {
        startRow <- Array(0, cavern.tiles.length - 1)
        startCol <- 0 until cavern.tiles(0).length
        dir = startDir(startRow, startCol)
        startVisit = Visit(Point(startCol, startRow), dir)
    } yield {
        calcTrace(startVisit)
    }

    val totalY = for {
        startRow <- 0 until cavern.tiles.length
        startCol <- Array(0, cavern.tiles(0).length - 1)
        dir = startDir(startRow, startCol)
        startVisit = Visit(Point(startCol, startRow), dir)
    } yield {
        calcTrace(startVisit)
    }

    println("Totals X: " + totalX.mkString(", "))
    println("Totals Y: " + totalY.mkString(", "))

    println(s"Maximum: ${(totalX ++ totalY).max}")

}

val startTime = java.time.LocalTime.now

println("Start time: " + startTime)

val cavern = parseFile(fileInput)
printGrid(cavern.tiles)

part1(cavern)

part2(cavern)


val endTime = java.time.LocalTime.now
println("End time: " + endTime)
println("Elapsed time: " + java.time.Duration.between(startTime, endTime))


// Part 1 : Your puzzle answer was 7632.
