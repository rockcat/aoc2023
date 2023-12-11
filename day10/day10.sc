
import scala.io.Source
import scala.util.matching.Regex

val TEST = false
val DEBUG = false

val INPUTFILE = if (TEST) "day10\\test3.txt" else "day10\\input.txt"

val fileInput = Source.fromFile(INPUTFILE).getLines.toList

trait Connected
case object North extends Connected
case object South extends Connected
case object East extends Connected
case object West extends Connected

val validConnections: Map[Char, List[Connected]] = Map(
    '|' -> List(North, South),
    '-' -> List(East, West),
    'L' -> List(North, East),
    'J' -> List(North, West),
    '7' -> List(South, West),
    'F' -> List(South, East),
    '.' -> List.empty
)


case class Position(x: Int, y:Int) {

    override def toString(): String = s"($x, $y)"

    def ==(other: Position): Boolean = {
        x == other.x && y == other.y
    }

    def moves(connections: List[Connected]): List[Position] = {
        connections.map {
            case North => Position(x, y - 1)
            case South => Position(x, y + 1)
            case East => Position(x + 1, y)
            case West => Position(x - 1, y)
        }
    }

}

type DistanceMap = Array[Array[Int]]

case class PipeMap(locations: Array[Array[Char]], startPos: Position) {

    def print: Unit = {
        locations.foreach(r => println(r.mkString("")))
    }

    def startCanMoveUp: Boolean = {
        startPos.y > 0 && Array('|','F','7').contains(locations(startPos.y - 1)(startPos.x))
    }

    def startCanMoveDown: Boolean = {
        startPos.y < locations.length - 1 && Array('|','L','J').contains(locations(startPos.y + 1)(startPos.x))
    }

    def startCanMoveLeft: Boolean = {
        startPos.x > 0 && Array('-','F','L').contains(locations(startPos.y)(startPos.x - 1))
    }

    def startCanMoveRight: Boolean = {
        startPos.x < locations(0).length - 1 && Array('-','J','7').contains(locations(startPos.y)(startPos.x + 1))
    }

    def startMoves: List[Position] = {
        List(
            if (startCanMoveUp) Some(Position(startPos.x, startPos.y - 1)) else None,
            if (startCanMoveDown) Some(Position(startPos.x, startPos.y + 1)) else None,
            if (startCanMoveLeft) Some(Position(startPos.x - 1, startPos.y)) else None,
            if (startCanMoveLeft) Some(Position(startPos.x + 1, startPos.y)) else None
        ).flatten
    }

    def replaceStart: PipeMap = {
        val replacement = (startCanMoveUp, startCanMoveDown, startCanMoveLeft, startCanMoveRight) match {

            case (true, true, false, false) => '|'
            case (true, false, false, true) => 'L'
            case (true, false, true, false) => 'J'
            case (false, true, false, true) => 'F'
            case (false, true, true, false) => '7'
            case (false, false, true, true) => '-'
            case (_,_,_,_) => '!'
        }

        val rowUpdate = locations(startPos.y).updated(startPos.x, replacement)
        PipeMap(locations.updated(startPos.y, rowUpdate), startPos)
    }

    def movesAt(pos: Position): List[Position] = {
        if (pos == startPos) {
            startMoves
        } else {
            val char = locations(pos.y)(pos.x)
            val connections = validConnections(char)
            pos.moves(connections)
        }
    }

    @scala.annotation.tailrec
    final def calculateDistances(max: Int, dMap: DistanceMap, continue: Boolean): (Int, DistanceMap, Boolean) = {

        if (DEBUG) {
            println("Calculating Distance: " + max )
        }

        val currentMax = for {
            x <- 0 until locations(0).length
            y <- 0 until locations.length
            if dMap(y)(x) == max
        } yield {
            Position(x, y)
        }

        if (DEBUG) {
            println("Current Max: " + max)
            println(currentMax.map(_.toString).mkString(", "))
        }

        val needUpdate = for {
            current <- currentMax
            next <- movesAt(current)
            if dMap(next.y)(next.x) == -1
        } yield {
            next
        }

        if (DEBUG) {
            println("Need Update: ")
            println(needUpdate.map(_.toString).mkString(", "))
        }

        if (needUpdate.isEmpty) {
            (max, dMap, true)
        } else {
            val newMax = max + 1
            val newMap = needUpdate.foldLeft(dMap)((accMap, next) => accMap.updated(next.y, accMap(next.y).updated(next.x, newMax)))

            if (DEBUG) {
                println("New Map: ")
                printDistanceMap(newMap)
            }
            calculateDistances(newMax, newMap, false)
        }
    }


    def removeDeadPoints(dMap: DistanceMap): PipeMap = {

        val lineLen = locations(0).length

        val newLocations =                 
            (for (y <- 0 until locations.length) yield {
                (for (x <- 0 until lineLen) yield {
                    val c: Char = if (dMap(y)(x) == -1) ' ' else locations(y)(x)
                    c
                }).toArray
            }).toArray

        PipeMap(newLocations, startPos)
    }


}

def printDistanceMap(distanceMap: DistanceMap): Unit = {
    // find largest number
    val max = distanceMap.flatten.max
    val maxLen = max.toString.length

    distanceMap.foreach(row => {
        row.foreach(d => {
            val dStr = d.toString
            if (d == -1) {
                print("+" * maxLen)
            } else {
                print((" " * (maxLen - dStr.length)))
                print(dStr)
            }
        })
        println()
    })
}


def maxInDistanceMap(distanceMap: DistanceMap): Int = {
    distanceMap.flatten.max
}


def parseFile(lines: List[String]): PipeMap = {

    val locations = lines.map(_.toCharArray).toArray
    val distance = Array.fill(locations.length, locations(0).length)(-1)

    val startY = locations.indexWhere(l => l.contains('S'))
    val startX = locations(startY).indexOf('S')

    PipeMap(locations, Position(startX, startY))
}

def EmptyDistanceMap(cols: Int, rows: Int, startPos: Position): DistanceMap = {
    val startRow = Array.fill(cols)(-1).updated(startPos.x, 0)
    (Array.fill(rows, cols)(-1)).updated(startPos.y, startRow)
}

def part1(pipeMap: PipeMap): DistanceMap = {

    println("Part 1")

    val mapRows = pipeMap.locations.length
    val mapCols = pipeMap.locations(0).length

    println("Map Size: " + mapCols + " x " + mapRows)
    

    println("Start Pos: " + pipeMap.startPos)
    println("Start Moves: ")
    val startMoves = pipeMap.startMoves
    startMoves.foreach(println)

    val startingMap = EmptyDistanceMap(mapCols, mapRows, pipeMap.startPos)
    println("Start Map: ")
    printDistanceMap(startingMap)
    println()

    val (max, dMap, _) = pipeMap.calculateDistances(0, startingMap, true)
    if (DEBUG) {
        printDistanceMap(dMap)
    }

    println("Max: " + max)
    println()
    dMap
}

// TWO METHODS

case class Ray(inside: Boolean, crossings: Int, count: Int)

def scoreLine(line: Array[Char]): Ray = {
    val clean = line.mkString
                    .replace("-","")
                    .replace("F7","")
                    .replace("LJ","")
                    .replace("FJ","|")
                    .replace("L7","|")
    println(clean)
    clean.foldLeft(Ray(false, 0, 0))((rt, next) => {
        next match {
            case '|' => Ray(!rt.inside, rt.crossings + 1, rt.count)
            case _ if rt.inside => Ray(rt.inside, rt.crossings, rt.count + 1)
            case _ if !rt.inside => rt
        }
    })
}


case class RayState(inside: Boolean, upStart: Boolean, downStart: Boolean, 
                    crossings: Int, line: String)

def mapLine(line: String): RayState = {
    line.toCharArray.foldLeft(RayState(false, false, false, 0, ""))((rs, next) => {
        // println(next + " " + rs.toString + " -> ")
        val crossingIn = rs.crossings % 2 == 0
        val nextLine = rs.line + next
        next match {
            case ' ' if (rs.inside) => rs.copy(line = rs.line + 'I') 
            case ' ' if (!rs.inside) => rs.copy(line = rs.line + 'O')
            case '-' => rs.copy(line = nextLine)

            case '|' => RayState(!rs.inside, false, false, rs.crossings + 1, nextLine)

            case 'F' => RayState(rs.inside, true, false, rs.crossings, line = nextLine)
            case 'L' => RayState(rs.inside, false, true, rs.crossings, line = nextLine)
            
            case 'J' if rs.upStart => RayState(!rs.inside, false, false, rs.crossings + 1, nextLine)
            case 'J' if rs.downStart => RayState(rs.inside, false, false, rs.crossings, line = nextLine)


            case '7' if rs.upStart => RayState(rs.inside, false, false, rs.crossings, nextLine)
            case '7' if rs.downStart => RayState(!rs.inside, false, false, rs.crossings + 1, nextLine)

            case _ => rs.copy(line = rs.line + 'X')
        }
    })
}

def part2(pipeMap: PipeMap, dMap: DistanceMap): Unit = {

    println("Part 2")

    val  cleanMap = pipeMap.removeDeadPoints(dMap).replaceStart


    cleanMap.print

    // METHOD 1
    val counts = cleanMap.locations.map(l => scoreLine(l))
    println(counts.map(_.count).mkString(", "))

    val sum = counts.map(_.count).sum
    println("Sum v1: " + sum)


    // METHOD 2
    val mappedLines = cleanMap.locations.map(l => mapLine(l.mkString))
    mappedLines.foreach(l => println(l.line))

    val sum2 = mappedLines.map(rs => rs.line.filter(_ =='I').length).sum
    println("Sum v2: " + sum2)

}

val pipeMap = parseFile(fileInput)
pipeMap.print
println()

val dMap = part1(pipeMap)

part2(pipeMap, dMap)



// Part 1: 6882
// Both methods Part 2: 491

