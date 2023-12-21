import scala.annotation.tailrec

import scala.io.Source
import scala.util.matching.Regex

val TEST = false
val DEBUG = false
val ANIMATE = true

val ANIMATE_DELAY = 40

val INPUTFILE = if (TEST) "day17\\test6.txt" else "day17\\input.txt"

val fileInput = Source.fromFile(INPUTFILE).getLines.toList


trait Direction {

    override def toString: String = this match {
        case Right => "Right"
        case Left => "Left"
        case Up => "Up"
        case Down => "Down"
    }

    def toChar: Char = this match {
        case Right => '>'
        case Left => '<'
        case Up => '^'
        case Down => 'v'
    }

    def next(count: Int): Array[Direction] = {
        (if (count < Direction.MAX_DIR) Array(this) else Array.empty[Direction])
        ++ (this match {
            case Right => Array(Down,Up)
            case Left => Array(Down, Up)
            case Up => Array(Right, Left)
            case Down => Array(Right, Left)
        }) 
    }

    def nextUltra(count: Int): Array[Direction] = {

        if (count < Direction.MIN_ULTRA_DIR) {
            Array(this)
        } else {
            (if (count < Direction.MAX_ULTRA_DIR) Array(this) else Array.empty[Direction])
            ++ (this match {
                case Right => Array(Down,Up)
                case Left => Array(Down, Up)
                case Up => Array(Right, Left)
                case Down => Array(Right, Left)
            }) 
        }
    }

}

object Direction {
    val MAX_DIR = 3
    val MIN_ULTRA_DIR = 4
    val MAX_ULTRA_DIR = 10
}

case object Right extends Direction
case object Left extends Direction
case object Up extends Direction
case object Down extends Direction


case class Point(x: Int, y: Int, value: Int) {
    override def toString: String = s"($x, $y, $value)"

    def locationString: String = s"($x, $y)"

    def move(dir: Direction, heatMap: HeatMap): Point = dir match {
        case Right => heatMap.pointAt(x + 1, y)
        case Left => heatMap.pointAt(x - 1, y)
        case Up => heatMap.pointAt(x, y - 1)
        case Down => heatMap.pointAt(x, y + 1)
    }
}

case class HeatMap(temp: Array[Array[Int]]) {

    override def toString(): String = temp.map(_.mkString).mkString("\n")

    def pointAt(x: Int, y: Int): Point = Point(x, y, temp(y)(x))

    def tempAt(p: Point): Int = temp(p.y)(p.x)


    val maxY = temp.length - 1
    val maxX = temp(0).length - 1

    def validMove(from: Point, dirs: Array[Direction]): Array[Direction]= {
        dirs.filter(d => d match {
            case Right if (from.x < maxX) => true
            case Left if (from.x > 0) => true
            case Up if (from.y > 0) => true
            case Down if (from.y < maxY) => true
            case _ => false
        })
    }
}

case class Visit(point: Point, dir: Direction, dirCount: Int) {
    override def toString: String = s"Visit(${point.locationString}, $dir)"

    def nextSteps(heatMap: HeatMap): Array[Visit] = {
        val dirs = heatMap.validMove(point, dir.next(dirCount))
        dirs.map(d => Visit(point.move(d, heatMap), d, if (d == dir) dirCount + 1 else 1))
    }

    def ultraNextSteps(heatMap: HeatMap): Array[Visit] = {
        val dirs = heatMap.validMove(point, dir.nextUltra(dirCount))
        dirs.map(d => Visit(point.move(d, heatMap), d, if (d == dir) dirCount + 1 else 1))
    }
}

case class Path(visits: Array[Visit]) {

    override def toString: String = visits.mkString("Path(", ", ", s", Steps: ${visits.length} Total: $value)")

    def value: Int = visits.tail.map(_.point.value).sum

    def dir = visits.lastOption.map(_.dir).getOrElse(Right)

    def dirCount = visits.lastOption.map(_.dirCount).getOrElse(0)

    def +(visit: Visit): Path = Path(visits :+ visit)

    def +(p: Path): Path = Path(visits ++ p.visits)

    def render(heatMap: HeatMap): String = {
        val map = for (y <- 0 to heatMap.maxY) yield {
            val row = (for (x <- 0 to heatMap.maxX) yield {
                if (x == 0 && y == 0) {
                    heatMap.tempAt(Point(0,0,0))
                } else {
                    visits.filter(v => v.point.x == x && v.point.y == y)
                        .headOption
                        .map(v => v.dir.toChar)
                        .getOrElse(heatMap.tempAt(Point(x,y,0)))
                }
            }).mkString
            row
        }
        map.mkString("\n" )
    }

}

object Path {
    def empty: Path = Path(Array.empty[Visit])

    def apply(visit: Visit): Path = Path(Array(visit))
}


def parseFile(lines: List[String]): HeatMap = {
    HeatMap(lines.map(l => l.toCharArray.map(_ - '0')).toArray)
}

def clearTerminal: Unit = {
    print("\u001bc")
}  

def homeCursor: Unit = {
    print("\u001b[H")
}

def cursorDown(lines: Int): Unit = {
    print(s"\u001b[${lines}B")
}

val cache = scala.collection.mutable.Map[Visit, Option[Path]]()


def aStar(from: Visit, goal: Point, ultra: Boolean): Option[Path] = {
    
    val openSet = scala.collection.mutable.Set[Visit]()
    val closedSet = scala.collection.mutable.Set[Visit]()

    val cameFrom = scala.collection.mutable.Map[Visit, Visit]()

    val gScore = scala.collection.mutable.Map[Visit, Long]()
    val fScore = scala.collection.mutable.Map[Visit, Long]()

    def heuristicCostEstimate(from: Point, goal: Point): Long = {
        (math.abs(from.x - goal.x) + math.abs(from.y - goal.y))
    }

    @tailrec
    def reconstructPath(to: Visit, acc: Path): Path = {
        val from = cameFrom.find((t,f) => (t == to))

        if (!from.isDefined) {
            Path(acc.visits.reverse)
        } else {
            reconstructPath(from.get._2, acc + to)
        }
    }    

    openSet += from
    gScore += (from -> 0)
    fScore += (from -> heuristicCostEstimate(from.point, goal))

    
    while (openSet.nonEmpty) {
    
        val current = openSet.minBy(fScore(_))

        if (current.point == goal && (!ultra || (current.dirCount >= Direction.MIN_ULTRA_DIR))) {
            println("Arrived at goal")
            return Some(reconstructPath(current, Path.empty))
        }
    
        openSet -= current
        // closedSet += current
    
        val neighbors = (if (ultra) current.ultraNextSteps(heatMap) else current.nextSteps(heatMap))
                        // .filter(!closedSet.contains(_))

        for (n <- neighbors) {

            val tentativeGScore = gScore(current) + current.point.value + n.point.value
            val nScore = gScore.get(n)

            if (!nScore.isDefined || tentativeGScore < nScore.get) {

                cameFrom += (n -> current)
                gScore += (n -> tentativeGScore)
                fScore += (n -> (tentativeGScore + heuristicCostEstimate(n.point, goal)))

                if (!openSet.contains(n)) {
                    openSet += n
                }            
            }
        }
    }
    None
}


def part1(heatMap: HeatMap): Unit = {

    println("Part 1")

    val p = aStar(Visit(heatMap.pointAt(0,0), Right, 0), heatMap.pointAt(heatMap.maxX, heatMap.maxY), false)
    println(p)

    if (p.isDefined) {
        println(p.get.render(heatMap))
        val cost = p.get.value + heatMap.pointAt(heatMap.maxX, heatMap.maxY).value
        println(s"Cost: $cost")
    }

}

def part2(heatMap: HeatMap): Unit = {

    println("Part 2")

    val p = aStar(Visit(heatMap.pointAt(0,0), Right, 0), heatMap.pointAt(heatMap.maxX, heatMap.maxY), true)
    println(p)

    if (p.isDefined) {
        println(p.get.render(heatMap))
        val cost = p.get.value + heatMap.pointAt(heatMap.maxX, heatMap.maxY).value
        println(s"Cost: $cost")
    }
}

val startTime = java.time.LocalTime.now

println("Start time: " + startTime)

val heatMap = parseFile(fileInput)

println(heatMap)


part1(heatMap)

part2(heatMap)


val endTime = java.time.LocalTime.now
println("End time: " + endTime)
println("Elapsed time: " + java.time.Duration.between(startTime, endTime))

// Part 1: Your puzzle answer was 956.
// Part 2: 1106
