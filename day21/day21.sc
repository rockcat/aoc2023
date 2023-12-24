import java.awt.Point

import scala.io.Source
import scala.util.matching.Regex

val TEST = false
val DEBUG = true
val ANIMATE_DELAY = 50

val INPUTFILE = if (TEST) "day21\\test.txt" else "day21\\input.txt"

val fileInput = Source.fromFile(INPUTFILE).getLines.toList

val ROCK ='#'
val PLOT ='.'
val START ='S'

def clearTerminal: Unit = {
    print("\u001bc")
}  

def homeCursor: Unit = {
    print("\u001b[H")
}

def cursorDown(lines: Int): Unit = {
    print(s"\u001b[${lines}B")
}

case class Point(x: Int, y: Int) {
    def up: Point = Point(x, y-1)
    def down: Point = Point(x, y+1)
    def left: Point = Point(x-1, y)
    def right: Point = Point(x+1, y)
}

case class Garden(grid: Array[Array[Char]], startPos: Point) {

    val maxX = grid(0).length
    val maxY = grid.length

    def canGoUp(from: Point): Boolean = from.y > 0 && grid(from.y-1)(from.x) == PLOT
    def canGoDown(from: Point): Boolean = from.y < maxY-1 && grid(from.y+1)(from.x) == PLOT
    def canGoLeft(from: Point): Boolean = from.x > 0 && grid(from.y)(from.x-1) == PLOT
    def canGoRight(from: Point): Boolean = from.x < maxX-1 && grid(from.y)(from.x+1) == PLOT

    override def toString(): String = {
        grid.map(_.mkString).mkString("\n") + "\nStart Pos: " + startPos
    }

    def nextSteps(from: Array[Point]): Array[Point] = {

        val upMoves =  from.filter(canGoUp).map(_.up)
        val downMoves = from.filter(canGoDown).map(_.down)
        val leftMoves = from.filter(canGoLeft).map(_.left)
        val rightMoves = from.filter(canGoRight).map(_.right)

        (upMoves ++ downMoves ++ leftMoves ++ rightMoves).distinct
    }

    def plotSteps(steps: Array[Point]): Unit = {

        for (i <- 0 until maxY) {
            for (j <- 0 until maxX) {
                if (steps.contains(Point(j, i))) print('O') else print(grid(i)(j))
            }
            println()
        }

    }


}


def parseFile(lines: List[String]): Garden = {

    val grid = lines.map(_.trim).filter(_.nonEmpty).map(_.toCharArray).toArray

    val startLine = lines.zipWithIndex.filter(_._1.contains(START)).head._2
    val startPos = Point(lines(startLine).indexOf(START), startLine)

    val newStartLine = grid(startLine).updated(startPos.x, PLOT)
    
    Garden(grid.updated(startLine, newStartLine), startPos)
}



def part1(garden: Garden): Unit = {

    println("Part 1")

    val LOOP = 64

    val finalSteps = (1 to LOOP).foldLeft(Array(garden.startPos))((steps, next) => {

        val newSteps = garden.nextSteps(steps)
        homeCursor
        garden.plotSteps(newSteps)
        Thread.sleep(ANIMATE_DELAY)
        println()
        newSteps
    })

    println("Final Steps: " + finalSteps.length)


}

def part2(garden: Garden): Unit = {

    println("Part 2")

}

val startTime = java.time.LocalTime.now

println("Start time: " + startTime)

val garden = parseFile(fileInput)

println(garden)

clearTerminal
part1(garden)

part2(garden)


val endTime = java.time.LocalTime.now
println("End time: " + endTime)
println("Elapsed time: " + java.time.Duration.between(startTime, endTime))


