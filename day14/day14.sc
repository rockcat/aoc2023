
import scala.io.Source
import scala.util.matching.Regex

val TEST = false
val DEBUG = false
val INPUTFILE = if (TEST) "day14\\test.txt" else "day14\\input.txt"

val fileInput = Source.fromFile(INPUTFILE).getLines.toList

val BOULDER = 'O'
val EMPTY = '.'
val ROCK = '#'



case class Rocks(positions: Array[Array[Char]], numLines : Int) {


    def printPositions: Unit = {
        positions.zipWithIndex.foreach((p,i) => println(p.mkString("") + " " + (numLines - i)))
        println("Score: " + score)
    }

    def score: Int = {
        positions.zipWithIndex.map((p,i) => p.filter(_ == 'O').length * (numLines - i)).sum

    }

    def rotateRight: Rocks = {
        val newPositions = positions.reverse.transpose
        Rocks(newPositions, numLines)
    }

    def rotateLeft: Rocks = {
        val newPositions = positions.map(_.reverse).transpose
        Rocks(newPositions, numLines)
    }

    def rotate180: Rocks = {
        val newPositions = positions.reverse.map(_.reverse)
        Rocks(newPositions, numLines)
    }

    def shiftNorth: Rocks = {

        def moveRocks(to: Array[Char], from: Array[Char]): (Array[Char],Array[Char]) = {
            to.zip(from).map[(Char,Char)](c => if ((c._1 == EMPTY) && (c._2 == BOULDER)) (BOULDER, EMPTY) else (c._1, c._2)).unzip
        }

        def shift(r: Rocks, repeat: Int): Rocks = {
            if (repeat == 0) {
                r
            } else {
                val startRow = Array(r.positions(0))
                val newPos = r.positions.drop(1).foldLeft(startRow)((acc, row) => {
                    val (newLast, newRow) = moveRocks(acc.last, row)
                    acc.dropRight(1) :+ newLast :+ newRow
                })

                shift(r.copy(positions = newPos), repeat - 1)
            }
        }

        shift(this, numLines)
    }

    def shiftEast: Rocks = {
        rotateLeft.shiftNorth.rotateRight
    }

    def shiftSouth: Rocks = {
        rotate180.shiftNorth.rotate180
    }

    def shiftWest: Rocks = {
        rotateRight.shiftNorth.rotateLeft
    }

    def cacheKey: String = {
        positions.map(_.mkString("")).mkString("")
    }
}



def parseFile(lines: List[String]): Rocks = {
    Rocks(lines.map(l => l.toCharArray()).toArray, lines.length)
}



def part1(rocks: Rocks): Unit = {

    println("Part 1")

    val r2 = rocks.shiftNorth
    r2.printPositions
    println()

}

val CYCLES = 1000000000
val cache = scala.collection.mutable.Map[String, Rocks]()


def cacheLoop = cache.forall((k,v) => cache.get(v.cacheKey).isDefined)

def loopLen(start: String): Int = {

    def followLoop(key: String): Int = {
        val next = cache(key)
        val nextKey = next.cacheKey
        if (nextKey == start) {
            1
        } else {
            1 + followLoop(nextKey)
        }
    }

    followLoop(start)

}


def cycle(rocks: Rocks, repeat: Int): Rocks = {

    if (DEBUG) {
        println("\nCycle " + repeat)
    } else {
        print(s"$repeat Cycle ")
    }

    val key: String = rocks.cacheKey

    val newRocks = if (cache.contains(key)) {
        if (DEBUG) println("Cache Hit")
        val updated = cache(key)
        if (DEBUG) updated.printPositions
        updated
    } else {
        if (DEBUG) println("Cache miss")
        val updated = rocks.shiftNorth.shiftWest.shiftSouth.shiftEast
        cache += (key -> updated)
        if (DEBUG) updated.printPositions
        updated
    }

    if (!DEBUG) println(" Score: " + newRocks.score)

    if (repeat >= CYCLES) {
        newRocks
    } else {
        val cl = cacheLoop
        val cacheLen = cache.size

        if (cl && repeat < CYCLES - cacheLen) {
            println("Cache loop detected")
            println(cacheLen + " entries in cache")

            val loopLength = loopLen(newRocks.cacheKey)
            println("Loop length: " + loopLength)

            val skip = ((CYCLES - (repeat + 1)) / loopLength) * loopLength
            println("Skip " + skip + " cycles")
            val newReps = repeat + skip + 1

            println("Skip to " + newReps)
            cycle(rocks, newReps) 
        } else {
            cycle(newRocks, repeat + 1)
        }
    }
}

def part2(rocks: Rocks): Unit = {

    println("Part 2")


    val cycled = cycle(rocks, 0)
    cycled.printPositions

}

val startTime = java.time.LocalTime.now

println("Start time: " + startTime)

val rocks = parseFile(fileInput)

rocks.printPositions
println()

part1(rocks)

part2(rocks)


val endTime = java.time.LocalTime.now
println("End time: " + endTime)
println("Elapsed time: " + java.time.Duration.between(startTime, endTime))


// Part 1: Your puzzle answer was 110821.
// Part 2: Your puzzle answer was 83516 
