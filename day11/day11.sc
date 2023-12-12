
import scala.io.Source
import scala.util.matching.Regex

val TEST = false
val DEBUG = false

val INPUTFILE = if (TEST) "day11\\test.txt" else "day11\\input.txt"

val fileInput = Source.fromFile(INPUTFILE).getLines.toList

case class Point(x: Long, y: Long)
case class GalaxyPair(g1: Point, g2: Point, distance: Long)

case class Universe(points: Array[Array[Char]]) {

    def emptyRows: List[Int] = {
        for {
            y <- 0 until points.length
            if points(y).forall(_ == '.')
         } yield y
    }.toList

    def emptyCols: List[Int] = {
        for {
            x <- 0 until points(0).length
            if points.forall(_(x) == '.')
         } yield x
    }.toList

    def expandUniverse: Universe = {

        val emptyR = emptyRows
        val emptyC = emptyCols


        println("Found " + emptyR.length + " empty rows")
        println("Found " + emptyC.length + " empty cols")

        def expandRow(row: Array[Char]): Array[Char] = {

            val newRow = for {
                x <- 0 until row.length
            } yield {
                if (emptyC.contains(x)) ".." else row(x)
            }

            newRow.mkString.toCharArray
        }

        val expanded = for {
            y <- 0 until points.length
        } yield {
            val newRow = expandRow(points(y))
            if (emptyR.contains(y)) Array(newRow, newRow) else Array(newRow)

        }

        Universe(expanded.flatten.toArray)
    }


    def superExpandedUniverse(locations: List[Point], scale: Long): List[Point] = {

        println("Expanding universe to " + scale + "x size " + locations.length + " galaxies")

        val emptyR = emptyRows
        val emptyC = emptyCols

        println("Empty rows: " + emptyR.mkString(", "))
        println("Empty cols: " + emptyC.mkString(", "))

        val superExpandedY = for {
            loc <- locations
            gaps = emptyR.filter(_ < loc.y)
            newY = if (gaps.length > 0) (gaps.length * (scale - 1)) + loc.y else loc.y
        } yield {
            Point(loc.x, newY)
        }

        val superExpandedXY = for {
            loc <- superExpandedY
            gaps = emptyC.filter(_ < loc.x)
            newX = if (gaps.length > 0) (gaps.length * (scale - 1)) + loc.x else loc.x
        } yield {
            Point(newX, loc.y)
        }

        if (DEBUG) {
            superExpandedXY.foreach(g => print(g.toString + " "))
            println()
        }
        superExpandedXY
    }

    def numberGalaxies: List[Point] = {

        (for {
            y <- 0 until points.length
            x <- 0 until points(0).length
            if points(y)(x) == '#'
        } yield Point(x, y)).toList

    }

    def printUniverse: Unit = {
        points.foreach(row => println(row.mkString))
    }
}

def distanceBetween(p1: Point, p2: Point): Long = {
    Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y)
}

def galaxyPairs(galaxies: List[Point]): List[GalaxyPair] = {
    (for {
        g1 <- 0 until galaxies.length
        g2 <- 0 until g1
        distance = distanceBetween(galaxies(g1), galaxies(g2))
    } yield GalaxyPair(galaxies(g1), galaxies(g2), distance)).toList
}

def parseFile(lines: List[String]): Universe = {

    val width = lines(0).length
    val height = lines.length

    val points = Array.ofDim[Char](width, height)

    for (y <- 0 until height) {
        points(y) = lines(y).toCharArray()
    }

    Universe(points)
}


val universe = parseFile(fileInput)
universe.printUniverse
println()

val expandedUniverse = universe.expandUniverse
expandedUniverse.printUniverse
println()

val numberedUniverse = expandedUniverse.numberGalaxies
println("Galaxies: " + numberedUniverse.length)
if (DEBUG) {
    numberedUniverse.foreach(println)
    println()
}

val gp = galaxyPairs(numberedUniverse)
println("Found " + gp.length + " pairs")
println()

println("Part 1")
println("Sum of distances: " + gp.map(_.distance).sum)
println()

println("Part 2")
val numberedBaseUniverse = universe.numberGalaxies

def expand(scale: Long): Unit = {
    val superExpandedUniverse = universe.superExpandedUniverse(numberedBaseUniverse, scale)
    println("Galaxies: " + superExpandedUniverse.length)
    val superPairs = galaxyPairs(superExpandedUniverse)
    println("Found " + superPairs.length + " pairs")
    println()
    println("Sum of distances: " + superPairs.map(_.distance).sum)
    println()
}

expand(10L)
expand(100L)
expand(1000000L)

/*

Found 101475 pairs

Part 1
Sum of distances: 10289334

Part 2
Sum of distances: 649862989626

*/





