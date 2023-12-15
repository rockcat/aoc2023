import scala.languageFeature.reflectiveCalls

import scala.io.Source
import scala.util.matching.Regex

val TEST = false
val DEBUG = false

val INPUTFILE = if (TEST) "day13\\test2.txt" else "day13\\input.txt"

val fileInput = Source.fromFile(INPUTFILE).getLines.toList

case class Reflection(at: Int, size: Int, perfect: Boolean) {
    def isPerfect: Boolean = perfect
}

case class Intscape(pattern: Array[Int], offset: Int, reflection: Int) {

    override def toString(): String = s"Intscape(${pattern.mkString(",")}), $offset, $reflection)"

    def pairIndices: List[Int] = {
        pattern.zipWithIndex.dropRight(1).filter((v,i) => pattern(i) == pattern(i + 1)).map(_._2).toList
    }
    
    def testReflection(from: Int): Boolean = {
        (for {
            i <- 0 to from
            a = from - i
            b = from + i + 1
        } yield {
            (a < 0 || b >= pattern.length || pattern(a) == pattern(b))
        }).forall(_ == true)
    }
    
    def trimFrom(index: Int): Intscape = {
        if (index >= pattern.length / 2) {
            val takeR = (pattern.length - (index + 1)) * 2
            val off = pattern.length - takeR
            Intscape(pattern.takeRight(takeR), off, index - offset)
        } else {
            val takeL = (index + 1) * 2
            val offs = pattern.length - takeL
            Intscape(pattern.take(takeL), -offs, index)
        }
    }

    def oneBitDiff: List[(Int,Int)] = {
        val diffs = for {
            i <- 0 until pattern.length - 1
            j <- i + 1 until pattern.length
            diff = pattern(i) ^ pattern(j)
            if (diff != 0) && ((diff & (diff - 1)) == 0)
        } yield {
            (j,i)
        }
        diffs.toList
    }

    def mismatch: List[(Int,Int)] = {
        val mismatches = for {
            i <- 0 until pattern.length/2
            j = pattern.length - i - 1
            diff = pattern(i) ^ pattern(j)
            if (diff != 0) && ((diff & (diff - 1)) == 0)
        } yield (i,j)

        mismatches.toList
    }

    def isPerfect:Boolean = {
        (0 until pattern.length/2).forall(i => pattern(i) == pattern(pattern.length - i - 1))
    }

    def update(index: Int, value: Int): Intscape = {
        Intscape(pattern.updated(index, value), offset, reflection)
    }

    def printAsMap(vert: Boolean): Unit = {

        val asArr = pattern.map(p => p.toBinaryString.replace('0', '.').replace('1', '#'))
        val longest = asArr.map(_.length).max
        val as2DArr = asArr.map(p => p.reverse.padTo(longest, '.').reverse.toCharArray)


        if (vert) {
            as2DArr.take(reflection + 1).foreach(row => println(row.mkString))
            println("".padTo(longest, '-'))
            as2DArr.drop(reflection + 1).foreach(col => println(col.mkString))
            println()
        } else {
            as2DArr.transpose.foreach(r => 
                println(r.take(reflection + 1).mkString + "|" + r.drop(reflection + 1).mkString)
            )
            println()
        }
    }

    def desmudge(fix: Intscape): Intscape = {
        if (fix.offset > 0) {
            Intscape(pattern.take(fix.offset) ++ fix.pattern, 0, fix.reflection)
        } else {
            Intscape(fix.pattern ++ pattern.takeRight(-fix.offset), 0, fix.reflection)
        }
    }
 }

case class Landscape(pattern: Array[Array[Char]]) {

    val width = pattern(0).length
    val height = pattern.length

    def row(y: Int): String= pattern(y).mkString
    def col(x: Int): String = pattern.map(_(x)).mkString

    def printLandscape: Unit = {
        pattern.foreach(row => println(row.mkString))
        println()
    }


    def reflectionWidth(pos: Int): Int = {

        def isReflected(x: Int): Boolean = {
            (x + pos + 1 < width) && (col(pos - x) == col(pos + x + 1))
        }
        (0 to pos).takeWhile(isReflected).max + 1        
    }

    def reflectionHeight(pos: Int): Int = {

        def isReflected(y: Int): Boolean = {
            (y + pos + 1 < height) && (row(pos - y) == row(pos + y + 1))
        }
        (0 to pos).takeWhile(isReflected).max + 1        
    }

    def reflectionsY: List[Reflection] = {
        (for {
            y <- 0 until height - 1
            if row(y) == row(y + 1)
        } yield {
            val rh = reflectionHeight(y)
            val perfect = (rh == y + 1) || (rh + y + 1 == height)
            Reflection(y, rh, perfect)
        }).toList
    }

    def reflectionsX: List[Reflection] = {
        (for {
            x <- 0 until width - 1
            if col(x) == col(x + 1)
        } yield {
            val rw = reflectionWidth(x)
            val perfect = (rw == x + 1) || (rw + x + 1 == width)
            Reflection(x, rw, perfect)
        }).toList
    }

    def toNumeric(str: Array[Char]): Int = {
        val i = str.foldLeft(0)((acc, c) => (acc << 1) + (if (c == '.') 0 else 1))
        if (DEBUG) {
            println(str.mkString + " -> " + i + " -> " + i.toBinaryString)
        }
        i
    }


    def asIntArrayRows: Intscape = {
        Intscape(pattern.map(toNumeric),0,0)
    }

    def asIntArrayCols: Intscape= {
        val cols = (0 until width).map(col)
        Intscape(cols.map(c => toNumeric(c.toCharArray)).toArray,0,0)
    }

    lazy val intRows: Intscape = asIntArrayRows
    lazy val intCols: Intscape = asIntArrayCols


}

def parseFile(lines: List[String]): List[Landscape] = {

    val lastLine = lines.length - 1

    lines.foldLeft(List.empty[Landscape], Array.empty[Array[Char]], 0) { (acc, line) =>

        val (all, current, index) = acc

        if (line.isEmpty) {
            (all :+ Landscape(current), Array.empty[Array[Char]], index + 1)
        } else if (index == lastLine) {
            (all :+ Landscape(current :+ line.toCharArray), Array.empty[Array[Char]], index + 1)
        } else {
            (all, current :+ line.toCharArray, index + 1)
        }
    }._1

}


def part2Calc(is: Intscape, vert: Boolean): List[Intscape] = {

    if (DEBUG) println("\nTesting " + is.toString + " Vertical: " + vert)

    val obdMap = is.oneBitDiff
    if (DEBUG) println("OBDMap: " + obdMap)

    val obd = obdMap.map((a,b) => {        
        val tests = List(is.update(b, is.pattern(a)), is.update(a, is.pattern(b)))
        if (DEBUG) {
            println("Test with " + a + " " + b)        
            tests.foreach(_.printAsMap(vert))
        }
        tests.map(t => {
            val pairs = t.pairIndices.filterNot(_ == is.reflection - 1)
            if (DEBUG) println("Pairs: " + pairs.mkString(", "))
            
            pairs.map(p => {
                if (DEBUG) println("Pair: " + p)
                if (t.testReflection(p)) {
                    if (DEBUG) println("perfect " + t.toString)
                    Some(t.copy(reflection = p))
                } else {
                    if (DEBUG) println("not perfect")
                    None
                }
            }).flatten.distinctBy(_.reflection)


        }).flatten.distinctBy(_.reflection)
    }).flatten.distinctBy(_.reflection)
    println()
    obd
}


def bothParts(landscapes: List[Landscape]): Unit = {

    println("Part 1")

    val part1Reflections = landscapes.zipWithIndex.map((landscape, index) => {

        println("Landscape " + (index + 1) + ":")
        
        val reflectionsY = landscape.reflectionsY
        println("Horizontal reflections: ")
        reflectionsY.foreach(println)
        val reflectionsX = landscape.reflectionsX
        println("Vertical reflections: ")
        reflectionsX.foreach(println)
        println()

        val perfectY = reflectionsY.filter(_.isPerfect)
        println("Perfect horizontal: ")
        perfectY.foreach(println)
        val perfectX = reflectionsX.filter(_.isPerfect)
        println("Perfect vertical: ")
        perfectX.foreach(println)

        val horzTotal = perfectY.map(r => (r.at + 1)).sum
        val vertTotal = perfectX.map(r => (r.at + 1)).sum

        val total = vertTotal + 100 * horzTotal

        println("Total: " + total)

        println()

        (index, vertTotal, horzTotal)

    })

    println("Part 1 Total: " + part1Reflections.map(_._2).sum)

    println("=====================================")

    println("Part 2")

    val totals2 = landscapes.zipWithIndex.map((landscape, index) => {

        val pt1Reflection = part1Reflections(index)._1

        println("Landscape " + (index + 1) + ":")
        landscape.printLandscape

        val xInts = landscape.asIntArrayCols.copy(reflection = part1Reflections(index)._2)
        val desmudgedX = part2Calc(xInts, false)
        
        val yInts = landscape.asIntArrayRows.copy(reflection = part1Reflections(index)._3)
        val desmudgedY = part2Calc(yInts, true)

        val desmudgedXY = (desmudgedX ++ desmudgedY).distinct

        println("Repaired: ")
        desmudgedX.foreach(_.printAsMap(false))
        desmudgedY.foreach(_.printAsMap(true))

        val reflectionsX = desmudgedX.map(_.reflection + 1).distinct
        val reflectionsY = desmudgedY.map(_.reflection + 1).distinct

        println("Reflections X: " + reflectionsX.mkString(", "))
        println("Reflections Y: " + reflectionsY.mkString(", "))

        if (reflectionsX.length + reflectionsY.length > 1) {
            println("ERROR: More than one reflection")
            return
        } else if (reflectionsX.length + reflectionsY.length == 0) {
            println("ERROR: No reflections")
            return
        }

        println()

        reflectionsX.sum + 100 * reflectionsY.sum

    })

    println("Part 2 Totals: " + totals2.mkString(", "))

    println("Part 2 Total: " + totals2.sum)

}

val startTime = java.time.LocalTime.now

println("Start time: " + startTime)

val landscapes = parseFile(fileInput)
println("Number of landscapes: " + landscapes.length)
landscapes.foreach(landscape => {
    landscape.printLandscape
    println()
})

println()


bothParts(landscapes)

val endTime = java.time.LocalTime.now
println("End time: " + endTime)
println("Elapsed time: " + java.time.Duration.between(startTime, endTime))


// Part 1 Your puzzle answer was 34202.

// part 2 not 50100
