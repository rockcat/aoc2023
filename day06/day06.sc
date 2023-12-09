
import scala.io.Source
import scala.util.matching.Regex

val TEST = false
val DEBUG = true

val INPUTFILE = if (TEST) "day06\\test.txt" else "day06\\input.txt"

val fileInput = Source.fromFile(INPUTFILE).getLines.toList

// race length = l
// charge time = c
// distance = c * (l-c) = -c^2 + cl + 0

// a = -c, b = l, c = 0  

def quadSolutions(length: Long, record: Long): (Double, Double) = {
    val a = -1
    val b = length
    val c = -record
    val d = b*b - 4*a*c
    val x1 = (-b + Math.sqrt(d)) / (2*a)
    val x2 = (-b - Math.sqrt(d)) / (2*a)
    (x1, x2)
}

val raceLengths = fileInput(0).split(" ").tail.map(_.trim).filterNot(_.isBlank).map(_.toLong)
val distances = fileInput(1).split(" ").tail.map(_.trim).filterNot(_.isBlank).map(_.toLong)

println("Lengths: " + raceLengths.mkString(","))
println("Distances: " + distances.mkString(","))

def distanceForCharge(charge: Long, length: Long): Long = {
    charge * (length - charge)
}

case class Win(min: Long, max: Long) {
    def count = max - min + 1
}

def waysToWin(lengths: Array[Long], records: Array[Long]): Array[Win] = {
    println("Calculating ways to win for" +
        "lengths: " + lengths.mkString(",") + "\n" +
        "records: " + records.mkString(","))
    (for {
        i <- 0 until lengths.length
        length = lengths(i)
        record = records(i)
        (min,max) = quadSolutions(length, record)
    } yield {
        println(s"Race $i Length $length Record $record")
        println(s"min: $min max: $max")
        Win(Math.floor(min + 1).toLong, Math.ceil(max - 1).toLong)
    }).toArray
}

val waysToWin1 = waysToWin(raceLengths, distances)
println("Ways to win: " + waysToWin1.mkString(","))
val product = waysToWin1.map(_.count).product
println()
println("Star 11: " + product)

val longRace = raceLengths.map(_.toString).mkString.toLong
val longRecord = distances.map(_.toString).mkString.toLong

println()
println("Long Race: " + longRace)
println("Long Record: " + longRecord)

val waysToWin2 = waysToWin(Array(longRace), Array(longRecord))
println("Ways to win: " + waysToWin2.mkString(","))
println()
println("Star 12: " + waysToWin2(0).count)
