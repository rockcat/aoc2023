
import scala.io.Source
import scala.util.matching.Regex

val TEST = true
val DEBUG = true

val INPUTFILE = if (TEST) "day16\\test.txt" else "day16\\input.txt"

val fileInput = Source.fromFile(INPUTFILE).getLines.toList

case class ParsedData()


def parseFile(lines: List[String]): ParsedData= ???



def part1(data: ParsedData): Unit = {

    println("Part 1")

}

def part2(data: ParsedData): Unit = {

    println("Part 2")

}

val startTime = java.time.LocalTime.now

println("Start time: " + startTime)

val data = parseFile(fileInput)

part1(data)

part2(data)


val endTime = java.time.LocalTime.now
println("End time: " + endTime)
println("Elapsed time: " + java.time.Duration.between(startTime, endTime))


