
import scala.io.Source
import scala.util.matching.Regex

val TEST = false
val DEBUG = true

val INPUTFILE = if (TEST) "day07\\test.txt" else "day07\\input.txt"

val fileInput = Source.fromFile(INPUTFILE).getLines.toList

case class ParsedData()


def parseFile[T](lines: List[String]): T = ???



def part1(data: ParsedData): Unit = {

    println("Part 1")

}

def part2(data: ParsedData): Unit = {

    println("Part 2")

}

val data = parseFile(fileInput)

part1(data)

part2(data)





