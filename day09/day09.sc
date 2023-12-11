
import scala.io.Source
import scala.util.matching.Regex

val TEST = false
val DEBUG = true

val INPUTFILE = if (TEST) "day09\\test.txt" else "day09\\input.txt"

val fileInput = Source.fromFile(INPUTFILE).getLines.toList

case class Sequence(numbers: List[Long], level: Int = 0) {
    
    override def toString(): String = {
        "   " * level +
        numbers.mkString("   ")
    }

    def differences: Sequence = {
        Sequence(numbers.sliding(2).map(pair => pair(1) - pair(0)).toList, level + 1)
    }

    def allSame: Boolean = {
        numbers.forall(_ == numbers(0))
    }

    def allZero: Boolean = {
        numbers.forall(_ == 0)
    }

    def predict: Sequence = {
        val newSeq = if (allZero) {
            Sequence(numbers, level)
        } else {
            println(this)
            val prediction = differences.predict
            println(prediction)
            val newVal = prediction.lastNumber + lastNumber
            Sequence(numbers :+ newVal, level - 1)
        }
        newSeq
    }

    def predictNeg: Sequence = {
        val newSeq = if (allZero) {
            Sequence(numbers, level)
        } else {
            println(this)
            val prediction = differences.predictNeg
            println(prediction)
            val newVal = firstNumber - prediction.firstNumber
            Sequence(List(newVal) ++ numbers, level - 1)
        }
        newSeq
    }

    def firstNumber = numbers.head
    def lastNumber = numbers.last

}


def parseFile(lines: List[String]): List[Sequence] = {
    lines.map(line => Sequence(line.split(" ").map(_.trim.toLong).toList))
}

def part1(data: List[Sequence]): Unit = {

    println("Part 1")

    val predictions = data.map(sequence => {
        val predicted = sequence.predict
        println(predicted)
        println()
        predicted.lastNumber
    })

    println(predictions)
    println("Sum of predictions: " + predictions.sum)

}

def part2(data: List[Sequence]): Unit = {

    println("Part 2")

    val predictions = data.map(sequence => {
        println(sequence)
        val predicted = sequence.predictNeg
        println(predicted)
        println()
        predicted.firstNumber
    })

    println(predictions)
    println("Sum of predictions: " + predictions.sum)

}

val data = parseFile(fileInput)

data.foreach(println)
println()

part1(data)

part2(data)


// Part 1 Sum of predictions: 1930746032
// Part 2 Sum of predictions: 1154




