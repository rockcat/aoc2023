
import scala.io.Source
import scala.util.matching.Regex

val TEST = false
val DEBUG = false

val INPUTFILE = if (TEST) "day15\\test.txt" else "day15\\input.txt"

val fileInput = Source.fromFile(INPUTFILE).getLines.toList

trait Action
case object Remove extends Action
case class Insert(focalLen: Int) extends Action

val NUM_BOXES = 256

case class Instruction(box: Int, label: String, action: Action) {
    override def toString: String = {
        val aStr = action match {
            case Remove => "-"
            case Insert(l) => "=" + l
        }
        s"[$label$aStr]"
    }
}

case class Lens(label: String, focalLen: Int) {
    override def toString: String = {
        s"[$label $focalLen]"
    }
}

type Boxes = Array[List[Lens]]

case class LabelPrinter(boxes: Boxes) {

    def focusingPower: Int = {
        boxes.zipWithIndex.map((box, boxIndex) => 
            box.zipWithIndex.map((lens, slot) => 
                val power = (boxIndex + 1) * (slot + 1) * lens.focalLen
                if (DEBUG) println(s"${lens.label}: ${boxIndex + 1} (box ${boxIndex}) * ${slot + 1} (slot) * ${lens.focalLen} (focal length) = ${(boxIndex + 1) * (slot + 1) * lens.focalLen} = $power")
                power
            ).sum
        ).sum
    }
}

object LabelPrinter {
    def empty: LabelPrinter = LabelPrinter(Array.fill(NUM_BOXES)(List.empty[Lens]))
}


case class InitSeq(steps: List[String]) {

    def hash(s: String): Int = {
        s.foldLeft(0)((h, c) => {
            ((h + c.toInt) * 17) & 0xFF
        })
    }

    def hashSteps: List[Int] = {
        steps.map(hash)
    }

    def decodeInstruction(s: String): Instruction = {
        val pattern = """(\w+)([-=])(\d+)*""".r

        val decoded = s match {
            case pattern(lens, "-", _) => Instruction(hash(lens), lens, Remove)
            case pattern(lens, "=", label) => Instruction(hash(lens), lens, Insert(label.toInt))
        }
        if (DEBUG) {
            println(s + " => " + decoded)
        }
        decoded
    }

    def printBoxArray(boxes: Boxes): Unit = {
        boxes.zipWithIndex.foreach((box, i) => {
            if (box.nonEmpty) {
                println("Box " + i + ": " + box.map(_.toString).mkString(" "))
            }
        })
    }

    def executeInstructions(instructions: List[Instruction]): LabelPrinter = {

        instructions.foldLeft(LabelPrinter.empty)((p, i) => {
            val updatedBoxes: Boxes = i.action match {
                case Remove => {
                    val newBox = p.boxes(i.box).filterNot(_.label == i.label)
                    p.boxes.updated(i.box, newBox)
                }
                case Insert(l) => {
                    val box = p.boxes(i.box)
                    val lens = Lens(i.label, l)
                    val index = box.indexWhere(_.label == i.label)
                    if (index >= 0) {
                        p.boxes.updated(i.box, box.patch(index, List(lens), 1))
                    } else {
                        val newBox = box :+ lens
                        p.boxes.updated(i.box, newBox)
                    }
                }
            }
            if (DEBUG) {
                println(s"""After "${i.toString}":""")
                printBoxArray(updatedBoxes)
                println()
            }
            LabelPrinter(updatedBoxes)
        })
    }


}


def parseFile(lines: List[String]): InitSeq = {
    val steps = lines.map(_.split(",").map(_.trim)).flatten
    InitSeq(steps)
}



def part1(initSeq: InitSeq): Unit = {

    println("Part 1")

    val hashSteps = initSeq.hashSteps
    println(hashSteps.mkString(", "))
    val sum = hashSteps.sum
    println("Sum: " + sum)

}

def part2(initSeq: InitSeq): Unit = {

    println("Part 2")
    val instructions = initSeq.steps.map(initSeq.decodeInstruction)
    println()

    println(instructions.mkString(", "))
    println()

    val printer = initSeq.executeInstructions(instructions)
    println("Focusing power: " + printer.focusingPower)

}

val startTime = java.time.LocalTime.now

println("Start time: " + startTime)

val initSeq = parseFile(fileInput)

part1(initSeq)

part2(initSeq)


val endTime = java.time.LocalTime.now
println("End time: " + endTime)
println("Elapsed time: " + java.time.Duration.between(startTime, endTime))


