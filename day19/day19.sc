
import scala.io.Source
import scala.util.matching.Regex

val TEST = true
val DEBUG = true

val INPUTFILE = if (TEST) "day19\\test.txt" else "day19\\input.txt"

val fileInput = Source.fromFile(INPUTFILE).getLines.toList


trait Label 
case object Accepted extends Label {
    override def toString(): String = "A"
}
case object Rejected extends Label {
    override def toString(): String = "R"
}
case class Next(step: String) extends Label {
    override def toString(): String = step
}


val IN: Label = Next("in")

object Label {
    def apply(s: String): Label = s match {
        case "A" => Accepted
        case "R" => Rejected
        case _ => Next(s)
    }
}

trait Test(value: Int)

case object Default extends Test(0) {
    override def toString(): String = "?"
}

case class GreaterThan(v: Int) extends Test(v) {
    override def toString(): String = ">" + v
}
case class LessThan(v: Int) extends Test(v) {
    override def toString(): String = "<" + v
}
case class EqualTo(v: Int) extends Test(v) {
    override def toString(): String = "=" + v
}

object Test {
    def apply(s: String, v: Int): Test = s match {
        case ">" => GreaterThan(v)
        case "<" => LessThan(v)
        case "=" => EqualTo(v)
        case _ => Default
    }
}

case class XmasRange(valid: Map[String, Range], label: Label = IN)  {
    override def toString(): String = {
        valid.keys
             .toList
             .sorted
             .map(key => key + ":" + valid(key).toString).mkString("XmasRange(", ", ", ", " +label.toString + " : " + combinations + ")")
    }

    def combinations: BigInt = Array("x","m","a","s")
                                .map(c => valid.get(c) match {
                                    case Some(r) => r.end - r.start + 1
                                    case None => XmasRange.FULL_RANGE.end - XmasRange.FULL_RANGE.start + 1
                                })
                                .map(BigInt(_))
                                .product

}

object XmasRange {
    def FULL_RANGE = Range(1,4000)
    def full: XmasRange = XmasRange(
        Map(
            "x" -> FULL_RANGE,
            "m" -> FULL_RANGE, 
            "a" -> FULL_RANGE, 
            "s" -> FULL_RANGE
        )
    )
    def empty: XmasRange = XmasRange(Map.empty[String, Range])
}

case class XmasSequences(valid: Map[String, IndexedSeq[Int]]) {
    override def toString(): String = {
        "XmasSequences(" + valid.keys.toList.sorted.map(key => key + ": " + valid(key).size).mkString(", ") + " => " + combinations +")"
    }

    def combinations: BigInt = valid.values.map(_.size).map(BigInt(_)).product
}

object XmasSequences {
    def FULL_SEQ = Range(1,4001).toIndexedSeq
    def full = XmasSequences(
        Map(
            "x" -> FULL_SEQ,
            "m" -> FULL_SEQ, 
            "a" -> FULL_SEQ, 
            "s" -> FULL_SEQ
        )
    )

    def empty = XmasSequences(Map.empty[String, IndexedSeq[Int]])
}

case class Condition(field: String, test: Test, goto: Label, inverse: Boolean){
    override def toString(): String = (if (inverse) "!" else "") + field + test.toString + ":" + goto.toString

    def pass(part: Part): Boolean = {
        val rating = part.ratings(field)
        test match {
            case GreaterThan(x) => rating > x
            case LessThan(x) => rating < x
            case EqualTo(x) => rating == x
        }
    }

    def filterSeq(seq: IndexedSeq[Int]): IndexedSeq[Int] = {
        test match {
            case GreaterThan(x) if inverse => seq.filter(_ <= x)
            case LessThan(x) if inverse => seq.filter(_ >= x)
            case EqualTo(x) if inverse => seq.filter(_ != x)
            case GreaterThan(x) => seq.filter(_ > x)
            case LessThan(x) => seq.filter(_ < x)
            case EqualTo(x) => seq.filter(_ == x)
        }
    }

}

case class Step(conditions: Array[Condition], next: Label) {

    override def toString(): String = conditions.map(_.toString).mkString("{", ",", "," + next.toString + "}")

    def allAccept = conditions.forall(_.goto == Accepted && next == Accepted)

    def allReject = conditions.forall(_.goto == Rejected && next == Rejected)

    def process(part: Part): Part = {
        val label = conditions.find(_.pass(part)) match {
            case Some(c) => c.goto
            case None => next
        }
        Part(part.ratings, label)
    }

}

case class Workflow(steps: Map[Label, Step]) {

    override def toString(): String = {
        steps.map(s => s._1.toString + s._2.toString).mkString("\n")
    }

    def process(part: Part): Label = {

        if (part.label == Accepted || part.label == Rejected) {
            part.label
        } else {
            steps.get(part.label) match {
                case Some(step) => 
                    val newPart = step.process(part)
                    if (DEBUG) print(" -> " + newPart.label)
                    process(newPart)
                case None => 
                    println("No step found for label " + part.label)
                    part.label
            }
        }
    }

    def allRoutes: Array[Array[Condition]] = {

        def allRoutes(condition: Condition): Array[Array[Condition]] = {
            println("All routes for " + condition)
            steps.get(condition.goto) match {
                case Some(step) =>
                    print("Step: " + step)
                    val destinations = step.conditions ++ step.conditions.map(c => c.copy(inverse = true, goto = step.next))
                    println(destinations.mkString(" -> ",", ",""))
                    val routes = destinations.flatMap(allRoutes)
                    if (routes.length == 0) {
                        Array(Array(condition))
                    } else {
                        routes.map(condition +: _)
                    }

                case None => 
                    Array(Array(condition))
            }
        }

        allRoutes(Condition("f", Default, IN, false))
    }

    def applyConditions(conditions: Array[Condition]): XmasSequences = {

        val startSeqs = XmasSequences.full

        val result = conditions.foldLeft(startSeqs)((acc, cond) => 

            acc.valid.get(cond.field) match {
                case Some(seq) => 
                    val newSeq = cond.filterSeq(seq)
                    if (newSeq.isEmpty) {
                        XmasSequences.empty
                    } else {
                        XmasSequences(acc.valid.filterKeys(_ != cond.field).toMap + (cond.field -> newSeq))
                    }
                case None => 
                    acc
            }
        )
        result
    }


}

case class Part(ratings: Map[String, Int], label: Label) {
    override def toString(): String =
        ratings.map(r => r._1 + "=" + r._2).mkString("{", ",", "}")

    def totalRating: Int = ratings.values.sum
}


case class ParsedData(workflow: Workflow, parts: Array[Part])

def parseFile(lines: List[String]): ParsedData = {
    val workflowLines = lines.takeWhile(_.length > 0)
    val partLines = lines.drop(workflowLines.length + 1)
    
    val WF_REGEX = """(\w+)\{(.*),(\w+)\}""".r
    val STEP_REGEX = """(\w)([<|>])(\d+):([^,]+)""".r


    val workflow = workflowLines.map(wl => 
        wl match {
            case WF_REGEX(name, conditions, next) => 
                val conds = STEP_REGEX.findAllIn(conditions).map(c => 
                    c match {
                        case STEP_REGEX(field, cond, value, label) =>
                            Condition(field, Test(cond, value.toInt), Label(label), false)
                    }
                ).toArray
                Label(name) -> Step(conds, Label(next))
        }
    ).toMap

    val parts = partLines.map(pl => 
        val parts = pl.drop(1).dropRight(1).split(",")
        Part(parts.map(p => 
            val kv = p.split("=")
            kv(0) -> kv(1).toInt
        ).toMap, IN)
    ).toArray
    
    ParsedData(Workflow(workflow), parts)
}


def part1(data: ParsedData): Unit = {

    println("Part 1")

    val accepted = for {
        part <- data.parts
        _ = print(part.label.toString + " ")
        result = data.workflow.process(part)
        _ = println()
        if result == Accepted
    } yield {
        part
    }

    println ("Accepted: " + accepted.length)
    println ("Total rating: " + accepted.map(_.totalRating).sum)
    println()

}

def part2(data: ParsedData): Unit = {

    println("Part 2")

    val ar = data.workflow.allRoutes
    println("All routes: " + ar.length)
    ar.foreach(r => println(r.mkString(" -> ")))

    println("\nAccepted:")
    val accepted = ar.filter(_.last.goto == Accepted)
    accepted.foreach(r => println(r.mkString(" -> ")))

    val applied = accepted.map(data.workflow.applyConditions)
    println("\nApplied:")
    applied.foreach(println)

}

val startTime = java.time.LocalTime.now

println("Start time: " + startTime)

val data = parseFile(fileInput)

println("Parsed data:")
println(data.workflow)
println()
data.parts.foreach(println)

part1(data)

part2(data)


val endTime = java.time.LocalTime.now
println("End time: " + endTime)
println("Elapsed time: " + java.time.Duration.between(startTime, endTime))


// Part 1: Your puzzle answer was 389114