
import scala.io.Source
import scala.util.matching.Regex

val TEST = false
val DEBUG = true

val INPUTFILE = if (TEST) "day08\\test3.txt" else "day08\\input.txt"

val fileInput = Source.fromFile(INPUTFILE).getLines.toList

type Steps = String
type Location = String
type Route = Map[Location, (Location, Location)]


case class Plan(steps: Steps, route: Route) {
    override def toString(): String = {
        "Steps = " + steps + "\n\n" +
        route.keys.toList.sorted.map(location => location + " = " + route(location)).mkString("\n")
    }
}

case class Result(part: Int, count: Int, multiplier: BigInt = 0) {

    override def toString(): String = {
        "Result for part " + part + ":\n" +
        "Count = " + count + "\n" +
        "Multiplier = " + multiplier
    }

    def print: Unit = {
        println(this.toString)
    }

}

object Result {
    def empty = Result(0, 0)
}

def gcd(a: BigInt, b: BigInt):BigInt = if (b==0) a.abs else gcd(b, a%b)
def lcm(values: List[BigInt]): BigInt = values.foldLeft(BigInt(1))((a, b) => (a/gcd(a,b))*b)

def parseFile(lines: List[String]) = {
    val steps = lines.head
    val routeLines = lines.tail.tail
    val pattern = """([0-9A-Z]+) = \(([0-9A-Z]+), ([0-9A-Z]+)\)""".r

    val route = routeLines.map(line =>
        line match {
            case pattern(location, left, right) => Some((location, (left, right)))
            case _ => None
        }
    ).flatten.toMap

    Plan(steps, route)
}


def routeStep(from: Location, step: Char, route: Route): Location = {
    val (left, right) = route(from)
    step match {
        case 'L' => left
        case 'R' => right
    }
}

def routeToTarget(from: Location, targetTest: String => Boolean,  plan: Plan): (Int, Location) = {

    val steps = plan.steps
    val route = plan.route

    def followRoute(from: Location, stepIndex: Int, stepCount: Int = 1): (Int, Location) = {
        val step = steps(stepIndex)
        if (DEBUG) {
            // val fromStr = from.mkString(" ")
            // println(s"from = $fromStr, $step")
        }
        val newLocation = routeStep(from, step, route)
        if (targetTest(newLocation)) {
            (stepCount, newLocation)
        } else {
            val nextStep = (stepIndex + 1) % steps.length
            followRoute(newLocation, nextStep, stepCount + 1)
        }
    }

    followRoute(from, 0)
}

def part1(plan: Plan): Unit = {


    val startLocation = "AAA"
    val (count,_) = routeToTarget(startLocation, _ == "ZZZ", plan)

    println("Part 1")
    println("Start location = " + startLocation)
    println("End location = ZZZ")
    println("Count = " + count)
}

def part2(plan: Plan): Unit = {


    val startLocations = plan.route.keys.filter(k => k.endsWith("A")).toList

    val results = startLocations.map(startLocation => routeToTarget(startLocation, _.endsWith("Z"), plan))

    println("Part 2")
    println("Start locations = " + startLocations.mkString("    "))

    results.foreach(r => println(r._1.toString + " " + r._2))

    val multiplier = lcm(results.map(_._1))

    println("Multiplier = " + multiplier)

}

val data = parseFile(fileInput)

println("Parser data")
println(data)

part1(data)

part2(data)


// part 1: Your puzzle answer was 16043.
// part 2: Your puzzle answer was 15726453850399