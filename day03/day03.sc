import scala.io.Source
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

val INPUTFILE = "day03\\input.txt"
val DEBUG = true

val input = Source.fromFile(INPUTFILE).getLines.toList

val maxX = input(0).length
val maxY = input.length


case class Coordinate(x: Int, y: Int) {

    override def toString(): String = s"($x,$y)"

    def inRange(minX: Int, maxX: Int, minY: Int, maxY: Int): Boolean = {
        x >= minX && x <= maxX && y >= minY && y <= maxY
    }

    def leftOf: Option[Coordinate] = {
        if (x == 0) {
            None
        } else {
            Some(Coordinate(x-1,y))
        }
    }

    def rightOf: Option[Coordinate] = {
        if (x == maxX - 1) {
            None
        } else {
            Some(Coordinate(x+1,y))
        }
    }

    def above: Option[Coordinate] = {
        if (y == 0) {
            None
        } else {
            Some(Coordinate(x,y-1))
        }
    }

    def below: Option[Coordinate] = {
        if (y == maxY - 1) {
            None
        } else {
            Some(Coordinate(x,y+1))
        }
    }

    def aboveLeft: Option[Coordinate] = {
        if (y == 0 || x == 0) {
            None
        } else {
            Some(Coordinate(x-1,y-1))
        }
    }

    def aboveRight: Option[Coordinate] = {
        if (y == 0 || x == maxX - 1) {
            None
        } else {
            Some(Coordinate(x+1,y-1))
        }
    }

    def belowLeft: Option[Coordinate] = {
        if (y == maxY - 1 || x == 0) {
            None
        } else {
            Some(Coordinate(x-1,y+1))
        }
    }

    def belowRight: Option[Coordinate] = {
        if (y == maxY - 1 || x == maxX - 1) {
            None
        } else {
            Some(Coordinate(x+1,y+1))
        }
    }
    
    def surrounding: Array[Coordinate] = {
        Array(aboveLeft, above, aboveRight, 
              leftOf, rightOf, 
              belowLeft, below, belowRight)
        .flatten
    }
}

case class Engine(parts: Array[Array[Char]]) {

    val NUMBERS = "0123456789"
    val numRegex: Regex = "[0-9]+".r

    def numberAt(c: Coordinate): Boolean = {
        val part = parts(c.y)(c.x)
        NUMBERS.contains(part)
    }

    def findAll(cond: Char => Boolean): List[Coordinate] = {
        val coords = for {
            y <- 0 until maxY
            x <- 0 until maxX
            part = parts(y)(x)
            if cond(part)
        } yield Coordinate(x,y)
        coords.toList
    }

    
    def findSymbols: List[Coordinate] = {
        findAll( x => (x != '.' && !NUMBERS.contains(x)) )
    }

    def findGears: List[Coordinate] = {
        findAll(_ == '*')
    }

    def findNumbers(from: List[Coordinate]): List[Coordinate] = {
        val coords = for {
            c <- from
            around = c.surrounding
            numbers = around.filter(numberAt)
            a <- numbers
        } yield a
        coords.toList
    }

    def gearNumberCoords(from: List[Coordinate]): List[(Coordinate, List[Coordinate])] = {
        from.map(c => (c, findNumbers(List(c))) )
    }

    def allNumbers: Array[(Iterator[Match], Int)] = {
        parts.map(line => 
            numRegex.findAllMatchIn(line.mkString)
        ).zipWithIndex
    }

    def collidingMatches(coords: List[Coordinate]): List[String] = {
        val matches = allNumbers
        val collisions: Array[Match] = for {
            (matches, line) <- matches
            m <- matches
            start = m.start
            end = m.end
            // _ = println(s"Checking $m, ${m.start}, ${m.end}, $line")
            if coords.exists(c => c.inRange(start, end, line, line))
        } yield m
        collisions.distinct.map(_.toString).toList
    }
}

val engine: Engine =  Engine(input.map(line => line.toArray).toArray)

val symbols = engine.findSymbols

println("Symbols")
symbols.foreach(println)

println("Numbers")
val numbers = engine.findNumbers(symbols)
numbers.foreach(n => print(n.toString + " "))
println()

println("Collisions")
val collisions = engine.collidingMatches(numbers)
collisions.foreach(n => print(n.toString + " "))
println()

val sum = collisions.map(_.toInt).sum
println()
println(s"Star 5: $sum")


println("Gears")
val gears = engine.findGears
gears.foreach(println)
println()

println("Numbers")
val gearNumbers = engine.findNumbers(gears)
gearNumbers.foreach(n => print(n.toString + " "))
println()

println("Collisions")
val gearNumberCoords = engine.gearNumberCoords(gears)
gearNumberCoords.foreach(n => println(n.toString + " "))
println()

val gearCollisions = gearNumberCoords.map((g,nums) => engine.collidingMatches(nums).map(_.toInt))
gearCollisions.foreach(n => print(n.toString + " "))
println()


val powers = gearCollisions.filter(_.length == 2).map(l => l(0) * l(1))
println("Powers")
powers.foreach(n => print(n.toString + " "))
println()

val sum2 = powers.sum

println()
println(s"Star 6: $sum2")
         