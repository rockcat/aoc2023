import scala.io.Source
import scala.util.matching.Regex

val INPUTFILE = "input2.txt"
val DEBUG = true

val input = Source.fromFile(INPUTFILE).getLines.toList

case class Combo(red: Int, green: Int, blue: Int) {

    def addCount(str: String): Combo = {
        val parts = str.trim.split(" ")
        val num = parts(0).trim.toInt
        val colour = parts(1).trim.toLowerCase
        colour match {
            case "red" => Combo(red + num, green, blue)
            case "green" => Combo(red, green + num, blue)
            case "blue" => Combo(red, green, blue + num)
        }
    }

    def addCounts(counts: Array[String]): Combo = {
        if (counts.length == 0) {
            this
        } else {
            addCount(counts.head).addCounts(counts.tail)
        }
    }

    def possible(max: Combo): Boolean = {
        red <= max.red && green <= max.green && blue <= max.blue
    }

    def power: Int = {
        red * green * blue
    }
}

object Combo {

    def apply(str: String): Combo = {
        Combo(0,0,0).addCounts(str.split(","))
    }
}


case class Game(id: Int, counts: List[Combo]) {

    def possible(max: Combo): Boolean = {
        counts.forall(_.possible(max))
    }

    def minPossible: Combo = {
        val red = counts.maxBy(_.red)
        val green = counts.maxBy(_.green)   
        val blue = counts.maxBy(_.blue)        
        Combo(red.red, green.green, blue.blue)
    }

}

object Game {

    def apply(gameStr: String): Game = {
        val parts = gameStr.split(":")
        val id = parts(0).split(" ").map(_.trim).last.toInt
        val counts = parts(1).split(";").map( pulled =>
             Combo(pulled)
        )
        Game(id, counts.toList)
    }
}

val MAX = Combo(12, 13, 14)

val games = input.map(Game(_))

val possibleGames = games.filter(_.possible(MAX))
println("Star 3 : " + possibleGames.map(_.id).sum)

val powers = games.map(g => 
    val minPos = g.minPossible
    println(s"${g.id} ${minPos} ${minPos.power}")
    minPos.power
)

println("Star 4 : " + powers.sum)



// star 3   2369