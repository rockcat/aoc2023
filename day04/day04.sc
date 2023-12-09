
import scala.io.Source
import scala.util.matching.Regex

val INPUTFILE = "day04\\input.txt"
// val INPUTFILE = "teststar7.txt"
val DEBUG = true

val input = Source.fromFile(INPUTFILE).getLines.toList

case class Card(id: Int, winning: Array[Int], card: Array[Int], winCount: Int, score: Int, additions: Array[Int]) {

    override def toString(): String = {
        def listWin(l: Array[Int]) = "(" + l.map(_.toString).mkString(" ") + ")"
        def format(n: Int): String = {
            if (winning.contains(n)) s"*$n*" else s" $n "
        }
        def listCard(l: Array[Int]) = "(" + l.map(format(_)).mkString(" ") + ")"

        s"$id ${listWin(winning)} ${listCard(card)}"
    }

}

object Card {

    val PATTERN = """Card\s+(\d+):([\s+\d+]+)\s\|([\s+\d+]+)""".r

    def apply(str: String): Card = {
        println(s"Parsing >$str<")
        val p = PATTERN.findFirstMatchIn(str).toList.head
        val id = p.group(1).toInt
        val winning = p.group(2).split("\\s+").filter(_.length > 0).map(i => i.trim.toInt)
        val card = p.group(3).split("\\s+").filter(_.length > 0).map(i => i.trim.toInt)

        val winCount: Int = card.filter(winning.contains(_)).length
        val score = Math.pow(2,winCount - 1).toInt

        val additions = if (winCount > 0) {
            val extras = (for (i <- (id + 1) until (id + 1 + winCount)) yield i).toArray
            extras
        } else {
            Array.empty[Int]
        }

        Card(id, winning, card, winCount, score, additions)
    }
}

def scoreCards(cds: Array[Card], sum: Int = 0): Int = {
    // println("Scoring " + cds.map(_.id).mkString(","))
    if (cds.length == 0) {
        sum
    } else {
        val all = cds.map(_.additions).flatten.map(i => cards(i - 1))
        sum + cds.length + scoreCards(all)
    }
}

val cards = input.map(Card(_)).toArray

val allIds = cards.map(_.id).toArray

println("Cards")
cards.foreach(c => {
    println(c.toString + " Additions:" + c.additions.mkString(",") + " Score : " + c.score)
})

val total = cards.map(_.score).sum
println("Star 7: " + total)


val total2 = scoreCards(cards)
println("Star 8: " + total2)


