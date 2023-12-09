import javax.smartcardio.Card

import scala.io.Source
import scala.util.matching.Regex

val TEST = false
val DEBUG = true

val INPUTFILE = if (TEST) "day07\\test.txt" else "day07\\input.txt"

val fileInput = Source.fromFile(INPUTFILE).getLines.toList

trait HandType extends Ordered[HandType] {

    def score: Int = this match {
        case FiveOfAKind => 7
        case FourOfAKind => 6
        case FullHouse => 5
        case ThreeOfAKind => 4
        case TwoPairs => 3
        case OnePair => 2
        case HighCard(_) => 1
    }

    def addJoker: HandType = {
        this match {
            case FiveOfAKind => FiveOfAKind
            case FourOfAKind => FiveOfAKind
            case FullHouse => FourOfAKind
            case ThreeOfAKind => FourOfAKind
            case TwoPairs => FullHouse
            case OnePair => ThreeOfAKind
            case HighCard(_) => OnePair
            case EmptyHand => HighCard('J')
        }
    }

    def addJokers(numJokers: Int): HandType = {
        if (numJokers == 0) {
            this
        } else {
            addJokers(numJokers - 1).addJoker
        }
    }

    override def compare(that: HandType): Int = {
        this.score - that.score
    }
}

case class CardGroup(card: Char, count: Int)

object HandType {

    def fromArray(hand: Array[Char]): HandType = {
        val cards = hand.sorted
        val cardGroups = cards.groupBy(identity)
                              .map((c, a) => CardGroup(c, a.length))
                              .toList
                              .sortBy(_.count)(Ordering[Int].reverse)

        print("cards: " + cards.mkString + "\n")
        print("cardGroups: " + cardGroups + "\n")

        val group1Count = if (cardGroups.length > 1) cardGroups(1).count else 0

        if (cardGroups.length > 0) {
            cardGroups(0).count match {
                case 5 => FiveOfAKind
                case 4 => FourOfAKind
                case 3 => group1Count match {
                    case 2 => FullHouse
                    case _ => ThreeOfAKind
                }
                case 2 => group1Count match {
                    case 2 => TwoPairs
                    case _ => OnePair
                }
                case 1 => 
                    val highCard = cards.sortBy(Hand.CARDS.indexOf(_)).last
                    HighCard(highCard)
                case _ => EmptyHand
            }
        } else {
            EmptyHand
        }
    }

}

case object FiveOfAKind extends HandType
case object FourOfAKind extends HandType
case object FullHouse extends HandType
case object ThreeOfAKind extends HandType
case object TwoPairs extends HandType
case object OnePair extends HandType
case class HighCard(card: Char) extends HandType
case object EmptyHand extends HandType

case class Hand(cards: Array[Char], handType: HandType, bid: Int, jokerHandType: HandType) extends Ordered[Hand] {

    def highestCardIndex(deck: String): Int = {
        deck.indexOf(cards(0))
    }

    def highestCard(deck: String): Char = {
        cards.sortBy(deck.indexOf(_)).last
    }

    def compareByCards(deck: String, that: Hand, index: Int = 0): Int = {

        def cardByCard(index: Int = 0): Int = {
            if (this.cards.length == index) {
                0
            } else {
                val comp = deck.indexOf(this.cards(index)) - deck.indexOf(that.cards(index))
                if (comp == 0) {
                    cardByCard(index + 1)
                } else {
                    comp
                }
            }
        }

        cardByCard()

    }

    def compareWithJoker(that: Hand): Int = {
        this.jokerHandType.compare(that.jokerHandType) match {
            case 0 => compareByCards(Hand.CARDSJOKER, that)
            case x => x
        }
    }

    override def compare(that: Hand): Int = {
        this.handType.compare(that.handType) match {
            case 0 => compareByCards(Hand.CARDS, that)
            case x => x
        }
    }

    override def toString(): String = {
        "Hand( " + cards.mkString + "\t" + handType + "\t" + bid + "\t" + jokerHandType + " )"
    }
     
}

object Hand {

    def CARDS = "23456789TJQKA"
    def CARDSJOKER = "J23456789TQKA"

}

case class ParsedData(hands: List[Hand] = List()) {

    override def toString(): String = {
        "ParsedData:\n" + hands.mkString("\n")
    }

}
case class Result(part: Int, winnings: Long = 0) {

    override def toString(): String = {
        "Result for part " + part + ":\n" +
        "Winnings: " + winnings + "\n\n"
    }

    def print: Unit = {
        println(this.toString)
    }

}

object Result {
    def empty = Result(0)
}

def parseFile(lines: List[String]) = {

    val hands = lines.map { line =>

        println(line)
        val parts = line.split(" ").map(_.trim)

        val hand = parts(0).toArray
        val bid = parts(1).toInt
        val handType = HandType.fromArray(hand)
        val numJokers = hand.filter(_ == 'J').length

        val handWithoutJokers = HandType.fromArray(hand.filter(_ != 'J'))
        val jokerHandType = handWithoutJokers.addJokers(numJokers)

        Hand(hand, handType, bid, jokerHandType)
    }

    ParsedData(hands)
}




def part1(data: ParsedData): Result = {

    val orderdHands = data.hands.sorted.zipWithIndex

    if (DEBUG) {
        orderdHands.foreach { case (hand, index) =>
            println(index + ": " + hand)
        }
    }

    val winnings = orderdHands.map { case (hand, index) =>
        hand.bid * (index + 1)
    }.sum

    Result(1, winnings)
}

def part2(data: ParsedData): Result = {

    val orderdHands = data.hands.sortWith( (h1, h2) => h1.compareWithJoker(h2) <= 0 ).zipWithIndex
    

    orderdHands.foreach { case (hand, index) =>
        println(index + ": " + hand)
    }

    val winnings = orderdHands.map { case (hand, index) =>
        hand.bid * (index + 1)
    }.sum

    Result(2, winnings)
}

val data = parseFile(fileInput)

val result1 = part1(data)
result1.print

val result2 = part2(data)
result2.print


// Result for part 1:
// Winnings: 248422077
// Result for part 2:
// Winnings: 249817836
