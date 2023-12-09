import scala.io.Source
import scala.util.matching.Regex

val INPUTFILE = "input1.txt"
val DEBUG = true

val input = Source.fromFile(INPUTFILE).getLines.toList


val star1Regex: Regex = "1|2|3|4|5|6|7|8|9|0".r
val star1revRegex: Regex = "1|2|3|4|5|6|7|8|9|0".r
val star2Regex: Regex = "1|2|3|4|5|6|7|8|9|0|one|two|three|four|five|six|seven|eight|nine".r
val star2revRegex: Regex = "1|2|3|4|5|6|7|8|9|0|eno|owt|eerht|ruof|evif|xis|neves|thgie|enin".r

def numberStrToValue(str: String) = {
    str match {
        case "one" => "1"
        case "two" => "2"
        case "three" => "3"
        case "four" => "4"
        case "five" => "5"
        case "six" => "6"
        case "seven" => "7"
        case "eight" => "8"
        case "nine" => "9"
        case _ => str
    }
}


val codes = input.map(l => {
    val first = star2Regex.findFirstIn(l).get
    val last = star2revRegex.findFirstIn(l.reverse).get
    println(first + " " +last)
    (numberStrToValue(first) + numberStrToValue(last.reverse)).toInt
})

val sum = codes.sum
print(sum)


// star 1 54390
// star 2 54277