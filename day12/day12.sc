import scala.collection.mutable.HashMap

import scala.io.Source
import scala.util.matching.Regex

val TEST = false
val DEBUG = false

val INPUTFILE = if (TEST) "day12\\test.txt" else "day12\\input.txt"

val fileInput = Source.fromFile(INPUTFILE).getLines.toList

val QUERY: Char = '?'
val DOT: Char = '.'
val HASH: Char = '#'

case class Pattern(raw: String, valid: List[Int]) {

    def *(n: Int): Pattern = {
        val multRaw = (List.fill(n)(raw)).mkString("?")
        val multValid = (List.fill(n)(valid)).flatten
        Pattern(multRaw, multValid)
    }


    def validRegex(vStr: List[Int]): Regex = {
        val matchHash = for {
            v <- vStr
            r = s"#{$v}"
        } yield r
        ("""^\.*""" + matchHash.mkString("""\.+""") + """\.*$""").r
    }

    def combinations(base: String, valid: Regex): List[String] = {

        def makeCombo(v: Long): String = {
            base.reverse.foldLeft((v, ""))((acc, c) => {
                val (v, str) = acc
                if (c == QUERY) {
                    (v/2, str + (if (v % 2 == 1) HASH else DOT))
                } else {
                    (v, str + c)
                }
            })._2.reverse
        }

        val numQuery = base.filter(_ == QUERY).length
        val padding = "." * numQuery

        (for {
            i <- 0 until Math.pow(2,numQuery).toInt
            merged = makeCombo(i)
            if (merged.matches(valid.toString))
        } yield {
            merged
        }).toList

    }

    def validCount: Long = {

        var cache = HashMap[String, Long]()

        def cacheKey(str: String, nums: List[Int], hash: String): String = {
            str + nums.mkString(",") + hash
        }


        def deadEnd(str: String): Long = {
            if (DEBUG) {
                println("Dead end: " + str)
            }
            0
        }

        def count(str: String, nums: List[Int], soFar: String = "", hash: String = ""): Long = {

            val ck = cacheKey(str, nums, hash)

            if (cache.contains(ck)) {
                if (DEBUG) {
                    println("Cache hit: " + ck + "  Result:" + cache(ck))
                }
                cache(ck)
            } else {

                val result = if (str.isEmpty || nums.isEmpty) {

                    val result = if ((str.isEmpty && nums.isEmpty) ||
                                    (str.isEmpty && nums.length == 1 && hash.length == nums.head) ||
                                    (nums.isEmpty && str.forall(c => (c == DOT || c == QUERY)))
                                    ) {
                                        1L
                                    } else {
                                        0L
                                    }
                    if (DEBUG) {
                        println("SoFar:" + soFar + str + " Result:" + result)
                    }
                    result
                } else {
                    val newSoFar = soFar + str.head
                    str.head match {
                        case HASH if (hash.length >= nums.head) => deadEnd(soFar)
                        case HASH => count(str.tail, nums, newSoFar, hash + HASH)

                        case DOT if (hash.length == nums.head) => count(str.tail, nums.tail, soFar + '!')
                        case DOT if (hash.length > 0 && hash.length != nums.head) => deadEnd(soFar)
                        case DOT => count(str.tail, nums, newSoFar)

                        case QUERY if (hash.length == nums.head) => count(str.tail, nums.tail, soFar + '!')
                        case QUERY if (hash.length > 0) => count(str.tail, nums, soFar + HASH, hash + HASH)
                        case QUERY => count(str.tail, nums, soFar + HASH, hash + HASH) + count(str.tail, nums, soFar + DOT)
                    }

                }
                cache += (ck -> result)
                result
            }
        }

        count(raw, valid)

    }

}


def parseFile(lines: List[String]): List[Pattern] =
    lines.map(line => {
        val pattern = line.split(" ")
        val raw = pattern(0)
        val valid = pattern(1).split(",").map(_.toInt).toList
        Pattern(raw, valid)
    })

def part1(patterns: List[Pattern]): Unit = {

    println("Part 1")
    val counts = patterns.map { p =>
        val matcher = p.validRegex(p.valid)
        if (DEBUG) {
            println("Pattern:" + matcher.toString + "   Raw: " + p.raw + "   Valid: " + p.valid.mkString(","))
        }
        val combos = p.combinations(p.raw, matcher)
        if (DEBUG) {
            println("Combinations:" + combos.length)
            combos.foreach(println)
            println()
        }
        combos.length
    }
    println("Counts:" + counts.mkString(","))
    val total = counts.sum
    println("Total:" + total)
    println()
}

def matchesAgainst(raw: String, str: String): Boolean = {
    val zipped = str.zip(raw)
    zipped.forall { case (c, r) => (r == QUERY) || c == r }
}

def combosFromValid(left: List[Int], len: Int, validate: String): List[String] = {

    if (left.isEmpty) {
        List("")
    } else {
        val v = "#" * left.head
        val rest = left.tail

        val tail = if (rest.length == 0) "" else "."
        val remainderLen = (rest.sum + rest.length - 1)
        val maxLen = if (remainderLen > 0) len - remainderLen else len    // leave room for remainder
        val dotLen = maxLen - v.length - tail.length

        (for {
            i <- 0 to dotLen
            prefix = "." * i
            j <- 0 to dotLen - i
            suffix = "." * j
            combo = prefix + v + suffix + tail
            comboLen = combo.length
            if matchesAgainst(validate, combo)
            restCombo <- combosFromValid(rest, len - comboLen, validate.substring(comboLen))
            combined = combo + restCombo
            if (combined.length == len)
        } yield {
            combined
        }).toList.distinct
    }
}


def part2(patterns: List[Pattern]): Unit = {

    val MULTIPLIER = 5

    println("Part 2")

    val counts = patterns.map { p =>

        val multPattern = p * MULTIPLIER
        val c = multPattern.validCount

        if (DEBUG) {
            println(p)
            print("MultCounts:" + multPattern.valid.mkString(",") + " MultRaw:" + multPattern.raw)
            println(" Count: " + c)
        }
        c
    }
    println("Counts:" + counts.mkString(","))
    val total = counts.sum
    println("Total:" + total)
}

val startTime = java.time.LocalTime.now

println("Start time: " + startTime)

val patterns = parseFile(fileInput)

if (DEBUG) {
    patterns.foreach(println)
    println()
}

part1(patterns)

println("============================================================================")

part2(patterns)

val endTime = java.time.LocalTime.now
println("End time: " + endTime)
println("Elapsed time: " + java.time.Duration.between(startTime, endTime))

// Part 1 Your puzzle answer was 7922.

// Part 2 Total:18093821750095