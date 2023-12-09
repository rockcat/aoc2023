import scala.io.Source
import scala.util.matching.Regex

val TEST = false
val DEBUG = false

val INPUTFILE = if (TEST) "day05\\test.txt" else "day05\\input.txt"

val fileInput = Source.fromFile(INPUTFILE).getLines.toList

val SEED_PATTERN = """seeds:([\s+\d+]+)""".r
val MAP_HEADER = """(\w+)-to-(\w+)\s+map:""".r
val MAP_ROW = """(\d+)\s+(\d+)\s+(\d+)\s*""".r

case class Segment(dest: Long, source: Long, range: Long)
case class AMap(from: String, to: String, segments: List[Segment]) {
    override def toString(): String = {
        from + "-to-" + to + " map:\n" +
        segments.map(s => s"${s.dest} ${s.source} ${s.range}").mkString("\n") +
        "\n"
    }
    
    def ==> (n: Long): Long = {
        val seg = segments.find(s => (n >= s.source) && (n < s.source + s.range))
        seg match {
            case Some(s) => s.dest + (n - s.source)
            case None => n
        }
    }
}

object AMap {
    def empty = AMap("", "", List.empty[Segment])
}

case class SeedRange(start: Long, len: Long) {
    def overlaps(that: SeedRange): Boolean = {
        (start >= that.start && start < that.start + that.len) ||
        (that.start >= start && that.start < start + len)
    }
}

case class  Almanac(seeds: List[SeedRange], maps: List[AMap]) {
    override def toString(): String = {
        "seeds: " + seeds.mkString(" ") + "\n\n" +
        maps.map(_.toString).mkString("\n")
    }

    def lookup(amap: AMap, n: Long): Long = {
        val next = amap ==> n
        if (DEBUG) {
            print(amap.from + " " + n + " ==> " + amap.to + " " + next + " ==> ")
        }
        next
    }

    def lookup(from: String, to: String, n: Long): Long = {
        val aMap = maps.find(m => (m.from == from) && (m.to == to))
        aMap match {
            case Some(m) => {
                lookup(m, n)
            }
            case None => n
        }
    }

    def lookup(chain: List[String], n: Long): Long = {
        val from = chain.head
        val to = chain.tail.head
        val next = lookup(from, to, n)
        if (chain.length == 2) {
            if (DEBUG) println()
            next
        } else {
            lookup(chain.tail, next)
        }
    }

    def lookupMap(chain: List[AMap], n: Long): Long = {
        val next = chain.head ==> n
        if (chain.length == 1) {
            if (DEBUG) println()
            next
        } else {
            lookupMap(chain.tail, next)
        }
    }    
}

object Almanac {
    def empty = Almanac(List.empty[SeedRange], List.empty[AMap])
}

def parseDefinitions(lineNo: Int, input: List[String], almanac: Almanac, partialMap: AMap): Almanac = {

    def finishPartialMap(almanac: Almanac, partialMap: AMap): Almanac = {
        if (partialMap.segments.length > 0) {
            val newAlmanac = almanac.copy(maps = almanac.maps :+ partialMap)
            newAlmanac
        } else {
            almanac
        }
    }

    println(s"Line $lineNo: ")
    val nextLineNo = lineNo + 1

    if (input.length == 0) {
        finishPartialMap(almanac, partialMap)
    } else {
        val (newAlmanac, newMap ) = input.head match {

            // capture seeds
            case SEED_PATTERN(seeds) => {
                val seedList = seeds.split("\\s+").filter(_.length > 0).map(i => i.trim.toLong).toList

                val allSeeds = seedList.grouped(2).toList.map(l => SeedRange(l(0), l(1))).sortBy(_.start)

                allSeeds.foreach(println)

                val merged = allSeeds.foldLeft(List.empty[SeedRange]) { (a, s) =>
                    if (a.length > 0 && a.last.overlaps(s)) {
                        val start = Math.min(a.last.start, s.start)
                        val length = Math.max(a.last.start + a.last.len, s.start + s.len) - start
                        val newLast = SeedRange(start, length)
                        a.dropRight(1) :+ newLast
                    } else {
                        a :+ s
                    }   
                }

                merged.foreach(println)

                val newAlmanac = almanac.copy(seeds = merged)
                (newAlmanac, AMap.empty)
            }

            // start a map
            case MAP_HEADER(from,to) => {
                val aMap = AMap(from, to, List.empty[Segment])
                (almanac, aMap)
            }

            // add to map
            case MAP_ROW(dest, source, range) => {
                val segment = Segment(dest.toLong, source.toLong, range.toLong)
                val newMap = partialMap.copy(segments = partialMap.segments :+ segment)
                (almanac, newMap)
            }

            // finish map if there one
            case "" => {
                if (partialMap.segments.length > 0) {
                    val newAlmanac = almanac.copy(maps = almanac.maps :+ partialMap)
                    (newAlmanac, AMap.empty)
                } else {
                    (almanac, AMap.empty)
                }
            }
            
            // ignore anything else
            case _ => {
                (almanac, partialMap)
            }
        }
        parseDefinitions(nextLineNo, input.tail, newAlmanac, newMap)
    }
}

val almanac = parseDefinitions(1, fileInput, Almanac.empty, AMap.empty)
println("Alamanac")
println(almanac)

val v1Seeds = almanac.seeds.flatMap(s => List(s.start, s.len))

for (t <- v1Seeds) {
    println(s"$t ==> ${almanac.lookup("seed","soil", t)}")
}

val NEAREST = List("seed","soil","fertilizer","water","light","temperature","humidity","location")
val NEAREST_MAPS = {
    (for {
        i <- 0 until NEAREST.length - 1
        from = NEAREST(i)
        to = NEAREST(i+1)
        map = almanac.maps.find(m => (m.from == from) && (m.to == to)).headOption
        if map.isDefined
    } yield map.get).toList
}


val lowest = v1Seeds.map(s => almanac.lookup(NEAREST, s)).min
println(s"Star 9: $lowest")

def lowestInRanges(range: List[SeedRange]): Long = {

    def low(s: Long, f: Long, lowest: Long = Long.MaxValue): Long = {
        if (s > f) {
            lowest
        } else {
            almanac.lookupMap(NEAREST_MAPS, s) match {
                case n if n < lowest => low(s + 1, f, n)
                case _ => low(s + 1, f, lowest)
            }
            // almanac.lookup(NEAREST, s) match {
            //     case n if n < lowest => low(s + 1, f, n)
            //     case _ => low(s + 1, f, lowest)
            // }
        }
    }

    def lowR(s: List[SeedRange], lowest: Long = Long.MaxValue): Long = {
        if (s.length == 0) {
            lowest
        } else {
            val next = s.head
            println("Checking range " + next.start + " to " + (next.start + next.len - 1))
            val newLowest = low(next.start, next.start + next.len - 1, lowest)
            lowR(s.tail, newLowest)
        }
    }

    lowR(range)
}

val lowest2 = lowestInRanges(almanac.seeds)
println(s"Star 10: $lowest2")