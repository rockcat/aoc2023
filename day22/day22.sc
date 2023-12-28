
import scala.io.Source
import scala.util.matching.Regex

val TEST = false
val DEBUG = false

val INPUTFILE = if (TEST) ".\\test.txt" else ".\\input.txt"

val fileInput = Source.fromFile(INPUTFILE).getLines.toList


type Space3D = Array[Array[Array[Int]]]

case class Voxel(x: Int, y: Int, z: Int) {
    def up: Voxel = Voxel(x, y, z+1)
    def down: Voxel = Voxel(x, y, z-1)
    def left: Voxel = Voxel(x-1, y, z)
    def right: Voxel = Voxel(x+1, y, z)
    def forward: Voxel = Voxel(x, y+1, z)
    def back: Voxel = Voxel(x, y-1, z)
}

case class Brick(label: String, start: Voxel, end: Voxel) {

    override def toString(): String = s"Brick($label, ${start} -> ${end})"

    def up = Brick(label, start.up, end.up)
    def down = Brick(label, start.down, end.down)
    def left = Brick(label, start.left, end.left)
    def right = Brick(label, start.right, end.right)
    def forward = Brick(label, start.forward, end.forward)
    def back = Brick(label, start.back, end.back)

    def size: Int = (end.x - start.x + 1) * (end.y - start.y + 1) * (end.z - start.z + 1)

    def voxels: List[Voxel] = {
        for {
            x <- start.x to end.x
            y <- start.y to end.y
            z <- start.z to end.z
        } yield Voxel(x, y, z)
    }.toList

    def contains(voxel: Voxel): Boolean = {
        voxel.x >= start.x && voxel.x <= end.x &&
        voxel.y >= start.y && voxel.y <= end.y &&
        voxel.z >= start.z && voxel.z <= end.z
    }

    def collide(other: Brick): Boolean = {
        contains(other.start) || contains(other.end) ||
        other.contains(start) || other.contains(end)
    }

    def brickAtXZ(x: Int, z: Int): Boolean = (x >= start.x && x <= end.x && z >= start.z && z <= end.z)
    def brickAtYZ(y: Int, z: Int): Boolean = (y >= start.y && y <= end.y && z >= start.z && z <= end.z)

    def onGround: Boolean = (start.z == 1 || end.z == 1)
}

case class Support(brick: Brick, supportedBy: List[Brick]) {
    
    override def toString(): String = brick.label + " supported by " + supportedBy.map(_.label).mkString(", ")
}

case class Scene(space: Space3D, bricks: List[Brick]) {

    def maxX = space.size
    def maxY = space(0).size
    def maxZ = space(0)(0).size


    def printScene: Unit = {
        println(s"Scene with ${bricks.size} bricks, dimensions ${space.size}x${space(0).size}x${space(0)(0).size}")
        bricks.foreach(b => println(b))
    }

    def lowestBrick: Brick = bricks.minBy(_.start.z)

    def dropBrick(brick: Brick, allBricks: List[Brick]): Support = {
        if (brick.onGround) {
            println(s"Brick ${brick.start} -> ${brick.end} on ground")
            Support(brick, List.empty[Brick])
        } else {
            val newPos = brick.down
            if (DEBUG) println(s"Brick ${brick.start} -> ${brick.end} dropping to ${newPos.start} -> ${newPos.end}")
            val collisions = allBricks.filter(_.collide(newPos))
            if (DEBUG) {
                println(s"Collisions: ${collisions.size}")
                collisions.foreach(b => println(s"Colliding brick ${b.start} -> ${b.end}"))
            }
            if (collisions.isEmpty) {
                if (DEBUG) print(" -> " + newPos)
                dropBrick(newPos, allBricks)
            } else {
                Support(brick, collisions)
            }
        }
    }

    def dropBricks: (Scene, List[Support]) = {
        val sorted = bricks.sortBy(b => Math.min(b.start.z, b.end.z))

        val (newBricks, support) = sorted.foldLeft((List.empty[Brick], List.empty[Support]))((acc, brick) => {
            val (allBricks, supportList) = acc
            print("\nDropping brick: " + brick)
            val newSupport = dropBrick(brick, allBricks)
            (allBricks :+ newSupport.brick , supportList :+ newSupport)

        })
        println()
        (Scene(space, newBricks), support)
    }

    def yView: Array[String] = {

        val rows = for (z <- maxZ to 1 by -1) yield {
            (for(y <- 0 until maxY) yield {
                val bricksAtZ = bricks.filter(_.brickAtYZ(y, z))
                if (bricksAtZ.isEmpty) {
                    '.'
                } else {
                    bricksAtZ.head.label.last
                }
            }).mkString
        }

        rows.toArray :+ ("-" * maxX)

    }

    def xView: Array[String] = {
        val rows = for (z <- maxZ to 1 by -1) yield {
            (for(x <- 0 until maxX) yield {
                val bricksAtZ = bricks.filter(_.brickAtXZ(x, z))
                if (bricksAtZ.isEmpty) {
                    '.'
                } else {
                    bricksAtZ.head.label.last
                }
            }).mkString
        }

        rows.toArray :+ ("-" * maxX)

    }

    def drawXYView: Unit = {
        (xView zip yView).foreach((x,y) => println(x + "    " + y))
        println()
    }

}

def parseFile(lines: List[String]): Scene = {

    def makeLabel(i: Int): String = {
        val base = i % 26
        val c = 'A' + base
        c.toChar + (if (i > 26) makeLabel(i / 26) else "")
    }

    val BRICK_REGEX = """(\d+),(\d+),(\d+)~(\d+),(\d+),(\d+)""".r

    val bricks = lines.zipWithIndex.map(l => 
        l._1 match {
            case BRICK_REGEX(x1, y1, z1, x2, y2, z2) => Brick(makeLabel(l._2), Voxel(x1.toInt, y1.toInt, z1.toInt), Voxel(x2.toInt, y2.toInt, z2.toInt))
        }
    )    
    val brickMaxX = bricks.map(_.end.x).max
    val brickMaxY = bricks.map(_.end.y).max
    val brickMaxZ = bricks.map(_.end.z).max

    val space = Array.ofDim[Int](brickMaxX + 1, brickMaxY + 1, brickMaxZ + 1)

    Scene(space, bricks)

}

def labelList(bricks: List[Brick]): String = bricks.map(_.label).mkString(", ")

def part1(scene: Scene): Unit = {

    println("Part 1")

    val (droppedScene, support) = scene.dropBricks
    if (DEBUG) {
        droppedScene.printScene
        println()
        support.foreach(println)
    }

    droppedScene.drawXYView

    val multiSupport = support.filter(_.supportedBy.size > 1)
    val multiSupporters = multiSupport.map(_.supportedBy).flatten.distinct.sortBy(_.label)
    val notSupporting = droppedScene.bricks.filterNot(b => support.exists(s => s.supportedBy.contains(b))).sortBy(_.label)

    println(s"\n${multiSupport.length} bricks have multiple supports:\n${multiSupport.mkString("\n")}")
    println(s"\n${multiSupporters.length} bricks support a brick with more than one support:\n${labelList(multiSupporters)}")
    println(s"\n${notSupporting.length} bricks are not supporting anything:\n")
    notSupporting.foreach(println)

    val removable = multiSupporters ++ notSupporting
    
    println("Bricks which could be removed:")
    println(labelList(removable))
 
    println(s"Number of removable bricks: ${removable.size}")

}

def part2(scene: Scene): Unit = {

    println("Part 2")

}

val startTime = java.time.LocalTime.now

println("Start time: " + startTime)

val scene = parseFile(fileInput)
scene.drawXYView

part1(scene)


val endTime = java.time.LocalTime.now
println("End time: " + endTime)
println("Elapsed time: " + java.time.Duration.between(startTime, endTime))


// Part 1 : 