import scala.annotation.tailrec

import scala.io.Source
import scala.util.matching.Regex

val TEST = false
val DEBUG = false

val INPUTFILE = if (TEST) "day20\\test3.txt" else "day20\\input.txt"

val fileInput = Source.fromFile(INPUTFILE).getLines.toList

trait Pulse {
    def flip = this match {
        case High => Low
        case Low => High
    }
}
case object High extends Pulse {
    override def toString(): String = "high"
}
case object Low extends Pulse {
    override def toString(): String = "low"
}

case class Signal(target: String, pulse: Pulse, from: String) {
    override def toString(): String = {
        s"$from -$pulse-> $target"
    }
}

case class MsgCount(low: Int, high: Int) {
    def product = low * high
    def +(other: MsgCount): MsgCount = MsgCount(low + other.low, high + other.high)
}

object MsgCount {
    def apply(signals: List[Signal]): MsgCount = {
        MsgCount(signals.count(_.pulse == Low), signals.count(_.pulse == High))
    }
}

case class IOState(name: String, pulse: Pulse) {
    override def toString(): String = s"$name: $pulse"

    def toSignal(from: String): Signal = Signal(name, pulse, from)

    def lowState(target: String): IOState = IOState(target, Low)
    def highState(target: String): IOState = IOState(target, High)
}

trait Module(name: String, inputs: List[IOState], outputs: List[IOState]) {

    def getName: String = name
    def displayName: String

    override def toString(): String = {

        val in = if (inputs.isEmpty) "" else inputs.map(_.toString).mkString("Inputs: ", ", ", "")
        val out = if (outputs.isEmpty) "" else outputs.map(_.toString).mkString("-> ", ", ", "")
        s"$displayName $out $in"
    }

    def recieve(inputs: List[Signal]): (Module, List[Signal])
}

object Module {
    val BUTTON = "button"
    val BROADCASTER = "broadcaster"
    val OUTPUT = "output"

    val NO_INPUTS = List[IOState]()
    val NO_OUTPUTS = List[IOState]()
}

type ModuleMap = Map[String, Module]

case object Button
    extends Module(Module.BUTTON, Module.NO_INPUTS, List(IOState(Module.BROADCASTER, Low))) {

    def displayName = Module.BUTTON

    def recieve(inputs: List[Signal]): (Module, List[Signal]) = {
        println("Error - button should not recieve signals")
        (this, List())
    }

    def press(config: Configuration): (Configuration, MsgCount) = {
        config.run(List(Signal(Module.BROADCASTER, Low, Module.BUTTON)), MsgCount(1,0))
    }
}

case class Output(name: String, inputs: List[IOState]) 
    extends Module(name,inputs, Module.NO_OUTPUTS) {

    def displayName = "OP: " + name

    def recieve(signals: List[Signal]): (Module, List[Signal]) = {
        if (DEBUG) {
            println("Output recieved signals:")
            signals.foreach(println)
        }
        (this, List())
    }
}


case class FlipFlop(name: String, inputs: List[IOState], outputs: List[IOState], state: Pulse) 
    extends Module(name, inputs, outputs) {

    def displayName = s"FF: %$name"
    
    override def recieve(signals: List[Signal]): (Module, List[Signal])= {

        val newInputs = signals.foldLeft(inputs) { (acc, next) =>
            val (from, pulse) = (next.from, next.pulse)
            acc.filterNot(_.name == from) :+ IOState(from,pulse)
        }

        if (signals.exists(_.pulse == Low)) {

            val newFlipFlop = signals.filter(_.pulse == Low).foldLeft((state, List.empty[Signal]))((acc, signal) => {

                val (accState, accSignals) = acc
                if (DEBUG) println(s"FlipFlop $name recieved low signal from ${signal.from}")
                val newState = accState.flip
                val newOutputs = outputs.map { output => IOState(output.name, newState) }
                (newState, accSignals ++ newOutputs.map(_.toSignal(name)))
            })

            (FlipFlop(name, inputs, outputs, newFlipFlop._1), newFlipFlop._2)
        } else {
            (FlipFlop(name, newInputs, outputs, state), List())            // no change to outputs
        }

    }
}

case class Conjunction(name: String, inputs: List[IOState], outputs: List[IOState]) 
    extends Module(name, inputs, outputs) {
    def displayName = s"CJ: &$name"

    override def recieve(signals: List[Signal]): (Module, List[Signal])= {

        val newInputs = signals.foldLeft(inputs) { (acc, next) =>
            val (from, pulse) = (next.from, next.pulse)
            acc.filterNot(_.name == from) :+ IOState(from,pulse)
        }



        val newPulse = if (newInputs.forall(_.pulse == High)) Low else High
        val newOutputs = outputs.map { output => IOState(output.name, newPulse) }
        val newConjunction = Conjunction(name, newInputs, newOutputs)

        val outputSignals = newOutputs.map(_.toSignal(name))
        (newConjunction, outputSignals)
    }

}

case class Broadcaster(outputs: List[IOState]) extends Module(Module.BROADCASTER, List(IOState(Module.BUTTON, Low)), outputs) {
        def displayName = Module.BROADCASTER

        override def recieve(inputs: List[Signal]): (Module, List[Signal])= {

        if (inputs.length > 1) {
            println(s"Too many inputs for broadcaster")
            (this, List())
        } else {
            val signal = inputs.head
            (this, outputs.map(_.toSignal(Module.BROADCASTER)))
        }
        }
}

case class Configuration(modules: ModuleMap) {

    override def toString(): String = {
        modules.values.mkString("\n")
    }

    val broadcaster = modules(Module.BROADCASTER)

    def processSignals(messages: List[Signal]): (Configuration, List[Signal]) = {

        println("-------------------------------------")
        if (DEBUG) {
            println()
            println("Modules: ")
            modules.foreach(println)
            println("Messages: ")
        }
        messages.foreach(println)

        val (newModMap, outputs) = messages.foldLeft((Map[String, Module](), List[Signal]())) { (acc, next) =>

            val (accModules, accMessages) = acc

            val target = next.target
            val (newModule, outputMessages) = modules(target).recieve(List(next))

            (accModules + (newModule.getName -> newModule), accMessages ++ outputMessages)
        }

        if (DEBUG) {
            println("New modules: ")
            newModMap.values.foreach(println)
            println("New messages: ")
            outputs.foreach(println)
        }

        val newModuleNames = newModMap.keys.toList

        (Configuration(modules.filterNot(newModuleNames.contains(_)) ++ newModMap), outputs)

    }


    @tailrec
    final def run(inputs: List[Signal], msgCount: MsgCount): (Configuration, MsgCount) = {
        if (inputs.isEmpty) {
            (this, msgCount)
        } else {
            val (newConfig, newInputs) = processSignals(inputs)
            val newMsgCount = MsgCount(newInputs)
            newConfig.run(newInputs, newMsgCount + msgCount)
        }
    }

}



def parseFile(all: List[String]): Configuration = {

    case class Mapping(mType: String, name: String, outputs: List[String])

    val lines = all.filterNot(l => l.isEmpty || l.head == '#')

    val MODULE_REGEX = """([b|%|&]\w+) -> (.*)""".r

    val mappings = lines.map { line => 
        line match {
            case MODULE_REGEX(name, outputs) => {
                val outputsList = outputs.split(", ").toList
                if (name.head == 'b') {
                    Mapping(Module.BROADCASTER, name, outputsList)
                } else {
                    Mapping(name.head.toString, name.tail, outputsList)
                }
            }
        }
    }

    val moduleMap = mappings.map { m => 

        val inputNames = mappings.filter(_.outputs.contains(m.name)).map(_.name)
        val inIOStates = inputNames.map { input => IOState(input, Low) }
        val outIOStates = m.outputs.map { output => IOState(output, Low) }

        val module = m.mType match {
            case Module.BROADCASTER => Broadcaster(m.outputs.map(IOState(_, Low)))
            case "%" => FlipFlop(m.name, inIOStates, outIOStates, Low)
            case "&" => Conjunction(m.name, inIOStates, outIOStates)
        }
        (module.getName -> module)
    }.toMap

    val missingModules = mappings.map(_.outputs).flatten.toSet -- moduleMap.keys
    println("Missing modules: " + missingModules.mkString(", "))

    val outputModules = missingModules.map(name => {
        val inputNames = mappings.filter(_.outputs.contains(name)).map(_.name)
        val inputIOs = inputNames.map { input => IOState(input, Low) }
        (name -> Output(name, inputIOs))
    }).toMap

    Configuration(moduleMap ++ outputModules)

}
    



def part1(config: Configuration): Unit = {

    println("Part 1")

    val LOOP = 4
    val total = (1 to LOOP).foldLeft((config,MsgCount(0,0))) { (acc, next) =>
        val (config, msgCount) = acc
        val (newConfig, newCount) = Button.press(config)
        println(s"Press $next: $newCount")
        println()
        (newConfig, msgCount + newCount)
    }
    println("Message count: " + total._2)
    println("Message product: " + total._2.product)

}

def part2(config: Configuration): Unit = {

    println("\nPart 2")


}

val startTime = java.time.LocalTime.now

println("Start time: " + startTime)

val config = parseFile(fileInput)
println("Starting config:")
println(config)
println()

part1(config)

// part2(config)


val endTime = java.time.LocalTime.now
println("End time: " + endTime)
println("Elapsed time: " + java.time.Duration.between(startTime, endTime))


// Part 1: Not 803176238 - too low