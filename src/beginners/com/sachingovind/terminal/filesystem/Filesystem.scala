package beginners.com.sachingovind.terminal.filesystem

import java.util.Scanner

import beginners.com.sachingovind.terminal.commands.Command
import beginners.com.sachingovind.terminal.files.Directory

object Filesystem extends App {

  val root = Directory.ROOT
  var state = State(root,root)

  val scanner = new Scanner(System.in)
  while (true) {
    state.show
    val input = scanner.nextLine
    state = Command.from(input).apply(state)
  }


}
