package beginners.com.sachingovind.terminal.commands
import beginners.com.sachingovind.terminal.filesystem.State

class UnknownCommand extends Command {
  override def apply(state: State): State = state.setMessage("Command not found!")
}
