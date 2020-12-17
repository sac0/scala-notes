package beginners.com.sachingovind.terminal.commands

import beginners.com.sachingovind.terminal.files.{DirEntry, Directory}
import beginners.com.sachingovind.terminal.filesystem.State

class Mkdir(name: String) extends Command {
  override def apply(state: State): State = {
    val wd = state.wd
    if (wd.hasEntry(name)) {
      state.setMessage("Entry " + name + " already exists!")
    } else if (name.contains(Directory.SEPARATOR)) {
      // mkdir -p sdf/asdf/asdf
      state.setMessage(name + " must not contain separators")
    } else if (checkIllegal(name)) {
      state.setMessage(name + ": illegal entry name")
    } else {
      doMkdir(state, name)
    }
  }

  def checkIllegal(name: String): Boolean = {
    name.contains(".")
  }

  def doMkdir(state: State, name: String): State = {
    val wd = state.wd

    def findDescendant(path: List[String]):Directory = ???

    def updateStructure(currentDirectory: Directory, path: List[String], newEntry: DirEntry): Directory = {
      /*
      * SomeDir -> a,b, new d
      * new Somedir -> have a and b similar and new d
      * the some dir is updated at a higher path in the same way
      *
      *
      * */
      if(path.isEmpty) currentDirectory.addEntry(newEntry)
      else {
        val oldEntry = currentDirectory.findEntry(path.head).asDirectory
        currentDirectory.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
      }
    }


    // all the directories in the full path
    val allDirsInPath = wd.getAllFoldersInPath
    // create an empty directory in the path
    val newDir = Directory.empty(wd.path, name)
    // update the directory structure started from the root
    // directory structure is Immutable
    val newRoot = updateStructure(state.root, allDirsInPath, newDir)
    // new working directory instance given wd full path in the new directory
    val newWd = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWd)
  }
}
