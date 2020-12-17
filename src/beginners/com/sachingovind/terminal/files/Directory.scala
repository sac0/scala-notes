package beginners.com.sachingovind.terminal.files

import scala.annotation.tailrec

class Directory(override val parentPath: String, override val name: String,
                val contents: List[DirEntry]) extends DirEntry(parentPath, name) {
  def replaceEntry(entryName: String, newEntry: Directory): Directory = {
    new Directory(parentPath, name, contents.filter(e => !e.name.equals(entryName)) :+ newEntry)
  }

  def findDescendant(path: List[String]): Directory = {
    if (path.isEmpty) this
    else findEntry(path.head).asDirectory.findDescendant(path.tail)
  }

  def hasEntry(name: String): Boolean = findEntry(name) != null

  def addEntry(newEntry: DirEntry): Directory = {
    new Directory(parentPath, name, contents :+ newEntry)
  }

  def findEntry(entryName: String): Directory = {
    @tailrec
    def findEntryHelper(name: String, contentList: List[DirEntry]): DirEntry =
      if (contentList.isEmpty) null
      else if (contentList.head.name.equals(name)) contentList.head
      else findEntryHelper(name, contentList.tail)

    findEntryHelper(entryName, contents).asDirectory
  }

  def getAllFoldersInPath: List[String] = {
    path.substring(1).split(Directory.SEPARATOR).toList.filter(!_.isEmpty)
    // /a/b/c/d -> [a,b,c,d]
  }

  override def asDirectory: Directory = this

}

object Directory {
  val SEPARATOR: String = "/"
  val ROOT_PATH: String = "/"

  def ROOT: Directory = Directory.empty("", "")

  def empty(parentPath: String, name: String): Directory = new Directory(parentPath, name, List[DirEntry]())

}
