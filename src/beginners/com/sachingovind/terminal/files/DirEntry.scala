package beginners.com.sachingovind.terminal.files

abstract class DirEntry(val parentPath: String, val name: String) {

  def path: String = parentPath + Directory.SEPARATOR + name
  def asDirectory: Directory
}
