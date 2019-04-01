package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.Directory
import com.rtjvm.scala.oop.filesystem.State

class Mkdir(name: String) extends Command {

  override def apply(state: State): State = {
    val wd = state.wd
    if (wd.hasEntry(name)) {
      state.setMessage(s"Entry $name already exists!")
    } else if (name.contains(Directory.SEPARATOR)) {
      state.setMessage(s"$name must not contain separators!")
    } else if (checkIllegal(name)) {
      state.setMessage(s"$name illegal entry name!")
    } else {
      doMkdir(name, state)
    }
  }

  def checkIllegal(name: String): Boolean = {
    name.contains(".")
  }

  def doMkdir(name: String, state: State): State = {

    def updateStructure(currentDir: Directory, path: List[String], newEntry: Directory): Directory = {
      if (path.isEmpty) currentDir.addEntry(newEntry)
      else {
        val oldEntry = currentDir.findEntry(path.head).asDirectory
        currentDir.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
      }
    }

    val wd = state.wd
    val allDirsInPaths = wd.getAllFoldersInPath
    val newDir = Directory.empty(wd.path, name)
    val newRoot = updateStructure(state.root, allDirsInPaths, newDir)
    val newWd = newRoot.findDescendant(allDirsInPaths)

    State(newRoot, newWd)
  }

}
