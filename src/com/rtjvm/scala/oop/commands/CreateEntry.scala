package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

abstract class CreateEntry(entryName: String) extends Command {

  override def apply(state: State): State = {
    val wd = state.wd
    if (wd.hasEntry(entryName)) {
      state.setMessage(s"Entry $entryName already exists!")
    } else if (entryName.contains(Directory.SEPARATOR)) {
      state.setMessage(s"$entryName must not contain separators!")
    } else if (checkIllegal(entryName)) {
      state.setMessage(s"$entryName illegal entry name!")
    } else {
      doCreateEntry(entryName, state)
    }
  }

  def checkIllegal(name: String): Boolean = {
    name.contains(".")
  }

  def doCreateEntry(name: String, state: State): State = {

    def updateStructure(currentDir: Directory, path: List[String], newEntry: DirEntry): Directory = {
      if (path.isEmpty) currentDir.addEntry(newEntry)
      else {
        val oldEntry = currentDir.findEntry(path.head).asDirectory
        currentDir.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
      }
    }

    val wd = state.wd
    val allDirsInPaths = wd.getAllFoldersInPath

    //val newDir = Directory.empty(wd.path, name)
    // TODO implement this
    val newEntry: DirEntry = doSpecificCreateEntry(state)

    val newRoot = updateStructure(state.root, allDirsInPaths, newEntry)
    val newWd = newRoot.findDescendant(allDirsInPaths)

    State(newRoot, newWd)
  }

  def doSpecificCreateEntry(state: State): DirEntry
}
