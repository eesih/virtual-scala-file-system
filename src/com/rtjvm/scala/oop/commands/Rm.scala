package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.Directory
import com.rtjvm.scala.oop.filesystem.State

class Rm(name: String) extends Command {
   def apply(state: State): State = {
     val wd = state.wd
     val absolutePath =
       if (name.startsWith(Directory.SEPARATOR)) name
       else if (wd.isRoot) wd.path + name
       else wd.path + Directory.SEPARATOR + name

     if(Directory.ROOT_PATH.equals(absolutePath))
        state.setMessage("There is no file or folder to remove")
     else
       doRm(state, absolutePath)
   }

  def doRm(state: State, path: String): State = {

    def rmHelper(curDir: Directory, path: List[String]): Directory = {
      if (path.isEmpty) curDir
      else if (path.tail.isEmpty) curDir.removeEntry(path.head)
      else {
        val nextDir = curDir.findEntry(path.head)
        if (!nextDir.isDirectory) curDir
        else {
          val newNextDir = rmHelper(nextDir.asDirectory, path.tail)
          if (newNextDir == nextDir) curDir
          else curDir.replaceEntry(path.head, newNextDir)
        }
      }
    }

    val tokens = path.substring(1).split(Directory.SEPARATOR).toList
    val newRoot: Directory = rmHelper(state.root, tokens)

    if (newRoot == state.root)
      state.setMessage(s"$path no such file or directory")
    else
      State(newRoot, newRoot.findDescendant(state.wd.path.substring(1)))
  }
}
