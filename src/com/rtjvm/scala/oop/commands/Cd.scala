package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Cd(dir: String) extends Command {


  override def apply(state: State): State = {
    val root = state.root
    val wd = state.wd

    val absolutePath =
      if (dir.startsWith(Directory.SEPARATOR)) dir
      else if (wd.isRoot) wd.path + dir
      else wd.path + Directory.SEPARATOR + dir

      val destinationDir = doFindEntry(root, absolutePath)

      if(destinationDir == null || !destinationDir.isDirectory)
          state.setMessage(s"$dir: no such directory!")
      else
        State(root, destinationDir.asDirectory)


  }


  def doFindEntry(root: Directory, path: String):DirEntry = {

    @tailrec
    def findEntryHelper(curDir: Directory, path: List[String]): DirEntry =
      if (path.isEmpty || path.head.isEmpty) curDir
      else if (path.tail.isEmpty) curDir.findEntry(path.head)
      else {
        val nextDir = curDir.findEntry(path.head)
        if (nextDir == null || !nextDir.isDirectory) null
        else findEntryHelper(nextDir.asDirectory, path.tail)
      }

      val tokens: List[String] = path.substring(1).split(Directory.SEPARATOR).toList

      findEntryHelper(root, tokens)
  }

}
