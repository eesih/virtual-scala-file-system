package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{Directory, File}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Echo(args: Array[String]) extends Command {

   def apply(state: State): State = {

     if (args.isEmpty) state
     else if (args.length == 1) state.setMessage(args(0))
     else {
       val operator = args(args.length - 2)
       val fileName = args(args.length -1)
       val contents = createContent(args, args.length - 2)

       if (">>".equals(operator))
         doEcho(state, contents, fileName, append = true)
       else if (">".equals(operator))
         doEcho(state, contents, fileName, append= false)
       else
         state.setMessage(createContent(args, args.length))
     }
   }

  def getRootAfterEcho(curDir: Directory, path: List[String], contents: String, append: Boolean): Directory = {
    if (path.isEmpty) curDir
    else if (path.tail.isEmpty) {
      val dirEntry = curDir.findEntry(path.head)

      if (dirEntry == null) curDir.addEntry(new File(curDir.path, path.head, contents))
      else if (dirEntry.isDirectory) curDir
      else
       if (append) curDir.replaceEntry(path.head, dirEntry.asFile.appendContents(contents))
       else curDir.replaceEntry(path.head, dirEntry.asFile.setContents(contents))
    } else {
      val nextDir = curDir.findEntry(path.head).asDirectory
      val newNextDir = getRootAfterEcho(nextDir, path.tail, contents, append)

      if (newNextDir == nextDir) curDir
      else curDir.replaceEntry(path.head, newNextDir)

    }
  }

  def doEcho(state: State, contents: String, fileName: String, append: Boolean): State = {
    if (fileName.contains(Directory.SEPARATOR))
      state.setMessage("Echo filename must not contain separators")
    else {
      val newRoot: Directory = getRootAfterEcho(state.root, state.wd.getAllFoldersInPath :+ fileName, contents, append)
      if (newRoot == state.root)
        state.setMessage(s"$fileName : no such file")
      else
        State(newRoot, newRoot.findDescendant(state.wd.getAllFoldersInPath))
    }
  }

  def createContent(args: Array[String], topIndex: Int): String = {
    @tailrec
    def createContentHelper(currentIndex: Int, acc: String): String = {
      if (currentIndex >= topIndex) acc
      else createContentHelper(currentIndex + 1 , acc + " " + args(currentIndex))
    }
    createContentHelper(0, "")
  }
}
