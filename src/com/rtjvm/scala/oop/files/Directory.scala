package com.rtjvm.scala.oop.files

import com.rtjvm.scala.oop.filesystem.FileSystemException

import scala.annotation.tailrec

class Directory(override val parentPath: String, override val name: String, val contents: List[DirEntry]) extends DirEntry(parentPath, name) {

  def hasEntry(name: String): Boolean =
    findEntry(name) != null

  def getAllFoldersInPath: List[String] =
    path.substring(1).split(Directory.SEPARATOR).toList.filter(x => !x.isEmpty)

  def findDescendant(path: List[String]): Directory =
    if (path.isEmpty) this
    else findEntry(path.head).asDirectory.findDescendant(path.tail)

  def findDescendant(relativePath: String): Directory =
    if (relativePath.isEmpty) this
    else findDescendant(relativePath.split(Directory.SEPARATOR).toList)

  def removeEntry(entryName: String): Directory =
    if (!hasEntry(entryName)) this
    else new Directory(parentPath, name, contents.filter(e => !e.name.equals(entryName)))

  def addEntry(newEntry: DirEntry): Directory =
    new Directory(parentPath, name, contents :+ newEntry)

  def findEntry(entryName: String): DirEntry = {
    @tailrec
    def findEntryHelper(entryName: String, contentList: List[DirEntry]): DirEntry =
      if(contentList.isEmpty) null
      else if (contentList.head.name.equals(entryName)) contentList.head
      else findEntryHelper(entryName, contentList.tail)

    findEntryHelper(entryName, contents)
  }

  def replaceEntry(entryName: String, newEntry: DirEntry): Directory =
    new Directory(parentPath, name, contents.filter(e => !e.name.equals(entryName)) :+ newEntry)

  def asDirectory: Directory = this

  def asFile: File = throw new FileSystemException("Directory can't be converted to a file!")

  def getType: String = "Directory"

  def isRoot: Boolean = parentPath.isEmpty

  def isDirectory: Boolean = true

  def isFile: Boolean = false

}

object Directory {
  val SEPARATOR = "/"
  val ROOT_PATH = "/"

  def ROOT: Directory = Directory.empty("", "")

  def empty(parentPath: String, name: String): Directory =
    new Directory(parentPath, name, List())
}
