package com.rtjvm.scala.oop.files

class File(override val parentPath: String, override val name: String, contents: String) extends DirEntry(parentPath, name) {

   def asDirectory: Directory = throw new FileSystemException("File can't be converted to a directory")

   def getType: String = "File"

   def asFile: File = this

   def isDirectory: Boolean = false

   def isFile: Boolean = true
}


object File {
  def empty(parentPath: String, name: String) : File = new File(parentPath, name,"")
}