package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

class Mkdir(name: String) extends CreateEntry(name) {

  def doSpecificCreateEntry(state: State): DirEntry =
    Directory.empty(state.wd.path, name)

}
