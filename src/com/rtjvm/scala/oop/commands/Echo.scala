package com.rtjvm.scala.oop.commands

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

  def doEcho(state: State, contents: String, fileName: String, append: Boolean): State = ???

  def createContent(args: Array[String], topIndex: Int): String = {
    @tailrec
    def createContentHelper(currentIndex: Int, acc: String): String = {
      if (currentIndex >= topIndex) acc
      else createContentHelper(currentIndex + 1 , acc + " " + args(currentIndex))
    }
    createContentHelper(0, "")
  }
}
