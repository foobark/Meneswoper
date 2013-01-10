

import minesweeper.view._
import minesweeper.controller._
object Minesweeper {
  

  def main(args: Array[String]) {
  val controller = new GridController
  val tui = new TuiReactor(controller)
  val gui = new GuiReactor(controller)
  
    while (tui.processInputLine(readLine())) {}
  }

}