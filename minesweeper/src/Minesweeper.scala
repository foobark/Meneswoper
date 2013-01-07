

import de.fhkoeln.minesweeper.view._
import de.fhkoeln.minesweeper.controller._
object Minesweeper {
  

  def main(args: Array[String]) {
  val controller = new GridController
  val tui = new TuiReactor(controller)
  
    while (tui.processInputLine(readLine())) {}
  }

}