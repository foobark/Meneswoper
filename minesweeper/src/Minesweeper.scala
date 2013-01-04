

import de.fhkoeln.minesweeper.view.TUI

object Minesweeper {
  
  val tui = new TUI
  
  def main(args: Array[String]) {

    while (tui.processInputLine(readLine())) {}
  }

}