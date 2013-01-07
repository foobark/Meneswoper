/**
 *
 */
package de.fhkoeln.minesweeper.view

import de.fhkoeln.minesweeper.controller._

/**
 * @author ti614
 *
 */
class TuiReactor(controller: GridController) extends GridReactor(controller) {

  def react(e: GridEvent): Unit = {
		  e match{
		    case ngs: NewGameStarted => println("New game has started")
		    case gu: GridUpdated => {}
		    case gw: GameWon => println("You've won the Game!")
		    case gl: GameLost => println("You've lost the Game!")		    
		    case ge: GridEvent => {}
		    case _ => {}
		  }
		  println(e.grid.map(_.mkString(" ")).mkString("\n"))
  }

  def processInputLine(input: String) = {
    var continue = true
    input.filter(c => c != ' ').split(',').toList match {
      case "q" :: Nil => continue = false
      case "n" :: Nil => {
        println("difficulty?")
        println("  -easy       e")
        println("  -medium     m")
        println("  -difficult  d")
        readLine() match {
          case "e" => controller.startNewGame(Difficulties.easy)
          case "m" => controller.startNewGame(Difficulties.medium)
          case "d" => controller.startNewGame(Difficulties.difficult)
        }
      }
      case "n" :: "e" :: Nil => controller.startNewGame(Difficulties.easy)
      case "n" :: "m" :: Nil => controller.startNewGame(Difficulties.medium)
      case "n" :: "d" :: Nil => controller.startNewGame(Difficulties.difficult)

        case row :: column :: value :: Nil => {
          var ro = row.toInt
          var col = column.toInt
          controller.markPosition(ro,col)
        }

        case row :: column :: Nil => {
          var ro = row.toInt
          var col = column.toInt
          controller.uncoverPosition(ro,col)
      }
        case Nil => {}
        case _ => println("false Input")

    }

    continue
  }
}