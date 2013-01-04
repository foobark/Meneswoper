package de.fhkoeln.minesweeper.view

import de.fhkoeln.minesweeper.model.MineField
import de.fhkoeln.minesweeper.model.MineFieldGrid
import scala.io.Source._
import swing._

class TUI {

  var grid = MineFieldGrid(1, 1, 0)
  var firstMove = true

  def processInputLine(input: String) = {
    var continue = true
    var endgame = (grid.getGrid(), false, false)
    input match {
      case "q" => continue = false
      case "n" => {
        println("new game starts now")
        grid = MineFieldGrid(8, 8, 10) //chooseGrid
        firstMove = true
        println(grid.toString)
      }
      case _ => {
        input.toList.filter(c => c != ' ').map(c => c.toString.toInt) match {
          case row :: column :: value :: Nil => {
            if (firstMove) println("start to uncover first")
            else if (grid.getGrid()(row)(column).marked) {
              if (!endgame._1(row)(column).uncovered) {
                grid.unmarkField((row, column))
                println("Field " + "(" + row + "," + column + ") got unmarked")
              }
            } else {
              if (!endgame._1(row)(column).uncovered) {
                println("Field " + "(" + row + "," + column + ") got marked")
                grid.markField((row, column))
              }
            }
            println(grid.toString)
          }

          case row :: column :: Nil => {
            if (firstMove) {
              firstMove = false
              grid = MineFieldGrid(8, 8, 10, (row, column))
            }
            if (!endgame._1(row)(column).uncovered) {
              endgame = grid.uncoverField((row, column))
              println("Field " + "(" + row + "," + column + ") got uncovered")
              println(grid.toString)
              if (endgame._2) println("you're dead!")
              else if (endgame._3) println("you've won the Game")
            }
          }
          case _ => println("False Input!!!")
        }
      }
    }

    continue
  }

  def chooseGrid {

  }

}