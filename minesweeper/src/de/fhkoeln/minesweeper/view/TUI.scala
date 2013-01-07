package de.fhkoeln.minesweeper.view

import de.fhkoeln.minesweeper.model.Field
import de.fhkoeln.minesweeper.model.MineFieldGrid
import scala.io.Source._
import swing._

class TUI {

  var grid = MineFieldGrid(1, 1, 0)

  var firstMove = true

  var size = (14, 14, 10)

  def processInputLine(input: String) = {

    var continue = true

    var gamestate = grid.getGridState()

    var win = false

    var loss = false

    input match {

      case "q" => continue = false

      case "n" =>
        {
          newGame
        }
        
          case _ if (!win && !loss) => {

            input.filter(c => c != ' ').split(',').toList match {

              case row :: column :: value :: Nil => {
                var ro = row.toInt
                var col = column.toInt

                val field = gamestate(ro)(col)

                if (firstMove) println("start to uncover first")

                else if (!field.uncovered) {
                  var op = ""
                  field.marked match {
                    case true => {
                      grid.unmarkField((ro, col))
                      op = "unmarked"
                    }
                    case false =>
                      {
                        grid.markField((ro, col))
                        op = "marked"
                      }
                      println("Field " + "(" + ro + "," + col + ") got " + op)
                  }
                }
                println(grid.toString)
              }

              case row :: column :: Nil => {

                var ro = row.toInt
                var col = column.toInt

                val field = gamestate(ro)(col)

                if (firstMove) {
                  firstMove = false
                  grid = MineFieldGrid(size._1, size._2, size._3, (ro, col))
                }

                if (!field.uncovered) {

                  val newgrid = grid.uncoverField((ro, col))

                  gamestate = newgrid._1
                  loss = newgrid._2
                  win = newgrid._3

                  println("Field " + "(" + ro + "," + col + ") got uncovered")
                  println(grid.toString)

                  if (loss) {
                    println("you're dead!")
                    println("press n for new game, press q to quit")
                  }
                  else if (win) {
                    println("you've won the Game")
                    println("press n for new game, press q to quit")
                  }
                  
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

  private def newGame = {
    println("new game starts now")
    grid = MineFieldGrid(size._1, size._2, size._3) //chooseGrid
    firstMove = true
    println(grid.toString)
  }

}