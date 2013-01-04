package de.fhkoeln.minesweeper.view

import de.fhkoeln.minesweeper.model.MineField
import de.fhkoeln.minesweeper.model.MineFieldGrid
import scala.io.Source._
import swing._

class TUI {

    var grid = MineFieldGrid( 1, 1, 0 )

    var firstMove = true

    def processInputLine( input: String ) = {

        var continue = true

        var gamestate = grid.getGrid()

        var win = false

        var loss = false

        input match {

            case "q" => continue = false

            case "n" => {
                newGame
            }

            case _ => {

                input.toList.filter( c => c != ' ' ).map( c => c.toString.toInt ) match {

                    case row :: column :: value :: Nil => {

                        val field = gamestate( row )( column )

                        if ( firstMove ) println( "start to uncover first" )

                        else if ( !field.uncovered ) {

                            if ( field.marked ) {
                                grid.unmarkField( ( row, column ) )
                                println( "Field " + "(" + row + "," + column + ") got unmarked" )
                                
                            } else if ( !field.uncovered ) {
                                println( "Field " + "(" + row + "," + column + ") got marked" )
                                grid.markField( ( row, column ) )
                            }
                        }
                        println( grid.toString )
                    }

                    case row :: column :: Nil => {

                        val field = gamestate( row )( column )

                        if ( firstMove ) {
                            firstMove = false
                            grid = MineFieldGrid( 8, 8, 10, ( row, column ) )
                        }

                        if ( !field.uncovered ) {

                            val newgrid = grid.uncoverField( ( row, column ) )

                            gamestate = newgrid._1
                            loss = newgrid._2
                            win = newgrid._3

                            println( "Field " + "(" + row + "," + column + ") got uncovered" )
                            println( grid.toString )

                            if ( loss ) println( "you're dead!" )
                            else if ( win ) println( "you've won the Game" )
                        }
                    }

                    case _ => println( "False Input!!!" )
                }
            }
        }

        continue
    }

    def chooseGrid {

    }

    private def newGame: Unit = {
        println( "new game starts now" )
        grid = MineFieldGrid( 8, 8, 10 ) //chooseGrid
        firstMove = true
        println( grid.toString )
    }

}