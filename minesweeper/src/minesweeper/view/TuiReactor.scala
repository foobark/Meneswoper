/**
 *
 */
package minesweeper.view

import minesweeper.controller._

/**
 * @author ti614
 *
 */
class TuiReactor( controller: StandardGridController ) extends GridReactor {

    listenTo( controller )

    private var difficultySelect = false

    private val difficulties = "e" :: "m" :: "h" :: Nil

    private var gameEnd = false

    reactions += (
        { case e: GridEvent if e.isInstanceOf[GameLost] || e.isInstanceOf[GameWon] => { gameEnd = true; println( generateOutput( e ) ) } },
        { case e: GridEvent => println( generateOutput( e ) ) } )

    def processInputLine( input: String ): Boolean = {

        !gameEnd &&
            (
                input.filter( c => c != ' ' ).split( ',' ).toList match {

                    case row :: column :: cmd :: Nil => {
                        var ro = row.toInt
                        var col = column.toInt
                        if ( cmd == "m" ) controller.markPosition( ro, col )
                        if ( cmd == "u" ) controller.unmarkPosition( ro, col )
                        true
                    }

                    case row :: column :: Nil => {
                        var ro = row.toInt
                        var col = column.toInt
                        controller.uncoverPosition( ro, col )
                        true
                    }

                    case "q" :: Nil => {
                        println( generateOutput( 'q' ) )
                        false
                    }

                    case "n" :: Nil => {
                        println( generateOutput( 'n' ) )
                        difficultySelect = true
                        true
                    }

                    case diff :: Nil if difficultySelect && difficulties.contains( diff ) => {
                        val op = diff.charAt( 0 )
                        difficultySelect = false
                        controller.startNewGame( chooseDifficulty( op ) )
                        println( generateOutput( op ) )
                        true
                    }

                    case Nil => true

                    case _ => {
                        println( generateOutput( '\0' ) )
                        true
                    }
                } )
    }

    def generateOutput( cmd: Char ): String = {
        cmd match {
            case 'q' => "Game quit"
            case 'n' => """Select a difficulty:
            				easy   -> e
            				medium -> m
            				hard   -> h
    		        		""".stripMargin
            case 'e' if difficultySelect => "Difficulty easy selected\n\n"
            case 'm' if difficultySelect => "Difficulty medium selected\n\n"
            case 'h' if difficultySelect => "Difficulty hard selected\n\n"
            case _                       => "Invalid input\n\n"
        }
    }

    def generateOutput( event: GridEvent ): String = {
        val msg: String = event match {
            case fu: FieldUncovered  => "Field " ++ fu.field.toString ++ " got uncovered"
            case fm: FieldMarked     => "Field " ++ fm.field.toString ++ " got marked"
            case fum: FieldUnmarked  => "Field " ++ fum.field.toString ++ " got unmarked"
            case gw: GameWon         => "You cleared the Minefield"
            case gl: GameLost        => "You stepped on a Mine!"
            case ngs: NewGameStarted => "Starting new game"
        }
        msg concat ( "\n\n" ) concat ( ( event.grid ).map( _.mkString( " " ) ).mkString( "\n" ) )
    }

    def chooseDifficulty( difficulty: Char ): GridDifficulty = {
        difficulty match {
            case 'e' => Difficulties.easy
            case 'm' => Difficulties.medium
            case 'd' => Difficulties.difficult
            case _   => Difficulties.easy
        }
    }
}