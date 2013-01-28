package minesweeper.view

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope
import minesweeper.controller.StandardGridController
import minesweeper.controller.GameLost
import minesweeper.controller.GameWon
import minesweeper.controller.FieldUncovered
import minesweeper.controller.FieldMarked
import minesweeper.controller.FieldUnmarked
import minesweeper.model.MineFieldState

class TUIReactorTest extends SpecificationWithJUnit {

    "A TUIReactor" should {

        "terminate processing" in new ReactorAndController {

            "on quit and generate a message" in {
                tui.processInputLine( "q" ) must beFalse
                tui.generateOutput( 'q' ) must be_!=( null )
            }

            "on lost game and generate a message" in {
                val event = new GameLost( ( MineFieldState.triggered() :: Nil ) :: Nil )
                tui.react( event )
                tui.processInputLine( "1,2" ) must beFalse
            }

            "on won game and generate a message" in {
                val event = new GameWon( ( MineFieldState.uncovered() :: Nil ) :: Nil )
                tui.react( event )
                tui.processInputLine( "1,2" ) must beFalse
                tui.generateOutput( event ) must be_!=( null )
            }
        }

        "Continue processing" in new ReactorAndController {

            "on invalid input and generate an error  message" in {
                tui.processInputLine( "$§%" ) must beTrue
                val msg = "Invalid input"
                tui.generateOutput( '\0' ) contains ( msg ) must beTrue
            }

            "on starting new game and ask for difficulty" in {
                tui.processInputLine( "n" ) must beTrue
                tui.generateOutput( 'n' ) contains ( "difficulty" ) must beTrue
            }

            "and generate a message" in new GameStarted {
                
                "when uncovering a field" in {
                    tui.processInputLine( "1,1" ) must beTrue
                    val event = new FieldUncovered( ( MineFieldState.uncovered() :: Nil ) :: Nil, ( 0, 0 ) )
                    tui.generateOutput( event ) contains ( "uncover" ) must beTrue
                }

                "when marking a field" in {
                    tui.processInputLine( "1,1,m" ) must beTrue
                    val event = new FieldMarked( ( MineFieldState.marked() :: Nil ) :: Nil, ( 0, 0 ) )
                    tui.generateOutput( event ) contains ( "mark" ) must beTrue
                }

                "when unmarking a field" in {
                    tui.processInputLine( "1,1,u" ) must beTrue
                    val event = new FieldUnmarked( ( MineFieldState.covered() :: Nil ) :: Nil, ( 0, 0 ) )
                    tui.generateOutput( event ) contains ( "unmark" ) must beTrue
                }
            }
        }

        "Only allow selecting difficulty after starting new game" in new ReactorAndController {
            tui.processInputLine( "e" ) must beTrue
            val err = "Invalid input"
            tui.generateOutput( 'e' ) contains ( err ) must beTrue
            tui.processInputLine( "n" )
            tui.generateOutput( 'm' ) contains ( err ) must beFalse
            tui.processInputLine( "h" ) must beTrue
            tui.generateOutput( 'h' ) contains ( err ) must beTrue
        }
    }
    
    trait ReactorAndController extends Scope {
        val controller = new StandardGridController()
        val tui = new TuiReactor( controller )
    }

    trait GameStarted extends ReactorAndController {
        tui.processInputLine( "n" )
        tui.processInputLine( "e" )
    }
}