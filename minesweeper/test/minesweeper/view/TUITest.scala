package minesweeper.view

import org.specs.SpecificationWithJUnit
import minesweeper.controller.GridController
import minesweeper.controller.GameLost
import minesweeper.controller.GameWon
import minesweeper.controller.FieldUncovered
import minesweeper.controller.FieldMarked
import minesweeper.controller.FieldUnmarked
import minesweeper.model.MineFieldState

class TUIReactorTest extends SpecificationWithJUnit {

    "A TUIReactor" should {
        val controller = new GridController()
        val tui = new TuiReactor( controller )

        "terminate processing on quit and generate a message" in {
            tui.processInputLine( "q" ) must beFalse
            tui.generateOutput( 'q' ) must be_!=( null )
        }

        "terminate processing on lost game and generate a message" in {
            val event = new GameLost( ( MineFieldState.triggered() :: Nil ) :: Nil )
            tui.react( event )
            tui.processInputLine( "1,2" ) must beFalse
        }

        "terminate processing on won game and generate a message" in {
            val event = new GameWon( ( MineFieldState.uncovered() :: Nil ) :: Nil )
            tui.react( event )
            tui.processInputLine( "1,2" ) must beFalse
            tui.generateOutput( event ) must be_!=( null )
        }

        "Continue processing on invalid input and generate a proper message" in {
            tui.processInputLine( "$§%" ) must beTrue
            val msg = "Invalid input"
            tui.generateOutput( '\0' ) contains(msg) must beTrue
        }

        "Continue processing on starting new game and ask for difficulty" in {
            tui.processInputLine( "n" ) must beTrue
            tui.generateOutput( 'n' ) contains ( "difficulty" ) must beTrue
        }

        "Only allow selecting difficulty after starting new game" in {
            tui.processInputLine( "e" ) must beTrue
            val err = "Invalid input"
            tui.generateOutput( 'e' ) contains(err) must beTrue
            tui.processInputLine( "n" )
            tui.generateOutput( 'm' ) contains(err) must beFalse
            tui.processInputLine( "h" ) must beTrue
            tui.generateOutput( 'h' ) contains(err) must beTrue
        }

        "Continue processing and generate a message when uncovering a field" in {
            tui.processInputLine( "n" )
            tui.processInputLine( "e" )
            tui.processInputLine( "1,1" ) must beTrue
            val event = new FieldUncovered( ( MineFieldState.uncovered() :: Nil ) :: Nil, (0,0))
            tui.generateOutput(event) contains("uncover") must beTrue
        }
        
        "Continue processing and generate a message when marking a field" in {
            tui.processInputLine( "n" )
            tui.processInputLine( "e" )
            tui.processInputLine( "1,1,m" ) must beTrue
            val event = new FieldMarked( ( MineFieldState.marked() :: Nil ) :: Nil, (0,0))
            tui.generateOutput(event) contains("mark") must beTrue
        }
        
        "Continue processing and generate a message when unmarking a field" in {
            tui.processInputLine( "n" )
            tui.processInputLine( "e" )
            tui.processInputLine( "1,1,u" ) must beTrue
            val event = new FieldUnmarked( ( MineFieldState.covered() :: Nil ) :: Nil, (0,0))
            tui.generateOutput(event) contains("unmark") must beTrue
        }
    }
}