package minesweeper.controller

import org.specs.SpecificationWithJUnit
import scala.Math.sqrt
import scala.Math.pow

class GridControllerTest extends SpecificationWithJUnit {

    private class MockReactor( controller: GridController ) extends GridReactor( controller ) {

        var gridReceived: GridState = null

        var fieldUncovered = false
        var fieldMarked = false
        var fieldUnmarked = false
        var gameWon = false
        var gameLost = false
        var newGame = false

        override def react( event: GridEvent ) = {
            event match {
                case ngs: NewGameStarted => newGame = true

                case fu: FieldUncovered  => fieldUncovered = true

                case fm: FieldMarked     => fieldMarked = true

                case fum: FieldUnmarked  => fieldUnmarked = true

                case gw: GameWon         => gameWon = true

                case gl: GameLost        => gameLost = true
            }
            gridReceived = event.grid
        }
    }

    "A GridController controlling a 3 * 3 field with 1 mine" should {
        val controller = new GridController()
        val reactor = new MockReactor( controller )
        val diff = ( 3, 3, 1 )

        "Create a new 3 * 3 field with all fields covered" in {
            controller.startNewGame( diff )
            reactor.newGame must beTrue
            reactor.fieldUncovered must beFalse
            reactor.fieldMarked must beFalse
            reactor.fieldUnmarked must beFalse
            reactor.gameLost must beFalse
            reactor.gameWon must beFalse
            reactor.gridReceived.flatten forall ( _.covered ) must beTrue
        }

        "Not uncover a mine the first time" in {
            controller.startNewGame( diff )
            controller.uncoverPosition( 0, 0 )
            reactor.gameLost must beFalse
            reactor.gridReceived( 0 )( 0 ).uncovered must beTrue

            controller.startNewGame( diff )
            controller.uncoverPosition( 2, 2 )
            reactor.gameLost must beFalse
            reactor.gridReceived( 2 )( 2 ).uncovered must beTrue
        }
    }

    "A GridController controlling a 5 * 5 field with 16 mines" should {
        val controller = new GridController()
        val reactor = new MockReactor( controller )
        val diff = ( 5, 5, 16 )

        "Trigger a GridUpdate" in {
            controller.startNewGame( diff )
            controller.uncoverPosition( 0, 0 )
            reactor.fieldUncovered must beTrue
        }

        "Trigger a GridUpdate and mark the field" in {
            controller.startNewGame( diff )
            controller.uncoverPosition( 0, 0 )
            controller.markPosition( 4, 4 )
            reactor.fieldUncovered must beTrue
            reactor.fieldMarked must beTrue
            reactor.gridReceived( 4 )( 4 ).marked must beTrue
            reactor.gridReceived( 0 )( 0 ).uncovered must beTrue
        }

        "Trigger a GridUpdate and unmark the field" in {
            controller.startNewGame( diff )
            controller.uncoverPosition( 0, 0 )
            controller.markPosition( 4, 4 )
            controller.unmarkPosition( 4, 4 )
            reactor.fieldUnmarked must beTrue
            reactor.gameWon must beFalse
            reactor.gameLost must beFalse
            reactor.gridReceived( 4 )( 4 ).marked must beFalse
        }

        "Have no mines in the 8 positions around the first field and uncover them" in {
            def distance( x1: Int, y1: Int, x2: Int, y2: Int ): Int = sqrt( pow( x2 - x1, 2 ) + pow( y2 - y1, 2 ) ).round.intValue
            controller.startNewGame( diff )
            controller.uncoverPosition( 2, 2 )
            reactor.gridReceived.flatten count ( _.uncovered ) must be_==( 9 )
            reactor.gridReceived.flatten count ( _.covered ) must be_==( 16 )
            reactor.gameWon must beTrue
            reactor.gameLost must beFalse
        }
    }
}