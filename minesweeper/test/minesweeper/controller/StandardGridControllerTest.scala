package minesweeper.controller

import org.specs2.mutable._
import org.specs2.specification.Scope
import org.specs2.mock._
import scala.math.sqrt
import scala.math.pow

class StandardGridControllerTest extends SpecificationWithJUnit with Mockito {

    "A GridController controlling a 3 * 3 field with 1 mine" should {

        "Create a new 3 * 3 field with all fields covered" in new NineFields() {
            there was one(reactor).react(any[NewGameStarted])
            there was no(reactor).react(any[FieldUncovered])
            there was no(reactor).react(any[FieldMarked])
            there was no(reactor).react(any[GameLost])
            there was no(reactor).react(any[GameWon])
            gridReceived.flatten forall ( _.covered ) must beTrue
        }

        "Not uncover a mine the first time" in new NineFields() {
            controller.uncoverPosition( 2, 2 )
            there was no(reactor).react(any[GameLost])
            gridReceived( 0 )( 0 ).uncovered must beTrue
        }

    }

    "A GridController controlling a 5 * 5 field with 16 mines" should {

        "Trigger a GridUpdate" in new TwentyFiveFirstOpen() {
            there was one(reactor).react(any[FieldUncovered])
        }

        "Trigger a GridUpdate and mark the field" in new TwentyFiveMarked() {
            there was one(reactor).react(any[FieldUncovered])
            there was one(reactor).react(any[FieldMarked])
            gridReceived( 4 )( 4 ).marked must beTrue
            gridReceived( 0 )( 0 ).uncovered must beTrue
        }

        "Trigger a GridUpdate and unmark the field" in new TwentyFiveMarked() {
            controller.unmarkPosition( 4, 4 )
            there was one(reactor).react(any[FieldUnmarked])
            there was no(reactor).react(any[GameWon])
            there was no(reactor).react(any[GameLost])
            gridReceived( 4 )( 4 ).marked must beFalse
        }

        "Have no mines in the 8 positions around the first field and uncover them" in new TwentyFiveFields() {
            def distance( x1: Int, y1: Int, x2: Int, y2: Int ): Int = sqrt( pow( x2 - x1, 2 ) + pow( y2 - y1, 2 ) ).round.intValue
            controller.uncoverPosition( 2, 2 )
            gridReceived.flatten count ( _.uncovered ) must be_==( 9 )
            gridReceived.flatten count ( _.covered ) must be_==( 16 )
            there was one(reactor).react(any[GameWon])
            there was no(reactor).react(any[GameLost])
        }
    }

    trait ControllerAndReactor extends Scope {
        val controller = new StandardGridController()
        val reactor =  mock[GridReactor]
        var gridReceived: GridState = null
        reactor.react(any) answers { _ match {case g: GridEvent => gridReceived = g.grid}}	
        controller.register(reactor)
    }

    trait NineFields extends ControllerAndReactor {
        val diff = ( 3, 3, 1 )
        controller.startNewGame( diff )
    }

    trait TwentyFiveFields extends ControllerAndReactor {
        val diff = ( 5, 5, 16 )
        controller.startNewGame( diff )
    }

    trait TwentyFiveFirstOpen extends TwentyFiveFields {
        controller.uncoverPosition( 0, 0 )
    }
    
    trait TwentyFiveMarked extends TwentyFiveFirstOpen {
        controller.markPosition(4, 4)
    }
}