package de.fhkoeln.minesweeper.controller

import org.specs.SpecificationWithJUnit

class GridControllerTest extends SpecificationWithJUnit {

    private class MockReactor( controller: GridController ) extends GridReactor( controller ) {

        var gridReceived: GridState = null

        var gridUpdate = false
        var gameWon = false
        var gameLost = false
        var newGame = false

        override def react( event: GridEvent ) = {
            event match {
                case ngs: NewGameStarted => {
                    newGame = true
                }

                case gu: GridUpdated => {
                    gridUpdate = true
                }

                case gw: GameWon => {
                    gameWon = true
                }

                case gl: GameLost => {
                    gameLost = true
                }
            }
            gridReceived = event.grid
        }
    }

    "A GridController controlling a 1 * 2 field with 1 mine" should {
        val controller = new GridController()
        val reactor = new MockReactor(controller)
        val diff = (1,2,1)
        
        "Create a new 1 * 2 field with all fields covered" in {
            controller.startNewGame(diff)
            reactor.newGame must beTrue
            reactor.gridUpdate must beFalse
            reactor.gameLost must beFalse
            reactor.gameWon must beTrue
            reactor.gridReceived(0) forall(_.covered) must beTrue
        }
        
        "Not uncover a mine the first time and tell me i won the game" in {
            controller.startNewGame(diff)
            controller.uncoverPosition(0, 0)
            reactor.gameLost must beFalse
            reactor.gameWon must beTrue
            reactor.gridReceived(0)(0).uncovered must beTrue
            reactor.gridReceived(0)(1).covered must beTrue
            
            controller.startNewGame(diff)
            controller.uncoverPosition(0, 1)
            reactor.gameLost must beFalse
            reactor.gameWon must beTrue
            reactor.gridReceived(0)(1).uncovered must beTrue
            reactor.gridReceived(0)(0).covered must beTrue
        }
    }
}