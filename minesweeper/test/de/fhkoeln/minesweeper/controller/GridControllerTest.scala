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
            reactor.gameWon must beFalse
            reactor.gridReceived(0) forall(_.covered) must beTrue
        }
        
        "Not uncover a mine the first time and tell me i won the game" in {
            controller.startNewGame(diff)
            reactor.gridUpdate = false
            controller.uncoverPosition(0, 0)
            reactor.gridUpdate must beFalse
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
        
        "Uncover a mine the 2nd time" in {
            controller.startNewGame(diff)
            controller.uncoverPosition(0,0)
            controller.uncoverPosition(0,1)
            reactor.gameLost must beTrue
            reactor.gridReceived(0)(0).uncovered must beTrue
            reactor.gridReceived(0)(1).triggered must beTrue
        }
    }
    
    "A GridController controlling a 10 * 10 field with 98 mines" should {
        val controller= new GridController()
        val reactor = new MockReactor(controller)
        val diff = (10, 10, 98)
        
        "Trigger a GridUpdate" in {  
            controller.startNewGame(diff)
            controller.uncoverPosition(0,0)
            reactor.gridUpdate must beTrue
        }
        
        "Trigger a GridUpdate and mark the field" in {
            controller.startNewGame(diff)
            controller.uncoverPosition(0,0)
            reactor.gridUpdate = false
            controller.markPosition(5,5)
            reactor.gridUpdate must beTrue
            reactor.gameWon must beFalse
            reactor.gameLost must beFalse
            reactor.gridReceived(5)(5).marked must beTrue
            reactor.gridReceived.flatten.count(_.uncovered) must be_==(1)
            reactor.gridReceived.flatten.count(_.covered) must be_== (98)
        }
        
        "Trigger a GridUpdate and unmark the field" in {
            controller.startNewGame(diff)
            controller.uncoverPosition(0,0)
            reactor.gridUpdate = false
            controller.markPosition(5,5)
            reactor.gridUpdate = false
            controller.unmarkPosition(5, 5)
            reactor.gridUpdate must beTrue
            reactor.gameWon must beFalse
            reactor.gameLost must beFalse
            reactor.gridReceived(5)(5).marked must beFalse
            reactor.gridReceived.flatten.count(_.uncovered) must be_==(1)
            reactor.gridReceived.flatten.count(_.covered) must be_== (99)
        }
    }
}