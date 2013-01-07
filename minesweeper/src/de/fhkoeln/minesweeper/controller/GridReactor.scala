package de.fhkoeln.minesweeper.controller

abstract class GridReactor() {
	
    def react(e: GridEvent)
    
    def listenTo(controller: GridController) = {
        controller.register(this)
    }
}