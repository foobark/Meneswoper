package minesweeper.controller

trait GridController {
    
    def register(reactor: GridReactor)
    
    def unregister(reactor: GridReactor)
    
    def startNewGame(diff: GridDifficulty)
    
    def uncoverPosition(y: Int, x: Int)
    
    def markPosition(y: Int, x: Int)
    
    def unmarkPosition(y: Int, x: Int)
    
	protected def update( event: GridEvent )
}