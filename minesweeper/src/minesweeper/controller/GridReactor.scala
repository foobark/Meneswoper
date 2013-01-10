package minesweeper.controller

abstract class GridReactor(protected var controller: GridController) {
    
    listenTo(controller)
    
    def react(e: GridEvent)
    
    def listenTo(contr: GridController) = {
        require(contr != null)
        contr.register(this)
        controller = contr
    }
}