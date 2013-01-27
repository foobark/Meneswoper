package minesweeper.view

import swing._
import minesweeper.controller._
import swing.event._
import GridBagPanel._
import java.awt.Insets

class GuiReactor( private var controller: StandardGridController ) extends GridReactor {

    listenTo( controller )

    private var gameEnd = false

    def gameOver: Boolean = gameEnd
    
    private object publisher extends Publisher

    reactions += (
        {
            case ge: GridEvent if !ge.isInstanceOf[GameLost] &&
                !ge.isInstanceOf[GameWon] &&
                !gameEnd =>
                publisher.publish( new GridUpdated( ge.grid ) )
        },
        {
            case ge: GridEvent if ( ge.isInstanceOf[GameLost] || ge.isInstanceOf[GameWon] )
                && !gameEnd =>
                publisher.publish( new GridUpdated( ge.grid ) ); gameEnd = true
        } )

    publisher.reactions += { case neg: EasyGameStarted => controller.startNewGame(Difficulties.easy) }
    publisher.reactions += { case nmg: MediumGameStarted => controller.startNewGame(Difficulties.medium) }
    publisher.reactions += { case hgs: HardGameStarted => controller.startNewGame( Difficulties.difficult ) }
    publisher.reactions += { case gq: GameQuit => gameEnd = true; mainWindow.close() }
    publisher.reactions += { case uncovered: CoveredFieldClicked => controller.uncoverPosition(uncovered.y, uncovered.x) }
    publisher.reactions += { case marked: CoveredFieldRightClicked => controller.markPosition(marked.y, marked.x) }
    
    val mainWindow = new MineGridFrame()
    mainWindow.listenTo( publisher )
    publisher.listenTo(mainWindow)
    
    mainWindow.title = "Mineswoper"
    mainWindow.visible = true
    mainWindow.resizable = true
}

