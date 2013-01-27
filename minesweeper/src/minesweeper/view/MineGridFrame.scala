package minesweeper.view

import scala.swing.MainFrame
import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.event.Event
import minesweeper.controller.GridState

class MineGridFrame extends MainFrame {
    private var menu = new MinesweeperMenu()
    private var grid = new MineGridGUI()
    listenTo( menu )
    listenTo( grid )
    deafTo(this)
    reactions += { case neg: EasyGameStarted => publish( neg ) }
    reactions += { case nmg: MediumGameStarted => publish( nmg ) }
    reactions += { case hgs: HardGameStarted => publish( hgs ) }
    reactions += { case gq: GameQuit => publish( gq ) }
    reactions += { case uncovered: CoveredFieldClicked => publish( uncovered )}
    reactions += { case marked: CoveredFieldRightClicked => publish( marked )}
    reactions += {case gu: GridUpdated => grid.updateGrid(gu.grid); pack()}

    contents = new BoxPanel( Orientation.Vertical ) {
        contents += ( menu,
            grid )
    }

}

class GridUpdated(val grid: GridState) extends Event