package minesweeper.view

import swing.Component
import swing.GridBagPanel
import swing.AbstractButton
import swing.event._
import javax.swing.SwingUtilities
import minesweeper.controller.GridState

class MineGridGUI extends GridBagPanel {

    def this( grid: GridState ) = {
        this()
        updateGrid( grid )
    }

    reactions +=
        {
            case uncover: MouseClicked => uncover.source match {
                case cfb: CoveredFieldButton if SwingUtilities.isLeftMouseButton( uncover.peer ) =>
                    publish( new CoveredFieldClicked( layout( cfb ).gridy, layout( cfb ).gridx ) )
                case cfb: CoveredFieldButton if SwingUtilities.isRightMouseButton( uncover.peer ) =>
                    publish( new CoveredFieldRightClicked( layout( cfb ).gridy, layout( cfb ).gridx ) )
                case _ =>
            }
        }

    def updateGrid( grid: GridState ) = {
        layout.clear
        for( i <- grid.indices; j <- grid(i).indices)
            layout += FieldButton(grid(i)(j)) -> new Constraints { gridy = i; gridx = j}
        contents.foreach( ( x: Component ) => listenTo( x.mouse.clicks ) )
    }
}

class CoveredFieldClicked( val y: Int, val x: Int ) extends Event

class CoveredFieldRightClicked( val y: Int, val x: Int ) extends Event