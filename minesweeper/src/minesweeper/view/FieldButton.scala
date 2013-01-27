package minesweeper.view

import scala.swing.Button
import scala.swing.event.ButtonClicked
import scala.swing.event.MouseClicked
import scala.swing.Dimension
import scala.swing.Insets
import javax.swing.SwingUtilities
import minesweeper.controller.GridState
import minesweeper.controller.MineFieldState

abstract class FieldButton(text: String = "") extends Button(text) {
    preferredSize = new Dimension(20, 20)
    margin = new Insets(2, 2, 2, 2)
}

object FieldButton {
    def apply( state: MineFieldState ): FieldButton =
        state match {
            case covered: MineFieldState.covered     => new CoveredFieldButton()
            case marked: MineFieldState.marked       => new MarkedFieldButton()
            case triggered: MineFieldState.triggered => new TriggeredMineButton()
            case uncovered: MineFieldState.uncovered => new UncoveredFieldButton( uncovered.adjacent )
        }
}

class CoveredFieldButton extends FieldButton 

class UncoveredFieldButton( adjacents: Int = 0 ) extends FieldButton( adjacents.toString ) 

class TriggeredMineButton() extends FieldButton( "*" ) 

class MarkedFieldButton() extends FieldButton( "#" )