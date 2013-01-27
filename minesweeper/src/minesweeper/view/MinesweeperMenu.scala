package minesweeper.view

import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.event.ButtonClicked
import scala.swing.event.Event

class MinesweeperMenu extends BoxPanel( Orientation.Horizontal ) {
    contents += (
        new EasyGameButton(),
        new MediumGameButton(),
        new HardGameButton(),
        new QuitButton() )

    contents foreach ( listenTo( _ ) )
    reactions += 
        {
            case bc: ButtonClicked =>
                bc.source match {
                    case easy: EasyGameButton     => publish( new EasyGameStarted() )
                    case medium: MediumGameButton => publish( new MediumGameStarted() )
                    case hard: HardGameButton     => publish( new HardGameStarted() )
                    case quit: QuitButton => publish( new GameQuit() )
                }
        }
}

class EasyGameStarted extends Event

class MediumGameStarted extends Event

class HardGameStarted extends Event

class GameQuit extends Event