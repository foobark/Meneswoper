package de.fhkoeln.minesweeper.view

import swing._
import de.fhkoeln.minesweeper.controller._
import swing.event._
import GridBagPanel._
import java.awt.Insets

class GuiReactor(controller: GridController) extends GridReactor(controller) {

  private var menuPanel = new BoxPanel(Orientation.Horizontal) {
    contents += gameButtons.newGameButtonEasy
    contents += gameButtons.newGameButtonMedium
    contents += gameButtons.newGameButtonDifficult
    contents += gameButtons.quitButton
  }

  private var mineGrid = new GridBagPanel {
    val c = new Constraints
    for (y <- 0 until 8; x <- 0 until 8) {
      var minefield = new MineFieldButton("", (y, x))
      c.gridx = minefield.pos._2
      c.gridy = minefield.pos._1
      layout(minefield) = c
    }
  }

  private var diff = (Difficulties.easy, Difficulties.medium, Difficulties.difficult)

  val mainWindow = new MainFrame {

    contents = new BoxPanel(Orientation.Vertical) {
      contents += menuPanel
      contents += mineGrid
    }

  }

  draw

  def react(e: GridEvent) {
//    gameEnd = event match {
//      case gw: GameWon => true
//      case gl: GameLost => true
//      case _ => false
//    }
//    generateOutput(event)
  }

  private def draw {
    mainWindow.title = "Mineswoper"
    mainWindow.visible = true
    mainWindow.resizable = true
  }
  
//  private def generateOutput( event: GridEvent ): String = {
//        val msg: String = event match {
//            case fu: FieldUncovered  => event.grid.
//            case fm: FieldMarked     => "Field " ++ fm.field.toString ++ " got marked"
//            case fum: FieldUnmarked  => "Field " ++ fum.field.toString ++ " got unmarked"
//            case gw: GameWon         => "You cleared the Minefield"
//            case gl: GameLost        => "You stepped on a Mine!"
//            case ngs: NewGameStarted => ngs.field.
//        }
//        msg concat ( "\n\n" ) concat ( ( event.grid ).map( _.mkString( " " ) ).mkString( "\n" ) )
//    }

}

private object gameButtons {
  val newGameButtonEasy = new Button {
    text = "NewGame - easy"
  }
  val newGameButtonMedium = new Button {
    text = "NewGame - medium"
  }
  val newGameButtonDifficult = new Button {
    text = "NewGame - difficult"
  }
  val quitButton = new Button {
    text = "Quit"
  }
}

private class MineFieldButton(text: String = "", val pos: (Int, Int)) extends Button(text) {
  preferredSize = new Dimension(25, 25)
  minimumSize = new Dimension(10, 10)
}