package de.fhkoeln.minesweeper.view

import swing._
import de.fhkoeln.minesweeper.controller._
import swing.event._
import GridBagPanel._
import java.awt.Insets

class GuiReactor(controller: GridController) extends GridReactor(controller) {

  private var menuPanel=new BoxPanel(Orientation.Horizontal) {
        contents += gameButtons.newGameButtonEasy
        contents += gameButtons.newGameButtonMedium
        contents += gameButtons.newGameButtonDifficult
        contents += gameButtons.quitButton
      }
  
  private var diff = (Difficulties.easy,Difficulties.medium,Difficulties.difficult)
  val mainWindow = new MainFrame {

    contents = new BoxPanel(Orientation.Vertical) {
      contents += menuPanel
      contents += new GridBagPanel{
        val c = new Constraints
        for(y <- 0 until diff._3._1; x <- 0 until diff._3._2) {
          var minefield = new MineFieldButton("",10,(y,x))
          c.fill= Fill.Horizontal
          c.gridx=minefield.pos._2
          c.gridy=minefield.pos._1
          c.gridheight=minefield.fieldSize
          c.gridwidth=minefield.fieldSize
          layout(minefield)=c
        }
      }

    }

  }

  draw

  def react(e: GridEvent) {

  }

  private def draw {
    mainWindow.title = "Mineswoper"
    mainWindow.size = new Dimension(800, 600)
    mainWindow.visible = true
    mainWindow.resizable = true
  }
  
  

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

private class MineFieldButton (text: String ="", val fieldSize: Int, val pos: (Int, Int)) extends Button(text) {
 maximumSize=new Dimension(fieldSize,fieldSize)
 minimumSize=new Dimension(fieldSize,fieldSize)
}