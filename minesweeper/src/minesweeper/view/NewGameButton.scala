package minesweeper.view

import scala.swing.Button

abstract class NewGameButton extends Button

class EasyGameButton extends NewGameButton { text = "NewGame - easy" }

class MediumGameButton extends NewGameButton { text = "NewGame - medium" }

class HardGameButton extends NewGameButton { text = "NewGame - difficult" }