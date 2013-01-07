package de.fhkoeln.minesweeper.controller

abstract class GridEvent(val grid: GridState) 

class NewGameStarted(override val grid: GridState) extends GridEvent(grid)

class GridUpdated(override val grid: GridState) extends GridEvent(grid)

class GameWon(override val grid: GridState) extends GridEvent(grid)

class GameLost(override val grid: GridState) extends GridEvent(grid)