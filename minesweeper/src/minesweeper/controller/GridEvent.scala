package minesweeper.controller

abstract class GridEvent( val grid: GridState, val field: ( Int, Int ) )

class NewGameStarted( grid: GridState) extends GridEvent( grid, (0,0) )

class FieldUncovered( grid: GridState, field: ( Int, Int ) ) extends GridEvent( grid, field )

class FieldMarked( grid: GridState, field: ( Int, Int ) ) extends GridEvent( grid, field )

class FieldUnmarked( grid: GridState, field: ( Int, Int ) ) extends GridEvent( grid, field )

class GameWon( grid: GridState ) extends GridEvent( grid, (0,0) )

class GameLost( grid: GridState) extends GridEvent( grid, (0,0) )