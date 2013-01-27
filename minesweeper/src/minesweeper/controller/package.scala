package minesweeper

package object controller {
	type GridState = model.GridState
	type GridDifficulty = (Int, Int, Int)
	type MineFieldState = model.MineFieldState
	val MineFieldState = model.MineFieldState
}