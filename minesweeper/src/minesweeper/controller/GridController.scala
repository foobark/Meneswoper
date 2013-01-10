package minesweeper.controller

import scala.collection.mutable.LinkedList
import minesweeper.model.MineFieldGrid
import minesweeper.model.MineFieldState

class GridController {

    private var reactors = List[ GridReactor ]()
    private var grid: MineFieldGrid = null
    private var newgame = true
    private var difficulty = Difficulties.easy
    private var xsize = difficulty._1
    private var ysize = difficulty._2
    private var xboundaries = 0 until xsize
    private var yboundaries = 0 until ysize

    def register( reactor: GridReactor ) = {
        require( reactor != null )
        reactors = reactors :+ reactor
    }

    protected def update( event: GridEvent ) = {
        reactors foreach ( _.react( event ) )
    }

    def startNewGame( diff: GridDifficulty = Difficulties.easy ) = {
        require( difficulty != null )
        difficulty = diff
        xsize = difficulty._2
        ysize = difficulty._1
        yboundaries = 0 until ysize
        xboundaries = 0 until xsize
        grid = null
        newgame = true
        val coveredRow = List[ MineFieldState ]() ++ ( for ( i <- 0 until difficulty._2 ) yield MineFieldState.covered() )
        val stategrid: GridState = List() ++ ( for ( i <- 0 until difficulty._1 ) yield coveredRow )
        update( new NewGameStarted( stategrid ) )
    }

    def uncoverPosition( y: Int, x: Int ) = {
        boundaryCheck( y, x )
        if ( newgame ) {
            grid = MineFieldGrid( ysize, xsize, difficulty._3, ( y, x ) :: Nil)
            newgame = false
        }
        val result = grid.uncoverField( y, x )
        val newgrid = result._1
        val lost = result._2
        val won = result._3
        if ( lost ) update( new GameLost( newgrid ) )
        else {
            if ( won ) update( new GameWon( newgrid ) )
            else update( new FieldUncovered( newgrid, ( y, x ) ) )
        }

    }

    def markPosition( y: Int, x: Int ) = {
        boundaryCheck( y, x )
        if ( !newgame ) {
            grid.markField( ( y, x ) )
            update( new FieldMarked( grid.getGridState(), ( y, x ) ) )
        }
    }

    def unmarkPosition( y: Int, x: Int ) = {
        boundaryCheck( y, x )
        if ( !newgame ) {
            grid.unmarkField( ( y, x ) )
            update( new FieldUnmarked( grid.getGridState(), ( y, x ) ) )
        }
    }

    private def boundaryCheck( y: Int, x: Int ) = require( ( yboundaries contains y ) && ( xboundaries contains x ) )
}