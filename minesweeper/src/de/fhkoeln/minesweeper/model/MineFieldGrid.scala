package de.fhkoeln.minesweeper.model

import scala.util.Random
import scala.collection.mutable.HashSet

case class MineFieldGrid( val xsize: Int, val ysize: Int, val minecount: Int, val initial_field: ( Int, Int ) = ( 0, 0 ) ) {

    if ( minecount > xsize * ysize ) throw new IllegalArgumentException( "Too many mines for grid size" )

    if ( minecount < 0 ) throw new IllegalArgumentException( "minecount must be positive" )

    if ( xsize <= 0 || ysize <= 0 ) throw new IllegalArgumentException( "xsize and ysize must be positive" )

    if ( initial_field._1 < 0 || initial_field._2 < 0 ) throw new IllegalArgumentException( "Initial field coordinates must be positive" )

    private var grid = Array.ofDim[ MineField ]( ysize, xsize )

    private val xboundaries = 0 until xsize

    private val yboundaries = 0 until ysize

    private var mineUncovered = false

    private var uncovered = new HashSet[ ( Int, Int ) ]()

    populateField()

    //get a copy of the internal grid
    def getGrid(): Array[ Array[ MineField ] ] = grid.clone()

    /*
	 * 
	 * @Return: tupel containing updated grid, Boolean indicating whether a mine was uncovered and finally a Boolean
	 * if everything besides the mines was uncovered.
	 */
    def uncoverField( pos: ( Int, Int ) ): ( Array[ Array[ MineField ] ], Boolean, Boolean ) = {
        doUncover( pos )
        ( grid.clone(), mineUncovered, gameWon() )
    }

    //mark the Field at specified position. Throws Exception if said field is already uncovered
    def markField( pos: ( Int, Int ) ): Array[Array[MineField]] = {
        try {
        	grid( pos._1 )( pos._2 ).marked = true
        	grid.clone()
        }
        catch {
            case iae: IllegalArgumentException => throw new MineGridException( "Trying to mark uncovered field at: " + pos.toString() )
        }
    }

    //unmark the Field at specified position
    def unmarkField( pos: ( Int, Int ) ) = grid( pos._1 )( pos._2 ).marked = false

    //setup the mine grid with, i.e. initialize all fields.
    private def populateField() {
        //place mines first...
        placeMines()
        //...then fill the rest and calculate no. of adjacent mines for fields
        for ( i <- 0 until ysize; j <- 0 until xsize ) {
            //make sure no double placements happen (and mines get overwritten)
            if ( fieldEmpty( i, j ) ) {
                grid( i )( j ) = MineField( adjacent = countAdjacentMines( i, j ) )
            }
        }
    }

    //Position mines randomly on the grid. Avoid initial position
    private def placeMines() {
        var placed = 0
        var randy = new Random()
        while ( placed != minecount ) {
            val row = randy.nextInt( ysize )
            val col = randy.nextInt( xsize )
            //make sure same field doesn't get populated twice
            if ( fieldEmpty( row, col ) && ( row, col ) != initial_field ) {
                grid( row )( col ) = MineField( armed = true )
                placed += 1
            }
        }
    }

    //Count the number of Mines adjacent to a field
    private def countAdjacentMines( y: Int, x: Int ): Int = getAdjacentPos( y, x ) count ( isMineField )

    //get adjacent, valid positions to a field
    private def getAdjacentPos( y: Int, x: Int ): List[ ( Int, Int ) ] = {
        List(
            ( y + 1, x ),
            ( y + 1, x - 1 ),
            ( y, x - 1 ),
            ( y - 1, x - 1 ),
            ( y - 1, x ),
            ( y - 1, x + 1 ),
            ( y, x + 1 ),
            ( y + 1, x + 1 ) ).filter( isValidPos )
    }

    //check whether the field is an armed mine
    private def isMineField( pos: ( Int, Int ) ): Boolean = grid( pos._1 )( pos._2 ).armed

    //Check if position is within boundaries
    private def isValidPos( pos: ( Int, Int ) ): Boolean = yboundaries.contains( pos._1 ) && xboundaries.contains( pos._2 ) && !fieldEmpty( pos._1, pos._2 )

    //check whether the position has already been populated with a field
    private def fieldEmpty( y: Int, x: Int ): Boolean = {
        grid( y )( x ) eq null
    }

    private def doUncover( position: ( Int, Int ) ) {
        val y = position._1
        val x = position._2
        val field = grid( y )( x )
        try {
            field.uncover()
            mineUncovered = field.armed
            uncovered.add( position )
        } catch {
            case iae: IllegalArgumentException => throw new MineGridException( "Trying to uncover marked field at: " + position.toString() )
        }
        if ( field.adjacent == 0 && !field.armed ) {
            getAdjacentPos( y, x ) foreach doUncover
        }
    }

    private def gameWon(): Boolean = uncovered.size == xsize * ysize - minecount && !mineUncovered

}

case class MineGridException( s: String ) extends Exception