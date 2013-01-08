package de.fhkoeln.minesweeper.model

import scala.util.Random
import scala.collection.mutable.HashSet
import scala.annotation.tailrec

case class MineFieldGrid( val ysize: Int, val xsize: Int, val minecount: Int, val initial_field: ( Int, Int ) = ( 0, 0 ) ) {

    require( minecount < xsize * ysize, { "Too many mines for grid size" } )

    require( minecount >= 0, { "minecount must be positive" } )

    require( xsize > 0 && ysize > 0, { "xsize and ysize must be positive" } )

    require( initial_field._1 >= 0 && initial_field._2 >= 0, { "Initial field coordinates must be positive" } )

    private var grid = Array.ofDim[ Field ]( ysize, xsize )

    private val xboundaries = 0 until xsize

    private val yboundaries = 0 until ysize

    private var mineUncovered = false

    private var uncovered = 0

    populateField()

    //get a representation of the State of the grid
    def getGridState(): GridState = grid map ( x => x.toList.map( _.state ) ) toList

    /*
	 *  @Return: tuple containing updated grid, Boolean indicating whether a mine was uncovered and finally a Boolean
	 * if everything besides the mines was uncovered.
	 */
    def uncoverField( pos: ( Int, Int ) ): ( GridState, Boolean, Boolean ) = {
        try {
            doUncover( pos :: Nil )
            ( getGridState, mineUncovered, gameWon() )
        } catch {
            case aaobe: ArrayIndexOutOfBoundsException => throw new MineGridException( "Invalid position: " + pos.toString() )
        }

    }

    //mark the Field at specified position. Throws Exception if said field is already uncovered
    def markField( pos: ( Int, Int ) ): GridState = {
        try {
            val ( y, x ) = pos
            grid( y )( x ) = grid( y )( x ).mark
            getGridState
        } catch {
            case iae: IllegalArgumentException          => throw new MineGridException( "Trying to mark uncovered field at: " + pos.toString() )
            case aioobe: ArrayIndexOutOfBoundsException => throw new MineGridException( "Invalid position: " + pos.toString() )
        }
    }

    //unmark the Field at specified position
    def unmarkField( pos: ( Int, Int ) ): GridState = {
        try {
            val ( y, x ) = pos
            grid( y )( x ) = grid( y )( x ).unmark
            getGridState
        } catch {
            case aioobe: ArrayIndexOutOfBoundsException => throw new MineGridException( "Invalid position: " + pos.toString() )
        }

    }

    override def toString: String = {
        grid.map( _.mkString( " " ) ).mkString( "\n" )
    }

    //setup the mine grid, i.e. initialize all fields.
    private def populateField() {
        //place mines first...
        placeMines()
        //...then fill the rest and calculate no. of adjacent mines for fields
        for ( i <- 0 until ysize; j <- 0 until xsize; if fieldEmpty( i, j ) ) {
            //make sure no double placements happen (and mines get overwritten)
            grid( i )( j ) = NumberField( MineFieldState.covered( countAdjacentMines( i, j ) ) )
        }
    }

    //Position mines randomly on the grid. Avoid initial position
    private def placeMines() {
        var placed = 0
        val randy = new Random()
        while ( placed != minecount ) {
            val row = randy.nextInt( ysize )
            val col = randy.nextInt( xsize )
            //make sure same field doesn't get populated twice
            if ( fieldEmpty( row, col ) && ( row, col ) != initial_field ) {
                grid( row )( col ) = MineField()
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

    //auxilary method for uncovering fields
    //keep recursively uncovering fields until a number field or marked field is uncovered

    private def doUncover( positions: List[ ( Int, Int ) ] ) {
        positions match {
            case head :: tail => {
                val ( y, x ) = head
                val field = grid( y )( x )
                if ( field.covered ) {
                    grid( y )( x ) = field.uncover()
                    uncovered += 1
                    mineUncovered = field.armed
                }
                if ( field.adjacent == 0 && !field.armed && !field.marked )
                    doUncover( (tail ++ getAdjacentPos( y, x ).filter( z => grid( z._1 )( z._2 ).covered )).distinct )
            }

            case nil => {}
        }
    }

    //method returning whether the game has been won or not
    private def gameWon(): Boolean = uncovered == xsize * ysize - minecount && !mineUncovered

}

case class MineGridException( s: String ) extends Exception