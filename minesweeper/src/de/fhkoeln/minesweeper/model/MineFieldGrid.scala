package de.fhkoeln.minesweeper.model

import scala.util.Random
import scala.collection.mutable.HashSet



case class MineFieldGrid( val ysize: Int, val xsize: Int, val minecount: Int, val initial_field: ( Int, Int ) = ( 0, 0 ) ) {

    if ( minecount >= xsize * ysize ) throw new IllegalArgumentException( "Too many mines for grid size" )

    if ( minecount < 0 ) throw new IllegalArgumentException( "minecount must be positive" )

    if ( xsize <= 0 || ysize <= 0 ) throw new IllegalArgumentException( "xsize and ysize must be positive" )

    if ( initial_field._1 < 0 || initial_field._2 < 0 ) throw new IllegalArgumentException( "Initial field coordinates must be positive" )

    private var grid = Array.ofDim[ Field ]( ysize, xsize )

    private val xboundaries = 0 until xsize

    private val yboundaries = 0 until ysize

    private var mineUncovered = false

    private var uncovered = new HashSet[ ( Int, Int ) ]()

    populateField()

    //get a copy of the internal grid
    def getGridState(): GridState = grid map( x => (x map (_.state)) toIndexedSeq)
    /*
	 * 
	 * @Return: tupel containing updated grid, Boolean indicating whether a mine was uncovered and finally a Boolean
	 * if everything besides the mines was uncovered.
	 */
    def uncoverField( pos: ( Int, Int ) ): ( GridState, Boolean, Boolean ) = {
        try {
            doUncover( pos )
            ( getGridState, mineUncovered, gameWon() )
        } catch {
            case aaobe: ArrayIndexOutOfBoundsException => throw new MineGridException( "Invalid position: " + pos.toString() )
        }

    }

    //Uncover fields around a number field if it's surrounded by a number of marked fields
    //equal to its number.
    def uncoverAdjacents( pos: ( Int, Int ) ): ( GridState, Boolean, Boolean ) = {
        if ( countMarkedAdjacents( pos ) == grid( pos._1 )( pos._2 ).adjacent ) {
            getAdjacentPos( pos._1, pos._2 ) foreach doUncover
        }
        ( getGridState, mineUncovered, gameWon() )
    }

    //mark the Field at specified position. Throws Exception if said field is already uncovered
    def markField( pos: ( Int, Int ) ): GridState = {
        try {
            val y = pos._1
            val x = pos._2
            grid( y )( x ) = grid(y)(x).mark
            getGridState
        } catch {
            case iae: IllegalArgumentException          => throw new MineGridException( "Trying to mark uncovered field at: " + pos.toString() )
            case aioobe: ArrayIndexOutOfBoundsException => throw new MineGridException( "Invalid position: " + pos.toString() )
        }
    }

    //unmark the Field at specified position
    def unmarkField( pos: ( Int, Int ) ): GridState = {
        try {
            val y = pos._1
            val x = pos._2
            grid( y )( x ) = grid(y)(x).unmark
            getGridState
        } catch {
            case aioobe: ArrayIndexOutOfBoundsException => throw new MineGridException( "Invalid position: " + pos.toString() )
        }

    }
    
    override def toString : String = {
        grid.map(_.mkString(" ")).mkString("\n")
    }

    //Count  marked fields adjacent to the specified one
    private def countMarkedAdjacents( pos: ( Int, Int ) ): Int = {
        getAdjacentPos( pos._1, pos._2 ) count ( x => grid( x._1 )( x._2 ).marked )
    }

    //setup the mine grid, i.e. initialize all fields.
    private def populateField() {
        //place mines first...
        placeMines()
        //...then fill the rest and calculate no. of adjacent mines for fields
        for ( i <- 0 until ysize; j <- 0 until xsize ) {
            //make sure no double placements happen (and mines get overwritten)
            if ( fieldEmpty( i, j ) ) {
                grid( i )( j ) = NumberField( MineFieldState.covered( countAdjacentMines( i, j ) ) )
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
    private def doUncover( position: ( Int, Int ) ) {
        val y = position._1
        val x = position._2
        val field = grid( y )( x )

        if ( !field.marked ) {
            grid(y)(x) = field.uncover()
            mineUncovered = field.armed
            uncovered.add( position )
        }

        if ( field.adjacent == 0 && !field.armed ) {
            getAdjacentPos( y, x ) filter ( x => !uncovered.contains( x ) ) foreach doUncover
        }
    }

    //method returning whether the game has been won or not
    private def gameWon(): Boolean = uncovered.size == xsize * ysize - minecount && !mineUncovered

}

case class MineGridException( s: String ) extends Exception