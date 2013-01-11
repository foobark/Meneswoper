package minesweeper.model

import scala.util.Random
import scala.collection.mutable.HashSet
import scala.annotation.tailrec

case class MineFieldGrid( val ysize: Int,
                          val xsize: Int,
                          private val mines: List[( Int, Int )] = Nil ) {

    val minecount = mines.length

    require( minecount < xsize * ysize, { "Too many mines for grid size" } )

    require( minecount >= 0, { "minecount must be positive" } )

    require( xsize > 0 && ysize > 0, { "xsize and ysize must be positive" } )

    require( ( xsize * ysize ) > 1, { "Grid must have at least 2 fields" } )

    //require( mine_free forall ( x => x._1 >= 0 && x._2 >= 0 ), { "Initial field coordinates must be positive" } )

    private var grid = Array.ofDim[Field]( ysize, xsize )

    private val xboundaries = 0 until xsize

    private val yboundaries = 0 until ysize

    private var gameLost = false

    private var gameWon = false

    private var uncovered = 0

    placeMines()

    def addMine( pos: ( Int, Int ) ): MineFieldGrid = {
        val ( y, x ) = pos
        require( !grid( y )( x ).armed, { throw new MineGridException( pos + " already is a mine. can't place another one" ) } )
        new MineFieldGrid( ysize, xsize, mines :+ pos )
    }
    
    def removeMine( pos: (Int, Int) ): MineFieldGrid = {
        val (y, x) = pos
        require( grid(y)(x).armed, { throw new MineGridException( pos + " is not a mine. can't remove it")})
        new MineFieldGrid( ysize, xsize, mines - pos)
    }

    //get a representation of the State of the grid
    def getGridState(): GridState = grid map ( _.toList.map( _.state ) ) toList

    /*
	 *  @Return: tuple containing updated grid, Boolean indicating whether a mine was uncovered and finally a Boolean
	 * if everything besides the mines was uncovered.
	 */
    def uncoverField( pos: ( Int, Int ) ): ( GridState, Boolean, Boolean ) = {
        require( inBoundaries( pos ), throw new MineGridException( "Invalid position: " + pos.toString() ) )
        doUncover( pos :: Nil )
        ( getGridState, lost, won )
    }

    //mark the Field at specified position. Throws Exception if said field is already uncovered
    def markField( pos: ( Int, Int ) ): GridState = {
        val ( y, x ) = pos
        require( inBoundaries( pos ), throw new MineGridException( "Invalid position: " + pos.toString() ) )
        require( !grid( y )( x ).uncovered, throw new MineGridException( "Trying to mark uncovered field at: " + pos.toString() ) )
        grid( y )( x ) = grid( y )( x ).mark
        getGridState

    }

    //unmark the Field at specified position
    def unmarkField( pos: ( Int, Int ) ): GridState = {
        require( inBoundaries( pos ), throw new MineGridException( "Invalid position: " + pos.toString() ) )
        val ( y, x ) = pos
        grid( y )( x ) = grid( y )( x ).unmark
        getGridState
    }

    def won: Boolean = (uncovered == xsize * ysize - minecount) && !gameLost

    def lost: Boolean = gameLost
    
    override def toString: String = {
        grid.map( _.mkString( " " ) ).mkString( "\n" )
    }

    //setup the mine grid, i.e. initialize all fields.
    private def placeMines() {
        //place mines first...
        mines.foreach( x => grid( x._1 )( x._2 ) = MineField() )
        //...then fill the rest and calculate no. of adjacent mines for fields
        for ( i <- 0 until ysize; j <- 0 until xsize; if fieldEmpty( i, j ) )
            grid( i )( j ) = NumberField( MineFieldState.covered( countAdjacentMines( i, j ) ) )
    }
     
    //Count the number of Mines adjacent to a field
    private def countAdjacentMines( y: Int, x: Int ): Int = getAdjacentPos( y, x ) count ( isMineField )

    //get adjacent, valid positions to a field
    private def getAdjacentPos( y: Int, x: Int ): List[( Int, Int )] = {
        val offsets = ( -1 to 1 )
        //every combination pair of the values from -1 to 1
        val combos = offsets.map( x => offsets.map( ( x, _ ) ) ).flatten.toList
        combos.map( ( o: ( Int, Int ) ) => ( y + o._1, x + o._2 ) ).filter( isValidPos )
    }

    //check whether the field is an armed mine
    private def isMineField( pos: ( Int, Int ) ): Boolean = grid( pos._1 )( pos._2 ).armed

    //Check if position is within boundaries
    private def isValidPos( pos: ( Int, Int ) ): Boolean = inBoundaries( pos ) && !fieldEmpty( pos._1, pos._2 )

    //check whether the position has already been populated with a field
    private def fieldEmpty( y: Int, x: Int ): Boolean = {
        grid( y )( x ) == null
    }

    //auxilary method for uncovering fields
    //keep recursively uncovering fields until a number field or marked field is uncovered

    private def doUncover( positions: List[( Int, Int )] ) {
        if ( positions.nonEmpty ) {
            val ( y, x ) = positions.head
            val field = grid( y )( x )
            if ( field.covered ) {
                grid( y )( x ) = field.uncover()
                uncovered += 1
                gameLost = if(!gameLost) field.armed else true
            }
            if ( field.adjacent == 0 && !field.armed && !field.marked ) {
                val neighbours = getAdjacentPos( y, x ).filter( z => grid( z._1 )( z._2 ).covered )
                doUncover( ( positions.tail ++ neighbours ).distinct )
            }
        }
    }

    //method returning whether the game has been won or not
    

    private def inBoundaries( pos: ( Int, Int ) ): Boolean = {
        yboundaries.contains( pos._1 ) && xboundaries.contains( pos._2 )
    }
}

object MineFieldGrid {
    
	def apply(ysize: Int, xsize: Int, minecount: Int, excludes: List[(Int, Int)]): MineFieldGrid = {
	    def randstream: Stream[(Int, Int)] = (Random.nextInt(ysize), Random.nextInt(xsize)) #:: randstream
	    val mines = randstream.distinct.filter(!excludes.contains(_)).take(minecount).toList
	    new MineFieldGrid(ysize, xsize, mines)
	}
	
	
}

case class MineGridException( s: String ) extends Exception