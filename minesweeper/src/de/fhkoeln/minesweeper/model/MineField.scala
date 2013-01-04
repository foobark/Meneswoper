package de.fhkoeln.minesweeper.model

case class MineField( val adjacent: Int = 0,
                      val armed: Boolean = false,
                      private var _uncovered: Boolean = false,
                      private var _marked: Boolean = false ) {

    if ( adjacent < 0 ) throw new IllegalArgumentException( "parameter adjacent needs to be positive" )

    if ( adjacent > 0 && armed ) throw new IllegalArgumentException( "Armed Minefield has no adjacent mines" )

    checkMarkedConstraint()

    def marked: Boolean = _marked

    def marked_=( x: Boolean ) = {
        _marked = x
        //throw Exception when field is already uncovered 
        checkMarkedConstraint()
    }

    def uncovered(): Boolean = _uncovered

    //Uncovering is one time operation
    def uncover() = {
        _uncovered = true
        //throw Exception when Field is already marked
        checkMarkedConstraint()
    }

    override def toString: String = {
        this match {
            case MineField( _, true, _, _ ) if uncovered           => "*"
            case MineField( x, _, _, _ ) if uncovered  => x.toString;
            case _ if !uncovered && !marked => "H"
            case _ if marked => "#"
        }
    }

    //Uncovered field can't be marked and vice versa
    private def checkMarkedConstraint() = if ( _marked && _uncovered ) throw new IllegalArgumentException( "Can't mark uncovered Field" )
}