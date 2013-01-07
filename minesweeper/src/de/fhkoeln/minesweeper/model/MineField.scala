package de.fhkoeln.minesweeper.model

case class MineField private (override val state: MineFieldState = MineFieldState.covered( 0 ) ) extends Field( true, state ) {

    def mark(): Field = {
        checkMarkedConstraint
        MineField.marked
    }

    def uncover(): Field = {
        checkUncoveredConstraint
        MineField.triggered
    }

    def unmark(): Field = {
        checkUnmarkConstraint
        MineField.covered
    }
    
    override def toString() = "MineField - state: " + state.toString
}

object MineField {
    lazy val covered = new MineField()
    lazy val marked = new MineField( MineFieldState.marked() )
    lazy val triggered = new MineField( MineFieldState.triggered() )
    
    def apply():MineField = covered
}