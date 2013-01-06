package de.fhkoeln.minesweeper.model

abstract case class Field(val armed:Boolean = false, val state: MineFieldState = MineFieldState.covered(0) ) {

    val adjacent = state.adjacent
    
    def covered: Boolean = state.covered
    
    def marked: Boolean = state.marked

    def mark(): Field
    
    def unmark(): Field

    def uncovered: Boolean = state.uncovered
    
    def triggered: Boolean = state.triggered

    //Uncovering is one time operation
    def uncover(): Field

    override def toString: String = {
        state match {
            case t: MineFieldState.triggered => "*"
            case MineFieldState.uncovered(x) => x.toString;
            case c: MineFieldState.covered   => "H"
            case m: MineFieldState.marked    => "#"
        }
    }
    
    protected def checkMarkedConstraint = if(uncovered || triggered) throw new IllegalArgumentException("can't mark uncovered Field")
    protected def checkUncoveredConstraint = if(marked) throw new IllegalArgumentException("Can't uncover marked Field")
    protected def checkUnmarkConstraint = if(!marked) throw new IllegalArgumentException("Can't unmark field that is not marked")
}