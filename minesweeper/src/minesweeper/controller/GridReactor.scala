package minesweeper.controller

import scala.collection.mutable

abstract class GridReactor {
    type Reaction = PartialFunction[GridEvent, Unit]
    
    var reactions = new mutable.MutableList[Reaction]()
     
    final def react(e: GridEvent) = {
        executeReaction(e, reactions.toList)
    }
    
    final def listenTo(contr: StandardGridController) = {
        require(contr != null)
        contr.register(this)
    }
    
    private def executeReaction(e: GridEvent, reacs: List[Reaction]): Unit = {
        if(reacs.nonEmpty) {
            if(reacs.head.isDefinedAt(e)) reacs.head(e)
            else executeReaction(e, reacs.tail)
        }
    }
}