package sutd.compiler.simp.lattice

import sutd.compiler.simp.lattice.CompleteLattice.{given, *} 

object SignLattice {
    import CompleteLattice.* 
    enum SignAbsVal {
        case Bot    // _|_
        case Minus  // -
        case Plus   // + 
        case Top    // T
        case Zero   // 0
    }

    import SignAbsVal.*
    // Cohort Problem Exercise 2
    given signLattice:CompleteLattice[SignAbsVal] = new CompleteLattice[SignAbsVal] {
        def sqSubSetEq(a: SignAbsVal, b: SignAbsVal): Option[Boolean] =
            Some(a == b)
        def lub(a:SignAbsVal, b:SignAbsVal):SignAbsVal = (a, b) match
            case (Top, _) => Top
            case (_, Top) => Top
            case (Bot, _) => Bot
            case (_, Bot) => Bot
            case (Plus, Plus) => Plus
            case (Minus, Minus) => Minus
            case (Zero, Zero) => Zero
            case (_, _) => Top        
    }
}