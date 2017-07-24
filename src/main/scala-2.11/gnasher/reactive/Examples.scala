package gnasher.reactive

object Examples {
  val mutex =
    """
      |arena mutex controls x0, y0, x1, y1, x2, y2
      |
      |module rbme0 controls x0, y0
      |init
      |     --> x0 := false; y0 := false
      |update
      |     y2 | x0 --> x0 := true
      |  :: y2 | x0 --> x0 := false; y0 := true
      |  :: y0      --> y0 := false
      |endmodule
      |
      |module rbme1 controls x1, y1
      |init
      |     --> x1 := false; y1 := false
      |update
      |     y0 | x1 --> x1 := true
      |  :: y0 | x1 --> x1 := false; y1 := true
      |  :: y1      --> y1 := false
      |endmodule
      |
      |module rbme2 controls x2, y2
      |init
      |     --> x2 := false; y2 := true
      |update
      |     y1 | x2 --> x2 := true
      |  :: y1 | x2 --> x2 := false; y2 := true
      |  :: y2      --> y2 := false
      |endmodule
      |
      |endarena
    """.stripMargin

  val mutexGoals1 =
    """
      |goals
      |  rbme0 := F(x0 | y0);
      |  rbme1 := F(x1 | y1);
      |  rbme2 := F(x2 | y2)
      |endgoals
    """.stripMargin

  val mutexWithButler =
    """
      |arena mutex2 controls x0, y0, x1, y1, x2, y2, p
      |
      |module butler controls p
      |init
      |     --> p := false
      |update
      |     true --> p := true
      |  :: true --> p := false
      |endmodule
      |
      |module rbme0 controls x0, y0
      |init
      |     --> x0 := false; y0 := false
      |update
      |     (y2 | x0) & ~p --> x0 := true
      |  :: y2 | x0 --> x0 := false; y0 := true
      |  :: y0      --> y0 := false
      |endmodule
      |
      |module rbme1 controls x1, y1
      |init
      |     --> x1 := false; y1 := false
      |update
      |     (y0 | x1) & ~p --> x1 := true
      |  :: y0 | x1 --> x1 := false; y1 := true
      |  :: y1      --> y1 := false
      |endmodule
      |
      |module rbme2 controls x2, y2
      |init
      |     --> x2 := false; y2 := true
      |update
      |     (y1 | x2) & ~p --> x2 := true
      |  :: y1 | x2 --> x2 := false; y2 := true
      |  :: y2      --> y2 := false
      |endmodule
      |
      |endarena
    """.stripMargin

  val rps =
    """
      |arena rps controls r1, r2, p1, p2, s1, s2, w1, w2, t
      |
      |module judge controls w1, w2, t
      |init
      |     --> t := false
      |update
      |     ~t --> t := true; w1 := false; w2 := false
      |  :: r1 & s2 --> w1 := true; t := false
      |  :: s1 & p2 --> w1 := true; t := false
      |  :: p1 & r2 --> w1 := true; t := false
      |
      |  :: r2 & s1 --> w2 := true; t := false
      |  :: s2 & p1 --> w2 := true; t := false
      |  :: p2 & r1 --> w2 := true; t := false
      |
      |  :: r1 & r2 --> t := false
      |  :: s1 & s2 --> t := false
      |  :: p1 & p2 --> t := false
      |endmodule
      |
      |module rps1 controls r1, p1, s1
      |init
      |     --> r1 := false; p1 := false; s1 := false
      |update
      |     ~t --> r1 := true
      |  :: ~t --> p1 := true
      |  :: ~t --> s1 := true
      |  :: t  --> r1 := false; p1 := false; s1 := false
      |endmodule
      |
      |module rps2 controls r2, p2, s2
      |init
      |     --> r2 := false; p2 := false; s2 := false
      |update
      |     ~t --> r2 := true
      |  :: ~t --> p2 := true
      |  :: ~t --> s2 := true
      |  :: t  --> r2 := false; p2 := false; s2 := false
      |endmodule
      |
      |endarena
    """.stripMargin

  val copycat =
    """
      |arena copycat controls x,y
      |
      |module a controls x
      |init
      |     --> x := false
      |update
      |     true --> x := true
      |  :: true --> x := false
      |endmodule
      |
      |module b controls y
      |init
      |     --> y := false
      |update
      |     x  --> y := true
      |  :: ~x --> y:= false
      |endmodule
      |
      |endarena
    """.stripMargin
}
