package gnasher

object Util {
    def cartesianProduct[A](xss: Seq[Seq[A]]): Seq[Seq[A]] = {
      def prod1[A](xss: Seq[Seq[A]]): Seq[Seq[A]] = {
        if (xss.isEmpty) Seq.empty
        else if (xss.length == 1) xss.head.map(x => Seq(x))
        else for {
          xs <- prod1(xss.tail)
          x <- xss.head
        } yield xs :+ x
      }
      prod1(xss.reverse)
    }
}
