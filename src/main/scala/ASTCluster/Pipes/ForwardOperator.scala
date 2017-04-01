/**
 * Created by nperez on 11/9/15.
 */

package ASTCluster.Pipes

class Pipe[T](a: T) {
  def |>[B](f: T => B) = f(a)
}

object IPipe {
  implicit def toPipe[T](v: T) = {
    new Pipe[T](v)
  }
}

object Pipe{
  def apply[T](a: T) = new Pipe[T](a)
}
