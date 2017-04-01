/**
 * Created by nperez on 11/9/15.
 */


import ASTCluster.Parser.Parser
import ASTCluster.Tokenizer.Lexer

object App {
  def main(args: Array[String]) {
    print(args)
//    val source = "let s = 5 0 in rec i -> " +
//      "func x -> fun y -> if iszero y then x else i (succ x) (pred y)"

    val ast = Lexer.lexerFile(args(0))

//    val result = Interpreter.eval(ast)

    println(ast)
  }
}
