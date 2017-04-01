package ASTCluster.Tokenizer

import ASTCluster.Pipes.IPipe._

object Lexer {
  private def expand(s: String) = s.toCharArray.toList

  private def isDigit(c: Char) = Character.isDigit(c)

  private def isWhiteSpace(c: Char) =  Character.isWhitespace(c)

  private def isValidIdStart(c: Char) = Character.isJavaIdentifierStart(c)

  private def isValidIdPart(c: Char) = Character.isJavaIdentifierStart(c)

  private def getKeyword(s: String): Token = s match {
    // Types
    case "bool"          => TYPE_BOOL()
    case "byte"          => TYPE_BYTE()
    case "char"          => TYPE_CHAR()
    case "short"         => TYPE_SHORT()
    case "int"           => TYPE_INT()
    case "long"          => TYPE_LONG()
    case "float"         => TYPE_FLOAT()
    case "double"        => TYPE_DOUBLE()
    case "void"          => TYPE_VOID()
    case "null"          => TYPE_NULL()

    // Branch Control
    case "if"            => IF()
    case "else"          => ELSE()
    case "for"           => FOR()
    case "while"         => WHILE()
    case "return"        => RETURN()
    case "continue"      => CONTINUE()
    case "break"         => BREAK()
    case "do"            => DO()

    // Switch Cases
    case "case"          =>  CASE()
    case "switch"        =>  SWITCH()
    case "default"       =>  DEFAULT()

    // Keywords
    case "true"          => TRUE()
    case "false"         => FALSE()
    case "new"           => NEW()
    case "this"          => THIS()
    case "assert"        => ASSERT()
    case "instanceof"    => INSTANCEOF()
    case "enum"          => ENUM()

    // Decorators
    case "synchronized"  => SYNCHRONIZED()
    case "final"         => FINAL()
    case "static"        => STATIC()
    case "volatile"      => VOLATILE()
    case "super"         => SUPER()
    case "transient"     => TRANSIENT()
    case "strictfp"      => STRICTFP()
    case "native"        => NATIVE()

    // Scope
    case "private"       => PRIVATE()
    case "public"        => PUBLIC()
    case "protected"     => PROTECTED()

    // Classes
    case "class"         => CLASS()
    case "implements"    => IMPLEMENTS()
    case "abstract"      => ABSTRACT()
    case "extends"       => EXTENDS()
    case "interface"     => INTERFACE()

    // Bundling
    case "package"       => PACKAGE()
    case "import"        => IMPORT()

    // Exceptions
    case "try"           => TRY()
    case "catch"         => CATCH()
    case "finally"       => FINALLY()
    case "throw"         => THROW()
    case "throws"        => THROWS()

    case id              => LIT_ID(id)
  }

  private def getId(ident: String, l: List[Char]): (Token, List[Char])  = l match {
    case c :: cs if isValidIdPart(c)  => getId(ident + c, cs)
    case cs                           => (getKeyword(ident), cs)
  }

  private def getFloat(num: Double, l: List[Char]): (Token, List[Char]) = l match {
    case c :: cs if isDigit(c)  => getFloat(10 * num + c.toString.toInt, cs)
    case cs                     => (LIT_DOUBLE(num), cs)
  }

  private def getNum(num: Int, l: List[Char]): (Token, List[Char]) = l match {
    case '.' :: cs              => getFloat(num.toDouble, cs)
    case c :: cs if isDigit(c)  => getNum(10 * num + c.toString.toInt, cs)
    case cs                     => (LIT_INT(num), cs)
  }

  private def getStr(str: String, x: List[Char]): (Token, List[Char]) = x match {
    case '\"' :: cs  => (LIT_STRING(str), cs)
    case c :: cs     => getStr(str + c, cs)
    case Nil         => (EOF(), List())
  }

  def skipLine(x: List[Char]): (Token, List[Char]) = x match {
    case Nil => (EOF(), List())
    case '\n' :: cs1  => getTok(cs1)
    case _ :: cs1     => skipLine(cs1)
  }

  def skipBlock(x: List[Char]): (Token, List[Char]) = x match {
    case Nil => (EOF(), List())
    case '*' :: '/' :: cs1 => getTok(cs1)
    case _ :: cs1 => skipBlock(cs1)
  }

  private def getTok(l: List[Char]): (Token, List[Char]) = l match {
    case Nil => (EOF(), List())

    case c :: cs if isWhiteSpace(c)    => getTok(cs)
    case c :: cs if isValidIdStart(c)  => getId(c.toString, cs)
    case c :: cs if isDigit(c)         => getNum(c.toString.toInt, cs)

    case '/' :: '/' :: cs              => skipLine(cs)
    case '/' :: '*' :: cs              => skipBlock(cs)

    case '\"' :: cs                    => getStr("", cs)

    case '\'' :: c :: '\'' :: cs       => (LIT_CHAR(c), cs)

    case ';' :: cs                     => (SEMI(), cs)
    case ',' :: cs                     => (COMMA(), cs)
    case '(' :: cs                     => (LPAREN(), cs)
    case ')' :: cs                     => (RPAREN(), cs)
    case '{' :: cs                     => (LBRACE(), cs)
    case '}' :: cs                     => (RBRACE(), cs)
    case '[' :: cs                     => (LBRACKET(), cs)
    case ']' :: cs                     => (RBRACKET(), cs)

    case '<' :: '<' :: '<' :: cs       => (BIT_UNSIGNED_SHIFT_LEFT(), cs)
    case '>' :: '>' :: '>' :: cs       => (BIT_UNSIGNED_SHIFT_RIGHT(), cs)
    case '<' :: '<' :: cs              => (BIT_SHIFT_LEFT(), cs)
    case '>' :: '>' :: cs              => (BIT_SHIFT_RIGHT(), cs)
    case '<' :: '=' :: cs              => (LOGIC_LEQ(), cs)
    case '>' :: '=' :: cs              => (LOGIC_GEQ(), cs)
    case '<' :: cs                     => (LANGLE(), cs)
    case '>' :: cs                     => (RANGLE(), cs)

    case '+' :: cs                     => (OP_PLUS(), cs)
    case '-' :: cs                     => (OP_MINUS(), cs)
    case '*' :: cs                     => (OP_TIMES(), cs)
    case '/' :: cs                     => (OP_DIVIDE(), cs)
    case '%' :: cs                     => (OP_MODULO(), cs)

    case '.' :: cs                     => (DOT(), cs)

    case '|' :: '|' :: cs              => (LOGIC_OR(), cs)
    case '|' :: cs                     => (BIT_OR(), cs)

    case '&' :: '&' :: cs              => (LOGIC_AND(), cs)
    case '&' :: cs                     => (BIT_AND(), cs)

    case '=' :: '=' :: cs              => (LOGIC_EQUAL(), cs)
    case '=' :: cs                     => (ASSIGN(), cs)
    case '!' :: '=' :: cs              => (LOGIC_NEQ(), cs)
    case '!' :: cs                     => (LOGIC_NOT(), cs)

    case c :: cs                       => {
      println("Skipping illegal character: " + c)

      getTok(cs)
    }
  }

  private def tokenize(cs: List[Char]): List[Token] = {
    def getToks(toks: List[Token], cs: List[Char]): List[Token] = {
      getTok(cs) match {
        case (EOF(), cs)  => (EOF() :: toks).reverse
        case (tok, css)   => getToks(tok :: toks, css)
      }
    }

    getToks(List(), cs)
  }

  def lexerStr(sourceCode: String): List[Token] = sourceCode |> expand |> tokenize

  def lexerFile(fileName: String): List[Token] = scala.io.Source.fromFile(fileName).mkString |> lexerStr
}
