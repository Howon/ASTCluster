package ASTCluster.Tokenizer

trait Token

case class EOF() extends Token

/**
  * Types
  */
case class TYPE_BOOL() extends Token
case class TYPE_BYTE() extends Token
case class TYPE_CHAR() extends Token
case class TYPE_SHORT() extends Token
case class TYPE_INT() extends Token
case class TYPE_LONG() extends Token
case class TYPE_FLOAT() extends Token
case class TYPE_DOUBLE() extends Token
case class TYPE_VOID() extends Token
case class TYPE_NULL() extends Token

/**
  * Literals
  */
case class LIT_BOOL(value: Boolean) extends Token
case class LIT_BYTE(value: Byte) extends Token
case class LIT_CHAR(value: Char) extends Token
case class LIT_SHORT(value: Short) extends Token
case class LIT_INT(value: Int) extends Token
case class LIT_LONG(value: Long) extends Token
case class LIT_FLOAT(value: Float) extends Token
case class LIT_DOUBLE(value: Double) extends Token
case class LIT_STRING(value: String) extends Token
case class LIT_VOID() extends Token
case class LIT_NULL() extends Token
case class LIT_ID(value: String) extends Token

/**
  * Delimiters
  */
case class LPAREN() extends Token
case class RPAREN() extends Token
case class LBRACE() extends Token
case class RBRACE() extends Token
case class LBRACKET() extends Token
case class RBRACKET() extends Token
case class LANGLE() extends Token
case class RANGLE() extends Token

case class SEMI() extends Token
case class COMMA() extends Token

case class BEGIN_MULTICOMMENT() extends Token
case class END_MULTICOMMENT() extends Token

/**
  * Arithmetic Operations
  */
case class OP_PLUS() extends Token
case class OP_MINUS() extends Token
case class OP_TIMES() extends Token
case class OP_DIVIDE() extends Token
case class OP_MODULO() extends Token

/**
  * Bit Operations
  */
case class BIT_AND() extends Token
case class BIT_OR() extends Token
case class BIT_XOR() extends Token
case class BIT_SHIFT_LEFT() extends Token
case class BIT_SHIFT_RIGHT() extends Token
case class BIT_UNSIGNED_SHIFT_LEFT() extends Token
case class BIT_UNSIGNED_SHIFT_RIGHT() extends Token

case class ASSIGN() extends Token
case class DOT() extends Token

case class LOGIC_EQUAL() extends Token
case class LOGIC_NEQ() extends Token
case class LOGIC_LEQ() extends Token
case class LOGIC_GEQ() extends Token
case class LOGIC_AND() extends Token
case class LOGIC_OR() extends Token
case class LOGIC_NOT() extends Token

/**
  * Branch Control
  */
case class IF() extends Token
case class ELSE() extends Token
case class FOR() extends Token
case class WHILE() extends Token
case class RETURN() extends Token
case class CONTINUE() extends Token
case class BREAK() extends Token
case class DO() extends Token

/**
  * Switch Cases
  */
case class CASE() extends Token
case class SWITCH() extends Token
case class DEFAULT() extends Token

/**
  * Keywords
  */
case class TRUE() extends Token
case class FALSE() extends Token
case class NEW() extends Token
case class THIS() extends Token
case class ASSERT() extends Token
case class INSTANCEOF() extends Token
case class ENUM() extends Token

/**
  * Decorators
  */
case class SYNCHRONIZED() extends Token
case class FINAL() extends Token
case class STATIC() extends Token
case class VOLATILE() extends Token
case class SUPER() extends Token
case class TRANSIENT() extends Token
case class STRICTFP() extends Token
case class NATIVE() extends Token

/**
  * Scope
  */
case class PRIVATE() extends Token
case class PUBLIC() extends Token
case class PROTECTED() extends Token

/**
  * Classes
  */
case class CLASS() extends Token
case class IMPLEMENTS() extends Token
case class ABSTRACT() extends Token
case class EXTENDS() extends Token
case class INTERFACE() extends Token

/**
  * Bundling
  */
case class PACKAGE() extends Token
case class IMPORT() extends Token

/**
  * Exceptions
  */
case class TRY() extends Token
case class CATCH() extends Token
case class FINALLY() extends Token
case class THROW() extends Token
case class THROWS() extends Token
