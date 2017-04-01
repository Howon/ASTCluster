/**
  * Created by nperez on 11/10/15.
 */


package ASTCluster.Parser.AST

trait Term
/**
  * Primitive Types
  *
  * PrimitiveType =
  *    BooleanT
  *  | ByteT
  *  | CharT
  *  | ShortT
  *  | IntT
  *  | LongT
  *  | FloatT
  *  | DoubleT
  *  | NullT
  *  | VoidT
  *  | ObjectT of String
  *  | ConstructorT
  */
sealed trait PrimitiveType extends Term
case class BoolT() extends PrimitiveType
case class ByteT() extends PrimitiveType
case class CharT() extends PrimitiveType
case class ShortT() extends PrimitiveType
case class IntT() extends PrimitiveType
case class LongT() extends PrimitiveType
case class FloatT() extends PrimitiveType
case class DoubleT() extends PrimitiveType
case class NullT() extends PrimitiveType
case class VoidT() extends PrimitiveType
case class ObjectT(name: String) extends PrimitiveType
case class ConstructorT() extends PrimitiveType

/**
  * DataTypes
  *
  * Type =
  *    Array of Int * PrimitiveType
  *  | DataType of PrimitiveType
  *  | Any
  */
sealed trait Type extends Term
case class Array(typ: PrimitiveType) extends Type
case class DataType(typ: PrimitiveType) extends Type
case class Any() extends Type

/**
  * Extending a class
  * Type =
  *    NoParent
  *  | Parent of String
  */
sealed trait Extend extends Term
case class NoParent() extends Extend
case class Parent(name: String) extends Extend

/**
  * Funciton name
  * FuncName =
  *    Constructor
  *  | FName of String
  */
sealed trait FuncName extends Term
case class Constructor() extends FuncName
case class FName(name: String) extends FuncName

/**
  * Function formals
  * FuncFormal of Datatype * String
  */
case class FuncFormal(typ: Type, name: String) extends Term

/**
  * Program Encapsulation Level
  * Scope  =
  *    Public
  *  | Protected
  *  | Private
  */
sealed trait Scope extends Term
case class Public() extends Scope
case class Protected() extends Scope
case class Private() extends Scope

/**
  * Unary Operations
  * OpUnOp  =
  *    Not
  *  | Neg
  */
sealed trait OpUnOp extends Term
case class Not() extends OpUnOp
case class Neg() extends OpUnOp

/**
  * Unary Operations
  * OpBinOp  =
  *    Add
  *  | Subtract
  *  | Multiply
  *  | Divide
  *  | Mod
  *  | Equal
  *  | Neq
  *  | Less
  *  | Leq
  *  | Greater
  *  | Geq
  *  | And
  *  | Or
  *  | BitAnd
  *  | BitOr
  *  | BitXor
  *  | BitShiftR
  *  | BitShiftL
  */
sealed trait OpBinOp extends Term

// Arithmetic Operations
case class Add() extends OpBinOp
case class Subtract() extends OpBinOp
case class Multiply() extends OpBinOp
case class Divide() extends OpBinOp
case class Mod() extends OpBinOp

// Boolean Operations
case class Equal() extends OpBinOp
case class Neq() extends OpBinOp
case class Less() extends OpBinOp
case class Leq() extends OpBinOp
case class Greater() extends OpBinOp
case class Geq() extends OpBinOp
case class And() extends OpBinOp
case class Or() extends OpBinOp

// Bitwise Operations
case class BitAnd() extends OpBinOp
case class BitOr() extends OpBinOp
case class BitXor() extends OpBinOp
case class BitShiftR() extends OpBinOp
case class BitShiftL() extends OpBinOp

/**
  * Decorators
  * OpBinOp  =
  *    Synchronized
  *  | Final
  *  | Static
  *  | Volatile
  *  | Super
  *  | Transient
  *  | Strictfp
  *  | Native
  *  | NoDec
  */
sealed trait Decorator
case class Synchronized() extends Decorator
case class Final() extends Decorator
case class Static() extends Decorator
case class Volatile() extends Decorator
case class Super() extends Decorator
case class Transient() extends Decorator
case class Strictfp() extends Decorator
case class Native() extends Decorator
case class NoDec() extends Decorator

/**
  * Expressions
  *
  * expr =
  *    BooleanLiT of Boolean
  *  | ByteLiT of Byte
  *  | CharLiT of Char
  *  | ShortLiT of Short
  *  | IntLit of Int
  *  | LongLit of Long
  *  | FloatLit of Float
  *  | DoubleLit of Double
  *  | NullLit
  *  | VoidLit
  *  | Binop of Expression * BinOp * Expression
  *  | Unop of UnOp * Expression
  *  | Id of String
  *  | Assign of Expression * Expression
  *  | FuncCall of String * List[Expression]
  *  | ArrayLit of List[Expression]
  *  | ArrayCreate of DataType * List[Expression]
  *  | ArrayAccess of Expression * List[Expression]
  *  | ObjCreate of String * List[Expression]
  *  | ObjAccess of Expression * Expression
  *  | This
  *  | NoExpr
  */
sealed trait Expression extends Term
case class BoolLit(value: Boolean) extends Expression
case class ByteLit(value: Byte) extends Expression
case class CharLit(value: Char) extends Expression
case class ShortLit(value: Short) extends Expression
case class IntLit(value: Int) extends Expression
case class LongLit(value: Long) extends Expression
case class FloatLit(value: Float) extends Expression
case class DoubleLit(value: Double) extends Expression
case class NullLit() extends Expression
case class VoidLit() extends Expression

case class Id(value: String) extends Expression

case class Binop(left: Expression, op: OpBinOp, right: Expression) extends Expression
case class Unop(op: OpUnOp, operand: Expression) extends Expression

case class Assign(dest: Expression, source: Expression) extends Expression
case class FuncCall(id: String, params: List[Expression]) extends Expression

case class ArrayLit(elem: List[Expression]) extends Expression
case class ArrayCreate(typ: Type, elem: List[Expression]) extends Expression
case class ArrayAccess(indx: Expression, elem: List[Expression]) extends Expression

case class ObjCreate(className: String, params: List[Expression]) extends Expression
case class ObjAccess(obj: Expression, call: Expression) extends Expression

case class This() extends Expression
case class NoExpr() extends Expression

/**
  * Statements
  *
  * Stmt =
  *    Block of List[Stmt]
  *  | Expr of Expression
  *  | Return of Expression
  *  | If of Expression * Stmt * Stmt
  *  | For of Expression * Expression * Expression * Stmt
  *  | While of Expression * Stmt
  *  | Break
  *  | Continue
  *  | Local of Datatype * String * Expression
  */
sealed trait Stmt extends Term
case class Block(block: List[Stmt]) extends Stmt
case class Expr(expr: Expression) extends Stmt
case class Return(expr: Expression) extends Stmt
case class If(cond: Expression, ifBranch: Stmt, elseBranch: Stmt) extends Stmt
case class For(start: Expression, to: Expression, at: Expression, body: Stmt) extends Stmt
case class While(cond: Expression, body: Stmt) extends Stmt
case class Break() extends Stmt
case class Continue() extends Stmt
case class Local(typ: Type, name: String, expr: Expression) extends Stmt

case class Field(
  scope: Scope,
  decorator: List[Decorator],
  typ: Type,
  name: String
)

case class FuncDecl(
  decorator:  List[Decorator],
  scope:      Scope,
  name:       FuncName,
  returnTyp:  Type,
  formals:    List[FuncFormal],
  body:       List[Stmt],
  rootClass:  Option[String]
)

case class ClassBody(fields: List[Field], constructors: List[FuncDecl], methods: List[FuncDecl])
case class ClassDecl(scope: Scope, name: String, extend: Extend, body: ClassBody)

case class Include(packages: List[String])

case class program(includes: List[Include], classes: List[ClassDecl])