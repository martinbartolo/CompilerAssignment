#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <string.h>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

using namespace llvm;
using namespace llvm::sys;

FILE *pFile;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns one of these for known things.
enum TOKEN_TYPE{

  IDENT = -1,        // [a-zA-Z_][a-zA-Z_0-9]*
  ASSIGN = int('='), // '='

  // delimiters
  LBRA = int('{'),  // left brace
  RBRA = int('}'),  // right brace
  LPAR = int('('),  // left parenthesis
  RPAR = int(')'),  // right parenthesis
  SC = int(';'),    // semicolon
  COMMA = int(','), // comma

  // types
  INT_TOK = -2,   // "int"
  VOID_TOK = -3,  // "void"
  FLOAT_TOK = -4, // "float"
  BOOL_TOK = -5,  // "bool"

  // keywords
  EXTERN = -6,  // "extern"
  IF = -7,      // "if"
  ELSE = -8,    // "else"
  WHILE = -9,   // "while"
  RETURN = -10, // "return"
  // TRUE   = -12,     // "true"
  // FALSE   = -13,     // "false"

  // literals
  INT_LIT = -14,   // [0-9]+
  FLOAT_LIT = -15, // [0-9]+.[0-9]+
  BOOL_LIT = -16,  // "true" or "false" key words

  // logical operators
  AND = -17, // "&&"
  OR = -18,  // "||"

  // operators
  PLUS = int('+'),    // addition or unary plus
  MINUS = int('-'),   // substraction or unary negative
  ASTERIX = int('*'), // multiplication
  DIV = int('/'),     // division
  MOD = int('%'),     // modular
  NOT = int('!'),     // unary negation

  // comparison operators
  EQ = -19,      // equal
  NE = -20,      // not equal
  LE = -21,      // less than or equal to
  LT = int('<'), // less than
  GE = -23,      // greater than or equal to
  GT = int('>'), // greater than

  // special tokens
  EOF_TOK = 0, // signal end of file

  // invalid
  INVALID = -100 // signal invalid token
};

// TOKEN struct is used to keep track of information about a token
struct TOKEN{
  int type = -100;
  std::string lexeme;
  int lineNo;
  int columnNo;
};

static std::string IdentifierStr; // Filled in ifIDENT
static int IntVal;                // Filled in ifINT_LIT
static bool BoolVal;              // Filled in ifBOOL_LIT
static float FloatVal;            // Filled in ifFLOAT_LIT
static std::string StringVal;     // Filled in ifString Literal
static int lineNo, columnNo;

static TOKEN returnTok(std::string lexVal, int tok_type){
  TOKEN return_tok;
  return_tok.lexeme = lexVal;
  return_tok.type = tok_type;
  return_tok.lineNo = lineNo;
  return_tok.columnNo = columnNo - lexVal.length() - 1;
  return return_tok;
}

// Read file line by line -- or look for \n and iffound add 1 to line number
// and reset column number to 0
/// gettok - Return the next token from standard input.
static TOKEN gettok(){

  static int LastChar = ' ';
  static int NextChar = ' ';

  // Skip any whitespace.
  while (isspace(LastChar)){
    if(LastChar == '\n' || LastChar == '\r'){
      lineNo++;
      columnNo = 1;
    }
    LastChar = getc(pFile);
    columnNo++;
  }

  if(isalpha(LastChar) ||
      (LastChar == '_')){ // identifier: [a-zA-Z_][a-zA-Z_0-9]*
    IdentifierStr = LastChar;
    columnNo++;

    while (isalnum((LastChar = getc(pFile))) || (LastChar == '_')){
      IdentifierStr += LastChar;
      columnNo++;
    }

    if(IdentifierStr == "int")
      return returnTok("int", INT_TOK);
    if(IdentifierStr == "bool")
      return returnTok("bool", BOOL_TOK);
    if(IdentifierStr == "float")
      return returnTok("float", FLOAT_TOK);
    if(IdentifierStr == "void")
      return returnTok("void", VOID_TOK);
    if(IdentifierStr == "bool")
      return returnTok("bool", BOOL_TOK);
    if(IdentifierStr == "extern")
      return returnTok("extern", EXTERN);
    if(IdentifierStr == "if")
      return returnTok("if", IF);
    if(IdentifierStr == "else")
      return returnTok("else", ELSE);
    if(IdentifierStr == "while")
      return returnTok("while", WHILE);
    if(IdentifierStr == "return")
      return returnTok("return", RETURN);
    if(IdentifierStr == "true"){
      BoolVal = true;
      return returnTok("true", BOOL_LIT);
    }
    if(IdentifierStr == "false"){
      BoolVal = false;
      return returnTok("false", BOOL_LIT);
    }

    return returnTok(IdentifierStr.c_str(), IDENT);
  }

  if(LastChar == '='){
    NextChar = getc(pFile);
    if(NextChar == '='){ // EQ: ==
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("==", EQ);
    } else{
      LastChar = NextChar;
      columnNo++;
      return returnTok("=", ASSIGN);
    }
  }

  if(LastChar == '{'){
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("{", LBRA);
  }
  if(LastChar == '}'){
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("}", RBRA);
  }
  if(LastChar == '('){
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("(", LPAR);
  }
  if(LastChar == ')'){
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(")", RPAR);
  }
  if(LastChar == ';'){
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(";", SC);
  }
  if(LastChar == ','){
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(",", COMMA);
  }

  if(isdigit(LastChar) || LastChar == '.'){ // Number: [0-9]+.
    std::string NumStr;

    if(LastChar == '.'){ // Floatingpoint Number: .[0-9]+
      do{
        NumStr += LastChar;
        LastChar = getc(pFile);
        columnNo++;
      } while (isdigit(LastChar));

      FloatVal = strtof(NumStr.c_str(), nullptr);
      return returnTok(NumStr, FLOAT_LIT);
    } else{
      do{ // Start of Number: [0-9]+
        NumStr += LastChar;
        LastChar = getc(pFile);
        columnNo++;
      } while (isdigit(LastChar));

      if(LastChar == '.'){ // Floatingpoint Number: [0-9]+.[0-9]+)
        do{
          NumStr += LastChar;
          LastChar = getc(pFile);
          columnNo++;
        } while (isdigit(LastChar));

        FloatVal = strtof(NumStr.c_str(), nullptr);
        return returnTok(NumStr, FLOAT_LIT);
      } else{ // Integer : [0-9]+
        IntVal = strtod(NumStr.c_str(), nullptr);
        return returnTok(NumStr, INT_LIT);
      }
    }
  }

  if(LastChar == '&'){
    NextChar = getc(pFile);
    if(NextChar == '&'){ // AND: &&
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("&&", AND);
    } else{
      LastChar = NextChar;
      columnNo++;
      return returnTok("&", int('&'));
    }
  }

  if(LastChar == '|'){
    NextChar = getc(pFile);
    if(NextChar == '|'){ // OR: ||
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("||", OR);
    } else{
      LastChar = NextChar;
      columnNo++;
      return returnTok("|", int('|'));
    }
  }

  if(LastChar == '!'){
    NextChar = getc(pFile);
    if(NextChar == '='){ // NE: !=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("!=", NE);
    } else{
      LastChar = NextChar;
      columnNo++;
      return returnTok("!", NOT);
      ;
    }
  }

  if(LastChar == '<'){
    NextChar = getc(pFile);
    if(NextChar == '='){ // LE: <=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("<=", LE);
    } else{
      LastChar = NextChar;
      columnNo++;
      return returnTok("<", LT);
    }
  }

  if(LastChar == '>'){
    NextChar = getc(pFile);
    if(NextChar == '='){ // GE: >=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok(">=", GE);
    } else{
      LastChar = NextChar;
      columnNo++;
      return returnTok(">", GT);
    }
  }

  if(LastChar == '/'){ // could be division or could be the start of a comment
    LastChar = getc(pFile);
    columnNo++;
    if(LastChar == '/'){ // definitely a comment
      do{
        LastChar = getc(pFile);
        columnNo++;
      } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

      if(LastChar != EOF)
        return gettok();
    } else
      return returnTok("/", DIV);
  }

  // Check for end of file.  Don't eat the EOF.
  if(LastChar == EOF){
    columnNo++;
    return returnTok("0", EOF_TOK);
  }

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  std::string s(1, ThisChar);
  LastChar = getc(pFile);
  columnNo++;
  return returnTok(s, int(ThisChar));
}

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static TOKEN CurTok;
static TOKEN NextTok;
static TOKEN NextNextTok;
static std::deque<TOKEN> tok_buffer;

static TOKEN getNextToken(){
  CurTok = NextTok;
  NextTok = NextNextTok;

  if(tok_buffer.size() == 0)
    tok_buffer.push_back(gettok());

  TOKEN temp = tok_buffer.front();
  tok_buffer.pop_front();

  return NextNextTok = temp;
}

static void putBackToken(TOKEN tok){ tok_buffer.push_front(tok); }

//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

///Base class for all AST nodes.
class ASTnode{
public:
  virtual ~ASTnode(){};
  virtual Value *codegen() = 0;
  virtual std::string to_string() const{
    std::cout << "AST node";
    return "";
  };
};

//AST node for Integer Literals
class IntLitASTnode : public ASTnode{
  int Val;
  TOKEN Tok;

public:
  IntLitASTnode(TOKEN tok, int val) : Tok(tok), Val(val){}
  virtual Value *codegen() override;

  virtual std::string to_string() const override{
    std::string s = "";
    s += "INT_LIT: ";
    s += std::to_string(Val);
    return s;
  };
};

//AST node for Float Literals
class FloatLitASTnode : public ASTnode{
  float Val;
  TOKEN Tok;

public:
  FloatLitASTnode(TOKEN tok, float val) : Tok(tok), Val(val){}
  virtual Value *codegen() override;

  virtual std::string to_string() const override{
    std::string s = "";
    s += "FLOAT_LIT: ";
    s += std::to_string(Val);
    return s;
  };
};

//AST node for Bool Literals
class BoolLitASTnode : public ASTnode{
  bool Val;
  TOKEN Tok;

public:
  BoolLitASTnode(TOKEN tok, bool val) : Tok(tok), Val(val){}
  virtual Value *codegen() override;

  virtual std::string to_string() const override{
    std::string s = "";
    s += "BOOL_LIT: ";
    s += std::to_string(Val);
    return s;
  };
};

//AST node for Literal Terms
class LiteralsASTnode : public ASTnode{
  std::unique_ptr<ASTnode> Literal;

public:
  LiteralsASTnode(std::unique_ptr<IntLitASTnode> literal) :Literal(std::move(literal)){}
  LiteralsASTnode(std::unique_ptr<FloatLitASTnode> literal) :Literal(std::move(literal)){}
  LiteralsASTnode(std::unique_ptr<BoolLitASTnode> literal) :Literal(std::move(literal)){}

  virtual Value *codegen() override;

  virtual std::string to_string() const override{
    return Literal->to_string();
  };
};

//AST node for Identifiers
class IdentASTnode : public ASTnode{
  std::string Val;
  TOKEN Tok;

public:
  IdentASTnode(TOKEN tok, std::string val) : Tok(tok), Val(val){}

  std::string getVal(){
    return Val;
  }

  virtual Value *codegen() override;

  virtual std::string to_string() const override{
    return Val;
  };
};

//AST node for Types
class TypesASTnode : public ASTnode{
  TOKEN Tok;

public:
  TypesASTnode(TOKEN tok) : Tok(tok){}

  int getType(){
    return Tok.type;
  }

  virtual Value *codegen() override{
    return nullptr;
  };

  virtual std::string to_string() const override{
    if(Tok.type == INT_TOK){
      return "TYPE_INT";
    }
    else if(Tok.type == VOID_TOK){
      return "TYPE_VOID";
    }
    else if(Tok.type == FLOAT_TOK){
      return "TYPE_FLOAT";
    }
    else if(Tok.type == BOOL_TOK){
      return "TYPE_BOOL";
    }
    else{
      return "";
    }
  };
};

//AST node for Variable Declarations
class VarDeclsASTnode : public ASTnode{
  std::unique_ptr<TypesASTnode> Type;
  std::unique_ptr<IdentASTnode> Identifier;

public:
  VarDeclsASTnode(std::unique_ptr<TypesASTnode> type, std::unique_ptr<IdentASTnode> identifier) :
  Type(std::move(type)), Identifier(std::move(identifier)){}

  int getType(){
    return Type->getType();
  }

  std::string getName(){
    return Identifier->getVal();
  }

  virtual Value *codegen() override;

  virtual std::string to_string() const override{
    std::string s = "VARIABLE: VARIABLE_TYPE: ";
    s += Type->to_string();
    s += " VARIABLE_NAME: ";
    s += Identifier->to_string();
    s += " ";
    return s;
  };
};

//AST node for List of Variables
class VarListASTnode : public ASTnode{
public:
  std::vector<std::unique_ptr<VarDeclsASTnode>> Variables;
  VarListASTnode(){};
  VarListASTnode(std::vector<std::unique_ptr<VarDeclsASTnode>> variables) : Variables(std::move(variables)){}

  void addToParams(std::unique_ptr<VarDeclsASTnode> variable){
    Variables.push_back(std::move(variable));
  }

  virtual Value *codegen() override{
    return nullptr;
  };

  virtual std::string to_string() const override{
    std::string s = "\nPARAMS: ";
    if((int)Variables.size() == 0){
      s += "VOID_TOK";
    }
    else{
      for (int i = 0; i < (int)Variables.size(); i++){
        s += Variables[i]->to_string();
      }
    }
    return s;
  };
};

//AST node for Function Calls
class FunctionCallASTnode : public ASTnode{
  std::unique_ptr<IdentASTnode> Identifier;
  std::vector<std::unique_ptr<ASTnode>> Arguments;

public:
  bool isVar = false;
  FunctionCallASTnode(std::unique_ptr<IdentASTnode> identifier, std::vector<std::unique_ptr<ASTnode>> arguments) :
  Identifier(std::move(identifier)), Arguments(std::move(arguments)){}
  FunctionCallASTnode(std::unique_ptr<IdentASTnode> identifier) : Identifier(std::move(identifier)){ isVar = true; }

  virtual Value *codegen() override;

  virtual std::string to_string() const override{
    std::string s = "";
    if(isVar){
      s += "VARIABLE_CALL: ";
      s += "Variable_Name: ";
      s += Identifier->to_string();
    }
    else{
      s += "\nFUNCTION_CALL: ";
      s += "FUNCTION_NAME: ";
      s += Identifier->to_string();
      s += " ARGUMENTS: ";
      for (int i = 0; i < (int)Arguments.size(); i++){
        s += Arguments[i]->to_string();
      }
    }
    return s;
  };
};

//AST node for Assignment Expressions
class AssExprASTnode : public ASTnode{
  std::unique_ptr<IdentASTnode> Identifier;
  std::unique_ptr<ASTnode> Expression;

public:
  AssExprASTnode(std::unique_ptr<IdentASTnode> identifier, std::unique_ptr<ASTnode> expression) :
  Identifier(std::move(identifier)), Expression(std::move(expression)){}

  virtual Value *codegen() override;

  virtual std::string to_string() const override{
    std::string s = "\nASSIGNMENT_EXPR:  ";
    s += "VARIABLE NAME: ";
    s += Identifier->to_string();
    s += " VALUE: ";
    s += Expression->to_string();
    return s;
  };
};

//AST node for Binary Operator Expressions
class BinOpExprASTnode : public ASTnode{

public:
  TOKEN Op;
  std::unique_ptr<ASTnode> LHS;
  std::unique_ptr<ASTnode> RHS;

  BinOpExprASTnode(TOKEN op) : Op(op){}
  BinOpExprASTnode(TOKEN op, std::unique_ptr<ASTnode> lhs, std::unique_ptr<ASTnode> rhs) :
  Op(op), LHS(std::move(lhs)), RHS(std::move(rhs)){}

  virtual Value *codegen() override;

  virtual std::string to_string() const override{
    std::string s = " BINARY_OPERATOR_EXPR: ";
    s += LHS->to_string();
    s += " Operation : ";
    s += Op.lexeme;
    s += " ";
    s += RHS->to_string();
    return s;
  };
};

//AST node for Unary Operator Expressions
class UnaryOpExprASTnode : public ASTnode{
  TOKEN Op;
  std::unique_ptr<ASTnode> Val;

public:
  UnaryOpExprASTnode(TOKEN op, std::unique_ptr<ASTnode> val) : Op(op), Val(std::move(val)){}

  virtual Value *codegen() override;

  virtual std::string to_string() const override{
    std::string s = "UNARY_OPERATOR_EXPR: ";
    s += Op.lexeme;
    s += " VALUE: ";
    s += Val->to_string();
    return s;
  };
};

//AST node for Black of Statements
class BlockASTnode : public ASTnode{
  std::vector<std::unique_ptr<VarDeclsASTnode>> LocalDecls;
  std::vector<std::unique_ptr<ASTnode>> Statements;

public:
  BlockASTnode(std::vector<std::unique_ptr<VarDeclsASTnode>> localdecls, std::vector<std::unique_ptr<ASTnode>> statements) :
  LocalDecls(std::move(localdecls)), Statements(std::move(statements)){}

  virtual Value *codegen() override;

  virtual std::string to_string() const override{
    std::string s = "";
    if((int)LocalDecls.size() > 0){
      s += "\nLOCAL VARIABLE DECLARATIONS: \n";
      for (int i = 0; i < (int)LocalDecls.size(); i++){
        s += LocalDecls[i]->to_string();
        s += "\n";
      }
    }
    s += "STATEMENTS: ";
    for (int i = 0; i < (int)Statements.size(); i++){
      s += Statements[i]->to_string();
    }
    return s;
  };
};

//AST node for While Statement
class WhileStmtASTnode : public ASTnode{
  std::unique_ptr<ASTnode> Expression;
  std::unique_ptr<ASTnode> Statement;

public:
  WhileStmtASTnode(std::unique_ptr<ASTnode> expression, std::unique_ptr<ASTnode> statement) :
  Expression(std::move(expression)), Statement(std::move(statement)){}

  virtual Value *codegen() override;

  virtual std::string to_string() const override{
    std::string s = "\nWHILE_STATEMENT:\nCONDITION: ";
    s += Expression->to_string();
    s += "\nBODY: ";
    s += Statement->to_string();
    return s;
  };
};

//AST node to Build If Statement - Will be used by IfASTnode()
class IfBuilderASTnode : public ASTnode{
  std::unique_ptr<BlockASTnode> IfStmt;
  std::unique_ptr<BlockASTnode> ElseStmt;

public:
  IfBuilderASTnode(std::unique_ptr<BlockASTnode> ifStmt, std::unique_ptr<BlockASTnode> elseStmt) :
  IfStmt(std::move(ifStmt)), ElseStmt(std::move(elseStmt)){}

  std::unique_ptr<BlockASTnode> getIfStmt(){
    return std::move(IfStmt);
  }

  std::unique_ptr<BlockASTnode> getElseStmt(){
    return std::move(ElseStmt);
  }

  virtual Value *codegen() override{
    return nullptr;
  };

  virtual std::string to_string() const override{
    std::string s = "";
    s += IfStmt->to_string();
    s += "\nELSE_STATEMENT: ";
    s += ElseStmt->to_string();
    return s;
  };
};

//AST node for If Statement
class IfStmtASTnode : public ASTnode{
  std::unique_ptr<ASTnode> Expression;
  std::unique_ptr<IfBuilderASTnode> IfElseStmts;

public:
  IfStmtASTnode(std::unique_ptr<ASTnode> expression, std::unique_ptr<IfBuilderASTnode> ifelseStmt) :
  Expression(std::move(expression)), IfElseStmts(std::move(ifelseStmt)){}

  virtual Value *codegen() override;

  virtual std::string to_string() const override{
    std::string s = "\nIF_STATEMENT:\nCONDITION: ";
    s += Expression->to_string();
    s += "\nBODY:";
    s += IfElseStmts->to_string();
    return s;
  };
};

//AST node for Return Statement
class ReturnStmtASTnode : public ASTnode{
  std::unique_ptr<ASTnode> Expression;

public:
  ReturnStmtASTnode(std::unique_ptr<ASTnode> expression) : Expression(std::move(expression)){}
  ReturnStmtASTnode(){}

  virtual Value *codegen() override;

  virtual std::string to_string() const override{
    std::string s = "\nRETURN_STMT: ";
    if(Expression != nullptr){
      s += Expression->to_string();
    }
    return s;
  };
};

//AST node for Building Functions
class FunctionBuilderASTnode : public ASTnode{
  std::unique_ptr<TypesASTnode> ReturnType;
  std::unique_ptr<IdentASTnode> Identifier;
  std::unique_ptr<VarListASTnode> Parameters;

public:
  FunctionBuilderASTnode(std::unique_ptr<TypesASTnode> returnType, std::unique_ptr<IdentASTnode> identifier, std::unique_ptr<VarListASTnode> parameters) :
  ReturnType(std::move(returnType)), Identifier(std::move(identifier)), Parameters(std::move(parameters)){}

  virtual Function *codegen() override;

  std::string getVal(){
    return Identifier->getVal();
  }

  virtual std::string to_string() const override{
    std::string s = "FUNCTION_DECLARATION: RETURN_TYPE: ";
    s += ReturnType->to_string();
    s += " FUNCTION_NAME: ";
    s += Identifier->to_string();
    if(Parameters != nullptr){
      s += Parameters->to_string();
    }
    return s;
  };
};

//AST node for Functions
class FunctionASTnode : public ASTnode{
  std::unique_ptr<FunctionBuilderASTnode> FBuilder;
  std::unique_ptr<BlockASTnode> Statements;

public:
  FunctionASTnode(std::unique_ptr<FunctionBuilderASTnode> builder, std::unique_ptr<BlockASTnode> statements) :
  FBuilder(std::move(builder)), Statements(std::move(statements)){}

  virtual Function *codegen() override;

  virtual std::string to_string() const override{
    std::string s = "\nFUNCTION:\n";
    s += FBuilder->to_string();
    s += "\nBODY: ";
    if(Statements != nullptr){
      s += Statements->to_string();
    }
    return s;
  };
};

//AST node for Program
class ProgramASTnode : public ASTnode{
  std::vector<std::unique_ptr<FunctionBuilderASTnode>> ExtList;
  std::vector<std::unique_ptr<ASTnode>> DeclList;

public:
  virtual ~ProgramASTnode(){};
  ProgramASTnode(std::vector<std::unique_ptr<FunctionBuilderASTnode>> extList, std::vector<std::unique_ptr<ASTnode>> declList) :
  ExtList(std::move(extList)), DeclList(std::move(declList)){}
  ProgramASTnode(std::vector<std::unique_ptr<ASTnode>> declList) :DeclList(std::move(declList)){}

  virtual Value *codegen() override;

  virtual std::string to_string() const override{
    std::string s = "AST NODES: \n\nPROGRAM: \n\nEXTERN_LIST:\n";
    for (int i = 0; i < (int)ExtList.size(); i++){
      s += ExtList[i]->to_string();
      s += "\n";
    }
    s += "\nDECLARATIONS_LIST:";

    for (int i = 0; i < (int)DeclList.size(); i++){
      s += DeclList[i]->to_string();
      s += "\n";
    }
    return s;
  };
};

//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//

static bool err_flag = false;

//displays error message
static void showErr(std::string s){
  fprintf(stderr, "Unexpected Token %s at Row %d and Column %d \n", CurTok.lexeme.c_str(), CurTok.lineNo, CurTok.columnNo);
  fprintf(stderr, "%s\n", s.c_str());
  err_flag = true;
}

static void showCodeGenErr(const char *s){
  fprintf(stderr, "Error in Code Generation: %s", s);
}

//matches token and returns false iferror
static bool match(int type){
  if(CurTok.type == type){
    return true;
  }
  return false;
}

//First and Follow sets for every non-terminal

static bool firstProgram(){
  return(CurTok.type==EXTERN || CurTok.type==VOID_TOK || CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK);
}

static bool followProgram(){
  return(CurTok.type==EOF_TOK);
}

static bool firstExternList(){
  return(CurTok.type==EXTERN);
}

static bool followExternList(){
  return(CurTok.type==VOID_TOK || CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK);
}

static bool firstplusExternListSub(){
  return(CurTok.type==EXTERN || followExternList());
}

static bool followExternListSub(){
  return(CurTok.type==VOID_TOK || CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK);
}

static bool firstExtern(){
  return(CurTok.type==EXTERN);
}

static bool followExtern(){
  return(CurTok.type == EXTERN || CurTok.type==VOID_TOK || CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK);
}

static bool firstDeclList(){
  return(CurTok.type==VOID_TOK || CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK);
}

static bool followDeclList(){
  return(CurTok.type==EOF_TOK);
}

static bool firstplusDeclListSub(){
  return(CurTok.type==VOID_TOK || CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK || followDeclList());
}

static bool followDeclListSub(){
  return(CurTok.type==EOF_TOK);
}

static bool firstDecl(){
  return(CurTok.type==VOID_TOK || CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK);
}

static bool followDecl(){
  return(CurTok.type==VOID_TOK || CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK || CurTok.type==EOF_TOK);
}

static bool firstVarDecl(){
  return(CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK);
}

static bool followVarDecl(){
  return(CurTok.type==VOID_TOK || CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK || CurTok.type==EOF_TOK);
}

static bool firstFunDecl(){
  return(CurTok.type==VOID_TOK || CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK);
}

static bool followFunDecl(){
  return(CurTok.type==VOID_TOK || CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK || CurTok.type==EOF_TOK);
}

static bool firstTypeSpec(){
  return(CurTok.type==VOID_TOK || CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK);
}

static bool followTypeSpec(){
  return(CurTok.type==IDENT);
}

static bool firstVarType(){
  return(CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK);
}

static bool followVarType(){
  return(CurTok.type==IDENT);
}

static bool followParams(){
  return(CurTok.type==RPAR);
}

static bool firstplusParams(){
  return(CurTok.type==VOID_TOK || CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK || followParams());
}

static bool firstParamList(){
  return(CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK);
}

static bool followParamList(){
  return(CurTok.type==RPAR);
}

static bool firstplusParamListSub(){
  return(CurTok.type==COMMA || followParamList());
}

static bool followParamListSub(){
  return(CurTok.type==RPAR);
}

static bool firstParam(){
  return(CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK);
}

static bool followParam(){
  return(CurTok.type==RPAR || CurTok.type==COMMA);
}

static bool firstBlock(){
  return(CurTok.type==LBRA);
}

static bool followBlock(){
  return(CurTok.type==EOF_TOK || CurTok.type==IDENT || CurTok.type==SC || CurTok.type==VOID_TOK || CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK || CurTok.type==LBRA || CurTok.type==WHILE || CurTok.type==IF || CurTok.type== ELSE || CurTok.type==RETURN
  || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==BOOL_LIT || CurTok.type==FLOAT_LIT);
}

static bool followLocalDecls(){
  return(CurTok.type==IDENT || CurTok.type==SC || CurTok.type==LBRA ||CurTok.type==WHILE || CurTok.type==IF || CurTok.type==RETURN || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==BOOL_LIT || CurTok.type==FLOAT_LIT);
}

static bool firstplusLocalDecls(){
  return(CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK || followLocalDecls());
}

static bool firstLocalDecl(){
  return(CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK);
}

static bool followLocalDecl(){
  return(CurTok.type==INT_TOK || CurTok.type==FLOAT_TOK || CurTok.type==BOOL_TOK || CurTok.type==IDENT || CurTok.type==SC || CurTok.type==LBRA ||CurTok.type==WHILE || CurTok.type==IF || CurTok.type==RETURN || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR
  || CurTok.type==INT_LIT || CurTok.type==BOOL_LIT || CurTok.type==FLOAT_LIT);
}

static bool followStmtList(){
  return(CurTok.type==RBRA);
}

static bool firstplusStmtList(){
  return(CurTok.type==IDENT || CurTok.type==SC || CurTok.type==LBRA || CurTok.type==WHILE || CurTok.type==IF || CurTok.type==RETURN || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==BOOL_LIT || CurTok.type==FLOAT_LIT || followStmtList());
}

static bool firstStmt(){
  return(CurTok.type==IDENT || CurTok.type==SC || CurTok.type==LBRA || CurTok.type==WHILE || CurTok.type==IF || CurTok.type==RETURN || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==BOOL_LIT || CurTok.type==FLOAT_LIT);
}

static bool followStmt(){
  return(CurTok.type==IDENT || CurTok.type==SC || CurTok.type==LBRA || CurTok.type==WHILE || CurTok.type==IF || CurTok.type==RETURN || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==BOOL_LIT || CurTok.type==FLOAT_LIT || CurTok.type==RBRA);
}

static bool firstExprStmt(){
  return(CurTok.type==IDENT || CurTok.type==SC || CurTok.type==LBRA || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==BOOL_LIT || CurTok.type==FLOAT_LIT);
}

static bool followExprStmt(){
  return(CurTok.type==IDENT || CurTok.type==SC || CurTok.type==LBRA || CurTok.type==WHILE || CurTok.type==IF || CurTok.type==RETURN || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==BOOL_LIT || CurTok.type==FLOAT_LIT || CurTok.type==RBRA);
}

static bool firstWhileStmt(){
  return(CurTok.type==WHILE);
}

static bool followWhileStmt(){
  return(CurTok.type==IDENT || CurTok.type==SC || CurTok.type==LBRA || CurTok.type==WHILE || CurTok.type==IF || CurTok.type==RETURN || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==BOOL_LIT || CurTok.type==FLOAT_LIT || CurTok.type==RBRA);
}

static bool firstIfStmt(){
  return(CurTok.type==IF);
}

static bool followIfStmt(){
  return(CurTok.type==IDENT || CurTok.type==SC || CurTok.type==LBRA || CurTok.type==WHILE || CurTok.type==IF || CurTok.type==RETURN || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==BOOL_LIT || CurTok.type==FLOAT_LIT || CurTok.type==RBRA);
}

static bool firstplusElseStmt(){
  return(CurTok.type==ELSE || followIfStmt());
}

static bool followElseStmt(){
  return(CurTok.type==IDENT || CurTok.type==SC || CurTok.type==LBRA || CurTok.type==WHILE || CurTok.type==IF || CurTok.type==RETURN || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==BOOL_LIT || CurTok.type==FLOAT_LIT || CurTok.type==RBRA);
}

static bool firstReturnStmt(){
  return(CurTok.type==RETURN);
}

static bool followReturnStmt(){
  return(CurTok.type==IDENT || CurTok.type==SC || CurTok.type==LBRA || CurTok.type==WHILE || CurTok.type==IF || CurTok.type==RETURN || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==BOOL_LIT || CurTok.type==FLOAT_LIT || CurTok.type==RBRA);
}

static bool firstReturnStmtSub(){
  return(CurTok.type==IDENT || CurTok.type==SC || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==BOOL_LIT || CurTok.type==FLOAT_LIT);
}

static bool followReturnStmtSub(){
  return(CurTok.type==IDENT || CurTok.type==SC || CurTok.type==LBRA || CurTok.type==WHILE || CurTok.type==IF || CurTok.type==RETURN || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==BOOL_LIT || CurTok.type==FLOAT_LIT || CurTok.type==RBRA);
}

static bool firstExpr(){
  return(CurTok.type==IDENT || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==BOOL_LIT || CurTok.type==FLOAT_LIT);
}

static bool followExpr(){
  return(CurTok.type==IDENT || CurTok.type==SC || CurTok.type==COMMA || CurTok.type==RPAR || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==BOOL_LIT || CurTok.type==FLOAT_LIT);
}

static bool firstOrTerm(){
  return(CurTok.type==IDENT || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==BOOL_LIT || CurTok.type==FLOAT_LIT);
}

static bool followOrTerm(){
  return(CurTok.type==IDENT || CurTok.type==SC || CurTok.type==COMMA || CurTok.type==RPAR || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==BOOL_LIT || CurTok.type==FLOAT_LIT || CurTok.type==OR);
}

static bool firstplusOrTermSub(){
  return(CurTok.type==OR || followOrTerm());
}

static bool followOrTermSub(){
  return(followOrTerm());
}

static bool firstAndTerm(){
  return(firstOrTerm());
}

static bool followAndTerm(){
  return(followOrTerm() || CurTok.type==AND);
}

static bool firstplusAndTermSub(){
  return(CurTok.type==AND || followAndTerm());
}

static bool followAndTermSub(){
  return(followAndTerm());
}

static bool firstEqualityTerm(){
  return(firstAndTerm());
}

static bool followEqualityTerm(){
  return(followAndTerm() || CurTok.type==EQ || CurTok.type==NE);
}

static bool firstplusEqualityTermSub(){
  return(CurTok.type==EQ || CurTok.type ==NE || followEqualityTerm());
}

static bool followEqualityTermSub(){
  return(followEqualityTerm());
}

static bool firstComparisonTerm(){
  return(firstEqualityTerm());
}

static bool followComparisonTerm(){
  return(followEqualityTerm() || CurTok.type==GE || CurTok.type==GT || CurTok.type==LE || CurTok.type==LT);
}

static bool firstplusComparisonTermSub(){
  return(CurTok.type==GE || CurTok.type==GT || CurTok.type==LE || CurTok.type==LT || followComparisonTerm());
}

static bool followComparisonTermSub(){
  return(followComparisonTerm());
}

static bool firstAddSubtTerm(){
  return(firstComparisonTerm());
}

static bool followAddSubtTerm(){
  return(followComparisonTerm() || CurTok.type==PLUS);
}

static bool firstplusAddSubtTermSub(){
  return(CurTok.type==PLUS || CurTok.type==MINUS || followAddSubtTerm());
}

static bool followAddSubtTermSub(){
  return(followComparisonTerm());
}

static bool firstMultDivModTerm(){
  return(firstAddSubtTerm());
}

static bool followMultDivModTerm(){
  return(followAddSubtTerm() || CurTok.type==ASTERIX || CurTok.type==DIV || CurTok.type==MOD);
}

static bool firstplusMultDivModTermSub(){
  return(CurTok.type==ASTERIX || CurTok.type==DIV || CurTok.type==MOD || followMultDivModTerm());
}

static bool followMultDivModTermSub(){
  return(followMultDivModTerm());
}

static bool firstNegNotTerm(){
  return(CurTok.type==IDENT || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==FLOAT_LIT || CurTok.type==BOOL_LIT);
}

static bool followNegNotTerm(){
  return(followMultDivModTerm());
}

static bool firstParenTerm(){
  return(CurTok.type==IDENT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==FLOAT_LIT || CurTok.type==BOOL_LIT);
}

static bool followParenTerm(){
  return(followNegNotTerm());
}

static bool firstIdentTerm(){
  return(CurTok.type==IDENT || CurTok.type==INT_LIT || CurTok.type==FLOAT_LIT || CurTok.type==BOOL_LIT);
}

static bool followIdentTerm(){
  return(followParenTerm());
}

static bool firstplusIdentTermSub(){
  return(CurTok.type==LPAR || followIdentTerm());
}

static bool followIdentTermSub(){
  return(followIdentTerm());
}

static bool firstLitTerm(){
  return(CurTok.type==INT_LIT || CurTok.type==FLOAT_LIT || CurTok.type==BOOL_LIT);
}

static bool followLitTerm(){
  return(followIdentTerm());
}

static bool followArgList(){
  return(CurTok.type==RPAR);
}

static bool firstplusArgList(){
  return(CurTok.type==IDENT || CurTok.type==MINUS || CurTok.type==NOT || CurTok.type==LPAR || CurTok.type==INT_LIT || CurTok.type==FLOAT_LIT || CurTok.type==BOOL_LIT || followArgList());
}

static bool firstplusArgListSub(){
  return(CurTok.type==COMMA || followArgList());
}

static bool followArgListSub(){
  return(CurTok.type==RPAR);
}

//Required function declarations
static std::unique_ptr<BlockASTnode> Block();
static std::unique_ptr<ASTnode> Expr();
static std::unique_ptr<ASTnode> Stmt();
static std::unique_ptr<ASTnode> Or_Term();
static std::unique_ptr<ASTnode> And_Term();
static std::unique_ptr<ASTnode> Equality_Term();
static std::unique_ptr<ASTnode> Comparison_Term();
static std::unique_ptr<ASTnode> Add_Subt_Term();
static std::unique_ptr<ASTnode> Mult_Div_Mod_Term();

// arg_list ::= "," expr arg_list_sub | epsilon
static std::vector<std::unique_ptr<ASTnode>> Arg_List_Sub(){
  std::vector<std::unique_ptr<ASTnode>> list;
  std::vector<std::unique_ptr<ASTnode>> nullVector;

  if(err_flag || !firstplusArgListSub()){
    showErr("Expected COMMA, RPAR.");
    return nullVector;
  }

  if(CurTok.type == COMMA){
    if(match(COMMA)){
      getNextToken();
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type COMMA.";
      showErr(s);
    }

    auto expr = Expr();
    auto arg_list_sub = Arg_List_Sub();

    if(expr != nullptr){
      list.push_back(std::move(expr));
    }
    for (int i = 0; i < (int)arg_list_sub.size(); i++){
      list.push_back(std::move(arg_list_sub[i]));
    }
  }
  else{
    if(err_flag || !followArgListSub()){
      showErr("1Expected RPAR.");
      return nullVector;
    }
  }
  return list;
}

// arg_list ::= expr arg_list_sub | epsilon
static std::vector<std::unique_ptr<ASTnode>> Arg_List(){
  std::vector<std::unique_ptr<ASTnode>> list;
  std::vector<std::unique_ptr<ASTnode>> nullVector;

  if(err_flag || !firstplusArgList()){
    showErr("Expected an identifier, MINUS, NOT, LPAR or a literal.");
    return nullVector;
  }

  auto expr = Expr();
  auto arg_list_sub = Arg_List_Sub();

  if(expr != nullptr){
    list.push_back(std::move(expr));
  }
  for (int i = 0; i < (int)arg_list_sub.size(); i++){
    list.push_back(std::move(arg_list_sub[i]));
  }

  if(err_flag || !followArgList()){
    showErr("Expected RPAR.");
    return nullVector;
  }
  return list;
}

// lit_term ::= INT_LIT | BOOL_LIT | FLOAT_LIT
static std::unique_ptr<LiteralsASTnode> Lit_Term(){
  if(err_flag || !firstLitTerm()){
    showErr("Expected a literal.");
    return nullptr;
  }

  if(CurTok.type == INT_LIT){
    if(match(INT_LIT)){
      TOKEN LitTok = CurTok;
      getNextToken();
      auto intlit = make_unique<IntLitASTnode>(LitTok, std::stoi(LitTok.lexeme, nullptr, 10));
      return make_unique<LiteralsASTnode>(std::move(intlit));
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type INT_LIT.";
      showErr(s);
    }
  }
  else if(CurTok.type == BOOL_LIT){
    if(match(BOOL_LIT)){
      bool val = false;

      TOKEN LitTok = CurTok;
      getNextToken();
      if(LitTok.lexeme == "true"){
        val = true;
      }

      auto boollit = make_unique<BoolLitASTnode>(LitTok, val);
      return make_unique<LiteralsASTnode>(std::move(boollit));
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type BOOL_LIT.";
      showErr(s);
    }
  }
  else if(CurTok.type == FLOAT_LIT){
    if(match(FLOAT_LIT)){
      TOKEN LitTok = CurTok;
      getNextToken();
      auto floatlit = make_unique<FloatLitASTnode>(LitTok, std::stof(LitTok.lexeme,nullptr));
      return make_unique<LiteralsASTnode>(std::move(floatlit));
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type FLOAT_LIT.";
      showErr(s);
    }
  }
  return nullptr;
}

// ident_term_sub ::= "(" arg_list ")" | epsilon
static std::vector<std::unique_ptr<ASTnode>> Ident_Term_Sub(){
  std::vector<std::unique_ptr<ASTnode>> list;
  std::vector<std::unique_ptr<ASTnode>> nullVector;

  if(err_flag || !firstplusIdentTermSub()){
    showErr("ASTERIX, DIV, MOD, PLUS, GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
    return nullVector;
  }

  if(CurTok.type == LPAR){
    if(match(LPAR)){
      getNextToken();
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type LPAR.";
      showErr(s);
    }

    auto arg_list = Arg_List();

    if(match(RPAR)){
      getNextToken();
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type RPAR.";
      showErr(s);
    }

    if(err_flag || !followIdentTermSub()){
      showErr("Expected ASTERIX, DIV, MOD, PLUS, GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
      return nullVector;
    }
    return arg_list;
  }
  return list;
}

// ident_term ::= IDENT ident_term_sub | lit_term
static std::unique_ptr<ASTnode> Ident_Term(){
  bool matched = false;

  if(err_flag || !firstIdentTerm()){
    showErr("Expected an identifier or a literal.");
    return nullptr;
  }

  if(CurTok.type == IDENT){
    if(!match(IDENT)){
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type IDENT.";
      showErr(s);
    }
    else{
      matched = true;
    }

    auto ident = make_unique<IdentASTnode>(CurTok, CurTok.lexeme);
    if(matched){
      getNextToken();
    }

    auto ident_term_sub = Ident_Term_Sub();

    if(err_flag || !followIdentTerm()){
      showErr("Expected ASTERIX, DIV, MOD, PLUS, GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
      return nullptr;
    }

    if(ident_term_sub.size() > 0){
      return make_unique<FunctionCallASTnode>(std::move(ident), std::move(ident_term_sub));
    }
    else{
      return make_unique<FunctionCallASTnode>(std::move(ident));
    }
  }
  else{
    if(err_flag || !followIdentTerm()){
      showErr("Expected ASTERIX, DIV, MOD, PLUS, GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
      return nullptr;
    }
    return Lit_Term();
  }
}

// paren_term ::= "(" expr ")" | ident_term
static std::unique_ptr<ASTnode> Paren_Term(){
  if(err_flag || !firstParenTerm()){
    showErr("Expected an identifier, LPAR or a literal.");
    return nullptr;
  }

  if(CurTok.type == LPAR){
    if(match(LPAR)){
      getNextToken();
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type LPAR.";
      showErr(s);
    }

    auto expr = Expr();

    if(match(RPAR)){
      getNextToken();
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type RPAR.";
      showErr(s);
    }

    if(err_flag || !followParenTerm()){
      showErr("Expected ASTERIX, DIV, MOD, PLUS, GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
      return nullptr;
    }
    return expr;
  }
  else{
    if(err_flag || !followParenTerm()){
      showErr("Expected ASTERIX, DIV, MOD, PLUS, GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
      return nullptr;
    }
    return Ident_Term();
  }
}

// neg_not_term ::= "-" neg_not_term | "!" neg_not_term | paren_term
static std::unique_ptr<ASTnode> Neg_Not_Term(){
  if(err_flag || !firstNegNotTerm()){
    showErr("Expected an identifier, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  if(CurTok.type == MINUS){
    if(!match(MINUS)){
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type MINUS.";
      showErr(s);
    }
      TOKEN UnaryTok = CurTok;
      getNextToken();

      auto neg_not_term = Neg_Not_Term();

      auto unary = make_unique<UnaryOpExprASTnode>(UnaryTok, std::move(neg_not_term));

      if(err_flag || !followNegNotTerm()){
        showErr("Expected ASTERIX, DIV, MOD, PLUS, GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
        return nullptr;
      }
      return unary;
  }
  else if(CurTok.type == NOT){
    if(!match(NOT)){
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type NOT.";
      showErr(s);
    }
    else{
      TOKEN UnaryTok = CurTok;
      getNextToken();

      auto neg_not_term = Neg_Not_Term();

      auto unary = make_unique<UnaryOpExprASTnode>(UnaryTok, std::move(neg_not_term));

      if(err_flag || !followNegNotTerm()){
        showErr("Expected ASTERIX, DIV, MOD, PLUS, GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
        return nullptr;
      }
      return unary;
    }
  }
  return Paren_Term();
}

// mult_div_mod_term_sub ::= "*" mult_div_mod mult_div_mod_term_sub | "/" mult_div_mod mult_div_mod_term_sub |
// "%" mult_div_mod mult_div_mod_term_sub | epsilon
static std::unique_ptr<BinOpExprASTnode> Mult_Div_Mod_Term_Sub(){
  bool matched = false;

  if(err_flag || !firstplusMultDivModTermSub()){
    showErr("Expected ASTERIX, DIV, MOD, PLUS, GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  if(CurTok.type == ASTERIX || CurTok.type == DIV || CurTok.type == MOD){
    if(CurTok.type == ASTERIX){
      if(!match(ASTERIX)){
        std::string s = "Error: Token ";
        s += CurTok.lexeme;
        s +=  " does not match required type ASTERIX.";
        showErr(s);
      }
      else{
        matched = true;
      }
    }
    else if(CurTok.type == DIV){
      if(!match(DIV)){
        std::string s = "Error: Token ";
        s += CurTok.lexeme;
        s +=  " does not match required type DIV.";
        showErr(s);
      }
      else{
        matched = true;
      }
    }
    else if(CurTok.type == MOD){
      if(!match(MOD)){
        std::string s = "Error: Token ";
        s += CurTok.lexeme;
        s +=  " does not match required type MOD.";
        showErr(s);
      }
      else{
        matched = true;
      }
    }
    auto binary = make_unique<BinOpExprASTnode>(CurTok);
    if(matched){
      getNextToken();
    }

    auto mult_div_mod_term = Mult_Div_Mod_Term();
    auto mult_div_mod_term_sub = Mult_Div_Mod_Term_Sub();

    if(mult_div_mod_term_sub == nullptr){
      binary->RHS = std::move(mult_div_mod_term);
    }
    else{
      binary->RHS = make_unique<BinOpExprASTnode>(mult_div_mod_term_sub->Op, std::move(mult_div_mod_term), std::move(mult_div_mod_term_sub->RHS));
    }
    if(err_flag || !followMultDivModTermSub()){
      showErr("Expected ASTERIX, DIV, MOD, PLUS, GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
      return nullptr;
    }
    return binary;
  }
  else{
    if(err_flag || !followMultDivModTermSub()){
      showErr("Expected ASTERIX, DIV, MOD, PLUS, GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
    }
    return nullptr;
  }
  return nullptr;
}

// mult_div_mod_term ::= neg_not_term mult_div_mod_term_sub
static std::unique_ptr<ASTnode> Mult_Div_Mod_Term(){
  if(err_flag || !firstMultDivModTerm()){
    showErr("Expected an identifier, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  auto neg_not_term = Neg_Not_Term();
  auto mult_div_mod_term_sub = Mult_Div_Mod_Term_Sub();

  if(err_flag || !followMultDivModTerm()){
    showErr("Expected ASTERIX, DIV, MOD, PLUS, GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  if(mult_div_mod_term_sub == nullptr){
    return neg_not_term;
  }
  else{
    mult_div_mod_term_sub->LHS = std::move(neg_not_term);
    return mult_div_mod_term_sub;
  }
}

// add_subt_term_sub ::= "+" add_subt_term add_subt_term_sub | "-" add_subt_term add_subt_term_sub | epsilon
static std::unique_ptr<BinOpExprASTnode> Add_Subt_Term_Sub(){
  bool matched = false;

  if(err_flag || !firstplusAddSubtTermSub()){
    showErr("Expected PLUS, GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  if(CurTok.type == PLUS || CurTok.type == MINUS){
    if(CurTok.type == PLUS){
      if(!match(PLUS)){
        std::string s = "Error: Token ";
        s += CurTok.lexeme;
        s +=  " does not match required type PLUS.";
        showErr(s);
      }
      else{
        matched = true;
      }
    }
    else if(CurTok.type == MINUS){
      if(!match(MINUS)){
        std::string s = "Error: Token ";
        s += CurTok.lexeme;
        s +=  " does not match required type MINUS.";
        showErr(s);
      }
      else{
        matched = true;
      }
    }
    auto binary = make_unique<BinOpExprASTnode>(CurTok);
    if(matched){
      getNextToken();
    }

    auto add_subt_term = Add_Subt_Term();
    auto add_subt_term_sub = Add_Subt_Term_Sub();

    if(add_subt_term_sub == nullptr){
      binary->RHS = std::move(add_subt_term);
    }
    else{
      binary->RHS = make_unique<BinOpExprASTnode>(add_subt_term_sub->Op, std::move(add_subt_term), std::move(add_subt_term_sub->RHS));
    }
    if(err_flag || !followAddSubtTermSub()){
      showErr("Expected PLUS, GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
      return nullptr;
    }
    return binary;
  }
  else{
    if(err_flag || !followAddSubtTermSub()){
      showErr("Expected PLUS, GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
    }
    return nullptr;
  }
  return nullptr;
}

// add_subt_term ::= add_subt_term add_subt_term_sub
static std::unique_ptr<ASTnode> Add_Subt_Term(){
  if(err_flag || !firstAddSubtTerm()){
    showErr("Expected an identifier, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  auto mult_div_mod_term = Mult_Div_Mod_Term();
  auto add_subt_term_sub = Add_Subt_Term_Sub();

  if(err_flag || !followAddSubtTerm()){
    showErr("Expected PLUS, GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  if(add_subt_term_sub == nullptr){
    return mult_div_mod_term;
  }
  else{
    add_subt_term_sub->LHS = std::move(mult_div_mod_term);
    return add_subt_term_sub;
  }
}

// comparison_term_sub ::= "<=" comparison_term comparison_term_sub | "<" comparison_term comparison_term_sub |
// ">=" comparison_term comparison_term_sub | ">" comparison_term comparison_term_sub | epsilon
static std::unique_ptr<BinOpExprASTnode> Comparison_Term_Sub(){
  bool matched = false;

  if(err_flag || !firstplusComparisonTermSub()){
    showErr("Expected GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  if(CurTok.type == GT || CurTok.type == GE || CurTok.type == LT || CurTok.type == LE){
    if(CurTok.type == GT){
      if(!match(GT)){
        std::string s = "Error: Token ";
        s += CurTok.lexeme;
        s +=  " does not match required type GT.";
        showErr(s);
      }
      else{
        matched = true;
      }
    }
    else if(CurTok.type == GE){
      if(!match(GE)){
        std::string s = "Error: Token ";
        s += CurTok.lexeme;
        s +=  " does not match required type GE.";
        showErr(s);
      }
      else{
        matched = true;
      }
    }
    else if(CurTok.type == LT){
      if(!match(LT)){
        std::string s = "Error: Token ";
        s += CurTok.lexeme;
        s +=  " does not match required type LT.";
        showErr(s);
      }
      else{
        matched = true;
      }
    }
    else if(CurTok.type == LE){
      if(!match(LE)){
        std::string s = "Error: Token ";
        s += CurTok.lexeme;
        s +=  " does not match required type LE.";
        showErr(s);
      }
      else{
        matched = true;
      }
    }

    auto binary = make_unique<BinOpExprASTnode>(CurTok);
    if(matched){
      getNextToken();
    }

    auto comparison_term = Comparison_Term();
    auto comparison_term_sub = Comparison_Term_Sub();

    if(comparison_term_sub == nullptr){
      binary->RHS = std::move(comparison_term);
    }
    else{
      binary->RHS = make_unique<BinOpExprASTnode>(comparison_term_sub->Op, std::move(comparison_term), std::move(comparison_term_sub->RHS));
    }
    if(err_flag || !followComparisonTermSub()){
      showErr("Expected GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
      return nullptr;
    }
    return binary;
  }
  else{
    if(err_flag || !followComparisonTermSub()){
      showErr("Expected GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
    }
    return nullptr;
  }
  return nullptr;
}

// comparison_term ::= add_subt_term comparison_term_sub
static std::unique_ptr<ASTnode> Comparison_Term(){
  if(err_flag || !firstComparisonTerm()){
    showErr("Expected an identifier, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  auto add_subt_term = Add_Subt_Term();
  auto comparison_term_sub = Comparison_Term_Sub();

  if(err_flag || !followComparisonTerm()){
    showErr("Expected GT, GE, LT, LE, EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  if(comparison_term_sub == nullptr){
    return add_subt_term;
  }
  else{
    comparison_term_sub->LHS = std::move(add_subt_term);
    return comparison_term_sub;
  }
}

// equality_term_sub ::= "==" equality_term equality_term_sub | "!=" equality_term equality_term_sub | epsilon
static std::unique_ptr<BinOpExprASTnode> Equality_Term_Sub(){
  bool matched = true;

  if(err_flag || !firstplusEqualityTermSub()){
    showErr("Expected EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  if(CurTok.type == EQ || CurTok.type == NE){
    if(CurTok.type == EQ){
      if(!match(EQ)){
        std::string s = "Error: Token ";
        s += CurTok.lexeme;
        s +=  " does not match required type EQ.";
        showErr(s);
      }
      else{
        matched = true;
      }
    }
    else if(CurTok.type == NE){
      if(!match(NE)){
        std::string s = "Error: Token ";
        s += CurTok.lexeme;
        s +=  " does not match required type NE.";
        showErr(s);
      }
      else{
        matched = true;
      }
    }
    auto binary = make_unique<BinOpExprASTnode>(CurTok);
    if(matched){
      getNextToken();
    }

    auto equality_term = Equality_Term();
    auto equality_term_sub = Equality_Term_Sub();

    if(equality_term_sub == nullptr){
      binary->RHS = std::move(equality_term);
    }
    else{
      binary->RHS = make_unique<BinOpExprASTnode>(equality_term_sub->Op, std::move(equality_term), std::move(equality_term_sub->RHS));
    }
    if(err_flag || !followEqualityTermSub()){
      showErr("Expected EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
      return nullptr;
    }
    return binary;
  }
  else{
    if(err_flag || !followEqualityTermSub()){
      showErr("Expected EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
    }
    return nullptr;
  }
  return nullptr;
}

// equality_term ::= comparison_term equality_term_sub
static std::unique_ptr<ASTnode> Equality_Term(){
  if(err_flag || !firstEqualityTerm()){
    showErr("Expected an identifier, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  auto comparison_term = Comparison_Term();
  auto equality_term_sub = Equality_Term_Sub();

  if(err_flag || !followEqualityTerm()){
    showErr("Expected EQ, NE, AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  if(equality_term_sub == nullptr){
    return comparison_term;
  }
  else{
    equality_term_sub->LHS = std::move(comparison_term);
    return equality_term_sub;
  }
}

// and_term_sub ::= "&&" and_term and_term_sub | epsilon
static std::unique_ptr<BinOpExprASTnode> And_Term_Sub(){
  if(err_flag || !firstplusAndTermSub()){
    showErr("Expected AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  if(CurTok.type == AND){
    if(match(AND)){
      auto binary = make_unique<BinOpExprASTnode>(CurTok);
      getNextToken();

      auto and_term = And_Term();
      auto and_term_sub = And_Term_Sub();

      if(and_term_sub == nullptr){
        binary->RHS = std::move(and_term);
      }
      else{
        binary->RHS = make_unique<BinOpExprASTnode>(and_term_sub->Op, std::move(and_term), std::move(and_term_sub->RHS));
      }
      if(err_flag || !followAndTermSub()){
        showErr("Expected AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
        return nullptr;
      }
      return binary;
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type AND.";
      showErr(s);
    }
  }
  else{
    if(err_flag || !followAndTermSub()){
      showErr("Expected AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
    }
    return nullptr;
  }
  return nullptr;
}

// and_term ::= equality_term and_term_sub
static std::unique_ptr<ASTnode> And_Term(){
  if(err_flag || !firstAndTerm()){
    showErr("Expected an identifier, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  auto equality_term = Equality_Term();
  auto and_term_sub = And_Term_Sub();

  if(err_flag || !followAndTerm()){
    showErr("Expected AND, OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  if(and_term_sub == nullptr){
    return equality_term;
  }
  else{
    and_term_sub->LHS = std::move(equality_term);
    return and_term_sub;
  }
}

// or_term_sub ::= "||" or_term or_term_sub | epsilon
static std::unique_ptr<BinOpExprASTnode> Or_Term_Sub(){
  if(err_flag || !firstplusOrTermSub()){
    showErr("Expected OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  if(CurTok.type == OR){
    if(match(OR)){
      auto binary = make_unique<BinOpExprASTnode>(CurTok);
      getNextToken();

      auto or_term = Or_Term();
      auto or_term_sub = Or_Term_Sub();

      if(or_term_sub == nullptr){
        binary->RHS = std::move(or_term);
      }
      else{
        binary->RHS = make_unique<BinOpExprASTnode>(or_term_sub->Op, std::move(or_term), std::move(or_term_sub->RHS));
      }
      if(err_flag || !followOrTermSub()){
        showErr("Expected OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
        return nullptr;
      }
      return binary;
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type OR.";
      showErr(s);
    }
  }
  else{
    if(err_flag || !followOrTermSub()){
      showErr("Expected OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
    }
    return nullptr;
  }
  return nullptr;
}

// or_term ::= and_term or_term_sub
static std::unique_ptr<ASTnode> Or_Term(){
  if(err_flag || !firstOrTerm()){
    showErr("Expected an identifier, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  auto and_term = And_Term();
  auto or_term_sub = Or_Term_Sub();

  if(err_flag || !followOrTerm()){
    showErr("Expected OR, RPAR, an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  if(or_term_sub == nullptr){
    return and_term;
  }
  else{
    or_term_sub->LHS = std::move(and_term);
    return or_term_sub;
  }
}

// expr ::= IDENT "=" expr | or_term
static std::unique_ptr<ASTnode> Expr(){
  bool matched = false;
  if(err_flag || !firstExpr()){
    showErr("Expected an identifier, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  if(CurTok.type == IDENT && NextTok.type == ASSIGN){
    if(!match(IDENT)){
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type IDENT.";
      showErr(s);
    }
    else{
      matched = true;
    }
    auto ident = make_unique<IdentASTnode>(CurTok, CurTok.lexeme);
    if(matched){
      getNextToken();
    }

    if(match(ASSIGN)){
      getNextToken();
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type EQ.";
      showErr(s);
    }

    auto expr = Expr();

    auto result = make_unique<AssExprASTnode>(std::move(ident), std::move(expr));

    if(err_flag || !followExpr()){
      showErr("Expected an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
      return nullptr;
    }
    result->to_string();
    return result;
  }
  else{
    auto result = Or_Term();

    if(err_flag || !followExpr()){
      showErr("Expected an identifier, SC, COMMA, RPAR, MINUS, NOT, LPAR or a literal.");
      return nullptr;
    }
    return result;
  }
}

// return_stmt_sub ::= ";" | ";" expr ";"
static std::unique_ptr<ReturnStmtASTnode> Return_Stmt_Sub(){
  if(err_flag || !firstReturnStmtSub()){
    showErr("Expected an identifier, SC, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  if(CurTok.type == SC){
    if(match(SC)){
      getNextToken();
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type SC.";
      showErr(s);
    }

    auto returnstmt = make_unique<ReturnStmtASTnode>();

    if(err_flag || !followReturnStmtSub()){
      showErr("Expected an identifier, SC, LBRA, RBRA, WHILE, IF, RETURN, MINUS, NOT, LPAR or a literal.");
      return nullptr;
    }
    return returnstmt;
  }
  else{
    auto expr = Expr();

    if(match(SC)){
      getNextToken();
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type SC.";
      showErr(s);
    }

    auto returnstmt = make_unique<ReturnStmtASTnode>(std::move(expr));

    if(err_flag || !followReturnStmtSub()){
      showErr("Expected an identifier, SC, LBRA, RBRA, WHILE, IF, RETURN, MINUS, NOT, LPAR or a literal.");
      return nullptr;
    }
    return returnstmt;
  }
}

// return_stmt ::= "return" return_stmt_sub
static std::unique_ptr<ReturnStmtASTnode> Return_Stmt(){
  if(err_flag || !firstReturnStmt()){
    showErr("Expected RETURN");
    return nullptr;
  }

  if(match(RETURN)){
    getNextToken();
  }
  else{
    std::string s = "Error: Token ";
    s += CurTok.lexeme;
    s +=  " does not match required type RETURN.";
    showErr(s);
  }

  auto return_stmt_sub = Return_Stmt_Sub();

  if(err_flag || !followReturnStmt()){
    showErr("Expected an identifier, SC, LBRA, RBRA, WHILE, IF, RETURN, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }
  return return_stmt_sub;
}

// else_stmt ::= "else" block | epsilon
static std::unique_ptr<BlockASTnode> Else_Stmt(){
  if(err_flag || !firstplusElseStmt()){
    showErr("Expected ELSE, an identifier, SC, LBRA, RBRA, WHILE, IF, RETURN, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  if(CurTok.type == ELSE){
    if(match(ELSE)){
      getNextToken();
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type ELSE.";
      showErr(s);
    }

    auto block = Block();

    if(err_flag || !followElseStmt()){
      showErr("Expected an identifier, SC, LBRA, RBRA, WHILE, IF, RETURN, MINUS, NOT, LPAR or a literal.");
      return nullptr;
    }
    return block;
  }
  else{
    if(err_flag || !followElseStmt()){
      showErr("Expected an identifier, SC, LBRA, RBRA, WHILE, IF, RETURN, MINUS, NOT, LPAR or a literal.");
    }
    return nullptr;
  }
}

// if_stmt ::= "if" "(" expr ")" block else_stmt
static std::unique_ptr<IfStmtASTnode> If_Stmt(){
  if(err_flag || !firstIfStmt()){
    showErr("Expected IF");
    return nullptr;
  }

  if(match(IF)){
    getNextToken();
  }
  else{
    std::string s = "Error: Token ";
    s += CurTok.lexeme;
    s +=  " does not match required type IF.";
    showErr(s);
  }

  if(match(LPAR)){
    getNextToken();
  }
  else{
    std::string s = "Error: Token ";
    s += CurTok.lexeme;
    s +=  " does not match required type LPAR.";
    showErr(s);
  }

  auto expr = Expr();

  if(match(RPAR)){
    getNextToken();
  }
  else{
    std::string s = "Error: Token ";
    s += CurTok.lexeme;
    s +=  " does not match required type RPAR.";
    showErr(s);
  }
  auto block = Block();
  auto else_stmt = Else_Stmt();

  if(err_flag || !followIfStmt()){
    showErr("Expected an identifier, SC, LBRA, RBRA, WHILE, IF, RETURN, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  auto builder = make_unique<IfBuilderASTnode>(std::move(block), std::move(else_stmt));

  return make_unique<IfStmtASTnode>(std::move(expr), std::move(builder));
}

// while_stmt ::= "while" "(" expr ")" stmt
static std::unique_ptr<WhileStmtASTnode> While_Stmt(){
  if(err_flag || !firstWhileStmt()){
    showErr("Expected WHILE");
    return nullptr;
  }

  if(match(WHILE)){
    getNextToken();
  }
  else{
    std::string s = "Error: Token ";
    s += CurTok.lexeme;
    s +=  " does not match required type WHILE.";
    showErr(s);
  }

  if(match(LPAR)){
    getNextToken();
  }
  else{
    std::string s = "Error: Token ";
    s += CurTok.lexeme;
    s +=  " does not match required type LPAR.";
    showErr(s);
  }

  auto expr = Expr();

  if(match(RPAR)){
    getNextToken();
  }
  else{
    std::string s = "Error: Token ";
    s += CurTok.lexeme;
    s +=  " does not match required type RPAR.";
    showErr(s);
  }

  auto stmt = Stmt();

  if(err_flag || !followWhileStmt()){
    showErr("Expected an identifier, SC, LBRA, RBRA, WHILE, IF, RETURN, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }
  return make_unique<WhileStmtASTnode>(std::move(expr), std::move(stmt));
}

// expr_stmt ::= expr ";" | ";"
static std::unique_ptr<ASTnode> Expr_Stmt(){
  if(err_flag || !firstExprStmt()){
    showErr("Expected an identifier, SC, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  if(CurTok.type==SC){
    if(match(SC)){
      getNextToken();
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type SC.";
      showErr(s);
    }

    if(err_flag || !followExprStmt()){
      showErr("Expected an identifier, SC, LBRA, RBRA, WHILE, IF, RETURN, MINUS, NOT, LPAR or a literal.");
      return nullptr;
    }
  }
  else{
    auto expr = Expr();

    if(match(SC)){
      getNextToken();
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type SC.";
      showErr(s);
    }

    if(err_flag || !followExprStmt()){
      showErr("Expected an identifier, SC, LBRA, RBRA, WHILE, IF, RETURN, MINUS, NOT, LPAR or a literal.");
      return nullptr;
    }
    return expr;
  }
  return nullptr;
}

// stmt ::= expr_stmt | block | if_stmt | while_stmt | return_stmt
static std::unique_ptr<ASTnode> Stmt(){

  if(err_flag || !firstStmt()){
    showErr("Expected an identifier, SC, LBRA, WHILE, IF, RETURN, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }

  if(CurTok.type==LBRA){
    return Block();
  }
  else if(CurTok.type==WHILE){
    return While_Stmt();
  }
  else if(CurTok.type==IF){
    return If_Stmt();
  }
  else if(CurTok.type==RETURN){
    return Return_Stmt();
  }
  else{
    return Expr_Stmt();
  }
}

// stmt_list ::= stmt stmt_list | epsilon
static std::vector<std::unique_ptr<ASTnode>> Stmt_List(){
  std::vector<std::unique_ptr<ASTnode>> list;
  std::vector<std::unique_ptr<ASTnode>> nullVector;

  if(err_flag || !firstplusStmtList()){
    showErr("Expected an identifier, SC, LBRA, WHILE, IF, RETURN, MINUS, NOT, LPAR or a literal.");
    return nullVector;
  }

  if(CurTok.type == RBRA){
    if(err_flag || !followStmtList()){
      showErr("Expected RBRA.");
    }
    return nullVector;
  }
  else{
    auto stmt = Stmt();

    auto stmt_list = Stmt_List();

    if(stmt != nullptr){
      list.push_back(std::move(stmt));
    }
    for (int i = 0;i < (int)stmt_list.size();i++){
      list.push_back(std::move(stmt_list[i]));
    }

    if(err_flag || !followStmtList()){
      showErr("Expected RBRA.");
      return nullVector;
    }
    return list;
  }
}

// var_type ::= "int" | "float" | "bool"
static std::unique_ptr<TypesASTnode> Var_Type(){
  TOKEN TypeTok;

  if(err_flag || !firstVarType()){
    showErr("Expected variable declaration type.");
    return nullptr;
  }

  if(CurTok.type == INT_TOK){
    if(match(INT_TOK)){
      TypeTok = CurTok;
      getNextToken();
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type INT_TOK.";
      showErr(s);
    }
  }
  else if(CurTok.type == FLOAT_TOK){
    if(match(FLOAT_TOK)){
      TypeTok = CurTok;
      getNextToken();
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type FLOAT_TOK.";
      showErr(s);
    }
  }
  else if(CurTok.type == BOOL_TOK){
    if(match(BOOL_TOK)){
      TypeTok = CurTok;
      getNextToken();
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type BOOL_TOK.";
      showErr(s);
    }
  }

  if(err_flag || !followVarType()){
    showErr("Expected identifier");
    return nullptr;
  }
  return make_unique<TypesASTnode>(TypeTok);
}

// local_decl ::= var_type IDENT ";"
static std::unique_ptr<VarDeclsASTnode> Local_Decl(){

  bool matched = false;

  if(err_flag || !firstLocalDecl()){
    showErr("Expected variable declaration type");
    return nullptr;
  }

  auto type = Var_Type();

  if(!match(IDENT)){
    std::string s = "Error: Token ";
    s += CurTok.lexeme;
    s +=  " does not match required type IDENT.";
    showErr(s);
  }
  else{
    matched = true;
  }
  auto ident = make_unique<IdentASTnode>(CurTok, CurTok.lexeme);
  if(matched){
    getNextToken();
  }

  if(CurTok.type == SC){
    if(match(SC)){
      getNextToken();
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type SC.";
      showErr(s);
    }
  }

  if(err_flag || !followLocalDecl()){
    showErr("Expected an identifier, SC, a declaration type, WHILE, IF, RETURN, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }
  return make_unique<VarDeclsASTnode>(std::move(type), std::move(ident));
}

// local_decls ::= local_decl local_decls | epsilon
static std::vector<std::unique_ptr<VarDeclsASTnode>> Local_Decls(){
  std::vector<std::unique_ptr<VarDeclsASTnode>> list;
  std::vector<std::unique_ptr<VarDeclsASTnode>> nullVector;

  if(err_flag || !firstplusLocalDecls()){
    showErr("Expected variable declaration type");
    return nullVector;
  }

  if(CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK){
    auto local_decl = Local_Decl();
    auto local_decls = Local_Decls();

    if(local_decl!= nullptr){
      list.push_back(std::move(local_decl));
    }
    for (int i = 0;i < (int)local_decls.size();i++){
      list.push_back(std::move(local_decls[i]));
    }
  }

  if(err_flag || !followLocalDecls()){
    showErr("Expected an identifier, SC, LBRA, WHILE, IF, RETURN, MINUS, NOT, LPAR or a literal.");
    return nullVector;
  }
  return list;
}

// block ::= "{" local_decls stmt_list "}"
static std::unique_ptr<BlockASTnode> Block(){
  if(err_flag || !firstBlock()){
    showErr("Expected LBRA.");
    return nullptr;
  }

  if(CurTok.type == LBRA){
    if(match(LBRA)){
      getNextToken();
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type LBRA.";
      showErr(s);
    }
  }

  auto localdecls = Local_Decls();
  auto stmtlist = Stmt_List();

  if(CurTok.type == RBRA){
    if(match(RBRA)){
      getNextToken();
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type RBRA.";
      showErr(s);
    }
  }

  if(err_flag || !followBlock()){
    showErr("Expected an identifier, SC, a declaration type, LBRA, RBRA, WHILE, IF, ELSE, RETURN, MINUS, NOT, LPAR or a literal.");
    return nullptr;
  }
  return make_unique<BlockASTnode>(std::move(localdecls), std::move(stmtlist));
}

// param ::= var_type IDENT
static std::unique_ptr<VarDeclsASTnode> Param(){
bool matched = false;

  if(err_flag || !firstParam()){
    showErr("Expected variable declaration type.");
    return nullptr;
  }

  auto type = Var_Type();

  if(!match(IDENT)){
    std::string s = "Error: Token ";
    s += CurTok.lexeme;
    s +=  " does not match required type IDENT.";
    showErr(s);
  }
  else{
    matched = true;
  }
  auto ident = make_unique<IdentASTnode>(CurTok, CurTok.lexeme);
  if(matched){
    getNextToken();
  }

  if(err_flag || !followParam()){
    showErr("Expected RPAR or COMMA.");
    return nullptr;
  }
  return make_unique<VarDeclsASTnode>(std::move(type), std::move(ident));
}

// params_list_sub ::= "," param param_list_sub | epsilon
static std::vector<std::unique_ptr<VarDeclsASTnode>> Param_List_Sub(){
  std::vector<std::unique_ptr<VarDeclsASTnode>> list;
  std::vector<std::unique_ptr<VarDeclsASTnode>> nullVector;

  if(err_flag || !firstplusParamListSub()){
    showErr("Expected COMMA");
    return nullVector;
  }

  if(CurTok.type == COMMA){
    if(match(COMMA)){
      getNextToken();
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type COMMA.";
      showErr(s);
    }

    auto param = Param();
    auto param_list_sub = Param_List_Sub();

    if(param != nullptr){
      list.push_back(std::move(param));
    }
    for(int i = 0; i<(int)param_list_sub.size(); i++){
      list.push_back(std::move(param_list_sub[i]));
    }
  }

  if(err_flag || !followParamListSub()){
    showErr("Expected RPAR.");
    return nullVector;
  }
  return list;
}

// params_list ::= param param_list_sub
static std::vector<std::unique_ptr<VarDeclsASTnode>> Param_List(){
  std::vector<std::unique_ptr<VarDeclsASTnode>> list;
  std::vector<std::unique_ptr<VarDeclsASTnode>> nullVector;

  if(err_flag || !firstParamList()){
    showErr("Expected variable declaration type.");
    return nullVector;
  }

  auto param = Param();
  auto param_list_sub = Param_List_Sub();

  if(param != nullptr){
    list.push_back(std::move(param));
  }
  for(int i = 0; i<(int)param_list_sub.size(); i++){
    list.push_back(std::move(param_list_sub[i]));
  }

  if(err_flag || !followParamList()){
    showErr("Expected RPAR.");
    return nullVector;
  }
  return list;
}

// params ::= param_list | "void" | epsilon
static std::unique_ptr<VarListASTnode> Params(){
  if(err_flag || !firstplusParams()){
    showErr("Expected declaration type or RPAR.");
    return nullptr;
  }

  if(CurTok.type == VOID_TOK){
    if(match(VOID_TOK)){
      auto params = make_unique<VarListASTnode>();
      getNextToken();
      if(err_flag || !followParams()){
        showErr("Expected RPAR.");
        return nullptr;
      }
      return params;
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type VOID_TOK.";
      showErr(s);
    }
  }

  else if(CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK){
    auto params = Param_List();

    if(err_flag || !followParams()){
      showErr("Expected RPAR.");
      return nullptr;
    }
    return make_unique<VarListASTnode>(std::move(params));
  }

  if(err_flag || !followParams()){
    showErr("Expected RPAR.");
    return nullptr;
  }
  return nullptr;
}

// type_spec ::= "void" | var_type
static std::unique_ptr<TypesASTnode> Type_Spec(){
  if(err_flag || !firstTypeSpec()){
    showErr("Expected declaration type.");
    return nullptr;
  }

  if(CurTok.type == VOID_TOK){
    if(match(VOID_TOK)){
      auto type = make_unique<TypesASTnode>(CurTok);
      getNextToken();
      if(err_flag || !followTypeSpec()){
        showErr("Expected identifier");
        return nullptr;
      }
      return type;
    }
    else{
      std::string s = "Error: Token ";
      s += CurTok.lexeme;
      s +=  " does not match required type VOID_TOK.";
      showErr(s);
    }
  }
  else{
    auto type = Var_Type();
    if(err_flag || !followTypeSpec()){
      showErr("Expected identifier");
      return nullptr;
    }
    return type;
  }
  return nullptr;
}

// fun_decl ::= type_spec IDENT "(" params ")" block
static std::unique_ptr<FunctionASTnode> Fun_Decl(){
  bool matched = false;

  if(err_flag || !firstFunDecl()){
    showErr("Expected variable declaration type.");
    return nullptr;
  }

  auto type = Type_Spec();

  if(!match(IDENT)){
    std::string s = "Error: Token ";
    s += CurTok.lexeme;
    s +=  " does not match required type IDENT.";
    showErr(s);
  }
  else{
    matched = true;
  }
  auto ident = make_unique<IdentASTnode>(CurTok, CurTok.lexeme);
  if(matched){
    getNextToken();
  }

  if(match(LPAR)){
    getNextToken();
  }
  else{
    std::string s = "Error: Token ";
    s += CurTok.lexeme;
    s +=  " does not match required type LPAR.";
    showErr(s);
  }

  auto params = Params();

  if(match(RPAR)){
    getNextToken();
  }
  else{
    std::string s = "Error: Token ";
    s += CurTok.lexeme;
    s +=  " does not match required type RPAR.";
    showErr(s);
  }

  auto block = Block();

  if(err_flag || !followFunDecl()){
    showErr("Expected declaration type or EOF.");
    return nullptr;
  }

  auto builder = make_unique<FunctionBuilderASTnode>(std::move(type), std::move(ident), std::move(params));

  return make_unique<FunctionASTnode>(std::move(builder), std::move(block));
}

// var_decl ::= var_type IDENT ";"
static std::unique_ptr<VarDeclsASTnode> Var_Decl(){
  bool matched = false;

  if(err_flag || !firstVarDecl()){
    showErr("Expected variable declaration type.");
    return nullptr;
  }

  auto type = Var_Type();

  if(!match(IDENT)){
    std::string s = "Error: Token ";
    s += CurTok.lexeme;
    s +=  " does not match required type IDENT.";
    showErr(s);
  }
  else{
    matched = true;
  }
  auto ident = make_unique<IdentASTnode>(CurTok, CurTok.lexeme);
  if(matched){
    getNextToken();
  }

  if(match(SC)){
    getNextToken();
  }
  else{
    std::string s = "Error: Token ";
    s += CurTok.lexeme;
    s +=  " does not match required type SC.";
    showErr(s);
  }

  if(err_flag || !followVarDecl()){
    showErr("Expected declaration type or EOF.");
    return nullptr;
  }
  return make_unique<VarDeclsASTnode>(std::move(type), std::move(ident));
}

// decl ::= var_decl | fun_decl
static std::unique_ptr<ASTnode> Decl(){
  if(err_flag || !firstDecl()){
    showErr("Expected declaration type.");
    return nullptr;
  }

  if(CurTok.type == VOID_TOK){
    auto fun_decl = Fun_Decl();
    if(err_flag || !followDecl()){
      showErr("Expected declaration type or EOF.");
      return nullptr;
    }
    return fun_decl;
  }
  else{
    if(NextNextTok.type == SC){
      auto var_decl = Var_Decl();
      if(err_flag || !followDecl()){
        showErr("Expected declaration type or EOF.");
        return nullptr;
      }
      return var_decl;
    }
    else{
      auto fun_decl = Fun_Decl();
      if(err_flag || !followDecl()){
        showErr("Expected declaration type or EOF.");
        return nullptr;
      }
      return fun_decl;
    }
  }
  return nullptr;
}

// decl_list_sub ::= decl decl_list_sub | epsilon
static std::vector<std::unique_ptr<ASTnode>> Decl_List_Sub(){
  std::vector<std::unique_ptr<ASTnode>> list;
  std::vector<std::unique_ptr<ASTnode>> nullVector;

  if(err_flag || !firstplusDeclListSub()){
    showErr("Expected declaration type or EOF.");
    return nullVector;
  }

  if(CurTok.type == VOID_TOK || CurTok.type == INT_TOK || CurTok.type == BOOL_TOK || CurTok.type == FLOAT_TOK){
    auto decl = Decl();
    auto decl_list_sub = Decl_List_Sub();

    if(decl != nullptr){
      list.push_back(std::move(decl));
    }
    for(int i = 0; i < (int)decl_list_sub.size(); i++){
      list.push_back(std::move(decl_list_sub[i]));
    }
  }

  if(err_flag || !followDeclListSub()){
    showErr("Expected EOF");
    return nullVector;
  }
  return list;
}

// decl_list ::= decl decl_list_sub
static std::vector<std::unique_ptr<ASTnode>> Decl_List(){
  std::vector<std::unique_ptr<ASTnode>> list;
  std::vector<std::unique_ptr<ASTnode>> nullVector;

  if(err_flag || !firstDeclList()){
    showErr("Expected declaration type.");
    return nullVector;
  }

  auto decl = Decl();
  auto decl_list_sub = Decl_List_Sub();

  if(decl != nullptr){
    list.push_back(std::move(decl));
  }
  for(int i = 0; i < (int)decl_list_sub.size(); i++){
    list.push_back(std::move(decl_list_sub[i]));
  }

  if(err_flag || !followDeclList()){
    showErr("Expected EOF");
    return nullVector;
  }
  return list;
}

// extern ::= "extern" type_spec IDENT"("params")" ";"
static std::unique_ptr<FunctionBuilderASTnode> Extern(){
  bool matched = false;

  if(err_flag || !firstExtern()){
    showErr("Expected 'extern'.");
    return nullptr;
  }

  if(match(EXTERN)){
    getNextToken();
  }
  else{
    std::string s = "Error: Token ";
    s += CurTok.lexeme;
    s +=  " does not match required type EXTERN.";
    showErr(s);
  }

  auto type = Type_Spec();

  if(!match(IDENT)){
    std::string s = "Error: Token ";
    s += CurTok.lexeme;
    s +=  " does not match required type IDENT.";
    showErr(s);
  }
  else{
    matched = true;
  }
  auto ident = make_unique<IdentASTnode>(CurTok, CurTok.lexeme);
  if(matched){
    getNextToken();
  }

  if(match(LPAR)){
    getNextToken();
  }
  else{
    std::string s = "Error: Token ";
    s += CurTok.lexeme;
    s +=  " does not match required type LPAR.";
    showErr(s);
  }

  auto params = Params();

  if(match(RPAR)){
    getNextToken();
  }
  else{
    std::string s = "Error: Token ";
    s += CurTok.lexeme;
    s +=  " does not match required type RPAR.";
    showErr(s);
  }

  if(match(SC)){
    getNextToken();
  }
  else{
    std::string s = "Error: Token ";
    s += CurTok.lexeme;
    s +=  " does not match required type SC.";
    showErr(s);
  }

  if(err_flag || !followExtern()){
    showErr("Expected declaration type");
    return nullptr;
  }
  return make_unique<FunctionBuilderASTnode>(std::move(type), std::move(ident), std::move(params));
}

// extern_list_sub ::= extern extern_list_sub | epsilon
static std::vector<std::unique_ptr<FunctionBuilderASTnode>> Extern_List_Sub(){
  std::vector<std::unique_ptr<FunctionBuilderASTnode>> list;
  std::vector<std::unique_ptr<FunctionBuilderASTnode>> nullVector;

  if(err_flag || !firstplusExternListSub()){
    showErr("Expected 'extern' or declaration type.");
    return nullVector;
  }

  if(CurTok.type == EXTERN){
    auto extern_element = Extern();
    auto extern_list_sub = Extern_List_Sub();

    if(extern_element != nullptr){
      list.push_back(std::move(extern_element));
    }
    for(int i = 0; i < (int)extern_list_sub.size(); i++){
      list.push_back(std::move(extern_list_sub[i]));
    }

    if(err_flag || !followExternListSub()){
      showErr("Expected declaration type");
      return nullVector;
    }
    return list;
  }
  return nullVector;
}

// extern_list ::= extern extern_list_sub
static std::vector<std::unique_ptr<FunctionBuilderASTnode>> Extern_List(){
  std::vector<std::unique_ptr<FunctionBuilderASTnode>> list;
  std::vector<std::unique_ptr<FunctionBuilderASTnode>> nullVector;

  if(err_flag || !firstExternList()){
    showErr("Expected 'extern'.");
    return nullVector;
  }

  auto extern_element = Extern();
  auto extern_list_sub = Extern_List_Sub();

  if(extern_element != nullptr){
    list.push_back(std::move(extern_element));
  }
  for(int i = 0; i < (int)extern_list_sub.size(); i++){
    list.push_back(std::move(extern_list_sub[i]));
  }

  if(err_flag || !followExternList()){
    showErr("Expected declaration type");
    return nullVector;
  }
  return list;
}

// program ::= extern_list decl_list | decl_list
static std::unique_ptr<ASTnode> parser(){
  if(err_flag || !firstProgram()){
    showErr("Expected 'extern' or declaration type.");
    return nullptr;
  }

  if(CurTok.type == EXTERN){
    auto extern_list = Extern_List();
    auto decl_list = Decl_List();
    if(err_flag || !followProgram()){
      showErr("Expected EOF.");
      return nullptr;
    }
    return make_unique<ProgramASTnode>(std::move(extern_list),std::move(decl_list));
  }
  else if(CurTok.type == VOID_TOK || CurTok.type == INT_TOK || CurTok.type == BOOL_TOK || CurTok.type == FLOAT_TOK){
    auto decl_list = Decl_List();
    if(err_flag || !followProgram()){
      showErr("Expected EOF.");
      return nullptr;
    }
    return make_unique<ProgramASTnode>(std::move(decl_list));
  }
  return nullptr;
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//
static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;
static std::map<std::string, AllocaInst*> NamedValues;

static AllocaInst *CreateEntryBlockAlloca(Function *F, const std::string &VName, Type* type) {
  IRBuilder<> Tmp(&F->getEntryBlock(), F->getEntryBlock().begin());
  return Tmp.CreateAlloca(type, 0, VName.c_str());
}

Value *LogErrorV(const char *s){
    showCodeGenErr(s);
    return nullptr;
}

Value *IntLitASTnode::codegen(){
  return ConstantInt::get(TheContext, APInt(32,Val));
}

Value *FloatLitASTnode::codegen(){
  return ConstantFP::get(TheContext, APFloat(Val));
}

Value *BoolLitASTnode::codegen(){
  return ConstantInt::get(TheContext, APInt(1,Val));
}

Value *LiteralsASTnode::codegen(){
  return Literal->codegen();
}

Value *BinOpExprASTnode::codegen(){
  Value *L = LHS->codegen();
  Value *R = RHS->codegen();
  if(!L || !R){
    return nullptr;
  }
  auto leftType = L->getType();
  auto rightType = R->getType();

  //change LHS and RHS types to ensure that they match
  if(leftType != rightType){
    if(leftType == Type::getFloatTy(TheContext)){
      R = Builder.CreateSIToFP(R, Type::getFloatTy(TheContext), "upgradeRHStoFloat");
    }
    else if(rightType == Type::getFloatTy(TheContext)){
      L = Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "upgradeLHStoFloat");
    }
    else if(leftType == Type::getInt32Ty(TheContext)){
      R = Builder.CreateSIToFP(R, Type::getFloatTy(TheContext), "upgradeRhsToFloat");
      R = Builder.CreateFPToSI(R, Type::getInt32Ty(TheContext), "downgradeRhsToInt");
    }
    else if(rightType == Type::getInt32Ty(TheContext)){
      L = Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "upgradeRhsToFloat");
      L = Builder.CreateFPToSI(L, Type::getInt32Ty(TheContext), "downgradeRhsToInt");
    }
  }
  leftType = L->getType();
  rightType = R->getType();

  // make final check to ensure LHS and RHS are now the same type
  // and perform the operation
  if(leftType == rightType){
    // for type float
    if(leftType == Type::getFloatTy(TheContext)){
      switch(Op.type){
        case(PLUS):
          return Builder.CreateFAdd(L, R, "addtmp");
        case(MINUS):
          return Builder.CreateFSub(L, R, "subtmp");
        case(ASTERIX):
          return Builder.CreateFMul(L, R, "multmp");
        case(DIV):
          return Builder.CreateFDiv(L, R, "divtmp");
        case(MOD):
          return Builder.CreateFRem(L, R, "remtmp");
        case(LT):
          L = Builder.CreateFCmpULT(L, R, "lttmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        case(GT):
          L = Builder.CreateFCmpUGT(L, R, "gttmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        case(LE):
          L = Builder.CreateFCmpULE(L, R, "letmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        case(GE):
          L = Builder.CreateFCmpUGE(L, R, "getmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        case(EQ):
          L = Builder.CreateFCmpUEQ(L, R, "eqtmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        case(NE):
          L = Builder.CreateFCmpUNE(L, R, "netmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        case(AND):
          return LogErrorV("AND cannot be applied to type float");
        case(OR):
          return LogErrorV("OR cannot be applied to type float");
        default:
          return LogErrorV("Invalid Binary Operator");
      }
    }
    // for type int
    else if(leftType == Type::getInt32Ty(TheContext)){
      switch(Op.type){
        case(PLUS):
          return Builder.CreateAdd(L, R, "addtmp");
        case(MINUS):
          return Builder.CreateSub(L, R, "subtmp");
        case(ASTERIX):
          return Builder.CreateMul(L, R, "multmp");
        case(DIV):
          return Builder.CreateSDiv(L, R, "divtmp");
        case(MOD):
          return Builder.CreateSRem(L, R, "remtmp");
        case(LT):
          L = Builder.CreateICmpULT(L, R, "lttmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        case(GT):
          L = Builder.CreateICmpUGT(L, R, "gttmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        case(LE):
          L = Builder.CreateICmpULE(L, R, "letmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        case(GE):
          L = Builder.CreateICmpUGE(L, R, "getmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        case(EQ):
          L = Builder.CreateICmpEQ(L, R, "eqtmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        case(NE):
          L = Builder.CreateICmpNE(L, R, "netmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        case(AND):
          return LogErrorV("AND cannot be applied to type float");
        case(OR):
          return LogErrorV("OR cannot be applied to type float");
        default:
          return LogErrorV("Invalid Binary Operator");
      }
    }
    //for type bool
    else if(leftType == Type::getInt1Ty(TheContext)){
      switch(Op.type){
        case(PLUS):
          return Builder.CreateOr(L, R, "addbool");
        case(MINUS):
          return LogErrorV("MINUS cannot be applied to type bool");
        case(ASTERIX):
          return Builder.CreateAnd(L, R, "mulbool");
        case(DIV):
          return LogErrorV("DIV cannot be applied to type bool");
        case(MOD):
          return LogErrorV("MOD cannot be applied to type bool");
        case(LT):
          L = Builder.CreateICmpULT(L, R, "lttmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        case(GT):
          L = Builder.CreateICmpUGT(L, R, "gttmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        case(LE):
          L = Builder.CreateICmpULE(L, R, "letmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        case(GE):
          L = Builder.CreateICmpUGE(L, R, "getmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        case(EQ):
          L = Builder.CreateICmpEQ(L, R, "eqtmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        case(NE):
          L = Builder.CreateICmpNE(L, R, "netmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        case(AND):
          L = Builder.CreateAnd(L, R, "andtmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        case(OR):
          L = Builder.CreateOr(L, R, "ortmp");
          return Builder.CreateSIToFP(L, Type::getFloatTy(TheContext), "booltmp");
        default:
          return LogErrorV("Invalid Binary Operator");
      }
    }
  }
  return LogErrorV("Could not convert types");
}

Value *UnaryOpExprASTnode::codegen(){
  Value *V = Val->codegen();

  if(!V){
    return nullptr;
  }

  if(V->getType() == Type::getFloatTy(TheContext)){
    switch(Op.type){
      case(MINUS):
        return Builder.CreateFNeg(V, "negtmp");
      case(NOT):
        return LogErrorV("NOT cannot be applied to type float");
      default:
        return LogErrorV("Invalid Unary Operator");
    }
  }
  else if(V->getType() == Type::getInt32Ty(TheContext)){
    switch(Op.type){
      case(MINUS):
        return Builder.CreateFPToSI(Builder.CreateFNeg(Builder.CreateSIToFP(V,
        Type::getFloatTy(TheContext), "upgradeRHStoFloat"), "negtmp"),
        Type::getInt32Ty(TheContext), "upgradeRHStoFloat");
      case(NOT):
        return LogErrorV("NOT cannot be applied to type int");
      default:
        return LogErrorV("Invalid Unary Operator");
    }
  }
  else if(V->getType() == Type::getInt1Ty(TheContext)){
    switch(Op.type){
      case(MINUS):
        return LogErrorV("NEG cannot be applied to type int");
      case(NOT):
        return Builder.CreateNot(V, "nottemp");
      default:
        return LogErrorV("Invalid Unary Operator");
    }
  }
  return LogErrorV("Invalid Unary Operator");
}

Value *IdentASTnode::codegen(){
  // check ifvariable is in local scope
  Value *V = NamedValues[Val];
  // ifnot, check ifit is in global scope
  if(!V){
    V = TheModule->getNamedValue(Val);
    // ifstill not found
    if(!V){
      std::string s = "Variable ";
      s += Val;
      s += " is not in current scope\n";
      return LogErrorV(s.c_str());
    }
  }
  return Builder.CreateLoad(V, Val.c_str());
}

// global variable declarations
Value *VarDeclsASTnode::codegen(){
  switch(Type->getType()){
      case INT_TOK:
        return new GlobalVariable(*TheModule,Type::getInt32Ty(TheContext),false,  GlobalValue::CommonLinkage, ConstantInt::get(TheContext, APInt(32,0)), Identifier->getVal());
      case FLOAT_TOK:
        return new GlobalVariable(*TheModule,Type::getFloatTy(TheContext),false,  GlobalValue::CommonLinkage, ConstantFP::get(TheContext, APFloat(0.0)), Identifier->getVal());
      case BOOL_TOK:
        return new GlobalVariable(*TheModule,Type::getInt1Ty(TheContext),false,  GlobalValue::CommonLinkage, ConstantInt::get(TheContext, APInt(1,0)), Identifier->getVal());
      default:
        return nullptr;
  }
  return nullptr;
}

Value *AssExprASTnode::codegen(){
  Value *Val = Expression->codegen();
  if(!Val){
    return nullptr;
  }

  // Check ifvariable name is in the local list
  Value *VName = NamedValues[Identifier->getVal()];
  // If not found, check ifit is in global list
  if(!VName){
    VName = TheModule->getNamedValue(Identifier->getVal());
    // If still not found
    if(!VName){
      std::string s = "Variable ";
      s += Identifier->getVal();
      s += "' cannot be assigned because it is not in current scope. \n";
      return LogErrorV(s.c_str());
    }
  }

  auto vType = Builder.CreateLoad(VName, Identifier->getVal())->getType();
  auto expType = Val ->getType();

  // stop float being stored in int
  if(vType ==  Type::getInt32Ty(TheContext) && expType == Type::Type::getFloatTy(TheContext)){
    return LogErrorV("Cannot assign float to int");
  }

  // stop int being stored in bool
  if(vType ==  Type::getInt1Ty(TheContext) && expType == Type::Type::getInt32Ty(TheContext)){
    return LogErrorV("Cannot assign int to bool");
  }

  // upgrade int to be stored in float
  if(vType == Type::Type::getFloatTy(TheContext) && expType ==  Type::getInt32Ty(TheContext)){
    Val = Builder.CreateSIToFP(Val, Type::getFloatTy(TheContext), "upgradeRHStoFloat");
  }

  // downgrade float from comparison operation to bool (1 or 0)
  if(vType == Type::Type::getInt1Ty(TheContext) && expType ==  Type::getFloatTy(TheContext)){
    Val = Builder.CreateSIToFP(Val, Type::getFloatTy(TheContext), "upgradeRHStoFloat");
    Val = Builder.CreateFPToSI(Val, Type::Type::getInt1Ty(TheContext), "downgradeRhsToInt");
  }

  Builder.CreateStore(Val, VName);
  return Val;
}

Value *FunctionCallASTnode::codegen(){
  Function *F = TheModule->getFunction(Identifier->getVal());

  if(!F){
    return nullptr;
  }

  if(F->arg_size() != Arguments.size()){
    return LogErrorV("Incorrect number of arguments");
  }

  std::vector<Value *> ArgsCodeGen;
  for (int i = 0; i < (int)Arguments.size(); i++){
    ArgsCodeGen.push_back(Arguments[i]->codegen());
    if(!ArgsCodeGen.back())
      return nullptr;
  }
  return Builder.CreateCall(F, ArgsCodeGen, "calltmp");
}

Function *FunctionBuilderASTnode::codegen(){
  std::vector<Type*> paramTypes;
  TOKEN Tok;

  if(Parameters != nullptr){
    for (int i = 0; i < (int)Parameters->Variables.size(); i++){
      Tok.type = Parameters->Variables[i]->getType();
      switch(Tok.type){
        case INT_TOK:
          paramTypes.push_back(Type::getInt32Ty(TheContext));
          break;
        case FLOAT_TOK:
          paramTypes.push_back(Type::getFloatTy(TheContext));
          break;
        case BOOL_TOK:
          paramTypes.push_back(Type::getInt1Ty(TheContext));
          break;
        default:
          break;
      }
    }
  }
  Type* returnType;
  Tok.type = ReturnType->getType();
  switch(Tok.type){
    case INT_TOK:
      returnType = Type::getInt32Ty(TheContext);
      break;
    case FLOAT_TOK:
      returnType = Type::getFloatTy(TheContext);
      break;
    case BOOL_TOK:
      returnType = Type::getInt1Ty(TheContext);
      break;
    case VOID_TOK:
      returnType = Type::getVoidTy(TheContext);
      break;
    default:
      return nullptr;
  }

  FunctionType *FType = FunctionType::get(returnType, paramTypes, false);

  Function *F = Function::Create(FType, Function::ExternalLinkage, Identifier->getVal(), TheModule.get());

  unsigned Idx = 0;
  for (auto &Arg : F->args())
      Arg.setName(Parameters->Variables[Idx++]->getName());

  return F;
}

Function *FunctionASTnode::codegen(){
  Function *F  = TheModule -> getFunction(FBuilder->getVal());

  if(!F){
    F = FBuilder->codegen();
  }

  if(!F){
    return nullptr;
  }

  BasicBlock *BB = BasicBlock::Create(TheContext, "block", F);
  Builder.SetInsertPoint(BB);

  // add args to NamedValues
  NamedValues.clear();
  for (auto &Arg : F->args()){
    AllocaInst *Alloca = CreateEntryBlockAlloca(F, Arg.getName(), Arg.getType());
    Builder.CreateStore(&Arg, Alloca);
    NamedValues[Arg.getName()] = Alloca;
  }

  // build return value of function
  Value *RetVal = Statements->codegen();
  Builder.CreateRet(RetVal);

  verifyFunction(*F);

  return F;
}

Value *WhileStmtASTnode::codegen(){
  Function *F = Builder.GetInsertBlock()->getParent();
  BasicBlock *ConditionBlock = BasicBlock::Create(TheContext, "condition", F);
  BasicBlock *LoopBlock = BasicBlock::Create(TheContext, "whileloop", F);
  BasicBlock *PostLoopBlock = BasicBlock::Create(TheContext, "postloop", F);
  Builder.CreateBr(ConditionBlock);
  Builder.SetInsertPoint(ConditionBlock);

  Value *Condition = Expression->codegen();
  if(!Condition){
    return nullptr;
  }

  Condition = Builder.CreateFCmpONE(Condition, ConstantFP::get(TheContext, APFloat(0.0)), "loopcondition");
  Builder.CreateCondBr(Condition, LoopBlock, PostLoopBlock);

  Builder.SetInsertPoint(LoopBlock);

  if(!Statement->codegen())
      return nullptr;
  Builder.CreateBr(ConditionBlock);

  Builder.SetInsertPoint(PostLoopBlock);

  return Constant::getNullValue(Type::getFloatTy(TheContext));
}

Value *IfStmtASTnode::codegen(){
  Value *Condition = Expression->codegen();

  if(!Condition){
    return nullptr;
  }

  // Upgrade bool value from comparison to float
  if(Condition->getType() == Type::getInt1Ty(TheContext)){
      Condition = Builder.CreateSIToFP(Condition, Type::getFloatTy(TheContext), "upgradeExptoFloat");
  }
  Condition = Builder.CreateFCmpONE(Condition, ConstantFP::get(TheContext, APFloat(0.0)), "ifcondition\n");

  Function *F = Builder.GetInsertBlock()->getParent();

  BasicBlock *IfBlock = BasicBlock::Create(TheContext, "ifblock", F);
  BasicBlock *ElseBlock = BasicBlock::Create(TheContext, "elseblock");
  BasicBlock *PostBlock = BasicBlock::Create(TheContext, "postif");

  // if there is an else statement
  if(IfElseStmts->getElseStmt()){
    Builder.CreateCondBr(Condition, IfBlock, ElseBlock);

    Builder.SetInsertPoint(IfBlock);
    Value *IfVal = IfElseStmts->getIfStmt()->codegen();
    if(!IfVal){
      return nullptr;
    }
    Builder.CreateBr(PostBlock);
    IfBlock = Builder.GetInsertBlock();

    F->getBasicBlockList().push_back(ElseBlock);
    Builder.SetInsertPoint(ElseBlock);
    Value *ElseVal;
    ElseVal =  IfElseStmts->getElseStmt()->codegen();
    if(!ElseVal){
      return nullptr;
    }

    Builder.CreateBr(PostBlock);

    ElseBlock = Builder.GetInsertBlock();

    F->getBasicBlockList().push_back(PostBlock);
    Builder.SetInsertPoint(PostBlock);
    PHINode *PN;
    if(IfVal->getType() == Type::getFloatTy(TheContext)){
      PN = Builder.CreatePHI(Type::getFloatTy(TheContext), 2, "iftmp");
    }
    else if(IfVal->getType() == Type::getInt32Ty(TheContext)){
        PN = Builder.CreatePHI(Type::getInt32Ty(TheContext), 2, "iftmp");
    }
    else if(IfVal->getType() == Type::getInt1Ty(TheContext)){
      PN = Builder.CreatePHI(Type::getInt1Ty(TheContext), 2, "iftmp");
    }
    else{
      return LogErrorV("Error in creating phi node.");
    }

    PN->addIncoming(IfVal, IfBlock);
    PN->addIncoming(ElseVal, ElseBlock);
    return PN;
  }
  // if there is not an else statement
  else{
    Builder.CreateCondBr(Condition, IfBlock, PostBlock);
    Builder.SetInsertPoint(IfBlock);

    Value *IfVal = IfElseStmts->getIfStmt()->codegen();

    if(!IfVal){
      return nullptr;
    }

    Builder.CreateBr(PostBlock);
    IfBlock = Builder.GetInsertBlock();
    F->getBasicBlockList().push_back(PostBlock);
    Builder.SetInsertPoint(PostBlock);
    return IfVal;
  }
}

Value *ReturnStmtASTnode::codegen(){
  if(Expression != nullptr){
    return Expression->codegen();
  }
  else{
    return nullptr;
  }
}

Value *BlockASTnode::codegen(){
  Value *returnStmt;
  std::vector<AllocaInst*> Tmp;

  if((int)LocalDecls.size() != 0){
    Function *F = Builder.GetInsertBlock()->getParent();
    TOKEN Tok;
    Type* type;
    Value * val;

    for (int i = 0 ; i < (int)LocalDecls.size(); i++){
      Tok.type = LocalDecls[i]->getType();
      switch(Tok.type){
        case INT_TOK:
          type = Type::getInt32Ty(TheContext);
          val = ConstantInt::get(TheContext, APInt(32,0));
          break;
        case FLOAT_TOK:
          type = Type::getFloatTy(TheContext);
          val = ConstantFP::get(TheContext, APFloat(0.0));
          break;
        case BOOL_TOK:
          type = Type::getInt1Ty(TheContext);
          val = ConstantInt::get(TheContext, APInt(1,0));
          break;
      }
      AllocaInst *Alloca = CreateEntryBlockAlloca(F, LocalDecls[i]->getName(), type);
      Builder.CreateStore(val, Alloca);

      //store outer scope variables until needed again
      Tmp.push_back(NamedValues[LocalDecls[i]->getName()]);
      //set to block variables
      NamedValues[LocalDecls[i]->getName()] = Alloca;
    }
  }

  for (int i = 0 ; i < (int)Statements.size(); i++){
    returnStmt = Statements[i]->codegen();
  }

  for (int i = 0 ; i < (int)LocalDecls.size(); i++){
    // set back to outer scope variables
    NamedValues[LocalDecls[i]->getName()] = Tmp[i];
  }

  return returnStmt;
}

Value *ProgramASTnode::codegen(){
  Value* decl_list;
  for (int i = 0; i < (int)ExtList.size(); i++){
    ExtList[i]->codegen();
  }
  for (int i = 0; i < (int)DeclList.size(); i++){
    decl_list = DeclList[i]->codegen();
  }

  return decl_list;
}

//===----------------------------------------------------------------------===//
// AST Printerelse
//===----------------------------------------------------------------------===//

inline raw_ostream &operator<<(raw_ostream &os, const ASTnode &ast){
  os << ast.to_string();
  return os;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char **argv){
  if(argc == 2){
    pFile = fopen(argv[1], "r");
    if(pFile == NULL)
      perror("Error opening file");
  } else{
    std::cout << "Usage: ./code InputFile\n";
    return 1;
  }

  // initialize line number and column numbers to zero
  lineNo = 1;
  columnNo = 1;

//Use to print tokens
  /*
  while (CurTok.type != EOF_TOK){
    fprintf(stderr, "Token: %s with type %d\n", CurTok.lexeme.c_str(),
            CurTok.type);
    getNextToken();
  }
  fprintf(stderr, "Lexer Finished\n");
  */


  // get the first tokens to populate CurTok NextTok and NextNextTok
  getNextToken();
  getNextToken();
  getNextToken();

  // Make the module, which holds all the code.
  TheModule = make_unique<Module>("mini-c", TheContext);

  // Run the parser now.
  static std::unique_ptr<ASTnode> ASTgraphic = parser();
  if(err_flag || ASTgraphic == nullptr){
      fprintf(stderr, "Parsing Failed Exiting the Program Now.");
      return 1;
  }
  fprintf(stderr, "Parsing Finished\n");

  outs() << *ASTgraphic << '\n';
  std::cout << "END OF AST NODES." << std::endl;
  //********************* Start printing final IR **************************

  ASTgraphic->codegen();

  // Print out all of the generated code into a file called output.ll
  auto Filename = "output.ll";
  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::F_None);

  if(EC){
    errs() << "Could not open file: " << EC.message();
    return 1;
  }

  TheModule->print(errs(), nullptr); // print IR to terminal
  TheModule->print(dest, nullptr);
  //********************* End printing final IR ****************************

  fclose(pFile); // close the file that contains the code that was parsed
  return 0;
}
