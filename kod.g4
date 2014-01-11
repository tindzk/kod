/* Based upon:
 *  - http://media.pragprog.com/titles/tpantlr2/code/tour/Java.g4
 *  - http://media.pragprog.com/titles/tpantlr2/code/lexmagic/SimplePy.g4
 */
grammar kod;

@lexer::members {
  private int statementNesting = 0;
  private int indentationNesting = 0;
  java.util.Deque<Token> tokens = new java.util.LinkedList<>();

  @Override
  public Token nextToken() {
    if (tokens.isEmpty()) {
      tokens.offer(super.nextToken());

      Token t = tokens.peek();

      if (t.getType() == Token.EOF) {
        /* Add dedent tokens to the queue before the EOF token for each all
         * remaining un-closed blocks. */
        while (indentationNesting > 0) {
          indent(Dedent, t.getStartIndex(), t.getStopIndex());
        }

        line(t.getStartIndex(), t.getStopIndex());
      }
    }

    Token t = tokens.poll();

    if (t.getType() == Line) {
      tokens.offer(super.nextToken());
      Token next = tokens.poll();

      if (next.getType() == SpaceChars) {
        int n = next.getText().length();

        /* Skip all lines only containing indentation and no other tokens. */
        Token nextNext = super.nextToken();

        if (nextNext.getType() != Line) {
          while (n < indentationNesting) {
            indent(Dedent, next.getStartIndex(), next.getStopIndex());
          }

          while (n > indentationNesting) {
            indent(Indent, next.getStartIndex(), next.getStopIndex());
          }
        }

        tokens.offer(nextNext);
      } else {
        tokens.offerFirst(next);

        if (next.getType() != Line) {
          /* Reset indentation. */
          while (indentationNesting > 0) {
            indent(Dedent, next.getStartIndex(), next.getStopIndex());
          }
        }
      }
    }

    return t;
  }

  private void indent(int type, int start, int end) {
    indentationNesting += (type == Indent ? 1 : -1);

    Pair<TokenSource, CharStream> source = new Pair<TokenSource, CharStream>(this, _input);

    if (type == Dedent) {
      CommonToken t = new CommonToken(source, Semicolon, Token.DEFAULT_CHANNEL, start, end);
      t.setText("Injected statement separator");
      tokens.offerFirst(t);
    }

    CommonToken t = new CommonToken(source, type, Token.DEFAULT_CHANNEL, start, end);
    t.setText(type == Indent ? "Indent" : "Dedent");
    tokens.offerFirst(t);
  }

  /* Used so that we can always expect an empty line before the EOF. */
  private void line(int start, int end) {
    Pair<TokenSource, CharStream> source = new Pair<TokenSource, CharStream>(this, _input);
    CommonToken t = new CommonToken(source, Line, Token.DEFAULT_CHANNEL, start, end);
    t.setText("Injected line");
    tokens.offerFirst(t);
  }
}

compilationUnit
  : module? imports declarations statements EOF
  ;

module
  : separator* ('module' expression)? separator+
  ;

imports
  : (import_? separator)*
  ;

import_
  : 'import' Identifier ('.' Identifier)* '.' ('*' | ('{' importListAtom (',' importListAtom)* '}'))
  ;

importListAtom
  : Identifier ('as' Identifier)?
  ;

declarations
  : (declaration | separator)*
  ;

statements
  : (statement? separator)*
  ;

declaration
  : classDeclaration
  | objectDeclaration
  | enumDeclaration
  | traitDeclaration
  | typeDeclaration
  | functionDeclaration
  ;

typeDeclaration
  : 'type' Identifier Identifier? 'is' type codeBlock?
  ;

enumDeclaration
  : 'enum' Identifier 'is' (expression | enumMap)
  ;

traitDeclaration
  : 'trait' Identifier typeParameters? ('extends' type)?
    (':' separator Indent initialMemberDeclaration* memberDeclaration* Dedent)?
  ;

objectDeclaration
  : 'object' Identifier
    ('extends' type
      ('(' variableOrInitialiser (',' variableOrInitialiser)* ')')?
    )?
    (':' separator Indent initialMemberDeclaration* memberDeclaration* Dedent)?
  ;

classDeclaration
  : 'abstract'? 'class' Identifier
    typeParameters?
    ('(' parameterDeclaration (',' parameterDeclaration)* ')')?
    ('extends' type
      ('(' variableOrInitialiser (',' variableOrInitialiser)* ')')?
    )?
    ('with' type (',' type)*)?
    (':' separator Indent initialMemberDeclaration* memberDeclaration* Dedent)?
  ;

accessLevel
  : 'protected'
  | 'public'
  | 'private'
  ;

initialMemberDeclaration
  /* Declaration of a new member variable. */
  : accessLevel? 'read'? variableDeclaration separator+

  /* Class construction statement. */
  | statement separator+

  /* Skip empty lines. */
  | separator+
  ;

memberDeclaration
  : methodDeclaration
  | operatorDeclaration
  | separator+
  ;

/* Declare require and ensure for contracts. */
require
  : 'require:' separator Indent (statement? separator)* Dedent
  ;

ensure
  : 'ensure:' separator Indent (statement? separator)* Dedent
  ;

methodDeclaration
  : 'override'? functionDeclaration
  ;

decorator
  : '@' Identifier ('(' expressionList? ')')? separator+
  ;

/* self denotes an alternative constructor. */
functionDeclaration
  : decorator* 'def' typeParameters? type? (Identifier | 'self')
    '(' (parameterDeclaration (',' parameterDeclaration)*)? ')'
    codeBlock?
  ;

/* Operator overloading. */
operatorDeclaration
  : 'op' type? ('+' | '-' | '*' | '/' | '<' | '>' | '<=' | '>=')
    '(' (parameterDeclaration (',' parameterDeclaration)*)? ')'
    codeBlock?
  ;

variableOrInitialiser
  : Identifier
  | initialiser
  ;

/* In contrast to variableDeclaration this does not permit any arbitrary
 * expressions (cf. `initialiser').
 */
parameterDeclaration
  : type? Identifier ('=' initialiser)?
  ;

initialiser
  : literal
  | Identifier '(' (initialiser (',' initialiser)*)? ')'
  ;

separator
  : Line
  | Semicolon
  ;

Semicolon
  : ';'
  ;

typeParameter
  : ('+' | '-')? type ('extends' type)?
  ;

typeParameters
  : '<' typeParameter (',' typeParameter)* '>'
  ;

/* A variable must always be initialised with a value. */
variableDeclaration
  /* Typed lambda function. */
  : (type | 'let') Identifier '(' (parameterDeclaration (',' parameterDeclaration)*)? ')' codeBlock

  /* Variable declaration without type inference. */
  | type Identifier ('[' tuple ']')? '=' expression

  /* Variable declaration with type inference. */
  | 'let' (Identifier | tuple) ('[' tuple ']')? '=' expression
  ;

codeBlock
  : ':' statementBlock
  | '=' expressionBlock
  ;

statementBlock
  : separator*
    Indent
      require?
      separator*
      ensure?
      (statement? separator)*
    Dedent
  | statement
  ;

expressionBlock
  : separator*
    Indent
      separator*
      expression
      separator*
    Dedent
  | expression
  ;

statement
  /* NOP statement needed due to indentation. */
  : 'pass'

  /* Assertions. */
  | 'assert' expression

  /* for loop: The expression must be a range, list, iterator or generator. */
  | 'for' untypedParameters 'in' expression (('where' | 'while') expression)? ':' statementBlock

  /* while loop: The expression must be a condition (i.e. Boolean expression). */
  | 'while' expression ':' statementBlock

  /* repeat loop: The expression must be an Integer expression. If the expression
   * is omitted, this equals an endless loop.
   */
  | 'repeat' expression? ':' statementBlock

  /* Local variable declaration. */
  | variableDeclaration

  /* Value assignment. */
  | expression '=' expression

  /* Short-cirucit expression. The LHS must evaluate to a Boolean value. */
  | expression 'or' controlStatement

  /* Some expression. */
  | expression

  /* Increase/decrease statement. */
  | expression ('++' | '--')

  /* Control statement. */
  | controlStatement
  ;

controlStatement
  : 'return' expression?

  /* Throws an exception. */
  | 'raise' expression

  /* In generators this returns the given value to the callee. */
  | 'yield' expression

  /* Loop control statements; skip is called `continue' in other languages. */
  | ('skip' | 'break')
  ;

type
  /* Lambda type. */
  : type '(' (type Identifier? (',' type Identifier?)*)? ')'

  /* List. */
  | '[' type ']'

  /* Map. */
  | '{' type ':' type (',' type ':' type)* '}'

  /* Tuple. */
  | '(' type (',' type)+ ')'

  /* Nullable type. */
  | type '?'

  /* Class name, instantiating the type parameters. */
  | Identifier typeParameters?

  /* Member access. */
  | type ('.' type)+

  /* Return type for methods that do not return anything. */
  | 'Void'
  ;

list
  : '[' expressionList? ']'
  ;

/* Tuple have at least two elements. */
tuple
  : '(' tupleItemList ')'
  ;
tupleItemList
  : tupleItem? (',' tupleItem?)+
  ;
tupleItem
  : expression
  | Identifier ':' expression
  ;

/* Dictionaries. */
map
  : '{' mapItemList? '}'
  ;
mapItemList
  : mapItem (',' mapItem)*
  ;
mapItem
  : literal ':' expression
  ;

/* Map for enumerations. */
enumMap
  : '{' enumMapItemList '}'
  ;
enumMapItemList
  : enumMapItem (',' enumMapItem)*
  ;
enumMapItem
  : Identifier ('=' literal)?
  ;

withAtom
  : Identifier untypedParameters? '=' expressionBlock
  | Identifier untypedParameters? ':' statementBlock
  ;

with
  : 'with' withAtom (',' withAtom)*
  | 'with:' separator+ Indent (withAtom | separator)* Dedent
  ;

arguments
  : '(' expressionList? ')' with?
  ;

expression
  : literal
  | Identifier
  | type
  | list
  | tuple
  | map
  | 'self'
  | (Identifier | type) arguments
  | expression ('.' | '?.') Identifier arguments?
  | expression '?' expression ':' expression
  | expression '=>' expression /* Implication. */
  | expression '[' expression ']'

  /* Unary expressions */
  | '-' expression
  | 'not' expression
  | 'typeOf' expression

  /* Binary overloadable expressions */
  | expression ('*' | '/' | 'mod') expression
  | expression ('+' | '-') expression /* + for appending, adding. */
  | expression ':' expression /* Prepending. */
  | expression ('<=' | '>=' | '<' | '>') expression
  | expression 'and' expression
  | expression 'or' expression
  | expression 'xor' expression
  | expression 'nor' expression
  | expression
    ('=='<assoc=right>
    |'!='<assoc=right>
    |'<=>'<assoc=right>
    )
    expression

  /* Binary non-overloadable expressions */
  | expression '$' expression /* String interpolation. */
  | expression '?:' (expression | controlStatement) /* Elvis operator; expression must be nullable. */
  | expression 'in' expression
  | expression 'is' expression
  | expression 'as' expression
  | expression '..' expression?
  | expression
    ('==='<assoc=right>
    |'!=='<assoc=right>)
    expression

  /* Other expressions. */
  | 'if' expression ':' statementBlock
    (separator* 'else' expression ':' statementBlock)*
    (separator* 'else:' statementBlock)?
  | 'try:' statementBlock
    (separator* 'catch' (type Identifier?)? ':' statementBlock)+
    (separator* 'finally:' statementBlock)?
  | 'match' expression ':'
    separator+ Indent (matchCase | separator)* Dedent
  | untypedParameters '->' expression /* Lambda expression. */
  | '[' expression 'for' untypedParameters 'in' expression ('if' expression)? ']' /* List comprehension */
  | '(' expression ')'
  ;

/* Defined recursively to support tuple unpacking independently from the depth. */
untypedParameters
  : Identifier
  | '(' (untypedParameters (',' untypedParameters)*)? ')'
  ;

typedTuple
  : type Identifier?
  | '(' typedTuple (',' typedTuple)* ')'
  ;

matchAtom
  : typedTuple Identifier?
  | expression Identifier?
  ;

matchCase
  : 'is' matchAtom ':' statementBlock
  | 'in' list Identifier? ':' statementBlock
  | 'or:' statementBlock
  ;

expressionList
  : expression (',' expression)*
  ;

literal
  : IntegerLiteral
  | FloatingPointLiteral
  | HexLiteral
  | CharacterLiteral
  | StringLiteral
  | 'None'
  | 'True' | 'False'
  | '_'
  ;

/* See http://www.antlr.org/wiki/display/ANTLR4/Grammar+Lexicon */
fragment IdentifierStart
  : Letter
  | '\u00C0' .. '\u00D6'
  | '\u00D8' .. '\u00F6'
  | '\u00F8' .. '\u02FF'
  | '\u0370' .. '\u037D'
  | '\u037F' .. '\u1FFF'
  | '\u200C' .. '\u200D'
  | '\u2070' .. '\u218F'
  | '\u2C00' .. '\u2FEF'
  | '\u3001' .. '\uD7FF'
  | '\uF900' .. '\uFDCF'
  | '\uFDF0' .. '\uFFFD'
  ;

fragment IdentifierCharacter
  : IdentifierStart
  | Digit
  | '_'
  | '\u00B7'
  | '\u0300' .. '\u036F'
  | '\u203F' .. '\u2040'
  ;

/* Unicode identifiers. */
Identifier
  : IdentifierStart IdentifierCharacter*
  ;

IntegerLiteral
  : Digit+ Exponent?
  ;

Digit
  : [0-9]
  ;

Letter
  : [a-zA-Z]
  ;

fragment HexDigit
  : '0'..'9'
  | 'a'..'f'
  ;

FloatingPointLiteral
  : Digit+ '.' Digit+ Exponent?
  ;

HexLiteral
  : '0x' HexDigit+
  ;

fragment Exponent
  : 'e' ('+' | '-')? Digit+
  ;

CharacterLiteral
  : '\'' (CharacterEscapeSequence | ~('\'' | '\\')) '\''
  ;

StringLiteral
  : '"' (StringEscapeSequence | ~('\\' | '"'))* '"'
  ;

fragment CharacterEscapeSequence
  : '\\' ('b' | 't' | 'n' | 'f' | 'r' | '\'' | '\\')
  ;

fragment StringEscapeSequence
  : '\\' ('b' | 't' | 'n' | 'f' | 'r' | '\"' | '\\' | '$')
  ;

LeftParenthesis
  : '(' { statementNesting++; }
  ;

RightParenthesis
  : ')' { statementNesting--; }
  ;

LeftSquareBracket
  : '[' { statementNesting++; }
  ;

RightSquareBracket
  : ']' { statementNesting--; }
  ;

LeftCurlyBracket
  : '{' { statementNesting++; }
  ;

RightCurlyBracket
  : '}' { statementNesting--; }
  ;

/* Ignore backslash line sequences. This disallows comments after the backslash
 * because a newline must occur next.
 */
EscapedLine
  : '\\' '\r'? '\n' '\t'* -> skip
  ;

/* Nested newline within a (...), [...] or {...} are ignored. */
IgnoreLine
  : ('\r'? '\n' | '\r') {statementNesting > 0}? -> skip
  ;

/* Let the lexer recognise newlines. These must not be redirected to a HIDDEN
 * channel as some rules rely on the Line token during the parsing phase.
 */
Line
  : '\r'? '\n' | '\r'
  ;

// Match anything between /* and */.
Comment
  : '/*' .*? '*/' -> channel(HIDDEN)
  ;

/* Filter out all comments. */
LineComment
  : '#' ~[\r\n]* -> channel(HIDDEN)
  ;

/* Toss out whitespaces. */
Whitespaces
  : ' '+ -> skip
  ;

/* Treat one or more tabs as a token representing a sequence of indentation
 * characters.
 */
SpaceChars
  : '\t'+ -> channel(HIDDEN)
  ;

/* Create fake tokens. */
Indent : {false}? .;
Dedent : {false}? .;