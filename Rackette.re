open CS17SetupRackette;
open Read.Reader;
open Types;


/*
Data Definitions

rawProgram:
a string that represents code from a racket program 
examples: "(+ 4 8)", "(- 13 19)", "(define x 7)", "(define fact (lambda (x) (if (zero? x) 1 (* x (fact ( − x 1)))))) (fact 3)"

concreteProgramPiece
- either a NumberC(int), a SymbolC(string), or a ListC(list(concreteProgramPiece)). 
examples: NumberC(5), SymbolC("*"), ListC(Symbol("*"), Number(5), Number(9))

concreteProgram
- a list of concrete program pieces
examples: [SymbolC("+"), NumberC(10), ListC(SymbolC("/"), NumberC(15), NumberC(5))] , [NumberC(9)]

name
- represented as Name(string), and is used for all sequences of non-special characters that are not numbers,
  booleans or keywords. It is an identifying string
examples: Name("x"), Name("y")

expression
- an expression has a very wide range of ways it can look. This is the list of variations
  | NumE(int)                    
  | BoolE(bool)                  
  | EmptyE                        
  | NameE(name)                     
  | AndE(expression, expression)    
  | OrE(expression, expression)
  | IfE(ifData)
  | CondE(list(condData)) 
  | LambdaE(lambdaData)
  | LetE(letData)
  | ApplicationE(list(expression))
Where:
 ifData is made up of a boolExpr a trueExpr and a falseExpr, which are all expressions.
 condData is made up of a conditionExpr and a resultExpr, which are both expressions.
 lambdaData is made up of a nameList, and a lambdaBody, which are of types list(name) and expression respectively.
 letPair is made up of a pairName, and a pairExpr, which are of types name and expression respectively.
 letData is made up of a letPairs, and a letBody, which are of types list(letPair) and expression respectively.

examples: NumE(7), BoolE(true), Empty

definition 
- A definition contains the keyword define, followed by a name and an expression, all enclosed in parentheses, as a tuple.
examples: (define f (lambda (x) (+ x 2))), (define x empty)

abstractProgramPiece
- an abstractProgramPiece is either:
  | Definition(definition)
  | Expression(expression);
examples: Definition(NameE("x"), BoolE(false)), Expression(NumE(9))

abstractProgram
- a list of abstractProgramPieces
- represents a Racket program in Rackette
examples:[Definition(NameE("x"), BoolE(false)), Expression(NumE(9))], [BoolE(true)]

value
- the "output", ie the result of evaluating a rackette expression. The types can be shown as 
type value =
  | NumV(int)
  | BoolV(bool)
  | ListV(list(value))
  | BuiltinV(builtinData)
  | ClosureV(closureData)
  and builtinData = { 
    bName: string,
    bProc: list(value) => value,
  }
  and closureData = {
    cNameList: list(name),
    cExpr: expression, 
    cEnv: environment,
  }
examples: NumV(2), BoolV(false)

environment
- an environment is a list of bindings
examples:[(NameE("hello"), NumV(1)), (NameE("y"), NumV(2))]

binding
- a binding is a tuple that contains a name and a value. bindings make up environments
examples: (NameE("hello"), NumV(3)), (NameE("y"), NumV(4))
*/

/* TODO: fill this with your initial top level environment,
 * consisting of built-in procedures like + or empty? */

/*
lookup: name * environment * environment -> value

Input: 
nm, a name
tle, the top level environment
env, an environment (typically the local environment)

Output:
the value, v, associated with the input name nm. Lookup will begin by looking in the local environment and then move to the tle
if there name is not already bound lookup will output None. (Checks whether a name is already predefined)

lookup RD:
OI: NameE("a"), initialTle, [NameE("a"), NumV(1), NameE("b"), NumV(2)]
RI: NameE("a"), initialTle, [NumV(1), NameE("b"), NumV(2)]
RO: None
iterate through the local environment looking for nm, if not found, iterate through the tle, if not found, output None
OO: Some(1)

OI: NameE("b"), initialTle, [NameE("a"), NumV(1), NameE("b"), NumV(2)]
RI: NameE("b"), initialTle, [NumV(1), NameE("b"), NumV(2)]
RO: Some(2)
iterate through the local environment looking for nm, if not found, iterate through the tle, if not found, output None
OO: Some(2)
*/

let rec lookup: (name, environment, environment) => option(value) = (nm, tle, env) =>
  switch(env) {
    |[(str, v), ...tl] => if (nm == str) {Some(v)} else {lookup(nm, tle, tl)};
    |[] => switch(tle) {
      | [(str, v), ...tl] => if (nm == str) {Some(v)} else {lookup(nm, tl, [])}; 
      | [] => None
    }
  }
/* -------------------------------- Arithmetic TLE--------------------------------*/

/*
plus: list(value) -> value

Input: alov, a list of values

Output: the sum of the two input values. 
If given an invalid input, a failwith statement will be output stating the error. 
This happens when the input list does not strictly contain two NumVs.
*/

let plus: list(value) => value = alov =>
  switch(alov){
    |[NumV(x), NumV(y)] => NumV(x + y)
    |_ => failwith("+ requires a list of two integer values")
  };

/*
minus: list(value) -> value

Input: alov, a list of values

Output: the result of subtracting the two input values. 
If given an invalid input, a failwith statement will be output stating the error. 
This happens when the input list does not strictly contain two NumVs.
*/
let minus: list(value) => value = alov =>
  switch(alov){
    |[NumV(x), NumV(y)] => NumV(x - y)
    |_ => failwith("- requires two integer values")
  };

/*
times: list(value) -> value

Input: alov, a list of values

Output: the result of multiplying the two input values. 
If given an invalid input, a failwith statement will be output stating the error. 
This happens when the input list does not strictly contain two NumVs.
*/

let times: list(value) => value = alov =>
  switch(alov){
    |[NumV(x), NumV(y)] => NumV(x * y)
    |_ => failwith("* requires two integer values")
  };

/*
divide: list(value) -> value

Input: alov, a list of values

Output: the result of dividing the two input values. 
If given an invalid input, a failwith statement will be output stating the error. 
This happens when the input list does not strictly contain two NumVs.
*/

let divide: list(value) => value = alov =>
  switch(alov){
    |[NumV(x), NumV(y)] => NumV(x / y)
    |_ => failwith("/ requires two integer values")
  };

/*
rem: list(value) -> value

Input: alov, a list of values

Output: the remainder of the two input values. 
If given an invalid input, a failwith statement will be output stating the error. 
This happens when the input list does not strictly contain two NumVs.
*/

let rem: list(value) => value = alov =>
  switch(alov){
    |[NumV(x), NumV(y)] => NumV(x - ((x / y) * y))
    |_ => failwith("remainder requires two integer values")
  };
/* -------------------------------- Equality TLE--------------------------------*/

/*
equalNum: list(value) => value

Input: alov, a list of values

Output: if the input list has two equal NumVs, BoolV(true) will be output. 
If the input instead has two NumVs which are not equal, BoolV(false) will be output.
If given an invalid input, a failwith statement will be output stating the error. 
This happens when the input list does not strictly contain two NumVs.
*/

let equalNum: list(value) => value = alov =>
  switch(alov){
    |[NumV(x), NumV(y)] => if (x == y) {BoolV(true)} else {BoolV(false)};
    |_ => failwith("= requires two integer values")
  };

/*
isLess: list(value) => value

Input: alov, a list of values

Output: When the input list has two equal NumVs, if the second NumV is greater than the first, BoolV(true) will be output. 
If the first NumV is greater than the second, BoolV(false) will be output.
If given an invalid input, a failwith statement will be output stating the error. 
This happens when the input list does not strictly contain two NumVs.
*/

let isLess: list(value) => value = alov =>
  switch(alov){
    |[NumV(x), NumV(y)] => if (x < y) {BoolV(true)} else {BoolV(false)};
    |_ => failwith("< requires two integer values")
  };

/*
isGreater: list(value) => value

Input: alov, a list of values

Output: When the input list has two equal NumVs, if the second NumV is greater than the first, BoolV(false) will be output. 
If the first NumV is greater than the second, BoolV(true) will be output.
If given an invalid input, a failwith statement will be output stating the error. 
This happens when the input list does not strictly contain two NumVs.
*/

let isGreater: list(value) => value = alov =>
  switch(alov){
    |[NumV(x), NumV(y)] => if (x > y) {BoolV(true)} else {BoolV(false)};
    |_ => failwith("> requires two integer values")
  };

/*
lessOrEqual: list(value) => value

Input: alov, a list of values

Output: When the input list has two equal NumVs, if the second NumV is greater or equal than the first, BoolV(true) will be output. 
If the first NumV is greater than the second, BoolV(false) will be output.
If given an invalid input, a failwith statement will be output stating the error. 
This happens when the input list does not strictly contain two NumVs.
*/

let lessOrEqual: list(value) => value = alov =>
  switch(alov){
    |[NumV(x), NumV(y)] => if (x <= y) {BoolV(true)} else {BoolV(false)};
    |_ => failwith("<= requires two integer values")
  };



let greaterOrEqual: list(value) => value = alov =>
  switch(alov){
    |[NumV(x), NumV(y)] => if (x >= y) {BoolV(true)} else {BoolV(false)};
    |_ => failwith(">= requires two integer values")
  };

/* -------------------------------- Query TLE--------------------------------*/

/*
equalAnyQ: list(value) => value

Input: alov, a list of values

Output: When the input list has two arguments, if the arguments are of equal type, BoolV(true) will be output. 
If the argument are not of equal type, BoolV(false) will be output.
If given an invalid input, a failwith statement will be output stating the error. 
This happens when the input list does not strictly contain two arguments.
*/

let equalAnyQ: list(value) => value = alov =>
  switch(alov){
    |[NumV(x), NumV(y)] => if (x == y) {BoolV(true)} else {BoolV(false)};
    |[BoolV(x), BoolV(y)] => if (x == y) {BoolV(true)} else {BoolV(false)};
    |[ListV(x), ListV(y)] => if (x == y) {BoolV(true)} else {BoolV(false)};
    |_ => failwith("equal? requires an input list with strictly two arguments")
  };

/*
isConsQ: list(value) => value

Input: alov, a list of values of type ListV

Output: if the input is of type ListV, and populated, BoolV(true) will be output. 
If the input is of type ListV, and empty, BoolV(false) will be output.
If given an invalid input, a failwith statement will be output stating the error. 
This happens when the input is not of type list(value).
*/

let isConsQ: list(value) => value = alov =>
  switch(alov){
    |[ListV(x)] => if (x ==[]) {BoolV(false)} else {BoolV(true)};
    |_ => failwith("cons? requires a value list")
  };

/*
numberQ: list(value) => value

Input: alov, a list of values of types NumV, BoolV, ListV

Output: When the input list has value of type NumV, BoolV(true) will be output. 
If the input list has value of BoolV or ListV, BoolV(false) will be output.
If given an invalid input, a failwith statement will be output stating the error. 
This happens when the input list does not strictly contain one of the previously mentioned data types.
*/

let numberQ: list(value) => value = alov =>
  switch(alov){
    |[NumV(_)] => BoolV(true)
    |[BoolV(_)] => BoolV(false)
    |[ListV(_)] => BoolV(false)
    |_ => failwith("number? requires a one element list containing a value")
  };

/*
zeroQ: list(value) => value

Input: alov, a list of values of type NumV

Output: When the input list has value of type NumV, and the NumV is non-zero, BoolV(true) will be output. 
If the input list has value of type NumV, which is zero, BoolV(false) will be output.
If given an invalid input, a failwith statement will be output stating the error. 
This happens when the input list is not of type NumV.
*/

let zeroQ: list(value) => value = alov =>
  switch(alov){
    |[NumV(x)] => if (x == 0) {BoolV(true)} else {BoolV(false)};
    |_ => failwith("zero? requires an integer value")
  };

/*
emptyQ: list(value) => value

Input: alov, a list of values of type ListV

Output: if the input is of type ListV, and empty, BoolV(true) will be output. 
If the input is of type ListV, and populated, BoolV(false) will be output.
If given an invalid input, a failwith statement will be output stating the error. 
This happens when the input is not of type list(value).
*/

let emptyQ: list(value) => value = alov =>
  switch(alov){
    |[ListV(x)] => if (x ==[]) {BoolV(true)} else {BoolV(false)};
    |_ => failwith("empty? requires a value list")
  };

/* -------------------------------- List Manipulation TLE--------------------------------*/

/*
firstOfL: list(value) => value

Input: alov, a list of values of type ListV

Output: if the input is a ListV with at least one element, first will output the value of the first element (aka the head) in the list
as a ListV. If given an invalid input, a failwith statement will be output stating the error. 
This happens when the input is not strictly a ListV with at least one element.
*/

let firstOfL: list(value) => value = alov =>
  switch(alov){
    |[ListV[hd, ... _]] => ListV([hd])
    |[ListV([])] => failwith("first requires a list of at least one value")
    |_ => failwith("first requires a list of values")
  };

/*
restOfL: list(value) => value

Input: alov, a list of values of type ListV

Output: if the input is a ListV with at least one element, rest will output the value of the tail of the input list
as a ListV. If given an invalid input, a failwith statement will be output stating the error. 
This happens when the input is not strictly a ListV with at least one element.
*/

let restOfL: list(value) => value = alov =>
  switch(alov){
    |[ListV([_, ...tl])] => ListV(tl)
    |[ListV([])] => failwith("rest requires a list of at least one value")
    |_ => failwith("rest requires a list of values")
  };

/*
notProcL: list(value) => value

Input: alov, a list of values of type BoolV

Output: if the input is a ListV with value BoolV(true), output false. If the input is a ListV with value BoolV(false), output true.
If given an invalid input, a failwith statement will be output stating the error. 
This happens when the input is not strictly a ListV with a boolean value.
*/

let notProcL: list(value) => value = alov =>
  switch(alov){
    |[BoolV(x)] => if (x == true) {BoolV(false)} else {BoolV(true)};
    | _ => failwith("notProcL requires a boolean value")
  };

/*
consAddL: list(value) => value

Input: alov, a value and a list of values (in that order)

Output: cons the input value onto the input list of values
If given an invalid input, a failwith statement will be output stating the error. 
This happens when the input is not strictly a value and ListV.
*/

let consAddL: list(value) => value = alov =>
  switch(alov){
    |[x, ListV(y)] => ListV([x, ...y])
    |_=> failwith("consAddL requires a list of one value and one list of values (in that order)")
  };


let initialTle: environment = [
  (Name("+"), BuiltinV({bName: "plus", bProc: plus})),
  (Name("-"), BuiltinV({bName: "minus", bProc: minus})),
  (Name("*"), BuiltinV({bName: "times", bProc: times})),
  (Name("/"), BuiltinV({bName: "divide", bProc: divide})),
  (Name("remainder"), BuiltinV({bName: "remainder", bProc: rem})),

  (Name("="), BuiltinV({bName: "=", bProc: equalNum})),
  (Name("<"), BuiltinV({bName: "<", bProc: isLess})),
  (Name(">"), BuiltinV({bName: ">", bProc: isGreater})),
  (Name("<="), BuiltinV({bName: "<=", bProc: lessOrEqual})),
  (Name(">="), BuiltinV({bName: ">=", bProc: greaterOrEqual})),
  
  (Name("equal?"), BuiltinV({bName: "equal?", bProc: equalAnyQ})),
  (Name("cons?"), BuiltinV({bName: "cons?", bProc: isConsQ})),
  (Name("number?"), BuiltinV({bName: "number?", bProc: numberQ})),
  (Name("zero?"), BuiltinV({bName: "zero?", bProc: zeroQ})),
  (Name("empty?"), BuiltinV({bName: "empty?", bProc: emptyQ})),
  
  (Name("first"), BuiltinV({bName: "first", bProc: firstOfL})),
  (Name("rest"), BuiltinV({bName: "rest", bProc: restOfL})),
  (Name("not"), BuiltinV({bName: "not", bProc: notProcL})),
  (Name("cons"), BuiltinV({bName: "cons", bProc: consAddL})),
];



/* ------------------------------- parseExpression ---------------------------------
 * Input: input, a concreteProgramPiece
 * Output: consumes a Rackette program piece represented as a concreteProgramPiece and produces an expression
 * TODO: Continue building out for all expression cases
 
parseExpression RD:

OI: ListC([ListC([SymbolC("if"), SymbolC("true")]), NumberC(10), NumberC(20)])
RI: [ListC([SymbolC("if"), SymbolC("true")]), NumberC(10), NumberC(20)]
RO: IfE(BoolE(true), NumE(10), NumE(20))
iterate through running parseExpression on embedded portions of the input
OO: IfE(BoolE(true), NumE(10), NumE(20))

OI: ListC([ListC([SymbolC("if"), SymbolC("true")]), NumberC(20), NumberC(30)])
RI: [ListC([SymbolC("if"), SymbolC("true")]), NumberC(20), NumberC(30)]
RO: IfE(BoolE(true), NumE(20), NumE(30))
iterate through running parseExpression on embedded portions of the input
OO: IfE(BoolE(true), NumE(20), NumE(30))

*/

let rec parseExpression: concreteProgramPiece => expression =
  input => switch(input) {
    | SymbolC("empty") => EmptyE
    | NumberC(a) => NumE(a)
    | SymbolC(b) => switch(b) {
      | "true" => BoolE(true)
      | "false" => BoolE(false)
      | _ => NameE(Name(b))
    };
    | ListC([ListC([SymbolC("if"), b]), tr, fl]) => IfE({boolExpr: parseExpression(b), trueExpr: parseExpression(tr), falseExpr: parseExpression(fl)});
    | ListC([SymbolC("lambda"), ListC(a), b]) => LambdaE({nameList: lambdaHelper(a), lambdaBody: parseExpression(b)});
    | ListC([SymbolC("cond"), ListC(condList)]) => CondE(condHelper(condList))
    | ListC([SymbolC("let"), ListC(a), x]) => LetE{letPairs: letHelper(a), letBody: parseExpression(x)}
    | ListC([SymbolC(a), b, c]) => switch(a) {
          | "and" => AndE(parseExpression(b), parseExpression(c))
          | "or" => OrE(parseExpression(b), parseExpression(c))
          | _ => ApplicationE([NameE(Name(a)), parseExpression(b), parseExpression(c)])
    };
    | ListC([SymbolC(a), x]) => ApplicationE([NameE(Name(a)), parseExpression(x)])
    | _ => failwith("incorrect syntax")
  } // condHelper takes a list of concreteProgramPieces and outputs a list of expression tuples (condData)
  and condHelper: list(concreteProgramPiece) => list(condData) = aloc => 
   switch(aloc) {
     | [] => []
     | [ListC([a, b]), ..._tl] => [{conditionExpr: parseExpression(a), resultExpr: parseExpression(b)}, ...condHelper(List.tl(aloc))];
     | _ => failwith("format not fitted")
   } //lambdaHelper takes in a list of concreteProgramPieces and outputs a list of names 
   and lambdaHelper: list(concreteProgramPiece) => list(name) = alon =>
    switch(alon) {
    | [] => []
    | [SymbolC(a), ...tl] => [Name(a), ... lambdaHelper(tl)]
    | _ => failwith("incorrect inputs")
    } //letHelper takes in a list of concreteProgram Pieces and turns it into a list of bindings
    and letHelper: list(concreteProgramPiece) => list(letPair) = pairs =>
    switch(pairs) {
      | [] => []
      | [ListC([SymbolC(a), x]), ...tl] => [{pairName: Name(a), pairExpr: parseExpression(x)}, ... letHelper(tl)]
      | _ => failwith("wrong input")
    };
/* Check Expects*/

checkExpectExpression(parseExpression(read("(+ (- 2 1) 3)")),
ApplicationE([NameE(Name("+")),ApplicationE([NameE(Name("-")), NumE(2), NumE(1)]), NumE(3)]),
"ApplicationE test 1");
checkExpectExpression(parseExpression(read("true")), BoolE(true), "parse Rackette boolean expression");
checkExpectExpression(parseExpression(read("false")), BoolE(false), "parse Rackette boolean expression");
checkExpectExpression(parseExpression(read("9")), NumE(9), "parse Rackette number expression");
checkExpectExpression(parseExpression(read("empty")), EmptyE, "parse Rackette empty expression");
checkExpectExpression(parseExpression(read("(let ((x 1)(y 2)) 12)")),
LetE({letPairs:[{pairName: Name("x"), pairExpr: NumE(1)}, {pairName: Name("y"), pairExpr: NumE(2)}], letBody: NumE(12)}), "let test 1");
//checkExpectExpression(parseExpression(read("(cond (false 9) (true 8))")),
//CondE({condData:({conditionExpr: BoolE(false)}, {resultExpr: NumE(9)}, {conditionExpr: BoolE(true)}, {resultExpr: NumE(8)})}), "double conditional expression");
//checkExpectExpression(parseExpression(read("((lambda (y)(+ 1 y))17)")),
//ApplicationE([LambdaE({lambdaData:[{nameList: Name("y")}, {lambdaBody: ApplicationE([NameE(Name("+")), NumE(1), NameE(Name("y"))])}, NumE(17)]})]), "lambda test 1")

checkExpect(parseExpression(ListC([SymbolC("let"), ListC([ListC([SymbolC("x"), NumberC(17)]), ListC([SymbolC("f"), ListC([SymbolC("+"), SymbolC("x"), NumberC(12)])])]), ListC([SymbolC("*"), SymbolC("x"), NumberC(2)])])),
LetE({letPairs:
       [{pairName: Name("x"), pairExpr: NumE(17)},
        {pairName: Name("f"),
         pairExpr:
          ApplicationE([NameE((Name("+"))), NameE((Name("x"))), NumE(12)])}],
      letBody: ApplicationE([NameE((Name("*"))), NameE((Name("x"))), NumE(2)])}), "check Expect let expression")


/* ------------------------------- parseDefinition ---------------------------------
 * Input: input, a concreteProgramPiece which must be a ListC with 3 entries, the 
 * first of which being a SymbolC("define")
 * Output: a definition representing a named expression
 */
let parseDefinition: concreteProgramPiece => definition =
  input => switch(input) {
    | ListC([SymbolC("define"), SymbolC(b), c]) => (Name(b), parseExpression(c))
    | ListC([SymbolC("define"), ListC([SymbolC(a),..._]), c]) => (Name(a), parseExpression(c))
    | _ => failwith("incorrect syntax")
  };

/* ------------------------------- parsePiece ---------------------------------
 * Input: input, a concreteProgramPiece
 * Output: an abstractProgramPiece which is either a Definition or
 * an Expression depending on whether it is a ListC with first element
 * SymbolC("define") or not
 */
let parsePiece: concreteProgramPiece => abstractProgramPiece =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), ..._tl]) =>
      Definition(parseDefinition(input))
    | _ => Expression(parseExpression(input))
    };

/* ------------------------------- parse ---------------------------------
 * Input: input, a concreteProgram
 * Output: an abstractProgram which is either a Definition that is equivalent to the input concreteProgram
 */
let parse: concreteProgram => abstractProgram =
  input =>
    /* this will parse all of the pieces of this program,
     * giving us a list of pieces, our abstract syntax */
    List.map(parsePiece, input);

/*
Eval: (Evaluates our final Rackette Value)
environment * environment * expression => value

Input: Two environments, (TLE, and env), and an input expression.
Output: The value of that expression found in one of the environments.
*/
let rec eval: (environment, environment, expression) => value =
  (tle, env, expr) => switch(expr) {
  | NumE(a) => NumV(a)
  | BoolE(a) => BoolV(a)
  | EmptyE => ListV([])
  | NameE(Name(a)) => switch(lookup(Name(a), tle, env)) {
    | Some(a) => a
    | None => failwith("no binding in either environment: " ++ a)
  }
  | AndE(x, y) => switch(eval(tle, env, x), eval(tle, env, y)) {
    | (BoolV(a), BoolV(b)) => BoolV(a && b)
    | _ => failwith("no boolean bindings found")
  } 
  | OrE(x, y) => switch(eval(tle, env, x), eval(tle, env, y)) {
    | (BoolV(a), BoolV(b)) => BoolV(a || b)
    | _ => failwith("no boolean bindings found")
  }
  | IfE(a) => switch(eval(tle, env, a.boolExpr)) {
    | BoolV(x) when (x == true) => eval(tle, env, a.trueExpr)
    | BoolV(x) when (x == false) => eval(tle, env, a.falseExpr)
    | _ => failwith("boolExpr did not produce a boolean value")
  }
  | CondE(a) => switch(a) {
    | [] => failwith("incomplete cond matching")
    | [hd, ...tl] => switch(eval(tle, env, hd.conditionExpr)) {
      | BoolV(a) when (a == true) => eval(tle, env, hd.resultExpr);
      | BoolV(a) when (a == false) => eval(tle, env, CondE(tl))
      | _ => failwith("conditionExpr does not evaluate to bool");
    }
  }
  | LambdaE(lmbd) => ClosureV({cNameList: lmbd.nameList, cExpr: lmbd.lambdaBody, cEnv: env})
  | LetE(ldt) => eval(tle, letHelperEval(env, ldt.letPairs), ldt.letBody);
  | ApplicationE([a, ...tl]) => switch(eval(tle, env, a)) {
    | BuiltinV(a) => a.bProc(informalToActual(tl, env))
    | ClosureV(a) => eval(tle, closureHelper(a.cNameList, tl), a.cExpr)
    | _ => failwith("first argument is not a procedure value")
  }
  | _ => failwith("doesnt match specs")
  } // This helper takes in letPairs and an environment to turn into an environment the added binding
  and letHelperEval: (environment, list(letPair)) => environment = (env, ldt) =>
  switch(ldt) {
    | [] => env
    | [hd,...tl] => [(hd.pairName, eval(initialTle, env, hd.pairExpr)), ... letHelperEval(env, tl)]
  } // This helper takes in a list of expressions and an environment to turn into a list of values
  and informalToActual: (list(expression), environment) => list(value) = (x, env) =>
  switch(x) {
    | [] => []
    | [hd, ...tl] => [eval(initialTle, env, hd), ...informalToActual(tl, env)]
  } // This helper takes in a list of names and a list of expressions to turn into an environment
  and closureHelper: (list(name), list(expression)) => environment = (x, expr) =>
  switch(x, expr) {
    | ([], []) => []
    | ([], [_, ..._]) => failwith("arguments didn't match actuals")
    | ([_, ..._], []) => failwith("arguments didn't match actuals")
    | ([hd, ...tl], [hd2, ...tl2]) => [(hd, eval(initialTle, [], hd2)), ...closureHelper(tl, tl2)]
};

/*
addDefinition: (environment, (name, expression)) => environment

Input:
env, an environment
nm, a name
express, an expression

Output: 
adds a new definition, fails if the name is already defined
*/

let  addDefinition: (environment, (name, expression)) => environment = (env, (nm, express)) =>
  switch(lookup(nm, initialTle, env)){
    | None => [(nm, eval(initialTle, env, express)), ...env]
    | Some(_) => failwith("has been previously defined")
};

/*
stringOfValue: value => string 

Input:
u, a value

Output: 
a string that represents that value

stringOfValue RD:

OI: ListV(NumV(1), NumV(2))
RI: NumV(1), NumV(2)
RO: "1 2"
iterate through the input creating a string version of the inputs and compilling them all into a singular string
OO: "1 2"

OI: ListV(NumV(5), NumV(6))
RI: NumV(5), NumV(6)
RO: "5 6"
iterate through the input creating a string version of the inputs and compilling them all into a singular string
OO: "5 6"

*/

let rec stringOfValue: value => string = u =>
switch(u) {
  | NumV(a) => string_of_int(a);
  | BoolV(a) => string_of_bool(a);
  | ListV(a) => switch(a) {
    | [] => "empty"
    | [hd, ...tl] => "(cons " ++ stringOfValue(hd) ++ stringOfValue(ListV(tl))
  };
  | BuiltinV(a) => a.bName
  | ClosureV(a) => ("(" ++ stringHelper(a.cNameList) ++ ")" ++ stringOfValue(eval(initialTle, [], a.cExpr)))
} // this helper helps with closures and takes a list(name) and outputs a string
and stringHelper: list(name)  => string = lst =>
switch(lst) {
 | [] => ""
 | [Name(a), ...tl] => a ++ stringHelper(tl)
}

/* process: this procedure processes the abstract program representation of a Rackette program following the
Rackette rules of processing

process: abstractProgram => list(value)

Input: pieces, an abstract program representation of a Rackette program
Output: the list of values corresponding to the evaluation of any expressions present in pieces

*/

let process: abstractProgram => list(value) =
  pieces => {
    let rec processHelper: (environment, abstractProgram) => list(value) =
      (tle, pieces) =>
        switch (pieces) {
        | [] => []
        | [Definition(d), ...tl] => processHelper(addDefinition(tle, d), tl)
        | [Expression(e), ...tl] => [eval(initialTle, tle, e), ...processHelper(tle, tl),]
        };
    processHelper(initialTle, pieces);
  };

/* rackette: this procedure will interpret a Rackette program and return its value as a string, if it has one

rackette: rawProgram => list(string) = program 

Input: program, a Rackette program represented as a raw program
Output: a list of the string representations of the evaluated Rackette expressions in programs 

*/

let rackette: rawProgram => list(string) =
  program => List.map(stringOfValue, process(parse(readAll(program))));
  
/* TODO: Test Cases (we have included a few sample check-expects) */
// sample test: parseExpression on concreteProgramPiece
checkExpectExpression(parseExpression(SymbolC("empty")), EmptyE,
  "parse empty expression");
// sample test: parseExpression with read
checkExpectExpression(parseExpression(read("empty")), EmptyE,
  "read and parse empty expression");
checkExpect(eval(initialTle, [], LambdaE({nameList: [Name("x"), Name("y")], lambdaBody: ApplicationE([NameE(Name("*")), NameE(Name("x")), NameE(Name("y"))])})), 
ClosureV({cNameList: [Name("x"), Name("y")], cExpr: ApplicationE([NameE((Name("*"))), NameE((Name("x"))), NameE((Name("y")))]), cEnv: []}), "checkExpect Lambda")

checkExpect(eval(initialTle, [], ApplicationE([LambdaE({nameList: [Name("x"), Name("y")], lambdaBody: ApplicationE([NameE(Name("*")), NameE(Name("x")), NameE(Name("y"))])}), NumE(3), NumE(4)])), NumV(12), "ProcApp Lambda")
checkExpect(eval(initialTle, [], LetE({letPairs: [{pairName: Name("x"), pairExpr: NumE(5)}, {pairName: Name("y"), pairExpr: NumE(6)}], letBody: ApplicationE([NameE(Name("+")), NameE(Name("x")), NameE(Name("y"))])})), NumV(11), "Let Expression")
checkExpect(process([Definition((Name("x"), NumE(5))), Expression(ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(12)]))]), [NumV(17)], "checkExpect process")

checkExpect(stringOfValue(plus([NumV(1), NumV(2)])),"3","plus test");
checkError(() => stringOfValue(plus([BoolV(true), NumV(7)])),"+ expects two int values");
checkExpect(stringOfValue(minus([NumV(1), NumV(2)])),"-1","minus test");
checkError(() => stringOfValue(minus([BoolV(true), NumV(7)])),"- expects two int values");
checkExpect(stringOfValue(times([NumV(1), NumV(2)])),"2","times test");
checkError(() => stringOfValue(times([BoolV(true), NumV(7)])),"* expects two int values");
checkExpect(stringOfValue(divide([NumV(2), NumV(2)])),"1","divide test");
checkError(() => stringOfValue(divide([BoolV(true), NumV(7)])),"/ expects two int values");
checkExpect(stringOfValue(rem([NumV(7), NumV(2)])),"1","rem test");
checkError(() => stringOfValue(rem([BoolV(true), NumV(7)])),"remainder expects two int values");
checkExpect(stringOfValue(equalNum([NumV(7), NumV(2)])),"false","equalNum test");
checkError(() => stringOfValue(equalNum([BoolV(true), NumV(7)])),"= expects two int values");
checkExpect(stringOfValue(isLess([NumV(7), NumV(2)])),"false","isLess test");
checkError(() => stringOfValue(isLess([BoolV(true), NumV(7)])),"< expects two int values");
checkExpect(stringOfValue(isGreater([NumV(7), NumV(2)])),"true","isGreater test");
checkError(() => stringOfValue(isGreater([BoolV(true), NumV(7)])),"> expects two int values");
checkExpect(stringOfValue(lessOrEqual([NumV(7), NumV(2)])),"false","lessOrEqual test");
checkError(() => stringOfValue(lessOrEqual([BoolV(true), NumV(7)])),"<= expects two int values");
checkExpect(stringOfValue(greaterOrEqual([NumV(7), NumV(2)])),"true","greaterOrEqual test");
checkError(() => stringOfValue(greaterOrEqual([BoolV(true), NumV(7)])),">= expects two int values");
checkExpect(stringOfValue(equalAnyQ([BoolV(true), BoolV(true)])),"true","equalAnyQ test");
checkError(() => stringOfValue(equalAnyQ([BoolV(true), NumV(7)])),"equal? expects two values of same type");
checkExpect(stringOfValue(numberQ([NumV(7)])),"true","numberQ test");
checkError(() => stringOfValue(numberQ([BoolV(true), NumV(7)])),"number? expects a list containing one value");
checkExpect(stringOfValue(zeroQ([NumV(7)])),"false","zero test");
checkError(() => stringOfValue(zeroQ([BoolV(true)])),"zero? expects an integer value");
checkExpect(stringOfValue(consAddL([NumV(7), ListV([NumV(2), NumV(3)])])),"(cons 7(cons 2(cons 3empty)","consOn test");
checkError(() => stringOfValue(consAddL([NumV(3)])),"consOn expects a list of one list of values and one value");
checkExpect(stringOfValue(emptyQ([ListV([])])),"true","empty test");
checkError(() => stringOfValue(emptyQ([NumV(3)])), "empty? expects a value list");
checkExpect(stringOfValue(firstOfL([ListV([NumV(7), NumV(2)])])),"(cons 7empty)","firstOf test");
checkError(() => stringOfValue(firstOfL([NumV(3)])), "first expects a list of values");
checkError(() => stringOfValue(firstOfL([ListV([])])), "first expects a list of at least one value");
checkExpect(stringOfValue(restOfL([ListV([NumV(7), NumV(2)])])),"(cons 2empty)","restOf test");
checkError(() => stringOfValue(restOfL([ListV([])])), "rest expects a list of at least one value");
checkError(() => stringOfValue(restOfL([NumV(3)])), "rest expects a list of values");
checkExpect(stringOfValue(isConsQ([ListV([NumV(7), NumV(2)])])),"true","isCons test");
checkError(() => stringOfValue(isConsQ([NumV(7)])), "cons? expects a value list");
checkExpect(stringOfValue(notProcL([BoolV(true)])),"false","procNot test");
checkError(() => stringOfValue(notProcL([NumV(7)])), "procNot expects a boolean value");


  /* Test Cases */ 

  let testEnv: environment = [];
checkExpect(rackette("7"),["7"],"test 1");

checkExpect(rackette("true"),["true"],"test 2");

/*parse(readAll("((lambda(x y) (- x y)) 3 1)"));*/
checkExpect(rackette("(+ 2 2)"),["4"],"test 3");


checkExpectExpression(parseExpression(read("(+ (- 2 1) 3)")),
ApplicationE([NameE(Name("+")),ApplicationE([NameE(Name("-")), NumE(2), NumE(1)]), NumE(3)]),
"application test 1");
checkExpectExpression(parseExpression(read("true")), BoolE(true), "parse Rackette boolean expression");
checkExpectExpression(parseExpression(read("false")), BoolE(false), "parse Rackette boolean expression");
checkExpectExpression(parseExpression(read("90")), NumE(90), "parse Rackette # expression");
checkExpectExpression(parseExpression(read("empty")), EmptyE, "parse Rackette empty expression");

checkExpectExpression(parseExpression(read("(let ((x 1)(y 2)) 12)")),
LetE({letPairs:[{pairName: Name("x"), pairExpr: NumE(1)}, {pairName: Name("y"), pairExpr: NumE(2)}], letBody: NumE(12)}), "let test 1");
checkExpectExpression(parseExpression(read("(cond (false 9) (true 8))")),
//CondE({condData:[{conditionExpr: BoolE(false)}, {resultExpr: NumE(9)}, {conditionExpr: BoolE(true)}, {resultExpr: NumE(8)}]}), "double conditional expression");
//checkExpectExpression(parseExpression(read("((lambda (y)(+ 1 y))17)")),
//ApplicationE([LambdaE({lambdaData:[{nameList: Name("y")}, {lambdaBody: ApplicationE([NameE(Name("+")), NumE(1), NameE(Name("y"))])}, NumE(17)]})]), "lambda test 1")
//checkExpectExpression(parseExpression(read("(lambda (a b) false)")),
//LambdaE({lambdaData:[{nameList: [Name("a"), Name("b")]}, {lambdaBody: BoolE(false)}]}), "lambda test 1");
//checkExpectAbstractProgramPiece(parsePiece(read("(if true 1 1)")), IfE({ifData:[{boolExpr: BoolE(true)},{trueExpr:NumE(1)}, {falseExpr:NumE(1)}]}), "if test");


checkExpect((eval((initialTle, [], parseExpression(read("((lambda (x y) ((lambda (y)(+ x y))x))17 18)"))))),
NumV(34), "practice on paper 1")
checkExpect(eval((initialTle, [], parseExpression(read("((lambda (x y) ((lambda (x)(+ x y))x))17 18)")))),
NumV(35), "practice on paper 2");
checkExpect(eval((initialTle, [], parseExpression(read("((lambda (x y) ((lambda (x)(+ x y))y))17 18)")))),
NumV(36), "practice on paper 3");
checkExpect(eval((initialTle, testEnv, parseExpression(read("((lambda (x y) (- x y)) 5 1)")))),
NumV(4), "application of lambda eval test");
checkExpect(eval((initialTle, testEnv, parseExpression(read("(- (- 4 1) 2)")))),
NumV(1), "minus proc eval");
checkExpect(eval((initialTle, testEnv, parseExpression(read("(cond (false 1) (true 2))")))),
NumV(2),"cond statment with first argument false");
checkExpect(eval((initialTle, testEnv, parseExpression(read("(cond (true 1) (false 2))")))),
NumV(1),"cond statment with first argument true");
checkExpect(eval((initialTle, testEnv, parseExpression(read("(if true 4 6)")))),
NumV(4),"if statment eval true");
checkExpect(eval((initialTle, testEnv, parseExpression(read("(and true false)")))),
BoolV(false),"eval and expression");
checkExpect(eval((initialTle, testEnv, parseExpression(read("(or true false)")))),
BoolV(true),"eval or expression");
checkExpect(eval((initialTle, testEnv, parseExpression(read("empty")))),
ListV([]),"eval empty list");
checkExpect(eval((initialTle, testEnv, parseExpression(read("(if false 4 7)")))),
NumV(7),"if statment eval false");
checkExpect(rackette("((lambda (x y) (+ x y)) 1 3)"),["4"],"lambda test");
checkExpect(rackette("(if true 1 9)"),["1"],"if test"); 
checkExpect(rackette("(or (< 1 12) (false))"), ["true"],"or test");
checkExpect(rackette("(and (< 7 1) (zero? 0))"), ["false"],"and test");
checkExpect(rackette("(define r 11)(- r 9)"),["2"],"define test");
checkExpect(rackette("(cond(false 5)(true 6))"),["6"],"cond test");  