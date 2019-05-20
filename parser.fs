// NEEL PATEL
// Parser for simple C programs.  This component checks
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a tuple containing
// 3 values:
//
//   (result, msg, program)
//
// where result is true or false (legal or not legal),
// msg is a success or syntax error message, and program
// is a list of instructions if parsing was successful.
//
// Prof. Joe Hummel
// U. of Illinois, Chicago
// CS 341, Spring 2019
// Project #05: Solution
//
// Modifed by: << YOUR NAME >>
// Project #08
//

#light

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // These are debug routines that output the tokens, or
  // program, respectively.  They are written so you can
  // inject these into a pipeline to output the current
  // state of the tokens or program.
  //
  let private __outputTokens (tokens, program) =
    printfn "Tokens: %A" tokens
    (tokens, program)

  let private __outputProgram (tokens, program) =
    printfn "Program: %A" program
    (tokens, program)


  //
  // matchToken
  //
  let private matchToken expected_token (tokens, program) =
    let (nextToken, _) = List.head tokens
    //
    // if the token matches the expected token, keep parsing by
    // returning the rest of the tokens.  Otherwise throw an
    // exception because there's a syntax error, effectively
    // stopping compilation:
    //
    if expected_token = nextToken then
      (List.tail tokens, program)
    else
      failwith ("expecting " + (string expected_token) + ", but found " + (string nextToken))

  //
  // <vardecl> -> int ID ;
  //
  let private vardecl (tokens, program) =
    let (newT, newP) =
      (tokens, program)
      |> matchToken lexer.Tokens.Int
      |> matchToken lexer.Tokens.ID
      |> matchToken lexer.Tokens.Semicolon
    //
    // Since the stmt is legal, we know the 2nd token is
    // the identifer, e.g. (ID, "x").  We need the "x".
    //
    let (_, identifier) = List.head (List.tail tokens)
    (newT, ["$DECL"; identifier] :: newP)


  //
  // <inputstmt> -> cin >> ID ;
  //
  let private inputstmt (tokens, program) =
    let (newT, newP) =
      (tokens, program)
      |> matchToken lexer.Tokens.Cin
      |> matchToken lexer.Tokens.Input
      |> matchToken lexer.Tokens.ID
      |> matchToken lexer.Tokens.Semicolon
    //
    // Since the stmt is legal, we know the 3rd token is
    // the identifer, e.g. (ID, "x").  We need the "x".
    //
    let (_, identifier) = List.head (List.tail (List.tail tokens))
    (newT, ["$INPUT"; identifier] :: newP)


  //
  // <output-value> -> <expr-value>
  //                   | endl
  //
  let private outputvalue (tokens, program) =
    let (nextToken, value) = List.head tokens
    //
    if nextToken = lexer.Tokens.ID           ||
       nextToken = lexer.Tokens.Str_Literal  ||
       nextToken = lexer.Tokens.Int_Literal  ||
       nextToken = lexer.Tokens.Bool_Literal ||
       nextToken = lexer.Tokens.Endl then
      let newT = List.tail tokens
      let newP = ["$OUTPUT"; (string nextToken); value] :: program
      (newT, newP)
    else
      failwith ("expecting identifier or literal or endl, but found " + (string nextToken))


  //
  // <outputstmt> -> cout << <output-value> ;
  //
  let private outputstmt (tokens, program) =
    (tokens, program)
    |> matchToken lexer.Tokens.Cout
    |> matchToken lexer.Tokens.Output
    |> outputvalue
    |> matchToken lexer.Tokens.Semicolon


  //
  // <expr-value> -> ID
  //               | Int_Literal
  //               | Str_Literal
  //               | Bool_Literal
  //
  let private expr_value (tokens, program) =
    let (nextToken, value) = List.head tokens
    //
    if nextToken = lexer.Tokens.ID          ||
       nextToken = lexer.Tokens.Int_Literal ||
       nextToken = lexer.Tokens.Str_Literal ||
       nextToken = lexer.Tokens.Bool_Literal then
      let instr = List.head program
      let newInstr = instr @ [(string nextToken); value];
      let newT = List.tail tokens
      let newP = newInstr :: (List.tail program)
      (newT, newP)
    else
      failwith ("expecting identifier or literal, but found " + (string nextToken))

  //
  // <expr> -> <expr-value>
  //           | <expr-value> <op> <expr-value>
  //
  let private expr (tokens, program) =
    let (newT, newP) = expr_value (tokens, program)
    //
    let (nextToken, op) = List.head newT
    //
    // is this a binary expression?  It is if the next token
    // is one of the simpleC operators:
    //
    if nextToken = lexer.Tokens.Plus   ||
       nextToken = lexer.Tokens.Minus  ||
       nextToken = lexer.Tokens.Times  ||
       nextToken = lexer.Tokens.Divide ||
       nextToken = lexer.Tokens.Power  ||
       nextToken = lexer.Tokens.LT     ||
       nextToken = lexer.Tokens.LTE    ||
       nextToken = lexer.Tokens.GT     ||
       nextToken = lexer.Tokens.GTE    ||
       nextToken = lexer.Tokens.EQ     ||
       nextToken = lexer.Tokens.NE then
      let instr = List.head newP
      let newInstr = instr @ [op];  // append expr operator, collect right-hand side:
      expr_value (List.tail newT, newInstr :: (List.tail newP))
    else
      (newT, newP)


  //
  // <condition> -> <expr>
  //
  let condition (tokens, program) =
    expr (tokens, program)


  //
  // <assignment> -> ID = <expr> ;
  // assignmentInternal doesn't expect a semi-colon
  //
  let private assignmentInternal (tokens, program) =
    let (newT, newP) =
      (tokens, program)
      |> matchToken lexer.Tokens.ID
      |> matchToken lexer.Tokens.Assign
    //
    // we have the valid start of an assignment stmt, so let's
    // start a new instruction so we can collect the expr as
    // part of this instr.  Then we'll parse the expression.
    //
    let (_, identifier) = List.head tokens
    //
    (newT, ["$ASSIGN"; identifier] :: newP)
      |> expr

  //
  // assignment uses assignment internal followed by a semi-colon
  //
  let private assignment (tokens, program) =
    (tokens, program)
    |> assignmentInternal
    |> matchToken lexer.Tokens.Semicolon


  //
  // <emptystmt> -> ;
  //
  let private emptystmt (tokens, program) =
    let (newT, newP) = matchToken lexer.Tokens.Semicolon (tokens, program)
    (newT, ["$EMPTY"] :: newP)


  //
  // <stmt> -> <vardecl>
  //           | <inputstmt>
  //           | <outputstmt>
  //           | <assignment>
  //           | <ifstmt>
  //           | <emptystmt>
  //
  let rec private stmt (tokens, program) =
    let (nextToken, _) = List.head tokens
    match nextToken with
    | lexer.Tokens.Int  -> vardecl (tokens, program)
    | lexer.Tokens.Cin  -> inputstmt (tokens, program)
    | lexer.Tokens.Cout -> outputstmt (tokens, program)
    | lexer.Tokens.ID   -> assignment (tokens, program)
    | lexer.Tokens.If   -> ifstmt (tokens, program)
    | lexer.Tokens.For  -> forstmt (tokens, program)
    | lexer.Tokens.Semicolon -> emptystmt (tokens, program)
    | _ -> failwith ("expecting statement, but found " + (string nextToken))

  //
  // <ifstmt> is mutually-recursive since <then-part> and <else-part>
  // recursively refer to <stmt>:
  //
  // <then-part> -> <stmt>
  //
  and private thenpart (tokens, program) =
    (tokens, program)
    |> stmt

  //
  // <else-part> -> <stmt>
  //                | EMPTY
  //
  and private elsepart (tokens, program) =
    let (nextToken, _) = List.head tokens
    //
    if nextToken = lexer.Tokens.Else then
      (tokens, program)
      |> matchToken lexer.Tokens.Else
      |> stmt
    else
      (tokens, ["$EMPTY"] :: program)

  //
  // <ifstmt> -> if ( <condition> ) <then-part> <else-part>
  //
  and private ifstmt (tokens, program) =
    let (T1, P1) =
      (tokens, program)
      |> matchToken lexer.Tokens.If
      |> matchToken lexer.Tokens.OpenParen
    //
    // we have the valid start of an if stmt, so let's
    // start a new instruction so we can collect the
    // condition:
    //
    let (T2, P2) =
      (T1, ["$IF"] :: P1)
      |> condition
      |> matchToken lexer.Tokens.CloseParen
    //
    // now we need to collect the then and else parts,
    // which appear as separate instructions.  Note
    // that if else-part is missing, an EMPTY instr is
    // added so there always 2 instructions following
    // the condition.
    //
    (T2, P2)
    |> thenpart
    |> elsepart

  //
  // <init> -> <assignment>
  //           | EMPTY
  //
  and private forinit (tokens, program) =
    let (nextToken, _) = List.head tokens
    match nextToken with
    | lexer.Tokens.ID        -> assignment (tokens, program)
    | lexer.Tokens.Semicolon -> emptystmt (tokens, program)
    | _ -> failwith ("expecting assignment or empty statment, but found " + (string nextToken))

  //
  // <update> -> <assignment>
  //           | EMPTY
  //
  and private forupdate (tokens, program) =
    let (nextToken, _) = List.head tokens
    match nextToken with
    | lexer.Tokens.ID         -> assignmentInternal (tokens, program)
    | lexer.Tokens.CloseParen -> (tokens, ["$EMPTY"] :: program)
    | _ -> failwith ("expecting assignment or empty statment, but found " + (string nextToken))

  //
  // <forloop> -> for ( <init> ; <condition> ; init<update> ) <stmt>
  //
  and private forstmt (tokens, program) =
    let (T1, P1) =
      (tokens, program)
      |> matchToken lexer.Tokens.For
      |> matchToken lexer.Tokens.OpenParen
    //
    // we have the valid start of a for stmt, so let's
    // start a new instruction $FOR and collect the
    // initialisation statment:
    //
    let (T2, P2) =
      (T1, ["$FOR"] :: P1)
      |> forinit
    //
    // now we need to collect the condition
    // we will collect it in its own instruction
    // we also match against the semi colon after the condition
    //
    let (T3, P3) =
      (T2, [] :: P2)
      |> condition
      |> matchToken lexer.Tokens.Semicolon
    //
    // now we need to collect update statment and
    // match against the closing parenthese
    //
    let (T4, P4) =
      (T3, P3)
      |> forupdate
      |> matchToken lexer.Tokens.CloseParen
    //
    // Finally we will collect the stmt in the body
    // of the for loop
    //
    (T4, P4)
    |> stmt

  //
  // <morestmts> -> <stmt> <morestmts>
  //                | EMPTY
  //
  let rec private morestmts (tokens, program) =
    let (nextToken, _) = List.head tokens
    //
    if nextToken = lexer.Tokens.Int  ||
       nextToken = lexer.Tokens.Cin  ||
       nextToken = lexer.Tokens.Cout ||
       nextToken = lexer.Tokens.ID   ||
       nextToken = lexer.Tokens.If   ||
       nextToken = lexer.Tokens.For  ||
       nextToken = lexer.Tokens.Semicolon then  // we have the start of a stmt:
      (tokens, program)
      |> stmt
      |> morestmts
    else
      (tokens, program)


  //
  // <stmts> -> <stmt> <morestmts>
  //
  let private stmts (tokens, program) =
    (tokens, program)
    |> stmt
    |> morestmts


  //
  // <simpleC> -> void main ( ) { <stmts> }
  //
  let private simpleC (tokens, program) =
    (tokens, program)
    |> matchToken lexer.Tokens.Void
    |> matchToken lexer.Tokens.Main
    |> matchToken lexer.Tokens.OpenParen
    |> matchToken lexer.Tokens.CloseParen
    |> matchToken lexer.Tokens.OpenBrace
    |> stmts
    |> matchToken lexer.Tokens.CloseBrace
    |> matchToken lexer.Tokens.EOF


  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // a tuple containing 3 values:
  //
  //   (result, msg, program)
  //
  // where result is true or false (legal or not legal),
  // msg is a success or syntax error message, and program
  // is a list of instructions if parsing was successful.
  //
  let parse tokens =
    try
      let (_, program) = simpleC (tokens, [])
      (true, "success", List.rev program)
    with
      | ex -> (false, ex.Message, [])
