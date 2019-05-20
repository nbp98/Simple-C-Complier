//
// Lexical analyzer for simple C programs.  This component
// translates the input into lexical units or "tokens".
// NEEL PATEL
// Prof. Joe Hummel
// U. of Illinois, Chicago
// CS 341, Spring 2019
// Project #05
//

#light

namespace compiler

module lexer =
  //
  // NOTE: all types, functions in the module must be indented.
  //

  type Tokens =
      EOF = 1
    | Unknown = 2
    | OpenParen = 3
    | CloseParen = 4
    | OpenBrace = 5
    | CloseBrace = 6
    | Plus = 7
    | Minus = 8
    | Times = 9
    | Divide = 10
    | Assign = 11
    | LT = 12
    | LTE = 13
    | GT = 14
    | GTE = 15
    | EQ = 16
    | NE = 17
    | Input = 19
    | Output = 20
    | Not = 21
    | And = 22
    | Or = 23
    | Semicolon = 24
    | Power = 25
    | Void = 30
    | Main = 31
    | Int = 32
    | If = 33
    | Else = 34
    | Cout = 35
    | Cin = 36
    | Endl = 37
    | ID = 40
    | Int_Literal = 41
    | Str_Literal = 42
    | Bool_Literal = 43
    | For = 44

  let private keywords = [
      (Tokens.Void, "void");
      (Tokens.Main, "main");
      (Tokens.Int, "int");
      (Tokens.If, "if");
      (Tokens.Else, "else");
      (Tokens.Cout, "cout");
      (Tokens.Cin, "cin");
      (Tokens.Endl, "endl");
      (Tokens.Bool_Literal, "true");
      (Tokens.Bool_Literal, "false");
      (Tokens.For, "for");
    ]

  let rec private lookupKeyword id =
    let R = List.filter (fun (token,value) -> value = id) keywords
    if (List.length R) > 0 then
      List.head R
    else // not found, so it's an identifier:
      (Tokens.ID, id)

  let private nextChar (input:System.IO.StreamReader) =
    if input.EndOfStream then
      '$'
    else
      (char (input.Read()))

  let rec private skipRestOfLine input =
    match (nextChar input) with
    | '$'  -> ()
    | '\n' -> ()
    | '\r' -> ()
    |  _   -> skipRestOfLine input

  let rec private collectID nextc input id =
    match nextc with
    | _ when List.contains nextc (['a'..'z']@['A'..'Z']@['0'..'9']) ->
           collectID (nextChar input) input (id + (string nextc))
    | _ ->
           (id,nextc)

  let rec private collectStrLiteral nextc input literal =
    match nextc with
    | '"'  -> literal  // end of string "
    | '\n' -> literal  // end of line:
    | '\r' -> literal
    | '$'  -> literal  // end of file:
    |  _   -> collectStrLiteral (nextChar input) input (literal + (string nextc))

  let rec private collectIntLiteral nextc input literal =
    match nextc with
    | _ when List.contains nextc ['0'..'9'] ->
           collectIntLiteral (nextChar input) input (literal + (string nextc))
    | _ ->
           (literal,nextc)

  let rec private lexer nextc input tokens =
      match nextc with
      | '$' -> List.rev ((Tokens.EOF, "$") :: tokens)  // EOF:

      | ' ' -> lexer (nextChar input) input tokens   // whitespace:
      | '\t' -> lexer (nextChar input) input tokens
      | '\n' -> lexer (nextChar input) input tokens
      | '\r' -> lexer (nextChar input) input tokens

      | ';' -> lexer (nextChar input) input ((Tokens.Semicolon, ";") :: tokens)
      | '(' -> lexer (nextChar input) input ((Tokens.OpenParen, "(") :: tokens)
      | ')' -> lexer (nextChar input) input ((Tokens.CloseParen, ")") :: tokens)
      | '{' -> lexer (nextChar input) input ((Tokens.OpenBrace, "{") :: tokens)
      | '}' -> lexer (nextChar input) input ((Tokens.CloseBrace, "}") :: tokens)
      | '^' -> lexer (nextChar input) input ((Tokens.Power, "^") :: tokens)
      | '+' -> lexer (nextChar input) input ((Tokens.Plus, "+") :: tokens)
      | '-' -> lexer (nextChar input) input ((Tokens.Minus, "-") :: tokens)
      | '*' -> lexer (nextChar input) input ((Tokens.Times, "*") :: tokens)
      | '/' -> // could be division, or a comment:
               let lookahead = (nextChar input)
               if lookahead = '/' then
                 skipRestOfLine input
                 lexer (nextChar input) input tokens
               else
                 lexer lookahead input ((Tokens.Divide, "/") :: tokens)
      | '=' -> // could be =, or ==
               let lookahead = (nextChar input)
               if lookahead = '=' then
                 lexer (nextChar input) input ((Tokens.EQ, "==") :: tokens)
               else
                 lexer lookahead input ((Tokens.Assign, "=") :: tokens)
      | '<' -> // could be <, <=, or <<
               let lookahead = (nextChar input)
               if lookahead = '=' then
                 lexer (nextChar input) input ((Tokens.LTE, "<=") :: tokens)
               else if lookahead = '<' then
                 lexer (nextChar input) input ((Tokens.Output, "<<") :: tokens)
               else
                 lexer lookahead input ((Tokens.LT, "<") :: tokens)
      | '>' -> // could be >, >=, or >>
               let lookahead = (nextChar input)
               if lookahead = '=' then
                 lexer (nextChar input) input ((Tokens.GTE, ">=") :: tokens)
               else if lookahead = '>' then
                 lexer (nextChar input) input ((Tokens.Input, ">>") :: tokens)
               else
                 lexer lookahead input ((Tokens.GT, ">") :: tokens)
      | '!' -> // could be !, or !=
               let lookahead = (nextChar input)
               if lookahead = '=' then
                 lexer (nextChar input) input ((Tokens.NE, "!=") :: tokens)
               else
                 lexer lookahead input ((Tokens.Not, "!") :: tokens)
      | '&' -> // expecting &&
               let lookahead = (nextChar input)
               if lookahead = '&' then
                 lexer (nextChar input) input ((Tokens.And, "&&") :: tokens)
               else
                 lexer lookahead input ((Tokens.Unknown, "&") :: tokens)
      | '|' -> // expecting ||
               let lookahead = (nextChar input)
               if lookahead = '|' then
                 lexer (nextChar input) input ((Tokens.Or, "||") :: tokens)
               else
                 lexer lookahead input ((Tokens.Unknown, "|") :: tokens)

      | '"' -> let literal = collectStrLiteral (nextChar input) input ""  // "
               lexer (nextChar input) input ((Tokens.Str_Literal,literal) :: tokens)

      |  _  when List.contains nextc (['a'..'z']@['A'..'Z']) ->
               // collect letters and numbers into identifier, either keyword or identifier:
               let (id,lookahead) = collectID (nextChar input) input (string nextc)
               let (token,value) = lookupKeyword id
               lexer lookahead input ((token,value) :: tokens)

      |  _  when List.contains nextc ['0'..'9'] ->
               // collect digits into an integer literal:
               let (literal,lookahead) = collectIntLiteral (nextChar input) input (string nextc)
               lexer lookahead input ((Tokens.Int_Literal,literal) :: tokens)

      |  _  -> lexer (nextChar input) input ((Tokens.Unknown, (string nextc)) :: tokens)


  //
  // analyzer filename
  //
  // Given a filename representing a simple C program, returns
  // a list of (token, value) pairs.
  //
  let analyze (filename:string) =
    use input = new System.IO.StreamReader(filename)
    lexer (nextChar input) input []


