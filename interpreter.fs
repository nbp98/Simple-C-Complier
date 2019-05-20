//
// Interpreter for simple C programs.  This component
// executes the generated simple ASM program produced
// by the parser.
//
// << Neel Patel >>
// U. of Illinois, Chicago
// CS 341, Spring 2019
// Project #08
//

#light

namespace compiler

module interpreter =

  // Value: type to represent different C types
  type private Value =
    | Int of int
    | Str of string
    // If we wanted to implement a "proper" Boolean type, we could!
    // | Bool of bool

  // boolToInt:
  // Turn an integer to a boolean
  // Works according to C/C++ methodology
  let private boolToInt bool =
    if bool then 1 else 0

  // booleanifyExpr:
  // Turn an expression (after its evaluation) into
  // True or False according to C's rules:
  // - All ints are true except 0
  // - All strings are true except empty strings
  let private booleanifyExpr value =
    let number = match value with
                  | Str(concreteValue) -> String.length concreteValue
                  | Int(concreteValue) -> concreteValue

    number <> 0

  // findByID:
  // Searches memory for whether searchID
  // exists or not
  // Returns an option (Some or None)
  let private findByID memory searchID =
    List.tryFind (fun (id, _) -> id = searchID) memory

  // reassign:
  // Reassigns a specific ID in memory
  // Uses List.map to avoid mutability by creating
  // A new modified map with the modified id
  let private reassign memory id value =
    match findByID memory id with
    | Some(_) ->
      memory
      |> List.map (fun (cmp_id, old_value) -> if id = cmp_id then (id, value) else (cmp_id, old_value))
      |> Some
    | None -> None

  // expect:
  // fails with a specific error if option is None
  let private expect error option =
    match option with
    | Some(value) -> value
    | None -> failwith error

  //
  // smartDrop:
  // smartDrop updates the program by removing instructions
  // It smartly detects each instruction's "length" and recursively
  // drops values until nested instructions are dropped
  //
  let rec private smartDrop list =
      drop list 1

  and private drop list amount =
    if amount = 0 then
      list
    else
      match List.head (List.head list) with
      | "$DECL"    -> amount - 1
      | "$ASSIGN"  -> amount - 1
      | "$INPUT"   -> amount - 1
      | "$OUTPUT"  -> amount - 1
      | "$IF"      -> amount + 1
      | "$FOR"     -> amount + 3
      | "$EMPTY"   -> amount - 1
      // Quirky field for for's condition
      | "ID"       -> amount - 1
      | identifier -> failwith ("Attempted to drop: " + (string identifier) + ", but failed.")
      |> drop (List.tail list)

  //
  // update:
  // Simple function for simplifying  deling with tuples
  let private update program newMemory =
    (program, newMemory)

  //
  // ignoreProgram:
  // Sometimes we should ignore changes to program instructions
  // Since smartDrop is responsible for dropping everything for us
  // But we still want to perserve modified memory
  // Simple utility function
  let private ignoreProgram oldProgram (_, newMemory) =
    (oldProgram, newMemory)

  // exprVal:
  // exprVal (Expression Value): is a function that evaluates
  // values in an expression to the "Value" type
  let private exprVal (expression, memory) =
    // ["Int_Literal";    "5"; .....]
    //   ^^^^^^^^^^^      ^^^
    //   |                |
    //   | Discriminant   nextValue
    let nextDiscriminant = List.head expression
    let nextValue = List.head (List.tail expression)
    //
    // Turns nextValue into a Value type
    let value = match nextDiscriminant with
                | "ID" ->
                  nextValue
                  |> findByID memory
                  |> expect ("Can't find undefined variable: " + (string nextValue))
                  |> snd
                  |> Int
                | "Int_Literal" ->
                  nextValue
                  |> int
                  |> Int
                | "Str_Literal" ->
                  nextValue
                  |> string
                  |> Str
                | "Bool_Literal" ->
                  if nextValue = "true" then
                    Int(1)
                  else
                    Int(0)
                | "Endl" -> Str("\n")
                | _ -> failwith ("expecting identifier or literal, but found " + (string nextDiscriminant))

    // Remove Discriminant and value from expression and pass it back
    let newExpression = List.tail (List.tail expression)
    (newExpression, value)

  // evalExpr:
  // evalExpr (Evaluate Expression): is a function that evaluates
  // A full expression of left and right hand sides
  // Since the parser doesn't support more complex expressions anyway
  let rec private evalExpr memory expression =
    // Find left hand side value of expression
    let (newExpression, value) = exprVal (expression, memory)

    // If expression consists of a single value return it
    if List.isEmpty newExpression then
      value
    // Otherwise find right hand side and perform appropriate operation
    else
      let (_, rightValue) = exprVal (List.tail newExpression, memory)
      match (value, List.head newExpression, rightValue) with
      // Integers:
      | (Int(left), "+", Int(right)) -> Int(left + right)
      | (Int(left), "-", Int(right)) -> Int(left - right)
      | (Int(left), "*", Int(right)) -> Int(left * right)
      | (Int(left), "/", Int(right)) -> Int(left / right)
      | (Int(left), "^", Int(right)) -> Int(pown left right)
      | (Int(left), "<", Int(right)) -> Int(boolToInt (left < right))
      | (Int(left), "<=", Int(right)) -> Int(boolToInt (left <= right))
      | (Int(left), ">", Int(right)) -> Int(boolToInt (left > right))
      | (Int(left), ">=", Int(right)) -> Int(boolToInt (left >= right))
      | (Int(left), "=", Int(right)) -> Int(boolToInt (left = right))
      | (Int(left), "!=", Int(right)) -> Int(boolToInt (left <> right))
      //
      // String types can only be compared together with equality
      //
      | (Str(left), "=", Str(right)) -> Int(boolToInt (left = right))
      | (Str(left), "!=", Str(right)) -> Int(boolToInt (left <> right))
      | (Str(_), operator, Str(_)) -> failwith ("strings don't implement" + (string operator) + "operator")
      //
      // This is a demonstration of how Booleans could be treated
      // if they existed "properly" in our C-like language
      // It implicitly converts any boolean to 0 or 1
      // for all numeric and comparison operations (besides equality)
      // (like we do but during evaluation instead of operation)
      //
      // | (Bool(left), "+", Bool(right)) -> Int(boolToInt left + boolToInt right)
      // | (Bool(left), "-", Bool(right)) -> Int(boolToInt left - boolToInt right)
      // | (Bool(left), "*", Bool(right)) -> Int(boolToInt left * boolToInt right)
      // | (Bool(left), "/", Bool(right)) -> Int(boolToInt left / boolToInt right)
      // | (Bool(left), "^", Bool(right)) -> Int(pown (boolToInt left) (boolToInt right))
      // | (Bool(left), "<", Bool(right)) -> Bool(boolToInt left < boolToInt right)
      // | (Bool(left), "<=", Bool(right)) -> Bool(boolToInt left <= boolToInt right)
      // | (Bool(left), ">", Bool(right)) -> Bool(boolToInt left > boolToInt right)
      // | (Bool(left), ">=", Bool(right)) -> Bool(boolToInt left >= boolToInt right)
      // | (Bool(left), "=", Bool(right)) -> Bool(left = right)
      // | (Bool(left), "!=", Bool(right)) -> Bool(left <> right)
      //
      // Handling type mismatch:
      | (Int(_), operator, Str(_)) -> failwith ("can't apply " + (string operator) + " operator to strings and integers")
      | (Str(_), operator, Int(_)) -> failwith ("can't apply " + (string operator) + " operator to strings and integers")

      | _   -> failwith ("expecting an expression but found " + (string expression))

  //
  // (id, 0) :: memory
  //
  let private declaration (program, memory) =
    let declInstruction = List.head program

    //
    // Since the declaration is legal (created by our parser),
    // we know the 2nd part of declInstruction is
    // the identifer, e.g. ["$DECL"; "x"].  We need the "x".
    //
    let id = List.head (List.tail declInstruction)

    // Errors if id has been found in memory
    match findByID memory id with
    | Some(_) -> failwith ("Can't reassign an already declared variable: " + (string id))
    | None -> (program, (id, 0) :: memory)

  //
  // map memory (findID id) val
  // Update a cell in memory.
  //
  let private assignment (program, memory) =
    let assignInstruction = List.head program

    //
    // Since the declaration is legal (created by our parser),
    // we know the 2nd part of assignInstruction is
    // the identifer, e.g. ["$ASSIGN"; "x"; ....].  We need it.
    // We also know the rest of assignInstruction is an expression
    // That we can evaluate
    //
    let id = List.head (List.tail assignInstruction)
    let value = List.tail (List.tail assignInstruction)
              |> evalExpr memory

    // Errors on assignment to undefined
    // and invalid assignment of mismatched types
    match value with
    | Int(concreteValue) ->
      reassign memory id concreteValue
      |> expect ("Can't assign to an undefined variable: " + (string id) + " the value: " + (string value))
      |> update program
    | Str(concreteValue) -> failwith ("can't assign" + (string concreteValue) + " to int")

  //
  // $INPUT>
  //
  let private input (program, memory) =
    let inputInstruction = List.head program

    //
    // Since the declaration is legal (created by our parser),
    // we know the 2nd part of inputInstruction is
    // the identifer, e.g. ["$INPUT"; "x"].  We need "x".
    //
    let id = List.head (List.tail inputInstruction)

    //
    // Check value's exsitence before asking for input
    findByID memory id
    |> expect ("Can't input into an undefined variable: " + (string id))
    |> ignore

    printf "$INPUT> "

    // ReadLine then update memory
    System.Console.ReadLine ()
    |> int
    |> reassign memory id
    |> Option.get
    |> update program

  //
  // $OUTPUT>
  //
  let private output (program, memory) =
    let outputInstruction = List.head program

    //
    // Since the declaration is legal (created by our parser),
    // we know the rest of outputInstruction is an expression
    // That we can evaluate
    //

    let value = List.tail outputInstruction
              |> evalExpr memory

    // printf "$OUTPUT> "

    match value with
    | Str(concreteValue) -> printf "%s" concreteValue
    | Int(concreteValue) -> printf "%d" concreteValue

    update program memory

  //
  // Nothingness.
  //
  let private emptystmt (program, memory) =
    (program, memory)

  //
  // execute:
  // Keep executing instruction until the instruction list (program)
  // is empty
  //
  let rec execute (program, memory) =
    if List.isEmpty program then
      memory
    else
      // After every instruction we call smartDrop to clean up
      (program, memory)
      |> instruction
      |> (fun (newProgram, newMemory) -> ((smartDrop newProgram), newMemory))
      |> execute

  //
  // instruction:
  // Call function passed on the type of instruction at head
  // Of instruction list
  //
  and private instruction (program, memory) =
    let instructionType = (List.head (List.head program))
    //
    match instructionType with
    | "$DECL"    -> declaration (program, memory)
    | "$ASSIGN"  -> assignment (program, memory)
    | "$INPUT"   -> input (program, memory)
    | "$OUTPUT"  -> output (program, memory)
    | "$IF"      -> ifstmt (program, memory)
    | "$FOR"     -> forstmt (program, memory)
    | "$EMPTY"   -> emptystmt (program, memory)
    | _ -> failwith ("expecting instruction, but found " + (string instructionType))


  //
  // if else if else
  //
  and private ifstmt (program, memory) =
    let ifInstruction = List.head program

    //
    // Since the declaration is legal (created by our parser),
    // we know the rest of ifInstructions is an expression
    // That we can evaluate for "truthiness"
    //

    let condition = List.tail ifInstruction
                  |> evalExpr memory
                  |> booleanifyExpr

    // If the condition evaluates to true
    // We know that the next instruction(s) are the ones
    // that we will need to execute
    // But if it evaluates to false we might need to jump
    // several instructions (several fors or ifs) before we find
    // the "else" branch
    let branchToExecute =
      if condition then
        List.tail program
      else
        smartDrop (List.tail program)

    instruction (branchToExecute, memory)
    |> ignoreProgram program

  //
  // innerFor:
  // This function calls itself recursively as long as for's condition
  // Remains "truthy"
  //
  and private innerFor (program, memory) =
    // Evaluate for's condition
    let condition = List.head program
                  |> evalExpr memory
                  |> booleanifyExpr

    // Figure out whether to break out of for or not yet
    if condition then
      // If not, execute body instructions then "update" statment then call
      // innerFor again with updated memory (while ignoring changes to program)
      let forBody = List.tail (List.tail program)
      let updateStmt = List.tail program

      instruction (forBody, memory)
      |> (fun (_, newMemory) -> instruction (updateStmt, newMemory))
      |> ignoreProgram program
      |> innerFor
    else
      (program, memory)

  //
  // forstmt:
  //
  and private forstmt (program, memory) =

    //
    // Since the declaration is legal (created by our parser),
    // we know the first value after [$FOR] is the init statment
    // That we can execute
    //
    // Dismiss ["$FOR"] statment
    let initStmt = List.tail program

    // Execute init statment then run innerFor with rest of instructions
    // Dismiss changes to program as we will let smartDrop handle them
    instruction (initStmt, memory)
    |> ignoreProgram (List.tail initStmt)
    |> innerFor
    |> ignoreProgram program


  //
  // interpret program
  //
  // Given a list of simple ASM instructions, interprets
  // each instruction in order to execute the entire
  // program.  The function returns the state of memory
  // when execution has completed, which is a list of
  // tuples of the form ("ID", value); all values are
  // integers.  Example: [("x", 100); ("y", -1)].
  //
  let interpret program =
    try
      execute (program, [])
    with
      | ex -> printfn "semantic error: %s" ex.Message
              []
