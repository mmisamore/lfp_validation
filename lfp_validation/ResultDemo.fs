module ResultDemo

/// Type for values with failure
type Result<'a> =
    private | Success of 'a
            | Failure of string list

/// Smart constructor for successful Results
let success: 'a -> Result<'a> = Success

/// Smart constructor for single failures
let failure: string -> Result<'a> = fun s -> Failure [s]

/// Smart constructor for multiple failures
let failures: string list -> Result<'a> = Failure

/// Handle pattern matches for Results via success and failure continuations
let either: ('a -> 'b) -> (string list -> 'b) -> Result<'a> -> 'b =
    fun successHandler failureHandler ra ->
        match ra with
        | Success a -> successHandler a
        | Failure s -> failureHandler s

/// Get the failures, if any, for a Result
let messages: Result<'a> -> string list =
    fun ra -> either (fun _ -> []) id ra

/// Map over any Result type
let mapResult: ('a -> 'b) -> Result<'a> -> Result<'b> =
    fun f -> either (f >> success) failures

/// Infix notation for mapping over Results
let (<!>) = mapResult

/// Lift any pure function over Result types
let apResult: Result<'a -> 'b> -> Result<'a> -> Result<'b> =
    fun rf ra -> match (rf,ra) with
                 | (Success f, Success a) -> Success (f a)
                 | (Failure s, Success _) -> Failure s
                 | (Success _, Failure s) -> Failure s
                 | (Failure s1, Failure s2) -> Failure (List.append s1 s2)

/// Infix notation for lifting pure functions over Result types
let (<*>) = apResult

/// Return a successful Result or a default value in case of failure
let resultDef: 'a -> Result<'a> -> 'a =
    fun def ra -> either id (fun _ -> def) ra

// Example pure function
let add: int -> int -> int =
    fun x y -> x + y

// Lifted example function with possible failures
let addResults: Result<int> -> Result<int> -> Result<int> =
    fun rx ry -> add <!> rx <*> ry

// messages (addResults (failure "boo") (success 1))
// messages (addResults (success 2) (failure "ghosts!"))
// messages (addResults (failure "boo!") (failure "ghosts!"))
// resultDef 0 (addResults (success 1) (success 2))
