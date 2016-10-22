module Validator

open ResultDemo

/// Type for validators from input types i to output types o, with possible failures
type Validator<'i,'o> = 'i -> Result<'o>

// Type synonym 
type OptionMap = Map<string,string>

// Sample map of user input strings
let cmdLineOptions: OptionMap =
    Map.ofList [
        ("stringOpt","good string");
        ("intOpt", "3");
        ("boolOpt", "true");
    ]

// Type for the validated input record
type ValidatedInput = {
    stringOpt: string;
    intOpt: int;
    boolOpt: bool;
}

/// Smart constructor for validated inputs. Works over pure values.
let validatedInput: string -> int -> bool -> ValidatedInput =
    fun stringOpt intOpt boolOpt -> { stringOpt = stringOpt; intOpt = intOpt; boolOpt = boolOpt; }

// Validate the stringOpt option
let stringOptValidator: Validator<OptionMap,string> =
    fun strMap ->
        let failureMsg = "Couldn't validate string option"
        match (Map.tryFind "stringOpt" cmdLineOptions) with
        | Some v -> if v = "good string" then success v else failure failureMsg
        | _      -> failure failureMsg

// resultDef "" (stringOptValidator cmdLineOptions)

// Validate the intOpt option
let intOptValidator: Validator<OptionMap,int> =
    fun strMap ->
        let failureMsg = "Couldn't validate int option"
        // Catch any exception from attempt to parse as int
        try
            match (Map.tryFind "intOpt" strMap) with
            | Some intString -> success (System.Int32.Parse(intString))
            | _ -> failure failureMsg
        with | e -> failure failureMsg

// resultDef 0 (intOptValidator cmdLineOptions)
// messages (intOptValidator cmdLineOptions)

// Validate the boolOpt option
let boolOptValidator: Validator<OptionMap,bool> =
    fun strMap ->
        let failureMsg = "Couldn't validate bool option"
        match (Map.tryFind "boolOpt" strMap) with
        | Some boolStr -> match boolStr with
                          | "true" -> success true
                          | "false" -> success false
                          | _ -> failure failureMsg
        | _ -> failure failureMsg

/// Map over output of any Validator
let mapValidator: ('a -> 'b) -> Validator<'i,'a> -> Validator<'i,'b> =
    fun f va -> fun i -> mapResult f (va i)

/// Infix notation for mapping over Validators
let (<!>) = mapValidator

/// Lift any pure function over Validators
let apValidator: Validator<'i,'a -> 'b> -> Validator<'i,'a> -> Validator<'i,'b> =
    fun vab va ->
        fun i -> ResultDemo.apResult (vab i) (va i)  // Result<'a -> 'b> -> Result<'a> -> Result<'b>

/// Infix notation for lifting pure functions over Validators
let (<*>) = apValidator

/// Lift our pure input validator to account for possible failures
let cmdLineOptionsValidator: Validator<OptionMap,ValidatedInput> =
    validatedInput <!> stringOptValidator <*> intOptValidator <*> boolOptValidator

// Example usage
let validatedCmdLineOptions: Result<ValidatedInput> = cmdLineOptionsValidator cmdLineOptions
