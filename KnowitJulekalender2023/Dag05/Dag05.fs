open Utilities.Any
open Utilities

let primes = MathHelpers.generatePrimeArray 100_000_001

let isSumDividePrime number = 
    let sum = MathHelpers.digitSum number 0
    match number % sum with
    | 0 -> primes[number / sum]
    | _ -> false

seq{1 .. 100_000_000}
|> Seq.map isSumDividePrime
|> Seq.filter (fun p -> p)
|> Seq.length
|> print
