open Utilities.Any
open Utilities

let sw = System.Diagnostics.Stopwatch.StartNew()

let primes = MathHelpers.generatePrimeArray 10_000_000

let isSumDividePrime number = 
    let sum = MathHelpers.digitSum number 0
    match number % sum with
    | 0 -> primes[number / sum]
    | _ -> false

seq{1 .. 100_000_000 - 1}
|> Seq.map isSumDividePrime
|> Seq.filter (fun p -> p)
|> Seq.length
|> print

sw.Elapsed |> print