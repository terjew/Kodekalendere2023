﻿namespace Utilities

module MathHelpers = 
    let generatePrimeArray limit =
        let primeArray = Array.create limit true
        let rec setArray l h s x =
            if l <= h then
                primeArray.[l] <- x
                setArray (l + s) h s x
        primeArray.[0] <- false; primeArray.[1] <- false
        for i = 0 to primeArray.Length - 1 do
            if primeArray.[i]
            then setArray (i + i) (primeArray.Length - 1) i false
        primeArray

    let rec digitSum number carry =
        let digit = number % 10
        match number - digit with 
        | 0 -> carry + digit
        | _ -> digitSum (number / 10) (carry + digit)

    let solveQuadraticEquation (a,b,c) = 
        let q = System.Math.Sqrt(b*b - 4.0*a*c)
        ((-b + q)/(2.0*a), (-b - q)/(2.0*a))