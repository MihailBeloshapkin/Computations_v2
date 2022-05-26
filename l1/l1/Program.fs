open System
open FsAlg.Generic

let norm (A : Matrix<float>) =
    let rows = A |> Matrix.toRows |> Seq.map Matrix.toVector
    let getRowSum : Vector<float> -> float = Vector.fold (fun acc x -> acc + Math.Abs(x)) 0.
    let sumList = rows |> Seq.map getRowSum
    Seq.max sumList

let spectral A =
    let Ainv = Matrix.inverse A
    (norm A) * (norm Ainv)

let processMatrix2 A =
    let getRowSum = Vector.fold (fun acc x -> acc + x * x) 0.
    let rows = A |> Matrix.toRows |> Seq.map Matrix.toVector
    let numerator = Seq.fold (fun acc x -> acc * Math.Sqrt(getRowSum x)) 1. rows
    numerator / (Matrix.det A)

let angle A =
    let C : Matrix<float> = Matrix.inverse A
    let aRows = A |> Matrix.toRows |> Seq.map Matrix.toVector |> Seq.toList
    let cCols = C |> Matrix.toCols |> Seq.map Matrix.toVector |> Seq.toList
    (aRows, cCols) ||> List.zip |> List.map (fun (f, s) -> (Vector.length f) * (Vector.length s)) |> List.max


let m1 = matrix [[1.; 2.]; [3.; 4.]]
printfn "result: %f" (processMatrix2 m1)
