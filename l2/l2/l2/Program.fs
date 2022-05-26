// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open FsAlg.Generic

let LU (A : Matrix<float>) =
    let rec step currentA m_list iter =
        match iter with
        | _ when iter = (Matrix.rows A) -> (currentA, m_list)
        | _ ->
          let row = currentA |> Matrix.col iter |> Matrix.toVector
          let data =  row |> Vector.split [iter + 1; (Vector.length row - iter - 1)] 
                          |> Seq.toList
                          |> List.rev 
                          |> List.head 
                          |> Vector.toSeq 
                          |> Seq.toList
          let a = currentA.[iter, iter]
          let mu_list = List.map (fun x ->  - x / a) data
          printf "\nMu: "
          List.iter (fun x -> printf "%f " x) data
          printf "\n"
          let M' = Matrix.init (Matrix.rows A) (Matrix.rows A) (fun i j -> if i = j then 1.0 else 0.0)
          Matrix.replacei (fun i j x -> if j = iter && i > j then mu_list.[i - j - 1] else x ) M'
          printf "\nM': %s\n" (M'.ToMathematicaString())
          let new_A = M' * currentA
          printf "\nNEW: %s\n" (new_A.ToMathematicaString())
          step new_A (M'::m_list) (iter + 1)
    step A [] 0


[<EntryPoint>]
let main argv =
    let A = matrix [[1.; 2.; 4.]; [3.; 4.; 1.]; [3.; 2.; 2.]]
    let _, m = LU A
    let E = Matrix.init 3 3 (fun i j -> if i = j then 1.0 else 0.0)
    let res = m |> List.rev |> List.map Matrix.inverse |> List.fold (fun acc x -> acc * x) E 
    printfn "\nRes: %s\n" (res.ToMathematicaString()) 
    0 // return an integer exit code