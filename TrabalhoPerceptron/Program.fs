// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.

open FSharp.Charting
open System.Windows.Forms
open MathNet.Numerics.LinearAlgebra

let treinamento = [([2.0; 2.0], 1.0) ; ([4.0; 4.0], 0.0)]

let w0 = vector[-0.5441; 0.5562; -0.4074]

let xn x = vector (-1.0 :: x)

let linear w x = 
    w .* x |>
    Vector.sum

let degrau u = 
    if u >= 0.0 then 1.0 else 0.0

let ativacao w x = 
    linear w x  |> 
    degrau

let erro w x y = y - ativacao w x

let proximo (w: Vector<_>) (x: Vector<_>) e =
    w + 0.1 * e * x


let rec atualizaPesos' tr w e =
    match tr with
    | [] -> (w, e)
    | par :: tail ->
        match par with
        | (x, y) -> 
            let x' = xn x
            let e' = erro w x' y
            let w' = proximo w x' e'
            
            atualizaPesos' tail w' (if e = 0.0 then e' else e)

let rec atualizaPesos tr w =
    let e = match List.head tr with (x, y) -> erro w (xn x) y
    let (w', e') = atualizaPesos' tr w e
    if e' = 0.0 then w' else atualizaPesos tr w'

let displayFn w x1 =
    List.ofSeq w |> 
    function | w0 :: w1 :: w2 :: _ -> -x1 * (w1 / w2) + (w0 / w2)

let arrayTuple x =
    match x with
    | x1 :: x2 :: tail -> (x1, x2)
    | [] -> (0.0, 0.0)
    | _ -> (0.0, 0.0)

let point (x, y: float) = Chart.Point([arrayTuple x])

[<EntryPoint>]
let main argv = 
    //let w = atualizaPesos treinamento w0
    //treinamento |>
    //List.iter (fun (x, y) -> printfn "%A" (ativacao w (xn x)))
    //List.map
    let w = atualizaPesos treinamento w0
    let point = 
        treinamento |>
        List.map (fun (x, y) -> arrayTuple x ) |>
        Chart.Point
    
    let line = 
        [1.0 .. 10.0] |>
        List.map (fun x0 -> (x0, displayFn w x0)) |>
        Chart.Line
        
        

    let form = new Form()

    let r = Chart.Combine([point ; line]).ShowChart()

    Application.Run(form)
    //Chart.Point
    0 // retornar um código de saída inteiro
