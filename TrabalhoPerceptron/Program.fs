// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.

open FSharp.Data
open FSharp.Charting
open System.Windows.Forms
open MathNet.Numerics
open MathNet.Numerics.Random
open MathNet.Numerics.Distributions
open MathNet.Numerics.LinearAlgebra

type Realizacao = { Acuracia:float; Confusao: Matrix<float>; W: Vector<float>; Dados: seq<float list * float> }
type RealizacaoIris2a2 = { Realizacao: Realizacao; Par: int * int }

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

let vetorPesos tr =
    let rec atualizaPesos tr w =
        let rec atualizaPesos1 tr w e =
            match tr with
            | [] -> (w, e)
            | par :: tail ->
                match par with
                | (x, y) -> 
                    let xv = xn x
                    let e1 = erro w xv y
                    let w1 = proximo w xv e1
                    atualizaPesos1 tail w1 (if e <> 0.0 then e else e1)

        let e = match List.head tr with (x, y) -> erro w (xn x) y
        let (w', e') = atualizaPesos1 tr w e
        if e' = 0.0 then w' else atualizaPesos (tr.SelectPermutation() |> List.ofSeq) w'
    let tamanhoVetor =
        tr |> 
        List.head |> 
        fun (x, y) -> x |> List.length |> (+) 1

    let w0 = Random.doubles tamanhoVetor |> vector

    atualizaPesos tr w0

let realizacao dados =
    let confusao = matrix([[0.0; 0.0]; [0.0; 0.0]])

    let dadosList = dados |> List.ofSeq
    let treinamento = 
        let n = dadosList |> List.length |> float |> (*) 0.8 |> int
        dadosList |> List.take n

    let teste = dadosList |> List.except treinamento

    let w = vetorPesos treinamento

    teste |>
        Seq.iter (fun (x, y) -> 
            let a = int (ativacao (xn x) w)
            confusao.[a, int y] <- confusao.[a, int y] + 1.0)
        
    { Acuracia = confusao.Diagonal().Sum() / float (teste |> Seq.length) ; Confusao = confusao; Dados = dadosList; W = w }

let algoritmoIris =
    let db = CsvFile.Load("iris.data").Cache()
    let classes = dict["Iris-setosa", 0; "Iris-versicolor", 1; "Iris-virginica", 1]
    
    let parse s = s |> System.Double.Parse
    let mapRow (row: CsvRow) = (row.Columns |> Array.take 4 |> Array.map parse |> List.ofArray, float classes.[row.["class"]])
    
    let dados = db.Rows |> Seq.map mapRow
    let realizacoes =
        [1..20] |>
        Seq.map (fun _ -> realizacao (dados.SelectPermutation()))
    
    let maior = 
        realizacoes |>
        Seq.maxBy (fun r -> r.Acuracia)

    maior

let algoritmoIris2a2 =
    let db = CsvFile.Load("iris.data").Cache()
    let classes = dict["Iris-setosa", 0; "Iris-versicolor", 1; "Iris-virginica", 1]

    let pares =
        seq {
        for x1 in 0..2 do
            for x2 in x1+1..3 do
                yield (x1, x2)
        }

    let mapRow x1 x2 (row: CsvRow) = ([row.[int x1].AsFloat(); row.[int x2].AsFloat()], float classes.[row.["class"]])
    let mapPar (x1, x2) = 
        db.Rows |>
        Seq.map (mapRow x1 x2)
    
    let fazRealizacoes (x1, x2) =
        [1..20] |>
        Seq.map (fun _ -> realizacao ((mapPar (x1, x2)).SelectPermutation())) |>
        Seq.map (fun r -> { Realizacao = r; Par = (x1, x2) })

    let realizacoes = 
        pares |>
        Seq.collect fazRealizacoes
    
    
    let maior = 
        realizacoes |>
            Seq.maxBy (fun r -> r.Realizacao.Acuracia)

    maior

let algoritmoCustom =
    let xa = Array.zeroCreate 50
    let xb = Array.zeroCreate 50
    let xc = Array.zeroCreate 50
    let xd = Array.zeroCreate 50
    
    let ya = Array.zeroCreate 50
    let yb = Array.zeroCreate 50
    let yc = Array.zeroCreate 50
    let yd = Array.zeroCreate 50

    //Distribuição normal (ou gaussiana)
    Normal.Samples(xa, 2.0, 0.2)
    Normal.Samples(xb, 2.0, 0.2)
    Normal.Samples(xc, 6.0, 0.2)
    Normal.Samples(xd, 4.5, 0.2)

    Normal.Samples(ya, 3.5, 0.2)
    Normal.Samples(yb, 0.5, 0.2)
    Normal.Samples(yc, 0.8, 0.2)
    Normal.Samples(yd, 3.5, 0.2)

    let x = Array.append xa xb
    let y = Array.append ya yb
    
    let classe0 =
        let x = Array.append xa xb |> Array.append xc
        let y = Array.append ya yb |> Array.append yc
        [0 .. x.Length-1] |>
        List.map (fun n -> ([x.[n]; y.[n]], 0.0))

    let classe1 =
        [0..49] |>
        List.map (fun n -> ([xd.[n]; yd.[n]], 1.0))
    
    let classes = classe0 @ classe1

    let realizacoes =
        [1..20] |>
        Seq.map (fun _ -> realizacao (classes.SelectPermutation()))

    let maior = 
        realizacoes |>
            Seq.maxBy (fun r -> r.Acuracia)

    maior
    
///Retorna x2 para dado x1
let funcaoX2 w x1 =
    List.ofSeq w |> 
    function 
        | w0 :: w1 :: w2 :: _ -> -x1 * (w1 / w2) + (w0 / w2)
        | _ -> 0.0

///Converte os 2 primeiros elementos da lista 'x' em uma tupla
let tupla x =
    match x with
    | x1 :: x2 :: tail -> (x1, x2)
    | _ -> (0.0, 0.0)

//Exibe 'realizacao' em um gráfico.
let exibe realizacao =
    let point0 = 
        realizacao.Dados |>
        Seq.filter (fun (x, y) -> y = 0.0) |>
        List.ofSeq |>
        List.map (fun (x, y) -> tupla x ) |>
        function 
        | x -> Chart.Point(data = x, Color = System.Drawing.Color.Red)

    let point1 = 
        realizacao.Dados |>
        Seq.filter (fun (x, y) -> y = 1.0) |>
        List.ofSeq |>
        List.map (fun (x, y) -> tupla x ) |>
        function 
        | x -> Chart.Point(data = x, Color = System.Drawing.Color.Blue)

    let line = 
        [0.0 .. 10.0] |>
        List.map (fun x0 -> (x0, funcaoX2 realizacao.W x0)) |>
        Chart.Line

    Chart.Combine([point0; point1 ; line]).ShowChart() |> ignore
    realizacao

[<EntryPoint>]
let main argv = 
    let form = new Form()

    algoritmoIris |> printfn "\n\nIris:\n%A"
    algoritmoIris2a2 |> printfn "\n\nIris2a2\n%A"

    algoritmoIris2a2.Realizacao |> exibe |> ignore
    algoritmoCustom |> exibe |> printfn "\n\nCustom\n%A"
    Application.Run(form)

    0 // retornar um código de saída inteiro
