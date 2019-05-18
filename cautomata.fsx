#r @"..\FSharpx.Collections.dll"
// open System.Runtime.Remoting.Metadata.W3cXsd2001
open System
open System.Drawing
open System.IO
open FSharpx.Collections

type System.Boolean with
    member this.ToBinary() = 
        if (this) then '1'
        else '0'

type U<'x> = 
    | U of (LazyList<'x> * 'x * LazyList<'x>)

type Motion = 
    | Left
    | Right

let shift' (dir : Motion) (U(a, b, c)) = 
    match dir with
    | Left -> 
        let (l, taill) = LazyList.uncons a
        U(taill, l, LazyList.cons b c)
    | Right -> 
        let (r, tailr) = LazyList.uncons c
        U(LazyList.cons b a, r, tailr)

let rec mapU (f : 'a -> 'b) (U(a, b, c)) = 
    U(LazyList.map f a, f b, LazyList.map f c)

let rec iterateU f value = 
    LazyList.consDelayed value (fun () -> iterateU f (f value))

let cojoin a = 
    U
        (LazyList.tail(iterateU (shift' Left) a), a, 
         LazyList.tail(iterateU (shift' Right) a))
let (=>>) f x = mapU f (cojoin x)
let wolframUncode ruleNumber (pattern : byte) = 
    (ruleNumber >>> (int32 pattern)) &&& 0x00000001 = 1

let ruleU ruleNumber = function 
    | (U(left, focus, right) : U<bool>) -> 
        Convert.ToByte([|LazyList.head left;
                         focus;
                         LazyList.head right|]
                       |> Array.map(fun b -> b.ToBinary())
                       |> String, 2)
        |> wolframUncode ruleNumber

let shift i u = 
    ((iterateU (if i < 0 then (shift' Left)
                else (shift' Right)) u)
     |> LazyList.skip(abs i))
    |> LazyList.uncons
    |> fst

let toList i j u = 
    let half(U(_, b, c)) = LazyList.cons b c
    LazyList.take (j - i) (half(shift i u))

let runAutomaton width height wolframNumber () = 
    let u = U(LazyList.repeat false, true, LazyList.repeat false)
    let rule = ruleU wolframNumber
    (iterateU ((=>>) rule) u)
    |> LazyList.map(toList (-width / 2) (width / 2))
    |> LazyList.take height

let drawSquare xScale yScale () = 
    let bitmap = new Bitmap((xScale + 1) * 18, (yScale + 1) * 18)
    
    let mesh = 
        seq {
            for x in [0..15] do
                for y in [0..15] do
                    yield (x * xScale, y * yScale)
        }
    mesh 
    |> Seq.iteri
           (fun rule (ex, why) -> 
           runAutomaton xScale yScale rule () 
           |> Seq.iteri
                  (fun y v -> 
                  v 
                  |> Seq.iteri
                         (fun x w -> 
                         match w with
                         | false -> 
                             bitmap.SetPixel(ex + x, why + y, Color.WhiteSmoke)
                         | true -> bitmap.SetPixel(ex + x, why + y, Color.Black))))
    bitmap

(drawSquare 135 150 ())
    .Save(Path.Combine
              (__SOURCE_DIRECTORY__, sprintf "cellular-automaton-%s.png" "all"))
