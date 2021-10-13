module tests

open Modules.TreasureMapModel
open Modules.TreasureMapProcess
open NUnit.Framework


let compareFiles (fp1 : string) (fp2: string) =

  let lines1 = seq {
                     use sr = new System.IO.StreamReader(fp1)
                     while not sr.EndOfStream do
                     yield sr.ReadLine()
                   } |> Seq.toArray |> Array.filter (fun e -> not (e.StartsWith('#') || e = "" ))|> Array.map (fun e -> (e |> String.filter (fun c -> c <> ' ' )).Split('-') ) |> Array.sort
  let lines2 = seq {
                     use sr = new System.IO.StreamReader(fp2)
                     while not sr.EndOfStream do
                     yield sr.ReadLine()
                   } |> Seq.toArray |> Array.filter (fun e -> not (e.StartsWith('#') || e = ""))|> Array.map (fun e -> (e |> String.filter (fun c -> c <> ' ' && c <> '\n')).Split('-') ) |> Array.sort
  printfn "%A" lines1
  printfn "%A" lines2
  lines1 = lines2
let filespath = "C:/Users/guill/Desktop/projet/fsharp/Carte au tresor/tests/Ressources"
[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``Modelisation de la carte au tresor`` () =
 
    let expected =  { Size = (3, 4);
                       Content = [|[|Empty; Empty; Empty|];
                                  [|Empty; Mountain; Empty|];
                                  [|Empty; Empty; Mountain|];
                                  [|Treasure 2; Treasure 1; Empty|]|] |> array2D;
                       Adventurers =
                                    [{ Name = "Jones"
                                       X = 1
                                       Y = 2
                                       Orientation = South
                                       Movments = [Forward;TurnLeft; TurnLeft; Forward; Forward]
                                       Treasures = 0
                                       Priority = 1 };
                                     { Name = "Indiana"
                                       X = 1
                                       Y = 0
                                       Orientation = South
                                       Movments = [Forward; TurnRight; Forward; TurnLeft; TurnLeft; Forward]
                                       Treasures = 0
                                       Priority = 0 }] }
    let actual = filespath + "/Test1.txt" |> parseFileMap 
    Assert.AreEqual(expected,actual)
[<Test>]
let ``Ecriture de la carte au tresor`` () =
 
    let expected =  { Size = (3, 4);
                       Content = [|[|Empty; Empty; Empty|];
                                  [|Empty; Mountain; Empty|];
                                  [|Empty; Empty; Mountain|];
                                  [|Treasure 2; Treasure 1; Empty|]|] |> array2D;
                       Adventurers =
                                    [{ Name = "Jones"
                                       X = 1
                                       Y = 2
                                       Orientation = South
                                       Movments = [Forward;TurnLeft; TurnLeft; Forward; Forward]
                                       Treasures = 0
                                       Priority = 1 };
                                     { Name = "Indiana"
                                       X = 1
                                       Y = 0
                                       Orientation = South
                                       Movments = [Forward; TurnRight; Forward; TurnLeft; TurnLeft; Forward]
                                       Treasures = 0
                                       Priority = 0 }] }
    let fp1 = filespath + "/TestWrite_Actual.txt"
    let fp2 = filespath + "/TestWrite_Expected.txt"
    expected  |> writeFileMap fp1 |> ignore
    Assert.IsTrue(compareFiles fp1 fp2)   

[<Test>]
let ``1 mouvement sans obstacle `` () =
 
    let startingState =  { Size = (3, 4);
                       Content = [|[|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|]|] |> array2D;
                       Adventurers =
                                    [{ Name = "Jones"
                                       X = 0
                                       Y = 0
                                       Orientation = East
                                       Movments = [Forward; TurnRight; Forward; TurnLeft; Forward; TurnRight;Forward; Forward]
                                       Treasures = 0
                                       Priority = 1 };
                                     { Name = "Indiana"
                                       X = 0
                                       Y = 3
                                       Orientation = North
                                       Movments = [TurnLeft; Forward; Forward;]
                                       Treasures = 0
                                       Priority = 0 };] }
    let expected =  { Size = (3, 4);
                       Content = [|[|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|]|] |> array2D;
                       Adventurers =
                                    [{ Name = "Jones"
                                       X = 1
                                       Y = 0
                                       Orientation = East
                                       Movments = [TurnRight; Forward; TurnLeft; Forward; TurnRight;Forward; Forward]
                                       Treasures = 0
                                       Priority = 1 };
                                     { Name = "Indiana"
                                       X = 0
                                       Y = 3
                                       Orientation = West
                                       Movments = [ Forward; Forward;]
                                       Treasures = 0
                                       Priority = 0 };] }                                       
    let actual = startingState |> processMovment
    Assert.AreEqual(expected,actual)

[<Test>]
let ``1 mouvement avec obstacle Montagne `` () =
 
    let startingState =  { Size = (3, 4);
                       Content = [|[|Empty; Mountain; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|]|] |> array2D;
                       Adventurers =
                                    [{ Name = "Jones"
                                       X = 0
                                       Y = 0
                                       Orientation = East
                                       Movments = [Forward; TurnRight; Forward; TurnLeft; Forward; TurnRight;Forward; Forward]
                                       Treasures = 0
                                       Priority = 1 };
                                     { Name = "Indiana"
                                       X = 0
                                       Y = 3
                                       Orientation = North
                                       Movments = [TurnLeft; Forward; Forward;]
                                       Treasures = 0
                                       Priority = 0 };] }
    let expected =  { Size = (3, 4);
                       Content = [|[|Empty; Mountain; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|]|] |> array2D;
                       Adventurers =
                                    [{ Name = "Jones"
                                       X = 0
                                       Y = 0
                                       Orientation = East
                                       Movments = [TurnRight; Forward; TurnLeft; Forward; TurnRight;Forward; Forward]
                                       Treasures = 0
                                       Priority = 1 };
                                     { Name = "Indiana"
                                       X = 0
                                       Y = 3
                                       Orientation = West
                                       Movments = [ Forward; Forward;]
                                       Treasures = 0
                                       Priority = 0 };] }                                       
    let actual = startingState |> processMovment
    Assert.AreEqual(expected,actual)

[<Test>]
let ``1 mouvement avec obstacle Aventurier`` () =
 
    let startingState =  { Size = (3, 4);
                       Content = [|[|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|]|] |> array2D;
                       Adventurers =
                                    [{ Name = "Jones"
                                       X = 0
                                       Y = 0
                                       Orientation = East
                                       Movments = [Forward; TurnRight; Forward; TurnLeft; Forward; TurnRight;Forward; Forward]
                                       Treasures = 0
                                       Priority = 1 };
                                     { Name = "Indiana"
                                       X = 1
                                       Y = 0
                                       Orientation = North
                                       Movments = [TurnLeft; Forward; Forward;]
                                       Treasures = 0
                                       Priority = 0 };] }
    let expected =  { Size = (3, 4);
                       Content = [|[|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|]|] |> array2D;
                       Adventurers =
                                    [{ Name = "Jones"
                                       X = 0
                                       Y = 0
                                       Orientation = East
                                       Movments = [TurnRight; Forward; TurnLeft; Forward; TurnRight;Forward; Forward]
                                       Treasures = 0
                                       Priority = 1 };
                                     { Name = "Indiana"
                                       X = 1
                                       Y = 0
                                       Orientation = West
                                       Movments = [ Forward; Forward;]
                                       Treasures = 0
                                       Priority = 0 };] } |> normalize                                  
    let actual = startingState |> processMovment |> resolveConflicts startingState |> normalize
    Assert.AreEqual(expected,actual)

[<Test>]
let ``Resoudre un conflit simple sur 1 mouvement`` () =
 
    let startingState =  { Size = (3, 4);
                       Content = [|[|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|]|] |> array2D;
                       Adventurers =
                                    [{ Name = "Jones"
                                       X = 0
                                       Y = 0
                                       Orientation = East
                                       Movments = [Forward; TurnRight; Forward; TurnLeft; Forward; TurnRight;Forward; Forward]
                                       Treasures = 0
                                       Priority = 1 };
                                     { Name = "Indiana"
                                       X = 1
                                       Y = 1
                                       Orientation = North
                                       Movments = [Forward; Forward; Forward;]
                                       Treasures = 0
                                       Priority = 0 };] }
    let expected =  { Size = (3, 4);
                       Content = [|[|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|]|] |> array2D;
                       Adventurers =
                                    [{ Name = "Jones"
                                       X = 0
                                       Y = 0
                                       Orientation = East
                                       Movments = [TurnRight; Forward; TurnLeft; Forward; TurnRight;Forward; Forward]
                                       Treasures = 0
                                       Priority = 1 };
                                     { Name = "Indiana"
                                       X = 1
                                       Y = 0
                                       Orientation = North
                                       Movments = [ Forward; Forward;]
                                       Treasures = 0
                                       Priority = 0 };] } |> normalize                                      
    let actual = startingState |> processMovment |> resolveConflicts startingState |> normalize
    Assert.AreEqual(expected,actual)

[<Test>]
let ``Resoudre un conflit complexe sur 1 mouvement`` () =
 
    let startingState =  { Size = (3, 4);
                       Content = [|[|Empty; Empty; Mountain|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|]|] |> array2D;
                       Adventurers =
                                    [{ Name = "Jones"
                                       X = 2
                                       Y = 1
                                       Orientation = North
                                       Movments = [Forward;]
                                       Treasures = 0
                                       Priority = 3 };
                                     { Name = "Indiana"
                                       X = 2
                                       Y = 2
                                       Orientation = North
                                       Movments = [Forward; Forward; Forward;]
                                       Treasures = 0
                                       Priority = 2 };
                                     { Name = "Bob"
                                       X = 1
                                       Y = 2
                                       Orientation = East
                                       Movments = [Forward; TurnRight; Forward; TurnLeft; Forward; TurnRight;Forward; Forward]
                                       Treasures = 0
                                       Priority = 1 };
                                     { Name = "Morane"
                                       X = 1
                                       Y = 3
                                       Orientation = North
                                       Movments = [Forward; Forward; Forward;]
                                       Treasures = 0
                                       Priority = 0 };] }
    let expected =   { Size = (3, 4);
                       Content = [|[|Empty; Empty; Mountain|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|]|] |> array2D;
                       Adventurers =
                                    [{ Name = "Jones"
                                       X = 2
                                       Y = 1
                                       Orientation = North
                                       Movments = []
                                       Treasures = 0
                                       Priority = 3 };
                                     { Name = "Indiana"
                                       X = 2
                                       Y = 2
                                       Orientation = North
                                       Movments = [Forward; Forward;]
                                       Treasures = 0
                                       Priority = 2 };
                                     { Name = "Bob"
                                       X = 1
                                       Y = 2
                                       Orientation = East
                                       Movments = [TurnRight; Forward; TurnLeft; Forward; TurnRight;Forward; Forward]
                                       Treasures = 0
                                       Priority = 1 };
                                     { Name = "Morane"
                                       X = 1
                                       Y = 3
                                       Orientation = North
                                       Movments = [Forward; Forward;]
                                       Treasures = 0
                                       Priority = 0 };] } |> normalize                                      
    let actual = startingState |> processMovment |> resolveConflicts startingState |> normalize
    Assert.AreEqual(expected,actual)

[<Test>]
let ``Collecte de tresor sur 1 mouvement`` () =
 
    let startingState =  { Size = (3, 4);
                       Content = [|[|Empty; Treasure 5; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Treasure 1; Empty|];
                                  [|Empty; Empty; Empty|]|] |> array2D;
                       Adventurers =
                                    [{ Name = "Jones"
                                       X = 0
                                       Y = 0
                                       Orientation = East
                                       Movments = [Forward; TurnRight; Forward; TurnLeft; Forward; TurnRight;Forward; Forward]
                                       Treasures = 0
                                       Priority = 1 };
                                     { Name = "Indiana"
                                       X = 1
                                       Y = 1
                                       Orientation = South
                                       Movments = [Forward; Forward; Forward;]
                                       Treasures = 0
                                       Priority = 0 };] }
    let expected =  { Size = (3, 4);
                       Content = [|[|Empty; Treasure 4; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|];
                                  [|Empty; Empty; Empty|]|] |> array2D;
                       Adventurers =
                                    [{ Name = "Jones"
                                       X = 1
                                       Y = 0
                                       Orientation = East
                                       Movments = [TurnRight; Forward; TurnLeft; Forward; TurnRight;Forward; Forward]
                                       Treasures = 1
                                       Priority = 1 };
                                     { Name = "Indiana"
                                       X = 1
                                       Y = 2
                                       Orientation = South
                                       Movments = [ Forward; Forward;]
                                       Treasures = 1
                                       Priority = 0 };] } |> normalize                                      
    let actual = startingState |> processMovment |> resolveConflicts startingState |> processTreasures |>  normalize
    Assert.AreEqual(expected,actual)


[<Test>]
let ``Test integration 1`` () =
 
    let expected =  { Size = (3, 4);
                       Content = [|[|Empty; Empty; Empty|];
                                  [|Empty; Mountain; Empty|];
                                  [|Empty; Empty; Mountain|];
                                  [|Treasure 2; Empty; Empty|]|] |> array2D;
                       Adventurers =
                                    [{ Name = "Jones"
                                       X = 1
                                       Y = 2
                                       Orientation = North
                                       Movments = []
                                       Treasures = 1
                                       Priority = 1 };
                                     { Name = "Indiana"
                                       X = 1
                                       Y = 0
                                       Orientation = East
                                       Movments = []
                                       Treasures = 0
                                       Priority = 0 }] } |> normalize
    let actual = filespath + "/Test1.txt" |> processTreasureMap |> normalize
    Assert.AreEqual(expected,actual)

[<Test>]
let ``Test integration 2`` () =
 
    let content = Array2D.init 25 20 (fun i j -> Empty)
    content.[11,10] <- Mountain
    content.[13,14] <- Mountain
    content.[0,1] <- Treasure 1
    content.[1,2] <- Treasure 1
    content.[19,16] <- Treasure 1
    content.[5,2] <- Treasure 2
    content.[24,1] <- Treasure 1        

    let expected =  { Size = (20, 25);
                       Content = content;
                       Adventurers =
                                    [{ Name = "Jones"
                                       X = 12
                                       Y = 11
                                       Orientation = East
                                       Movments = []
                                       Treasures = 2
                                       Priority = 1 };
                                     { Name = "Indiana"
                                       X = 3
                                       Y = 1
                                       Orientation = West
                                       Movments = []
                                       Treasures = 4
                                       Priority = 0 };
                                     { Name = "Bob"
                                       X = 11
                                       Y = 9
                                       Orientation = East
                                       Movments = []
                                       Treasures = 0
                                       Priority = 3 };
                                     { Name = "Morane"
                                       X = 8
                                       Y = 11
                                       Orientation = East
                                       Movments = []
                                       Treasures = 0
                                       Priority = 2 }] } |> normalize
    let actual = filespath + "/Test2.txt" |> processTreasureMap |> normalize
    printfn "actual \n %A " actual
    printfn "expected \n  %A " expected

    let fp1 = filespath + "/Test2_Result.txt"
    let fp2 = filespath + "/Test2_Actual.txt"
    Assert.IsTrue(compareFiles fp1 fp2)