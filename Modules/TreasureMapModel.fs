
namespace Modules
open System

module TreasureMapModel =
    type CardinalPoints =
     |North
     |East
     |West
     |South
    type Movment =
     |Forward
     |TurnRight
     |TurnLeft
     //X -> Ouest vers Est
     //Y -> Sud vers Nord
     //Priority -> 0 est plus prioritaire que 1
    type Adventurer = {Name : string; X : int; Y : int; Orientation : CardinalPoints;Movments : Movment list; Treasures : int; Priority : int}
    type Case =
      |Mountain
      |Treasure of int
      |Empty

    type Map = {Size : int*int; Content : Case[,]; Adventurers : Adventurer list}

    //normalise les map   
    let normalize map =
      {map with Adventurers = map.Adventurers |> List.sortBy (fun e -> e.Priority)}
    
    //fonctions de log
    let logt a =
        printfn "%A" a
        a
    let logm msg a =
        printfn "%s %A" msg a
        a
    let logmap msg  map =
        printfn ""
        let (long,larg) = map.Size
        let tmpContent = map.Content |> Array2D.mapi (fun i j e->let lineBreak = if (j = (long - 1)) then "\n" else ""
                                                                 match e with
                                                                   |Treasure n -> " T(" + n.ToString() + ") " + lineBreak
                                                                   |Mountain   -> "  M   " + lineBreak
                                                                   |Empty      -> "  .   " + lineBreak
                                                     )
        map.Adventurers |> List.iteri (fun i e ->  if (e.Y < larg && e.X < long && e.Y >= 0 && e.X >= 0) then tmpContent.[e.Y,e.X] <- "  " + e.Name.[0..1].ToString() + "  " )


        tmpContent |> Array2D.iter (fun e -> printf "%s" e)
        printfn ""
        map


    //initialise la map à partir des lignes lues du fichier en entrée
    let initMap (lines :array<array<string>>) =
          let mapInfo =  lines |> Seq.find (fun e -> e.[0] = "C")
          let long = mapInfo.[1] |> int
          let larg = mapInfo.[2] |> int
          let emptyContent =   Array2D.init larg long (fun _ _ -> Case.Empty)
          let myMap = {Size = (long,larg); Content = emptyContent; Adventurers = List.Empty }

          let addAdventurer (line : array<string>) map =
              let orientation =
                 match line.[4] with
                    |"N" -> CardinalPoints.North
                    |"S" -> CardinalPoints.South
                    |"E" -> CardinalPoints.East
                    |"W" -> CardinalPoints.West
                    |_ -> raise  (System.ArgumentException("Orientation de l'aventurier" + line.[1] + " incorrect " ))

              let movments =  Array.foldBack (fun e s ->      match e with
                                                                        |'A' -> Forward :: s
                                                                        |'G' -> TurnLeft :: s
                                                                        |'D' -> TurnRight :: s
                                                                        |_ -> raise (System.ArgumentException("Orientation de l'aventurier" + line.[1] + " incorrect " ))
                                              )
                                              (line.[5].ToCharArray()) []
              let posX = line.[2] |> int

              let posY = (line.[3] |> int)
              let priority = map.Adventurers.Length
              let newAdventurer = {Name = line.[1]; X = posX; Y = posY ; Orientation = orientation; Treasures =0; Movments = movments; Priority = priority}
              {map with Adventurers = newAdventurer :: map.Adventurers }

          let addMountain (line : array<string>) map =
               let x = line.[1] |> int
               let y = line.[2] |> int
               map.Content.[y,x ] <- Case.Mountain
               map

          let addTreasure (line : array<string>) map =
               let x = line.[1] |> int |> logt
               let y = line.[2] |> int |> logt
               line |> logt |> ignore
               map.Content.[y,x ] <- line.[3] |> int |> Case.Treasure
               map

          let addElementToMap (line :array<string>) map =
                match line.[0] with
                  |"A" -> map |> addAdventurer line
                  |"M" -> map |> addMountain line
                  |"T" -> map |> addTreasure line
                  |"C" -> map
                  |_ -> raise (FormatException("Ligne incorrect dans le fichier en entrée"))//gestion erreur

          lines |> Array.fold (fun s e -> addElementToMap e s ) myMap

    
    //initialise la map à partir du fichier en entrée
    let parseFileMap (filePath : string) =
        let lines = seq {
                           use sr = new System.IO.StreamReader(filePath)
                           while not sr.EndOfStream do
                           yield sr.ReadLine()
                        } |> Seq.toArray
        printfn "%A" lines

        let filteredLines = lines |> Array.filter (fun e -> not (e.StartsWith('#')))
                                  |> Array.map (fun e -> (e |> String.filter (fun c -> c <> ' ')).Split('-') )


        filteredLines |> initMap
       
    //écrit le fichier résultat à partir de la map en entrée.
    let writeFileMap (filePath : string) map =
         let sw = new IO.StreamWriter(filePath)
         let (lon,larg) = map.Size
         sw.Write("C" + " - " + lon.ToString() + " - " + larg.ToString() + "\n")
         let writeContent i j case =
           match case with
             |Mountain -> sw.Write("M"+ " - " + j.ToString() + " - " + i.ToString() + "\n")
             |Treasure n ->sw.Write("T"+ " - " + j.ToString() + " - " + i.ToString() + " - " + n.ToString() + "\n")
             |_ -> 0 |> ignore
         map.Content |> Array2D.iteri writeContent
         let writeAdventurers adventurer =
            let orientation =
               match adventurer.Orientation with
                 |North -> "N"
                 |South -> "S"
                 |East  -> "E"
                 |West  -> "W"
            sw.Write("A"+ " - " + adventurer.X.ToString() + " - " + adventurer.Y.ToString()+ " - " + orientation + " - " + adventurer.Treasures.ToString() + "\n")

         map.Adventurers |> List.iter writeAdventurers
         sw.Close();
         map
