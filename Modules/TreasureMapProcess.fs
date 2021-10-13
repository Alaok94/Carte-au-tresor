
namespace Modules
open System
open TreasureMapModel
module TreasureMapProcess =

    // calcul les différents mouvements des aventuriers
    let processMovment (map : Map) =

      let (mapLong, mapLarg) = map.Size

      let processForward adventurer =
        let posX = adventurer.X
        let posY = adventurer.Y
        let newPosX =
          match adventurer.Orientation with
            |East -> posX + 1
            |West -> posX - 1
            |_    -> posX
        let newPosY =
          match adventurer.Orientation with
            |North -> posY - 1
            |South -> posY + 1
            |_     -> posY
        
        if ((newPosX < mapLong) && (newPosY < mapLarg) && (newPosX >= 0) && (newPosY >= 0)) then
          match map.Content.[newPosY, newPosX] with
            |Mountain -> adventurer 
            |_        -> {adventurer with  X = newPosX;  Y = newPosY} 
        else
          {adventurer with  X = newPosX;  Y = newPosY}

      let processTurnRight adventurer =
        match adventurer.Orientation with
          |North -> {adventurer with Orientation = East}
          |South -> {adventurer with Orientation = West}
          |East  -> {adventurer with Orientation = South}
          |West  -> {adventurer with Orientation = North}

      let processTurnLeft adventurer =


        match adventurer.Orientation with
          |North -> {adventurer with Orientation = West}
          |South -> {adventurer with Orientation = East}
          |East  -> {adventurer with Orientation = North}
          |West  -> {adventurer with Orientation = South}

      let newAdventurers = map.Adventurers |> List.map (fun e -> match e.Movments with
                                                                  |Forward :: tail   -> {e with Movments = tail} |> processForward
                                                                  |TurnRight :: tail -> {e with Movments = tail} |> processTurnRight
                                                                  |TurnLeft :: tail  -> {e with Movments = tail} |> processTurnLeft
                                                                  |[]                -> e
                                                        )

      {map with Adventurers = newAdventurers}

    //collecte les trésors pour les aventuriers
    let processTreasures map =
      let (mapLong, mapLarg) = map.Size
      let processTreasure adventurer =
        if ((adventurer.X < mapLong) && (adventurer.Y < mapLarg) && (adventurer.X >= 0 ) && (adventurer.Y >= 0)) then

          match map.Content.[adventurer.Y, adventurer.X] with
            |Treasure 1 -> map.Content.[adventurer.Y, adventurer.X] <- Empty; {adventurer with Treasures = adventurer.Treasures + 1}
            |Treasure n -> map.Content.[adventurer.Y, adventurer.X] <- Treasure (n - 1); {adventurer with Treasures = adventurer.Treasures + 1}
            |_          -> adventurer
        else
          adventurer

      {map with Adventurers = map.Adventurers |> List.map processTreasure}
    

    //résoud les conflits de position entre les aventuriers
    let rec resolveConflicts mapBefore mapAfter =
      let groupedAdventurers = mapAfter.Adventurers |> List.groupBy (fun e -> (e.X,e.Y))

      let resolvePositionConflict ((x,y),(adventurers : Adventurer list)) =
        
        if (adventurers.Length > 1) then

          let premiumAdventurer :: restAdventurers =  adventurers |> List.sortBy (fun e -> e.Priority) 
          let beforePosition adventurer =
             let adventurerBefore = mapBefore.Adventurers |> List.find (fun e -> e.Priority = adventurer.Priority) 
             (adventurerBefore.X ,adventurerBefore.Y)
          let (newRestAdventurers,posResolved) = restAdventurers |> List.mapFold (fun b e -> let (x0,y0) = e |> beforePosition
                                                                                             ({e with X=x0;Y=y0}, (x0,y0) <> (x,y) && b)
                                                                                  ) true
          if posResolved then
            premiumAdventurer::newRestAdventurers
         
          else

            let newpremiumAdventurer =
              let (x0,y0) = premiumAdventurer |> beforePosition;
              {premiumAdventurer with X=x0;Y=y0}
            newpremiumAdventurer :: newRestAdventurers
        else
          adventurers
      
      if (groupedAdventurers |> List.exists (fun (_,n) -> n.Length > 1)) then
        let newAdventurers = groupedAdventurers |> List.map resolvePositionConflict |> List.reduce (@) 
        {mapAfter with Adventurers = newAdventurers} |> resolveConflicts mapBefore
      else
        mapAfter 
      
    //résoud les comportements des aventuriers
    let rec processAdventurers (map : Map) =
      let movmentsLeft = map.Adventurers |> List.exists (fun e -> e.Movments <> [])
      if movmentsLeft then
        let currentAdventurers = map.Adventurers

        map |> processMovment |> resolveConflicts map |> processTreasures |>  logmap "" |> processAdventurers

      else

        map

    //traite le fichier carte, écrit le fichier résultat et retourne la Map
    let processTreasureMap (filePath : string) =

        let writeFileMapPath = 
           let fileName = filePath.[filePath.LastIndexOf("/") + 1 ..filePath.LastIndexOf(".") - 1] + "_Result.txt"
           filePath.[..filePath.LastIndexOf("/")] + fileName
        filePath |> parseFileMap |>  logmap ""  |> processAdventurers |> normalize |> writeFileMap writeFileMapPath