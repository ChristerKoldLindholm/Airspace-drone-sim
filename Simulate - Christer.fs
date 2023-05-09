
module DroneSim 

/// <summary> Calculates the Euclidian distance between two drone positions. </summary>
/// <param name="(x1, y1)"> A drone's coordinate position. </param>
/// <returns> The Euclidian distance as a float. </returns>
let EuclidDist (x1, y1) (x2, y2) = 
    System.Math.Sqrt(
        (x2 - x1 |> float) ** 2 
        + (y2 - y1 |> float) ** 2
        ) 

type drone (position: (int * int), speed: int, destination: (int * int), name: string) =
    
    // Constructors 
    let startPos = position 
    let dest = destination
    let mutable currentPos = position
    let mutable direction = 0.0 
    let mutable speedStopper = speed 

    let mutable collided = false 

    // Members 
    member this.getPosition = currentPos 
    member this.speedStop () = speedStopper <- 0  
    member this.getStartPos = position 
    member this.getDestination = destination 

    member this.deadDrone = collided 
    member this.killDrone () = collided <- true  

    // Methods 

    /// <summary> Changes a drone's direction by calculating a direction vector 
    /// based on it's current position, then multiplies the resulting x and y 
    /// coordinates with its speed and mutates its current position. </summary>
    /// <returns> An updated current position for a drone. </returns>    
    member this.fly () = 
        direction <- System.Math.Atan2 (
            snd dest - snd currentPos|> float, fst dest - fst currentPos|> float
            )
        let x = float(speed) * System.Math.Cos direction |> int
        let y = float(speed) * System.Math.Sin direction |> int
        currentPos <- (fst currentPos + x, snd currentPos + y)
         
    /// <summary> Determines two Euclidian distances: between a drone's
    /// starting point and destination and between it's current position
    /// and destination. </summary> 
    /// <returns> Returns true if the drone has traveled further than
    /// it's destination, then mutates it's position to it's destination. </returns>    
    member this.atDestination =
        let midwayDistance = EuclidDist startPos currentPos
        printfn "Midway distance = %A" midwayDistance

        let completeDistance = EuclidDist startPos dest  
        printfn "Complete distance = %A" completeDistance 

        if midwayDistance >= completeDistance then 
            currentPos <- (fst dest, snd dest)
            true 
        else false
  
    override this.ToString() = 
        name  

type airspace () =
    let mutable drones = []
    let mutable list = []

    /// <summary> Calculates the distance between two drones. </summary> 
    /// <returns> Returns their distance. </returns> 
    member this.DroneDist (droneA: drone) (droneB: drone) = 
        let dist = EuclidDist droneA.getPosition droneB.getPosition  
        // printfn "Drone distance test = %A" dist 
        dist 
        
    member this.getDrones() = drones
    member this.add (drone: drone) = drones <- drone :: drones 

    override this.ToString () = drones.ToString()

    /// <summary> Calls fly() on every drone in an airspace. Mutates their speed to 0
    /// if they have traveled further than their destination. </summary> 
    /// <returns> A list of drones in an airspace with their positions
    /// and speeds updated. </returns> 
    member this.flyDrones () = 
        drones |> List.iter (fun (drone: drone) -> drone.fly())

        for l in 0..(drones.Length - 1) do 
            let drone = drones.[l]

            if (EuclidDist drone.getStartPos drone.getDestination) 
            >= (EuclidDist drone.getStartPos drone.getPosition) then 
                drone.speedStop ()

    /// <summary> Loops through the list of drones and compares every
    /// drone to every other drone. If the distance between a pair of drones
    /// is less than 5, they are put into a list of collided drones
    /// and filtered out of the active drone list. </summary> 
    /// <returns> A list of drones without the collided drones. </returns> 
    member this.collide () =
        let mutable collisionList = []

        for i in 0..(drones.Length - 1) do 
            for j in 0..(drones.Length - 1) do 
                if i = j then ()
                else 
                let droneA = drones.[i]
                let droneB = drones.[j]
                if (this.DroneDist droneA droneB) < 5.0 then 
                    collisionList <- (droneA, droneB) :: collisionList 
                    droneA.killDrone ()
                    droneB.killDrone ()
        drones <- drones |> List.filter (fun drone -> not drone.deadDrone)
        collisionList 

        
        