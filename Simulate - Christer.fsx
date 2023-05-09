#load "Simulate - Christer.fs"
open DroneSim 

// Define some test drones 
let droneA = drone ((8, 4), 3, (15, 10), "drone1")
let droneB = drone ((2, 1), 2, (12, 12), "drone2")
// let drone3 = drone ((15, 5), 4, (10, 20), "drone3")

/// <summary> Returns true or false based on the result of a given test. </summary>
/// <param name="str"> Text describing a test. </param>
/// <param name="a"> The first value a to compare with b. </param>
/// <param name="b"> The second value b to compare with a. </param>
/// <param name="f"> A function to compare a and b. </param>
/// <returns> A boolean conclusion. </returns>
type Assert =
    static member test (str: string) (a: 'a) (b: 'b) (f: ('a -> 'b -> bool)) : unit = 
        let result = f a b 
        if result = true then printfn "Test passed: %s" str
        else printfn "Test failed: %s" str 

printfn "\n TEST SECTION: \n"

Assert.test ("droneA.atDestination ()?") droneA.atDestination (false) (=)   

let airspaceTest = airspace()
printfn "Distance between droneA and droneB = %A" (EuclidDist (1,1) (2,1))
airspaceTest.add droneA
airspaceTest.add droneB 
Assert.test ("DroneDist droneA droneB < 5.0 ?") (airspaceTest.DroneDist droneA droneB) (5.0) (>) 

printfn "Test airspace before fly.Drones () = %A" (droneA.getPosition)
airspaceTest.flyDrones ()
printfn "Test airspace after fly.Drones () = %A" (droneA.getPosition)

Assert.test ("flyDrones () ?") droneA.getPosition (10,5) (=) 

Assert.test ("collide () ?") (airspaceTest.collide()) [] (=)

// Testing 10 seconds of 10 drones flying 

// Defines a list of 10 drones, adds them to the airspace1 object, then 
// calls flydrones() and collide() 10 times to simulate 10 seconds of flight.
let tenSecondSimulation = 
    let tenDrones = [drone((1,1), 1, (5,5), "Drone1"); drone((2,5), 2, (10,10), "Drone2"); 
        drone((10,5), 2, (15,15), "Drone3"); drone((15,20), 4, (15,1), "Drone4");
        drone((100,55), 3, (50,100), "Drone5"); drone((25,35), 2, (20,10), "Drone6");
        drone((50,5), 5, (7,12), "Drone7"); drone((70,70), 2, (75,75), "FlyAloneDrone8");
        drone((45,5), 7, (40,1), "Drone9"); drone((15,15), 2, (8,25), "Drone10")]
    
    printfn "\n SIMULATION SECTION: \n"

    let airspace1 = airspace()
    let mutable counter = 0 

    for drone in tenDrones do 
        airspace1.add drone 
    
    for second in 1..10 do
        counter <- counter + 1 
        printfn "%A second(s) in flight" counter 
        printfn "All airborn drones = %A" (airspace1.getDrones())
        printfn "Drones which collided = %A \n" (airspace1.collide())
        airspace1.flyDrones ()