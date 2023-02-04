module Hlp23Tick3

// important file containing all the Draw block (Symbol, BusWire and Sheet) types
// types are arranged in 3 submodules for each of Symbol, BusWire and Sheet
open DrawModelType

// open the submodules for these types which are needed
// so they do not need to be referred to using module name `BusWireT.Wire`
open DrawModelType.SymbolT
open DrawModelType.BusWireT

// open modules possiblly needed for drawSymbol
open Fable.React
open Fable.React.Props
open Elmish

// the standard Issie types for components, connections, etc
open CommonTypes

// some helpers to draw lines, text etc
open DrawHelpers

// the file containing symbol subfunctions etc
open Symbol

/// submodule for constant definitions used in this module
module Constants =
    let xxx = 111 // sample constant definition (with bad name) delete and replace
                  // your constants. Delete this comment as well!

/// Record containing BusWire helper functions that might be needed by updateWireHook
/// functions are fed in at the updatewireHook function call in BusWireUpdate.
/// This is needed because HLPTick3 is earlier in the F# compile order than Buswire so
/// the functions cannot be called directly.
/// Add functions as needed.
/// NB these helpers are not needed to do Tick3
type Tick3BusWireHelpers = {
    AutoRoute: BusWireT.Model -> Wire -> Wire
    ReverseWire: Wire -> Wire
    MoveSegment: Model -> Segment -> float -> Wire
    }

type rectangle = House | Window | Door
let toString (x:int,y:int) = 
    string (x,y)
/// Return Some reactElement list to replace drawSymbol by your own code
/// Choose which symbols this function controls by
/// returning None for the default Issie drawSymbol in SymbolView.
/// Drawhelpers contains some helpers you can use to draw lines, circles, etc.
/// drawsymbol contains lots of example code showing how they can be used.
/// The returned value is a list of SVG objects (reactElement list) that will be
/// displayed on screen.
//  for Tick 3 see the Tick 3 Powerpoint for what you need to do.
//  the house picture, and its dependence on the two parameters, will be assessed via interview.
let makeRectangle windowH windowV=
    let floorNum = [1..(int windowV)]
    let windowNum = [1..windowH]
    let gridH = 20 //horizontal distance between windows
    let gridV = 20//vertical distance between windows
    let windowLength = 40
    let windowHeight = 60
    let firstPos = [0,0; 0,windowHeight; windowLength,windowHeight; windowLength,0]
    let getNewCoord (input:(int * int) list) _ = 
        List.map (fun (x,y) -> (x + windowLength + gridH, y)) input
    let getPositions =
        let getAllCoordinates = 
            [1..windowH]
            |> List.scan getNewCoord firstPos
        let convertToString input=
            input
            |> List.map toString
            |> List.reduce (fun x y -> x + " " + y)
        List.map (convertToString) getAllCoordinates
    printfn $"CONSTANT: all positions={getPositions}"

    
    let getPositions' floorNum=
        let n =
            0 + (floorNum-1)*(windowHeight + gridV)
        let firstPos = [0,n; 0,n+windowHeight; windowLength,n+windowHeight; windowLength,n]
        let getAllCoordinates = 
            [1..(windowH-1)]//because firstposition is being returned
            |> List.scan getNewCoord firstPos
        let convertToString input=
            input
            |> List.map toString
            |> List.reduce (fun x y -> x + " " + y)
        printfn $"CONSTANT: all={List.map (convertToString) getAllCoordinates}"
        List.map (convertToString) getAllCoordinates
    

    let getAllWindows = List.collect getPositions' [1..(int windowV)]
    printfn $"CONSTANT: all windows hi={getAllWindows}"
    let buildingComponent (component':rectangle)= {
        Stroke = "black"
        StrokeWidth = match component' with 
                        | House -> "4px"
                        | _ -> "2px"
        FillOpacity = 1.0
        Fill = "None"
    }
    let makeWindowRow n =
       makePolygon(getAllWindows.[n-1])(buildingComponent Window)
    let buffer = gridH
    let houseDim=
        List.map toString [-buffer,-buffer; (windowLength + gridH)*windowH, -buffer; (windowLength + gridH)*windowH,(windowHeight + gridV)*(1 + int windowV); -buffer, (windowHeight + gridV)*(1 + int windowV) ]
        |> List.reduce (fun x y -> x + " " + y)
    
    let numWindows = (windowH*(int windowV))
    let makeDoor = 
        let position =
            let houseLength = (windowLength + gridH)*windowH - buffer;
            List.map toString [(houseLength/2)-(windowLength/4), (windowHeight + gridV)*(1 + int windowV); (houseLength/2)-(windowLength/4), (windowHeight + gridV)*(1 + int windowV)-windowHeight; (houseLength/2)+(windowLength/4), (windowHeight + gridV)*(1 + int windowV)-windowHeight; (houseLength/2)+(windowLength/4), (windowHeight + gridV)*(1 + int windowV)]
            |> List.reduce (fun x y -> x + " " + y)
        makePolygon(position)(buildingComponent Door)
    List.map (makeWindowRow) [1..numWindows] @ [makePolygon(houseDim)(buildingComponent House)] @ [makeDoor]

let drawSymbolHook 
        (symbol:Symbol) 
        (theme:ThemeType) 
        : ReactElement list option =
    // replace the code below by your own code
    match symbol.Component.Type with
    | Constant1 (width,constValue, _) ->
        Some (makeRectangle width constValue)//printfn $"CONSTANT: width={width} ConstVale={constValue}"
    | _ -> None//printfn "Symbol Hook"
    //None

/// Return Some newWire to replace updateWire by your own code defined here.
/// Choose which wires you control by returning None to use the
/// default updateWire function defined in BusWireUpdate.
/// The wire shape and position can be changed by changing wire.Segments and wire.StartPos.
/// See updateWire for the default autoroute wire update function.
/// The return value must be a (possibly modified) copy of wire.

// For tick 3 modify the updated wires (in some cases) somehow. 
// e.g. if they have 3 visual segments and have a standard (you decide what) orientation change where the middle
// segment is on screen so it is 1/3 of the way between the two components instead of 1/2.
// do something more creative or useful if you like.
// This part of Tick will pass if you can demo one wire changing as you move a symbol in some way different from
// Issie: the change need not work on all quadrants (where it is not implemented the wire should default to
// Issie standard.
let updateWireHook 
        (model: BusWireT.Model) 
        (wire: Wire) 
        (tick3Helpers: Tick3BusWireHelpers)
        : Wire option =
    let segmentInfo =
        wire.Segments
        |> List.map (fun (seg:Segment) -> seg.Length,seg.Mode)
    printfn "%s" $"Wire: Initial Orientation={wire.InitialOrientation}\nSegments={segmentInfo}"
    None

//---------------------------------------------------------------------//
//-------included here because it will be used in project work---------//
//---------------------------------------------------------------------//

/// This function is called at the end of a symbol (or multi-symbol) move
/// when the mouse goes up.
/// at this time it would make sense to try for a better autoroute of
/// all the moved wires e.g. avoiding eachother, avoiding other wires,
/// etc, etc.
///
/// wireIds is the list of wire ids that have one end connected to a
/// moved symbol.
/// Any required change in wire positions or shapes should be returned by 
/// changing the values of busWireModel.Wires which
/// is a Map<ConnectionId , Wire> and contains all wires
/// keyed by their wire Id (type ConnectionId)
/// No change required for Tick 3 
let smartAutoRouteWires
        (wireIds: ConnectionId list) 
        (tick3Helpers: Tick3BusWireHelpers)
        (model: SheetT.Model) 
        : SheetT.Model =
    let busWireModel = model.Wire // contained as field of Sheet model
    let symbolModel = model.Wire.Symbol // contained as field of BusWire Model
    let wires = busWireModel.Wires // all wire info
    // NB to return updated wires here you would need nested record update
    // {model with Wire = {model.Wire with Wires = wires'}}
    // Better syntax to do that can be found using optics lenses
    // see DrawModelT for already defined lenses and Issie wiki 
    // for how they work
    model // no smart autoroute for now, so return model with no chnage

//---------------------------------------------------------------------//
//------------------- Snap Functionality-------------------------------//
//---------------------------------------------------------------------//

(*

 Needed for one part of project work (not for Tick 3):
    Sheet.getNewSegmentSnapInfo
    Sheet.getNewSymbolSnapInfo

 These functions can be changed to alter which things symbols or segments snap to:
 They are called at the start of a segment or symbol drag operation.

 If you want to change these inside a drag operation - you may need to alter other code.
 The snap code is all in one place and well-structured - it should be easy to change.

 *)
