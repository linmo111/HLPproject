module SheetBeautifyHelpers

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team



open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open Optics
open Optic
open Symbol
open Helpers
open BusWireRoute
open BlockHelpers
module constants=
    ()

//get a custom input symbol and returns the height and width of the symbol in a xypos format
//B1R
let readCustomSymbolSize (sym:Symbol)=
    // let symPos = get posOfSym_ sym
    let comp = sym.Component 
    let compBox = {X=comp.W; Y=comp.H}
    compBox
//returns a symbol with dimension of the input
//B1W
let writeCustomSymbolSize (HW:XYPos) (sym: Symbol)=
    let W,H= HW.X,HW.Y
    let newSym={sym with Component= {sym.Component with H=H; W=W}}
    newSym

let RWCustomSymbolSize=Lens.create (readCustomSymbolSize) (writeCustomSymbolSize)

//B4R
let readMux2ReverseState (sym:Symbol)=
    sym.ReversedInputPorts
//B4W
let writeMux2ReverseState(state:option<bool>)(sym:Symbol)=
    {sym with ReversedInputPorts = state}

let RWMux2ReverseState= Lens.create (readMux2ReverseState) (writeMux2ReverseState)

//B2W
let writeSymbolPosition (pos:XYPos) (symId:ComponentId) (sheet : SheetT.Model)=
    let boundingBoxes= sheet.BoundingBoxes
    let symBoundingBox=boundingBoxes[symId]
    let newsymBox={symBoundingBox with TopLeft =pos-{X=symBoundingBox.W/2.; Y=symBoundingBox.H/2.} }
    let newSymBoundingBoxes= Map.change symId (fun stringsOpt -> Some newsymBox) boundingBoxes
    {sheet with BoundingBoxes=newSymBoundingBoxes}

//B3R

let readPortOrientation (sym:Symbol) (edge:Edge)=
    let portmap=sym.PortMaps
    let order= portmap.Order

    match Map.tryFind edge order with
    | Some keys -> keys
    | None -> []
    
// B3W
let WritePortOrientation (newOrder:list<string>)  (edge:Edge) (sym:Symbol)=
    let portmap=sym.PortMaps
    let order= portmap.Order
    let newOrderMap=Map.change edge (fun stringsOpt -> Some newOrder) order
    let newportmap= {portmap with Order = newOrderMap}
    {sym with PortMaps = portmap}


//B5R
let readPortPos(sym:Symbol) (port : Port)=
    getPortPos sym port

//B6R
let readSymbolBoundingBox (sym:Symbol) =
    sym.SymbolBoundingBox

//B7R
let readSymbolRotationState (sym:Symbol) =
    sym.STransform.Rotation
//B7W
let writeSymbolRotationState (rotation:Rotation) (sym:Symbol) =
    {sym with STransform= { sym.STransform with Rotation=rotation}}
let RWSymbolRotationState=Lens.create  (readSymbolRotationState) (writeSymbolRotationState)

//B8R 
let readSymbolFlipState(sym:Symbol) =
    sym.STransform.Flipped
//B8W
let writeSymbolFlipState (flipped:bool) (sym:Symbol) =
    {sym with STransform= { sym.STransform with Flipped=flipped}}
let RWSymbolFlipState= Lens.create  (readSymbolFlipState) (writeSymbolFlipState)

//T1R

let ReadSymbolIntersectsSymbolPairs  (sheet: SheetT.Model) =
            
            let boxes =
                mapValues sheet.BoundingBoxes
                |> Array.toList
                |> List.mapi (fun n box -> n,box)
            List.allPairs boxes boxes 
            |> List.fold (fun num ((n1,box1),(n2,box2))  -> if ((n1 <> n2) && BlockHelpers.overlap2DBox box1 box2) then num+1 else num) 0

        

//T2R
// let visibleSegments (wId: ConnectionId) (model: SheetT.Model): XYPos list =

//     let wire = model.Wire.Wires[wId] // get wire from model

//     /// helper to match even and off integers in patterns (active pattern)
//     let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

//     /// Convert seg into its XY Vector (from start to end of segment).
//     /// index must be the index of seg in its containing wire.
//     let getSegmentVector (index:int) (seg: BusWireT.Segment) =
//         // The implicit horizontal or vertical direction  of a segment is determined by 
//         // its index in the list of wire segments and the wire initial direction
//         match index, wire.InitialOrientation with
//         | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> {X=0.; Y=seg.Length}
//         | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> {X=seg.Length; Y=0.}

//     /// Return a list of segment vectors with 3 vectors coalesced into one visible equivalent
//     /// if this is possible, otherwise return segVecs unchanged.
//     /// Index must be in range 1..segVecs
//     let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int)  =
//         if segVecs[index] =~ XYPos.zero
//         then
//             segVecs[0..index-2] @
//             [segVecs[index-1] + segVecs[index+1]] @
//             segVecs[index+2..segVecs.Length - 1]
//         else
//             segVecs

//     wire.Segments
//     |> List.mapi getSegmentVector
//     |> (fun segVecs ->
//             (segVecs,[1..segVecs.Length-2])
//             ||> List.fold tryCoalesceAboutIndex)
// let failOnWireIntersectsSymbol (sample: int) (sheet: SheetT.Model) =
//             let wireModel = sheet.Wire
//             wireModel.Wires
//             |> Map.exists (fun _ wire -> BusWireRoute.findWireSymbolIntersections wireModel wire <> [])
//             |> (function | true -> Some $"Wire intersects a symbol outline in Sample {sample}"
//                          | false -> None)
let ReadSegmentIntersectSymbolNum ( sheet: SheetT.Model)=
    let wires= sheet.Wire.Wires
    // let keysList= wires |> Map.toList  |> List.map fst
    let wiresList= wires |> Map.toList  |> List.map snd
    let absSegments=List.collect id (List.map (fun wire ->getNonZeroAbsSegments wire) wiresList )
    let boundingBoxes= sheet.BoundingBoxes |> Map.toList  |> List.map snd

    List.allPairs absSegments boundingBoxes
    |> List.map (fun  (segment, boundingbox)-> segmentIntersectsBoundingBox boundingbox segment.Start segment.End)
    |> List.fold (fun num (intersectRes:option<float>) ->
        match intersectRes with
        | Some _ -> num+1
        | None->num) 0
    // List.map ( fun key-> visibleSegments key sheet) keysList##

//T3R

let readSegmentRightIntersectionPair(sheet: SheetT.Model)=
    let wires= sheet.Wire.Wires
    // let keysList= wires |> Map.toList  |> List.map fst
    let wiresList= wires |> Map.toList  |> List.map snd
    let absSegments=List.collect id (List.map (fun wire ->getNonZeroAbsSegments wire) wiresList )
    //the above line is inefficient because the segments in same wire will not cross
    //should not collapse the list asegment list, and use that to pair, then collapse later
    let half num=num/2
    let isZero (num:float)=
        match num with
        | 0.0 -> true
        |_ -> false
    let checkSegmentRightAngleIntersect (segmentA:ASegment) (segmentB:ASegment)=
        let segmentAVector:XYPos= {X=(segmentA.Start.X - segmentA.End.X); Y=(segmentA.Start.Y - segmentA.End.Y)}
        let segmentBVector:XYPos= {X=(segmentB.Start.X - segmentB.End.X); Y=(segmentB.Start.Y - segmentB.End.Y)}
        (segmentA <> segmentB) && (overlap2D (segmentA.Start,segmentA.End)(segmentB.Start,segmentB.End)) && (isZero (dotProduct segmentAVector segmentBVector))
        // they intersect right angle if dot product is 0, and they overlap. 
    List.allPairs absSegments absSegments
    |> List.map (fun (segmentA, segmentB)-> checkSegmentRightAngleIntersect segmentA segmentB)
    |> List.fold ( fun num bool -> 
    match bool with
    |true -> num+1
    |_ -> num ) 0
    |> half // checked twice because same list pair with themselves
    

//T5R
let ReadVisibleRightAngleNum (sheet:SheetT.Model)=
    let wires= sheet.Wire.Wires
    // let keysList= wires |> Map.toList  |> List.map fst
    let wiresList= wires |> Map.toList  |> List.map snd
    let absSegments= (List.map (fun wire ->getNonZeroAbsSegments wire) wiresList )
    absSegments|>
    List.fold( fun num (AsegList:List<ASegment>) -> num+ List.length AsegList-1) 0