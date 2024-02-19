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
let writeSymbolPosition (pos:XYPos) (sym:Symbol) (sheet : SheetT.Model)=
    ()

//B5R

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

