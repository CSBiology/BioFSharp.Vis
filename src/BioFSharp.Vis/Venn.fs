namespace BioFSharp.Vis

// Set theory
module Venn = 

    open Plotly.NET
    open Plotly.NET.LayoutObjects

    // #########################################

    type VennSet<'l,'a when 'a : comparison> = {
        Label  : list<'l>
        Set    : Set<'a>
        }

    type GenericVenn<'l,'a when 'a : comparison> = Map<string,VennSet<'l,'a>>    

    
    /// Converts from label list to string id
    let labelToId (label:list<'l>) =
        label
        |> List.map (fun x -> x.ToString())
        |> List.toArray
        |> String.concat "&" 


    /// Generates a generic venn from a list of sets
    let ofSetList (labels:array<string>) (sets:array<Set<_>>) =    
        let union = Set.unionMany sets
    
        let toLabel (arr:bool[]) = 
            arr |> Seq.mapi (fun i b -> i,b) |> Seq.choose (fun (i,b) -> if b then Some(labels.[i]) else None) |> Seq.toList        
    
        union
        |> Seq.map (fun item ->  item,Array.init sets.Length (fun i -> sets.[i].Contains(item)))
        |> Seq.groupBy snd
        |> Seq.map (fun (k,v) -> let label =  toLabel k
                                 let value = (v |> Seq.map fst |> Set.ofSeq)
                                 (labelToId label),{Label = label; Set = value})
        |> Seq.append [("union",{Label = []; Set = union})]
        |> Map.ofSeq


    /// Converts a generic venn to the count venn
    let toVennCount (genericVenn:GenericVenn<_,_>) =   
        genericVenn
        |> Seq.map (fun (kv) -> kv.Key ,kv.Value.Set.Count)
        |> Map.ofSeq

    /// Converts a generic venn to the count venn
    let toVennCountLabelArr (genericVenn:GenericVenn<_,_>) =   
        genericVenn
        |> Seq.map (fun (kv) -> kv.Value.Label |> Array.ofList,kv.Value.Set.Count)
        |> Array.ofSeq
    
    let initCircleShape x0 y0 x1 y1 color =
        Shape.init(
            Opacity = 0.3,
            Xref = "x",
            Yref = "y",
            Fillcolor = color,
            X0 = x0,
            Y0 = y0,
            X1 = x1,
            Y1 = y1,
            ShapeType = StyleParam.ShapeType.Circle,
            Line = Line.init( Color = color)
        )
    
    type Chart with
    
        /// Creates a Venn diagram from given labels and sets. 
        /// Includes additional customization parameters: 
        /// - colors: Colors for the circle shapes 
        /// - textFont: Textfont used
        static member Venn 
            (
                (labels: array<string>),
                (sets: array<Set<_>>),
                (?colors: array<Color>),
                (?textFont: Font)
            ) =
            let textFont =
                textFont
                |> Option.defaultValue (
                    Font.init (
                        Family = StyleParam.FontFamily.Arial,
                        Size = 18.,
                        Color = Color.fromString "black"
                    )
                )
            let vennCount = 
                ofSetList labels sets
                |> toVennCountLabelArr
                |> Array.filter (fun (label,_) -> label |> Array.isEmpty |> not)
    
            let axis =
                LinearAxis.init(
                    ShowTickLabels = false,
                    ShowGrid = false,
                    ZeroLine = false
                )
    
            match sets.Length with
            | 2 ->
                let colors = colors |> Option.defaultValue ([|Color.Table.Office.blue; Color.Table.Office.red|])
                let shapes =
                    Array.zip
                        [|
                            {|X0 = 0.; Y0 = 0.; X1 = 2.; Y1 = 2.|}
                            {|X0 = 1.5; Y0 = 0.; X1 = 3.5; Y1 = 2.|}
                        |]
                        colors
                    |> Array.map (fun (position, color) ->
                        initCircleShape position.X0 position.Y0 position.X1 position.Y1 color
                    )
                let layout =
                    Layout.init(
                        Shapes = shapes,
                        Margin = 
                            Margin.init(
                                Left = 20,
                                Right = 20,
                                Bottom = 100
                            )
                    )
                    |> Layout.AddLinearAxis(StyleParam.SubPlotId.XAxis 1, axis)
                    |> Layout.AddLinearAxis(StyleParam.SubPlotId.YAxis 1, axis)
                let positionSet,textSet =
                    let vennSingleText =
                        vennCount
                        |> Array.filter (fun (label,_) -> label.Length = 1)
                        |> Array.map (fun (label,count) ->
                            sprintf "%s<br>%i" label.[0] count
                        )       
                    let intersectionText =
                        vennCount
                        |> Array.filter (fun (label,_) -> label.Length > 1)
                        |> Array.map (fun (label,count) ->
                            sprintf "%i" count
                        )
                            
                    [|1.,1.;2.5,1.;1.75,1.|],
                    Array.append vennSingleText intersectionText
                Chart.Scatter(
                    positionSet,
                    StyleParam.Mode.Text,
                    Labels = textSet,
                    TextFont = textFont
                )
                |> Chart.withLayout layout
            | 3 ->
                let colors = colors |> Option.defaultValue ([|Color.Table.Office.blue; Color.Table.Office.red; Color.Table.Office.green|])
                let shapes =
                    Array.zip
                        [|
                            {|X0 = 0.; Y0 = 0.; X1 = 2.; Y1 = 2.|}
                            {|X0 = 1.5; Y0 = 0.; X1 = 3.5; Y1 = 2.|}
                            {|X0 = 0.75; Y0 = 1.3; X1 = 2.75; Y1 = 3.3|}
                        |]
                        colors
                    |> Array.map (fun (position, color) ->
                        initCircleShape position.X0 position.Y0 position.X1 position.Y1 color
                    )
                let layout =
                    Layout.init(
                        Shapes = shapes
                    )
                    |> Layout.AddLinearAxis(StyleParam.SubPlotId.XAxis 1, axis)
                    |> Layout.AddLinearAxis(StyleParam.SubPlotId.YAxis 1, axis)
                let positionSet,textSet =
                    let vennSingleText =
                        vennCount
                        |> Array.filter (fun (label,_) -> label.Length = 1)
                        |> Array.map (fun (label,count) ->
                            sprintf "%s<br>%i" label.[0] count
                        )       
                    let intersectionText =
                        let singleLabels =
                            vennCount
                            |> Array.filter (fun (label,_) -> label.Length = 1)
                            |> Array.collect fst
                        let multipleLabels =
                            vennCount
                            |> Array.filter (fun (label,_) -> label.Length > 1)

                        let checkLabel (labels: string[]) (singleLabels: string[]) i1 i2 =
                            labels |> Array.contains singleLabels.[i1] &&
                            labels |> Array.contains singleLabels.[i2]

                        [|
                            multipleLabels |> Array.filter (fun (label,_) -> checkLabel label singleLabels 0 1 && label.Length = 2)
                            multipleLabels |> Array.filter (fun (label,_) -> checkLabel label singleLabels 0 2 && label.Length = 2)
                            multipleLabels |> Array.filter (fun (label,_) -> checkLabel label singleLabels 1 2 && label.Length = 2)
                            multipleLabels |> Array.filter (fun (label,_) -> label.Length = 3)
                        |]
                        |> Array.concat
                        |> Array.map (fun (label,count) ->
                            sprintf "%i" count
                        )
                            
                    [|1.,1.;2.5,1.;1.75,2.25;1.75,1.; 1.325,1.6625;2.125,1.6625;1.75,1.45|],
                    Array.append vennSingleText intersectionText
                Chart.Scatter(
                    positionSet,
                    StyleParam.Mode.Text,
                    Labels = textSet,
                    TextFont = textFont
                )
                |> Chart.withLayout layout
            | _ -> failwith "Only 2 or 3 sets are supported"





// #########################################
// Chord connection for compatibility to Chord diagram

    type ChordConnections = {
        Group : int
        Value : int
    }

    let private chordConnectionsToJSON (c:ChordConnections) =
        sprintf "{\"group\":%i,\"value\":%i}" c.Group c.Value


    let toChordConnections (labels:array<'a>) (genericVenn:GenericVenn<'a,'b>) =     
        let f l = labels |> Array.findIndex (fun x -> x = l)
        genericVenn
        |> Seq.map (fun kv -> 
            kv.Value.Label 
            |> Seq.map (fun l -> { Group = f l; Value = kv.Value.Set.Count} )
                    )

    let chordConnectionsToString (d:seq<seq<ChordConnections>>) =
        d
        |> Seq.map (fun cl ->
            cl |> Seq.map (fun c -> chordConnectionsToJSON c) |> Seq.toArray |> String.concat "," 
                    )
        |> Seq.map (fun s -> sprintf "[%s]" s )
        |> Seq.toArray
        |> String.concat "," 
        |> fun s -> sprintf "[%s]" s




