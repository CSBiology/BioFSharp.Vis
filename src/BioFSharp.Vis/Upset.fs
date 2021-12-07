namespace BioFSharp.Vis

open Plotly.NET
open Plotly.NET.LayoutObjects

module UpSetParts =
    
    /// Combines two arrays by taking the next element alternating from array a and b
    let combineArrayAlternating (a: array<'a>) (b: array<'a>) =
        List.fold2 (fun acc a' b' ->
            b'::a'::acc   
        ) [] (a |> List.ofArray) (b |> List.ofArray)
        |> List.rev
        |> Array.ofList

    /// Creates a linear axis without lines and ticks with a given range
    let createLinearAxisWithRange (maxRange: float) =
        let range  = StyleParam.Range.MinMax (-0.5,maxRange)
        LinearAxis.init(Range=range, ShowGrid=false, ShowLine=false, ShowTickLabels=false, ZeroLine=false)

    /// Creates a linear axis without lines and ticks with a given range and domain
    let createLinearAxisWithRangeDomain (maxRange: float) (domain: float*float) =
        let range  = StyleParam.Range.MinMax (-0.5,maxRange)
        LinearAxis.init(Range=range, ShowGrid=false, ShowLine=false, ShowTickLabels= false, ZeroLine=false, Domain=StyleParam.Range.MinMax domain)

    /// Creates a linear axis without lines with a given range and custom tick labels
    let createLinearAxisWithRangeTickLabel (maxRange: float) (labels: string[]) (font: Font) =
        let range  = StyleParam.Range.MinMax (-0.5,maxRange)
        LinearAxis.init(Range=range, ShowGrid=false, ShowLine=false, ShowTickLabels=true, ZeroLine=false, TickMode=StyleParam.TickMode.Array, TickVals=[0 .. labels.Length - 1], TickText=labels, TickFont=font)

    /// Creates the line chart connecting the sets present in the intersection for the intersection matrix
    let createIntersectionLineChart (data: (int*int)[]) (markerSize: int) (color: Color) =
        Chart.Line (
            data,
            ShowMarkers = true,
            Dash = StyleParam.DrawingStyle.Solid,
            Width = (float markerSize / 5.),
            Color = color
        )
        |> Chart.withMarkerStyle(
            Symbol = StyleParam.MarkerSymbol.Circle,
            Size = markerSize
        )
    
    /// Creates the point chart for the sets not present in the intersection for the intersection matrix
    let createIntersectionPointChart (data: (int*int)[]) (markerSize: int) (color: Color) =
        Chart.Point(data)
        |> Chart.withMarkerStyle(
            Symbol = StyleParam.MarkerSymbol.Circle,
            Size = markerSize,
            Color = color
        )
    
    /// Creates the part of the intersection matrix representing the current intersection. 
    /// The position on the y-Axis is based on the order the labels and sets are given in. 
    /// The position on the x-Axis is based on the given position (determined by intersection size).
    let createIntersectionPlotPart (position:int) (intersectingSets: string list) (labelIDs: (string*int)[]) (markerSize: int) (colorIntersecting: Color) (colorNotIntersecting: Color) =
        let setIDsPresent, setIDsNotPresent =
            labelIDs
            |> Array.partition (fun (label,_) -> intersectingSets |> List.contains label)
        let lineChart =
            let idWithPosition =
                setIDsPresent
                |> Array.map (fun (_,id) ->
                    position,id
                )
            createIntersectionLineChart idWithPosition markerSize colorIntersecting
        let pointChart =
            let idWithPosition =
                setIDsNotPresent
                |> Array.map (fun (_,id) ->
                    position,id
                )
            createIntersectionPointChart idWithPosition markerSize colorNotIntersecting
        [
            pointChart
            lineChart
        ]
        |> Chart.combine

    /// Creates a bar chart with the set sizes
    let createSetSizePlot (labels:array<string>) (sets:array<Set<'a>>) (maxY: float) (color: Color) (domainSet: float*float) (textFont: Font) =
        let labelCount =
            Array.map2 (fun label (set: Set<'a>) ->
                label, set.Count
            ) labels sets
        let maxSetSize =
            labelCount
            |> Array.maxBy snd
            |> snd
        Chart.Bar (labelCount, Color = color)
        |> Chart.withXAxisStyle("Set Size",MinMax=(float maxSetSize,0.), Domain=domainSet, TitleFont=textFont)
        |> Chart.withYAxis (createLinearAxisWithRange maxY)
        |> Chart.withTraceName(ShowLegend=false)

    /// Creates a bar chart with the intersection sizes
    let createIntersectionSizePlots (intersectionCount: (string list*int)[]) (maxX: float) (color: Color) (domainIntersection: float*float) (textFont: Font) =
        intersectionCount
        |> Array.map snd
        |> fun count -> 
            Chart.Column(count, Color = color)
        |> Chart.withXAxis (createLinearAxisWithRangeDomain maxX domainIntersection)
        |> Chart.withYAxisStyle("Intersection Size", TitleFont=textFont)
        |> Chart.withTraceName(ShowLegend=false)

    /// Aligns the map with Setelement->Feature to the intersections
    let alignIntersectionData (venn: (string list * Set<'a>)[]) (setData: Map<'a,'b>) =
        venn
        |> Array.map (fun (_,set) ->
            set
            |> Set.toArray
            |> Array.map (fun entry ->
                setData.[entry]
            )
        )

    /// Creates the intersection feature plot with the given charting function
    let createIntersectionDataPlots (intersectionData: array<array<'b>>) (title: string) (maxX: float) (chartFun: array<'b> -> GenericChart.GenericChart) (domainIntersection: float*float) (textFont: Font) =
        intersectionData
        |> Array.map chartFun
        |> Chart.combine
        |> Chart.withLegend false
        |> Chart.withXAxis (createLinearAxisWithRangeDomain maxX domainIntersection)
        |> Chart.withYAxisStyle (title, TitleFont=textFont)

module UpSet =

    open UpSetParts
    
    /// Creates an Upset Plot (https://upset.app/) from given labels and sets. 
    /// Includes additional customization parameters: 
    /// - setData & setDataChartsTitle: Map of Setelement -> Feature and charting function with y-Axis title. Both are given as an array to allow multiple feature plots 
    /// - markerSize: Size of the markers in the intersection matrix 
    /// - mainColor & secondaryColor: Main and secondary color of the plot 
    /// - domainSet: Range for the set size part of the plot (left side). Range goes from 0 to 1 
    /// - domainIntersection: Range for the intersection part of the plot (right side). Range goes from 0 to 1 
    /// - textFont: Text font used for axis descriptions 
    /// - textFontLabel: Text font used for label descriptions 
    /// - minIntersectionSize: Minimal number of elements in an intersection
    /// - sortIntersectionsBy: Sorting function for intersections. The first element of the tuple is a list of sets in the intersection, the second part 
    ///   a Set of intersecting elements
    type Chart with
        static member UpSet 
            (
                (labels:array<string>),
                (sets:array<Set<'a>>),
                (?setData: array<Map<'a,'b>>),
                (?setDataChartsTitle: array<(array<'b> -> GenericChart.GenericChart)*string>),
                (?markerSize: int),
                (?mainColor: Color),
                (?secondaryColor: Color),
                (?domainSet: float*float),
                (?domainIntersection: float*float),
                (?textFont: Font),
                (?textFontLabel: Font),
                (?minIntersectionSize: int),
                (?sortIntersectionsBy: ((string list * Set<'a>)[]) -> ((string list * Set<'a>)[]))
            ) =
            let setData             = setData             |> Option.defaultValue Array.empty
            let setDataChartsTitle  = setDataChartsTitle  |> Option.defaultValue Array.empty
            let markerSize          = markerSize          |> Option.defaultValue 25
            let mainColor           = mainColor           |> Option.defaultValue Color.Table.Office.darkBlue
            let secondaryColor      = secondaryColor      |> Option.defaultValue Color.Table.Office.lightBlue
            let domainSet           = domainSet           |> Option.defaultValue (0., 0.2)
            let domainIntersection  = domainIntersection  |> Option.defaultValue (0.3, 1.)
            let textFont            = textFont            |> Option.defaultValue (Font.init(StyleParam.FontFamily.Arial, Size=20.))
            let textFontLabel       = textFontLabel       |> Option.defaultValue (Font.init(StyleParam.FontFamily.Arial, Size=20.))
            let minIntersectionSize = minIntersectionSize |> Option.defaultValue 5
            let sortIntersectionsBy = sortIntersectionsBy |> Option.defaultValue (Array.sortByDescending(snd >> Set.count))
            let labelIDs =
                labels
                |> Array.mapi (fun i label -> label,i)
            let venn =
                Venn.ofSetList labels sets
                |> Map.toArray
                |> Array.map (fun (_,labelSet) -> 
                    labelSet.Label, labelSet.Set
                )
                |> Array.filter (fun (id,s) -> not id.IsEmpty && s.Count >= minIntersectionSize)
                |> sortIntersectionsBy
            let vennCount =
                venn
                |> Array.map (fun (id,set) -> id,set.Count)
            let maxX = float vennCount.Length - 0.5
            let maxY = float labels.Length - 0.5
            let intersectionData =
                setData
                |> Array.map (alignIntersectionData venn)
            let intersectionDataCharts =
                let charts =
                    intersectionData
                    |> Array.map2 (fun (chartFun,title) sD ->
                        createIntersectionDataPlots sD title maxX chartFun domainIntersection textFont
                    ) setDataChartsTitle
                let emptyCharts = 
                    [|0 .. setData.Length - 1|]
                    |> Array.map (fun _ -> Chart.Invisible())
                combineArrayAlternating emptyCharts charts
            let intersectionPlot =
                vennCount
                |> Array.mapi (fun position (intersectingSets,_) ->
                    createIntersectionPlotPart position intersectingSets labelIDs markerSize mainColor secondaryColor
                )
                |> Chart.combine
                |> Chart.withYAxis (createLinearAxisWithRangeTickLabel maxY labels textFontLabel)
                |> Chart.withXAxis (createLinearAxisWithRangeDomain maxX domainIntersection)
                |> Chart.withTraceName(ShowLegend=false)
            let setSizePlot = createSetSizePlot labels sets maxY mainColor domainSet textFont
            let intersectionSizePlot = createIntersectionSizePlots vennCount maxX mainColor domainIntersection textFont
            let grid =
                Array.append
                    intersectionDataCharts
                    [|
                        Chart.Invisible()
                        intersectionSizePlot
                        setSizePlot
                        intersectionPlot
                    |]
                |> Chart.Grid(2+setData.Length,2)
            grid