namespace BioFSharp.Vis

open Plotly.NET
open Plotly.NET.LayoutObjects

module UpsetParts =

    let combineArrayAlternating (a: array<'a>) (b: array<'a>) =
        List.fold2 (fun acc a' b' ->
            b'::a'::acc   
        ) [] (a |> List.ofArray) (b |> List.ofArray)
        |> List.rev
        |> Array.ofList

    let createLinearAxisWithRange (maxRange: float) =
        let range  = StyleParam.Range.MinMax (-0.5,maxRange)
        LinearAxis.init(Range=range, ShowGrid=false, ShowLine=false, ShowTickLabels=false, ZeroLine=false)

    let createLinearAxisWithRangeDomain (maxRange: float) (domain: float*float) =
        let range  = StyleParam.Range.MinMax (-0.5,maxRange)
        LinearAxis.init(Range=range, ShowGrid=false, ShowLine=false, ShowTickLabels= false, ZeroLine=false, Domain=StyleParam.Range.MinMax domain)

    let createLinearAxisWithRangeTickLabel (maxRange: float) (labels: string[]) (font: Font) =
        let range  = StyleParam.Range.MinMax (-0.5,maxRange)
        LinearAxis.init(Range=range, ShowGrid=false, ShowLine=false, ShowTickLabels=true, ZeroLine=false, TickMode=StyleParam.TickMode.Array, TickVals=[0 .. labels.Length - 1], TickText=labels, TickFont=font)

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

    let createIntersectionPointChart (data: (int*int)[]) (markerSize: int) (color: Color) =
        Chart.Point(data)
        |> Chart.withMarkerStyle(
            Symbol = StyleParam.MarkerSymbol.Circle,
            Size = markerSize,
            Color = color
        )

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
            lineChart
            pointChart
        ]
        |> Chart.combine

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

    let createIntersectionSizePlots (intersectionCount: (string list*int)[]) (maxX: float) (color: Color) (domainIntersection: float*float) (textFont: Font) =
        intersectionCount
        |> Array.map snd
        |> fun count -> 
            Chart.Column(count, Color = color)
        |> Chart.withXAxis (createLinearAxisWithRangeDomain maxX domainIntersection)
        |> Chart.withYAxisStyle("Intersection Size", TitleFont=textFont)
        |> Chart.withTraceName(ShowLegend=false)

    let alignIntersectionData (venn: (string list * Set<'a>)[]) (setData: Map<'a,'b>) =
        venn
        |> Array.map (fun (_,set) ->
            set
            |> Set.toArray
            |> Array.map (fun entry ->
                setData.[entry]
            )
        )

    let createIntersectionDataPlots (intersectionData: array<array<'b>>) (title: string) (maxX: float) (chartFun: array<'b> -> GenericChart.GenericChart) (domainIntersection: float*float) (textFont: Font) =
        intersectionData
        |> Array.map chartFun
        |> Chart.combine
        |> Chart.withLegend false
        |> Chart.withXAxis (createLinearAxisWithRangeDomain maxX domainIntersection)
        |> Chart.withYAxisStyle (title, TitleFont=textFont)

module Upset =

    open UpsetParts
    
    let createUpsetWith (labels:array<string>) (sets:array<Set<'a>>) (setData: array<Map<'a,'b>>) (setDataChartsTitle: array<(array<'b> -> GenericChart.GenericChart)*string>) 
        (markerSize: int) (mainColor: Color) (secondaryColor: Color) (domainSet: float*float) (domainIntersection: float*float) (textFont: Font) (maxIntersections: int) =
        let labelIDs =
            labels
            |> Array.mapi (fun i label -> label,i)
        let venn =
            Venn.ofSetList labels sets|> Map.toArray
            |> Array.map (fun (_,labelSet) -> 
                labelSet.Label, labelSet.Set
            )
            |> Array.filter (fun (id,_) -> not id.IsEmpty)
            |> Array.sortByDescending (snd >> Set.count)
            |> fun v ->
                if v.Length <= maxIntersections then
                    v
                else
                    v
                    |> Array.take maxIntersections
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
            |> Chart.withYAxis (createLinearAxisWithRangeTickLabel maxY labels textFont)
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

    let createUpset (labels: array<string>) (sets: array<Set<'a>>) =
        createUpsetWith labels sets [||] [||] 15 Color.Table.Office.darkBlue Color.Table.Office.lightBlue (0., 0.2) (0.3, 1.) (Font.init(StyleParam.FontFamily.Arial, Size=20.)) 20
