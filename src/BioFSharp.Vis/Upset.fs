namespace BioFSharp.Vis


module Upset =

    open Plotly.NET
    open Plotly.NET.LayoutObjects
    
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

    let createIntersectionPlotPart (position:int) (intersectingSets: string) (labelIDs: (string*int)[]) (markerSize: int) (colorIntersecting: Color) (colorNotIntersecting: Color) =
        let setIDsPresent, setIDsNotPresent =
            let intersectingSetsArray = intersectingSets.Split '&'
            labelIDs
            |> Array.partition (fun (label,_) -> intersectingSetsArray |> Array.contains label)
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
    Layout.init
    let createIntersectionSizePlots (intersectionCount: (string*int)[]) (maxX: float) (color: Color) (domainIntersection: float*float) (textFont: Font) =
        intersectionCount
        |> Array.map snd
        |> fun count -> 
            Chart.Column(count, Color = color)
        |> Chart.withXAxis (createLinearAxisWithRangeDomain maxX domainIntersection)
        |> Chart.withYAxisStyle("Intersection Size", TitleFont=textFont)
        |> Chart.withTraceName(ShowLegend=false)

    let createUpsetWith (labels:array<string>) (sets:array<Set<'a>>) (markerSize: int) (mainColor: Color) (secondaryColor: Color) (domainSet: float*float) (domainIntersection: float*float) (textFont: Font) =
        let labelIDs =
            labels
            |> Array.mapi (fun i label -> label,i)
        let vennCount =
            Venn.ofSetList labels sets
            |> Venn.toVennCount
            |> Map.toArray
            |> Array.filter (fun (id,_) -> id <> "union")
            |> Array.sortByDescending snd
        let maxX = float vennCount.Length - 0.5
        let maxY = float labels.Length - 0.5
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
            [
                Chart.Invisible()
                intersectionSizePlot
                setSizePlot
                intersectionPlot
            ]
            |> Chart.Grid(2,2)
        grid

    let createUpset (labels:array<string>) (sets:array<Set<'a>>) =
        createUpsetWith labels sets 15 Color.Table.Office.darkBlue Color.Table.Office.lightBlue (0.,0.2) (0.3,1.) (Font.init(StyleParam.FontFamily.Arial,Size=20.))
