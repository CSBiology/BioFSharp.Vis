namespace BioFSharp.Vis


module Upset =

    open Plotly.NET
    open Plotly.NET.LayoutObjects
    
    let createIntersectionLineChart (data: (int*int)[]) =
        Chart.Line (
            data,
            ShowMarkers = true,
            Dash = StyleParam.DrawingStyle.Solid,
            Width = 5.,
            Color = Color.fromString "black"
        )
        |> Chart.withMarkerStyle(
            Symbol = StyleParam.MarkerSymbol.Circle,
            Size = 25
        )
    
    let createIntersectionPointChart (data: (int*int)[]) =
        Chart.Point(data)
        |> Chart.withMarkerStyle(
            Symbol = StyleParam.MarkerSymbol.Circle,
            Size = 25,
            Color = Color.fromString "grey"
        )
    
    let createIntersectionPlotPart (position:int) (intersectingSets: string) (labelIDs: Map<string,int>) =
        let setIDsPresent =
            intersectingSets.Split '&'
            |> Array.map (fun set -> labelIDs.[set])
        let setIDsNotPresent =
            [|0 .. labelIDs.Count - 1|]
            |> Array.filter (fun id -> 
                setIDsPresent
                |> Array.contains id
                |> not
            )
        let lineChart =
            let idWithPosition =
                setIDsPresent
                |> Array.map (fun id ->
                    position,id
                )
            createIntersectionLineChart idWithPosition
        let pointChart =
            let idWithPosition =
                setIDsNotPresent
                |> Array.map (fun id ->
                    position,id
                )
            createIntersectionPointChart idWithPosition
        [
            lineChart
            pointChart
        ]
        |> Chart.combine
    
    let createLinearAxisWithRange (maxRange: float) =
        let range  = StyleParam.Range.MinMax (-0.5,maxRange)
        LinearAxis.init(Range=range, ShowGrid=false, ShowLine=false, ShowTickLabels= false, ZeroLine=false)
    
    
    let createUpset (labels:array<string>) (sets:array<Set<'a>>) =
        let labelIDs =
            labels
            |> Array.mapi (fun i label -> label,i)
            |> Map.ofArray
        let vennCount =
            Venn.ofSetList labels sets
            |> Venn.toVennCount
            |> Map.toArray
            |> Array.filter (fun (id,count) -> id <> "union")
            |> Array.sortByDescending snd
        let maxX = float vennCount.Length - 0.5
        let maxY = float labels.Length - 0.5
        let intersectionPlot =
            vennCount
            |> Array.mapi (fun position (intersectingSets,_) ->
                createIntersectionPlotPart position intersectingSets labelIDs
            )
            |> Chart.combine
            |> Chart.withYAxis (createLinearAxisWithRange maxY)
            |> Chart.withXAxis (createLinearAxisWithRange maxX)
            |> Chart.withTraceName(ShowLegend=false)
        let setSizePlot =
            let labelCount =
                Array.map2 (fun label (set: Set<'a>) ->
                    label, set.Count
                ) labels sets
            let maxSetSize =
                labelCount
                |> Array.maxBy snd
                |> snd
            Chart.Bar (labelCount)
            |> Chart.withXAxisStyle("Set Size",MinMax = (float maxSetSize,0.))
            |> Chart.withYAxisStyle("",MinMax=(-0.5,maxY))
            |> Chart.withTraceName(ShowLegend=false)
        let intersectionSizePlot =
            vennCount
            |> Array.map snd
            |> Chart.Column
            |> Chart.withXAxis (createLinearAxisWithRange maxX)
            |> Chart.withYAxisStyle("Intersection Size")
            |> Chart.withTraceName(ShowLegend=false)
        let grid =
            [
                Chart.Invisible()
                intersectionSizePlot
                setSizePlot
                intersectionPlot
            ]
            |> Chart.Grid(2,2)
        grid
