unit LGSimpleGraphTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  LGUtils,
  LGSparseGraph,
  LGSimpleGraph;


type

  { TSimpleGraphTest }

  TSimpleGraphTest = class(TTestCase)
  private
  type
    TGraph  = TIntChart;
    TRef    = specialize TGAutoRef<TGraph>;

  var
    FCallCount: Integer;
    function  GenerateTestGr1: TGraph;
    function  GenerateTestGr2: TGraph;
    function  GenerateTestGr3: TGraph;
    function  GenerateStar: TGraph;
    function  GenerateCycle: TGraph;
    function  GenerateWheel: TGraph;
    function  GenerateComplete: TGraph;
    procedure EdgeAdding(constref {%H-}aSrc, {%H-}aDst: Integer; {%H-}aData: TGraph.PEdgeData);
  published
    procedure AddVertices;
    procedure SaveToStream;
    procedure Clone;
    procedure Degree;
    procedure Connected;
    procedure RemoveVertex;
    procedure SeparateCount1;
    procedure SeparateCount2;
    procedure SeparateGraph1;
    procedure SeparateGraph2;
    procedure SubgraphFromVertexList1;
    procedure SubgraphFromVertexList2;
    procedure SubgraphFromTree;
    procedure SubgraphFromEdges1;
    procedure SubgraphFromEdges2;
    procedure CreateLineGraph;
    procedure DistinctEdges;
    procedure EnsureConnected;
    procedure EnsureConnected2;
    procedure PathExists;
    procedure SeparatePop;
    procedure GetSeparate;
    procedure FindSeparates;
    procedure IsTree;
    procedure IsStar;
    procedure IsCycle;
    procedure IsWheel;
    procedure IsComplete;
    procedure IsRegular;
    procedure ContainsCycle;
    procedure ContainsEulerianPath;
    procedure ContainsEulerianCycle;
    procedure FindEulerianPath;
    procedure FindEulerianCycle;
    procedure ContainsCutVertex;
    procedure FindCutVertices;
    procedure RemoveCutVertices;
    procedure ContainsBridge;
    procedure FindBridges;

    procedure IsBiconnected;
    procedure FindBicomponents;
    procedure EnsureBiconnected;
    procedure FindMerics;
    procedure FindCenter;
  end;

implementation

function TSimpleGraphTest.GenerateTestGr1: TGraph;
begin
  //see TestGr1.png
  Result := TGraph.Create;
  Result.AddVertexRange(0, 12);
  Result.AddEdges([0, 1, 0, 2, 0, 3, 0, 5, 0, 6, 2, 3, 3, 5, 3, 4, 6, 4, 4, 9, 6, 9, 7, 6,
                   8, 7, 9, 10, 9, 11, 9, 12, 11, 12]);
end;

function TSimpleGraphTest.GenerateTestGr2: TGraph;
begin
  //see TestGr2.png
  Result := TGraph.Create;
  Result.AddVertexRange(1, 16);
  Result.AddEdges([1, 2, 1, 3, 1, 4, 1, 6, 1, 7, 3, 4, 4, 6, 4, 5, 7, 4, 5, 10, 7, 10,
                   10, 11, 10, 12, 10, 13, 12, 13,
                   8, 9, 8, 14, 8, 15, 9, 15, 9, 16, 14, 15, 14, 16]);
end;

function TSimpleGraphTest.GenerateTestGr3: TGraph;
begin
  //see TestGr3.png
  Result := TGraph.Create;
  Result.AddVertexRange(1, 10);
  Result.AddEdges([1, 2, 2, 3, 3, 4, 4, 1, 2, 6, 6, 4, 4, 5, 5, 2, 1, 7, 1, 8, 8, 9, 7, 9, 3, 10]);
end;

function TSimpleGraphTest.GenerateStar: TGraph;
begin
  //see Star.png
  Result := TGraph.Create;
  Result.AddVertexRange(1, 12);
  Result.AddEdges([1, 2, 1, 3, 1, 4, 1, 5, 1, 6, 1, 7, 1, 8, 1, 9, 1, 10, 1, 11, 1, 12]);
end;

function TSimpleGraphTest.GenerateCycle: TGraph;
begin
  //see Cycle.png
  Result := TGraph.Create;
  Result.AddVertexRange(1, 12);
  Result.AddEdges([1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 1, 12]);
end;

function TSimpleGraphTest.GenerateWheel: TGraph;
begin
  //see Wheel.png
  Result := TGraph.Create;
  Result.AddVertexRange(1, 12);
  Result.AddEdges([1, 2, 1, 3, 1, 4, 1, 5, 1, 6, 1, 7, 1, 8, 1, 9, 1, 10, 1, 11, 1, 12]);
  Result.AddEdges([2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 2, 12]);
end;

function TSimpleGraphTest.GenerateComplete: TGraph;
var
  I, J: SizeInt;
begin
  Result := TGraph.Create;
  Result.AddVertexRange(1, 12);
  for I := 0 to Pred(Result.VertexCount) do
    for J := 0 to Pred(Result.VertexCount) do
      if I > J then
        Result.AddEdgeI(I, J);
end;

procedure TSimpleGraphTest.EdgeAdding(constref aSrc, aDst: Integer; aData: TGraph.PEdgeData);
begin
  Inc(FCallCount);
end;

procedure TSimpleGraphTest.AddVertices;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertTrue(g.AddVertices([]) = 0);
  AssertTrue(g.AddVertices([1]) = 1);
  AssertTrue(g.VertexCount = 1);
  AssertTrue(g.AddVertices([3, 4, 5]) = 3);
  AssertTrue(g.VertexCount = 4);
  AssertTrue(g.AddVertices([3, 4, 5, 6]) = 1);
  AssertTrue(g.VertexCount = 5);
end;

procedure TSimpleGraphTest.SaveToStream;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
  Stream: TMemoryStream;
  e: TGraph.TEdge;
  Title, Description: string;
begin
  Title := 'Test graph';
  Description := 'for testing purposes';
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertTrue(g.VertexCount = 13);
  AssertTrue(g.EdgeCount = 17);
  g.Title := Title;
  g.Description.Text := Description;
  g2 := Ref2;
  Stream := TMemoryStream.Create;
  try
    g.SaveToStream(Stream);
    Stream.Seek(0, soFromBeginning);
    g2.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
  AssertTrue(g2.VertexCount = 13);
  AssertTrue(g2.EdgeCount = 17);
  for e in g.Edges do
    AssertTrue(g2.ContainsEdgeI(e.Source, e.Destination));
  for e in g2.Edges do
    AssertTrue(g.ContainsEdgeI(e.Source, e.Destination));
  AssertTrue(g2.Title = Title);
  AssertTrue(g2.Description.Text = Description);
end;

procedure TSimpleGraphTest.Clone;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
  e: TGraph.TEdge;
  Title, Description: string;
begin
  Title := 'Test graph';
  Description := 'for testing purposes';
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertTrue(g.VertexCount = 13);
  AssertTrue(g.EdgeCount = 17);
  g.Title := Title;
  g.Description.Text := Description;
  {%H-}Ref2.Instance := g.Clone;
  g2 := Ref2;
  AssertTrue(g2.VertexCount = 13);
  AssertTrue(g2.EdgeCount = 17);
  for e in g.Edges do
    AssertTrue(g2.ContainsEdgeI(e.Source, e.Destination));
  for e in g2.Edges do
    AssertTrue(g.ContainsEdgeI(e.Source, e.Destination));
  AssertTrue(g2.Title = Title);
  AssertTrue(g2.Description.Text = Description);
end;

procedure TSimpleGraphTest.Degree;
var
  Ref: TRef;
  g: TGraph;
  Raised: Boolean = False;
begin
  g := Ref;
  g.AddEdges([1, 2]);
  AssertTrue(g.Degree(1) = 1);
  AssertTrue(g.Degree(2) = 1);
  g.RemoveVertex(2);
  AssertTrue(g.Degree(1) = 0);
  try
    g.Degree(2);
  except
    on e: EGraphError do
      Raised := True;
  end;
  AssertTrue(Raised);
  g.AddEdges([1, 2, 1, 3, 1, 4]);
  AssertTrue(g.Degree(1) = 3);
  AssertTrue(g.Degree(2) = 1);
  AssertTrue(g.Degree(3) = 1);
  AssertTrue(g.Degree(4) = 1);
end;

procedure TSimpleGraphTest.Connected;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.Connected);
  g.AddVertex(1);
  AssertTrue(g.Connected);
  g.AddVertexRange(2, 4);
  AssertFalse(g.Connected);
  g.AddEdges([1, 2, 1, 3, 1, 4]);
  AssertTrue(g.Connected);
  g.RemoveEdge(1, 3);
  AssertFalse(g.Connected);
  g.RemoveVertex(3);
  AssertTrue(g.Connected);
end;

procedure TSimpleGraphTest.RemoveVertex;
var
  Ref: TRef;
  g: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertTrue(g.VertexCount = 13);
  g.RemoveVertex(9);
  AssertTrue(g.VertexCount = 12);
  AssertTrue(g.SeparateCount = 3);
  AssertTrue(g.Degree(10) = 0);
  AssertTrue(g.Degree(11) = 1);
  AssertTrue(g.Degree(12) = 1);
end;

procedure TSimpleGraphTest.SeparateCount1;
var
  Ref: TRef;
  g: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertTrue(g.SeparateCount = 1);
end;

procedure TSimpleGraphTest.SeparateCount2;
var
  Ref: TRef;
  g: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestGr2;
  g := Ref;
  AssertTrue(g.SeparateCount = 2);
end;

procedure TSimpleGraphTest.SeparateGraph1;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestGr2;
  g := Ref;
  AssertTrue(g.VertexCount = 16);
  {%H-}Ref2.Instance := g.SeparateGraph(8);
  g2 := Ref2;
  AssertTrue(g2.VertexCount = 5);
  AssertTrue(g2.Connected);
  AssertTrue(g2.ContainsVertex(8));
  AssertTrue(g2.ContainsVertex(9));
  AssertTrue(g2.ContainsVertex(14));
  AssertTrue(g2.ContainsVertex(15));
  AssertTrue(g2.ContainsVertex(16));
end;

procedure TSimpleGraphTest.SeparateGraph2;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestGr2;
  g := Ref;
  {%H-}Ref2.Instance := g.SeparateGraph(1);
  g2 := Ref2;
  AssertTrue(g2.VertexCount = 11);
  AssertTrue(g2.Connected);
  AssertTrue(g2.ContainsVertex(1));
  AssertTrue(g2.ContainsVertex(2));
  AssertTrue(g2.ContainsVertex(3));
  AssertTrue(g2.ContainsVertex(4));
  AssertTrue(g2.ContainsVertex(5));
  AssertTrue(g2.ContainsVertex(6));
  AssertTrue(g2.ContainsVertex(7));
  AssertTrue(g2.ContainsVertex(10));
  AssertTrue(g2.ContainsVertex(11));
  AssertTrue(g2.ContainsVertex(12));
  AssertTrue(g2.ContainsVertex(13));
end;

procedure TSimpleGraphTest.SubgraphFromVertexList1;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestGr2;
  g := Ref;
  AssertTrue(g.VertexCount = 16);
  {%H-}Ref2.Instance := g.SubgraphFromVertexList([]);
  g2 := Ref2;
  AssertTrue(g2.IsEmpty);
end;

procedure TSimpleGraphTest.SubgraphFromVertexList2;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestGr2;
  g := Ref;
  AssertTrue(g.VertexCount = 16);
  {%H-}Ref2.Instance := g.SubgraphFromVertexList(
    [g.IndexOf(8), g.IndexOf(9), g.IndexOf(14), g.IndexOf(15), g.IndexOf(16)]);
  g2 := Ref2;
  AssertTrue(g2.VertexCount = 5);
  AssertTrue(g2.EdgeCount = 7);
  AssertTrue(g2.ContainsEdge( 8,  9));
  AssertTrue(g2.ContainsEdge( 8, 14));
  AssertTrue(g2.ContainsEdge( 8, 15));
  AssertTrue(g2.ContainsEdge( 9, 15));
  AssertTrue(g2.ContainsEdge( 9, 16));
  AssertTrue(g2.ContainsEdge(14, 15));
  AssertTrue(g2.ContainsEdge(14, 16));
end;

procedure TSimpleGraphTest.SubgraphFromTree;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
  Tree: TIntArray;
begin
  {%H-}Ref.Instance := GenerateTestGr2;
  g := Ref;
  AssertTrue(g.VertexCount = 16);
  Tree := g.DfsTree(1);
  {%H-}Ref2.Instance := g.SubgraphFromTree(Tree);
  g2 := Ref2;
  AssertTrue(g2.VertexCount = 16);
  AssertTrue(g2.SeparateCount = 6);
  AssertTrue(g2.Degree(8) = 0);
  AssertTrue(g2.Degree(9) = 0);
  AssertTrue(g2.Degree(14) = 0);
  AssertTrue(g2.Degree(15) = 0);
  AssertTrue(g2.Degree(16) = 0);
end;

procedure TSimpleGraphTest.SubgraphFromEdges1;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
  Edges: TIntEdgeArray = nil;
begin
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertTrue(g.VertexCount = 13);
  {%H-}Ref2.Instance := g.SubgraphFromEdges(Edges);
  g2 := Ref2;
  AssertTrue(g2.IsEmpty);
end;

procedure TSimpleGraphTest.SubgraphFromEdges2;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
  Edges: TIntEdgeArray;
begin
  {%H-}Ref.Instance := GenerateTestGr2;
  g := Ref;
  AssertTrue(g.VertexCount = 16);
  Edges := [TIntEdge.Create(0, 1), TIntEdge.Create(0, 2), TIntEdge.Create(2, 3), TIntEdge.Create(3, 6)];
  {%H-}Ref2.Instance := g.SubgraphFromEdges(Edges);
  g2 := Ref2;
  AssertTrue(g2.VertexCount = 5);
  AssertTrue(g2.ContainsVertex(1));
  AssertTrue(g2.ContainsVertex(2));
  AssertTrue(g2.ContainsVertex(3));
  AssertTrue(g2.ContainsVertex(4));
  AssertTrue(g2.ContainsVertex(7));
end;

procedure TSimpleGraphTest.CreateLineGraph;
var
  Ref: TRef;
  g: TGraph;
  lg: TLineGraph;
  I, DegSum: Integer;
  v: TOrdIntPair;
  e: TLineGraph.TEdge;
begin
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  DegSum := 0;
  for I := 0 to Pred(g.VertexCount) do
    DegSum += g.DegreeI(I) * g.DegreeI(I);
  DegSum := DegSum div 2 - g.EdgeCount;
  lg := g.CreateLineGraph;
  try
    AssertTrue(lg.SeparateCount = 1);
    AssertTrue(lg.VertexCount = g.EdgeCount);
    AssertTrue(lg.EdgeCount = DegSum);
    for v in lg.Vertices do
      AssertTrue(g.ContainsEdge(v.Left, v.Right));
    for e in lg.Edges do
      AssertTrue(g.ContainsVertex(e.Data.Value));
  finally
    lg.Free;
  end;
end;

procedure TSimpleGraphTest.DistinctEdges;
var
  Ref: TRef;
  g: TGraph;
  e: TGraph.TEdge;
  I: Integer;
begin
  g := {%H-}Ref;
  I := 0;
  for e in g.DistinctEdges do
    Inc(I);
  AssertTrue(I = 0);
  Ref.Instance := GenerateTestGr2;
  g := Ref;
  I := 0;
  for e in g.Edges do
    Inc(I);
  AssertTrue(I = 44);
  I := 0;
  for e in g.DistinctEdges do
    Inc(I);
  AssertTrue(I = 22);
end;

procedure TSimpleGraphTest.EnsureConnected;
var
  Ref: TRef;
  g: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestGr2;
  g := Ref;
  AssertTrue(g.EdgeCount = 22);
  AssertFalse(g.Connected);
  AssertTrue(g.SeparateCount = 2);
  g.EnsureConnected;
  AssertTrue(g.Connected);
  AssertTrue(g.SeparateCount = 1);
  AssertTrue(g.EdgeCount = 23);
end;

procedure TSimpleGraphTest.EnsureConnected2;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  FCallCount := 0;
  g.EnsureConnected(@EdgeAdding);
  AssertTrue(FCallCount = 0);
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  g.EnsureConnected(@EdgeAdding);
  AssertTrue(FCallCount = 0);
  Ref.Instance := GenerateTestGr2;
  g := Ref;
  g.EnsureConnected(@EdgeAdding);
  AssertTrue(FCallCount = 1);
end;

procedure TSimpleGraphTest.PathExists;
var
  Ref: TRef;
  g: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestGr2;
  g := Ref;
  AssertTrue(g.PathExists(1, 2));
  AssertTrue(g.PathExists(1, 13));
  AssertFalse(g.PathExists(1, 8));
  AssertFalse(g.PathExists(13, 14));
  g.EnsureConnected;
  AssertTrue(g.PathExists(1, 8));
  AssertTrue(g.PathExists(13, 14));
end;

procedure TSimpleGraphTest.SeparatePop;
var
  Ref: TRef;
  g: TGraph;
  Pop: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  Pop := g.SeparatePop(1);
  AssertTrue(Pop = 13);
  Pop := g.SeparatePop(11);
  AssertTrue(Pop = 13);
  Ref.Instance := GenerateTestGr2;
  g := Ref;
  Pop := g.SeparatePop(1);
  AssertTrue(Pop = 11);
  Pop := g.SeparatePop(14);
  AssertTrue(Pop = 5);
  g.EnsureConnected;
  Pop := g.SeparatePop(1);
  AssertTrue(Pop = 16);
  Pop := g.SeparatePop(14);
  AssertTrue(Pop = 16);
end;

procedure TSimpleGraphTest.GetSeparate;
var
  Ref: TRef;
  g: TGraph;
  Separate: TIntArray;
begin
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  Separate := g.GetSeparate(1);
  AssertTrue(System.Length(Separate) = 13);
  Separate := g.GetSeparate(8);
  AssertTrue(System.Length(Separate) = 13);
  Ref.Instance := GenerateTestGr2;
  g := Ref;
  Separate := g.GetSeparate(1);
  AssertTrue(System.Length(Separate) = 11);
  Separate := g.GetSeparate(13);
  AssertTrue(System.Length(Separate) = 11);
  Separate := g.GetSeparate(8);
  AssertTrue(System.Length(Separate) = 5);
  Separate := g.GetSeparate(16);
  AssertTrue(System.Length(Separate) = 5);
end;

procedure TSimpleGraphTest.FindSeparates;
var
  Ref: TRef;
  g: TGraph;
  Separates: TIntVectorArray;
begin
  g := {%H-}Ref;
  Separates := g.FindSeparates;
  AssertTrue(System.Length(Separates) = 0);
  Ref.Instance := GenerateTestGr2;
  g := Ref;
  Separates := g.FindSeparates;
  AssertTrue(System.Length(Separates) = 2);
  if Separates[0].Count < Separates[1].Count then
    begin
      AssertTrue(Separates[0].Count = 5);
      AssertTrue(Separates[1].Count = 11);
    end
  else
    begin
      AssertTrue(Separates[0].Count = 11);
      AssertTrue(Separates[1].Count = 5);
    end;
end;

procedure TSimpleGraphTest.IsTree;
var
  Ref: TRef;
  g, g2: TGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.IsTree);
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertFalse(g.IsTree);
  g2 := g.SubgraphFromTree(g.BfsTree(0));
  try
    AssertTrue(g2.IsTree);
  finally
    g2.Free;
  end;
end;

procedure TSimpleGraphTest.IsStar;
var
  Ref: TRef;
  g: TGraph;
  Hub: SizeInt;
begin
  g := {%H-}Ref;
  AssertFalse(g.IsStar(Hub));
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertFalse(g.IsStar(Hub));
  Ref.Instance := GenerateCycle;
  g := Ref;
  AssertFalse(g.IsStar(Hub));
  Ref.Instance := GenerateStar;
  g := Ref;
  AssertTrue(g.IsStar(Hub));
  AssertTrue(g[Hub] = 1);
end;

procedure TSimpleGraphTest.IsCycle;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.IsCycle);
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertFalse(g.IsCycle);
  Ref.Instance := GenerateStar;
  g := Ref;
  AssertFalse(g.IsCycle);
  Ref.Instance := GenerateCycle;
  g := Ref;
  AssertTrue(g.IsCycle);
end;

procedure TSimpleGraphTest.IsWheel;
var
  Ref: TRef;
  g: TGraph;
  Hub: SizeInt;
begin
  g := {%H-}Ref;
  AssertFalse(g.IsWheel(Hub));
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertFalse(g.IsWheel(Hub));
  Ref.Instance := GenerateStar;
  g := Ref;
  AssertFalse(g.IsWheel(Hub));
  Ref.Instance := GenerateWheel;
  g := Ref;
  AssertTrue(g.IsWheel(Hub));
  AssertTrue(g[Hub] = 1);
end;

procedure TSimpleGraphTest.IsComplete;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.IsComplete);
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertFalse(g.IsComplete);
  Ref.Instance := GenerateStar;
  g := Ref;
  AssertFalse(g.IsComplete);
  Ref.Instance := GenerateCycle;
  g := Ref;
  AssertFalse(g.IsComplete);
  Ref.Instance := GenerateComplete;
  g := Ref;
  AssertTrue(g.IsComplete);
end;

procedure TSimpleGraphTest.IsRegular;
var
  Ref: TRef;
  g: TGraph;
  Deg: SizeInt;
begin
  g := {%H-}Ref;
  AssertTrue(g.IsRegular(Deg));
  AssertTrue(Deg = NULL_INDEX);
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertFalse(g.IsRegular(Deg));
  Ref.Instance := GenerateWheel;
  g := Ref;
  AssertFalse(g.IsRegular(Deg));
  Ref.Instance := GenerateCycle;
  g := Ref;
  AssertTrue(g.IsRegular(Deg));
  AssertTrue(Deg = 2);
  Ref.Instance := GenerateComplete;
  g := Ref;
  AssertTrue(g.IsRegular(Deg));
  AssertTrue(Deg = Pred(g.VertexCount));
end;

procedure TSimpleGraphTest.ContainsCycle;
var
  Ref: TRef;
  g: TGraph;
  Cycle: TIntArray;
begin
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertTrue(g.ContainsCycle(1, Cycle));
  AssertTrue(Cycle[0] = Cycle[System.High(Cycle)]);
  Ref.Instance := GenerateStar;
  g := Ref;
  AssertFalse(g.ContainsCycle(1, Cycle));
  Ref.Instance := GenerateWheel;
  g := Ref;
  AssertTrue(g.ContainsCycle(1, Cycle));
  Ref.Instance := GenerateCycle;
  g := Ref;
  AssertTrue(g.ContainsCycle(1, Cycle));
  AssertTrue(System.Length(Cycle) = Succ(g.VertexCount));
end;

procedure TSimpleGraphTest.ContainsEulerianPath;
var
  Ref: TRef;
  g: TGraph;
  vOdd: SizeInt;
begin
  g := {%H-}Ref;
  AssertFalse(g.ContainsEulerianPath(vOdd));
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertFalse(g.ContainsEulerianPath(vOdd));
  Ref.Instance := GenerateTestGr3;
  g := Ref;
  AssertTrue(g.ContainsEulerianPath(vOdd));
  AssertTrue(vOdd = 2);
  Ref.Instance := GenerateCycle;
  g := Ref;
  AssertTrue(g.ContainsEulerianPath(vOdd));
  AssertTrue(vOdd = -1);
end;

procedure TSimpleGraphTest.ContainsEulerianCycle;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.ContainsEulerianCycle);
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertFalse(g.ContainsEulerianCycle);
  Ref.Instance := GenerateCycle;
  g := Ref;
  AssertTrue(g.ContainsEulerianCycle);
  Ref.Instance := GenerateTestGr3;
  g := Ref;
  AssertFalse(g.ContainsEulerianCycle);
  g.RemoveEdge(3, 10);
  AssertTrue(g.ContainsEulerianCycle);
end;

procedure TSimpleGraphTest.FindEulerianPath;
var
  Ref: TRef;
  g: TGraph;
  Path: TIntArray;
begin
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  Path := g.FindEulerianPath;
  AssertTrue(Path.IsEmpty);
  Ref.Instance := GenerateTestGr3;
  g := Ref;
  Path := g.FindEulerianPath;
  AssertTrue(Path.Length = Succ(g.EdgeCount));
end;

procedure TSimpleGraphTest.FindEulerianCycle;
var
  Ref: TRef;
  g: TGraph;
  Cycle: TIntArray;
begin
  {%H-}Ref.Instance := GenerateCycle;
  g := Ref;
  Cycle := g.FindEulerianCycle;
  AssertTrue(Cycle.Length = Succ(g.EdgeCount));
  Ref.Instance := GenerateTestGr3;
  g := Ref;
  Cycle := g.FindEulerianCycle;
  AssertTrue(Cycle.IsEmpty);
  g.RemoveEdge(3, 10);
  Cycle := g.FindEulerianCycle;
  AssertTrue(Cycle.Length = Succ(g.EdgeCount));
end;

procedure TSimpleGraphTest.ContainsCutVertex;
var
  Ref: TRef;
  g: TGraph;
begin
  {%H-}Ref.Instance := GenerateCycle;
  g := Ref;
  AssertFalse(g.ContainsCutVertex(1));
  g.RemoveEdge(1, 12);
  AssertTrue(g.ContainsCutVertex(1));
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertTrue(g.ContainsCutVertex(1));
end;

procedure TSimpleGraphTest.FindCutVertices;
var
  Ref: TRef;
  g: TGraph;
  Points: TIntArray;
begin
  {%H-}Ref.Instance := GenerateCycle;
  g := Ref;
  Points := g.FindCutVertices(1);
  AssertTrue(Points.IsEmpty);
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  Points := g.FindCutVertices(1);
  AssertTrue(Points.Length = 4);
  AssertTrue((TIntHelper.SequentSearch(Points, 0) <> NULL_INDEX) and
             (TIntHelper.SequentSearch(Points, 6) <> NULL_INDEX) and
             (TIntHelper.SequentSearch(Points, 7) <> NULL_INDEX) and
             (TIntHelper.SequentSearch(Points, 9) <> NULL_INDEX));
end;

procedure TSimpleGraphTest.RemoveCutVertices;
var
  Ref: TRef;
  g: TGraph;
  EdgeCount: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertTrue(g.ContainsCutVertex(1));
  EdgeCount := g.RemoveCutVertices(1);
  AssertFalse(g.ContainsCutVertex(1));
  AssertTrue(EdgeCount = 5);
end;

procedure TSimpleGraphTest.ContainsBridge;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.ContainsBridge);
  Ref.Instance := GenerateCycle;
  g := Ref;
  AssertFalse(g.ContainsBridge);
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertTrue(g.ContainsBridge);
end;

procedure TSimpleGraphTest.FindBridges;
var
  Ref: TRef;
  g: TGraph;
  Bridges: TIntEdgeArray;
  BridgeSet: TIntPairSet;
  e: TIntEdge;
begin
  g := {%H-}Ref;
  Bridges := g.FindBridges;
  AssertTrue(System.Length(Bridges) = 0);
  Ref.Instance := GenerateCycle;
  g := Ref;
  Bridges := g.FindBridges;
  AssertTrue(Bridges = nil);
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  Bridges := g.FindBridges;
  for e in Bridges do
    BridgeSet.Add(e.Source, e.Destination);
  AssertTrue(BridgeSet.Count = 4);
  AssertTrue(BridgeSet.Contains(0, 1) and BridgeSet.Contains(10, 9) and
             BridgeSet.Contains(7, 6) and BridgeSet.Contains(8, 7));
end;

procedure TSimpleGraphTest.IsBiconnected;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.IsBiconnected);
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertFalse(g.IsBiconnected);
  Ref.Instance := GenerateCycle;
  g := Ref;
  AssertTrue(g.IsBiconnected);
  Ref.Instance := GenerateWheel;
  g := Ref;
  AssertTrue(g.IsBiconnected);
end;

procedure TSimpleGraphTest.FindBicomponents;
var
  Ref: TRef;
  g: TGraph;
  Comps: TEdgeArrayVector;
begin
  {%H-}Ref.Instance := GenerateCycle;
  g := Ref;
  Comps := g.FindBicomponentsI(1);
  AssertTrue(Comps.Count = 1);
  Ref.Instance := GenerateWheel;
  g := Ref;
  Comps := g.FindBicomponentsI(1);
  AssertTrue(Comps.Count = 1);
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  Comps := g.FindBicomponentsI(1);
  AssertTrue(Comps.Count = 6);
  Ref.Instance := GenerateTestGr3;
  g := Ref;
  Comps := g.FindBicomponentsI(1);
  AssertTrue(Comps.Count = 3);
end;

procedure TSimpleGraphTest.EnsureBiconnected;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  FCallCount := 0;
  g.EnsureBiconnected(@EdgeAdding);
  AssertTrue(FCallCount = 0);
  AssertFalse(g.IsBiconnected);
  Ref.Instance := GenerateCycle;
  g := Ref;
  g.EnsureBiconnected(@EdgeAdding);
  AssertTrue(FCallCount = 0);
  Ref.Instance := GenerateWheel;
  g := Ref;
  g.EnsureBiconnected(@EdgeAdding);
  AssertTrue(FCallCount = 0);
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  g.EnsureBiconnected(@EdgeAdding);
  AssertTrue(g.IsBiconnected);
  AssertTrue(FCallCount = 5);
  Ref.Instance := GenerateTestGr3;
  g := Ref;
  FCallCount := 0;
  g.EnsureBiconnected(@EdgeAdding);
  AssertTrue(g.IsBiconnected);
  AssertTrue(FCallCount = 2);
end;

procedure TSimpleGraphTest.FindMerics;
var
  Ref: TRef;
  g: TGraph;
  r, d: SizeInt;
begin
  g := {%H-}Ref;
  AssertFalse(g.FindMerics(r, d));
  Ref.Instance := GenerateTestGr2;
  g := Ref;
  AssertFalse(g.FindMerics(r, d));
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertTrue(g.FindMerics(r, d));
  AssertTrue(r = 2);
  AssertTrue(d = 4);
  Ref.Instance := GenerateWheel;
  g := Ref;
  AssertTrue(g.FindMerics(r, d));
  AssertTrue(r = 1);
  AssertTrue(d = 2);
end;

procedure TSimpleGraphTest.FindCenter;
var
  Ref: TRef;
  g: TGraph;
  c: TIntArray;
begin
  g := {%H-}Ref;
  c := g.FindCenter;
  AssertTrue(c = nil);
  Ref.Instance := GenerateTestGr2;
  g := Ref;
  c := g.FindCenter;
  AssertTrue(c = nil);
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  c := g.FindCenter;
  AssertTrue(c.Length = 1);
  AssertTrue(c[0] = 6)
end;


initialization
  RegisterTest(TSimpleGraphTest);
end.

