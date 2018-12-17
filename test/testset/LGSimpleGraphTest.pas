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
    FSetVector: TIntArrayVector;
    FCallCount: Integer;
    function  GenerateTestGr1: TGraph;
    function  GenerateTestGr2: TGraph;
    function  GenerateTestGr3: TGraph;
    function  GenerateTestGr4: TGraph;
    function  GenerateTestGr5: TGraph;
    function  GenerateTestGr5Compl: TGraph;
    function  GenerateStar: TGraph;
    function  GenerateCycle: TGraph;
    function  GenerateWheel: TGraph;
    function  GenerateComplete: TGraph;
    function  GenerateTestGrBip1: TGraph;
    function  GenerateTestTriangles: TGraph;
    function  GenerateC125Clique: TGraph;
    function  GenerateC125Mis: TGraph;
    procedure EdgeAdding(constref {%H-}aSrc, {%H-}aDst: Integer; var{%H-}aData: TEmptyRec);
    procedure SetFound(constref aSet: TIntArray; var {%H-}aCancel: Boolean);
    procedure SetFound2(constref aSet: TIntArray; var aCancel: Boolean);
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
    procedure FindMetrics;
    procedure FindCenter;
    procedure FindPeripheral;
    procedure MinCut;
    procedure FindMaxBipMatchHK;
    procedure GetMaxBipMatchHK;
    procedure FindMaxBipMatchBfs;
    procedure GetMaxBipMatchBfs;
    procedure FindMaxMatchEd;
    procedure FindMaxMatchPC;

    procedure ListAllMIS1;
    procedure ListAllMIS2;
    procedure FindMIS;
    procedure ListAllCliques1;
    procedure ListAllCliques2;
    procedure FindMaxClique;
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

function TSimpleGraphTest.GenerateTestGr4: TGraph;
begin
  //see TestGr4.png
  Result := TGraph.Create;
  Result.AddVertexRange(0, 8);
  Result.AddEdges([0, 1, 0, 2, 1, 3, 2, 3, 0, 3, 1, 2, 2, 4, 2, 5, 3, 5, 4, 5,
                   4, 6, 5, 7, 5, 7, 4, 7, 5, 6, 6, 8, 7, 8, 0, 8]);
end;

function TSimpleGraphTest.GenerateTestGr5: TGraph;
begin
  //see TestGr5.png, mis count 295, independance number 8
  Result := TGraph.Create;
  Result.AddVertexRange(1, 20);
  Result.AddEdges([1, 2, 2, 3, 3, 4, 4, 5, 5, 1, 6, 1, 7, 2, 8, 3, 9, 4, 10, 5, 6, 12, 12, 7,
                   7, 13, 13, 8, 8, 14, 14, 9, 9, 15, 15, 10, 10, 11, 11, 6, 12, 17, 13, 18,
                   14, 19, 15, 20, 16, 11, 16, 17, 17, 18, 18, 19, 19, 20, 20, 16]);
end;

function TSimpleGraphTest.GenerateTestGr5Compl: TGraph;
var
  Ref: TRef;
  g: TGraph;
  I, J: Integer;
begin
  //complement of TestGr5
  {%H-}Ref.Instance := GenerateTestGr5;
  g := Ref;
  Result := TGraph.Create;
  Result.AddVertexRange(1, 20);
  for I := 1 to 20 do
    for J := 1 to 20 do
      if I <> J then
        if not g.Adjacent(I, J) then
          Result.AddEdge(I, J);
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

function TSimpleGraphTest.GenerateTestGrBip1: TGraph;
begin
  //see TestGrBip1.png
  Result := TGraph.Create;
  Result.AddVertexRange(1, 16);
  Result.AddEdges([1, 2, 1, 4, 1, 6, 3, 4, 3, 6, 3, 8, 5, 6, 5, 8, 5, 10, 7, 8, 7, 10, 7,
                   12, 9, 10, 9, 12, 9, 14, 11, 12, 11, 14, 11, 16, 13, 14, 13, 16, 15, 16]);
end;

function TSimpleGraphTest.GenerateTestTriangles: TGraph;
var
  I: Integer = 0;
begin
  //see TestTriangles.png
  Result := TGraph.Create;
  Result.AddVertexRange(0, 12);
  while I < Result.VertexCount - 3 do
    begin
      Result.AddEdgeI(I, I + 1);
      Result.AddEdgeI(I + 1, I + 2);
      Result.AddEdgeI(I, I + 2);
      Result.AddEdgeI(I + 2, I + 3);
      I += 3;
    end;
end;

function TSimpleGraphTest.GenerateC125Clique: TGraph;
begin
  //C125.9.clq instance form the Second DIMACS Implementation Challenge, w = 34
  Result := TGraph.Create;
  Result.AddVertexRange(1, 125);
  Result.AddEdges([{$I C125Clique.inc}]);
end;

function TSimpleGraphTest.GenerateC125Mis: TGraph;
var
  Ref: TRef;
  g: TGraph;
  I, J: Integer;
begin
  //complement of C125.9.clq
  {%H-}Ref.Instance := GenerateC125Clique;
  g := Ref;
  Result := TGraph.Create;
  Result.AddVertexRange(1, 125);
  for I := 1 to 125 do
    for J := 1 to 125 do
      if I <> J then
        if not g.Adjacent(I, J) then
          Result.AddEdge(I, J);
end;

procedure TSimpleGraphTest.EdgeAdding(constref aSrc, aDst: Integer; var aData: TEmptyRec);
begin
  Inc(FCallCount);
end;

procedure TSimpleGraphTest.SetFound(constref aSet: TIntArray; var aCancel: Boolean);
begin
  FSetVector.Add(aSet);
end;

procedure TSimpleGraphTest.SetFound2(constref aSet: TIntArray; var aCancel: Boolean);
begin
  FSetVector.Add(aSet);
  aCancel := True;
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
  g.RemoveEdge(1, 12);
  AssertFalse(g.IsComplete);
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
  Points := g.FindCutVertices(0);
  AssertTrue(Points.Length = 4);
  AssertTrue((TIntHelper.SequentSearch(Points, 0) <> NULL_INDEX) and
             (TIntHelper.SequentSearch(Points, 6) <> NULL_INDEX) and
             (TIntHelper.SequentSearch(Points, 7) <> NULL_INDEX) and
             (TIntHelper.SequentSearch(Points, 9) <> NULL_INDEX));
  g.AddEdge(1, 2);
  Points := g.FindCutVertices(1);
  AssertTrue(Points.Length = 3);
  AssertTrue((TIntHelper.SequentSearch(Points, 6) <> NULL_INDEX) and
             (TIntHelper.SequentSearch(Points, 7) <> NULL_INDEX) and
             (TIntHelper.SequentSearch(Points, 9) <> NULL_INDEX));
  g.AddEdge(7, 9);
  Points := g.FindCutVertices(6);
  AssertTrue(Points.Length = 2);
  AssertTrue((TIntHelper.SequentSearch(Points, 7) <> NULL_INDEX) and
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
  Ref.Instance := GenerateTestGr4;
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

procedure TSimpleGraphTest.FindMetrics;
var
  Ref: TRef;
  g: TGraph;
  r, d: SizeInt;
begin
  g := {%H-}Ref;
  AssertFalse(g.FindMetrics(r, d));
  Ref.Instance := GenerateTestGr2;
  g := Ref;
  AssertFalse(g.FindMetrics(r, d));
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertTrue(g.FindMetrics(r, d));
  AssertTrue(r = 2);
  AssertTrue(d = 4);
  Ref.Instance := GenerateWheel;
  g := Ref;
  AssertTrue(g.FindMetrics(r, d));
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
  AssertTrue(c[0] = 6);
end;

procedure TSimpleGraphTest.FindPeripheral;
var
  Ref: TRef;
  g: TGraph;
  p: TIntArray;
begin
  g := {%H-}Ref;
  p := g.FindPeripheral;
  AssertTrue(p = nil);
  Ref.Instance := GenerateTestGr2;
  g := Ref;
  p := g.FindPeripheral;
  AssertTrue(p = nil);
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  p := g.FindPeripheral;
  AssertTrue(p.Length = 8);
end;

procedure TSimpleGraphTest.MinCut;
var
  Ref: TRef;
  g: TGraph;
  Cut: TGraph.TCut;
  Cross: TIntEdgeArray;
  CutSize: SizeInt;
begin
  g := {%H-}Ref;
  CutSize := g.MinCut;
  AssertTrue(CutSize = 0);
  Ref.Instance := GenerateTestGr2;
  g := Ref;
  CutSize := g.MinCut;
  AssertTrue(CutSize = 0);
  Ref.Instance := GenerateTestGr4;
  g := Ref;
  AssertTrue(g.IsBiconnected);
  CutSize := g.MinCut;
  AssertTrue(CutSize = 3);
  g.RemoveEdge(5, 6);
  CutSize := g.MinCut(Cut, Cross);
  AssertTrue(CutSize = 2);
  if Cut.A.Length <= Cut.B.Length then
    begin
      AssertTrue(Cut.A.Length = 1);
      AssertTrue(Cut.A[0] = 6);
    end
  else
    begin
      AssertTrue(Cut.B.Length = 1);
      AssertTrue(Cut.B[0] = 6);
    end;
  AssertTrue(Length(Cross) = 2);
  if Cross[0].Source = 4 then
    begin
      AssertTrue(Cross[0].Destination.ToString, Cross[0].Destination = 6);
      AssertTrue(Cross[1].Source.ToString, Cross[1].Source = 6);
      AssertTrue(Cross[1].Destination = 8);
    end
  else
    begin
      AssertTrue(Cross[0].Destination = 8);
      AssertTrue(Cross[1].Source = 4);
      AssertTrue(Cross[1].Destination = 6);
    end;
end;

procedure TSimpleGraphTest.FindMaxBipMatchHK;
var
  Ref: TRef;
  g: TGraph;
  Match: TIntEdgeArray;
begin
  g := {%H-}Ref;
  AssertFalse(g.FindMaxBipMatchHK(Match));
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertFalse(g.FindMaxBipMatchHK(Match));
  Ref.Instance := GenerateTestGrBip1;
  g := Ref;
  AssertTrue(g.FindMaxBipMatchHK(Match));
  AssertTrue(Length(Match) = 8);
  AssertTrue(g.IsMaxMatching(Match));
end;

procedure TSimpleGraphTest.GetMaxBipMatchHK;
var
  Ref: TRef;
  g: TGraph;
  Match: TIntEdgeArray;
  Whites, Grays: TIntArray;
  Tested: Boolean = False;
begin
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  if g.IsBipartite(Whites, Grays) then
    Tested := True;
  AssertFalse(Tested);
  Ref.Instance := GenerateTestGrBip1;
  g := Ref;
  if g.IsBipartite(Whites, Grays) then
    begin
      Tested := True;
      Match := g.GetMaxBipMatchHK(Whites, Grays);
      AssertTrue(Length(Match) = 8);
      AssertTrue(g.IsMaxMatching(Match));
    end;
  AssertTrue(Tested);
end;

procedure TSimpleGraphTest.FindMaxBipMatchBfs;
var
  Ref: TRef;
  g: TGraph;
  Match: TIntEdgeArray;
begin
  g := {%H-}Ref;
  AssertFalse(g.FindMaxBipMatchBfs(Match));
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertFalse(g.FindMaxBipMatchBfs(Match));
  Ref.Instance := GenerateTestGrBip1;
  g := Ref;
  AssertTrue(g.FindMaxBipMatchBfs(Match));
  AssertTrue(Length(Match) = 8);
  AssertTrue(g.IsMaxMatching(Match));
end;

procedure TSimpleGraphTest.GetMaxBipMatchBfs;
var
  Ref: TRef;
  g: TGraph;
  Match: TIntEdgeArray;
  Whites, Grays: TIntArray;
  Tested: Boolean = False;
begin
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  if g.IsBipartite(Whites, Grays) then
    Tested := True;
  AssertFalse(Tested);
  Ref.Instance := GenerateTestGrBip1;
  g := Ref;
  if g.IsBipartite(Whites, Grays) then
    begin
      Tested := True;
      Match := g.GetMaxBipMatchBfs(Whites, Grays);
      AssertTrue(Length(Match) = 8);
      AssertTrue(g.IsMaxMatching(Match));
    end;
  AssertTrue(Tested);
end;

procedure TSimpleGraphTest.FindMaxMatchEd;
var
  Ref: TRef;
  g: TGraph;
  Match: TIntEdgeArray;
begin
  g := {%H-}Ref;
  Match := g.FindMaxMatchEd;
  AssertTrue(Match = nil);
  Ref.Instance := GenerateTestGrBip1;
  g := Ref;
  Match := g.FindMaxMatchEd;
  AssertTrue(Length(Match) = 8);
  AssertTrue(g.IsMaxMatching(Match));
  Ref.Instance := GenerateTestTriangles;
  g := Ref;
  Match := g.FindMaxMatchEd;
  AssertTrue(Length(Match) = 6);
  AssertTrue(g.IsMaxMatching(Match));
end;

procedure TSimpleGraphTest.FindMaxMatchPC;
var
  Ref: TRef;
  g: TGraph;
  Match: TIntEdgeArray;
begin
  g := {%H-}Ref;
  Match := g.FindMaxMatchPC;
  AssertTrue(Match = nil);
  Ref.Instance := GenerateTestGrBip1;
  g := Ref;
  Match := g.FindMaxMatchPC;
  AssertTrue(Length(Match) = 8);
  AssertTrue(g.IsMaxMatching(Match));
  Ref.Instance := GenerateTestTriangles;
  g := Ref;
  Match := g.FindMaxMatchPC;
  AssertTrue(Length(Match) = 6);
  AssertTrue(g.IsMaxMatching(Match));
end;

procedure TSimpleGraphTest.ListAllMIS1;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  FSetVector.Clear;
  g.ListAllMIS(@SetFound);
  AssertTrue(FSetVector.IsEmpty);
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  g.ListAllMIS(@SetFound2);
  AssertTrue(FSetVector.Count = 1);
  AssertTrue(g.IsMIS(FSetVector[0]));
end;

procedure TSimpleGraphTest.ListAllMIS2;
var
  Ref: TRef;
  g: TGraph;
  CurrSet: TIntArray;
begin
  {%H-}Ref.Instance := GenerateTestGr5;
  g := Ref;
  FSetVector.Clear;
  g.ListAllMIS(@SetFound);
  AssertTrue(FSetVector.Count = 295);
  for CurrSet in FSetVector do
    begin
      AssertTrue(not CurrSet.IsEmpty and (CurrSet.Length <= 8));
      AssertTrue(g.IsMIS(CurrSet));
    end;
end;

procedure TSimpleGraphTest.FindMIS;
var
  Ref: TRef;
  g: TGraph;
  Mis: TIntArray;
  Exact: Boolean;
begin
  {%H-}Ref.Instance := GenerateC125Mis;
  g := Ref;
  Mis := g.FindMIS(Exact, 10);
  AssertTrue(Exact);
  AssertTrue(Mis.Length = 34);
  AssertTrue(g.IsMIS(Mis));
end;

procedure TSimpleGraphTest.ListAllCliques1;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  FSetVector.Clear;
  g.ListAllCliques(@SetFound);
  AssertTrue(FSetVector.IsEmpty);
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  g.ListAllCliques(@SetFound2);
  AssertTrue(FSetVector.Count = 1);
  AssertTrue(g.IsMaxClique(FSetVector[0]));
end;

procedure TSimpleGraphTest.ListAllCliques2;
var
  Ref: TRef;
  g: TGraph;
  CurrSet: TIntArray;
begin
  {%H-}Ref.Instance := GenerateTestGr5Compl;
  g := Ref;
  FSetVector.Clear;
  g.ListAllCliques(@SetFound);
  AssertTrue(FSetVector.Count = 295);
  for CurrSet in FSetVector do
    begin
      AssertTrue(not CurrSet.IsEmpty and (CurrSet.Length <= 8));
      AssertTrue(g.IsMaxClique(CurrSet));
    end;
end;

procedure TSimpleGraphTest.FindMaxClique;
var
  Ref: TRef;
  g: TGraph;
  Clique: TIntArray;
  Exact: Boolean;
begin
  {%H-}Ref.Instance := GenerateC125Clique;
  g := Ref;
  Clique := g.FindMaxClique(Exact, 10);
  AssertTrue(Exact);
  AssertTrue(Clique.Length = 34);
  AssertTrue(g.IsMaxClique(Clique));
end;


initialization
  RegisterTest(TSimpleGraphTest);
end.

