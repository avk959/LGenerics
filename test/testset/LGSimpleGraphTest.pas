unit LGSimpleGraphTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, math,
  LGUtils,
  LGArrayHelpers,
  LGSparseGraph,
  LGSimpleGraph;


type

  { TSimpleGraphTest }

  TSimpleGraphTest = class(TTestCase)
  private
  type
    TGraph = TIntChart;
    TRef   = specialize TGAutoRef<TGraph>;

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
    function  GenerateCycle11: TGraph;
    function  GenerateWheel: TGraph;
    function  GenerateComplete: TGraph;
    function  GenerateTestGrBip1: TGraph;
    function  GenerateTestTriangles: TGraph;
    function  GenerateC125Clique: TGraph;
    function  GenerateC125Mis: TGraph;
    function  GenerateQueen6_6: TGraph;
    procedure EdgeAdding(constref {%H-}aSrc, {%H-}aDst: Integer; var{%H-}aData: TEmptyRec);
    procedure SetFound(const aSet: TIntArray; var {%H-}aCancel: Boolean);
    procedure SetFound2(const aSet: TIntArray; var aCancel: Boolean);
  published
    procedure AddVertices;
    procedure SaveToStream;
    procedure Clone;
    procedure Degree;
    procedure Connected;
    procedure RemoveVertex;
    procedure SeparateCount1;
    procedure SeparateCount2;
    procedure RemoveEdge;
    procedure RemoveEdge1;
    procedure ContractEdge;
    procedure ContractEdge1;
    procedure SeparateGraph1;
    procedure SeparateGraph2;
    procedure InducedSubgraph1;
    procedure InducedSubgraph2;
    procedure SubgraphFromTree;
    procedure SubgraphFromEdges1;
    procedure SubgraphFromEdges2;
    procedure CreateLineGraph;
    procedure DistinctEdges;
    procedure CreateComplementMatrix;
    procedure EnsureConnected;
    procedure EnsureConnected2;
    procedure PathExists;
    procedure SeparatePop;
    procedure GetSeparate;
    procedure FindSeparates;
    procedure CreateAdjacencyMatrix;
    procedure IsTree;
    procedure IsStar;
    procedure IsCycle;
    procedure IsWheel;
    procedure IsComplete;
    procedure IsRegular;
    procedure ContainsCycle;
    procedure IsAcyclic;
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
    procedure GreedyMIS;
    procedure ListAllCliques1;
    procedure ListAllCliques2;
    procedure FindMaxClique;
    procedure GreedyMaxClique;
    procedure VertexColoring;
    procedure IsKColorable;
    procedure CompleteColoring;
    procedure FindHamiltonCycles;
    procedure FindHamiltonPaths;
    procedure FindMDS;

    procedure SetSymmDifferenceOf;
    procedure SetSymmDifferenceOf2;
    procedure SetSymmDifferenceOf3;
    procedure SetSymmDifferenceOf4;
    procedure SetSymmDifferenceOf5;
    procedure SetUnionOf;
    procedure SetUnionOf2;
    procedure SetIntersectionOf;
    procedure SetIntersectionOf2;
    procedure SetIntersectionOf3;
  end;

  { TWeightedGraphTest }

  TWeightedGraphTest = class(TTestCase)
  private
  type
    TIntWeight   = specialize TGSimpleWeight<Integer>;
    TInt64Weight = specialize TGSimpleWeight<Int64>;
    TGraph       = specialize TGWeightedGraph<Integer, Integer, TIntWeight, Integer>;
    TGraph64     = specialize TGInt64Net<Integer, TInt64Weight, Integer>;
    TRef         = specialize TGAutoRef<TGraph>;
    TIntHelper   = specialize TGOrdinalArrayHelper<Integer>;
    THelper      = specialize TGOrdinalArrayHelper<SizeInt>;
  const
    WEIGHTS_ARRAY: array of TGraph.TWeightArray  = (
               (0, 1, 0, 5, 2, 0), (1, 0, 1, 6, 3, 1), (0, 1, 0, 5, 2, 0),
               (5, 6, 5, 0, 4, 5), (2, 3, 2, 4, 0, 2), (0, 1, 0, 5, 2, 0));
    PATHTREE_ARRAY: array of TIntArray = (
               (-1, 2, 0, 2, 5, 2), (2, -1, 1, 2, 5, 2), (2, 2, -1, 2, 5, 2),
               (2, 2, 3, -1, 3, 2), (2, 2, 5, 4, -1, 4), (2, 2, 5, 2, 5, -1));
    PATHS_FROM_0: array of TIntArray   = (
               (0, 2, 1), (0, 2), (0, 2, 3), (0, 2, 5, 4), (0, 2, 5));

    ECCENTR: TGraph.TWeightArray = (5, 6, 5, 6, 4, 5);

    function  GenerateTestWGr1: TGraph;
    function  GenerateTestWGr2: TGraph;
    function  GenerateRandomPoints(aCount: Integer): TPointsChart;
    function  GenerateTestWGrBip1: TGraph64;
    function  GenerateTestWGr3: TGraph64;
  published
    procedure SetEdgeData;
    procedure ContainsNegWeightEdge;
    procedure ContainsNegCycle;
    procedure ContainsNegCycle1;
    procedure MinPathsMap;
    procedure MinPathsMap1;
    procedure MinPathsMap2;
    procedure FindMinPathsMap;
    procedure FindMinPathsMap1;
    procedure FindMinPathsMap2;
    procedure FindMinPathsMap3;
    procedure FindMinPathsMap4;
    procedure MinPath;
    procedure MinPath1;
    procedure FindMinPath;
    procedure FindMinPath1;
    procedure FindMinPath2;
    procedure MinPathAStar;
    procedure FindAllPairMinPaths;
    procedure FindAllPairMinPaths1;
    procedure FindAllPairMinPaths2;
    procedure FindEccentricity;
    procedure FindEccentricity1;
    procedure FindWeightedMetrics;
    procedure FindWeightedMetrics1;
    procedure MinSpanningTreeKrus;
    procedure MinSpanningTreeKrus1;
    procedure MinSpanningTreePrim;
    procedure MinSpanningTreePrim1;
    procedure MinSpanningTree;
    procedure FindMinWeightBipMatch;
    procedure FindMinWeightBipMatch1;
    procedure FindMaxWeightBipMatch;
    procedure FindMaxWeightBipMatch1;
    procedure MinWeightCutSW;
    procedure MinWeightCutSW1;
    procedure MinWeightCutSW2;
    procedure MinWeightCutNI;
    procedure MinWeightCutNI1;
    procedure MinWeightCutNI2;
    procedure MinWeightCutNI3;
    procedure MinWeightCutNI4;
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
  //see TestGr5.png, mis count is 295, independance number is 8, domination number is 6
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

function TSimpleGraphTest.GenerateCycle11: TGraph;
begin
  Result := TGraph.Create;
  Result.AddVertexRange(2, 12);
  Result.AddEdges([2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 2, 12]);
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
  //C125.9.clq instance from the Second DIMACS Implementation Challenge, clique number is 34
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

function TSimpleGraphTest.GenerateQueen6_6: TGraph;
begin
  //queen6_6.col instance from the Second DIMACS Implementation Challenge, chromatic number is 7
  Result := TGraph.Create;
  Result.AddVertexRange(1, 36);
  Result.AddEdges([{$I queen6_6.inc}]);
end;

procedure TSimpleGraphTest.EdgeAdding(constref aSrc, aDst: Integer; var aData: TEmptyRec);
begin
  Inc(FCallCount);
end;

procedure TSimpleGraphTest.SetFound(const aSet: TIntArray; var aCancel: Boolean);
begin
  FSetVector.Add(aSet);
end;

procedure TSimpleGraphTest.SetFound2(const aSet: TIntArray; var aCancel: Boolean);
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
  g := {%H-}Ref;
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

procedure TSimpleGraphTest.RemoveEdge;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.RemoveEdge(0, 0));
  AssertFalse(g.RemoveEdge(0, 1));
end;

procedure TSimpleGraphTest.RemoveEdge1;
var
  Ref: TRef;
  g: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertTrue(g.Connected);
  AssertTrue(g.EdgeCount = 17);
  AssertFalse(g.RemoveEdge(0, 0));
  AssertTrue(g.Connected);
  AssertTrue(g.EdgeCount = 17);
  AssertFalse(g.RemoveEdge(0, 4));
  AssertTrue(g.Connected);
  AssertTrue(g.EdgeCount = 17);
  AssertTrue(g.ContainsEdge(0, 1));
  AssertTrue(g.RemoveEdge(0, 1));
  AssertFalse(g.ContainsEdge(0, 1));
  AssertTrue(g.EdgeCount = 16);
  AssertFalse(g.Connected);
  AssertTrue(g.SeparateCount = 2);
  AssertTrue(g.ContainsEdge(6, 7));
  AssertTrue(g.RemoveEdge(6, 7));
  AssertFalse(g.ContainsEdge(6, 7));
  AssertTrue(g.EdgeCount = 15);
  AssertTrue(g.SeparateCount = 3);
end;

procedure TSimpleGraphTest.ContractEdge;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.ContractEdge(0, 0));
  AssertFalse(g.ContractEdge(0, 1));
end;

procedure TSimpleGraphTest.ContractEdge1;
var
  Ref: TRef;
  g: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertTrue(g.Connected);
  AssertTrue(g.EdgeCount = 17);
  AssertTrue(g.VertexCount = 13);
  AssertFalse(g.ContractEdge(0, 0));
  AssertTrue(g.Connected);
  AssertTrue(g.EdgeCount = 17);
  AssertTrue(g.VertexCount = 13);
  AssertFalse(g.ContractEdge(0, 4));
  AssertTrue(g.Connected);
  AssertTrue(g.EdgeCount = 17);
  AssertTrue(g.VertexCount = 13);
  AssertTrue(g.ContainsEdge(0, 1));
  AssertTrue(g.ContractEdge(0, 1));
  AssertFalse(g.ContainsEdge(0, 1));
  AssertTrue(g.ContainsVertex(0));
  AssertFalse(g.ContainsVertex(1));
  AssertTrue(g.EdgeCount = 16);
  AssertTrue(g.Connected);
  AssertTrue(g.ContainsEdge(6, 7));
  AssertTrue(g.ContractEdge(6, 7));
  AssertFalse(g.ContainsEdge(6, 7));
  AssertTrue(g.ContainsVertex(6));
  AssertFalse(g.ContainsVertex(7));
  AssertTrue(g.EdgeCount = 15);
  AssertTrue(g.Connected);
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

procedure TSimpleGraphTest.InducedSubgraph1;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestGr2;
  g := Ref;
  AssertTrue(g.VertexCount = 16);
  {%H-}Ref2.Instance := g.InducedSubgraph([]);
  g2 := Ref2;
  AssertTrue(g2.IsEmpty);
end;

procedure TSimpleGraphTest.InducedSubgraph2;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestGr2;
  g := Ref;
  AssertTrue(g.VertexCount = 16);
  {%H-}Ref2.Instance := g.InducedSubgraph(
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
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertTrue(g.VertexCount = 13);
  Tree := g.DfsTree;
  {%H-}Ref2.Instance := g.SubgraphFromTree(Tree);
  g2 := Ref2;
  AssertTrue(g2.VertexCount = 13);
  AssertTrue(g2.SeparateCount = 1);
  AssertTrue(g2.IsTree);
  Ref.Instance := GenerateTestGr2;
  g := Ref;
  AssertTrue(g.VertexCount = 16);
  Tree := g.DfsTree;
  Ref2.Instance := g.SubgraphFromTree(Tree);
  g2 := Ref2;
  AssertTrue(g2.VertexCount = 16);
  AssertTrue(g2.SeparateCount = 2);
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

procedure TSimpleGraphTest.CreateComplementMatrix;
var
  Ref: TRef;
  g: TGraph;
  m: TGraph.TAdjacencyMatrix;
  I, J: SizeInt;
begin
  g := {%H-}Ref;
  m := g.CreateComplementMatrix;
  AssertTrue(m.IsEmpty);
  Ref.Instance := GenerateTestGr5;
  g := Ref;
  AssertTrue(g.VertexCount = 20);
  m := g.CreateComplementMatrix;
  for I := 0 to Pred(g.VertexCount) do
    for J := 0 to Pred(g.VertexCount) do
      if I <> J then
        AssertTrue(g.AdjacentI(I, J) xor m[I, J]);
  Ref.Instance := GenerateTestGr5Compl;
  g := Ref;
  for I := 0 to Pred(g.VertexCount) do
    for J := 0 to Pred(g.VertexCount) do
      if I <> J then
        AssertFalse(g.AdjacentI(I, J) xor m[I, J]);
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

procedure TSimpleGraphTest.CreateAdjacencyMatrix;
var
  Ref: TRef;
  g: TGraph;
  m: TGraph.TAdjacencyMatrix;
  I, J: SizeInt;
begin
  g := {%H-}Ref;
  m := g.CreateAdjacencyMatrix;
  AssertTrue(m.IsEmpty);
  Ref.Instance := GenerateTestGr5;
  g := Ref;
  m := g.CreateAdjacencyMatrix;
  for I := 0 to Pred(g.VertexCount) do
    for J := 0 to Pred(g.VertexCount) do
      if I <> J then
        AssertFalse(g.AdjacentI(I, J) xor m[I, J]);
  Ref.Instance := GenerateC125Clique;
  g := Ref;
  m := g.CreateAdjacencyMatrix;
  for I := 0 to Pred(g.VertexCount) do
    for J := 0 to Pred(g.VertexCount) do
      if I <> J then
        AssertFalse(g.AdjacentI(I, J) xor m[I, J]);
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
  g2 := g.SubgraphFromTree(g.BfsTree);
  try
    AssertTrue(g2.IsTree);
  finally
    g2.Free;
  end;
  Ref.Instance := GenerateWheel;
  g := Ref;
  AssertFalse(g.IsTree);
  g2 := g.SubgraphFromTree(g.DfsTree);
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

procedure TSimpleGraphTest.IsAcyclic;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertTrue(g.IsAcyclic);
  g.AddVertex(1);
  AssertTrue(g.IsAcyclic);
  g.AddVertexRange(2, 4);
  g.AddEdges([1, 2, 2, 3, 3, 4]);
  AssertTrue(g.IsAcyclic);
  g.AddEdge(1, 4);
  AssertFalse(g.IsAcyclic);
  Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertFalse(g.IsAcyclic);
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
      AssertTrue(Cross[0].Destination = 6);
      AssertTrue(Cross[1].Source = 6);
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
  g := {%H-}Ref;
  Mis := g.FindMIS(Exact, 10);
  AssertTrue(Exact);
  AssertTrue(Mis.IsEmpty);
  Ref.Instance := GenerateComplete;
  g := Ref;
  Mis := g.FindMIS(Exact, 10);
  AssertTrue(Exact);
  AssertTrue(Mis.Length = 1);
  Ref.Instance := GenerateTestGr5;
  g := Ref;
  Mis := g.FindMIS(Exact, 10);
  AssertTrue(Exact);
  AssertTrue(Mis.Length = 8);
  AssertTrue(g.IsMIS(Mis));
  Ref.Instance := GenerateC125Mis;
  g := Ref;
  Mis := g.FindMIS(Exact, 10);
  AssertTrue(Exact);
  AssertTrue(Mis.Length = 34);
  AssertTrue(g.IsMIS(Mis));
end;

procedure TSimpleGraphTest.GreedyMIS;
var
  Ref: TRef;
  g: TGraph;
  Mis: TIntArray;
begin
  g := {%H-}Ref;
  Mis := g.GreedyMIS;
  AssertTrue(Mis.IsEmpty);
  Ref.Instance := GenerateComplete;
  g := Ref;
  Mis := g.GreedyMIS;
  AssertTrue(Mis.Length > 0);
  Ref.Instance := GenerateTestGr5;
  g := Ref;
  Mis := g.GreedyMIS;
  AssertTrue(Mis.Length > 0);
  AssertTrue(g.IsMIS(Mis));
  Ref.Instance := GenerateC125Mis;
  g := Ref;
  Mis := g.GreedyMIS;
  AssertTrue(Mis.Length > 0);
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
  g := {%H-}Ref;
  Clique := g.FindMaxClique(Exact, 10);
  AssertTrue(Exact);
  AssertTrue(Clique.IsEmpty);
  Ref.Instance := GenerateComplete;
  g := Ref;
  Clique := g.FindMaxClique(Exact, 10);
  AssertTrue(Exact);
  AssertTrue(Clique.Length = g.VertexCount);
  Ref.Instance := GenerateTestGr5Compl;
  g := Ref;
  Clique := g.FindMaxClique(Exact, 10);
  AssertTrue(Exact);
  AssertTrue(Clique.Length = 8);
  AssertTrue(g.IsMaxClique(Clique));
  Ref.Instance := GenerateC125Clique;
  g := Ref;
  Clique := g.FindMaxClique(Exact, 10);
  AssertTrue(Exact);
  AssertTrue(Clique.Length = 34);
  AssertTrue(g.IsMaxClique(Clique));
end;

procedure TSimpleGraphTest.GreedyMaxClique;
var
  Ref: TRef;
  g: TGraph;
  Clique: TIntArray;
begin
  g := {%H-}Ref;
  Clique := g.GreedyMaxClique;
  AssertTrue(Clique.IsEmpty);
  Ref.Instance := GenerateComplete;
  g := Ref;
  Clique := g.GreedyMaxClique;
  AssertTrue(Clique.Length > 0);
  Ref.Instance := GenerateTestGr5Compl;
  g := Ref;
  Clique := g.GreedyMaxClique;
  AssertTrue(Clique.Length > 0);
  AssertTrue(g.IsMaxClique(Clique));
  Ref.Instance := GenerateC125Clique;
  g := Ref;
  Clique := g.GreedyMaxClique;
  AssertTrue(Clique.Length > 0);
  AssertTrue(g.IsMaxClique(Clique));
end;

procedure TSimpleGraphTest.VertexColoring;
var
  Ref: TRef;
  g: TGraph;
  Colors: TIntArray;
  ColCount: SizeInt;
  Exact: Boolean;
begin
  g := {%H-}Ref;
  ColCount := g.VertexColoring(Colors, Exact, 5);
  AssertTrue(Exact);
  AssertTrue(ColCount = 0);
  Ref.Instance := GenerateComplete;
  g := Ref;
  ColCount := g.VertexColoring(Colors, Exact, 5);
  AssertTrue(Exact);
  AssertTrue(ColCount = g.VertexCount);
  AssertTrue(g.IsProperVertexColoring(Colors));
  Ref.Instance := GenerateTestGrBip1;
  g := Ref;
  ColCount := g.VertexColoring(Colors, Exact, 5);
  AssertTrue(Exact);
  AssertTrue(ColCount = 2);
  AssertTrue(g.IsProperVertexColoring(Colors));
  Ref.Instance := GenerateQueen6_6;
  g := Ref;
  ColCount := g.GreedyVertexColoringRlf(Colors);
  AssertTrue(ColCount > 7);
  ColCount := g.VertexColoring(Colors, Exact, 5);
  AssertTrue(Exact);
  AssertTrue(ColCount = 7);
  AssertTrue(g.IsProperVertexColoring(Colors));
end;

procedure TSimpleGraphTest.IsKColorable;
var
  Ref: TRef;
  g: TGraph;
  Colors: TIntArray;
  Colorable: TTriLean;
begin
  g := {%H-}Ref;
  Colorable := g.IsKColorable(3, Colors, 5);
  AssertTrue(Colorable = tlFalse);
  Ref.Instance := GenerateTestGrBip1;
  g := Ref;
  Colorable := g.IsKColorable(1, Colors, 5);
  AssertTrue(Colorable = tlFalse);
  Colorable := g.IsKColorable(2, Colors, 5);
  AssertTrue(Colorable = tlTrue);
  AssertTrue(g.IsProperVertexColoring(Colors));
  Ref.Instance := GenerateComplete;
  g := Ref;
  Colorable := g.IsKColorable(Pred(g.VertexCount), Colors, 5);
  AssertTrue(Colorable = tlFalse);
  Colorable := g.IsKColorable(g.VertexCount, Colors, 5);
  AssertTrue(Colorable = tlTrue);
  AssertTrue(g.IsProperVertexColoring(Colors));
  Ref.Instance := GenerateQueen6_6;
  g := Ref;
  Colorable := g.IsKColorable(6, Colors, 5);
  AssertTrue(Colorable = tlFalse);
  Colorable := g.IsKColorable(7, Colors, 5);
  AssertTrue(Colorable = tlTrue);
  AssertTrue(g.IsProperVertexColoring(Colors));
end;

procedure TSimpleGraphTest.CompleteColoring;
var
  Ref: TRef;
  g: TGraph;
  Colors: TIntArray;
  I: SizeInt;
  Done: Boolean;
begin
  {%H-}Ref.Instance := GenerateComplete;
  g := Ref;
  Colors.Length := g.VertexCount;
  for I := 0 to Pred(g.VertexCount) do
    Colors[I] := g.VertexCount - I;
  Done := g.CompleteColoring(g.VertexCount, Colors, 5);
  AssertTrue(Done);
  for I := 0 to Pred(g.VertexCount) do
    AssertTrue(Colors[I] = g.VertexCount - I);
  AssertTrue(g.IsProperVertexColoring(Colors));
  Ref.Instance := GenerateQueen6_6;
  g := Ref;
  Colors.Length := g.VertexCount;
  FillChar(Pointer(Colors)^, Colors.Length * SizeOf(SizeInt), 0);
  Colors[g.IndexOf(1)] := 7;
  Colors[g.IndexOf(2)] := 3;
  Done := g.CompleteColoring(7, Colors, 5);
  AssertTrue(Done);
  AssertTrue(Colors[g.IndexOf(1)] = 7);
  AssertTrue(Colors[g.IndexOf(2)] = 3);
  AssertTrue(g.IsProperVertexColoring(Colors));
end;

procedure TSimpleGraphTest.FindHamiltonCycles;
var
  Ref: TRef;
  g: TGraph;
  Cycles: TIntArrayVector;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateStar;
  g := Ref;
  AssertFalse(g.FindHamiltonCycles(1, 0, Cycles, 5));
  Ref.Instance := GenerateCycle;
  g := Ref;
  AssertTrue(g.FindHamiltonCycles(1, 0, Cycles, 5));
  AssertTrue(Cycles.Count = 2);
  AssertTrue(g.IsHamiltonCycle(Cycles[0], g.IndexOf(1)));
  AssertTrue(g.IsHamiltonCycle(Cycles[1], g.IndexOf(1)));
  AssertTrue(g.FindHamiltonCycles(1, 1, Cycles, 5));
  AssertTrue(Cycles.Count = 1);
  AssertTrue(g.IsHamiltonCycle(Cycles[0], g.IndexOf(1)));
  g.RemoveEdge(1, 2);
  AssertFalse(g.FindHamiltonCycles(1, 0, Cycles, 5));
  {%H-}Ref.Instance := GenerateWheel;
  g := Ref;
  AssertTrue(g.FindHamiltonCycles(1, 0, Cycles, 5));
  AssertTrue(Cycles.Count = 22);
  for I := 0 to Pred(Cycles.Count) do
    AssertTrue(g.IsHamiltonCycle(Cycles[I], g.IndexOf(1)));
  AssertTrue(g.FindHamiltonCycles(1, 1, Cycles, 5));
  AssertTrue(Cycles.Count = 1);
end;

procedure TSimpleGraphTest.FindHamiltonPaths;
var
  Ref: TRef;
  g: TGraph;
  Paths: TIntArrayVector;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateStar;
  g := Ref;
  AssertFalse(g.FindHamiltonPaths(1, 0, Paths, 5));
  Ref.Instance := GenerateCycle;
  g := Ref;
  g.FindHamiltonPaths(1, 0, Paths, 5);
  AssertTrue(g.FindHamiltonPaths(1, 0, Paths, 5));
  AssertTrue(Paths.Count = 2);
  AssertTrue(g.IsHamiltonPath(Paths[0], g.IndexOf(1)));
  AssertTrue(g.IsHamiltonPath(Paths[1], g.IndexOf(1)));
  g.RemoveEdge(1, 2);
  AssertTrue(g.FindHamiltonPaths(1, 0, Paths, 5));
  AssertTrue(Paths.Count = 1);
  {%H-}Ref.Instance := GenerateWheel;
  g := Ref;
  AssertTrue(g.FindHamiltonPaths(1, 0, Paths, 5));
  AssertTrue(Paths.Count = 22);
  for I := 0 to Pred(Paths.Count) do
    AssertTrue(g.IsHamiltonPath(Paths[I], g.IndexOf(1)));
  AssertTrue(g.FindHamiltonPaths(1, 1, Paths, 5));
  AssertTrue(Paths.Count = 1);
  AssertTrue(g.IsHamiltonPath(Paths[0], g.IndexOf(1)));
  g.RemoveEdge(2, 3);
  AssertTrue(g.FindHamiltonPaths(1, 0, Paths, 5));
  AssertTrue(Paths.Count = 2);
  AssertTrue(g.IsHamiltonPath(Paths[0], g.IndexOf(1)));
  AssertTrue(g.IsHamiltonPath(Paths[1], g.IndexOf(1)));
end;

procedure TSimpleGraphTest.FindMDS;
var
  Ref: TRef;
  g: TGraph;
  Mds: TIntArray;
  Exact: Boolean;
begin
  g := {%H-}Ref;
  Mds := g.FindMDS(Exact);
  AssertTrue(Mds.IsEmpty);
  g.AddVertexRange(0, 2);
  Mds := g.FindMDS(Exact);
  AssertTrue(Mds.Length = 3);
  AssertTrue(g.IsMDS(Mds));
  Ref.Instance := GenerateTestGr5;
  g := Ref;
  Mds := g.FindMDS(Exact, 5);
  AssertTrue(Mds.Length = 6);
  AssertTrue(g.IsMDS(Mds));
end;

procedure TSimpleGraphTest.SetSymmDifferenceOf;
var
  Ref: TRef;
  g, g2: TGraph;
  Hub: SizeInt;
begin
  g := {%H-}Ref;
  g2 := TGraph.Create;
  try
    g.SetSymmDifferenceOf(g2);
  finally
    g2.Free;
  end;
  AssertTrue(g.IsEmpty);
  g2 := GenerateStar;
  try
    g.SetSymmDifferenceOf(g2);
  finally
    g2.Free;
  end;
  AssertTrue(g.VertexCount = 12);
  AssertTrue(g.IsStar(Hub));
  AssertTrue(Hub = 0);
end;

procedure TSimpleGraphTest.SetSymmDifferenceOf2;
var
  Ref: TRef;
  g, g2: TGraph;
  Hub: SizeInt;
begin
  {%H-}Ref.Instance := GenerateStar;
  g := Ref;
  g2 := GenerateCycle11;
  try
    g.SetSymmDifferenceOf(g2);
  finally
    g2.Free;
  end;
  AssertTrue(g.VertexCount = 12);
  AssertTrue(g.IsWheel(Hub));
  AssertTrue(Hub = 0);
end;

procedure TSimpleGraphTest.SetSymmDifferenceOf3;
var
  Ref: TRef;
  g, g2: TGraph;
  Hub: SizeInt;
begin
  {%H-}Ref.Instance := GenerateWheel;
  g := Ref;
  g2 := GenerateCycle11;
  try
    g.SetSymmDifferenceOf(g2);
  finally
    g2.Free;
  end;
  AssertTrue(g.VertexCount = 12);
  AssertTrue(g.IsStar(Hub));
  AssertTrue(Hub = 0);
end;

procedure TSimpleGraphTest.SetSymmDifferenceOf4;
var
  Ref: TRef;
  g, g2: TGraph;
begin
  {%H-}Ref.Instance := GenerateWheel;
  g := Ref;
  g2 := GenerateWheel;
  try
    g.SetSymmDifferenceOf(g2);
  finally
    g2.Free;
  end;
  AssertTrue(g.IsEmpty);
end;

procedure TSimpleGraphTest.SetSymmDifferenceOf5;
var
  Ref: TRef;
  g: TGraph;
begin
  {%H-}Ref.Instance := GenerateWheel;
  g := Ref;
  g.SetSymmDifferenceOf(g);
  AssertTrue(g.IsEmpty);
end;

procedure TSimpleGraphTest.SetUnionOf;
var
  Ref: TRef;
  g, g2: TGraph;
  Hub: SizeInt;
begin
  g := {%H-}Ref;
  g2 := TGraph.Create;
  try
    g.SetUnionOf(g2);
  finally
    g2.Free;
  end;
  AssertTrue(g.IsEmpty);
  g2 := GenerateStar;
  try
    g.SetUnionOf(g2);
  finally
    g2.Free;
  end;
  AssertTrue(g.VertexCount = 12);
  AssertTrue(g.IsStar(Hub));
  AssertTrue(Hub = 0);
end;

procedure TSimpleGraphTest.SetUnionOf2;
var
  Ref: TRef;
  g, g2: TGraph;
  Hub: SizeInt;
begin
  {%H-}Ref.Instance := GenerateStar;
  g := Ref;
  g2 := GenerateCycle11;
  try
    g.SetUnionOf(g2);
  finally
    g2.Free;
  end;
  AssertTrue(g.VertexCount = 12);
  AssertTrue(g.IsWheel(Hub));
  AssertTrue(Hub = 0);
end;

procedure TSimpleGraphTest.SetIntersectionOf;
var
  Ref: TRef;
  g, g2: TGraph;
begin
  g := {%H-}Ref;
  g2 := TGraph.Create;
  try
    g.SetIntersectionOf(g2);
  finally
    g2.Free;
  end;
  AssertTrue(g.IsEmpty);
  g2 := GenerateStar;
  try
    g.SetIntersectionOf(g2);
  finally
    g2.Free;
  end;
  AssertTrue(g.IsEmpty);
end;

procedure TSimpleGraphTest.SetIntersectionOf2;
var
  Ref: TRef;
  g, g2: TGraph;
begin
  {%H-}Ref.Instance := GenerateStar;
  g := Ref;
  g2 := GenerateCycle11;
  try
    g.SetIntersectionOf(g2);
  finally
    g2.Free;
  end;
  AssertTrue(g.VertexCount = 11);
  AssertTrue(g.SeparateCount = 11);
end;

procedure TSimpleGraphTest.SetIntersectionOf3;
var
  Ref: TRef;
  g, g2: TGraph;
begin
  {%H-}Ref.Instance := GenerateWheel;
  g := Ref;
  g2 := GenerateCycle;
  try
    g.SetIntersectionOf(g2);
  finally
    g2.Free;
  end;
  AssertTrue(g.VertexCount = 12);
  AssertTrue(g.IsCycle);
end;

{ TWeightedGraphTest }

function TWeightedGraphTest.GenerateTestWGr1: TGraph;
var
  I: Integer;
begin
  Result := TGraph.Create;  //TestWGr1.png
  for I := 0 to 5 do
    Result.AddVertex(I);
  Result.AddEdge(0, 1, TIntWeight.Create(2));
  Result.AddEdge(1, 2, TIntWeight.Create(2));
  Result.AddEdge(2, 3, TIntWeight.Create(5));
  Result.AddEdge(4, 3, TIntWeight.Create(4));
  Result.AddEdge(5, 4, TIntWeight.Create(3));
  Result.AddEdge(5, 0, TIntWeight.Create(5));
  Result.AddEdge(5, 1, TIntWeight.Create(3));
  Result.AddEdge(4, 2, TIntWeight.Create(3));
  Result.AddEdge(2, 5, TIntWeight.Create(-6));
end;

function TWeightedGraphTest.GenerateTestWGr2: TGraph;
var
  I: Integer;
begin
  Result := TGraph.Create; //TestWGr2.png
  for I := 0 to 5 do
    Result.AddVertex(I);
  Result.AddEdge(0, 1, TIntWeight.Create(2));
  Result.AddEdge(1, 2, TIntWeight.Create(1));
  Result.AddEdge(2, 3, TIntWeight.Create(5));
  Result.AddEdge(4, 3, TIntWeight.Create(4));
  Result.AddEdge(5, 4, TIntWeight.Create(2));
  Result.AddEdge(5, 0, TIntWeight.Create(5));
  Result.AddEdge(5, 1, TIntWeight.Create(3));
  Result.AddEdge(4, 2, TIntWeight.Create(3));
  Result.AddEdge(2, 5, TIntWeight.Create(0));
  Result.AddEdge(2, 0, TIntWeight.Create(0));
end;

function TWeightedGraphTest.GenerateRandomPoints(aCount: Integer): TPointsChart;
var
  I, J: Integer;
begin
  Result := TPointsChart.Create;
  while Result.VertexCount < aCount do
    begin
      I := Random(High(Integer));
      repeat
        J := Random(High(Integer))
      until I <> J;
      Result.AddVertex(TPoint.Create(I, J));
    end;
  while True do
    begin
      I := Random(aCount);
      repeat
        J := Random(aCount)
      until I <> J;
      Result.AddEdgeI(I, J);
      if Result.EdgeCount >= aCount * 6 then
        break;
    end;
  Result.EnsureConnected;
end;

function TWeightedGraphTest.GenerateTestWGrBip1: TGraph64;
begin
  Result := TGraph64.Create;  // min match weight = 58, max match weight = 270
  Result.AddEdge(1, 2, TInt64Weight.Create(92));
  Result.AddEdge(1, 4, TInt64Weight.Create(17));
  Result.AddEdge(1, 6, TInt64Weight.Create(8));
  Result.AddEdge(1, 8, TInt64Weight.Create(60));
  Result.AddEdge(3, 2, TInt64Weight.Create(10));
  Result.AddEdge(3, 4, TInt64Weight.Create(24));
  Result.AddEdge(3, 6, TInt64Weight.Create(28));
  Result.AddEdge(3, 8, TInt64Weight.Create(3));
  Result.AddEdge(3, 10, TInt64Weight.Create(13));
  Result.AddEdge(5, 4, TInt64Weight.Create(39));
  Result.AddEdge(5, 6, TInt64Weight.Create(52));
  Result.AddEdge(5, 8, TInt64Weight.Create(68));
  Result.AddEdge(5, 10, TInt64Weight.Create(4));
  Result.AddEdge(5, 12, TInt64Weight.Create(79));
  Result.AddEdge(7, 6, TInt64Weight.Create(8));
  Result.AddEdge(7, 8, TInt64Weight.Create(20));
  Result.AddEdge(7, 10, TInt64Weight.Create(72));
  Result.AddEdge(7, 12, TInt64Weight.Create(27));
  Result.AddEdge(9, 8, TInt64Weight.Create(24));
  Result.AddEdge(9, 10, TInt64Weight.Create(6));
  Result.AddEdge(9, 12, TInt64Weight.Create(14));
  Result.AddEdge(11, 10, TInt64Weight.Create(43));
  Result.AddEdge(11, 8, TInt64Weight.Create(5));
end;

function TWeightedGraphTest.GenerateTestWGr3: TGraph64;
var
  I: Integer;
begin
  Result := TGraph64.Create;//TestWGr3.png
  for I := 0 to 7 do
    Result.AddVertex(I);
  Result.AddEdgeI(0, 1, TInt64Weight.Create(100));
  Result.AddEdgeI(0, 2, TInt64Weight.Create(60));
  Result.AddEdgeI(1, 3, TInt64Weight.Create(60));
  Result.AddEdgeI(2, 3, TInt64Weight.Create(100));
  Result.AddEdgeI(2, 4, TInt64Weight.Create(50));
  Result.AddEdgeI(3, 5, TInt64Weight.Create(60));
  Result.AddEdgeI(4, 5, TInt64Weight.Create(100));
  Result.AddEdgeI(4, 6, TInt64Weight.Create(60));
  Result.AddEdgeI(5, 7, TInt64Weight.Create(60));
  Result.AddEdgeI(6, 7, TInt64Weight.Create(100));
end;

procedure TWeightedGraphTest.SetEdgeData;
var
  Ref: TRef;
  g: TGraph;
  e: TGraph.TEdge;
begin
  {%H-}Ref.Instance := GenerateTestWGr1;
  g := Ref;
  AssertTrue(g.EdgeCount = 9);
  for e in g.DistinctEdges do
    AssertTrue(g.SetEdgeData(e.Source, e.Destination, TIntWeight.Create(5)));
  for e in g.Edges do
    AssertTrue(e.Data.Weight = 5);
end;

procedure TWeightedGraphTest.ContainsNegWeightEdge;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.ContainsNegWeightEdge);
  Ref.Instance := GenerateTestWGr1;
  g := Ref;
  AssertTrue(g.ContainsNegWeightEdge);
  g.SetEdgeData(2, 5, TIntWeight.Create(0));
  AssertFalse(g.ContainsNegWeightEdge);
end;

procedure TWeightedGraphTest.ContainsNegCycle;
var
  Ref: TRef;
  g: TGraph;
  Cycle: TIntArray;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  try
   g.ContainsNegCycle(0, Cycle);
  except
    Raised := True;
  end;
  AssertTrue(Raised);
  g.AddVertex(1);
  AssertFalse(g.ContainsNegCycle(1, Cycle));
  g.AddVertex(2);
  AssertFalse(g.ContainsNegCycle(2, Cycle));
end;

procedure TWeightedGraphTest.ContainsNegCycle1;
var
  Ref: TRef;
  g: TGraph;
  Cycle: TIntArray;
begin
  {%H-}Ref.Instance := GenerateTestWGr1;
  g := Ref;
  AssertTrue(g.ContainsNegCycle(0, Cycle));
  AssertTrue(Cycle.Length = 3);
  g.SetEdgeData(2, 5, TIntWeight.Create(0));
  AssertFalse(g.ContainsNegCycle(0, Cycle));
  AssertTrue(Cycle.IsEmpty);
end;

procedure TWeightedGraphTest.MinPathsMap;
var
  Ref: TRef;
  g: TGraph;
  Weights: TGraph.TWeightArray;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  try
    g.MinPathsMap(0);
  except
    Raised := True;
  end;
  AssertTrue(Raised);
  g.AddVertex(1);
  Weights := g.MinPathsMap(1);
  AssertTrue(Length(Weights) = 1);
  AssertTrue(Weights[0] = 0);
end;

procedure TWeightedGraphTest.MinPathsMap1;
var
  Ref: TRef;
  g: TGraph;
  Weights: TGraph.TWeightArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestWGr2;
  g := Ref;
  for I := 0 to Pred(g.VertexCount) do
    begin
      Weights := g.MinPathsMap(I);
      AssertTrue(TIntHelper.Same(Weights, WEIGHTS_ARRAY[I]))
    end;
end;

procedure TWeightedGraphTest.MinPathsMap2;
var
  Ref: TRef;
  g: TGraph;
  PathTree: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestWGr2;
  g := Ref;
  for I := 0 to Pred(g.VertexCount) do
    begin
      g.MinPathsMap(I, PathTree);
      AssertTrue(THelper.Same(PathTree, PATHTREE_ARRAY[I]))
    end;
end;

procedure TWeightedGraphTest.FindMinPathsMap;
var
  Ref: TRef;
  g: TGraph;
  Weights: TGraph.TWeightArray;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  try
    g.FindMinPathsMap(0, Weights);
  except
    Raised := True;
  end;
  AssertTrue(Raised);
  g.AddVertex(1);
  AssertTrue(g.FindMinPathsMap(1, Weights));
  AssertTrue(Length(Weights) = 1);
  AssertTrue(Weights[0] = 0);
end;

procedure TWeightedGraphTest.FindMinPathsMap1;
var
  Ref: TRef;
  g: TGraph;
  Weights: TGraph.TWeightArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestWGr2;
  g := Ref;
  for I := 0 to Pred(g.VertexCount) do
    begin
      AssertTrue(g.FindMinPathsMap(I, Weights));
      AssertTrue(TIntHelper.Same(Weights, WEIGHTS_ARRAY[I]))
    end;
end;

procedure TWeightedGraphTest.FindMinPathsMap2;
var
  Ref: TRef;
  g: TGraph;
  Weights: TGraph.TWeightArray;
  PathTree: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestWGr2;
  g := Ref;
  for I := 0 to Pred(g.VertexCount) do
    begin
      AssertTrue(g.FindMinPathsMap(I, PathTree, Weights));
      AssertTrue(THelper.Same(PathTree, PATHTREE_ARRAY[I]))
    end;
end;

procedure TWeightedGraphTest.FindMinPathsMap3;
var
  Ref: TRef;
  g: TGraph;
  Weights: TGraph.TWeightArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestWGr1;
  g := Ref;
  for I := 0 to Pred(g.VertexCount) do
    AssertFalse(g.FindMinPathsMap(I, Weights));
end;

procedure TWeightedGraphTest.FindMinPathsMap4;
var
  Ref: TRef;
  g: TGraph;
  Weights: TGraph.TWeightArray;
  PathTree: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestWGr1;
  g := Ref;
  for I := 0 to Pred(g.VertexCount) do
    AssertFalse(g.FindMinPathsMap(I, PathTree, Weights));
end;

procedure TWeightedGraphTest.MinPath;
var
  Ref: TRef;
  g: TGraph;
  Path: TIntArray;
  Weight: Integer;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  try
    g.MinPath(0, 1, Weight);
  except
    Raised := True;
  end;
  AssertTrue(Raised);
  g.AddVertex(1);
  g.AddVertex(2);
  Path := g.MinPath(1, 2, Weight);
  AssertTrue(Weight = g.InfWeight);
  AssertTrue(Path.IsEmpty);
end;

procedure TWeightedGraphTest.MinPath1;
var
  Ref: TRef;
  g: TGraph;
  Path: TIntArray;
  Weight: Integer;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestWGr2;
  g := Ref;
  for I := 1 to Pred(g.VertexCount) do
    begin
      Path := g.MinPath(0, I, Weight);
      AssertTrue(Weight = WEIGHTS_ARRAY[0, I]);
      AssertTrue(THelper.Same(Path, PATHS_FROM_0[I - 1]));
    end;
end;

procedure TWeightedGraphTest.FindMinPath;
var
  Ref: TRef;
  g: TGraph;
  Path: TIntArray;
  Weight: Integer;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  try
    g.FindMinPath(0, 1, Path, Weight);
  except
    Raised := True;
  end;
  AssertTrue(Raised);
  g.AddVertex(1);
  g.AddVertex(2);
  AssertFalse(g.FindMinPath(1, 2, Path, Weight));
  AssertTrue(Weight = g.InfWeight);
  AssertTrue(Path.IsEmpty);
end;

procedure TWeightedGraphTest.FindMinPath1;
var
  Ref: TRef;
  g: TGraph;
  Path: TIntArray;
  Weight: Integer;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestWGr2;
  g := Ref;
  for I := 1 to Pred(g.VertexCount) do
    begin
      AssertTrue(g.FindMinPath(0, I, Path, Weight));
      AssertTrue(Weight = WEIGHTS_ARRAY[0, I]);
      AssertTrue(THelper.Same(Path, PATHS_FROM_0[I - 1]));
    end;
end;

procedure TWeightedGraphTest.FindMinPath2;
var
  Ref: TRef;
  g: TGraph;
  Path: TIntArray;
  Weight: Integer;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestWGr1;
  g := Ref;
  for I := 1 to Pred(g.VertexCount) do
    begin
      AssertFalse(g.FindMinPath(0, I, Path, Weight));
      AssertTrue(Weight = 0);
      AssertTrue(Path.Length = 3);
    end;
end;

procedure TWeightedGraphTest.MinPathAStar;
var
  Ref: specialize TGAutoRef<TPointsChart>;
  g: TPointsChart;
  AStarPath, DijkPath: TIntArray;
  AStarWeight, DijkWeight: ValReal;
const
  TestSize = 100;
begin
  {%H-}Ref.Instance := GenerateRandomPoints(TestSize);
  g := Ref;
  AStarPath := g.MinPathAStarI(0, 99, AStarWeight);
  DijkPath := g.MinPathI(0, 99, DijkWeight);
  AssertTrue(math.SameValue(AStarWeight, DijkWeight));
  AssertTrue(THelper.Same(AStarPath, DijkPath));
end;

procedure TWeightedGraphTest.FindAllPairMinPaths;
var
  Ref: TRef;
  g: TGraph;
  m: TGraph.TApspMatrix;
begin
  g := {%H-}Ref;
  AssertTrue(g.FindAllPairMinPaths(m));
  AssertTrue(m = nil);
  g.AddVertex(1);
  AssertTrue(g.FindAllPairMinPaths(m));
  AssertTrue(Length(m) = 1);
  AssertTrue(Length(m[0]) = 1);
  AssertTrue(m[0][0].Weight = 0);
  AssertTrue(m[0][0].Predecessor = -1);
end;

procedure TWeightedGraphTest.FindAllPairMinPaths1;
var
  Ref: TRef;
  g: TGraph;
  m: TGraph.TApspMatrix;
begin
  {%H-}Ref.Instance := GenerateTestWGr1;
  g := Ref;
  AssertFalse(g.FindAllPairMinPaths(m));
  AssertTrue(Length(m) = 1);
  AssertTrue(Length(m[0]) = 1);
end;

procedure TWeightedGraphTest.FindAllPairMinPaths2;
var
  Ref: TRef;
  g: TGraph;
  m: TGraph.TApspMatrix;
  Path: TIntArray;
  I, J: Integer;
begin
  {%H-}Ref.Instance := GenerateTestWGr2;
  g := Ref;
  AssertTrue(g.FindAllPairMinPaths(m));
  AssertTrue(Length(m) = g.VertexCount);
  for I := 0 to Pred(g.VertexCount) do
    begin
      AssertTrue(Length(m[I]) = g.VertexCount);
      for J := 0 to Pred(g.VertexCount) do
        AssertTrue(m[I, J].Weight = WEIGHTS_ARRAY[I, J]);
    end;
  for I := 1 to Pred(g.VertexCount) do
    begin
      Path := g.ExtractMinPath(0, I, m);
      AssertTrue(THelper.Same(Path, PATHS_FROM_0[I - 1]));
    end;
end;

procedure TWeightedGraphTest.FindEccentricity;
var
  Ref: TRef;
  g: TGraph;
  Ecc: Integer;
begin
  {%H-}Ref.Instance := GenerateTestWGr1;
  g := Ref;
  AssertFalse(g.FindEccentricity(0, Ecc));
end;

procedure TWeightedGraphTest.FindEccentricity1;
var
  Ref: TRef;
  g: TGraph;
  I, Ecc: Integer;
begin
  {%H-}Ref.Instance := GenerateTestWGr2;
  g := Ref;
  for I := 0 to Pred(g.VertexCount) do
    begin
      AssertTrue(g.FindEccentricity(I, Ecc));
      AssertTrue(Ecc = ECCENTR[I]);
    end;
end;

procedure TWeightedGraphTest.FindWeightedMetrics;
var
  Ref: TRef;
  g: TGraph;
  r, d: Integer;
begin
  {%H-}Ref.Instance := GenerateTestWGr1;
  g := Ref;
  AssertFalse(g.FindWeightedMetrics(r, d));
end;

procedure TWeightedGraphTest.FindWeightedMetrics1;
var
  Ref: TRef;
  g: TGraph;
  r, d: Integer;
begin
  {%H-}Ref.Instance := GenerateTestWGr2;
  g := Ref;
  AssertTrue(g.FindWeightedMetrics(r, d));
  AssertTrue(r = 4);
  AssertTrue(d = 6);
end;

procedure TWeightedGraphTest.MinSpanningTreeKrus;
var
  Ref: TRef;
  g, g1: TGraph;
  Edges: TIntEdgeArray;
  Weight: Integer;
begin
  g := {%H-}Ref;
  Edges := g.MinSpanningTreeKrus(Weight);
  AssertTrue(Edges = nil);
  AssertTrue(Weight = 0);
  Ref.Instance := GenerateTestWGr1;
  g := Ref;
  Edges := g.MinSpanningTreeKrus(Weight);
  AssertTrue(Weight = 5);
  g1 := g.SubgraphFromEdges(Edges);
  try
    AssertTrue(g1.VertexCount = g.VertexCount);
    AssertTrue(g1.IsTree);
  finally
    g1.Free;
  end;
end;

procedure TWeightedGraphTest.MinSpanningTreeKrus1;
var
  Ref: TRef;
  g, g1: TGraph;
  Edges: TIntEdgeArray;
  w: Integer;
begin
  {%H-}Ref.Instance := GenerateTestWGr2;
  g := Ref;
  Edges := g.MinSpanningTreeKrus(w);
  AssertTrue(w = 7);
  g1 := g.SubgraphFromEdges(Edges);
  try
    AssertTrue(g1.VertexCount = g.VertexCount);
    AssertTrue(g1.IsTree);
  finally
    g1.Free;
  end;
end;

procedure TWeightedGraphTest.MinSpanningTreePrim;
var
  Ref: TRef;
  g, g1: TGraph;
  Tree: TIntArray;
  w: Integer;
begin
  g := {%H-}Ref;
  Tree := g.MinSpanningTreePrim(w);
  AssertTrue(Tree = nil);
  AssertTrue(w = 0);
  Ref.Instance := GenerateTestWGr1;
  g := Ref;
  Tree := g.MinSpanningTreePrim(w);
  AssertTrue(w = 5);
  g1 := g.SubgraphFromTree(Tree);
  try
    AssertTrue(g1.VertexCount = g.VertexCount);
    AssertTrue(g1.IsTree);
  finally
    g1.Free;
  end;
end;

procedure TWeightedGraphTest.MinSpanningTreePrim1;
var
  Ref: TRef;
  g, g1: TGraph;
  Tree: TIntArray;
  w: Integer;
begin
  {%H-}Ref.Instance := GenerateTestWGr2;
  g := Ref;
  Tree := g.MinSpanningTreePrim(w);
  AssertTrue(w = 7);
  g1 := g.SubgraphFromTree(Tree);
  try
    AssertTrue(g1.VertexCount = g.VertexCount);
    AssertTrue(g1.IsTree);
  finally
    g1.Free;
  end;
end;

procedure TWeightedGraphTest.MinSpanningTree;
var
  Ref: specialize TGAutoRef<TPointsChart>;
  g: TPointsChart;
  KrusWeight, PrimWeight: ValReal;
const
  TestSize = 100;
begin
  {%H-}Ref.Instance := GenerateRandomPoints(TestSize);
  g := Ref;
  g.MinSpanningTreeKrus(KrusWeight);
  g.MinSpanningTreePrim(PrimWeight);
  AssertTrue(math.SameValue(KrusWeight, PrimWeight));
end;

procedure TWeightedGraphTest.FindMinWeightBipMatch;
var
  Ref: specialize TGAutoRef<TGraph64>;
  g: TGraph64;
  Match: TGraph.TEdgeArray;
begin
  g := {%H-}Ref;
  AssertFalse(g.FindMinWeightBipMatch(Match));
  AssertTrue(Match = nil);
  g.AddVertex(1);
  AssertFalse(g.FindMinWeightBipMatch(Match));
  AssertTrue(Match = nil);
  g.AddVertex(2);
  AssertTrue(g.FindMinWeightBipMatch(Match));
  AssertTrue(Match = nil);
end;

procedure TWeightedGraphTest.FindMinWeightBipMatch1;
var
  Ref: specialize TGAutoRef<TGraph64>;
  g: TGraph64;
  Match: TGraph.TEdgeArray;
  IntMatch: TIntEdgeArray;
begin
  {%H-}Ref.Instance := GenerateTestWGrBip1;
  g := Ref;
  AssertTrue(g.FindMinWeightBipMatch(Match));
  IntMatch := g.EdgeArray2IntEdgeArray(Match);
  AssertTrue(g.IsMaxMatching(IntMatch));
  AssertTrue(g.IsPerfectMatching(IntMatch));
  AssertTrue(g.TotalWeight(Match) = 58);
end;

procedure TWeightedGraphTest.FindMaxWeightBipMatch;
var
  Ref: specialize TGAutoRef<TGraph64>;
  g: TGraph64;
  Match: TGraph.TEdgeArray;
begin
  g := {%H-}Ref;
  AssertFalse(g.FindMaxWeightBipMatch(Match));
  AssertTrue(Match = nil);
  g.AddVertex(1);
  AssertFalse(g.FindMaxWeightBipMatch(Match));
  AssertTrue(Match = nil);
  g.AddVertex(2);
  AssertTrue(g.FindMaxWeightBipMatch(Match));
  AssertTrue(Match = nil);
end;

procedure TWeightedGraphTest.FindMaxWeightBipMatch1;
var
  Ref: specialize TGAutoRef<TGraph64>;
  g: TGraph64;
  Match: TGraph.TEdgeArray;
  IntMatch: TIntEdgeArray;
begin
  {%H-}Ref.Instance := GenerateTestWGrBip1;
  g := Ref;
  AssertTrue(g.FindMaxWeightBipMatch(Match));
  IntMatch := g.EdgeArray2IntEdgeArray(Match);
  AssertTrue(g.IsMaxMatching(IntMatch));
  AssertTrue(g.IsPerfectMatching(IntMatch));
  AssertTrue(g.TotalWeight(Match) = 270);
end;

procedure TWeightedGraphTest.MinWeightCutSW;
var
  Ref: specialize TGAutoRef<TGraph64>;
  g: TGraph64;
  Cut: TGraph64.TCut;
  w: Int64;
begin
  g := {%H-}Ref;
  w := 50;
  Cut.A := [3];
  Cut.B := [4];
  AssertTrue(g.MinWeightCutSW(Cut, w) = gnsTrivial);
  AssertTrue(w = 0);
  AssertTrue(Cut.A.IsEmpty);
  AssertTrue(Cut.B.IsEmpty);
  g.AddVertex(1);
  w := 50;
  Cut.A := [3];
  Cut.B := [4];
  AssertTrue(g.MinWeightCutSW(Cut, w) = gnsTrivial);
  AssertTrue(w = 0);
  AssertTrue(Cut.A.IsEmpty);
  AssertTrue(Cut.B.IsEmpty);
  g.AddVertex(2);
  w := 50;
  Cut.A := [3];
  Cut.B := [4];
  AssertTrue(g.MinWeightCutSW(Cut, w) = gnsDisconnected);
  AssertTrue(w = 0);
  AssertTrue(Cut.A.IsEmpty);
  AssertTrue(Cut.B.IsEmpty);
  g.AddEdge(1, 2, TInt64Weight.Create(-100));
  w := 50;
  Cut.A := [3];
  Cut.B := [4];
  AssertTrue(g.MinWeightCutSW(Cut, w) = gnsNegEdgeCapacity);
  AssertTrue(w = 0);
  AssertTrue(Cut.A.IsEmpty);
  AssertTrue(Cut.B.IsEmpty);
  w := 50;
  Cut.A := [3];
  Cut.B := [4];
  g.SetEdgeData(1, 2, TInt64Weight.Create(100));
  AssertTrue(g.MinWeightCutSW(Cut, w) = gnsOk);
  AssertTrue(w = 100);
  AssertTrue(Cut.A.Length = 1);
  AssertTrue(Cut.B.Length = 1);
  AssertTrue(Cut.A[0] <> Cut.B[0]);
end;

procedure TWeightedGraphTest.MinWeightCutSW1;
var
  Ref: specialize TGAutoRef<TGraph64>;
  g: TGraph64;
  Cut: TGraph64.TCut;
  I: SizeInt;
  w: Int64;
begin
  {%H-}Ref.Instance := GenerateTestWGr3;
  g := Ref;
  AssertTrue(g.MinWeightCutSW(Cut, w) = gnsOk);
  AssertTrue(w = 110);
  AssertTrue(Cut.A.Length + Cut.B.Length = g.VertexCount);
  AssertTrue(Cut.A.Length = 4);
  if Cut.A[0] in [0..3] then
    begin
      for I in [0..3] do
        AssertTrue(THelper.SequentSearch(Cut.A, I) <> NULL_INDEX);
      for I in [4..7] do
        AssertTrue(THelper.SequentSearch(Cut.B, I) <> NULL_INDEX);
    end
  else
    begin
      for I in [0..3] do
        AssertTrue(THelper.SequentSearch(Cut.B, I) <> NULL_INDEX);
      for I in [4..7] do
        AssertTrue(THelper.SequentSearch(Cut.A, I) <> NULL_INDEX);
    end;
end;

procedure TWeightedGraphTest.MinWeightCutSW2;
var
  Ref: specialize TGAutoRef<TGraph64>;
  g: TGraph64;
  Cut: TGraph64.TCut;
  I: SizeInt;
  w: Int64;
begin
  {%H-}Ref.Instance := GenerateTestWGr3;
  g := Ref;
  g.SetEdgeData(4, 6, TInt64Weight.Create(45));
  AssertTrue(g.MinWeightCutSW(Cut, w) = gnsOk);
  AssertTrue(w = 105);
  AssertTrue(Cut.A.Length + Cut.B.Length = g.VertexCount);
  if Cut.A[0] in [0..5] then
    begin
      for I in [0..5] do
        AssertTrue(THelper.SequentSearch(Cut.A, I) <> NULL_INDEX);
      for I in [6..7] do
        AssertTrue(THelper.SequentSearch(Cut.B, I) <> NULL_INDEX);
    end
  else
    begin
      for I in [0..5] do
        AssertTrue(THelper.SequentSearch(Cut.B, I) <> NULL_INDEX);
      for I in [6..7] do
        AssertTrue(THelper.SequentSearch(Cut.A, I) <> NULL_INDEX);
    end;
end;

procedure TWeightedGraphTest.MinWeightCutNI;
var
  Ref: specialize TGAutoRef<TGraph64>;
  g: TGraph64;
  Cut: TGraph64.TCut;
  w: Int64;
begin
  g := {%H-}Ref;
  w := 50;
  Cut.A := [3];
  Cut.B := [4];
  AssertTrue(g.MinWeightCutNI(Cut, w) = gnsTrivial);
  AssertTrue(w = 0);
  AssertTrue(Cut.A.IsEmpty);
  AssertTrue(Cut.B.IsEmpty);
  g.AddVertex(1);
  w := 50;
  Cut.A := [3];
  Cut.B := [4];
  AssertTrue(g.MinWeightCutNI(Cut, w) = gnsTrivial);
  AssertTrue(w = 0);
  AssertTrue(Cut.A.IsEmpty);
  AssertTrue(Cut.B.IsEmpty);
  g.AddVertex(2);
  w := 50;
  Cut.A := [3];
  Cut.B := [4];
  AssertTrue(g.MinWeightCutNI(Cut, w) = gnsDisconnected);
  AssertTrue(w = 0);
  AssertTrue(Cut.A.IsEmpty);
  AssertTrue(Cut.B.IsEmpty);
  g.AddEdge(1, 2, TInt64Weight.Create(-100));
  w := 50;
  Cut.A := [3];
  Cut.B := [4];
  AssertTrue(g.MinWeightCutNI(Cut, w) = gnsNegEdgeCapacity);
  AssertTrue(w = 0);
  AssertTrue(Cut.A.IsEmpty);
  AssertTrue(Cut.B.IsEmpty);
  g.SetEdgeData(1, 2, TInt64Weight.Create(100));
  w := 50;
  Cut.A := [3];
  Cut.B := [4];
  AssertTrue(g.MinWeightCutNI(Cut, w) = gnsOk);
  AssertTrue(w = 100);
  AssertTrue(Cut.A.Length = 1);
  AssertTrue(Cut.B.Length = 1);
  AssertTrue(Cut.A[0] <> Cut.B[0]);
end;

procedure TWeightedGraphTest.MinWeightCutNI1;
var
  Ref: specialize TGAutoRef<TGraph64>;
  g: TGraph64;
  Cut: TGraph64.TCut;
  I: SizeInt;
  w: Int64;
begin
  {%H-}Ref.Instance := GenerateTestWGr3;
  g := Ref;
  AssertTrue(g.MinWeightCutNI(Cut, w) = gnsOk);
  AssertTrue(w = 110);
  AssertTrue(Cut.A.Length + Cut.B.Length = g.VertexCount);
  AssertTrue(Cut.A.Length = 4);
  if Cut.A[0] in [0..3] then
    begin
      for I in [0..3] do
        AssertTrue(THelper.SequentSearch(Cut.A, I) <> NULL_INDEX);
      for I in [4..7] do
        AssertTrue(THelper.SequentSearch(Cut.B, I) <> NULL_INDEX);
    end
  else
    begin
      for I in [0..3] do
        AssertTrue(THelper.SequentSearch(Cut.B, I) <> NULL_INDEX);
      for I in [4..7] do
        AssertTrue(THelper.SequentSearch(Cut.A, I) <> NULL_INDEX);
    end;
end;

procedure TWeightedGraphTest.MinWeightCutNI2;
var
  Ref: specialize TGAutoRef<TGraph64>;
  g: TGraph64;
  Cut: TGraph64.TCut;
  I: SizeInt;
  w: Int64;
begin
  {%H-}Ref.Instance := GenerateTestWGr3;
  g := Ref;
  g.SetEdgeData(4, 6, TInt64Weight.Create(45));
  AssertTrue(g.MinWeightCutNI(Cut, w) = gnsOk);
  AssertTrue(w = 105);
  AssertTrue(Cut.A.Length + Cut.B.Length = g.VertexCount);
  if Cut.A[0] in [0..5] then
    begin
      for I in [0..5] do
        AssertTrue(THelper.SequentSearch(Cut.A, I) <> NULL_INDEX);
      for I in [6..7] do
        AssertTrue(THelper.SequentSearch(Cut.B, I) <> NULL_INDEX);
    end
  else
    begin
      for I in [0..5] do
        AssertTrue(THelper.SequentSearch(Cut.B, I) <> NULL_INDEX);
      for I in [6..7] do
        AssertTrue(THelper.SequentSearch(Cut.A, I) <> NULL_INDEX);
    end;
end;

procedure TWeightedGraphTest.MinWeightCutNI3;
var
  Ref: specialize TGAutoRef<TGraph64>;
  g: TGraph64;
  Cut: TGraph64.TCut;
  a : TGraph64.TEdgeArray;
begin
  {%H-}Ref.Instance := GenerateTestWGr3;
  g := Ref;
  AssertTrue(g.MinWeightCutNI(Cut, a) = gnsOk);
  AssertTrue(Length(a) = 2);
  AssertTrue(g.TotalWeight(a) = 110);
  if (a[0].Source = 2) or (a[0].Source = 4) then
    begin
      if a[0].Source = 2 then
        AssertTrue(a[0].Destination = 4)
      else
        AssertTrue(a[0].Destination = 2);
      if a[1].Source = 3 then
        AssertTrue(a[1].Destination = 5)
      else
        AssertTrue(a[1].Destination = 3);
    end
  else
    begin
      if a[0].Source = 3 then
        AssertTrue(a[0].Destination = 5)
      else
        AssertTrue(a[0].Destination = 3);
      if a[1].Source = 2 then
        AssertTrue(a[1].Destination = 4)
      else
        AssertTrue(a[1].Destination = 2);
    end;
end;

procedure TWeightedGraphTest.MinWeightCutNI4;
var
  Ref: specialize TGAutoRef<TGraph64>;
  g: TGraph64;
  Cut: TGraph64.TCut;
  a : TGraph64.TEdgeArray;
begin
  {%H-}Ref.Instance := GenerateTestWGr3;
  g := Ref;
  g.SetEdgeData(1, 3, TInt64Weight.Create(45));
  AssertTrue(g.MinWeightCutNI(Cut, a) = gnsOk);
  AssertTrue(Length(a) = 2);
  AssertTrue(g.TotalWeight(a) = 105);
  if (a[0].Source = 0) or (a[0].Source = 2) then
    begin
      if a[0].Source = 0 then
        AssertTrue(a[0].Destination = 2)
      else
        AssertTrue(a[0].Destination = 0);
      if a[1].Source = 1 then
        AssertTrue(a[1].Destination = 3)
      else
        AssertTrue(a[1].Destination = 1);
    end
  else
    begin
      if a[0].Source = 1 then
        AssertTrue(a[0].Destination = 3)
      else
        AssertTrue(a[0].Destination = 1);
      if a[1].Source = 0 then
        AssertTrue(a[1].Destination = 2)
      else
        AssertTrue(a[1].Destination = 0);
    end;
end;

initialization
  RegisterTest(TSimpleGraphTest);
  RegisterTest(TWeightedGraphTest);
end.

