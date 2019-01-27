unit LGSimpleDiGraphTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  LGUtils,
  LGArrayHelpers,
  LGSparseGraph,
  LGSimpleDiGraph;

type

  { TSimpleDiGraphTest }

  TSimpleDiGraphTest = class(TTestCase)
  private
  type
    TGraph  = TIntFlowChart;
    TRef    = specialize TGAutoRef<TGraph>;
    THelper = specialize TGOrdinalArrayHelper<SizeInt>;

    function  GenerateTestDigr1: TGraph;
    function  GenerateTestDigr2: TGraph;
    function  GenerateTestDigr3: TGraph;
    function  GenerateRandomGraph(aSize, aDeg: Integer): TGraph;
  published
    procedure AddVertices;
    procedure SaveToStream;
    procedure Clone;
    procedure Reverse;
    procedure InducedSubgraph;
    procedure SymmDifferenceOf;
    procedure SymmDifferenceOf1;
    procedure SymmDifferenceOf2;
    procedure SetUnionOf;
    procedure SetIntersectionOf;
    procedure SetIntersectionOf2;
    procedure InDegree;
    procedure OutDegree;
    procedure Degree;
    procedure IsSource;
    procedure IsSink;
    procedure IncomingArcs;
    procedure IncomingArcs1;
    procedure PathExists;
    procedure PathExists1;
    procedure ContainsCycle;
    procedure ContainsCycle1;
    procedure ContainsEulerianCycle;
    procedure ContainsEulerianCycle1;
    procedure FindEulerianCycle;
    procedure IsStrongConnected;
    procedure FindStrongComponents;
    procedure FindStrongComponents1;
    procedure FindStrongComponents2;
    procedure GetStrongComponent;
    procedure BuildReachabilityMatrix;
    procedure BuildReachabilityMatrix1;
    procedure BuildReachabilityMatrix2;
    procedure FindMetrics;
    procedure FindCenter;
    procedure FindPeripheral;
    procedure IsDag;
    procedure TopologicalSort;
    procedure DagLongestPathsMap;
    procedure DagLongestPathsMap1;
    procedure DagLongestPaths;
    procedure FindHamiltonCycles;
    procedure FindHamiltonCycles1;
    procedure FindHamiltonCycles2;
    procedure FindHamiltonPaths;
    procedure FindHamiltonPaths1;
  end;

  { TWeightedDigraphTest }

  TWeightedDigraphTest = class(TTestCase)
  private
  type
    TIntWeight   = specialize TGSimpleWeight<Integer>;
    TInt64Weight = specialize TGSimpleWeight<Int64>;
    TGraph       = specialize TGWeightedDigraph<Integer, Integer, TIntWeight, Integer>;
    TGraph64     = specialize TGIntWeightDigraph<Integer, TInt64Weight, Integer>;
    TRef         = specialize TGAutoRef<TGraph>;
    TRef64       = specialize TGAutoRef<TGraph64>;
    THelper      = specialize TGOrdinalArrayHelper<Integer>;
    TSearch      = specialize TGNumArrayHelper<SizeInt>;

    function  GenerateTestWDigr1: TGraph;
    function  GenerateTestWDigr2: TGraph;
    function  GenerateTestWDigrBip1: TGraph64;
    function  GenerateTestWDigr3: TGraph64;
    function  GenerateTestWDigr4: TGraph64;
    function  GenerateCostFun3: TCostEdgeArray;
    function  GenerateCostFun4: TCostEdgeArray;
  published
    procedure ContainsNegWeightEdge;
    procedure ContainsNegCycle;
    procedure MinPathsMap;
    procedure MinPathsMap1;
    procedure FindMinPathsMap;
    procedure FindMinPathsMap1;
    procedure FindMinPathsMap3;
    procedure FindMinPathsMap4;
    procedure MinPath;
    procedure MinPath1;
    procedure FindMinPath;
    procedure FindMinPath1;
    procedure FindMinPath2;
    procedure FindMinPath3;
    procedure FindAllPairMinPaths;
    procedure FindAllPairMinPaths1;
    procedure FindAllPairMinPaths2;
    procedure FindEccentricity;
    procedure FindEccentricity1;
    procedure FindWeightedMetrics;
    procedure FindWeightedMetrics1;
    procedure FindWeightedCenter;
    procedure DagMinPathsMap;
    procedure DagMinPathsMap1;
    procedure FindDagAllPairMinPaths;
    procedure FindDagAllPairMinPaths1;
    procedure FindDagAllPairMinPaths2;
    procedure DagMaxPathsMap;
    procedure DagMaxPathsMap1;
    procedure DagMaxPaths;
    procedure DagMaxPaths1;
    procedure FindMinWeightBipMatch;
    procedure FindMinWeightBipMatch1;
    procedure FindMaxWeightBipMatch;
    procedure FindMaxWeightBipMatch1;
    procedure FindMaxFlowPr;
    procedure FindMaxFlowPr1;
    procedure FindMaxFlowPr2;
    procedure FindMaxFlowD;
    procedure FindMaxFlowD1;
    procedure FindMinSTCutPr;
    procedure FindMinSTCutPr1;
    procedure FindMinSTCutD;
    procedure FindMinSTCutD1;
    procedure FindMinCostFlowSsp;
    procedure FindMinCostFlowSsp1;
    procedure FindMinCostFlowSsp2;
    procedure FindMinCostFlowSsp3;
    procedure FindMinCostFlowCs;
    procedure FindMinCostFlowCs1;
    procedure FindMinCostFlowCs2;
    procedure FindMinCostFlowCs3;
  end;

implementation

function TSimpleDiGraphTest.GenerateTestDigr1: TGraph;
begin
  Result := TGraph.Create;  //TestDigr1.png
  Result.AddVertexRange(0, 12);
  Result.AddEdges([0, 1, 0, 2, 0, 3, 0, 5, 0, 6, 2, 3, 3, 5, 3, 4, 6, 4, 4, 9, 6, 9, 7, 6,
                   8, 7, 9, 10, 9, 11, 9, 12, 11, 12]);
end;

function TSimpleDiGraphTest.GenerateTestDigr2: TGraph;
begin
  Result := TGraph.Create;  //TestDigr2.png
  Result.AddVertexRange(0, 8);
  Result.AddEdges([0, 1, 1, 2, 2, 3, 3, 0, 1, 5, 5, 3, 3, 4, 4, 1, 0, 6, 6, 7, 7, 8, 8, 0]);
end;

function TSimpleDiGraphTest.GenerateTestDigr3: TGraph;
begin
  Result := TGraph.Create;
  Result.AddVertexRange(0, 5);
  Result.AddEdges([0, 1,  1, 1,  1, 2,  1, 4,  2, 0,  2, 3,  3, 2,  3, 5,  4, 2, 4, 3,  5, 0,  5, 1,  5, 2]);
end;

function TSimpleDiGraphTest.GenerateRandomGraph(aSize, aDeg: Integer): TGraph;
var
  s, d: Integer;
begin
  Result := TGraph.Create;
  if aSize < 1 then
    exit;
  if aDeg > Pred(aSize) then
    aDeg := Pred(aSize);
  for s := 0 to Pred(aSize) do
    Result.AddVertex(s);
  repeat
    s := Random(aSize);
    repeat
      d := Random(aSize)
    until d <> s;
    Result.AddEdge(s, d);
    if Result.EdgeCount >= aSize * aDeg then
      exit;
  until False;
end;

procedure TSimpleDiGraphTest.AddVertices;
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

procedure TSimpleDiGraphTest.SaveToStream;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
  Stream: TMemoryStream;
  e: TGraph.TEdge;
  Title, Description: string;
begin
  Title := 'Test graph';
  Description := 'for testing purposes';
  {%H-}Ref.Instance := GenerateTestDigr1;
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

procedure TSimpleDiGraphTest.Clone;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
  e: TGraph.TEdge;
  Title, Description: string;
begin
  g := {%H-}Ref;
  {%H-}Ref2.Instance := g.Clone;
  g2 := Ref2;
  AssertTrue(g2.IsEmpty);
  AssertTrue(g2.Title = 'Untitled');
  AssertTrue(g2.Description.Text = '');
  Title := 'Test graph';
  Description := 'for testing purposes';
  Ref.Instance := GenerateTestDigr1;
  g := Ref;
  AssertTrue(g.VertexCount = 13);
  AssertTrue(g.EdgeCount = 17);
  g.Title := Title;
  g.Description.Text := Description;
  Ref2.Instance := g.Clone;
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

procedure TSimpleDiGraphTest.Reverse;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
  e: TGraph.TEdge;
  Title, Description: string;
begin
  Title := 'Test graph';
  Description := 'for testing purposes';
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  AssertTrue(g.VertexCount = 13);
  AssertTrue(g.EdgeCount = 17);
  g.Title := Title;
  g.Description.Text := Description;
  {%H-}Ref2.Instance := g.Reverse;
  g2 := Ref2;
  AssertTrue(g2.VertexCount = 13);
  AssertTrue(g2.EdgeCount = 17);
  for e in g.Edges do
    AssertTrue(g2.ContainsEdge(g[e.Destination], g[e.Source]));
  for e in g2.Edges do
    AssertTrue(g.ContainsEdge(g2[e.Destination], g2[e.Source]));
  AssertTrue(g2.Title = Title);
  AssertTrue(g2.Description.Text = Description);
end;

procedure TSimpleDiGraphTest.InducedSubgraph;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  AssertTrue(g.VertexCount = 13);
  {%H-}Ref2.Instance := g.InducedSubgraph([]);
  g2 := Ref2;
  AssertTrue(g2.IsEmpty);
  Ref2.Instance := g.InducedSubgraph([3]);
  g2 := Ref2;
  AssertTrue(g2.VertexCount = 1);
  AssertTrue(g2.ContainsVertex(3));
  AssertTrue(g2.EdgeCount = 0);
  Ref2.Instance := g.InducedSubgraph([0, 1, 2, 3, 4]);
  g2 := Ref2;
  AssertTrue(g2.VertexCount = 5);
  AssertTrue(g2.EdgeCount = 5);
  AssertTrue(g2.ContainsEdge(0, 1));
  AssertTrue(g2.ContainsEdge(0, 2));
  AssertTrue(g2.ContainsEdge(0, 3));
  AssertTrue(g2.ContainsEdge(2, 3));
  AssertTrue(g2.ContainsEdge(3, 4));
end;

procedure TSimpleDiGraphTest.SymmDifferenceOf;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
  e: TGraph.TEdge;
  Title, Description: string;
begin
  Title := 'Test graph';
  Description := 'for testing purposes';
  g := {%H-}Ref;
  g.Title := Title;
  g.Description.Text := Description;
  g2 := {%H-}Ref2;
  g.SetSymmDifferenceOf(g2);
  AssertTrue(g.IsEmpty);
  Ref2.Instance := GenerateTestDigr1;
  g2 := Ref2;
  g.SetSymmDifferenceOf(g2);
  AssertTrue(g.VertexCount = 13);
  AssertTrue(g.EdgeCount = 17);
  AssertTrue(g.Title = Title);
  AssertTrue(g.Description.Text = Description);
  for e in g.Edges do
    AssertTrue(g2.ContainsEdge(g[e.Source], g[e.Destination]));
end;

procedure TSimpleDiGraphTest.SymmDifferenceOf1;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  {%H-}Ref2.Instance := GenerateTestDigr1;
  g := Ref;
  g2 := Ref2;
  g.SetSymmDifferenceOf(g2);
  AssertTrue(g.IsEmpty);
end;

procedure TSimpleDiGraphTest.SymmDifferenceOf2;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
  e: TGraph.TEdge;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  g2 := {%H-}Ref2;
  g2.AddEdge(5, 13);
  g2.SetSymmDifferenceOf(g);
  AssertTrue(g2.VertexCount = 14);
  AssertTrue(g2.EdgeCount = 18);
  for e in g.Edges do
    AssertTrue(g2.ContainsEdge(g[e.Source], g[e.Destination]));
  AssertTrue(g2.ContainsEdge(5, 13));
  g2.SetSymmDifferenceOf(g);
  for e in g.Edges do
    AssertFalse(g2.ContainsEdge(g[e.Source], g[e.Destination]));
  AssertTrue(g2.ContainsEdge(5, 13));
end;

procedure TSimpleDiGraphTest.SetUnionOf;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
  e: TGraph.TEdge;
begin
  g := {%H-}Ref;
  g2 := {%H-}Ref2;
  g.SetUnionOf(g2);
  AssertTrue(g.IsEmpty);
  Ref2.Instance := GenerateTestDigr1;
  g2 := Ref2;
  g.SetUnionOf(g2);
  AssertTrue(g.VertexCount = 13);
  AssertTrue(g.EdgeCount = 17);
  for e in g.Edges do
    AssertTrue(g2.ContainsEdge(g[e.Source], g[e.Destination]));
  g.SetUnionOf(g2);
  AssertTrue(g.VertexCount = 13);
  AssertTrue(g.EdgeCount = 17);
  for e in g.Edges do
    AssertTrue(g2.ContainsEdge(g[e.Source], g[e.Destination]));
end;

procedure TSimpleDiGraphTest.SetIntersectionOf;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
  e: TGraph.TEdge;
begin
  g := {%H-}Ref;
  g2 := {%H-}Ref2;
  g.SetIntersectionOf(g2);
  AssertTrue(g.IsEmpty);
  Ref2.Instance := GenerateTestDigr1;
  g2 := Ref2;
  g.SetIntersectionOf(g2);
  AssertTrue(g.IsEmpty);
  Ref.Instance := GenerateTestDigr1;
  g := Ref;
  g.SetIntersectionOf(g2);
  AssertTrue(g.VertexCount = 13);
  AssertTrue(g.EdgeCount = 17);
  for e in g.Edges do
    AssertTrue(g2.ContainsEdge(g[e.Source], g[e.Destination]));
end;

procedure TSimpleDiGraphTest.SetIntersectionOf2;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  g2 := {%H-}Ref2;
  g2.AddEdge(5, 13);
  g.SetIntersectionOf(g2);
  AssertTrue(g.VertexCount = 1);
  AssertTrue(g.EdgeCount = 0);
end;

procedure TSimpleDiGraphTest.InDegree;
var
  Ref: TRef;
  g: TGraph;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  g.AddEdge(1, 2);
  AssertTrue(g.InDegree(1) = 0);
  AssertTrue(g.InDegree(2) = 1);
  g.RemoveVertex(1);
  AssertTrue(g.InDegree(2) = 0);
  try
    g.InDegree(1);
  except
    on e: EGraphError do
      Raised := True;
  end;
  AssertTrue(Raised);
  g.AddEdges([1, 4, 2, 4, 3, 4]);
  AssertTrue(g.InDegree(1) = 0);
  AssertTrue(g.InDegree(2) = 0);
  AssertTrue(g.InDegree(3) = 0);
  AssertTrue(g.InDegree(4) = 3);
  g.RemoveEdge(2, 4);
  AssertTrue(g.InDegree(4) = 2);
end;

procedure TSimpleDiGraphTest.OutDegree;
var
  Ref: TRef;
  g: TGraph;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  g.AddEdge(1, 2);
  AssertTrue(g.OutDegree(1) = 1);
  AssertTrue(g.OutDegree(2) = 0);
  g.RemoveVertex(2);
  AssertTrue(g.OutDegree(1) = 0);
  try
    g.OutDegree(2);
  except
    on e: EGraphError do
      Raised := True;
  end;
  AssertTrue(Raised);
  g.AddEdges([1, 2, 1, 3, 1, 4]);
  AssertTrue(g.OutDegree(1) = 3);
  AssertTrue(g.OutDegree(2) = 0);
  AssertTrue(g.OutDegree(3) = 0);
  AssertTrue(g.OutDegree(4) = 0);
  g.RemoveEdge(1, 4);
  AssertTrue(g.OutDegree(1) = 2);
end;

procedure TSimpleDiGraphTest.Degree;
var
  Ref: TRef;
  g: TGraph;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  g.AddEdge(1, 2);
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
  g.RemoveEdge(1, 4);
  AssertTrue(g.Degree(1) = 2);
  AssertTrue(g.Degree(4) = 0);
end;

procedure TSimpleDiGraphTest.IsSource;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  g.AddEdges([1, 2, 1, 3, 1, 4]);
  AssertTrue(g.IsSource(1));
  AssertFalse(g.IsSource(2));
  AssertFalse(g.IsSource(3));
  AssertFalse(g.IsSource(4));
end;

procedure TSimpleDiGraphTest.IsSink;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  g.AddEdges([1, 2, 1, 3, 1, 4]);
  AssertFalse(g.IsSink(1));
  AssertTrue(g.IsSink(2));
  AssertTrue(g.IsSink(3));
  AssertTrue(g.IsSink(4));
end;

procedure TSimpleDiGraphTest.IncomingArcs;
var
  Ref: TRef;
  g: TGraph;
  a: TGraph.TIncomingArc;
  c: SizeInt;
begin
  g := {%H-}Ref;
  g.AddVertex(1);
  c := 0;
  for a in g.IncomingArcs(1) do
    Inc(c);
  AssertTrue(c = 0);
  g.AddEdge(2, 1);
  for a in g.IncomingArcs(1) do
    Inc(c);
  AssertTrue(c = 1);
end;

procedure TSimpleDiGraphTest.IncomingArcs1;
var
  Ref: TRef;
  g: TGraph;
  a: TGraph.TIncomingArc;
  I, c: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr3;
  g := Ref;
  for I := 0 to Pred(g.VertexCount) do
    begin
      c := 0;
      for a in g.IncomingArcsI(I) do
        begin
          Inc(c);
          AssertTrue(g.ContainsEdgeI(a.Source, I));
        end;
      AssertTrue(c = g.InDegreeI(I));
    end;
end;

procedure TSimpleDiGraphTest.PathExists;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  g.AddEdges([1, 2, 1, 3, 1, 4]);
  AssertTrue(g.PathExists(1, 2));
  AssertTrue(g.PathExists(1, 3));
  AssertTrue(g.PathExists(1, 4));
  AssertFalse(g.PathExists(2, 1));
  AssertFalse(g.PathExists(3, 1));
  AssertFalse(g.PathExists(4, 1));
end;

procedure TSimpleDiGraphTest.PathExists1;
var
  Ref: TRef;
  g: TGraph;
  I, Src: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  Src := 0;
  for I := 1 to Pred(g.VertexCount) do
    if (I <> 7) and (I <> 8) then
      AssertTrue(g.PathExists(Src, I));
  AssertFalse(g.PathExists(0, 7));
  AssertFalse(g.PathExists(0, 8));
end;

procedure TSimpleDiGraphTest.ContainsCycle;
var
  Ref: TRef;
  g: TGraph;
  Cycle: TIntArray;
begin
  g := {%H-}Ref;
  g.AddVertex(1);
  Cycle := [0];
  AssertFalse(g.ContainsCycle(1, Cycle));
  AssertTrue(Cycle.IsEmpty);
  g.AddEdge(1, 2);
  Cycle := [0];
  AssertFalse(g.ContainsCycle(1, Cycle));
  AssertTrue(Cycle.IsEmpty);
  g.AddEdge(2, 1);
  AssertTrue(g.ContainsCycle(1, Cycle));
  AssertTrue(THelper.Same(Cycle, [1, 0, 1]));
end;

procedure TSimpleDiGraphTest.ContainsCycle1;
var
  Ref: TRef;
  g: TGraph;
  Cycle: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  for I := 0 to Pred(g.VertexCount) do
    AssertFalse(g.ContainsCycle(I, Cycle));
  g.AddEdge(12, 3);
  AssertTrue(g.ContainsCycle(0, Cycle));
  AssertFalse(Cycle.IsEmpty);
end;

procedure TSimpleDiGraphTest.ContainsEulerianCycle;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.ContainsEulerianCycle);
  g.AddEdge(1, 2);
  AssertFalse(g.ContainsEulerianCycle);
  g.AddEdge(2, 1);
  AssertTrue(g.ContainsEulerianCycle);
  Ref.Instance := GenerateTestDigr1;
  g := Ref;
  AssertFalse(g.ContainsEulerianCycle);
end;

procedure TSimpleDiGraphTest.ContainsEulerianCycle1;
var
  Ref: TRef;
  g: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestDigr2;
  g := Ref;
  AssertTrue(g.ContainsEulerianCycle);
end;

procedure TSimpleDiGraphTest.FindEulerianCycle;
var
  Ref: TRef;
  g: TGraph;
  Cycle: TIntArray;
  I: Integer;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  Cycle := g.FindEulerianCycle;
  AssertTrue(Cycle.IsEmpty);
  Ref.Instance := GenerateTestDigr2;
  g := Ref;
  Cycle := g.FindEulerianCycle;
  AssertTrue(Cycle.Length = Succ(g.EdgeCount));
  AssertTrue(Cycle[0] = Cycle[Cycle.Length - 1]);
  for I := 0 to Cycle.Length - 2 do
    AssertTrue(g.ContainsEdgeI(Cycle[I], Cycle[Succ(I)]));
end;

procedure TSimpleDiGraphTest.IsStrongConnected;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.IsStrongConnected);
  Ref.Instance := GenerateTestDigr2;
  g := Ref;
  AssertTrue(g.IsStrongConnected);
  Ref.Instance := GenerateTestDigr1;
  g := Ref;
  AssertFalse(g.IsStrongConnected);
  g.AddEdges([0, 8, 1, 6, 5, 9, 10, 0, 12, 0]);
  AssertTrue(g.IsStrongConnected);
end;

procedure TSimpleDiGraphTest.FindStrongComponents;
var
  Ref: TRef;
  g: TGraph;
  Ids, DistIds: TIntArray;
begin
  g := {%H-}Ref;
  Ids := [0];
  AssertTrue(g.FindStrongComponents(Ids) = 0);
  AssertTrue(Ids.IsEmpty);
  Ref.Instance := GenerateTestDigr1;
  g := Ref;
  AssertTrue(g.FindStrongComponents(Ids) = g.VertexCount);
  AssertTrue(Ids.Length = g.VertexCount);
  DistIds := THelper.SelectDistinct(Ids);
  AssertTrue(DistIds.Length = g.VertexCount);
end;

procedure TSimpleDiGraphTest.FindStrongComponents1;
var
  Ref: TRef;
  g: TGraph;
  Ids: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr2;
  g := Ref;
  AssertTrue(g.FindStrongComponents(Ids) = 1);
  AssertTrue(Ids.Length = g.VertexCount);
  for I in Ids do
    AssertTrue(I = 0);
end;

procedure TSimpleDiGraphTest.FindStrongComponents2;
var
  Ref: TRef;
  g: TGraph;
  Ids: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  g.AddEdges([0, 8, 1, 6, 5, 9, 10, 0, 12, 0]);
  AssertTrue(g.FindStrongComponents(Ids) = 1);
  AssertTrue(Ids.Length = g.VertexCount);
  for I in Ids do
    AssertTrue(I = 0);
end;

procedure TSimpleDiGraphTest.GetStrongComponent;
var
  Ref: TRef;
  g: TGraph;
  c: TIntArray;
  I: SizeInt;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  try
    g.GetStrongComponent(0);
  except
    Raised := True;
  end;
  AssertTrue(Raised);
  Ref.Instance := GenerateTestDigr2;
  g := Ref;
  c := g.GetStrongComponent(3);
  AssertTrue(c.Length = g.VertexCount);
  for I := 0 to Pred(g.VertexCount) do
    AssertTrue(c[I] = I);
  Ref.Instance := GenerateTestDigr1;
  g := Ref;
  c := g.GetStrongComponent(3);
  AssertTrue(c.Length = 1);
  AssertTrue(c[0] = 3);
end;

procedure TSimpleDiGraphTest.BuildReachabilityMatrix;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.ReachabilityValid);
  g.BuildReachabilityMatrix;
  AssertFalse(g.ReachabilityValid);
  Ref.Instance := GenerateTestDigr1;
  g := Ref;
  AssertFalse(g.ReachabilityValid);
  g.BuildReachabilityMatrix;
  AssertTrue(g.ReachabilityValid);
  AssertTrue(g.PathExists(0, 12));
  AssertTrue(g.PathExists(0, 10));
  AssertFalse(g.PathExists(12, 0));
  AssertFalse(g.PathExists(10, 0));
  AssertFalse(g.PathExists(0, 7));
  AssertFalse(g.PathExists(0, 8));
  AssertFalse(g.PathExists(7, 0));
  AssertFalse(g.PathExists(8, 0));
  AssertTrue(g.PathExists(7, 12));
  AssertTrue(g.PathExists(7, 10));
end;

procedure TSimpleDiGraphTest.BuildReachabilityMatrix1;
var
  Ref: TRef;
  g: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  g.AddEdges([0, 8, 1, 6, 5, 9, 10, 0, 12, 0]);
  AssertFalse(g.ReachabilityValid);
  g.BuildReachabilityMatrix;
  AssertTrue(g.ReachabilityValid);
  AssertTrue(g.PathExists(0, 12));
  AssertTrue(g.PathExists(0, 10));
  AssertTrue(g.PathExists(12, 0));
  AssertTrue(g.PathExists(10, 0));
  AssertTrue(g.PathExists(0, 7));
  AssertTrue(g.PathExists(0, 8));
  AssertTrue(g.PathExists(7, 0));
  AssertTrue(g.PathExists(8, 0));
  AssertTrue(g.PathExists(7, 12));
  AssertTrue(g.PathExists(7, 10));
end;

procedure TSimpleDiGraphTest.BuildReachabilityMatrix2;
var
  Ref, Ref2: TRef;
  g, g2: TGraph;
  I, J: SizeInt;
const
  Size = 100;
  Deg  = 3;
begin
  repeat
    {%H-}Ref.Instance := GenerateRandomGraph(Size, Deg);
    g := Ref;
  until not g.IsStrongConnected;
  {%H-}Ref2.Instance := g.Clone;
  g2 := Ref2;
  g.BuildReachabilityMatrix;
  for I := 0 to Pred(g.VertexCount) do
    for J := 0 to Pred(g.VertexCount) do
      if I <> J then
        AssertFalse(g.PathExists(I, J) xor g2.PathExists(I, J));
end;

procedure TSimpleDiGraphTest.FindMetrics;
var
  Ref: TRef;
  g: TGraph;
  r, d: SizeInt;
begin
  g := {%H-}Ref;
  AssertFalse(g.FindMetrics(r, d));
  g.AddVertex(1);
  AssertTrue(g.FindMetrics(r, d));
  AssertTrue(r = 0);
  AssertTrue(d = 0);
  Ref.Instance := GenerateTestDigr1;
  g := Ref;
  AssertFalse(g.FindMetrics(r, d));
  g.AddEdges([0, 8, 1, 6, 5, 9, 10, 0, 12, 0]);
  AssertTrue(g.FindMetrics(r, d));
  AssertTrue(r = 3);
  AssertTrue(d = 7);
end;

procedure TSimpleDiGraphTest.FindCenter;
var
  Ref: TRef;
  g: TGraph;
  c: TIntArray;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  g.AddEdges([0, 8, 1, 6, 5, 9, 10, 0, 12, 0]);
  c := g.FindCenter;
  AssertTrue(c.Length = 1);
  AssertTrue(g.Eccentricity(c[0]) = 3);
  AssertTrue(c[0] = 0);
end;

procedure TSimpleDiGraphTest.FindPeripheral;
var
  Ref: TRef;
  g: TGraph;
  p: TIntArray;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  g.AddEdges([0, 8, 1, 6, 5, 9, 10, 0, 12, 0]);
  p := g.FindPeripheral;
  AssertTrue(p.Length = 1);
  AssertTrue(g.Eccentricity(p[0]) = 7);
  AssertTrue(p[0] = 2);
end;

procedure TSimpleDiGraphTest.IsDag;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertTrue(g.IsDag);
  Ref.Instance := GenerateTestDigr2;
  g := Ref;
  AssertFalse(g.IsDag);
  Ref.Instance := GenerateTestDigr1;
  g := Ref;
  AssertTrue(g.IsDag);
  g.AddEdge(0, 1);
  AssertTrue(g.IsDag);
  g.AddEdge(12, 3);
  AssertFalse(g.IsDag);
  g.RemoveEdge(12, 3);
  AssertTrue(g.IsDag);
end;

procedure TSimpleDiGraphTest.TopologicalSort;
var
  Ref: TRef;
  g: TGraph;
  Sorted: TIntArray;
begin
  g := {%H-}Ref;
  Sorted := g.TopologicalSort;
  AssertTrue(g.IsTopoSorted(Sorted, soAsc));
  Ref.Instance := GenerateTestDigr2;
  g := Ref;
  Sorted := g.TopologicalSort;
  AssertFalse(g.IsTopoSorted(Sorted, soAsc));
  Ref.Instance := GenerateTestDigr1;
  g := Ref;
  Sorted := g.TopologicalSort;
  AssertTrue(g.IsTopoSorted(Sorted, soAsc));
  Sorted := g.TopologicalSort(soDesc);
  AssertTrue(g.IsTopoSorted(Sorted, soDesc));
  g.AddEdge(12, 3);
  Sorted := g.TopologicalSort;
  AssertFalse(g.IsTopoSorted(Sorted, soAsc));
end;

procedure TSimpleDiGraphTest.DagLongestPathsMap;
var
  Ref: TRef;
  g: TGraph;
  Map: TIntArray;
begin
  g := {%H-}Ref;
  g.AddVertex(1);
  Map := g.DagLongestPathsMap(1);
  AssertTrue(Map.Length = 1);
  AssertTrue(Map[0] = 0);
  Ref.Instance := GenerateTestDigr1;
  g := Ref;
  Map := g.DagLongestPathsMap(0);
  AssertTrue(Map.Length = g.VertexCount);
  AssertTrue(Map[0] = 0);
  AssertTrue(Map[1] = 1);
  AssertTrue(Map[2] = 1);
  AssertTrue(Map[3] = 2);
  AssertTrue(Map[4] = 3);
  AssertTrue(Map[5] = 3);
  AssertTrue(Map[6] = 1);
  AssertTrue(Map[7] = -1);
  AssertTrue(Map[8] = -1);
  AssertTrue(Map[9] = 4);
  AssertTrue(Map[10] = 5);
  AssertTrue(Map[11] = 5);
  AssertTrue(Map[12] = 6);
end;

procedure TSimpleDiGraphTest.DagLongestPathsMap1;
var
  Ref: TRef;
  g: TGraph;
  Map: TIntArray;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  Map := g.DagLongestPathsMap(6);
  AssertTrue(Map.Length = g.VertexCount);
  AssertTrue(Map[0] = -1);
  AssertTrue(Map[1] = -1);
  AssertTrue(Map[2] = -1);
  AssertTrue(Map[3] = -1);
  AssertTrue(Map[4] = 1);
  AssertTrue(Map[5] = -1);
  AssertTrue(Map[6] = 0);
  AssertTrue(Map[7] = -1);
  AssertTrue(Map[8] = -1);
  AssertTrue(Map[9] = 2);
  AssertTrue(Map[10] = 3);
  AssertTrue(Map[11] = 3);
  AssertTrue(Map[12] = 4);
end;

procedure TSimpleDiGraphTest.DagLongestPaths;
var
  Ref: TRef;
  g: TGraph;
  Map: TIntArray;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  Map := g.DagLongestPaths;
  AssertTrue(Map.Length = g.VertexCount);
  AssertTrue(Map[0] = 6);
  AssertTrue(Map[1] = 0);
  AssertTrue(Map[2] = 5);
  AssertTrue(Map[3] = 4);
  AssertTrue(Map[4] = 3);
  AssertTrue(Map[5] = 0);
  AssertTrue(Map[6] = 4);
  AssertTrue(Map[7] = 5);
  AssertTrue(Map[8] = 6);
  AssertTrue(Map[9] = 2);
  AssertTrue(Map[10] = 0);
  AssertTrue(Map[11] = 1);
  AssertTrue(Map[12] = 0);
end;

procedure TSimpleDiGraphTest.FindHamiltonCycles;
var
  Ref: TRef;
  g: TGraph;
  Cycles: TIntArrayVector;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  AssertFalse(g.FindHamiltonCycles(1, 0, Cycles, 10));
end;

procedure TSimpleDiGraphTest.FindHamiltonCycles1;
var
  Ref: TRef;
  g: TGraph;
  Cycles: TIntArrayVector;
begin
  {%H-}Ref.Instance := GenerateTestDigr2;
  g := Ref;
  AssertFalse(g.FindHamiltonCycles(1, 0, Cycles, 10));
end;

procedure TSimpleDiGraphTest.FindHamiltonCycles2;
var
  Ref: TRef;
  g: TGraph;
  Cycles: TIntArrayVector;
begin
  g := {%H-}Ref;
  g.AddVertex(1);
  AssertFalse(g.FindHamiltonCycles(1, 0, Cycles, 10));
  Ref.Instance := GenerateTestDigr3;
  g := Ref;
  AssertTrue(g.FindHamiltonCycles(0, 0, Cycles, 10));
  AssertTrue(Cycles.Count = 2);
  AssertTrue(g.IsHamiltonCycle(Cycles[0], g.IndexOf(0)));
  AssertTrue(g.IsHamiltonCycle(Cycles[1], g.IndexOf(0)));
  AssertTrue(g.FindHamiltonCycles(4, 0, Cycles, 10));
  AssertTrue(Cycles.Count = 2);
  AssertTrue(g.IsHamiltonCycle(Cycles[0], g.IndexOf(4)));
  AssertTrue(g.IsHamiltonCycle(Cycles[1], g.IndexOf(4)));
  AssertTrue(g.FindHamiltonCycles(4, 1, Cycles, 10));
  AssertTrue(Cycles.Count = 1);
end;

procedure TSimpleDiGraphTest.FindHamiltonPaths;
var
  Ref: TRef;
  g: TGraph;
  Paths: TIntArrayVector;
begin
  g := {%H-}Ref;
  g.AddVertex(1);
  AssertFalse(g.FindHamiltonPaths(1, 0, Paths, 10));
  Ref.Instance := GenerateTestDigr3;
  g := Ref;
  AssertTrue(g.FindHamiltonPaths(0, 0, Paths, 10));
  AssertTrue(Paths.Count = 2);
  AssertTrue(g.IsHamiltonPath(Paths[0], g.IndexOf(0)));
  AssertTrue(g.IsHamiltonPath(Paths[1], g.IndexOf(0)));
end;

procedure TSimpleDiGraphTest.FindHamiltonPaths1;
var
  Ref: TRef;
  g: TGraph;
  Paths: TIntArrayVector;
begin
  {%H-}Ref.Instance := GenerateTestDigr3;
  g := Ref;
  g.RemoveEdge(5, 0);
  g.RemoveEdge(5, 1);
  g.RemoveEdge(2, 0);
  AssertTrue(g.FindHamiltonPaths(0, 0, Paths, 10));
  AssertTrue(Paths.Count = 1);
  AssertTrue(g.IsHamiltonPath(Paths[0], g.IndexOf(0)));
  AssertFalse(g.FindHamiltonPaths(1, 0, Paths, 10));
end;

{ TWeightedDigraphTest }

function TWeightedDigraphTest.GenerateTestWDigr1: TGraph;
var
  I: Integer;
begin
  Result := TGraph.Create; //TestWDigr1.png
  for I in [0..5] do
    Result.AddVertex(I);
  Result.AddEdge(0, 1, TIntWeight.Create(10));
  Result.AddEdge(1, 2, TIntWeight.Create(110));
  Result.AddEdge(1, 3, TIntWeight.Create(15));
  Result.AddEdge(1, 4, TIntWeight.Create(100));
  Result.AddEdge(3, 5, TIntWeight.Create(20));
  Result.AddEdge(5, 4, TIntWeight.Create(25));
  Result.AddEdge(4, 3, TIntWeight.Create(110));
  Result.AddEdge(4, 2, TIntWeight.Create(30));
  Result.AddEdge(2, 0, TIntWeight.Create(35));
end;

function TWeightedDigraphTest.GenerateTestWDigr2: TGraph;
var
  I: Integer;
begin
  Result := TGraph.Create; //TestWDigr2.png
  for I := 0 to 12 do
    Result.AddVertex(I);
  Result.AddEdge(0, 1, TIntWeight.Create(4));
  Result.AddEdge(0, 2, TIntWeight.Create(5));
  Result.AddEdge(0, 3, TIntWeight.Create(6));
  Result.AddEdge(0, 5, TIntWeight.Create(7));
  Result.AddEdge(0, 6, TIntWeight.Create(2));
  Result.AddEdge(2, 3, TIntWeight.Create(3));
  Result.AddEdge(3, 5, TIntWeight.Create(5));
  Result.AddEdge(3, 4, TIntWeight.Create(7));
  Result.AddEdge(6, 4, TIntWeight.Create(4));
  Result.AddEdge(4, 9, TIntWeight.Create(3));
  Result.AddEdge(6, 9, TIntWeight.Create(5));
  Result.AddEdge(7, 6, TIntWeight.Create(3));
  Result.AddEdge(8, 7, TIntWeight.Create(2));
  Result.AddEdge(9, 10, TIntWeight.Create(11));
  Result.AddEdge(9, 11, TIntWeight.Create(3));
  Result.AddEdge(9, 12, TIntWeight.Create(8));
  Result.AddEdge(11, 12, TIntWeight.Create(3));
end;

function TWeightedDigraphTest.GenerateTestWDigrBip1: TGraph64;
var
  I: Integer;
begin
  Result := TGraph64.Create;  // min match weight = 58, max match weight = 270
  for I := 1 to 12 do
    Result.AddVertex(I);
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

function TWeightedDigraphTest.GenerateTestWDigr3: TGraph64;
var
  I: Integer;
begin
  Result := TGraph64.Create; //TestWDigr3.png   source := 0; sink = 12; maxflow = 5 mincost = 11
  for I := 0 to 12 do
    Result.AddVertex(I);
  Result.AddEdge(0, 1, TInt64Weight.Create(1));
  Result.AddEdge(0, 2, TInt64Weight.Create(1));
  Result.AddEdge(0, 3, TInt64Weight.Create(1));
  Result.AddEdge(0, 4, TInt64Weight.Create(1));
  Result.AddEdge(0, 5, TInt64Weight.Create(1));
  Result.AddEdge(1, 6, TInt64Weight.Create(1));
  Result.AddEdge(1, 7, TInt64Weight.Create(1));
  Result.AddEdge(1, 8, TInt64Weight.Create(1));
  Result.AddEdge(2, 6, TInt64Weight.Create(1));
  Result.AddEdge(2, 7, TInt64Weight.Create(1));
  Result.AddEdge(3, 6, TInt64Weight.Create(1));
  Result.AddEdge(4, 6, TInt64Weight.Create(1));
  Result.AddEdge(4, 7, TInt64Weight.Create(1));
  Result.AddEdge(5, 6, TInt64Weight.Create(1));
  Result.AddEdge(5, 7, TInt64Weight.Create(1));
  Result.AddEdge(5, 8, TInt64Weight.Create(1));
  Result.AddEdge(6, 9, TInt64Weight.Create(1));
  Result.AddEdge(7, 10, TInt64Weight.Create(2));
  Result.AddEdge(8, 11, TInt64Weight.Create(3));
  Result.AddEdge(9, 12, TInt64Weight.Create(100));
  Result.AddEdge(10, 12, TInt64Weight.Create(100));
  Result.AddEdge(11, 12, TInt64Weight.Create(100));
end;

function TWeightedDigraphTest.GenerateTestWDigr4: TGraph64;
var
  I: Integer;
begin
  Result := TGraph64.Create; //TestWDigr4.png   source := 0; sink = 6; maxflow = 40 - mincost = 1296
  for I := 0 to 6 do         //                                        maxflow = 20 - mincost = 474
    Result.AddVertex(I);     // from Christofides
  Result.AddEdge(0, 1, TInt64Weight.Create(16));
  Result.AddEdge(0, 2, TInt64Weight.Create(11));
  Result.AddEdge(0, 3, TInt64Weight.Create(13));
  Result.AddEdge(1, 2, TInt64Weight.Create(18));
  Result.AddEdge(1, 4, TInt64Weight.Create(17));
  Result.AddEdge(1, 5, TInt64Weight.Create(16));
  Result.AddEdge(2, 3, TInt64Weight.Create(19));
  Result.AddEdge(2, 4, TInt64Weight.Create(12));
  Result.AddEdge(3, 5, TInt64Weight.Create(10));
  Result.AddEdge(3, 6, TInt64Weight.Create(5));
  Result.AddEdge(4, 6, TInt64Weight.Create(22));
  Result.AddEdge(5, 6, TInt64Weight.Create(16));
end;

function TWeightedDigraphTest.GenerateCostFun3: TCostEdgeArray;
begin
  Result := [
    TCostEdge.Create(0, 1, 0),  TCostEdge.Create(0, 2, 0),  TCostEdge.Create(0, 3, 0),  TCostEdge.Create(0, 4, 0),
    TCostEdge.Create(0, 5, 0),  TCostEdge.Create(1, 6, 1),  TCostEdge.Create(1, 7, 2),  TCostEdge.Create(1, 8,  3),
    TCostEdge.Create(2, 6, 1),  TCostEdge.Create(2, 7, 2),  TCostEdge.Create(3, 6, 1),  TCostEdge.Create(4, 6,  1),
    TCostEdge.Create(4, 7, 2),  TCostEdge.Create(5, 6, 1),  TCostEdge.Create(5, 7, 2),  TCostEdge.Create(5, 8, 3),
    TCostEdge.Create(6, 9, 0),  TCostEdge.Create(7, 10, 0), TCostEdge.Create(8, 11, 0), TCostEdge.Create(9, 12, 0),
    TCostEdge.Create(10,12, 0), TCostEdge.Create(11, 12, 0)];
end;

function TWeightedDigraphTest.GenerateCostFun4: TCostEdgeArray;
begin
  Result := [TCostEdge.Create(0, 1, 7), TCostEdge.Create(0, 2, 13), TCostEdge.Create(0, 3, 28),
             TCostEdge.Create(1, 2, 4), TCostEdge.Create(1, 4, 25), TCostEdge.Create(1, 5, 10),
             TCostEdge.Create(2, 3, 5), TCostEdge.Create(2, 4, 6), TCostEdge.Create(3, 5, 3),
             TCostEdge.Create(3, 6, 7), TCostEdge.Create(4, 6, 5), TCostEdge.Create(5, 6, 12)];
end;

procedure TWeightedDigraphTest.ContainsNegWeightEdge;
var
  Ref: TRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.ContainsNegWeightEdge);
  Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  AssertFalse(g.ContainsNegWeightEdge);
  g.SetEdgeData(4, 3, TIntWeight.Create(-1));
  AssertTrue(g.ContainsNegWeightEdge);
end;

procedure TWeightedDigraphTest.ContainsNegCycle;
var
  Ref: TRef;
  g: TGraph;
  c: TIntArray;
  I: SizeInt;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  try
    g.ContainsNegCycle(0, c)
  except
    Raised := True;
  end;
  AssertTrue(Raised);
  Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  c := [1];
  AssertFalse(g.ContainsNegCycle(0, c));
  AssertTrue(c = nil);
  g.SetEdgeData(4, 3, TIntWeight.Create(-30));
  AssertFalse(g.ContainsNegCycle(0, c));
  g.SetEdgeData(4, 3, TIntWeight.Create(-50));
  AssertTrue(g.ContainsNegCycle(0, c));
  AssertTrue(c.Length = 4);
  for I in [3..5] do
    AssertTrue(TSearch.SequentSearch(c, I) <> NULL_INDEX);
end;

procedure TWeightedDigraphTest.MinPathsMap;
var
  Ref: TRef;
  g: TGraph;
  m: TGraph.TWeightArray;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  try
    g.MinPathsMap(0);
  except
    Raised := True;
  end;
  AssertTrue(Raised);
  Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  m := g.MinPathsMap(0);
  AssertTrue(Length(m) = g.VertexCount);
  AssertTrue(THelper.Same(m, [0, 10, 100, 25, 70, 45]));
  g.SetEdgeData(1, 4, TIntWeight.Create(5));
  g.SetEdgeData(4, 3, TIntWeight.Create(5));
  m := g.MinPathsMap(0);
  AssertTrue(THelper.Same(m, [0, 10, 45, 20, 15, 40]));
end;

procedure TWeightedDigraphTest.MinPathsMap1;
var
  Ref: TRef;
  g: TGraph;
  m: TGraph.TWeightArray;
  p: TIntArray;
begin
  {%H-}Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  m := g.MinPathsMap(0, p);
  AssertTrue(THelper.Same(m, [0, 10, 100, 25, 70, 45]));
  AssertTrue(p.Length = g.VertexCount);
  AssertTrue(TSearch.Same(p, [-1, 0, 4, 1, 5, 3]));
  g.SetEdgeData(1, 4, TIntWeight.Create(5));
  g.SetEdgeData(4, 3, TIntWeight.Create(5));
  m := g.MinPathsMap(0, p);
  AssertTrue(THelper.Same(m, [0, 10, 45, 20, 15, 40]));
  AssertTrue(TSearch.Same(p, [-1, 0, 4, 4, 1, 3]));
end;

procedure TWeightedDigraphTest.FindMinPathsMap;
var
  Ref: TRef;
  g: TGraph;
  m: TGraph.TWeightArray;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  try
    g.FindMinPathsMap(0, m);
  except
    Raised := True;
  end;
  AssertTrue(Raised);
  Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  AssertTrue(g.FindMinPathsMap(0, m));
  AssertTrue(Length(m) = g.VertexCount);
  AssertTrue(THelper.Same(m, [0, 10, 100, 25, 70, 45]));
  g.SetEdgeData(1, 4, TIntWeight.Create(5));
  g.SetEdgeData(4, 3, TIntWeight.Create(5));
  AssertTrue(g.FindMinPathsMap(0, m));
  AssertTrue(THelper.Same(m, [0, 10, 45, 20, 15, 40]));
end;

procedure TWeightedDigraphTest.FindMinPathsMap1;
var
  Ref: TRef;
  g: TGraph;
  m: TGraph.TWeightArray;
  p: TIntArray;
begin
  {%H-}Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  AssertTrue(g.FindMinPathsMap(0, p, m));
  AssertTrue(THelper.Same(m, [0, 10, 100, 25, 70, 45]));
  AssertTrue(p.Length = g.VertexCount);
  AssertTrue(TSearch.Same(p, [-1, 0, 4, 1, 5, 3]));
  g.SetEdgeData(1, 4, TIntWeight.Create(5));
  g.SetEdgeData(4, 3, TIntWeight.Create(5));
  AssertTrue(g.FindMinPathsMap(0, p, m));
  AssertTrue(THelper.Same(m, [0, 10, 45, 20, 15, 40]));
  AssertTrue(TSearch.Same(p, [-1, 0, 4, 4, 1, 3]));
end;

procedure TWeightedDigraphTest.FindMinPathsMap3;
var
  Ref: TRef;
  g: TGraph;
  m: TGraph.TWeightArray;
  p: TIntArray;
begin
  {%H-}Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  g.SetEdgeData(1, 4, TIntWeight.Create(-5));
  g.SetEdgeData(4, 3, TIntWeight.Create(-5));
  AssertTrue(g.FindMinPathsMap(0, p, m));
  AssertTrue(THelper.Same(m, [0, 10, 35, 0, 5, 20]));
  AssertTrue(TSearch.Same(p, [-1, 0, 4, 4, 1, 3]));
end;

procedure TWeightedDigraphTest.FindMinPathsMap4;
var
  Ref: TRef;
  g: TGraph;
  m: TGraph.TWeightArray;
  p: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  g.SetEdgeData(1, 4, TIntWeight.Create(5));
  g.SetEdgeData(4, 3, TIntWeight.Create(-50));
  AssertFalse(g.FindMinPathsMap(0, p, m));
  AssertTrue(m = nil);
  AssertTrue(p.Length = 4);
  for I in [3..5] do
    AssertTrue(TSearch.SequentSearch(p, I) <> NULL_INDEX);
end;

procedure TWeightedDigraphTest.MinPath;
var
  Ref: TRef;
  g: TGraph;
  p: TIntArray;
  w: Integer;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  try
    g.MinPath(0, 1, w);
  except
    Raised := True;
  end;
  AssertTrue(Raised);
  g.AddVertex(1);
  g.AddVertex(2);
  p := g.MinPath(1, 2, w);
  AssertTrue(w = g.InfWeight);
  AssertTrue(p.IsEmpty);
end;

procedure TWeightedDigraphTest.MinPath1;
var
  Ref: TRef;
  g: TGraph;
  p: TIntArray;
  I: SizeInt;
  w: Integer;
begin
  {%H-}Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  p := g.MinPath(0, 5, w);
  AssertTrue(w = 45);
  AssertTrue(p.Length = 4);
  for I in [0, 1, 3, 5] do
    AssertTrue(TSearch.SequentSearch(p, I) <> NULL_INDEX);
  g.SetEdgeData(1, 4, TIntWeight.Create(2));
  g.SetEdgeData(4, 3, TIntWeight.Create(3));
  p := g.MinPath(0, 5, w);
  AssertTrue(w = 35);
  AssertTrue(p.Length = 5);
  for I in [0, 1, 3, 4, 5] do
    AssertTrue(TSearch.SequentSearch(p, I) <> NULL_INDEX);
end;

procedure TWeightedDigraphTest.FindMinPath;
var
  Ref: TRef;
  g: TGraph;
  p: TIntArray;
  w: Integer;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  try
    g.FindMinPath(0, 1, p, w);
  except
    Raised := True;
  end;
  AssertTrue(Raised);
  g.AddVertex(1);
  AssertTrue(g.FindMinPath(1, 1, p, w));
  AssertTrue(w = 0);
  AssertTrue(p.IsEmpty);
  g.AddVertex(2);
  AssertFalse(g.FindMinPath(1, 2, p, w));
  AssertTrue(w = g.InfWeight);
  AssertTrue(p.IsEmpty);
end;

procedure TWeightedDigraphTest.FindMinPath1;
var
  Ref: TRef;
  g: TGraph;
  p: TIntArray;
  I: SizeInt;
  w: Integer;
begin
  {%H-}Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  AssertTrue(g.FindMinPath(0, 5, p, w));
  AssertTrue(w = 45);
  AssertTrue(p.Length = 4);
  for I in [0, 1, 3, 5] do
    AssertTrue(TSearch.SequentSearch(p, I) <> NULL_INDEX);
  g.SetEdgeData(1, 4, TIntWeight.Create(2));
  g.SetEdgeData(4, 3, TIntWeight.Create(3));
  AssertTrue(g.FindMinPath(0, 5, p, w));
  AssertTrue(w = 35);
  AssertTrue(p.Length = 5);
  for I in [0, 1, 3, 4, 5] do
    AssertTrue(TSearch.SequentSearch(p, I) <> NULL_INDEX);
end;

procedure TWeightedDigraphTest.FindMinPath2;
var
  Ref: TRef;
  g: TGraph;
  p: TIntArray;
  I: SizeInt;
  w: Integer;
begin
  {%H-}Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  g.SetEdgeData(1, 4, TIntWeight.Create(-10));
  g.SetEdgeData(4, 3, TIntWeight.Create(-10));
  AssertTrue(g.FindMinPath(0, 5, p, w));
  AssertTrue(w = 10);
  AssertTrue(p.Length = 5);
  for I in [0, 1, 3, 4, 5] do
    AssertTrue(TSearch.SequentSearch(p, I) <> NULL_INDEX);
end;

procedure TWeightedDigraphTest.FindMinPath3;
var
  Ref: TRef;
  g: TGraph;
  p: TIntArray;
  I: SizeInt;
  w: Integer;
begin
  {%H-}Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  g.SetEdgeData(1, 4, TIntWeight.Create(-100));
  AssertFalse(g.FindMinPath(0, 5, p, w));
  AssertTrue(w = 0);
  AssertTrue(p.Length = 5);
  for I in [0, 1, 2, 4] do
    AssertTrue(TSearch.SequentSearch(p, I) <> NULL_INDEX);
end;

procedure TWeightedDigraphTest.FindAllPairMinPaths;
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

procedure TWeightedDigraphTest.FindAllPairMinPaths1;
var
  Ref: TRef;
  g: TGraph;
  m: TGraph.TApspMatrix;
  Map: TGraph.TWeightArray;
  Path, Path2: TIntArray;
  I, J: Integer;
begin
  {%H-}Ref.Instance := GenerateTestWDigr2;
  g := Ref;
  AssertTrue(g.FindAllPairMinPaths(m));
  AssertTrue(Length(m) = g.VertexCount);
  for I := 0 to Pred(g.VertexCount) do
    begin
      AssertTrue(Length(m[I]) = g.VertexCount);
      Map := g.MinPathsMap(I);
      for J := 0 to Pred(g.VertexCount) do
        AssertTrue(m[I, J].Weight = Map[J]);
    end;
  for I := 1 to Pred(g.VertexCount) do
    begin
      Path := g.ExtractMinPath(0, I, m);
      Path2 := g.MinPath(0, I, J);
      AssertTrue(TSearch.Same(Path, Path2));
    end;
end;

procedure TWeightedDigraphTest.FindAllPairMinPaths2;
var
  Ref: TRef;
  g: TGraph;
  m: TGraph.TApspMatrix;
begin
  {%H-}Ref.Instance := GenerateTestWDigr2;
  g := Ref;
  g.AddEdge(12, 0, TIntWeight.Create(-20));
  AssertFalse(g.FindAllPairMinPaths(m));
  AssertTrue(Length(m) = 1);
  AssertTrue(Length(m[0]) = 1);
end;

procedure TWeightedDigraphTest.FindEccentricity;
var
  Ref: TRef;
  g: TGraph;
  Ecc: Integer;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  try
    g.FindEccentricity(0, Ecc)
  except
    Raised := True;
  end;
  AssertTrue(Raised);
  g.AddVertex(1);
  Ecc := 5;
  AssertTrue(g.FindEccentricity(1, Ecc));
  AssertTrue(Ecc = 0);
  Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  g.SetEdgeData(1, 4, TIntWeight.Create(-80));
  Ecc := 5;
  AssertFalse(g.FindEccentricity(0, Ecc));
  AssertTrue(Ecc = 0);
end;

procedure TWeightedDigraphTest.FindEccentricity1;
var
  Ref: TRef;
  g: TGraph;
  I, Ecc: Integer;
const
  ECCENTR: array[0..5] of Integer = (100, 125, 105, 120, 110, 115);
begin
  {%H-}Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  for I := 0 to Pred(g.VertexCount) do
    begin
      AssertTrue(g.FindEccentricity(I, Ecc));
      AssertTrue(Ecc = ECCENTR[I]);
    end;
end;

procedure TWeightedDigraphTest.FindWeightedMetrics;
var
  Ref: TRef;
  g: TGraph;
  r, d: Integer;
begin
  g := {%H-}Ref;
  AssertFalse(g.FindWeightedMetrics(r, d));
  Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  g.SetEdgeData(1, 4, TIntWeight.Create(-80));
  AssertFalse(g.FindWeightedMetrics(r, d));
end;

procedure TWeightedDigraphTest.FindWeightedMetrics1;
var
  Ref: TRef;
  g: TGraph;
  r, d: Integer;
begin
  {%H-}Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  AssertTrue(g.FindWeightedMetrics(r, d));
  AssertTrue(r = 100);
  AssertTrue(d = 125);
end;

procedure TWeightedDigraphTest.FindWeightedCenter;
var
  Ref: TRef;
  g: TGraph;
  c: TIntArray;
begin
  g := {%H-}Ref;
  AssertFalse(g.FindWeightedCenter(c));
  Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  AssertTrue(g.FindWeightedCenter(c));
  AssertTrue(c.Length = 1);
  AssertTrue(c[0] = 0);
  g.SetEdgeData(1, 4, TIntWeight.Create(-80));
  AssertFalse(g.FindWeightedCenter(c));
end;

procedure TWeightedDigraphTest.DagMinPathsMap;
var
  Ref: TRef;
  g: TGraph;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  try
    g.DagMinPathsMap(0)
  except
    Raised := True;
  end;
  AssertTrue(Raised);
  Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  AssertTrue(g.DagMinPathsMap(0) = nil);
end;

procedure TWeightedDigraphTest.DagMinPathsMap1;
var
  Ref: TRef;
  g: TGraph;
  Map: TGraph.TWeightArray;
  Path: TIntArray;
begin
  {%H-}Ref.Instance := GenerateTestWDigr2;
  g := Ref;
  Map := g.DagMinPathsMap(0, Path);
  AssertTrue(Length(Map) = g.VertexCount);
  AssertTrue(Path.Length = g.VertexCount);
  AssertTrue(THelper.Same(Map, [0, 4, 5, 6, 6, 7, 2, MaxInt, MaxInt, 7, 18, 10, 13]));
  AssertTrue(TSearch.Same(Path, [-1, 0, 0, 0, 6, 0, 0, -1, -1, 6, 9, 9, 11]));
end;

procedure TWeightedDigraphTest.FindDagAllPairMinPaths;
var
  Ref: TRef;
  g: TGraph;
  m: TGraph.TApspMatrix;
begin
  g := {%H-}Ref;
  m := [[TGraph.TApspCell.Create(0, -1)]];
  AssertFalse(g.FindDagAllPairMinPaths(m));
  AssertTrue(m = nil);
end;

procedure TWeightedDigraphTest.FindDagAllPairMinPaths1;
var
  Ref: TRef;
  g: TGraph;
  m: TGraph.TApspMatrix;
begin
  {%H-}Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  m := [[TGraph.TApspCell.Create(0, -1)]];
  AssertFalse(g.FindDagAllPairMinPaths(m));
  AssertTrue(m = nil);
end;

procedure TWeightedDigraphTest.FindDagAllPairMinPaths2;
var
  Ref: TRef;
  g: TGraph;
  m: TGraph.TApspMatrix;
  Map: TGraph.TWeightArray;
  Path, Path2: TIntArray;
  I, J: Integer;
begin
  {%H-}Ref.Instance := GenerateTestWDigr2;
  g := Ref;
  AssertTrue(g.FindDagAllPairMinPaths(m));
  AssertTrue(Length(m) = g.VertexCount);
  for I := 0 to Pred(g.VertexCount) do
    begin
      AssertTrue(Length(m[I]) = g.VertexCount);
      Map := g.MinPathsMap(I);
      for J := 0 to Pred(g.VertexCount) do
        AssertTrue(m[I, J].Weight = Map[J]);
    end;
  for I := 1 to Pred(g.VertexCount) do
    begin
      Path := g.ExtractMinPath(0, I, m);
      Path2 := g.MinPath(0, I, J);
      AssertTrue(TSearch.Same(Path, Path2));
    end;
end;

procedure TWeightedDigraphTest.DagMaxPathsMap;
var
  Ref: TRef;
  g: TGraph;
  Map: TGraph.TWeightArray;
  Path: TIntArray;
begin
  {%H-}Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  Map := g.DagMaxPathsMap(0, Path);
  AssertTrue(Map = nil);
  AssertTrue(Path <> nil);
end;

procedure TWeightedDigraphTest.DagMaxPathsMap1;
var
  Ref: TRef;
  g: TGraph;
  Map: TGraph.TWeightArray;
  Path: TIntArray;
begin
  {%H-}Ref.Instance := GenerateTestWDigr2;
  g := Ref;
  Map := g.DagMaxPathsMap(0, Path);
  AssertTrue(Length(Map) = g.VertexCount);
  AssertTrue(Path.Length = g.VertexCount);
  AssertTrue(THelper.Same(Map, [0, 4, 5, 8, 15, 13, 2, LongInt.NEGINF_VALUE, LongInt.NEGINF_VALUE, 18, 29, 21, 26]));
  AssertTrue(TSearch.Same(Path, [-1, 0, 0, 2, 3, 3, 0, -1, -1, 4, 9, 9, 9]));
end;

procedure TWeightedDigraphTest.DagMaxPaths;
var
  Ref: TRef;
  g: TGraph;
  Map: TGraph.TWeightArray;
begin
  {%H-}Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  Map := g.DagMaxPaths;
  AssertTrue(Map = nil);
end;

procedure TWeightedDigraphTest.DagMaxPaths1;
var
  Ref: TRef;
  g: TGraph;
  Map: TGraph.TWeightArray;
begin
  {%H-}Ref.Instance := GenerateTestWDigr2;
  g := Ref;
  Map := g.DagMaxPaths;
  AssertTrue(Length(Map) = g.VertexCount);
  AssertTrue(THelper.Same(Map, [29, 0, 24, 21, 14, 0, 18, 21, 23, 11, 0, 3, 0]));
end;

procedure TWeightedDigraphTest.FindMinWeightBipMatch;
var
  Ref: TRef64;
  g: TGraph64;
  m: TGraph.TEdgeArray;
begin
  g := {%H-}Ref;
  AssertFalse(g.FindMinWeightBipMatch(m));
  g.AddVertex(1);
  AssertFalse(g.FindMinWeightBipMatch(m));
  g.AddVertex(2);
  AssertTrue(g.FindMinWeightBipMatch(m));
  AssertTrue(m = nil);
  Ref.Instance := GenerateTestWDigr4;
  g := Ref;
  AssertFalse(g.FindMinWeightBipMatch(m));
end;

procedure TWeightedDigraphTest.FindMinWeightBipMatch1;
var
  Ref: TRef64;
  g: TGraph64;
  m: TGraph.TEdgeArray;
  im: TIntEdgeArray;
begin
  {%H-}Ref.Instance := GenerateTestWDigrBip1;
  g := Ref;
  AssertTrue(g.FindMinWeightBipMatch(m));
  im := g.EdgeArray2IntEdgeArray(m);
  AssertTrue(g.IsMaxMatching(im));
  AssertTrue(g.IsPerfectMatching(im));
  AssertTrue(g.TotalWeight(m) = 58);
end;

procedure TWeightedDigraphTest.FindMaxWeightBipMatch;
var
  Ref: TRef64;
  g: TGraph64;
  m: TGraph.TEdgeArray;
begin
  g := {%H-}Ref;
  AssertFalse(g.FindMaxWeightBipMatch(m));
  g.AddVertex(1);
  AssertFalse(g.FindMaxWeightBipMatch(m));
  g.AddVertex(2);
  AssertTrue(g.FindMaxWeightBipMatch(m));
  AssertTrue(m = nil);
  Ref.Instance := GenerateTestWDigr4;
  g := Ref;
  AssertFalse(g.FindMaxWeightBipMatch(m));
end;

procedure TWeightedDigraphTest.FindMaxWeightBipMatch1;
var
  Ref: TRef64;
  g: TGraph64;
  m: TGraph.TEdgeArray;
  im: TIntEdgeArray;
begin
  {%H-}Ref.Instance := GenerateTestWDigrBip1;
  g := Ref;
  AssertTrue(g.FindMaxWeightBipMatch(m));
  im := g.EdgeArray2IntEdgeArray(m);
  AssertTrue(g.IsMaxMatching(im));
  AssertTrue(g.IsPerfectMatching(im));
  AssertTrue(g.TotalWeight(m) = 270);
end;

procedure TWeightedDigraphTest.FindMaxFlowPr;
var
  Ref: TRef64;
  g: TGraph64;
  Flow: Int64;
begin
  g := {%H-}Ref;
  g.AddVertex(1);
  g.AddVertex(2);
  AssertTrue(g.FindMaxFlowPr(1, 2, Flow) = nsInvalidSource);
  g.AddEdge(1, 2, TInt64Weight.Create(1));
  g.AddVertex(3);
  AssertTrue(g.FindMaxFlowPr(1, 3, Flow) = nsInvalidSink);
  g.AddEdge(2, 3, TInt64Weight.Create(-2));
  AssertTrue(g.FindMaxFlowPr(1, 3, Flow) = nsNegCapacity);
  g.SetEdgeData(2, 3, TInt64Weight.Create(0));
  AssertTrue(g.FindMaxFlowPr(1, 3, Flow) = nsSinkUnreachable);
end;

procedure TWeightedDigraphTest.FindMaxFlowPr1;
var
  Ref: TRef64;
  g: TGraph64;
  ArcFlows: TGraph64.TEdgeArray;
  Flow: Int64;
begin
  {%H-}Ref.Instance := GenerateTestWDigr3;
  g := Ref;
  AssertTrue(g.FindMaxFlowPr(0, 12, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 5);
  AssertTrue(g.IsFeasibleFlow(0, 12, Flow, ArcFlows));
end;

procedure TWeightedDigraphTest.FindMaxFlowPr2;
var
  Ref: TRef64;
  g: TGraph64;
  ArcFlows: TGraph64.TEdgeArray;
  Flow: Int64;
begin
  {%H-}Ref.Instance := GenerateTestWDigr4;
  g := Ref;
  AssertTrue(g.FindMaxFlowPr(0, 6, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 40);
  AssertTrue(g.IsFeasibleFlow(0, 6, Flow, ArcFlows));
end;

procedure TWeightedDigraphTest.FindMaxFlowD;
var
  Ref: TRef64;
  g: TGraph64;
  ArcFlows: TGraph64.TEdgeArray;
  Flow: Int64;
begin
  {%H-}Ref.Instance := GenerateTestWDigr3;
  g := Ref;
  AssertTrue(g.FindMaxFlowD(0, 12, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 5);
  AssertTrue(g.IsFeasibleFlow(0, 12, Flow, ArcFlows));
end;

procedure TWeightedDigraphTest.FindMaxFlowD1;
var
  Ref: TRef64;
  g: TGraph64;
  ArcFlows: TGraph64.TEdgeArray;
  Flow: Int64;
begin
  {%H-}Ref.Instance := GenerateTestWDigr4;
  g := Ref;
  AssertTrue(g.FindMaxFlowD(0, 6, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 40);
  AssertTrue(g.IsFeasibleFlow(0, 6, Flow, ArcFlows));
end;

procedure TWeightedDigraphTest.FindMinSTCutPr;
var
  Ref: TRef64;
  g: TGraph64;
  Cut: TGraph64.TStCut;
  Value: Int64;
begin
  {%H-}Ref.Instance := GenerateTestWDigr3;
  g := Ref;
  AssertTrue(g.FindMinSTCutPr(0, 12, Value, Cut) = nsOk);
  AssertTrue(Value = 5);
  AssertTrue(Cut.S.Length + Cut.T.Length = g.VertexCount);
  if Cut.S.Length = 1 then
    begin
      AssertTrue(Cut.T.Length = 12);
      AssertTrue(Cut.S[0] = 0);
    end
  else
    begin
      AssertTrue(Cut.S.Length = 11);
      AssertTrue(Cut.T.Length = 1);
      AssertTrue(Cut.T[0] = 0);
    end;
end;

procedure TWeightedDigraphTest.FindMinSTCutPr1;
var
  Ref: TRef64;
  g: TGraph64;
  Cut: TGraph64.TStCut;
  Value: Int64;
begin
  {%H-}Ref.Instance := GenerateTestWDigr4;
  g := Ref;
  AssertTrue(g.FindMinSTCutPr(0, 6, Value, Cut) = nsOk);
  AssertTrue(Value = 40);
  AssertTrue(Cut.S.Length + Cut.T.Length = g.VertexCount);
  if Cut.S.Length = 1 then
    begin
      AssertTrue(Cut.T.Length = 6);
      AssertTrue(Cut.S[0] = 0);
    end
  else
    begin
      AssertTrue(Cut.S.Length = 6);
      AssertTrue(Cut.T.Length = 1);
      AssertTrue(Cut.T[0] = 0);
    end;
end;

procedure TWeightedDigraphTest.FindMinSTCutD;
var
  Ref: TRef64;
  g: TGraph64;
  Cut: TGraph64.TStCut;
  Value: Int64;
begin
  {%H-}Ref.Instance := GenerateTestWDigr3;
  g := Ref;
  AssertTrue(g.FindMinSTCutD(0, 12, Value, Cut) = nsOk);
  AssertTrue(Value = 5);
  AssertTrue(Cut.S.Length + Cut.T.Length = g.VertexCount);
  if Cut.S.Length = 1 then
    begin
      AssertTrue(Cut.T.Length = 12);
      AssertTrue(Cut.S[0] = 0);
    end
  else
    begin
      AssertTrue(Cut.S.Length = 11);
      AssertTrue(Cut.T.Length = 1);
      AssertTrue(Cut.T[0] = 0);
    end;
end;

procedure TWeightedDigraphTest.FindMinSTCutD1;
var
  Ref: TRef64;
  g: TGraph64;
  Cut: TGraph64.TStCut;
  Value: Int64;
begin
  {%H-}Ref.Instance := GenerateTestWDigr4;
  g := Ref;
  AssertTrue(g.FindMinSTCutD(0, 6, Value, Cut) = nsOk);
  AssertTrue(Value = 40);
  AssertTrue(Cut.S.Length + Cut.T.Length = g.VertexCount);
  if Cut.S.Length = 1 then
    begin
      AssertTrue(Cut.T.Length = 6);
      AssertTrue(Cut.S[0] = 0);
    end
  else
    begin
      AssertTrue(Cut.S.Length = 6);
      AssertTrue(Cut.T.Length = 1);
      AssertTrue(Cut.T[0] = 0);
    end;
end;

procedure TWeightedDigraphTest.FindMinCostFlowSsp;
var
  Ref: TRef64;
  g: TGraph64;
  Flow: Int64;
  Cost: TCost;
  CostFunc: TCostEdgeArray;
begin
  g := {%H-}Ref;
  g.AddVertex(0);
  g.AddVertex(1);
  Flow := -2;
  AssertTrue(g.FindMinCostFlowSsp(0, 1, nil, Flow, Cost) = mcfNoFlowRequired);
  Flow := 2;
  AssertTrue(g.FindMinCostFlowSsp(0, 1, nil, Flow, Cost) = mcfInvalidNet);
  g.AddEdge(0, 1, TInt64Weight.Create(4));
  Flow := 2;
  AssertTrue(g.FindMinCostFlowSsp(0, 1, nil, Flow, Cost) = mcfInvalidCost);
  g.AddVertex(2);
  g.AddVertex(3);
  g.AddEdge(1, 2, TInt64Weight.Create(4));
  g.AddEdge(2, 1, TInt64Weight.Create(4));
  g.AddEdge(2, 3, TInt64Weight.Create(4));
  CostFunc :=
    [TCostEdge.Create(0, 1, 3), TCostEdge.Create(1, 2, -7), TCostEdge.Create(2, 1, -8), TCostEdge.Create(2, 3, 3)];
  Flow := 4;
  AssertTrue(g.FindMinCostFlowSsp(0, 3, CostFunc, Flow, Cost) = mcfNegCycle);
end;

procedure TWeightedDigraphTest.FindMinCostFlowSsp1;
var
  Ref: TRef64;
  g: TGraph64;
  CostFunc: TCostEdgeArray;
  Flows: TGraph64.TEdgeArray;
  Flow: Int64;
  Cost: TCost;
begin
  {%H-}Ref.Instance := GenerateTestWDigr3;
  g := Ref;
  CostFunc := GenerateCostFun3;
  Flow := 1000;
  AssertTrue(g.FindMinCostFlowSsp(0, 12, CostFunc, Flow, Cost, Flows) = mcfOk);
  AssertTrue(Flow = 5);
  AssertTrue(Cost = 11);
  AssertTrue(g.IsMcfFeasible(0, 12, CostFunc, Flows, Flow, Cost));
end;

procedure TWeightedDigraphTest.FindMinCostFlowSsp2;
var
  Ref: TRef64;
  g: TGraph64;
  CostFunc: TCostEdgeArray;
  Flows: TGraph64.TEdgeArray;
  Flow: Int64;
  Cost: TCost;
begin
  {%H-}Ref.Instance := GenerateTestWDigr4;
  g := Ref;
  CostFunc := GenerateCostFun4;
  Flow := 1000;
  AssertTrue(g.FindMinCostFlowSsp(0, 6, CostFunc, Flow, Cost, Flows) = mcfOk);
  AssertTrue(Flow = 40);
  AssertTrue(Cost = 1296);
  AssertTrue(g.IsMcfFeasible(0, 6, CostFunc, Flows, Flow, Cost));
end;

procedure TWeightedDigraphTest.FindMinCostFlowSsp3;
var
  Ref: TRef64;
  g: TGraph64;
  CostFunc: TCostEdgeArray;
  Flows: TGraph64.TEdgeArray;
  Flow: Int64;
  Cost: TCost;
begin
  {%H-}Ref.Instance := GenerateTestWDigr4;
  g := Ref;
  CostFunc := GenerateCostFun4;
  Flow := 20;
  AssertTrue(g.FindMinCostFlowSsp(0, 6, CostFunc, Flow, Cost, Flows) = mcfOk);
  AssertTrue(Flow = 20);
  AssertTrue(Cost = 474);
  AssertTrue(g.IsMcfFeasible(0, 6, CostFunc, Flows, Flow, Cost));
end;

procedure TWeightedDigraphTest.FindMinCostFlowCs;
var
  Ref: TRef64;
  g: TGraph64;
  Flow: Int64;
  Cost: TCost;
  CostFunc: TCostEdgeArray;
begin
  g := {%H-}Ref;
  g.AddVertex(0);
  g.AddVertex(1);
  Flow := -2;
  AssertTrue(g.FindMinCostFlowCs(0, 1, nil, Flow, Cost) = mcfNoFlowRequired);
  Flow := 2;
  AssertTrue(g.FindMinCostFlowCs(0, 1, nil, Flow, Cost) = mcfInvalidNet);
  g.AddEdge(0, 1, TInt64Weight.Create(4));
  Flow := 2;
  AssertTrue(g.FindMinCostFlowCs(0, 1, nil, Flow, Cost) = mcfInvalidCost);
  g.AddVertex(2);
  g.AddVertex(3);
  g.AddEdge(1, 2, TInt64Weight.Create(4));
  g.AddEdge(2, 1, TInt64Weight.Create(4));
  g.AddEdge(2, 3, TInt64Weight.Create(4));
  CostFunc :=
    [TCostEdge.Create(0, 1, 3), TCostEdge.Create(1, 2, -7), TCostEdge.Create(2, 1, -8), TCostEdge.Create(2, 3, 3)];
  Flow := 4;
  AssertTrue(g.FindMinCostFlowCs(0, 3, CostFunc, Flow, Cost) = mcfNegCycle);
end;

procedure TWeightedDigraphTest.FindMinCostFlowCs1;
var
  Ref: TRef64;
  g: TGraph64;
  CostFunc: TCostEdgeArray;
  Flows: TGraph64.TEdgeArray;
  Flow: Int64;
  Cost: TCost;
begin
  {%H-}Ref.Instance := GenerateTestWDigr3;
  g := Ref;
  CostFunc := GenerateCostFun3;
  Flow := 1000;
  AssertTrue(g.FindMinCostFlowCs(0, 12, CostFunc, Flow, Cost, Flows) = mcfOk);
  AssertTrue(Flow = 5);
  AssertTrue(Cost = 11);
  AssertTrue(g.IsMcfFeasible(0, 12, CostFunc, Flows, Flow, Cost));
end;

procedure TWeightedDigraphTest.FindMinCostFlowCs2;
var
  Ref: TRef64;
  g: TGraph64;
  CostFunc: TCostEdgeArray;
  Flows: TGraph64.TEdgeArray;
  Flow: Int64;
  Cost: TCost;
begin
  {%H-}Ref.Instance := GenerateTestWDigr4;
  g := Ref;
  CostFunc := GenerateCostFun4;
  Flow := 1000;
  AssertTrue(g.FindMinCostFlowCs(0, 6, CostFunc, Flow, Cost, Flows) = mcfOk);
  AssertTrue(Flow = 40);
  AssertTrue(Cost = 1296);
  AssertTrue(g.IsMcfFeasible(0, 6, CostFunc, Flows, Flow, Cost));
end;

procedure TWeightedDigraphTest.FindMinCostFlowCs3;
var
  Ref: TRef64;
  g: TGraph64;
  CostFunc: TCostEdgeArray;
  Flows: TGraph64.TEdgeArray;
  Flow: Int64;
  Cost: TCost;
begin
  {%H-}Ref.Instance := GenerateTestWDigr4;
  g := Ref;
  CostFunc := GenerateCostFun4;
  Flow := 20;
  AssertTrue(g.FindMinCostFlowCs(0, 6, CostFunc, Flow, Cost, Flows) = mcfOk);
  AssertTrue(Flow = 20);
  AssertTrue(Cost = 474);
  AssertTrue(g.IsMcfFeasible(0, 6, CostFunc, Flows, Flow, Cost));
end;


initialization
  RegisterTest(TSimpleDiGraphTest);
  RegisterTest(TWeightedDigraphTest);
end.

