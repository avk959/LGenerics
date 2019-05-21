unit LGSimpleDiGraphTest;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  LGUtils,
  LGArrayHelpers,
  LGSparseGraph,
  LGSimpleDiGraph;

type

  { TSimpleDigraphTest }

  TSimpleDigraphTest = class(TTestCase)
  private
  type
    TGraph  = TIntFlowChart;
    TRef    = specialize TGAutoRef<TGraph>;
    THelper = specialize TGOrdinalArrayHelper<SizeInt>;

    function  GenerateTestDigr1: TGraph;
    function  TestDigr1DomTree: TIntArray;
    function  GenerateTestDigr2: TGraph;
    function  TestDigr2DomTree: TIntArray;
    function  GenerateTestDigr3: TGraph;
    function  GenerateTestDigr4: TGraph;
    function  TestDigr4DomTree: TIntArray;
    function  GenerateTestDigr5: TGraph;
    function  TestDigr5DomTree: TIntArray;
    function  GenerateTestDigr6: TGraph;
    function  TestDigr6DomTree: TIntArray;
    function  GenerateRandomGraph(aSize, aDeg: Integer): TGraph;
    function  CreateDfsTree(g: TGraph; aSrc: Integer): TIntArray;
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
    procedure ReachableFrom;
    procedure ReachableFrom1;
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
    procedure IsFlowGraph;
    procedure IsFlowGraph1;
    procedure IsFlowGraph2;
    procedure FindDomTree;
    procedure FindDomTree1;
    procedure FindDomTree2;
    procedure FindDomTree3;
    procedure FindDomTree4;
    procedure FindDomTree5;
    procedure FindDomTreeSnca;
    procedure FindDomTreeSnca1;
    procedure FindDomTreeSnca2;
    procedure FindDomTreeSnca3;
    procedure FindDomTreeSnca4;
    procedure FindDomTreeSnca5;
    procedure IsDomTree;
    procedure IsDomTree1;
    procedure IsDomTree2;
    procedure IsDomTree3;
    procedure CreateDomTree;
    procedure CreateDomTree1;
    procedure CreateDomTree2;
    procedure CreateDomTree3;
    procedure CreateDomTree4;
    procedure FindDomFrontiers;
    procedure FindDomFrontiers1;
    procedure FindDomFrontiers2;
    procedure FindDomFrontiers3;
    procedure IsDag;
    procedure TopologicalSort;
    procedure TopologicalSort1;
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
    TNet         = specialize TGDirectInt64Net<Integer, TInt64Weight, Integer>;
    TCostNet     = specialize TGCostedInt64Net<Integer, TCostPair, Integer>;
    TRef         = specialize TGAutoRef<TGraph>;
    TNetRef      = specialize TGAutoRef<TNet>;
    TCostNetRef  = specialize TGAutoRef<TCostNet>;
    THelper      = specialize TGOrdinalArrayHelper<Integer>;
    TSearch      = specialize TGNumArrayHelper<SizeInt>;

    function  GenerateTestWDigr1: TGraph;
    function  GenerateTestWDigr2: TGraph;
    function  GenerateTestNetBip1: TNet;
    function  GenerateTestNet1: TNet;
    function  GenerateTestNet2: TNet;
    function  GenerateTestCostNet1: TCostNet;
    function  GenerateTestCostNet2: TCostNet;
  published
    procedure SetEdgeData;
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
    procedure FindFlowPr;
    procedure FindFlowPr1;
    procedure FindMaxFlowD;
    procedure FindMaxFlowD1;
    procedure FindFlowD;
    procedure FindFlowD1;
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

function TSimpleDigraphTest.GenerateTestDigr1: TGraph;
begin
  Result := TGraph.Create;  //TestDigr1.png
  Result.AddVertexRange(0, 12);
  Result.AddEdges([0, 1, 0, 2, 0, 3, 0, 5, 0, 6, 2, 3, 3, 5, 3, 4, 6, 4, 4, 9, 6, 9, 7, 6,
                   8, 7, 9, 10, 9, 11, 9, 12, 11, 12]);
end;

function TSimpleDigraphTest.TestDigr1DomTree: TIntArray;
begin
  Result := [-1, 0, 0, 0, 0, 0, 0, -1, -1, 0, 9, 9, 9];
end;

function TSimpleDigraphTest.GenerateTestDigr2: TGraph;
begin
  Result := TGraph.Create;  //TestDigr2.png
  Result.AddVertexRange(0, 8);
  Result.AddEdges([0, 1, 1, 2, 2, 3, 3, 0, 1, 5, 5, 3, 3, 4, 4, 1, 0, 6, 6, 7, 7, 8, 8, 0]);
end;

function TSimpleDigraphTest.TestDigr2DomTree: TIntArray;
begin
  Result := [-1, 0, 1, 1, 3, 1, 0, 6, 7];
end;

function TSimpleDigraphTest.GenerateTestDigr3: TGraph;
begin
  Result := TGraph.Create; //TestDigr3.png
  Result.AddVertexRange(0, 5);
  Result.AddEdges([0, 1,  1, 1,  1, 2,  1, 4,  2, 0,  2, 3,  3, 2,  3, 5,  4, 2, 4, 3,  5, 0,  5, 1,  5, 2]);
end;

function TSimpleDigraphTest.GenerateTestDigr4: TGraph;
begin
  Result := TGraph.Create; //TestDigr4.png - from Tarjan's paper
  Result.AddVertexRange(0, 12);
  Result.AddEdges([0, 1, 0, 2, 0, 3, 1, 4, 2, 1, 2, 4, 2, 5, 3, 6, 3, 7, 4, 12, 5, 8, 6, 9, 7, 9, 7, 10,
                   8, 5, 8, 11, 9, 11, 10, 9, 11, 0, 11, 9, 12, 8]);
end;

function TSimpleDigraphTest.TestDigr4DomTree: TIntArray;
begin
  Result := [-1, 0, 0, 0, 0, 0, 3, 3, 0, 0, 7, 0, 4];
end;

function TSimpleDigraphTest.GenerateTestDigr5: TGraph;
begin
  Result := TGraph.Create; //TestDigr5.png - from Cytron's paper, fig. 9
  Result.AddVertexRange(0, 13);
  Result.AddEdges([0, 1, 0, 13, 1, 2, 2, 3, 2, 7, 3, 4, 3, 5, 4, 6, 5, 6, 6, 8, 7, 8, 8, 9, 9, 10, 9, 11,
                   10, 11, 11, 9, 11, 12, 12, 2, 12, 13]);
end;

function TSimpleDigraphTest.TestDigr5DomTree: TIntArray;
begin
  Result := [-1, 0, 1, 2, 3, 3, 3, 2, 2, 8, 9, 9, 11, 0];
end;

function TSimpleDigraphTest.GenerateTestDigr6: TGraph;
begin
  Result := TGraph.Create; //TestDigr6.png
  Result.AddVertexRange(0, 12);
  Result.AddEdges([0, 1, 0, 2, 1, 3, 1, 6, 2, 4, 2, 7, 3, 5, 3, 6, 4, 7, 4, 2, 5, 8, 5, 10, 6, 9,7, 12,
                   8, 11, 9, 8, 10, 11, 11, 1, 11, 12]);
end;

function TSimpleDigraphTest.TestDigr6DomTree: TIntArray;
begin
  Result := [-1, 0, 0, 1, 2, 3, 1, 2, 1, 6, 5, 1, 0];
end;

function TSimpleDigraphTest.GenerateRandomGraph(aSize, aDeg: Integer): TGraph;
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

function TSimpleDigraphTest.CreateDfsTree(g: TGraph; aSrc: Integer): TIntArray;
var
  Parents: TIntArray;
  procedure OnWhite(aNode, aParent: SizeInt);
  begin
    Parents[aNode] := aParent;
  end;
begin
  Parents := TIntArray.Construct(g.VertexCount, NULL_INDEX);
  g.DfsTraversalI(aSrc, @OnWhite, nil, nil);
  Result := Parents;
end;

procedure TSimpleDigraphTest.AddVertices;
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

procedure TSimpleDigraphTest.SaveToStream;
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
  g2 := {%H-}Ref2;
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

procedure TSimpleDigraphTest.Clone;
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

procedure TSimpleDigraphTest.Reverse;
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

procedure TSimpleDigraphTest.InducedSubgraph;
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

procedure TSimpleDigraphTest.SymmDifferenceOf;
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

procedure TSimpleDigraphTest.SymmDifferenceOf1;
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

procedure TSimpleDigraphTest.SymmDifferenceOf2;
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

procedure TSimpleDigraphTest.SetUnionOf;
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

procedure TSimpleDigraphTest.SetIntersectionOf;
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

procedure TSimpleDigraphTest.SetIntersectionOf2;
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

procedure TSimpleDigraphTest.InDegree;
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

procedure TSimpleDigraphTest.OutDegree;
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

procedure TSimpleDigraphTest.Degree;
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

procedure TSimpleDigraphTest.IsSource;
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

procedure TSimpleDigraphTest.IsSink;
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

procedure TSimpleDigraphTest.IncomingArcs;
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

procedure TSimpleDigraphTest.IncomingArcs1;
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

procedure TSimpleDigraphTest.PathExists;
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

procedure TSimpleDigraphTest.PathExists1;
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

procedure TSimpleDigraphTest.ReachableFrom;
var
  Ref: TRef;
  g: TGraph;
  Reachable: TIntArray;
begin
  g := {%H-}Ref;
  g.AddVertex(1);
  Reachable := g.ReachableFrom(1);
  AssertTrue(Reachable.Length = 1);
  AssertTrue(Reachable[0] = 0);
  g.AddVertex(2);
  Reachable := g.ReachableFrom(1);
  AssertTrue(Reachable.Length = 1);
  AssertTrue(Reachable[0] = 0);
  g.AddEdge(1, 2);
  Reachable := g.ReachableFrom(1);
  AssertTrue(Reachable.Length = 2);
  AssertTrue(THelper.Same(Reachable, [0, 1]));
end;

procedure TSimpleDigraphTest.ReachableFrom1;
var
  Ref: TRef;
  g: TGraph;
  Reachable: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  Reachable := g.ReachableFrom(0);
  AssertTrue(Reachable.Length = 11);
  for I in [0..6, 9..12] do
    AssertTrue(THelper.SequentSearch(Reachable, I) >= 0);
  Reachable := g.ReachableFrom(7);
  AssertTrue(Reachable.Length = 7);
  for I in [4, 6, 7, 9..12] do
    AssertTrue(THelper.SequentSearch(Reachable, I) >= 0);
end;

procedure TSimpleDigraphTest.ContainsCycle;
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

procedure TSimpleDigraphTest.ContainsCycle1;
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

procedure TSimpleDigraphTest.ContainsEulerianCycle;
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

procedure TSimpleDigraphTest.ContainsEulerianCycle1;
var
  Ref: TRef;
  g: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestDigr2;
  g := Ref;
  AssertTrue(g.ContainsEulerianCycle);
end;

procedure TSimpleDigraphTest.FindEulerianCycle;
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

procedure TSimpleDigraphTest.IsStrongConnected;
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

procedure TSimpleDigraphTest.FindStrongComponents;
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

procedure TSimpleDigraphTest.FindStrongComponents1;
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

procedure TSimpleDigraphTest.FindStrongComponents2;
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

procedure TSimpleDigraphTest.GetStrongComponent;
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

procedure TSimpleDigraphTest.BuildReachabilityMatrix;
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

procedure TSimpleDigraphTest.BuildReachabilityMatrix1;
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

procedure TSimpleDigraphTest.BuildReachabilityMatrix2;
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

procedure TSimpleDigraphTest.FindMetrics;
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

procedure TSimpleDigraphTest.FindCenter;
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

procedure TSimpleDigraphTest.FindPeripheral;
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

procedure TSimpleDigraphTest.IsFlowGraph;
var
  Ref: TRef;
  g: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  AssertFalse(g.IsFlowGraph(0));
  g.AddEdge(0, 8);
  AssertTrue(g.IsFlowGraph(0));
  AssertFalse(g.IsFlowGraph(8));
  g.AddEdge(8, 0);
  AssertTrue(g.IsFlowGraph(8));
end;

procedure TSimpleDigraphTest.IsFlowGraph1;
var
  Ref: TRef;
  g: TGraph;
  Missed: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  AssertFalse(g.IsFlowGraph(0, Missed));
  I := g.IndexOf(8);
  AssertTrue(THelper.SequentSearch(Missed, I) <> NULL_INDEX);
  I := g.IndexOf(7);
  AssertTrue(THelper.SequentSearch(Missed, I) <> NULL_INDEX);
end;

procedure TSimpleDigraphTest.IsFlowGraph2;
var
  Ref: TRef;
  g: TGraph;
  I: Integer;
begin
  {%H-}Ref.Instance := GenerateTestDigr2;
  g := Ref;
  for I in g.Vertices do
    AssertTrue(g.IsFlowGraph(I));
end;

procedure TSimpleDigraphTest.FindDomTree;
var
  Ref: TRef;
  g: TGraph;
  Tree: TIntArray;
  I: SizeInt;
begin
  g := {%H-}Ref;
  g.AddVertex(0);
  Tree := g.FindDomTree(0, I);
  AssertTrue(I = g.VertexCount);
  AssertTrue(THelper.Same(Tree, TIntArray([-1])));
end;

procedure TSimpleDigraphTest.FindDomTree1;
var
  Ref: TRef;
  g: TGraph;
  Tree: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr4;
  g := Ref;
  Tree := g.FindDomTree(0, I);
  AssertTrue(I = g.VertexCount);
  AssertTrue(THelper.Same(Tree, TestDigr4DomTree));
end;

procedure TSimpleDigraphTest.FindDomTree2;
var
  Ref: TRef;
  g: TGraph;
  Tree: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr5;
  g := Ref;
  Tree := g.FindDomTree(0, I);
  AssertTrue(I = g.VertexCount);
  AssertTrue(THelper.Same(Tree, TestDigr5DomTree));
end;

procedure TSimpleDigraphTest.FindDomTree3;
var
  Ref: TRef;
  g: TGraph;
  Tree: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  Tree := g.FindDomTree(0, I);
  AssertTrue(I = g.VertexCount - 2);
  AssertTrue(THelper.Same(Tree, TestDigr1DomTree));
  g.AddEdge(0, 8);
  Tree := g.FindDomTree(0, I);
  AssertTrue(I = g.VertexCount);
  AssertTrue(THelper.Same(Tree, TIntArray([-1, 0, 0, 0, 0, 0, 0, 8, 0, 0, 9, 9, 9])));
end;

procedure TSimpleDigraphTest.FindDomTree4;
var
  Ref: TRef;
  g: TGraph;
  Tree: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr2;
  g := Ref;
  Tree := g.FindDomTree(0, I);
  AssertTrue(I = g.VertexCount);
  AssertTrue(THelper.Same(Tree, TestDigr2DomTree));
end;

procedure TSimpleDigraphTest.FindDomTree5;
var
  Ref: TRef;
  g: TGraph;
  Tree: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr6;
  g := Ref;
  Tree := g.FindDomTree(0, I);
  AssertTrue(I = g.VertexCount);
  AssertTrue(THelper.Same(Tree, TestDigr6DomTree));
end;

procedure TSimpleDigraphTest.FindDomTreeSnca;
var
  Ref: TRef;
  g: TGraph;
  Tree: TIntArray;
  I: SizeInt;
begin
  g := {%H-}Ref;
  g.AddVertex(0);
  Tree := g.FindDomTreeSnca(0, I);
  AssertTrue(I = g.VertexCount);
  AssertTrue(THelper.Same(Tree, TIntArray([-1])));
end;

procedure TSimpleDigraphTest.FindDomTreeSnca1;
var
  Ref: TRef;
  g: TGraph;
  Tree: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr4;
  g := Ref;
  Tree := g.FindDomTreeSnca(0, I);
  AssertTrue(I = g.VertexCount);
  AssertTrue(THelper.Same(Tree, TestDigr4DomTree));
end;

procedure TSimpleDigraphTest.FindDomTreeSnca2;
var
  Ref: TRef;
  g: TGraph;
  Tree: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr5;
  g := Ref;
  Tree := g.FindDomTreeSnca(0, I);
  AssertTrue(I = g.VertexCount);
  AssertTrue(THelper.Same(Tree, TestDigr5DomTree));
end;

procedure TSimpleDigraphTest.FindDomTreeSnca3;
var
  Ref: TRef;
  g: TGraph;
  Tree: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  Tree := g.FindDomTreeSnca(0, I);
  AssertTrue(I = g.VertexCount - 2);
  AssertTrue(THelper.Same(Tree, TestDigr1DomTree));
  g.AddEdge(0, 8);
  Tree := g.FindDomTreeSnca(0, I);
  AssertTrue(I = g.VertexCount);
  AssertTrue(THelper.Same(Tree, TIntArray([-1, 0, 0, 0, 0, 0, 0, 8, 0, 0, 9, 9, 9])));
end;

procedure TSimpleDigraphTest.FindDomTreeSnca4;
var
  Ref: TRef;
  g: TGraph;
  Tree: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr2;
  g := Ref;
  Tree := g.FindDomTreeSnca(0, I);
  AssertTrue(I = g.VertexCount);
  AssertTrue(THelper.Same(Tree, TestDigr2DomTree));
end;

procedure TSimpleDigraphTest.FindDomTreeSnca5;
var
  Ref: TRef;
  g: TGraph;
  Tree: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr6;
  g := Ref;
  Tree := g.FindDomTreeSnca(0, I);
  AssertTrue(I = g.VertexCount);
  AssertTrue(THelper.Same(Tree, TestDigr6DomTree));
end;

procedure TSimpleDigraphTest.IsDomTree;
var
  Ref: TRef;
  g: TGraph;
  Tree: TIntArray = nil;
begin
  g := {%H-}Ref;
  g.AddVertex(0);
  AssertFalse(g.IsDomTree(Tree, 0));
  Tree := [0];
  AssertFalse(g.IsDomTree(Tree, 0));
  Tree := [0, 1];
  AssertFalse(g.IsDomTree(Tree, 0));
  Tree := [-1];
  AssertTrue(g.IsDomTree(Tree, 0));
end;

procedure TSimpleDigraphTest.IsDomTree1;
var
  Ref: TRef;
  g: TGraph;
  Tree: TIntArray = nil;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  Tree := CreateDfsTree(g, 0);
  AssertFalse(g.IsDomTree(Tree, 0));
  Tree := TestDigr1DomTree;
  AssertTrue(g.IsDomTree(Tree, 0));
  Tree[11] := 8;
  AssertFalse(g.IsDomTree(Tree, 0));
end;

procedure TSimpleDigraphTest.IsDomTree2;
var
  Ref: TRef;
  g: TGraph;
  Tree: TIntArray = nil;
begin
  {%H-}Ref.Instance := GenerateTestDigr2;
  g := Ref;
  Tree := TestDigr2DomTree;
  AssertTrue(g.IsDomTree(Tree, 0));
  Tree[2] := 0;
  AssertFalse(g.IsDomTree(Tree, 0));
end;

procedure TSimpleDigraphTest.IsDomTree3;
var
  Ref: TRef;
  g: TGraph;
  Tree: TIntArray = nil;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr3;
  g := Ref;
  Tree := g.FindDomTreeSnca(0, I);
  AssertTrue(g.IsDomTree(Tree, 0));
  Tree := g.FindDomTreeSnca(2, I);
  AssertTrue(g.IsDomTree(Tree, 2));
end;

procedure TSimpleDigraphTest.CreateDomTree;
var
  Ref: TRef;
  g: TGraph;
  Tree: TGraph.TDomTree;
begin
  g := {%H-}Ref;
  g.AddVertex(0);
  Tree := g.CreateDomTree(0);
  AssertTrue(Tree.Size = g.VertexCount);
  AssertTrue(Tree.Dominates(0, 0));
  AssertTrue(Tree.ExtractDomSet(0) = nil);
  AssertTrue(Tree.ExtractDominated(0) = nil);
  AssertTrue(Tree[0] = NULL_INDEX);
end;

procedure TSimpleDigraphTest.CreateDomTree1;
var
  Ref: TRef;
  g: TGraph;
  Tree: TGraph.TDomTree;
  Tmp: TIntArray;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr5;
  g := Ref;
  Tree := g.CreateDomTree(0);
  Tmp := TestDigr5DomTree;
  AssertTrue(Tree.Size = 14);
  for I := 0 to Pred(Tree.Size) do
    AssertTrue(Tree[I] = Tmp[I]);
  for I := 1 to Pred(Tree.Size) do
    AssertTrue(Tree.Dominates(0, I));
  AssertTrue(Tree.Dominates(1, 12));
  AssertFalse(Tree.Dominates(1, 13));
  AssertFalse(Tree.Dominates(3, 8));
  AssertTrue(Tree.Dominates(1, 8));
  AssertTrue(Tree.Dominates(2, 8));
  AssertTrue(Tree.Dominates(8, 12));
  Tmp := Tree.ExtractDomSet(12);
  AssertTrue(Tmp.Length = 6);
  for I in [0, 1, 2, 8, 9, 11] do
    AssertTrue(THelper.SequentSearch(Tmp, I) >= 0);
  Tmp := Tree.ExtractDominated(1);
  AssertTrue(Tmp.Length = 11);
  for I in [2..12] do
    AssertTrue(THelper.SequentSearch(Tmp, I) >= 0);
  AssertTrue(Tree.ExtractDominated(4) = nil);
  AssertTrue(Tree.ExtractDominated(5) = nil);
  AssertTrue(Tree.ExtractDominated(7) = nil);
  AssertTrue(Tree.ExtractDominated(12) = nil);
  AssertTrue(Tree.ExtractDominated(13) = nil);
  Tmp := Tree.ExtractDominated(0);
  AssertTrue(Tmp.Length = 13);
  for I in [1..13] do
    AssertTrue(THelper.SequentSearch(Tmp, I) >= 0);
end;

procedure TSimpleDigraphTest.CreateDomTree2;
var
  Ref: TRef;
  g: TGraph;
  Tree: TGraph.TDomTree;
  Tmp: TIntArray;
  I, J: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr5;
  g := Ref;
  Tree := g.CreateDomTree(0);
  J := 0;
  for I in Tree.DomSetOf(0) do
    Inc(J);
  AssertTrue(J = 0);
  Tmp := [0, 1, 2, 8, 9, 11];
  for I in Tree.DomSetOf(12) do
    begin
      AssertTrue(THelper.SequentSearch(Tmp, I) >= 0);
      Inc(J);
    end;
  AssertTrue(J = 6);
  J := 0;
  Tmp := [0, 1];
  for I in Tree.DomSetOf(2) do
    begin
      AssertTrue(THelper.SequentSearch(Tmp, I) >= 0);
      Inc(J);
    end;
  AssertTrue(J = 2);
  J := 0;
  Tmp := [0];
  for I in Tree.DomSetOf(13) do
    begin
      AssertTrue(THelper.SequentSearch(Tmp, I) >= 0);
      Inc(J);
    end;
  AssertTrue(J = 1);
end;

procedure TSimpleDigraphTest.CreateDomTree3;
var
  Ref: TRef;
  g: TGraph;
  Tree: TGraph.TDomTree;
  Tmp: TIntArray;
  I, J: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr5;
  g := Ref;
  Tree := g.CreateDomTree(0);
  J := 0;
  for I in Tree.DominatedBy(13) do
    Inc(J);
  AssertTrue(J = 0);
  for I in Tree.DominatedBy(4) do
    Inc(J);
  AssertTrue(J = 0);
  for I in Tree.DominatedBy(5) do
    Inc(J);
  AssertTrue(J = 0);
  for I in Tree.DominatedBy(7) do
    Inc(J);
  AssertTrue(J = 0);
  for I in Tree.DominatedBy(10) do
    Inc(J);
  AssertTrue(J = 0);
  for I in Tree.DominatedBy(12) do
    Inc(J);
  AssertTrue(J = 0);
  Tmp := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13];
  for I in Tree.DominatedBy(0) do
    begin
      AssertTrue(THelper.SequentSearch(Tmp, I) >= 0);
      Inc(J);
    end;
  AssertTrue(J = 13);
  J := 0;
  Tmp := [9, 10, 11, 12];
  for I in Tree.DominatedBy(8) do
    begin
      AssertTrue(THelper.SequentSearch(Tmp, I) >= 0);
      Inc(J);
    end;
  AssertTrue(J = 4);
end;

procedure TSimpleDigraphTest.CreateDomTree4;
var
  Ref: TRef;
  g: TGraph;
  Tree: TGraph.TDomTree;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  Tree := g.CreateDomTree(0);
  AssertTrue(Tree.Root = 0);
  AssertFalse(Tree.InTree(7));
  AssertFalse(Tree.InTree(8));
  for I in [0..12] - [7..8] do
    AssertTrue(Tree.InTree(I));
end;

procedure TSimpleDigraphTest.FindDomFrontiers;
var
  Ref: TRef;
  g: TGraph;
  Tree: TGraph.TDomTree;
  DomFront: TIntMatrix;
begin
  g := {%H-}Ref;
  g.AddVertex(0);
  DomFront := g.FindDomFrontiers(0, Tree);
  AssertTrue(Tree.Size = 1);
  AssertTrue(Tree[0] = NULL_INDEX);
  AssertTrue(Length(DomFront) = g.VertexCount);
  AssertTrue(DomFront[0] = nil);
end;

procedure TSimpleDigraphTest.FindDomFrontiers1;
var
  Ref: TRef;
  g: TGraph;
  Tree: TGraph.TDomTree;
  Tmp: TIntArray;
  DomFront: TIntMatrix;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr4;
  g := Ref;
  DomFront := g.FindDomFrontiers(0, Tree);
  Tmp := TestDigr4DomTree;
  AssertTrue(Tree.Size = Tmp.Length);
  for I := 0 to Pred(Tmp.Length) do
    AssertTrue(THelper.SequentSearch(Tmp, Tree[I]) >= 0);
  AssertTrue(Length(DomFront) = g.VertexCount);

  AssertTrue(DomFront[0] = nil);

  AssertTrue(Length(DomFront[1]) = 1);
  AssertTrue(DomFront[1, 0] = 4);

  AssertTrue(Length(DomFront[2]) = 3); //1, 4, 5
  AssertTrue(THelper.SequentSearch(DomFront[2], 1) >= 0);
  AssertTrue(THelper.SequentSearch(DomFront[2], 4) >= 0);
  AssertTrue(THelper.SequentSearch(DomFront[2], 5) >= 0);

  AssertTrue(Length(DomFront[3]) = 1); //9
  AssertTrue(DomFront[3, 0] = 9);

  AssertTrue(Length(DomFront[4]) = 1); //8
  AssertTrue(DomFront[4, 0] = 8);

  AssertTrue(Length(DomFront[5]) = 1); //8
  AssertTrue(DomFront[5, 0] = 8);

  AssertTrue(Length(DomFront[6]) = 1); //9
  AssertTrue(DomFront[6, 0] = 9);

  AssertTrue(Length(DomFront[7]) = 1); //9
  AssertTrue(DomFront[7, 0] = 9);

  AssertTrue(Length(DomFront[8]) = 2); //5, 11
  AssertTrue(THelper.SequentSearch(DomFront[8], 5) >= 0);
  AssertTrue(THelper.SequentSearch(DomFront[8], 11) >= 0);

  AssertTrue(Length(DomFront[9]) = 1); //11
  AssertTrue(DomFront[9, 0] = 11);

  AssertTrue(Length(DomFront[10]) = 1); //9
  AssertTrue(DomFront[10, 0] = 9);

  AssertTrue(Length(DomFront[11]) = 1); //9
  AssertTrue(DomFront[11, 0] = 9);

  AssertTrue(Length(DomFront[12]) = 1); //8
  AssertTrue(DomFront[12, 0] = 8);
end;

procedure TSimpleDigraphTest.FindDomFrontiers2;
var
  Ref: TRef;
  g: TGraph;
  Tree: TGraph.TDomTree;
  Tmp: TIntArray;
  DomFront: TIntMatrix;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr5;
  g := Ref;
  DomFront := g.FindDomFrontiers(0, Tree);
  Tmp := TestDigr5DomTree;
  AssertTrue(Tree.Size = Tmp.Length);
  for I := 0 to Pred(Tmp.Length) do
    AssertTrue(THelper.SequentSearch(Tmp, Tree[I]) >= 0);
  AssertTrue(Length(DomFront) = g.VertexCount);

  AssertTrue(DomFront[0] = nil);

  AssertTrue(Length(DomFront[1]) = 1); //13
  AssertTrue(DomFront[1, 0] = 13);

  AssertTrue(Length(DomFront[2]) = 1); //13
  AssertTrue(DomFront[2, 0] = 13);

  AssertTrue(Length(DomFront[3]) = 1); //8
  AssertTrue(DomFront[3, 0] = 8);

  AssertTrue(Length(DomFront[4]) = 1); //6
  AssertTrue(DomFront[4, 0] = 6);

  AssertTrue(Length(DomFront[5]) = 1); //6
  AssertTrue(DomFront[5, 0] = 6);

  AssertTrue(Length(DomFront[6]) = 1); //8
  AssertTrue(DomFront[6, 0] = 8);

  AssertTrue(Length(DomFront[7]) = 1); //8
  AssertTrue(DomFront[7, 0] = 8);

  AssertTrue(Length(DomFront[8]) = 2); //2, 13
  AssertTrue(THelper.SequentSearch(DomFront[8], 2) >= 0);
  AssertTrue(THelper.SequentSearch(DomFront[8], 13) >= 0);

  AssertTrue(Length(DomFront[9]) = 2); //2, 13
  AssertTrue(THelper.SequentSearch(DomFront[9], 2) >= 0);
  AssertTrue(THelper.SequentSearch(DomFront[9], 13) >= 0);

  AssertTrue(Length(DomFront[10]) = 1); //11
  AssertTrue(DomFront[10, 0] = 11);

  AssertTrue(Length(DomFront[11]) = 3); //2, 9, 13
  AssertTrue(THelper.SequentSearch(DomFront[11], 2) >= 0);
  AssertTrue(THelper.SequentSearch(DomFront[11], 9) >= 0);
  AssertTrue(THelper.SequentSearch(DomFront[11], 13) >= 0);

  AssertTrue(Length(DomFront[12]) = 2); //2, 13
  AssertTrue(THelper.SequentSearch(DomFront[12], 2) >= 0);
  AssertTrue(THelper.SequentSearch(DomFront[12], 13) >= 0);

  AssertTrue(DomFront[13] = nil);
end;

procedure TSimpleDigraphTest.FindDomFrontiers3;
var
  Ref: TRef;
  g: TGraph;
  Tree: TGraph.TDomTree;
  Tmp: TIntArray;
  DomFront: TIntMatrix;
  I: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr6;
  g := Ref;
  DomFront := g.FindDomFrontiers(0, Tree);
  Tmp := TestDigr6DomTree;
  AssertTrue(Tree.Size = Tmp.Length);
  for I := 0 to Pred(Tmp.Length) do
    AssertTrue(THelper.SequentSearch(Tmp, Tree[I]) >= 0);
  AssertTrue(Length(DomFront) = g.VertexCount);

  AssertTrue(DomFront[0] = nil);

  AssertTrue(Length(DomFront[1]) = 1); //12
  AssertTrue(DomFront[1, 0] = 12);

  AssertTrue(Length(DomFront[2]) = 1); //12
  AssertTrue(DomFront[2, 0] = 12);

  AssertTrue(Length(DomFront[3]) = 3); //6, 8, 11
  AssertTrue(THelper.SequentSearch(DomFront[3], 6) >= 0);
  AssertTrue(THelper.SequentSearch(DomFront[3], 8) >= 0);
  AssertTrue(THelper.SequentSearch(DomFront[3], 11) >= 0);

  AssertTrue(Length(DomFront[4]) = 2); //2, 7
  AssertTrue(THelper.SequentSearch(DomFront[4], 2) >= 0);
  AssertTrue(THelper.SequentSearch(DomFront[4], 7) >= 0);

  AssertTrue(Length(DomFront[5]) = 2); //8, 11
  AssertTrue(THelper.SequentSearch(DomFront[5], 8) >= 0);
  AssertTrue(THelper.SequentSearch(DomFront[5], 11) >= 0);

  AssertTrue(Length(DomFront[6]) = 1); //8
  AssertTrue(DomFront[6, 0] = 8);

  AssertTrue(Length(DomFront[7]) = 1); //12
  AssertTrue(DomFront[7, 0] = 12);

  AssertTrue(Length(DomFront[8]) = 1); //11
  AssertTrue(DomFront[8, 0] = 11);

  AssertTrue(Length(DomFront[9]) = 1); //8
  AssertTrue(DomFront[9, 0] = 8);

  AssertTrue(Length(DomFront[10]) = 1); //11
  AssertTrue(DomFront[10, 0] = 11);

  AssertTrue(Length(DomFront[11]) = 2); //1, 12
  AssertTrue(THelper.SequentSearch(DomFront[11], 1) >= 0);
  AssertTrue(THelper.SequentSearch(DomFront[11], 12) >= 0);

  AssertTrue(DomFront[12] = nil);
end;

procedure TSimpleDigraphTest.IsDag;
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

procedure TSimpleDigraphTest.TopologicalSort;
var
  Ref: TRef;
  g: TGraph;
  Sorted: TIntArray;
begin
  g := {%H-}Ref;
  Sorted := g.TopologicalSort;
  AssertTrue(g.IsTopoSorted(Sorted));
  Ref.Instance := GenerateTestDigr2;
  g := Ref;
  Sorted := g.TopologicalSort;
  AssertFalse(g.IsTopoSorted(Sorted));
  Ref.Instance := GenerateTestDigr1;
  g := Ref;
  Sorted := g.TopologicalSort;
  AssertTrue(g.IsTopoSorted(Sorted));
  Sorted := g.TopologicalSort(soDesc);
  AssertTrue(g.IsTopoSorted(Sorted, soDesc));
  g.AddEdge(12, 3);
  Sorted := g.TopologicalSort;
  AssertFalse(g.IsTopoSorted(Sorted));
end;

procedure TSimpleDigraphTest.TopologicalSort1;
var
  Ref: TRef;
  g: TGraph;
  Sorted: TIntArray;
begin
  g := {%H-}Ref;
  AssertTrue(g.TopologicalSort(Sorted));
  AssertTrue(g.IsTopoSorted(Sorted, soDesc));
  Ref.Instance := GenerateTestDigr2;
  g := Ref;
  AssertFalse(g.TopologicalSort(Sorted));
  Ref.Instance := GenerateTestDigr1;
  g := Ref;
  AssertTrue(g.TopologicalSort(Sorted));
  AssertTrue(g.IsTopoSorted(Sorted));
  AssertTrue(g.TopologicalSort(Sorted, soDesc));
  AssertTrue(g.IsTopoSorted(Sorted, soDesc));
  g.AddEdge(4, 0);
  AssertFalse(g.TopologicalSort(Sorted));
end;

procedure TSimpleDigraphTest.DagLongestPathsMap;
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

procedure TSimpleDigraphTest.DagLongestPathsMap1;
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

procedure TSimpleDigraphTest.DagLongestPaths;
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

procedure TSimpleDigraphTest.FindHamiltonCycles;
var
  Ref: TRef;
  g: TGraph;
  Cycles: TIntArrayVector;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  AssertFalse(g.FindHamiltonCycles(1, 0, Cycles, 10));
end;

procedure TSimpleDigraphTest.FindHamiltonCycles1;
var
  Ref: TRef;
  g: TGraph;
  Cycles: TIntArrayVector;
begin
  {%H-}Ref.Instance := GenerateTestDigr2;
  g := Ref;
  AssertFalse(g.FindHamiltonCycles(1, 0, Cycles, 10));
end;

procedure TSimpleDigraphTest.FindHamiltonCycles2;
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

procedure TSimpleDigraphTest.FindHamiltonPaths;
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

procedure TSimpleDigraphTest.FindHamiltonPaths1;
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

function TWeightedDigraphTest.GenerateTestNetBip1: TNet;
var
  I: Integer;
begin
  Result := TNet.Create;  // min match weight = 58, max match weight = 270
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

function TWeightedDigraphTest.GenerateTestNet1: TNet;
var
  I: Integer;
begin
  Result := TNet.Create; //TestNet1.png   source: 0; sink: 12; maxflow = 5
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

function TWeightedDigraphTest.GenerateTestNet2: TNet;
var
  I: Integer;
begin
  Result := TNet.Create; //TestNet2.png (Christofides) source: 0; sink: 6; maxflow = 40
  for I := 0 to 6 do
    Result.AddVertex(I);
  Result.AddEdge(0, 1, TInt64Weight.Create(16));
  Result.AddEdge(0, 3, TInt64Weight.Create(11));
  Result.AddEdge(0, 5, TInt64Weight.Create(13));
  Result.AddEdge(1, 2, TInt64Weight.Create(17));
  Result.AddEdge(1, 3, TInt64Weight.Create(18));
  Result.AddEdge(1, 4, TInt64Weight.Create(16));
  Result.AddEdge(2, 6, TInt64Weight.Create(22));
  Result.AddEdge(3, 2, TInt64Weight.Create(12));
  Result.AddEdge(3, 5, TInt64Weight.Create(19));
  Result.AddEdge(4, 6, TInt64Weight.Create(16));
  Result.AddEdge(5, 4, TInt64Weight.Create(10));
  Result.AddEdge(5, 6, TInt64Weight.Create(5));
end;

function TWeightedDigraphTest.GenerateTestCostNet1: TCostNet;
var
  I: Integer;
begin
  Result := TCostNet.Create; //TestCostNet1.png   source: 0; sink: 12; maxflow = 5 mincost = 11
  for I := 0 to 12 do
    Result.AddVertex(I);
  Result.AddEdge(0, 1, TCostPair.Create(1, 0));
  Result.AddEdge(0, 2, TCostPair.Create(1, 0));
  Result.AddEdge(0, 3, TCostPair.Create(1, 0));
  Result.AddEdge(0, 4, TCostPair.Create(1, 0));
  Result.AddEdge(0, 5, TCostPair.Create(1, 0));
  Result.AddEdge(1, 6, TCostPair.Create(1, 1));
  Result.AddEdge(1, 7, TCostPair.Create(1, 2));
  Result.AddEdge(1, 8, TCostPair.Create(1, 3));
  Result.AddEdge(2, 6, TCostPair.Create(1, 1));
  Result.AddEdge(2, 7, TCostPair.Create(1, 2));
  Result.AddEdge(3, 6, TCostPair.Create(1, 1));
  Result.AddEdge(4, 6, TCostPair.Create(1, 1));
  Result.AddEdge(4, 7, TCostPair.Create(1, 2));
  Result.AddEdge(5, 6, TCostPair.Create(1, 1));
  Result.AddEdge(5, 7, TCostPair.Create(1, 2));
  Result.AddEdge(5, 8, TCostPair.Create(1, 3));
  Result.AddEdge(6, 9, TCostPair.Create(1, 0));
  Result.AddEdge(7, 10, TCostPair.Create(2, 0));
  Result.AddEdge(8, 11, TCostPair.Create(3, 0));
  Result.AddEdge(9, 12, TCostPair.Create(100, 0));
  Result.AddEdge(10, 12, TCostPair.Create(100, 0));
  Result.AddEdge(11, 12, TCostPair.Create(100, 0));
end;

function TWeightedDigraphTest.GenerateTestCostNet2: TCostNet;
var
  I: Integer;
begin
  Result := TCostNet.Create; //TestCostNet2.png (Christofides)
                             //source: 0; sink: 6; flow = 40: mincost = 1296
  for I := 0 to 6 do         //                    flow = 20: mincost = 474
    Result.AddVertex(I);
  Result.AddEdge(0, 1, TCostPair.Create(16, 7));
  Result.AddEdge(0, 3, TCostPair.Create(11, 13));
  Result.AddEdge(0, 5, TCostPair.Create(13, 28));
  Result.AddEdge(1, 2, TCostPair.Create(17, 25));
  Result.AddEdge(1, 3, TCostPair.Create(18, 4));
  Result.AddEdge(1, 4, TCostPair.Create(16, 10));
  Result.AddEdge(2, 6, TCostPair.Create(22, 5));
  Result.AddEdge(3, 2, TCostPair.Create(12, 6));
  Result.AddEdge(3, 5, TCostPair.Create(19, 5));
  Result.AddEdge(4, 6, TCostPair.Create(16, 12));
  Result.AddEdge(5, 4, TCostPair.Create(10, 3));
  Result.AddEdge(5, 6, TCostPair.Create(5, 7));
end;

procedure TWeightedDigraphTest.SetEdgeData;
var
  Ref: TRef;
  g: TGraph;
  e: TGraph.TEdge;
begin
  {%H-}Ref.Instance := GenerateTestWDigr1;
  g := Ref;
  AssertTrue(g.EdgeCount = 9);
  for e in g.Edges do
    AssertTrue(g.SetEdgeData(e.Source, e.Destination, TIntWeight.Create(0)));
  for e in g.Edges do
    AssertTrue(e.Data.Weight = 0);
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
  Ref: TNetRef;
  g: TNet;
  m: TGraph.TEdgeArray;
begin
  g := {%H-}Ref;
  AssertFalse(g.FindMinWeightBipMatch(m));
  g.AddVertex(1);
  AssertFalse(g.FindMinWeightBipMatch(m));
  g.AddVertex(2);
  AssertTrue(g.FindMinWeightBipMatch(m));
  AssertTrue(m = nil);
  Ref.Instance := GenerateTestNet2;
  g := Ref;
  AssertFalse(g.FindMinWeightBipMatch(m));
end;

procedure TWeightedDigraphTest.FindMinWeightBipMatch1;
var
  Ref: TNetRef;
  g: TNet;
  m: TGraph.TEdgeArray;
  im: TIntEdgeArray;
begin
  {%H-}Ref.Instance := GenerateTestNetBip1;
  g := Ref;
  AssertTrue(g.FindMinWeightBipMatch(m));
  im := g.EdgeArray2IntEdgeArray(m);
  AssertTrue(g.IsMaxMatching(im));
  AssertTrue(g.IsPerfectMatching(im));
  AssertTrue(g.TotalWeight(m) = 58);
end;

procedure TWeightedDigraphTest.FindMaxWeightBipMatch;
var
  Ref: TNetRef;
  g: TNet;
  m: TGraph.TEdgeArray;
begin
  g := {%H-}Ref;
  AssertFalse(g.FindMaxWeightBipMatch(m));
  g.AddVertex(1);
  AssertFalse(g.FindMaxWeightBipMatch(m));
  g.AddVertex(2);
  AssertTrue(g.FindMaxWeightBipMatch(m));
  AssertTrue(m = nil);
  Ref.Instance := GenerateTestNet2;
  g := Ref;
  AssertFalse(g.FindMaxWeightBipMatch(m));
end;

procedure TWeightedDigraphTest.FindMaxWeightBipMatch1;
var
  Ref: TNetRef;
  g: TNet;
  m: TGraph.TEdgeArray;
  im: TIntEdgeArray;
begin
  {%H-}Ref.Instance := GenerateTestNetBip1;
  g := Ref;
  AssertTrue(g.FindMaxWeightBipMatch(m));
  im := g.EdgeArray2IntEdgeArray(m);
  AssertTrue(g.IsMaxMatching(im));
  AssertTrue(g.IsPerfectMatching(im));
  AssertTrue(g.TotalWeight(m) = 270);
end;

procedure TWeightedDigraphTest.FindMaxFlowPr;
var
  Ref: TNetRef;
  g: TNet;
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
  Ref: TNetRef;
  g: TNet;
  ArcFlows: TNet.TEdgeArray;
  Flow: Int64;
begin
  {%H-}Ref.Instance := GenerateTestNet1;
  g := Ref;
  AssertTrue(g.FindMaxFlowPr(0, 12, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 5);
  AssertTrue(g.IsFeasibleFlow(0, 12, Flow, ArcFlows));
end;

procedure TWeightedDigraphTest.FindMaxFlowPr2;
var
  Ref: TNetRef;
  g: TNet;
  ArcFlows: TNet.TEdgeArray;
  Flow: Int64;
begin
  {%H-}Ref.Instance := GenerateTestNet2;
  g := Ref;
  AssertTrue(g.FindMaxFlowPr(0, 6, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 40);
  AssertTrue(g.IsFeasibleFlow(0, 6, Flow, ArcFlows));
end;

procedure TWeightedDigraphTest.FindFlowPr;
var
  Ref: TNetRef;
  g: TNet;
  ArcFlows: TNet.TEdgeArray;
  Flow: Int64;
begin
  {%H-}Ref.Instance := GenerateTestNet1;
  g := Ref;
  Flow := -3;
  AssertTrue(g.FindFlowPr(0, 12, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 0);
  AssertTrue(ArcFlows = nil);
  Flow := 3;
  AssertTrue(g.FindFlowPr(0, 12, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 3);
  AssertTrue(g.IsFeasibleFlow(0, 12, Flow, ArcFlows));
  Flow := 7;
  AssertTrue(g.FindFlowPr(0, 12, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 5);
  AssertTrue(g.IsFeasibleFlow(0, 12, Flow, ArcFlows));
end;

procedure TWeightedDigraphTest.FindFlowPr1;
var
  Ref: TNetRef;
  g: TNet;
  ArcFlows: TNet.TEdgeArray;
  Flow: Int64;
begin
  {%H-}Ref.Instance := GenerateTestNet2;
  g := Ref;
  Flow := 15;
  AssertTrue(g.FindFlowPr(0, 6, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 15);
  AssertTrue(g.IsFeasibleFlow(0, 6, Flow, ArcFlows));
  Flow := 35;
  AssertTrue(g.FindFlowPr(0, 6, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 35);
  AssertTrue(g.IsFeasibleFlow(0, 6, Flow, ArcFlows));
  Flow := 50;
  AssertTrue(g.FindFlowPr(0, 6, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 40);
  AssertTrue(g.IsFeasibleFlow(0, 6, Flow, ArcFlows));
end;

procedure TWeightedDigraphTest.FindMaxFlowD;
var
  Ref: TNetRef;
  g: TNet;
  ArcFlows: TNet.TEdgeArray;
  Flow: Int64;
begin
  {%H-}Ref.Instance := GenerateTestNet1;
  g := Ref;
  AssertTrue(g.FindMaxFlowD(0, 12, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 5);
  AssertTrue(g.IsFeasibleFlow(0, 12, Flow, ArcFlows));
end;

procedure TWeightedDigraphTest.FindMaxFlowD1;
var
  Ref: TNetRef;
  g: TNet;
  ArcFlows: TNet.TEdgeArray;
  Flow: Int64;
begin
  {%H-}Ref.Instance := GenerateTestNet2;
  g := Ref;
  AssertTrue(g.FindMaxFlowD(0, 6, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 40);
  AssertTrue(g.IsFeasibleFlow(0, 6, Flow, ArcFlows));
end;

procedure TWeightedDigraphTest.FindFlowD;
var
  Ref: TNetRef;
  g: TNet;
  ArcFlows: TNet.TEdgeArray;
  Flow: Int64;
begin
  {%H-}Ref.Instance := GenerateTestNet1;
  g := Ref;
  Flow := -3;
  AssertTrue(g.FindFlowD(0, 12, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 0);
  AssertTrue(ArcFlows = nil);
  Flow := 3;
  AssertTrue(g.FindFlowD(0, 12, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 3);
  AssertTrue(g.IsFeasibleFlow(0, 12, Flow, ArcFlows));
  Flow := 7;
  AssertTrue(g.FindFlowD(0, 12, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 5);
  AssertTrue(g.IsFeasibleFlow(0, 12, Flow, ArcFlows));
end;

procedure TWeightedDigraphTest.FindFlowD1;
var
  Ref: TNetRef;
  g: TNet;
  ArcFlows: TNet.TEdgeArray;
  Flow: Int64;
begin
  {%H-}Ref.Instance := GenerateTestNet2;
  g := Ref;
  Flow := 15;
  AssertTrue(g.FindFlowD(0, 6, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 15);
  AssertTrue(g.IsFeasibleFlow(0, 6, Flow, ArcFlows));
  Flow := 35;
  AssertTrue(g.FindFlowD(0, 6, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 35);
  AssertTrue(g.IsFeasibleFlow(0, 6, Flow, ArcFlows));
  Flow := 50;
  AssertTrue(g.FindFlowD(0, 6, Flow, ArcFlows) = nsOk);
  AssertTrue(Flow = 40);
  AssertTrue(g.IsFeasibleFlow(0, 6, Flow, ArcFlows));
end;

procedure TWeightedDigraphTest.FindMinSTCutPr;
var
  Ref: TNetRef;
  g: TNet;
  Cut: TNet.TStCut;
  Value: Int64;
begin
  {%H-}Ref.Instance := GenerateTestNet1;
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
  Ref: TNetRef;
  g: TNet;
  Cut: TNet.TStCut;
  Value: Int64;
begin
  {%H-}Ref.Instance := GenerateTestNet2;
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
  Ref: TNetRef;
  g: TNet;
  Cut: TNet.TStCut;
  Value: Int64;
begin
  {%H-}Ref.Instance := GenerateTestNet1;
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
  Ref: TNetRef;
  g: TNet;
  Cut: TNet.TStCut;
  Value: Int64;
begin
  {%H-}Ref.Instance := GenerateTestNet2;
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
  Ref: TCostNetRef;
  g: TCostNet;
  Flow: Int64;
  Cost: TCost;
begin
  g := {%H-}Ref;
  g.AddVertex(0);
  g.AddVertex(1);
  Flow := -2;
  AssertTrue(g.FindMinCostFlowSsp(0, 1, Flow, Cost) = mcfNoFlowRequired);
  Flow := 2;
  AssertTrue(g.FindMinCostFlowSsp(0, 1, Flow, Cost) = mcfInvalidNet);
  g.AddEdge(0, 1, TCostPair.Create(4, 3));
  g.AddVertex(2);
  g.AddVertex(3);
  g.AddEdge(1, 2, TCostPair.Create(4, -7));
  g.AddEdge(2, 1, TCostPair.Create(4, -8));
  g.AddEdge(2, 3, TCostPair.Create(4, 3));
  Flow := 4;
  AssertTrue(g.FindMinCostFlowSsp(0, 3, Flow, Cost) = mcfNegCycle);
end;

procedure TWeightedDigraphTest.FindMinCostFlowSsp1;
var
  Ref: TCostNetRef;
  g: TCostNet;
  Flows: TNet.TEdgeArray;
  Flow: Int64;
  Cost: TCost;
begin
  {%H-}Ref.Instance := GenerateTestCostNet1;
  g := Ref;
  Flow := 1000;
  AssertTrue(g.FindMinCostFlowSsp(0, 12, Flow, Cost, Flows) = mcfOk);
  AssertTrue(Flow = 5);
  AssertTrue(Cost = 11);
  AssertTrue(g.IsMcfFeasible(0, 12, Flows, Flow, Cost));
end;

procedure TWeightedDigraphTest.FindMinCostFlowSsp2;
var
  Ref: TCostNetRef;
  g: TCostNet;
  Flows: TNet.TEdgeArray;
  Flow: Int64;
  Cost: TCost;
begin
  {%H-}Ref.Instance := GenerateTestCostNet2;
  g := Ref;
  Flow := 1000;
  AssertTrue(g.FindMinCostFlowSsp(0, 6, Flow, Cost, Flows) = mcfOk);
  AssertTrue(Flow = 40);
  AssertTrue(Cost = 1296);
  AssertTrue(g.IsMcfFeasible(0, 6, Flows, Flow, Cost));
end;

procedure TWeightedDigraphTest.FindMinCostFlowSsp3;
var
  Ref: TCostNetRef;
  g: TCostNet;
  Flows: TNet.TEdgeArray;
  Flow: Int64;
  Cost: TCost;
begin
  {%H-}Ref.Instance := GenerateTestCostNet2;
  g := Ref;
  Flow := 20;
  AssertTrue(g.FindMinCostFlowSsp(0, 6, Flow, Cost, Flows) = mcfOk);
  AssertTrue(Flow = 20);
  AssertTrue(Cost = 474);
  AssertTrue(g.IsMcfFeasible(0, 6, Flows, Flow, Cost));
end;

procedure TWeightedDigraphTest.FindMinCostFlowCs;
var
  Ref: TCostNetRef;
  g: TCostNet;
  Flow: Int64;
  Cost: TCost;
begin
  g := {%H-}Ref;
  g.AddVertex(0);
  g.AddVertex(1);
  Flow := -2;
  AssertTrue(g.FindMinCostFlowCs(0, 1, Flow, Cost) = mcfNoFlowRequired);
  Flow := 2;
  AssertTrue(g.FindMinCostFlowCs(0, 1, Flow, Cost) = mcfInvalidNet);
  g.AddEdge(0, 1, TCostPair.Create(4, 3));
  g.AddVertex(2);
  g.AddVertex(3);
  g.AddEdge(1, 2, TCostPair.Create(4, -7));
  g.AddEdge(2, 1, TCostPair.Create(4, -8));
  g.AddEdge(2, 3, TCostPair.Create(4, 3));
  Flow := 4;
  AssertTrue(g.FindMinCostFlowCs(0, 3, Flow, Cost) = mcfNegCycle);
end;

procedure TWeightedDigraphTest.FindMinCostFlowCs1;
var
  Ref: TCostNetRef;
  g: TCostNet;
  Flows: TNet.TEdgeArray;
  Flow: Int64;
  Cost: TCost;
begin
  {%H-}Ref.Instance := GenerateTestCostNet1;
  g := Ref;
  Flow := 1000;
  AssertTrue(g.FindMinCostFlowCs(0, 12, Flow, Cost, Flows) = mcfOk);
  AssertTrue(Flow = 5);
  AssertTrue(Cost = 11);
  AssertTrue(g.IsMcfFeasible(0, 12, Flows, Flow, Cost));
end;

procedure TWeightedDigraphTest.FindMinCostFlowCs2;
var
  Ref: TCostNetRef;
  g: TCostNet;
  Flows: TNet.TEdgeArray;
  Flow: Int64;
  Cost: TCost;
begin
  {%H-}Ref.Instance := GenerateTestCostNet2;
  g := Ref;
  Flow := 1000;
  AssertTrue(g.FindMinCostFlowCs(0, 6, Flow, Cost, Flows) = mcfOk);
  AssertTrue(Flow = 40);
  AssertTrue(Cost = 1296);
  AssertTrue(g.IsMcfFeasible(0, 6, Flows, Flow, Cost));
end;

procedure TWeightedDigraphTest.FindMinCostFlowCs3;
var
  Ref: TCostNetRef;
  g: TCostNet;
  Flows: TNet.TEdgeArray;
  Flow: Int64;
  Cost: TCost;
begin
  {%H-}Ref.Instance := GenerateTestCostNet2;
  g := Ref;
  Flow := 20;
  AssertTrue(g.FindMinCostFlowCs(0, 6, Flow, Cost, Flows) = mcfOk);
  AssertTrue(Flow = 20);
  AssertTrue(Cost = 474);
  AssertTrue(g.IsMcfFeasible(0, 6, Flows, Flow, Cost));
end;


initialization
  RegisterTest(TSimpleDigraphTest);
  RegisterTest(TWeightedDigraphTest);
end.

