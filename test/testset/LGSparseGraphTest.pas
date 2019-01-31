unit LGSparseGraphTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  LGUtils,
  LGArrayHelpers,
  LGVector,
  LGSparseGraph,
  LGSimpleGraph,
  LGSimpleDigraph;

type

  { TSparseGraphTest }

  TSparseGraphTest = class(TTestCase)
  private
  type
    TGraph      = TIntChart;
    TDiGraph    = TIntFlowChart;
    TGraphRef   = specialize TGAutoRef<TGraph>;
    TDiGraphRef = specialize TGAutoRef<TDiGraph>;
    THelper     = specialize TGOrdinalArrayHelper<SizeInt>;

  var
    FFound,
    FDone: TBoolVector;
    function  GenerateTestGrBip1: TGraph;
    function  GenerateTestDigrBip1: TDiGraph;
    function  GenerateTestGr1: TGraph;
    function  GenerateTestDigr1: TDiGraph;
    function  vFound({%H-}aSender: TObject; aNode, {%H-}aParent: SizeInt): Boolean;
    function  vDone({%H-}aSender: TObject; aIndex: SizeInt): Boolean;
  published
    procedure IsEmpty;
    procedure IsEmptyDirect;
    procedure NonEmpty;
    procedure NonEmptyDirect;
    procedure EnsureCapacity;
    procedure EnsureCapacityDirect;
    procedure Clear;
    procedure ClearDirect;
    procedure AddVertex;
    procedure AddVertexDirect;
    procedure RemoveVertex;
    procedure RemoveVertexDirect;
    procedure IndexOf;
    procedure IndexOfDirect;
    procedure Items;
    procedure ItemsDirect;
    procedure RemoveVertexI;
    procedure RemoveVertexIDirect;
    procedure ContainsVertex;
    procedure ContainsVertexDirect;
    procedure AddEdge;
    procedure AddArc;
    procedure AddEdgeI;
    procedure AddEdgeIDirect;
    procedure Adjacent;
    procedure AdjacentDirect;
    procedure AdjVertices;
    procedure AdjVerticesDirect;
    procedure AdjVerticesI;
    procedure AdjVerticesIDirect;
    procedure Vertices;
    procedure VerticesDirect;
    procedure CreateAdjacencyMatrix;
    procedure CreateAdjacencyMatrixDirect;
    procedure IsBipartite;
    procedure IsBipartiteDirect;
    procedure DfsTraversal;
    procedure DfsTraversalDirect;
    procedure BfsTraversal;
    procedure BfsTraversalDirect;
    procedure ShortestPaths;
    procedure ShortestPathsDirect;
    procedure Eccentricity;
    procedure EccentricityDirect;
  end;

  { TTspTest }

  TTspTest = class(TTestCase)
  private
  type
    TSolver = specialize TGMetricTspHelper<Integer>;

    function CreateBr17: TSolver.TTspMatrix;
    function CreateSymm8: TSolver.TTspMatrix;
    function CreateGr17: TSolver.TTspMatrix;
  published
    procedure Asymmetric;
    procedure Symmetric;
    procedure Symmetric1;
  end;

implementation

function TSparseGraphTest.GenerateTestGrBip1: TGraph;
begin
  //see TestGrBip1.png
  Result := TGraph.Create;
  Result.AddVertexRange(1, 16);
  Result.AddEdges([1, 2, 1, 4, 1, 6, 3, 4, 3, 6, 3, 8, 5, 6, 5, 8, 5, 10, 7, 8, 7, 10, 7,
                   12, 9, 10, 9, 12, 9, 14, 11, 12, 11, 14, 11, 16, 13, 14, 13, 16, 15, 16]);
end;

function TSparseGraphTest.GenerateTestDigrBip1: TDiGraph;
begin
  //see TestDigrBip1.png
  Result := TDiGraph.Create;
  Result.AddVertexRange(1, 12);
  Result.AddEdges([1, 2, 1, 4, 1, 6, 2, 3, 3, 4, 3, 6, 3, 8, 4, 5, 5, 6, 5, 8, 5, 10,
                   6, 7, 7, 8, 7, 10, 7, 12, 8, 9, 9, 10, 9, 12, 10, 11, 11, 8, 12, 1]);
end;

function TSparseGraphTest.GenerateTestGr1: TGraph;
begin
  //see TestGr1.png
  Result := TGraph.Create;
  Result.AddVertexRange(0, 12);
  Result.AddEdges([0, 1, 0, 2, 0, 3, 0, 5, 0, 6, 2, 3, 3, 5, 3, 4, 6, 4, 4, 9, 6, 9, 7, 6,
                   8, 7, 9, 10, 9, 11, 9, 12, 11, 12]);
end;

function TSparseGraphTest.GenerateTestDigr1: TDiGraph;
begin
  //see TestDigr1.png
  Result := TDiGraph.Create;
  Result.AddVertexRange(0, 12);
  Result.AddEdges([0, 1, 0, 2, 0, 3, 0, 5, 0, 6, 2, 3, 3, 5, 3, 4, 6, 4, 4, 9, 6, 9, 7, 6,
                   8, 7, 9, 10, 9, 11, 9, 12, 11, 12]);
end;

function TSparseGraphTest.vFound(aSender: TObject; aNode, aParent: SizeInt): Boolean;
begin
  FFound[aNode] := False;
  Result := True;
end;

function TSparseGraphTest.vDone(aSender: TObject; aIndex: SizeInt): Boolean;
begin
  FDone[aIndex] := False;
  Result := True;
end;

procedure TSparseGraphTest.IsEmpty;
var
  Ref: TGraphRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertTrue(g.IsEmpty);
  AssertTrue(g.AddVertex(3));
  AssertFalse(g.IsEmpty);
end;

procedure TSparseGraphTest.IsEmptyDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
begin
  g := {%H-}Ref;
  AssertTrue(g.IsEmpty);
  AssertTrue(g.AddVertex(3));
  AssertFalse(g.IsEmpty);
end;

procedure TSparseGraphTest.NonEmpty;
var
  Ref: TGraphRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.NonEmpty);
  AssertTrue(g.AddVertex(11));
  AssertTrue(g.NonEmpty);
end;

procedure TSparseGraphTest.NonEmptyDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.NonEmpty);
  AssertTrue(g.AddVertex(11));
  AssertTrue(g.NonEmpty);
end;

procedure TSparseGraphTest.EnsureCapacity;
var
  Ref: TGraphRef;
  g: TGraph;
  c: SizeInt;
begin
  g := {%H-}Ref;
  g.AddVertex(11);
  c := g.Capacity;
  AssertTrue(c > 0);
  g.EnsureCapacity(c*2);
  AssertTrue(g.Capacity >= c*2);
end;

procedure TSparseGraphTest.EnsureCapacityDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
  c: SizeInt;
begin
  g := {%H-}Ref;
  g.AddVertex(11);
  c := g.Capacity;
  AssertTrue(c > 0);
  g.EnsureCapacity(c*2);
  AssertTrue(g.Capacity >= c*2);
end;

procedure TSparseGraphTest.Clear;
var
  Ref: TGraphRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  g.AddVertex(11);
  g.AddVertex(5);
  AssertTrue(g.VertexCount > 0);
  AssertTrue(g.Capacity > 0);
  g.Clear;
  AssertTrue(g.VertexCount = 0);
  AssertTrue(g.Capacity = 0);
end;

procedure TSparseGraphTest.ClearDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
begin
  g := {%H-}Ref;
  g.AddVertex(11);
  g.AddVertex(5);
  AssertTrue(g.VertexCount > 0);
  AssertTrue(g.Capacity > 0);
  g.Clear;
  AssertTrue(g.VertexCount = 0);
  AssertTrue(g.Capacity = 0);
end;

procedure TSparseGraphTest.AddVertex;
var
  Ref: TGraphRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertTrue(g.VertexCount = 0);
  AssertTrue(g.AddVertex(3));
  AssertTrue(g.VertexCount = 1);
  AssertFalse(g.AddVertex(3));
  AssertTrue(g.VertexCount = 1);
  AssertTrue(g.AddVertex(2));
  AssertTrue(g.VertexCount = 2);
end;

procedure TSparseGraphTest.AddVertexDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
begin
  g := {%H-}Ref;
  AssertTrue(g.VertexCount = 0);
  AssertTrue(g.AddVertex(3));
  AssertTrue(g.VertexCount = 1);
  AssertFalse(g.AddVertex(3));
  AssertTrue(g.VertexCount = 1);
  AssertTrue(g.AddVertex(2));
  AssertTrue(g.VertexCount = 2);
end;

procedure TSparseGraphTest.RemoveVertex;
var
  Ref: TGraphRef;
  g: TGraph;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  AssertTrue(g.AddVertex(3));
  AssertTrue(g.VertexCount = 1);
  g.RemoveVertex(3);
  AssertTrue(g.VertexCount = 0);
  AssertTrue(g.AddVertex(5));
  try
    g.RemoveVertex(2);
  except
    on e: EGraphError do
      Raised := True;
  end;
  AssertTrue(Raised);
  AssertTrue(g.VertexCount = 1);
end;

procedure TSparseGraphTest.RemoveVertexDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  AssertTrue(g.AddVertex(3));
  AssertTrue(g.VertexCount = 1);
  g.RemoveVertex(3);
  AssertTrue(g.VertexCount = 0);
  AssertTrue(g.AddVertex(5));
  try
    g.RemoveVertex(2);
  except
    on e: EGraphError do
      Raised := True;
  end;
  AssertTrue(Raised);
  AssertTrue(g.VertexCount = 1);
end;

procedure TSparseGraphTest.IndexOf;
var
  Ref: TGraphRef;
  g: TGraph;
  I: SizeInt;
begin
  g := {%H-}Ref;
  I := g.IndexOf(5);
  AssertTrue(I = NULL_INDEX);
  g.AddVertex(5);
  I := g.IndexOf(5);
  AssertTrue(I = 0);
  g.AddVertex(2);
  I := g.IndexOf(2);
  AssertTrue(I = 1);
  g.RemoveVertex(2);
  I := g.IndexOf(2);
  AssertTrue(I = NULL_INDEX);
end;

procedure TSparseGraphTest.IndexOfDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
  I: SizeInt;
begin
  g := {%H-}Ref;
  I := g.IndexOf(5);
  AssertTrue(I = NULL_INDEX);
  g.AddVertex(5);
  I := g.IndexOf(5);
  AssertTrue(I = 0);
  g.AddVertex(2);
  I := g.IndexOf(2);
  AssertTrue(I = 1);
  g.RemoveVertex(2);
  I := g.IndexOf(2);
  AssertTrue(I = NULL_INDEX);
end;

procedure TSparseGraphTest.Items;
var
  Ref: TGraphRef;
  g: TGraph;
  I: SizeInt;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  AssertTrue(g.AddVertex(5, I));
  AssertTrue(g[I] = 5);
  AssertTrue(g.AddVertex(7, I));
  AssertTrue(g[I] = 7);
  AssertTrue(g.VertexCount = 2);
  try
    I := g[2];
  except
    on e: EGraphError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TSparseGraphTest.ItemsDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
  I: SizeInt;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  AssertTrue(g.AddVertex(5, I));
  AssertTrue(g[I] = 5);
  AssertTrue(g.AddVertex(7, I));
  AssertTrue(g[I] = 7);
  AssertTrue(g.VertexCount = 2);
  try
    I := g[2];
  except
    on e: EGraphError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TSparseGraphTest.RemoveVertexI;
var
  Ref: TGraphRef;
  g: TGraph;
  I: SizeInt;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  AssertTrue(g.AddVertex(3));
  AssertTrue(g.VertexCount = 1);
  I := g.IndexOf(3);
  g.RemoveVertexI(I);
  AssertTrue(g.VertexCount = 0);
  AssertTrue(g.AddVertex(5));
  I := g.IndexOf(3);
  try
    g.RemoveVertexI(I);
  except
    on e: EGraphError do
      Raised := True;
  end;
  AssertTrue(Raised);
  AssertTrue(g.VertexCount = 1);
end;

procedure TSparseGraphTest.RemoveVertexIDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
  I: SizeInt;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  AssertTrue(g.AddVertex(3));
  AssertTrue(g.VertexCount = 1);
  I := g.IndexOf(3);
  g.RemoveVertexI(I);
  AssertTrue(g.VertexCount = 0);
  AssertTrue(g.AddVertex(5));
  I := g.IndexOf(3);
  try
    g.RemoveVertexI(I);
  except
    on e: EGraphError do
      Raised := True;
  end;
  AssertTrue(Raised);
  AssertTrue(g.VertexCount = 1);
end;

procedure TSparseGraphTest.ContainsVertex;
var
  Ref: TGraphRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.ContainsVertex(3));
  g.AddVertex(3);
  AssertTrue(g.ContainsVertex(3));
  AssertFalse(g.ContainsVertex(5));
  g.AddVertex(5);
  AssertTrue(g.ContainsVertex(5));
end;

procedure TSparseGraphTest.ContainsVertexDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.ContainsVertex(3));
  g.AddVertex(3);
  AssertTrue(g.ContainsVertex(3));
  AssertFalse(g.ContainsVertex(5));
  g.AddVertex(5);
  AssertTrue(g.ContainsVertex(5));
end;

procedure TSparseGraphTest.AddEdge;
var
  Ref: TGraphRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.ContainsEdge(3, 5));
  AssertTrue(g.AddEdge(3, 5));
  AssertTrue(g.ContainsEdge(3, 5));
  AssertFalse(g.AddEdge(3, 5));
end;

procedure TSparseGraphTest.AddArc;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.ContainsEdge(3, 5));
  AssertTrue(g.AddEdge(3, 5));
  AssertTrue(g.ContainsEdge(3, 5));
  AssertFalse(g.AddEdge(3, 5));
end;

procedure TSparseGraphTest.AddEdgeI;
var
  Ref: TGraphRef;
  g: TGraph;
  I, J: SizeInt;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  I := g.IndexOf(3);
  J := g.IndexOf(5);
  AssertFalse(g.ContainsEdgeI(I, J));
  try
    g.AddEdgeI(I, J);
  except
    on e: EGraphError do
      Raised := True;
  end;
  AssertTrue(Raised);
  AssertTrue(g.AddVertex(3, I));
  AssertTrue(g.AddVertex(5, J));
  AssertTrue(g.AddEdgeI(I, J));
  AssertTrue(g.ContainsEdgeI(I, J));
end;

procedure TSparseGraphTest.AddEdgeIDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
  I, J: SizeInt;
  Raised: Boolean = False;
begin
  g := {%H-}Ref;
  I := g.IndexOf(3);
  J := g.IndexOf(5);
  AssertFalse(g.ContainsEdgeI(I, J));
  try
    g.AddEdgeI(I, J);
  except
    on e: EGraphError do
      Raised := True;
  end;
  AssertTrue(Raised);
  AssertTrue(g.AddVertex(3, I));
  AssertTrue(g.AddVertex(5, J));
  AssertTrue(g.AddEdgeI(I, J));
  AssertTrue(g.ContainsEdgeI(I, J));
end;

procedure TSparseGraphTest.Adjacent;
var
  Ref: TGraphRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.Adjacent(3, 5));
  g.AddVertex(3);
  g.AddVertex(5);
  AssertFalse(g.Adjacent(3, 5));
  g.AddEdge(3, 5);
  AssertTrue(g.Adjacent(3, 5));
  AssertTrue(g.Adjacent(5, 3));
end;

procedure TSparseGraphTest.AdjacentDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.Adjacent(3, 5));
  g.AddVertex(3);
  g.AddVertex(5);
  AssertFalse(g.Adjacent(3, 5));
  g.AddEdge(3, 5);
  AssertTrue(g.Adjacent(3, 5));
  AssertFalse(g.Adjacent(5, 3));
end;

procedure TSparseGraphTest.AdjVertices;
var
  Ref: TGraphRef;
  g: TGraph;
  I, Node: SizeInt;
begin
  g := {%H-}Ref;
  g.AddVertex(3);
  I := 0;
  for Node in g.AdjVertices(3) do
    Inc(I);
  AssertTrue(I = 0);
  g.AddEdge(3, 5);
  g.AddEdge(3, 7);
  g.AddEdge(9, 3);
  I := 0;
  for Node in g.AdjVertices(3) do
    Inc(I);
  AssertTrue(I = 3);
end;

procedure TSparseGraphTest.AdjVerticesDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
  I, Node: SizeInt;
begin
  g := {%H-}Ref;
  g.AddVertex(3);
  I := 0;
  for Node in g.AdjVertices(3) do
    Inc(I);
  AssertTrue(I = 0);
  g.AddEdge(3, 5);
  g.AddEdge(3, 7);
  g.AddEdge(9, 3);
  I := 0;
  for Node in g.AdjVertices(3) do
    Inc(I);
  AssertTrue(I = 2);
end;

procedure TSparseGraphTest.AdjVerticesI;
var
  Ref: TGraphRef;
  g: TGraph;
  I, J, Node: SizeInt;
begin
  g := {%H-}Ref;
  g.AddVertex(3, J);
  I := 0;
  for Node in g.AdjVerticesI(J) do
    Inc(I);
  AssertTrue(I = 0);
  g.AddEdge(3, 5);
  g.AddEdge(3, 7);
  g.AddEdge(9, 3);
  I := 0;
  for Node in g.AdjVerticesI(J) do
    Inc(I);
  AssertTrue(I = 3);
end;

procedure TSparseGraphTest.AdjVerticesIDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
  I, J, Node: SizeInt;
begin
  g := {%H-}Ref;
  g.AddVertex(3, J);
  I := 0;
  for Node in g.AdjVerticesI(J) do
    Inc(I);
  AssertTrue(I = 0);
  g.AddEdge(3, 5);
  g.AddEdge(3, 7);
  g.AddEdge(9, 3);
  I := 0;
  for Node in g.AdjVerticesI(J) do
    Inc(I);
  AssertTrue(I = 2);
end;

procedure TSparseGraphTest.Vertices;
var
  Ref: TGraphRef;
  g: TGraph;
  Vert: Integer;
  I: SizeInt;
begin
  g := {%H-}Ref;
  I := 0;
  for Vert in g.Vertices do
    Inc(I);
  g.AddVertex(3);
  I := 0;
  for Vert in g.Vertices do
    Inc(I);
  AssertTrue(I = 1);
  g.AddVertex(5);
  g.AddVertex(7);
  g.AddVertex(9);
  I := 0;
  for Vert in g.Vertices do
    Inc(I);
  AssertTrue(I = 4);
end;

procedure TSparseGraphTest.VerticesDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
  Vert: Integer;
  I: SizeInt;
begin
  g := {%H-}Ref;
  I := 0;
  for Vert in g.Vertices do
    Inc(I);
  g.AddVertex(3);
  I := 0;
  for Vert in g.Vertices do
    Inc(I);
  AssertTrue(I = 1);
  g.AddVertex(5);
  g.AddVertex(7);
  g.AddVertex(9);
  I := 0;
  for Vert in g.Vertices do
    Inc(I);
  AssertTrue(I = 4);
end;

procedure TSparseGraphTest.CreateAdjacencyMatrix;
var
  Ref: TGraphRef;
  g: TGraph;
  m: TGraph.TAdjacencyMatrix;
  I, J: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  m := g.CreateAdjacencyMatrix;
  AssertTrue(g.VertexCount = 13);
  AssertTrue(m.Size = g.VertexCount);
  for I := 0 to Pred(g.VertexCount) do
    for J := 0 to Pred(g.VertexCount) do
      if I <> J then
        AssertFalse(g.ContainsEdgeI(I, J) xor m[I, J]);
end;

procedure TSparseGraphTest.CreateAdjacencyMatrixDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
  m: TDiGraph.TAdjacencyMatrix;
  I, J: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  m := g.CreateAdjacencyMatrix;
  AssertTrue(g.VertexCount = 13);
  AssertTrue(m.Size = g.VertexCount);
  for I := 0 to Pred(g.VertexCount) do
    for J := 0 to Pred(g.VertexCount) do
      if I <> J then
        AssertFalse(g.ContainsEdgeI(I, J) xor m[I, J]);
end;

procedure TSparseGraphTest.IsBipartite;
var
  Ref: TGraphRef;
  g: TGraph;
  Whites, Grays: TIntArray;
  I, J: Integer;
begin
  {%H-}Ref.Instance := GenerateTestGrBip1;
  g := Ref;
  AssertTrue(g.VertexCount = 16);
  AssertTrue(g.IsBipartite(Whites, Grays));
  AssertTrue(Whites.Length + Grays.Length = g.VertexCount);
  for I := 0 to Pred(Whites.Length) do
    for J := 0 to Pred(Whites.Length) do
      if I <> J then
        AssertFalse(g.AdjacentI(Whites[I], Whites[J]));
  for I := 0 to Pred(Grays.Length) do
    for J := 0 to Pred(Grays.Length) do
      if I <> J then
        AssertFalse(g.AdjacentI(Grays[I], Grays[J]));
end;

procedure TSparseGraphTest.IsBipartiteDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
  Whites, Grays: TIntArray;
  I, J: Integer;
begin
  {%H-}Ref.Instance := GenerateTestDigrBip1;
  g := Ref;
  AssertTrue(g.VertexCount = 12);
  AssertTrue(g.IsBipartite(Whites, Grays));
  AssertTrue(Whites.Length + Grays.Length = g.VertexCount);
  for I := 0 to Pred(Whites.Length) do
    for J := 0 to Pred(Whites.Length) do
      if I <> J then
        AssertFalse(g.AdjacentI(Whites[I], Whites[J]));
  for I := 0 to Pred(Grays.Length) do
    for J := 0 to Pred(Grays.Length) do
      if I <> J then
        AssertFalse(g.AdjacentI(Grays[I], Grays[J]));
end;

procedure TSparseGraphTest.DfsTraversal;
var
  Ref: TGraphRef;
  g: TGraph;
  vCount: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  vCount := g.VertexCount;
  AssertTrue(vCount = 13);
  FFound.InitRange(vCount);
  FDone.InitRange(vCount);
  AssertTrue(FFound.PopCount = vCount);
  AssertTrue(FDone.PopCount = vCount);
  AssertTrue(g.DfsTraversal(0, @vFound, @vDone) = vCount);
  AssertTrue(FFound.IsEmpty);
  AssertTrue(FDone.IsEmpty);
end;

procedure TSparseGraphTest.DfsTraversalDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
  vCount: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  vCount := g.VertexCount;
  AssertTrue(vCount = 13);
  FFound.InitRange(vCount);
  FDone.InitRange(vCount);
  AssertTrue(FFound.PopCount = vCount);
  AssertTrue(FDone.PopCount = vCount);
  AssertTrue(g.DfsTraversal(0, @vFound, @vDone) = vCount - 2);
  AssertTrue(FFound.PopCount = 2);
  AssertTrue(FFound[7]);
  AssertTrue(FFound[8]);
  AssertTrue(FDone.PopCount = 2);
  AssertTrue(FDone[7]);
  AssertTrue(FDone[8]);
end;

procedure TSparseGraphTest.BfsTraversal;
var
  Ref: TGraphRef;
  g: TGraph;
  vCount: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  vCount := g.VertexCount;
  AssertTrue(vCount = 13);
  FFound.InitRange(vCount);
  FDone.InitRange(vCount);
  AssertTrue(FFound.PopCount = vCount);
  AssertTrue(g.BfsTraversal(0, @vFound, @vDone) = vCount);
  AssertTrue(FFound.IsEmpty);
  AssertTrue(FDone.IsEmpty);
end;

procedure TSparseGraphTest.BfsTraversalDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
  vCount: SizeInt;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  vCount := g.VertexCount;
  AssertTrue(vCount = 13);
  FFound.InitRange(vCount);
  FDone.InitRange(vCount);
  AssertTrue(g.BfsTraversal(0, @vFound, @vDone) = vCount - 2);
  AssertTrue(FFound.PopCount = 2);
  AssertTrue(FFound[7]);
  AssertTrue(FFound[8]);
  AssertTrue(FDone.PopCount = 2);
  AssertTrue(FDone[7]);
  AssertTrue(FDone[8]);
end;

procedure TSparseGraphTest.ShortestPaths;
var
  Ref: TGraphRef;
  g: TGraph;
  Paths: TIntArray;
const
  RightPaths: array[1..13] of SizeInt = (0, 1, 1, 1, 2, 1, 1, 2, 3, 2, 3, 3, 3);
begin
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  Paths := g.ShortestPathsMap(0);
  AssertTrue(THelper.Same(Paths, RightPaths));
end;

procedure TSparseGraphTest.ShortestPathsDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
  Paths: TIntArray;
const
  RightPaths: array[1..13] of SizeInt = (0, 1, 1, 1, 2, 1, 1, -1, -1, 2, 3, 3, 3);
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  Paths := g.ShortestPathsMap(0);
  AssertTrue(THelper.Same(Paths, RightPaths));
end;

procedure TSparseGraphTest.Eccentricity;
var
  Ref: TGraphRef;
  g: TGraph;
begin
  {%H-}Ref.Instance := GenerateTestGr1;
  g := Ref;
  AssertTrue(g.Eccentricity(0) = 3);
  AssertTrue(g.Eccentricity(1) = 4);
  AssertTrue(g.Eccentricity(2) = 4);
  AssertTrue(g.Eccentricity(3) = 4);
  AssertTrue(g.Eccentricity(4) = 3);
  AssertTrue(g.Eccentricity(5) = 4);
  AssertTrue(g.Eccentricity(6) = 2);
  AssertTrue(g.Eccentricity(7) = 3);
  AssertTrue(g.Eccentricity(8) = 4);
  AssertTrue(g.Eccentricity(9) = 3);
  AssertTrue(g.Eccentricity(10) = 4);
  AssertTrue(g.Eccentricity(11) = 4);
  AssertTrue(g.Eccentricity(12) = 4);
end;

procedure TSparseGraphTest.EccentricityDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  AssertTrue(g.Eccentricity(0) = High(SizeInt));
  AssertTrue(g.Eccentricity(1) = High(SizeInt));
  AssertTrue(g.Eccentricity(2) = High(SizeInt));
  AssertTrue(g.Eccentricity(3) = High(SizeInt));
  AssertTrue(g.Eccentricity(4) = High(SizeInt));
  AssertTrue(g.Eccentricity(5) = High(SizeInt));
  AssertTrue(g.Eccentricity(6) = High(SizeInt));
  AssertTrue(g.Eccentricity(7) = High(SizeInt));
  AssertTrue(g.Eccentricity(8) = High(SizeInt));
  AssertTrue(g.Eccentricity(9) = High(SizeInt));
  AssertTrue(g.Eccentricity(10) = High(SizeInt));
  AssertTrue(g.Eccentricity(11) = High(SizeInt));
  AssertTrue(g.Eccentricity(12) = High(SizeInt));
end;

{ TTspTest }

function TTspTest.CreateBr17: TSolver.TTspMatrix;
begin
  //br17 from TSPLib, optimal tour cost is 39:
  Result := [
    [0,   3,  5, 48, 48,  8,  8,  5,  5,  3,  3,  0,  3,  5,  8,  8,  5],
    [3,   0,  3, 48, 48,  8,  8,  5,  5,  0,  0,  3,  0,  3,  8,  8,  5],
    [5,   3,  0, 72, 72, 48, 48, 24, 24,  3,  3,  5,  3,  0, 48, 48, 24],
    [48, 48, 74,  0,  0,  6,  6, 12, 12, 48, 48, 48, 48, 74,  6,  6, 12],
    [48, 48, 74,  0,  0,  6,  6, 12, 12, 48, 48, 48, 48, 74,  6,  6, 12],
    [8,   8, 50,  6,  6,  0,  0,  8,  8,  8,  8,  8,  8, 50,  0,  0,  8],
    [8,   8, 50,  6,  6,  0,  0,  8,  8,  8,  8,  8,  8, 50,  0,  0,  8],
    [5,   5, 26, 12, 12,  8,  8,  0,  0,  5,  5,  5,  5, 26,  8,  8,  0],
    [5,   5, 26, 12, 12,  8,  8,  0,  0,  5,  5,  5,  5, 26,  8,  8,  0],
    [3,   0,  3, 48, 48,  8,  8,  5,  5,  0,  0,  3,  0,  3,  8,  8,  5],
    [3,   0,  3, 48, 48,  8,  8,  5,  5,  0,  0,  3,  0,  3,  8,  8,  5],
    [0,   3,  5, 48, 48,  8,  8,  5,  5,  3,  3,  0,  3,  5,  8,  8,  5],
    [3,   0,  3, 48, 48,  8,  8,  5,  5,  0,  0,  3,  0,  3,  8,  8,  5],
    [5,   3,  0, 72, 72, 48, 48, 24, 24,  3,  3,  5,  3,  0, 48, 48, 24],
    [8,   8, 50,  6,  6,  0,  0,  8,  8,  8,  8,  8,  8, 50,  0,  0,  8],
    [8,   8, 50,  6,  6,  0,  0,  8,  8,  8,  8,  8,  8, 50,  0,  0,  8],
    [5,   5, 26, 12, 12,  8,  8,  0,  0,  5,  5,  5,  5, 26,  8,  8,  0]];
end;

function TTspTest.CreateSymm8: TSolver.TTspMatrix;
begin
  Result := [       //optimal tour cost is 70     - 0  5  7  4  1  2  6  3  0
    [ 0, 15, 14,  8, 22, 11,  6,  3],              //11 10  4 12  7 13  5  8
    [15,  0,  7,  9, 12, 14, 26, 23],
    [14,  7,  0, 17, 21, 19, 13, 10],
    [ 8,  9, 17,  0, 15, 16,  5, 20],
    [22, 12, 21, 15,  0, 18, 12,  4],
    [11, 14, 19, 16, 18,  0, 23, 10],
    [ 6, 26, 13,  5, 12, 23,  0, 14],
    [ 3, 23, 10, 20,  4, 10, 14,  0]];
end;

function TTspTest.CreateGr17: TSolver.TTspMatrix;
begin
  //gr17 from TSPLib, optimal tour cost is 2085:
  Result := [
    [  0, 633, 257,  91, 412, 150,  80, 134, 259, 505, 353, 324,  70, 211, 268, 246, 121],
    [633,   0, 390, 661, 227, 488, 572, 530, 555, 289, 282, 638, 567, 466, 420, 745, 518],
    [257, 390,   0, 228, 169, 112, 196, 154, 372, 262, 110, 437, 191,  74,  53, 472, 142],
    [ 91, 661, 228,   0, 383, 120,  77, 105, 175, 476, 324, 240,  27, 182, 239, 237,  84],
    [412, 227, 169, 383,   0, 267, 351, 309, 338, 196,  61, 421, 346, 243, 199, 528, 297],
    [150, 488, 112, 120, 267,   0,  63,  34, 264, 360, 208, 329,  83, 105, 123, 364,  35],
    [ 80, 572, 196,  77, 351,  63,   0,  29, 232, 444, 292, 297,  47, 150, 207, 332,  29],
    [134, 530, 154, 105, 309,  34,  29,   0, 249, 402, 250, 314,  68, 108, 165, 349,  36],
    [259, 555, 372, 175, 338, 264, 232, 249,   0, 495, 352,  95, 189, 326, 383, 202, 236],
    [505, 289, 262, 476, 196, 360, 444, 402, 495,   0, 154, 578, 439, 336, 240, 685, 390],
    [353, 282, 110, 324,  61, 208, 292, 250, 352, 154,   0, 435, 287, 184, 140, 542, 238],
    [324, 638, 437, 240, 421, 329, 297, 314,  95, 578, 435,   0, 254, 391, 448, 157, 301],
    [ 70, 567, 191,  27, 346,  83,  47,  68, 189, 439, 287, 254,   0, 145, 202, 289,  55],
    [211, 466,  74, 182, 243, 105, 150, 108, 326, 336, 184, 391, 145,   0,  57, 426,  96],
    [268, 420,  53, 239, 199, 123, 207, 165, 383, 240, 140, 448, 202,  57,   0, 483, 153],
    [246, 745, 472, 237, 528, 364, 332, 349, 202, 685, 542, 157, 289, 426, 483,   0, 336],
    [121, 518, 142,  84, 297,  35,  29,  36, 236, 390, 238, 301,  55,  96, 153, 336,   0]];
end;

procedure TTspTest.Asymmetric;
var
  m: TSolver.TTspMatrix;
  Tour: TIntArray;
  Cost: Integer;
  Exact: Boolean;
begin
  m := CreateBr17;
  Exact := TSolver.FindExact(m, Tour, Cost, 30);
  AssertTrue(Exact);
  AssertTrue(TSolver.GetTotalCost(m, Tour) = Cost);
  AssertTrue(Cost = 39);
end;

procedure TTspTest.Symmetric;
var
  m: TSolver.TTspMatrix;
  Tour: TIntArray;
  Cost: Integer;
  Exact: Boolean;
begin
  m := CreateSymm8;
  Exact := TSolver.FindExact(m, Tour, Cost, 30);
  AssertTrue(Exact);
  AssertTrue(TSolver.GetTotalCost(m, Tour) = Cost);
  AssertTrue(Cost = 70);
end;

procedure TTspTest.Symmetric1;
var
  m: TSolver.TTspMatrix;
  Tour: TIntArray;
  Cost: Integer;
  Exact: Boolean;
begin
  m := CreateGr17;
  Exact := TSolver.FindExact(m, Tour, Cost, 30);
  AssertTrue(Exact);
  AssertTrue(TSolver.GetTotalCost(m, Tour) = Cost);
  AssertTrue(Cost = 2085);
end;


initialization
  RegisterTest(TSparseGraphTest);
  RegisterTest(TTspTest);
end.

