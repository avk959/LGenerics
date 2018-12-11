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

  { SparseGraphTest }

  SparseGraphTest = class(TTestCase)
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
    function  vFound({%H-}aSender: TObject; aIndex: SizeInt): Boolean;
    procedure vDone({%H-}aSender: TObject; aIndex: SizeInt);
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

implementation

function SparseGraphTest.GenerateTestGrBip1: TGraph;
begin
  Result := TGraph.Create;
  Result.AddVertexRange(1, 16);
  Result.AddEdges([1, 2, 1, 4, 1, 6, 3, 4, 3, 6, 3, 8, 5, 6, 5, 8, 5, 10, 7, 8, 7, 10, 7,
                   12, 9, 10, 9, 12, 9, 14, 11, 12, 11, 14, 11, 16, 13, 14, 13, 16, 15, 16]);
end;

function SparseGraphTest.GenerateTestDigrBip1: TDiGraph;
begin
  Result := TDiGraph.Create;
  Result.AddVertexRange(1, 12);
  Result.AddEdges([1, 2, 1, 4, 1, 6, 2, 3, 3, 4, 3, 6, 3, 8, 4, 5, 5, 6, 5, 8, 5, 10,
                   6, 7, 7, 8, 7, 10, 7, 12, 8, 9, 9, 10, 9, 12, 10, 11, 11, 8, 12, 1]);
end;

function SparseGraphTest.GenerateTestGr1: TGraph;
begin
  Result := TGraph.Create;
  Result.AddVertexRange(0, 12);
  Result.AddEdges([0, 1, 0, 2, 0, 3, 0, 5, 0, 6, 2, 3, 3, 5, 3, 4, 6, 4, 4, 9, 6, 9, 7, 6,
                   8, 7, 9, 10, 9, 11, 9, 12, 11, 12]);
end;

function SparseGraphTest.GenerateTestDigr1: TDiGraph;
begin
  Result := TDiGraph.Create;
  Result.AddVertexRange(0, 12);
  Result.AddEdges([0, 1, 0, 2, 0, 3, 0, 5, 0, 6, 2, 3, 3, 5, 3, 4, 6, 4, 4, 9, 6, 9, 7, 6,
                   8, 7, 9, 10, 9, 11, 9, 12, 11, 12]);
end;

function SparseGraphTest.vFound(aSender: TObject; aIndex: SizeInt): Boolean;
begin
  FFound[aIndex] := False;
  Result := True;
end;

procedure SparseGraphTest.vDone(aSender: TObject; aIndex: SizeInt);
begin
  FDone[aIndex] := False;
end;

procedure SparseGraphTest.IsEmpty;
var
  Ref: TGraphRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertTrue(g.IsEmpty);
  AssertTrue(g.AddVertex(3));
  AssertFalse(g.IsEmpty);
end;

procedure SparseGraphTest.IsEmptyDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
begin
  g := {%H-}Ref;
  AssertTrue(g.IsEmpty);
  AssertTrue(g.AddVertex(3));
  AssertFalse(g.IsEmpty);
end;

procedure SparseGraphTest.NonEmpty;
var
  Ref: TGraphRef;
  g: TGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.NonEmpty);
  AssertTrue(g.AddVertex(11));
  AssertTrue(g.NonEmpty);
end;

procedure SparseGraphTest.NonEmptyDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
begin
  g := {%H-}Ref;
  AssertFalse(g.NonEmpty);
  AssertTrue(g.AddVertex(11));
  AssertTrue(g.NonEmpty);
end;

procedure SparseGraphTest.EnsureCapacity;
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

procedure SparseGraphTest.EnsureCapacityDirect;
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

procedure SparseGraphTest.Clear;
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

procedure SparseGraphTest.ClearDirect;
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

procedure SparseGraphTest.AddVertex;
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

procedure SparseGraphTest.AddVertexDirect;
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

procedure SparseGraphTest.RemoveVertex;
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

procedure SparseGraphTest.RemoveVertexDirect;
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

procedure SparseGraphTest.IndexOf;
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

procedure SparseGraphTest.IndexOfDirect;
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

procedure SparseGraphTest.Items;
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

procedure SparseGraphTest.ItemsDirect;
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

procedure SparseGraphTest.RemoveVertexI;
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

procedure SparseGraphTest.RemoveVertexIDirect;
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

procedure SparseGraphTest.ContainsVertex;
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

procedure SparseGraphTest.ContainsVertexDirect;
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

procedure SparseGraphTest.AddEdge;
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

procedure SparseGraphTest.AddArc;
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

procedure SparseGraphTest.AddEdgeI;
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

procedure SparseGraphTest.AddEdgeIDirect;
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

procedure SparseGraphTest.Adjacent;
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

procedure SparseGraphTest.AdjacentDirect;
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

procedure SparseGraphTest.AdjVertices;
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

procedure SparseGraphTest.AdjVerticesDirect;
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

procedure SparseGraphTest.AdjVerticesI;
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

procedure SparseGraphTest.AdjVerticesIDirect;
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

procedure SparseGraphTest.Vertices;
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

procedure SparseGraphTest.VerticesDirect;
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

procedure SparseGraphTest.IsBipartite;
var
  Ref: TGraphRef;
  g: TGraph;
  Whites, Grays: TIntArray;
  I, J: Integer;
begin
  Ref.Instance := GenerateTestGrBip1;
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

procedure SparseGraphTest.IsBipartiteDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
  Whites, Grays: TIntArray;
  I, J: Integer;
begin
  Ref.Instance := GenerateTestDigrBip1;
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

procedure SparseGraphTest.DfsTraversal;
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

procedure SparseGraphTest.DfsTraversalDirect;
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

procedure SparseGraphTest.BfsTraversal;
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
  AssertTrue(FFound.PopCount = vCount);
  AssertTrue(g.BfsTraversal(0, @vFound) = vCount);
  AssertTrue(FFound.IsEmpty);
end;

procedure SparseGraphTest.BfsTraversalDirect;
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
  AssertTrue(FFound.PopCount = vCount);
  AssertTrue(g.BfsTraversal(0, @vFound) = vCount - 2);
  AssertTrue(FFound.PopCount = 2);
  AssertTrue(FFound[7]);
  AssertTrue(FFound[8]);
end;

procedure SparseGraphTest.ShortestPaths;
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

procedure SparseGraphTest.ShortestPathsDirect;
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

procedure SparseGraphTest.Eccentricity;
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

procedure SparseGraphTest.EccentricityDirect;
var
  Ref: TDiGraphRef;
  g: TDiGraph;
begin
  {%H-}Ref.Instance := GenerateTestDigr1;
  g := Ref;
  AssertTrue(g.Eccentricity(0) = 3);
  AssertTrue(g.Eccentricity(1) = 0);
  AssertTrue(g.Eccentricity(2) = 4);
  AssertTrue(g.Eccentricity(3) = 3);
  AssertTrue(g.Eccentricity(4) = 2);
  AssertTrue(g.Eccentricity(5) = 0);
  AssertTrue(g.Eccentricity(6) = 2);
  AssertTrue(g.Eccentricity(7) = 3);
  AssertTrue(g.Eccentricity(8) = 4);
  AssertTrue(g.Eccentricity(9) = 1);
  AssertTrue(g.Eccentricity(10) = 0);
  AssertTrue(g.Eccentricity(11) = 1);
  AssertTrue(g.Eccentricity(12) = 0);
end;


initialization
  RegisterTest(SparseGraphTest);
end.

