unit LGSparseGraphTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  LGUtils,
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
  end;

implementation

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


initialization
  RegisterTest(SparseGraphTest);
end.

