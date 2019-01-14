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

    function  GenerateTestDigr1: TGraph;
  published
    procedure AddVertices;
    procedure SaveToStream;
    procedure Reverse;
    procedure SymmDifferenceOf;
    procedure SymmDifferenceOf1;
    procedure SymmDifferenceOf2;
    procedure SetUnionOf;
    procedure SetIntersectionOf;
    procedure SetIntersectionOf2;
  end;

implementation

function TSimpleDiGraphTest.GenerateTestDigr1: TGraph;
begin
  Result := TGraph.Create;  //TestDigr1.png
  Result.AddVertexRange(0, 12);
  Result.AddEdges([0, 1, 0, 2, 0, 3, 0, 5, 0, 6, 2, 3, 3, 5, 3, 4, 6, 4, 4, 9, 6, 9, 7, 6,
                   8, 7, 9, 10, 9, 11, 9, 12, 11, 12]);
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



initialization

  RegisterTest(TSimpleDiGraphTest);
end.

