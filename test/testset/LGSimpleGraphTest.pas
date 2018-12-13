unit LGSimpleGraphTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  LGUtils,
  LGArrayHelpers,
  LGSparseGraph,
  LGSimpleGraph;


type

  { TSimpleGraphTest }

  TSimpleGraphTest = class(TTestCase)
  private
  type
    TGraph  = TIntChart;
    TRef    = specialize TGAutoRef<TGraph>;
    THelper = specialize TGOrdinalArrayHelper<SizeInt>;

    function  GenerateTestGr1: TGraph;
    function  GenerateTestGr2: TGraph;
  published
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
  end;

implementation

function TSimpleGraphTest.GenerateTestGr1: TGraph;
begin
  Result := TGraph.Create;
  Result.AddVertexRange(0, 12);
  Result.AddEdges([0, 1, 0, 2, 0, 3, 0, 5, 0, 6, 2, 3, 3, 5, 3, 4, 6, 4, 4, 9, 6, 9, 7, 6,
                   8, 7, 9, 10, 9, 11, 9, 12, 11, 12]);
end;

function TSimpleGraphTest.GenerateTestGr2: TGraph;
begin
  Result := TGraph.Create;
  Result.AddVertexRange(1, 16);
  Result.AddEdges([1, 2, 1, 3, 1, 4, 1, 6, 1, 7, 3, 4, 4, 6, 4, 5, 7, 4, 5, 10, 7, 10,
                   10, 11, 10, 12, 10, 13, 12, 13,
                   8, 9, 8, 14, 8, 15, 9, 15, 9, 16, 14, 15, 14, 16]);
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


initialization
  RegisterTest(TSimpleGraphTest);
end.

