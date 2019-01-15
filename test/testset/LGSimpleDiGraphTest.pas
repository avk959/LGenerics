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
  published
    procedure AddVertices;
    procedure SaveToStream;
    procedure Clone;
    procedure Reverse;
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
    procedure PathExists;
    procedure PathExists1;
    procedure ContainsCycle;
    procedure ContainsCycle1;
    procedure ContainsEulerianCycle;
    procedure ContainsEulerianCycle1;
    procedure FindEulerianCycle;
    procedure FindStrongComponents;
    procedure FindStrongComponents1;
    procedure FindStrongComponents2;
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



initialization

  RegisterTest(TSimpleDiGraphTest);
end.

