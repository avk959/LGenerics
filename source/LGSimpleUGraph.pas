
unit LGSimpleUGraph;

{$mode objfpc}{$H+}
{$INLINE ON}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGArrayHelpers,
  LGStack,
  LGQueue,
  LGVector,
  LGraphUtils,
  LGraphType;

type

  { TGSimpleSparseUGraph simple sparse undirected graph based on adjacency lists;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGSimpleSparseUGraph<TVertex, TEdgeData, TVertexEqRel> = class(
    specialize TGCustomSimpleSparseGraph<TVertex, TEdgeData, TVertexEqRel>)
  protected
  class var
    CFData: TEdgeData;
    procedure DoRemoveVertex(aIndex: SizeInt);
    function  DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
  public
    function  RemoveVertex(constref v: TVertex): Boolean; inline;
    function  RemoveVertexI(aIndex: SizeInt): Boolean;
    function  AddEdge(constref aSrc, aDst: TVertex; aData: TEdgeData): Boolean;
    function  AddEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  AddEdgeI(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  AddEdgeI(aSrc, aDst: SizeInt): Boolean; inline;
    function  RemoveEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  RemoveEdgeI(aSrc, aDst: SizeInt): Boolean;
    function  Degree(constref v: TVertex): SizeInt; inline;
    function  DegreeI(aIndex: SizeInt): SizeInt;
    function  Isolated(constref v: TVertex): Boolean; inline;
    function  IsolatedI(aIndex: SizeInt): Boolean; inline;
    function  EulerPathExistsI(aSrc, aDst: SizeInt): Boolean;
  { returns the vector of the spanning tree, which is constructed starting from aRoot;
    each element contains the index of its parent (or -1 if it is root or not connected) }
    function  SpanningTreeI(aRoot: SizeInt = 0): TIntArray;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGSimpleSparseUGraph }

procedure TGSimpleSparseUGraph.DoRemoveVertex(aIndex: SizeInt);
var
  I, J: SizeInt;
  pV: ^TVertexItem;
  p: ^TAdjItem;
  CurrEdges: TVertexItem.TAdjItemArray;
begin
  FEdgeCount -= FVertexList.FNodeList[aIndex].Item.Count;
  FVertexList.Delete(aIndex);
  for I := 0 to Pred(FVertexList.Count) do
    begin
      pV := @FVertexList.FNodeList[I].Item;
      CurrEdges := pV^.ToArray;
      pV^.ClearItems;
      for J := 0 to System.High(CurrEdges) do
        begin
          if CurrEdges[J].Destination <> aIndex then
            begin
              if CurrEdges[J].Destination > aIndex then
                Dec(CurrEdges[J].Destination);
              pV^.Add(CurrEdges[J]);
            end;
        end;
    end;
end;

function TGSimpleSparseUGraph.DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
var
  p: ^TAdjItem;
begin
  if aSrc = aDst then
    exit(False);
  Result := not FVertexList.FNodeList[aSrc].Item.FindOrAdd(aDst, p);
  if Result then
    begin
      p^.Destination := aDst;
      p^.Data := aData;
      FVertexList.FNodeList[aDst].Item.FindOrAdd(aSrc, p);
      p^.Destination := aSrc;
      p^.Data := aData;
      Inc(FEdgeCount);
    end;
end;

function TGSimpleSparseUGraph.DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
begin
  if aSrc = aDst then
    exit(False);
  Result := FVertexList.FNodeList[aSrc].Item.Remove(aDst);
  if Result then
    begin
      FVertexList.FNodeList[aSrc].Item.Remove(aSrc);
      Dec(FEdgeCount);
    end;
end;

function TGSimpleSparseUGraph.RemoveVertex(constref v: TVertex): Boolean;
begin
  Result := RemoveVertexI(FVertexList.IndexOf(v));
end;

function TGSimpleSparseUGraph.RemoveVertexI(aIndex: SizeInt): Boolean;
begin
  Result := (aIndex >= 0) and (aIndex < FVertexList.Count);
  if Result then
    DoRemoveVertex(aIndex);
end;

function TGSimpleSparseUGraph.AddEdge(constref aSrc, aDst: TVertex; aData: TEdgeData): Boolean;
var
  SrcIdx, DstIdx: SizeInt;
begin
  AddVertex(aSrc, SrcIdx);
  AddVertex(aDst, DstIdx);
  Result := DoAddEdge(SrcIdx, DstIdx, aData);
end;

function TGSimpleSparseUGraph.AddEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := AddEdge(aSrc, aDst, CFData);
end;

function TGSimpleSparseUGraph.AddEdgeI(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
begin
  if (aSrc < 0) or (aSrc >= FVertexList.Count) then
    exit(False);
  if (aDst < 0) or (aDst >= FVertexList.Count) then
    exit(False);
  Result := DoAddEdge(aSrc, aDst, aData);
end;

function TGSimpleSparseUGraph.AddEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := AddEdgeI(aSrc, aDst, CFData);
end;

function TGSimpleSparseUGraph.RemoveEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := RemoveEdgeI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGSimpleSparseUGraph.RemoveEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  if (aSrc < 0) or (aSrc >= FVertexList.Count) then
    exit(False);
  if (aDst < 0) or (aDst >= FVertexList.Count) then
    exit(False);
  Result := DoRemoveEdge(aSrc, aDst);
end;

function TGSimpleSparseUGraph.Degree(constref v: TVertex): SizeInt;
begin
  Result := DegreeI(FVertexList.IndexOf(v));
end;

function TGSimpleSparseUGraph.DegreeI(aIndex: SizeInt): SizeInt;
begin
  Result := FVertexList.FNodeList[aIndex].Item.Count;
end;

function TGSimpleSparseUGraph.Isolated(constref v: TVertex): Boolean;
begin
  Result := Degree(v) = 0;
end;

function TGSimpleSparseUGraph.IsolatedI(aIndex: SizeInt): Boolean;
begin
  Result := DegreeI(aIndex) = 0;
end;

function TGSimpleSparseUGraph.EulerPathExistsI(aSrc, aDst: SizeInt): Boolean;
var
  I: SizeInt;
begin
  if Odd(DegreeI(aSrc) + DegreeI(aDst)) then
    exit(False);
  for I := 0 to Pred(VertexCount) do
    if (I <> aSrc) and (I <> aDst) then
      if Odd(DegreeI(I)) then
        exit(False);
  Result := True;
end;

function TGSimpleSparseUGraph.SpanningTreeI(aRoot: SizeInt): TIntArray;
var
  Visited: TBitVector;
  Stack: TIntStack;
  Parent: SizeInt;
begin
  if IsEmpty then
    exit(nil);
  Visited.Size := VertexCount;
  System.SetLength(Result, VertexCount);
  System.FillChar(Result[0], System.Length(Result) * SizeOf(SizeInt), $ff);
  {%H-}Stack.Push(aRoot);
  repeat
    if not Visited[aRoot] then
      begin
        Parent := aRoot;
        Visited[aRoot] := True;
        for aRoot in AdjVerticesI(aRoot) do
          if not Visited[aRoot] then
            begin
              Result[aRoot] := Parent;
              Stack.Push(aRoot);
            end;
      end;
  until not Stack.TryPop(aRoot);
end;

end.

