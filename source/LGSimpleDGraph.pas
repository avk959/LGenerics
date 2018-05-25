{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic simple directed graph implementation.                           *
*                                                                           *
*   Copyright(c) 2018 A.Koverdyaev(avk)                                     *
*                                                                           *
*   This code is free software; you can redistribute it and/or modify it    *
*   under the terms of the Apache License, Version 2.0;                     *
*   You may obtain a copy of the License at                                 *
*     http://www.apache.org/licenses/LICENSE-2.0.                           *
*                                                                           *
*  Unless required by applicable law or agreed to in writing, software      *
*  distributed under the License is distributed on an "AS IS" BASIS,        *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *
*  See the License for the specific language governing permissions and      *
*  limitations under the License.                                           *
*                                                                           *
*****************************************************************************}
unit LGSimpleDGraph;

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
  LGraphUtils;

type

  { TGSimpleDiGraph is simple sparse directed graph based on adjacency lists;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGSimpleDiGraph<TVertex, TEdgeData, TVertexEqRel> = class(
    specialize TGCustomSimpleGraph<TVertex, TEdgeData, TVertexEqRel>)
  protected
    procedure DoRemoveVertex(aIndex: SizeInt);
    function  DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
  public
  { returns True and vertex index, if it was added, False otherwise }
    function  AddVertex(constref v: TVertex; out aIndex: SizeInt): Boolean; inline;
    function  RemoveVertex(constref v: TVertex): Boolean; inline;
    function  RemoveVertexI(aIndex: SizeInt): Boolean;
    function  AddEdge(constref aSrc, aDst: TVertex; aData: TEdgeData): Boolean;
    function  AddEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  AddEdgeI(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  AddEdgeI(aSrc, aDst: SizeInt): Boolean; inline;
    function  RemoveEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  RemoveEdgeI(aSrc, aDst: SizeInt): Boolean;
    function  InDegree(constref v: TVertex): SizeInt; inline;
    function  InDegreeI(aIndex: SizeInt): SizeInt;
    function  OutDegree(constref v: TVertex): SizeInt; inline;
    function  OutDegreeI(aIndex: SizeInt): SizeInt;
    function  Degree(constref v: TVertex): SizeInt; inline;
    function  DegreeI(aIndex: SizeInt): SizeInt;
    function  Isolated(constref v: TVertex): Boolean; inline;
    function  IsolatedI(aIndex: SizeInt): Boolean; inline;
    function  EulerCycleExists: Boolean;
    function  FindEulerCycle: TIntArray;

    function  Clone: TGSimpleDiGraph;
    function  Reverse: TGSimpleDiGraph;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGSimpleDiGraph }

procedure TGSimpleDiGraph.DoRemoveVertex(aIndex: SizeInt);
var
  I, J: SizeInt;
  p: ^TAdjItem;
  CurrEdges: TVertexItem.TAdjItemArray;
begin
  FEdgeCount -= FVertexList.ItemRefs[aIndex]^.Count;
  //FConnectedValid := False;
  for p in FVertexList.ItemRefs[aIndex]^ do
    Dec(FVertexList.ItemRefs[p^.Destination]^.InDeg);
  FVertexList.Delete(aIndex);
  for I := 0 to Pred(FVertexList.Count) do
    begin
      CurrEdges := FVertexList.ItemRefs[I]^.ToArray;
      FVertexList.FNodeList[I].Item.MakeEmpty;
      for J := 0 to System.High(CurrEdges) do
        begin
          if CurrEdges[J].Destination <> aIndex then
            begin
              if CurrEdges[J].Destination > aIndex then
                Dec(CurrEdges[J].Destination);
              FVertexList.ItemRefs[I]^.Add(CurrEdges[J]);
            end;
        end;
    end;
end;

function TGSimpleDiGraph.DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
var
  p: ^TAdjItem;
begin
  if aSrc = aDst then
    exit(False);
  Result := not FVertexList.ItemRefs[aSrc]^.FindOrAdd(aDst, p);
  if Result then
    begin
      p^.Destination := aDst;
      p^.Data := aData;
      Inc(FVertexList.ItemRefs[aDst]^.InDeg);
      Inc(FEdgeCount);
      //FConnectedValid := False;
    end;
end;

function TGSimpleDiGraph.DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
begin
  if aSrc = aDst then
    exit(False);
  Result := FVertexList.ItemRefs[aSrc]^.Remove(aDst);
  if Result then
    begin
      Dec(FVertexList.ItemRefs[aDst]^.InDeg);
      Dec(FEdgeCount);
      //FConnectedValid := False;
    end;
end;

function TGSimpleDiGraph.AddVertex(constref v: TVertex; out aIndex: SizeInt): Boolean;
begin
  Result := not FVertexList.FindOrAdd(v, aIndex);
  if Result then
    begin
      FVertexList.ItemRefs[aIndex]^.InDeg := 0;
      //FConnectedValid := False;
    end;
end;

function TGSimpleDiGraph.RemoveVertex(constref v: TVertex): Boolean;
begin
  Result := RemoveVertexI(FVertexList.IndexOf(v));
end;

function TGSimpleDiGraph.RemoveVertexI(aIndex: SizeInt): Boolean;
begin
  Result := (aIndex >= 0) and (aIndex < FVertexList.Count);
  if Result then
    DoRemoveVertex(aIndex);
end;

function TGSimpleDiGraph.AddEdge(constref aSrc, aDst: TVertex; aData: TEdgeData): Boolean;
var
  SrcIdx, DstIdx: SizeInt;
begin
  AddVertex(aSrc, SrcIdx);
  AddVertex(aDst, DstIdx);
  Result := DoAddEdge(SrcIdx, DstIdx, aData);
end;

function TGSimpleDiGraph.AddEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := AddEdge(aSrc, aDst, CFData);
end;

function TGSimpleDiGraph.AddEdgeI(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
begin
  if (aSrc < 0) or (aSrc >= FVertexList.Count) then
    exit(False);
  if (aDst < 0) or (aDst >= FVertexList.Count) then
    exit(False);
  Result := DoAddEdge(aSrc, aDst, aData);
end;

function TGSimpleDiGraph.AddEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := AddEdgeI(aSrc, aDst, CFData);
end;

function TGSimpleDiGraph.RemoveEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := RemoveEdgeI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGSimpleDiGraph.RemoveEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  if (aSrc < 0) or (aSrc >= FVertexList.Count) then
    exit(False);
  if (aDst < 0) or (aDst >= FVertexList.Count) then
    exit(False);
  Result := DoRemoveEdge(aSrc, aDst);
end;

function TGSimpleDiGraph.InDegree(constref v: TVertex): SizeInt;
begin
  Result := InDegreeI(FVertexList.IndexOf(v));
end;

function TGSimpleDiGraph.InDegreeI(aIndex: SizeInt): SizeInt;
begin
  FVertexList.CheckIndexRange(aIndex);
  Result := FVertexList.ItemRefs[aIndex]^.InDeg;
end;

function TGSimpleDiGraph.OutDegree(constref v: TVertex): SizeInt;
begin
  Result := OutDegreeI(FVertexList.IndexOf(v));
end;

function TGSimpleDiGraph.OutDegreeI(aIndex: SizeInt): SizeInt;
begin
  FVertexList.CheckIndexRange(aIndex);
  Result := FVertexList.ItemRefs[aIndex]^.Count;
end;

function TGSimpleDiGraph.Degree(constref v: TVertex): SizeInt;
begin
  Result := DegreeI(FVertexList.IndexOf(v));
end;

function TGSimpleDiGraph.DegreeI(aIndex: SizeInt): SizeInt;
begin
  FVertexList.CheckIndexRange(aIndex);
  Result := FVertexList.ItemRefs[aIndex]^.Count + FVertexList.ItemRefs[aIndex]^.InDeg;
end;

function TGSimpleDiGraph.Isolated(constref v: TVertex): Boolean;
begin
  Result := Degree(v) = 0;
end;

function TGSimpleDiGraph.IsolatedI(aIndex: SizeInt): Boolean;
begin
  Result := DegreeI(aIndex) = 0;
end;

function TGSimpleDiGraph.EulerCycleExists: Boolean;
var
  I, d: SizeInt;
begin
  if VertexCount < 2 then
    exit(False);
  d := 0;
  for I := 0 to Pred(VertexCount) do
    begin
      if InDegreeI(I) <> OutDegreeI(I) then
        exit(False);
      d += DegreeI(I);
    end;
  Result := d > 0;
end;

function TGSimpleDiGraph.FindEulerCycle: TIntArray;
var
  g: TGSimpleDiGraph = nil;
  Stack: TIntStack;
  I, s, d, From: SizeInt;
begin
  if not EulerCycleExists then
    exit(nil);
  g := Clone;
  try
    I := 1;
    System.SetLength(Result, ARRAY_INITIAL_SIZE);
    s := 0;
    while g.DegreeI(s) = 0 do
      Inc(s);
    From := s;
    Result[0] := From;
    repeat
      repeat
        if not g.FVertexList.ItemRefs[s]^.FindFirst(d) then
          break;
        Stack.Push(s);
        g.RemoveEdgeI(s, d);
        s := d;
      until False;
      if not Stack.TryPop(s) then
        break;
      if System.Length(Result) = I then
        System.SetLength(Result, I shl 1);
      Result[I] := s;
      Inc(I);
    until False;
     System.SetLength(Result, I);
     TIntHelper.Reverse(Result);
  finally
    g.Free;
  end;
end;

function TGSimpleDiGraph.Clone: TGSimpleDiGraph;
begin
  Result := TGSimpleDiGraph.Create;
  Result.FVertexList := FVertexList;
  Result.FEdgeCount := EdgeCount;
  Result.FTitle := Title;
  //Result.FConnected := Connected;
  //Result.FConnectedValid := ConnectedValid;
end;

function TGSimpleDiGraph.Reverse: TGSimpleDiGraph;
var
  I,Dummy: SizeInt;
  e: TEdge;
begin
  Result := TGSimpleDiGraph.Create;
  for I := 0 to Pred(VertexCount) do
    Result.AddVertex(Items[I], Dummy);
  for e in Edges do
    Result.AddEdgeI(e.Destination, e.Source, e.Data);
end;

end.

