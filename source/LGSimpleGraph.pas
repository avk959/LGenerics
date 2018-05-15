{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic simple graph implementations.                                   *
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
unit LGSimpleGraph;

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
  LGHashTable,
  LGList,
  LGStack,
  LGQueue,
  LGVector,
  LGStrConst;

type

  ELGGraphError = class(Exception);
  TEmptyRec     = record end;


  { TGSimpleSparseGraphAL simple sparse graph prototype based on adjacency lists;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGSimpleSparseGraphAL<TVertex, TEdgeData, TVertexEqRel> = class
  public
  type
    PVertex        = ^TVertex;
    TStrToVertex   = function(constref aValue: string): TVertex of object;
    TVertexToStr   = function(constref aValue: TVertex): string of object;
    TSaveVertex    = procedure(constref aValue: TVertex; aStream: TStream) of object;
    TIntArray      = array of SizeInt;
    TVertexArray   = array of TVertex;
    TOnVertexVisit = function (aIndex: SizeInt): Boolean of object;

    TEdge = record
      Source,
      Destination: SizeInt;
      Data:  TEdgeData;
    end;

    TAdjItem = record
    public
      Destination: SizeInt;
      Data:  TEdgeData;
      property Key: SizeInt read Destination write Destination;
    private

    end;
    PAdjItem = ^TAdjItem;

  private
  type
    TAdjList      = specialize TGLiteIntHashTable<SizeInt, TAdjItem>;
    TAdjItemArray = array of TAdjItem;
    TIntHelper    = specialize TGNumArrayHelper<SizeInt>;
    TIntStack     = specialize TGLiteStack<SizeInt>;
    TIntQueue     = specialize TGLiteQueue<SizeInt>;
    PVertexItem   = ^TVertexItem;

    { TVertexItem }
    TVertexItem = record
      Vertex: TVertex;
      InDegree: SizeInt;
      AdjList: TAdjList;
      function  GetEnumerator: TAdjList.TEnumerator; inline;
      function  ToArray: TAdjItemArray;
      function  IsEmpty: Boolean; inline;
      procedure ClearItems; inline;
      procedure TrimToFit; inline;
      function  Contains(aKey: SizeInt): Boolean; inline;
      function  FindOrAdd(aKey: SizeInt; out e: PAdjItem): Boolean; inline;
      function  Find(aKey: SizeInt): PAdjItem; inline;
      function  FindFirst(out aKey: SizeInt): Boolean; inline;
      function  Add(constref aItem: TAdjItem): Boolean; inline;
      function  Remove(aKey: SizeInt): Boolean; inline;
      function  Count: SizeInt;
      property  Key: TVertex read Vertex;
    end;

    TVertexList = specialize TGLiteHashList2<TVertex, TVertexItem, TVertexEqRel>;

  public
  type
    TAdjEnumerator = record
    private
      FEnum: TAdjList.TEnumerator;
      function  GetCurrent: SizeInt; inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: SizeInt read GetCurrent;
    end;

    TAdjVertices = record
    private
      FGraph: TGSimpleSparseGraphAL;
      FSource: SizeInt;
      procedure Init(aGraph: TGSimpleSparseGraphAL; aIndex: SizeInt); inline;
    public
      function GetEnumerator: TAdjEnumerator; inline;
    end;

    TIncidentEnumerator = record
    private
      FEnum: TAdjList.TEnumerator;
      FSource: SizeInt;
      function  GetCurrent: TEdge;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: TEdge read GetCurrent;
    end;

    TIncidentEdges = record
    private
      FGraph: TGSimpleSparseGraphAL;
      FSource: SizeInt;
      procedure Init(aGraph: TGSimpleSparseGraphAL; aIndex: SizeInt); inline;
    public
      function GetEnumerator: TIncidentEnumerator; inline;
    end;

    TEdgeEnumerator = record
    private
      FGraph: TGSimpleSparseGraphAL;
      FEnum: TAdjList.TEnumerator;
      FCurrIndex,
      FLastIndex: SizeInt;
      FEnumDone: Boolean;
      function  GetCurrent: TEdge;
      procedure Init(aGraph: TGSimpleSparseGraphAL);
    public
      function  MoveNext: Boolean;
      procedure Reset; inline;
      property  Current: TEdge read GetCurrent;
    end;

    TEdges = record
    private
      FGraph: TGSimpleSparseGraphAL;
      procedure Init(aGraph: TGSimpleSparseGraphAL); inline;
    public
      function GetEnumerator: TEdgeEnumerator; inline;
    end;

  private
    FVertexList: TVertexList;
    FEdgeCount: SizeInt;
    FLabel: string;
    FDirected: Boolean;
    function  GetVertexCount: SizeInt; inline;
    function  GetVertex(aIndex: SizeInt): TVertex; inline;
    function  GetVertexRef(aIndex: SizeInt): PVertexItem; inline;
    function  DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
    procedure DoRemoveVertex(aIndex: SizeInt);
    function  GetAdjEnumerator(aIndex: SizeInt): TAdjEnumerator; inline;
    function  GetIncidentEnumerator(aIndex: SizeInt): TIncidentEnumerator; inline;
    function  GetEdgeEnumerator: TEdgeEnumerator; inline;
    property  VertexRefs[aIndex: SizeInt]: PVertexItem read GetVertexRef;
  public
    constructor Create(aDirected: Boolean = False);
    destructor Destroy; override;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear; inline;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    procedure TrimToFit;
    function  ContainsVertex(constref v: TVertex): Boolean; inline;
    function  ContainsEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  ContainsEdgeI(aSrc, aDst: SizeInt): Boolean;
    function  IndexOf(constref v: TVertex): SizeInt; inline;
    function  AddVertex(constref v: TVertex; out aIndex: SizeInt): Boolean; inline;
    function  AddEdge(constref aSrc, aDst: TVertex; aData: TEdgeData): Boolean;
    function  AddEdgeI(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  RemoveVertex(constref v: TVertex): Boolean;
    function  RemoveVertexI(aIndex: SizeInt): Boolean;
    function  RemoveEdge(constref aSrc, aDst: TVertex): Boolean;
    function  RemoveEdgeI(aSrc, aDst: SizeInt): Boolean;
    function  InDegree(constref v: TVertex): SizeInt; inline;
    function  InDegreeI(aIndex: SizeInt): SizeInt;
    function  OutDegree(constref v: TVertex): SizeInt; inline;
    function  OutDegreeI(aIndex: SizeInt): SizeInt; inline;
    function  Degree(constref v: TVertex): SizeInt; inline;
    function  DegreeI(aIndex: SizeInt): SizeInt;
    function  Isolated(constref v: TVertex): Boolean; inline;
    function  IsolatedI(aIndex: SizeInt): Boolean; inline;
    function  Adjacent(constref aSrc, aDst: TVertex): Boolean; inline;
    function  AdjacentI(aSrc, aDst: SizeInt): Boolean; inline;
  { enumerates indices of adjacent vertices }
    function  AdjVertices(constref v: TVertex): TAdjVertices; inline;
    function  AdjVerticesI(aIndex: SizeInt): TAdjVertices; inline;
  { enumerates incident edges }
    function  IncidentEdges(constref v: TVertex): TIncidentEdges; inline;
    function  IncidentEdgesI(aIndex: SizeInt): TIncidentEdges; inline;
  { enumerates all edges }
    function  Edges: TEdges; inline;
    function  SimplePathExists(constref aSrc, aDst: TVertex): Boolean; inline;
    function  SimplePathExistsI(aSrc, aDst: SizeInt): Boolean;
    function  SimplePathRec(constref aSrc, aDst: TVertex): TIntArray; inline;
    function  SimplePathRecI(aSrc, aDst: SizeInt): TIntArray;
    function  EulerPathExistsI(aSrc, aDst: SizeInt): Boolean;
    function  EulerPathI(aSrc, aDst: SizeInt): TIntArray;
  { returns count of visited vertices; if the aOnVisit call returns False, the traversal will be stopped }
    function  DSFTraversal(constref aFrom: TVertex; aOnVisit: TOnVertexVisit = nil): SizeInt; inline;
    function  DSFTraversalI(aFrom: SizeInt; aOnVisit: TOnVertexVisit = nil): SizeInt;
  { returns count of visited vertices; if the aOnVisit call returns False, the traversal will be stopped }
    function  BSFTraversal(constref aFrom: TVertex; aOnVisit: TOnVertexVisit = nil): SizeInt; inline;
    function  BSFTraversalI(aFrom: SizeInt; aOnVisit: TOnVertexVisit = nil): SizeInt;

    function  Clone: TGSimpleSparseGraphAL;
    property  GraphLabel: string read FLabel write FLabel;
    property  Directed: Boolean read FDirected;
    property  VertexCount: SizeInt read GetVertexCount;
    property  EdgeCount: SizeInt read FEdgeCount;
    property  Vertices[aIndex: SizeInt]: TVertex read GetVertex; default;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGSimpleSparseGraphAL.TVertexItem }

function TGSimpleSparseGraphAL.TVertexItem.GetEnumerator: TAdjList.TEnumerator;
begin
  Result := AdjList.GetEnumerator;
end;

function TGSimpleSparseGraphAL.TVertexItem.ToArray: TAdjItemArray;
var
  Item: PAdjItem;
  I: SizeInt = 0;
begin
  System.SetLength(Result, AdjList.Count);
  for Item in AdjList do
    begin
      Result[I] := Item^;
      Inc(I);
    end;
end;

function TGSimpleSparseGraphAL.TVertexItem.IsEmpty: Boolean;
begin
  Result := AdjList.Count = 0;
end;

procedure TGSimpleSparseGraphAL.TVertexItem.ClearItems;
begin
  AdjList.ClearItems;
end;

procedure TGSimpleSparseGraphAL.TVertexItem.TrimToFit;
begin
  AdjList.TrimToFit;
end;

function TGSimpleSparseGraphAL.TVertexItem.Contains(aKey: SizeInt): Boolean;
begin
  Result := AdjList.Contains(aKey);
end;

function TGSimpleSparseGraphAL.TVertexItem.FindOrAdd(aKey: SizeInt; out e: PAdjItem): Boolean;
var
  Dummy: SizeInt;
begin
  Result := AdjList.FindOrAdd(aKey, e, Dummy);
end;

function TGSimpleSparseGraphAL.TVertexItem.Find(aKey: SizeInt): PAdjItem;
var
  Dummy: SizeInt;
begin
  Result := AdjList.Find(aKey, Dummy);
end;

function TGSimpleSparseGraphAL.TVertexItem.FindFirst(out aKey: SizeInt): Boolean;
begin
  Result := AdjList.FindFirstKey(aKey);
end;

function TGSimpleSparseGraphAL.TVertexItem.Add(constref aItem: TAdjItem): Boolean;
var
  p: PAdjItem;
begin
  Result := not FindOrAdd(aItem.Key, p);
  if Result then
    p^ := aItem;
end;

function TGSimpleSparseGraphAL.TVertexItem.Remove(aKey: SizeInt): Boolean;
begin
  Result := AdjList.Remove(aKey);
end;

function TGSimpleSparseGraphAL.TVertexItem.Count: SizeInt;
begin
  Result := AdjList.Count;
end;

{ TGSimpleSparseGraphAL.TAdjEnumerator }

function TGSimpleSparseGraphAL.TAdjEnumerator.GetCurrent: SizeInt;
begin
  Result := FEnum.Current^.Destination;
end;

function TGSimpleSparseGraphAL.TAdjEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGSimpleSparseGraphAL.TAdjEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGSimpleSparseGraphAL.TAdjVertices }

procedure TGSimpleSparseGraphAL.TAdjVertices.Init(aGraph: TGSimpleSparseGraphAL; aIndex: SizeInt);
begin
  FGraph := aGraph;
  FSource := aIndex;
end;

function TGSimpleSparseGraphAL.TAdjVertices.GetEnumerator: TAdjEnumerator;
begin
  Result := FGraph.GetAdjEnumerator(FSource);
end;

{ TGSimpleSparseGraphAL.TIncidentEnumerator }

function TGSimpleSparseGraphAL.TIncidentEnumerator.GetCurrent: TEdge;
var
  p: PAdjItem;
begin
  p := FEnum.Current;
  Result.Source := FSource;
  Result.Destination := p^.Destination;
  Result.Data := p^.Data;
end;

function TGSimpleSparseGraphAL.TIncidentEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGSimpleSparseGraphAL.TIncidentEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGSimpleSparseGraphAL.TIncidentEdges }

procedure TGSimpleSparseGraphAL.TIncidentEdges.Init(aGraph: TGSimpleSparseGraphAL; aIndex: SizeInt);
begin
  FGraph := aGraph;
  FSource := aIndex;
end;

function TGSimpleSparseGraphAL.TIncidentEdges.GetEnumerator: TIncidentEnumerator;
begin
  Result := FGraph.GetIncidentEnumerator(FSource);
end;

{ TGSimpleSparseGraphAL.TEdgeEnumerator }

function TGSimpleSparseGraphAL.TEdgeEnumerator.GetCurrent: TEdge;
var
  p: PAdjItem;
begin
  p := FEnum.Current;
  Result.Source := FCurrIndex;
  Result.Destination := p^.Destination;
  Result.Data := p^.Data;
end;

procedure TGSimpleSparseGraphAL.TEdgeEnumerator.Init(aGraph: TGSimpleSparseGraphAL);
begin
  FGraph := aGraph;
  FLastIndex := Pred(aGraph.VertexCount);
  Reset;
end;

function TGSimpleSparseGraphAL.TEdgeEnumerator.MoveNext: Boolean;
begin
  repeat
    if FEnumDone then
      begin
        if FCurrIndex >= FLastIndex then
          exit(False);
        Inc(FCurrIndex);
        FEnum := FGraph.VertexRefs[FCurrIndex]^.GetEnumerator;
      end;
    Result := FEnum.MoveNext;
    FEnumDone := not Result;
  until Result;
end;

procedure TGSimpleSparseGraphAL.TEdgeEnumerator.Reset;
begin
  FCurrIndex := -1;
  FEnumDone := True;
end;

{ TGSimpleSparseGraphAL.TEdges }

procedure TGSimpleSparseGraphAL.TEdges.Init(aGraph: TGSimpleSparseGraphAL);
begin
  FGraph := aGraph;
end;

function TGSimpleSparseGraphAL.TEdges.GetEnumerator: TEdgeEnumerator;
begin
  Result := FGraph.GetEdgeEnumerator;
end;

{ TGSimpleSparseGraphAL }

function TGSimpleSparseGraphAL.GetVertexCount: SizeInt;
begin
  Result := FVertexList.Count;
end;

function TGSimpleSparseGraphAL.GetVertex(aIndex: SizeInt): TVertex;
begin
  Result := VertexRefs[aIndex]^.Vertex;
end;

function TGSimpleSparseGraphAL.GetVertexRef(aIndex: SizeInt): PVertexItem;
begin
  Result := FVertexList.ItemRefs[aIndex];
end;

function TGSimpleSparseGraphAL.DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
var
  p: PAdjItem;
begin
  if aSrc = aDst then
    exit(False);
  Result := not VertexRefs[aSrc]^.FindOrAdd(aDst, p);
  if Result then
    begin
      p^.Key := aDst;
      p^.Data := aData;
      if Directed then
        Inc(VertexRefs[aDst]^.InDegree)
      else
        begin
          VertexRefs[aDst]^.FindOrAdd(aSrc, p);
          p^.Key := aSrc;
          p^.Data := aData;
        end;
      Inc(FEdgeCount);
    end;
end;

function TGSimpleSparseGraphAL.DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
begin
  if aSrc = aDst then
    exit(False);
  Result := VertexRefs[aSrc]^.Remove(aDst);
  if Result then
    begin
      if Directed then
        Dec(VertexRefs[aDst]^.InDegree)
      else
        FVertexList.ItemRefs[aDst]^.Remove(aSrc);
      Dec(FEdgeCount);
    end;
end;

procedure TGSimpleSparseGraphAL.DoRemoveVertex(aIndex: SizeInt);
var
  I, J: SizeInt;
  pV: PVertexItem;
  p: PAdjItem;
  CurrEdges: TAdjItemArray;
begin
  FEdgeCount -= VertexRefs[aIndex]^.Count;
  if Directed then
    for p in VertexRefs[aIndex]^.AdjList do
      Dec(VertexRefs[p^.Destination]^.InDegree);
  FVertexList.Delete(aIndex);
  for I := 0 to Pred(FVertexList.Count) do
    begin
      pV := VertexRefs[I];
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

function TGSimpleSparseGraphAL.GetAdjEnumerator(aIndex: SizeInt): TAdjEnumerator;
begin
  Result.FEnum := VertexRefs[aIndex]^.GetEnumerator;
end;

function TGSimpleSparseGraphAL.GetIncidentEnumerator(aIndex: SizeInt): TIncidentEnumerator;
begin
  Result.FEnum := VertexRefs[aIndex]^.GetEnumerator;
  Result.FSource := aIndex;
end;

function TGSimpleSparseGraphAL.GetEdgeEnumerator: TEdgeEnumerator;
begin
  Result.Init(Self);
end;

constructor TGSimpleSparseGraphAL.Create(aDirected: Boolean);
begin
  FDirected := aDirected;
end;

destructor TGSimpleSparseGraphAL.Destroy;
begin
  Clear;
  inherited;
end;

function TGSimpleSparseGraphAL.IsEmpty: Boolean;
begin
  Result := FVertexList.IsEmpty;
end;

function TGSimpleSparseGraphAL.NonEmpty: Boolean;
begin
  Result := FVertexList.NonEmpty;
end;

procedure TGSimpleSparseGraphAL.Clear;
begin
  FVertexList.Clear;
end;

procedure TGSimpleSparseGraphAL.EnsureCapacity(aValue: SizeInt);
begin
  FVertexList.EnsureCapacity(aValue);
end;

procedure TGSimpleSparseGraphAL.TrimToFit;
var
  I: SizeInt;
begin
  for I := 0 to Pred(FVertexList.Count) do
    VertexRefs[I]^.TrimToFit;
  FVertexList.TrimToFit;
end;

function TGSimpleSparseGraphAL.ContainsVertex(constref v: TVertex): Boolean;
begin
  Result := FVertexList.Contains(v);
end;

function TGSimpleSparseGraphAL.ContainsEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := ContainsEdgeI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGSimpleSparseGraphAL.ContainsEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  if aSrc = aDst then
    exit(False);
  if (aSrc < 0) or (aSrc >= FVertexList.Count) then
    exit(False);
  if (aDst < 0) or (aDst >= FVertexList.Count) then
    exit(False);
  Result := VertexRefs[aSrc]^.Contains(aDst);
end;

function TGSimpleSparseGraphAL.IndexOf(constref v: TVertex): SizeInt;
begin
  Result := FVertexList.IndexOf(v);
end;

function TGSimpleSparseGraphAL.AddVertex(constref v: TVertex; out aIndex: SizeInt): Boolean;
var
  p: PVertexItem;
begin
  Result := not FVertexList.FindOrAdd(v, p, aIndex);
  if Result then
    begin
      p^.Vertex := v;
      p^.InDegree := 0;
    end;
end;

function TGSimpleSparseGraphAL.AddEdge(constref aSrc, aDst: TVertex; aData: TEdgeData): Boolean;
var
  SrcIdx, DstIdx: SizeInt;
begin
  AddVertex(aSrc, SrcIdx);
  AddVertex(aDst, DstIdx);
  Result := DoAddEdge(SrcIdx, DstIdx, aData);
end;

function TGSimpleSparseGraphAL.AddEdgeI(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
begin
  if (aSrc < 0) or (aSrc >= FVertexList.Count) then
    exit(False);
  if (aDst < 0) or (aDst >= FVertexList.Count) then
    exit(False);
  Result := DoAddEdge(aSrc, aDst, aData);
end;

function TGSimpleSparseGraphAL.RemoveVertex(constref v: TVertex): Boolean;
var
  ToRemove: SizeInt;
begin
  ToRemove := FVertexList.IndexOf(v);
  Result := ToRemove >= 0;
  if Result then
    DoRemoveVertex(ToRemove);
end;

function TGSimpleSparseGraphAL.RemoveVertexI(aIndex: SizeInt): Boolean;
begin
  Result := (aIndex >= 0) and (aIndex < FVertexList.Count);
  if Result then
    DoRemoveVertex(aIndex);
end;

function TGSimpleSparseGraphAL.RemoveEdge(constref aSrc, aDst: TVertex): Boolean;
var
  SrcIdx, DstIdx: SizeInt;
begin
  SrcIdx := FVertexList.IndexOf(aSrc);
  if SrcIdx < 0 then
    exit(False);
  DstIdx := FVertexList.IndexOf(aDst);
  if DstIdx < 0 then
    exit(False);
  Result := DoRemoveEdge(SrcIdx, DstIdx);
end;

function TGSimpleSparseGraphAL.RemoveEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  if (aSrc < 0) or (aSrc >= FVertexList.Count) then
    exit(False);
  if (aDst < 0) or (aDst >= FVertexList.Count) then
    exit(False);
 Result := DoRemoveEdge(aSrc, aDst);
end;

function TGSimpleSparseGraphAL.InDegree(constref v: TVertex): SizeInt;
begin
  Result := InDegreeI(FVertexList.IndexOf(v));
end;

function TGSimpleSparseGraphAL.InDegreeI(aIndex: SizeInt): SizeInt;
begin
  if Directed then
    Result := VertexRefs[aIndex]^.InDegree
  else
    Result := VertexRefs[aIndex]^.Count;
end;

function TGSimpleSparseGraphAL.OutDegree(constref v: TVertex): SizeInt;
begin
  Result := OutDegreeI(FVertexList.IndexOf(v));
end;

function TGSimpleSparseGraphAL.OutDegreeI(aIndex: SizeInt): SizeInt;
begin
  Result := VertexRefs[aIndex]^.Count;
end;

function TGSimpleSparseGraphAL.Degree(constref v: TVertex): SizeInt;
begin
  Result := DegreeI(FVertexList.IndexOf(v));
end;

function TGSimpleSparseGraphAL.DegreeI(aIndex: SizeInt): SizeInt;
var
  p: PVertexItem;
begin
  if Directed then
    begin
      p := VertexRefs[aIndex];
      Result := p^.InDegree + p^.Count;
    end
  else
    Result := OutDegreeI(aIndex);
end;

function TGSimpleSparseGraphAL.Isolated(constref v: TVertex): Boolean;
begin
  Result := Degree(v) = 0;
end;

function TGSimpleSparseGraphAL.IsolatedI(aIndex: SizeInt): Boolean;
begin
  Result := DegreeI(aIndex) = 0;
end;

function TGSimpleSparseGraphAL.Adjacent(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := AdjacentI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGSimpleSparseGraphAL.AdjacentI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := VertexRefs[aSrc]^.Contains(aDst) or VertexRefs[aDst]^.Contains(aSrc);
end;

function TGSimpleSparseGraphAL.AdjVertices(constref v: TVertex): TAdjVertices;
begin
  Result{%H-}.Init(Self, FVertexList.IndexOf(v));
end;

function TGSimpleSparseGraphAL.AdjVerticesI(aIndex: SizeInt): TAdjVertices;
begin
  Result{%H-}.Init(Self, aIndex);
end;

function TGSimpleSparseGraphAL.IncidentEdges(constref v: TVertex): TIncidentEdges;
begin
  Result{%H-}.Init(Self, FVertexList.IndexOf(v));
end;

function TGSimpleSparseGraphAL.IncidentEdgesI(aIndex: SizeInt): TIncidentEdges;
begin
  Result{%H-}.Init(Self, aIndex);
end;

function TGSimpleSparseGraphAL.Edges: TEdges;
begin
  Result{%H-}.Init(Self);
end;

function TGSimpleSparseGraphAL.SimplePathExists(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := SimplePathExistsI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGSimpleSparseGraphAL.SimplePathExistsI(aSrc, aDst: SizeInt): Boolean;
var
  Visited: TBitVector;
  Stack: TIntStack;
begin
  if aSrc = aDst then
    exit(False);
  Visited.Size := VertexCount;
  Stack.Push(aSrc);
  while Stack.TryPop(aSrc) do
    if not Visited[aSrc] then
      begin
        if AdjacentI(aSrc, aDst) then
           exit(True);
        Visited[aSrc] := True;
        for aSrc in AdjVerticesI(aSrc) do
          if not Visited[aSrc] then
            Stack.Push(aSrc);
      end;
  Result := False;
end;

function TGSimpleSparseGraphAL.SimplePathRec(constref aSrc, aDst: TVertex): TIntArray;
begin
  Result := SimplePathRecI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGSimpleSparseGraphAL.SimplePathRecI(aSrc, aDst: SizeInt): TIntArray;
var
  Visited: TBitVector;
  Path: TIntArray;
  I: SizeInt = 1;
  J: SizeInt;
  function Search: Boolean;
  begin
    if AdjacentI(J, aDst) then
       exit(True);
    Visited[J] := True;
    for J in AdjVerticesI(J) do
      if not Visited[J] then
        if Search then
          begin
            if System.Length(Path) = I then
              System.SetLength(Path, I shl 1);
            Path[I] := J;
            Inc(I);
            exit(True);
          end;
    Result := False;
  end;
begin
  if aSrc = aDst then
    exit(nil);
  Visited.Size := VertexCount;
  System.SetLength(Path, ARRAY_INITIAL_SIZE);
  J := aSrc;
  if Search then
    begin
      Path[0] := aDst;
      System.SetLength(Path, Succ(I));
      Path[I] := aSrc;
      TIntHelper.Reverse(Path);
      Result := Path;
    end
  else
    Result := nil;
end;

function TGSimpleSparseGraphAL.EulerPathExistsI(aSrc, aDst: SizeInt): Boolean;
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

function TGSimpleSparseGraphAL.EulerPathI(aSrc, aDst: SizeInt): TIntArray;
var
  I: SizeInt;
  g: TGSimpleSparseGraphAL = nil;
  Stack: TIntStack;
  function Tour(s: SizeInt): SizeInt;
  var
    d: SizeInt;
  begin
    repeat
      if not g.VertexRefs[s]^.FindFirst(d) then
        break;
      Stack.Push(s);
      g.RemoveEdgeI(s, d);
      s := d;
    until False;
    Result := s;
  end;
begin
  if not EulerPathExistsI(aSrc, aDst) then
    exit(nil);
  g := Clone;
  try
    I := 1;
    System.SetLength(Result, ARRAY_INITIAL_SIZE);
    Result[0] := aSrc;
    while (Tour(aSrc) <> aDst) and Stack.NonEmpty do
      begin
        aSrc := Stack.Pop;
        if System.Length(Result) = I then
          System.SetLength(Result, I shl 1);
        Result[I] := aSrc;
        Inc(I);
      end;
     System.SetLength(Result, I);
  finally
    g.Free;
  end;
end;

function TGSimpleSparseGraphAL.DSFTraversal(constref aFrom: TVertex; aOnVisit: TOnVertexVisit): SizeInt;
begin
  Result := DSFTraversalI(FVertexList.IndexOf(aFrom), aOnVisit);
end;

function TGSimpleSparseGraphAL.DSFTraversalI(aFrom: SizeInt; aOnVisit: TOnVertexVisit): SizeInt;
var
  Visited: TBitVector;
  Stack: TIntStack;
begin
  Result := 0;
  Visited.Size := VertexCount;
  Stack.Push(aFrom);
  while Stack.TryPop(aFrom) do
    if not Visited[aFrom] then
      begin
        Inc(Result);
        Visited[aFrom] := True;
        if (aOnVisit <> nil) and not aOnVisit(aFrom) then
          exit;
        for aFrom in AdjVerticesI(aFrom) do
          if not Visited[aFrom] then
            Stack.Push(aFrom);
      end;
end;

function TGSimpleSparseGraphAL.BSFTraversal(constref aFrom: TVertex; aOnVisit: TOnVertexVisit): SizeInt;
begin
  Result := BSFTraversalI(FVertexList.IndexOf(aFrom), aOnVisit);
end;

function TGSimpleSparseGraphAL.BSFTraversalI(aFrom: SizeInt; aOnVisit: TOnVertexVisit): SizeInt;
var
  Visited: TBitVector;
  Queue: TIntQueue;
begin
  Result := 0;
  Visited.Size := VertexCount;
  Queue.Enqueue(aFrom);
  while Queue.TryDequeue(aFrom) do
    begin
      Inc(Result);
      Visited[aFrom] := True;
      if (aOnVisit <> nil) and not aOnVisit(aFrom) then
        exit;
      for aFrom in AdjVerticesI(aFrom) do
        if not Visited[aFrom] then
          Queue.Enqueue(aFrom);
    end;
end;

function TGSimpleSparseGraphAL.Clone: TGSimpleSparseGraphAL;
begin
  Result := TGSimpleSparseGraphAL.Create(FDirected);
  Result.FVertexList := FVertexList;
  Result.FEdgeCount := EdgeCount;
  Result.FLabel := GraphLabel;
end;

end.

