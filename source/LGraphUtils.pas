{****************************************************************************
*                                                                           *
*   This file is part of the LGraphs package.                               *
*   Most common graph types and utils.                                      *
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
unit LGraphUtils;

{$mode objfpc}{$H+}
{$INLINE ON}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, math,
  LGUtils,
  {%H-}LGHelpers,
  LGArrayHelpers,
  LGStack,
  LGQueue,
  LGVector,
  LGHash,
  LGHashTable,
  LGStrConst;

type

  ELGGraphError = class(Exception); //???

  TEmptyRec     = record end;
  THandle       = LGUtils.THandle;

  TIntArray     = array of SizeInt;
  TShortArray   = array of ShortInt;
  THandleArray  = array of THandle;
  TIntHelper    = specialize TGNumArrayHelper<SizeInt>;
  TIntStack     = specialize TGLiteStack<SizeInt>;
  TIntQueue     = specialize TGLiteQueue<SizeInt>;

  TOnIntVisit   = procedure (aValue: SizeInt) of object;
  TOnIntTest    = function (aValue: SizeInt): Boolean of object;

  generic TGOnAddEdge<T> = procedure(aSrc, aDst: SizeInt; aData: Pointer) of object;

  { TDisjointSetUnion }

  TDisjointSetUnion = record
  private
    FList: array of SizeInt;
    function  GetSize: SizeInt; inline;
    procedure SetSize(aValue: SizeInt);
  public
    procedure Clear; inline;
    procedure Reset;
  { values related to the same set will have the same Marker }
    function  Marker(aValue: SizeInt): SizeInt;
    function  InSameSet(L, R: SizeInt): Boolean; inline;
    function  InDiffSets(L, R: SizeInt): Boolean; inline;
  { if L and R related to the different sets, these sets will be merged into one with a single Marker }
    procedure Union(L, R: SizeInt);
    property  Size: SizeInt read GetSize write SetSize;
  end;

  { TIntPair }

  TIntPair = record
    Source,
    Destination: SizeInt;
    function Key: TIntPair; inline;
    class function Equal(constref L, R: TIntPair): Boolean; static; inline;
    class function HashCode(constref aValue: TIntPair): SizeInt; static; inline;
    class function Construct(aSrc, aDst: SizeInt): TIntPair; static; inline;
  end;

  { TIntPairHashSet }

  TIntPairHashSet = record
  private
  type
    TTable = specialize TGLiteHashTableLP<TIntPair, TIntPair, TIntPair>;
  var
    FTable: TTable;
  public
    procedure Clear;
    function  Contains(constref aPair: TIntPair): Boolean; inline;
    function  Add(constref aPair: TIntPair): Boolean; inline;
  end;

  generic TGAdjItem<TData> = record
    Destination: SizeInt;
    Data: TData;
    property Key: SizeInt read Destination;
  end;

  { TGVertexItem }

  generic TGVertexItem<TVertex, TData> = record
  public
  type
    TAdjItem      = specialize TGAdjItem<TData>;
    TAdjItemArray = array of TAdjItem;
    PAdjItem      = ^TAdjItem;

  private
  type
    TNode = record
      Hash: SizeInt;
      Item: TAdjItem;
    end;

    PNode     = ^TNode;
    TNodeList = array of TNode;

  const
    NODE_SIZE               = SizeOf(TNode);
    SLOT_NOT_FOUND: SizeInt = Low(SizeInt);
    USED_FLAG: SizeInt      = SizeInt(1);
    MAX_CAPACITY: SizeInt   = MAX_CONTAINER_SIZE div NODE_SIZE;
    INITIAL_SIZE            = 8;

  public
  type
    TEnumerator = record
    private
      FList: PNode;
      FCurrIndex,
      FLastIndex: SizeInt;
      function  GetCurrent: PAdjItem; inline;
    public
      function  MoveNext: Boolean;
      procedure Reset; inline;
      property  Current: PAdjItem read GetCurrent;
    end;

  var
    Vertex: TVertex;
  private
    FNodeList: TNodeList;
    FShift,
    FCount: SizeInt;
    function  GetCapacity: SizeInt; inline;
    procedure Rehash(var aNewList: TNodeList);
    procedure Resize(aNewCapacity: SizeInt);
    procedure Expand;
    function  DoFind(aValue, aHash: SizeInt): SizeInt;
    procedure DoRemove(aIndex: SizeInt);
    class function HashCode(aValue: SizeInt): SizeInt; static; inline;
    class function NewList(aCapacity: SizeInt): TNodeList; static; //inline;
    property Shift: SizeInt read FShift;
    class constructor Init;
    class operator Initialize(var Item: TGVertexItem);
  public
    Tag: SizeInt;
    procedure Assign(constref aSrc: TGVertexItem);
    function  GetEnumerator: TEnumerator;
    function  ToArray: TAdjItemArray;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear;
    procedure MakeEmpty;
    procedure TrimToFit; inline;
    function  Contains(aDst: SizeInt): Boolean; inline;
    function  FindOrAdd(aDst: SizeInt; out e: PAdjItem): Boolean; inline;
    function  Find(aDst: SizeInt): PAdjItem;
    function  FindFirst(out aDst: SizeInt): Boolean;
    function  Add(constref aItem: TAdjItem): Boolean;
    function  Remove(aDst: SizeInt): Boolean; inline;
    property  Key: TVertex read Vertex;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
  end;

  { TGVertexHashList }

  generic TGVertexHashList<TVertex, TEdgeData, TEqRel> = record
  public
  type
    TVertexItem = specialize TGVertexItem<TVertex, TEdgeData>;
    PVertexItem = ^TVertexItem;

  private
  type
    TNode = record
      Hash,
      Next: SizeInt;
      Item: TVertexItem;
      procedure Assign(constref aSrc: TNode);
    end;

    TNodeList   = array of TNode;
    TChainList  = array of SizeInt;

  const
    NULL_INDEX  = SizeInt(-1);
    NODE_SIZE   = SizeOf(TNode);
    MAX_CAPACITY: SizeInt  = MAX_CONTAINER_SIZE div NODE_SIZE;

  var
    FNodeList: TNodeList;
    FChainList: TChainList;
    FCount: SizeInt;
    function  GetCapacity: SizeInt; inline;
    function  GetItem(aIndex: SizeInt): TVertex; inline;
    function  GetItemRef(aIndex: SizeInt): PVertexItem; inline;
    procedure InitialAlloc;
    procedure Rehash;
    procedure Resize(aNewCapacity: SizeInt);
    procedure Expand;
    function  Find(constref v: TVertex): SizeInt;
    function  Find(constref v: TVertex; aHash: SizeInt): SizeInt;
    function  DoAdd(constref v: TVertex; aHash: SizeInt): SizeInt;
    procedure RemoveFromChain(aIndex: SizeInt);
    procedure DoDelete(aIndex: SizeInt);
    function  DoRemove(constref v: TVertex): Boolean;
    procedure CheckIndexRange(aIndex: SizeInt); //inline;
    class constructor Init;
    class operator Initialize(var vl: TGVertexHashList);
    class operator Copy(constref aSrc: TGVertexHashList; var aDst: TGVertexHashList);
  public
    procedure Clear;
    procedure EnsureCapacity(aValue: SizeInt);
    procedure TrimToFit;
    function  IndexOf(constref v: TVertex): SizeInt; inline;
    function  FindOrAdd(constref v: TVertex; out aIndex: SizeInt): Boolean;
    function  Remove(constref v: TVertex): Boolean; inline;
    procedure Delete(aIndex: SizeInt); inline;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
    property  Items[aIndex: SizeInt]: TVertex read GetItem; default;
    property  ItemRefs[aIndex: SizeInt]: PVertexItem read GetItemRef;
  end;

  { TGCustomSimpleGraph: simple sparse graph abstract ancestor class based on adjacency lists;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGCustomSimpleGraph<TVertex, TEdgeData, TVertexEqRel> = class abstract
  protected
  type
    TVertexList = specialize TGVertexHashList<TVertex, TEdgeData, TVertexEqRel>;
    TVertexItem = TVertexList.TVertexItem;
    PEdgeData   = ^TEdgeData;

    TAdjVerticesPtr = record
    private
      FGraph: TGCustomSimpleGraph;
      FSource: SizeInt;
    public
      function GetEnumerator: TVertexItem.TEnumerator; inline;
    end;

  class var
    CFData: TEdgeData;

  var
    FVertexList: TVertexList;
    FEdgeCount: SizeInt;
    FTitle: string;
    function GetVertexCount: SizeInt; inline;
    function GetVertex(aIndex: SizeInt): TVertex; inline;
    function GetEdgeData(aSrc, aDst: SizeInt): PEdgeData; inline;
    function AdjVerticesPtr(aSrc: SizeInt): TAdjVerticesPtr;
  public
  type
    TOnAddEdge   = specialize TGOnAddEdge<TEdgeData>;
    TAdjItem     = TVertexItem.TAdjItem;
    PAdjItem     = ^TAdjItem;

    TEdge = record
      Source,
      Destination: SizeInt;
      Data:  TEdgeData;
      procedure Init(aSrc: SizeInt; p: PAdjItem); inline;
    end;

    TIncidentEdge = record
      Destination: SizeInt;
      Data:  TEdgeData;
    end;

    TAdjEnumerator = record
    private
      FEnum: TVertexItem.TEnumerator;
      function  GetCurrent: SizeInt; inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: SizeInt read GetCurrent;
    end;

    TAdjVertices = record
    private
      FGraph: TGCustomSimpleGraph;
      FSource: SizeInt;
    public
      function GetEnumerator: TAdjEnumerator; inline;
    end;

    TIncidentEnumerator = record
    private
      FEnum: TVertexItem.TEnumerator;
      function  GetCurrent: TIncidentEdge;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: TIncidentEdge read GetCurrent;
    end;

    TIncidentEdges = record
    private
      FGraph: TGCustomSimpleGraph;
      FSource: SizeInt;
    public
      function GetEnumerator: TIncidentEnumerator; inline;
    end;

    TEdgeEnumerator = record
    private
      FList: TVertexList.TNodeList;
      FEnum: TVertexItem.TEnumerator;
      FCurrIndex,
      FLastIndex: SizeInt;
      FEnumDone: Boolean;
      function  GetCurrent: TEdge;
    public
      function  MoveNext: Boolean;
      procedure Reset;
      property  Current: TEdge read GetCurrent;
    end;

    TEdges = record
    private
      FGraph: TGCustomSimpleGraph;
    public
      function GetEnumerator: TEdgeEnumerator;
    end;

    class function ChainFromTree(constref aTree: TIntArray; aIndex: SizeInt): TIntArray; static;
    procedure CheckIndexRange(aIndex: SizeInt); inline;
    function  CreateIntVector: TIntArray;
    function  CreateShortVector: TShortArray;
    function  CreateHandleVector: THandleArray;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear; inline;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    procedure TrimToFit; inline;
    function  ContainsVertex(constref v: TVertex): Boolean; inline;
    function  ContainsEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  ContainsEdgeI(aSrc, aDst: SizeInt): Boolean;
    function  ContainsEdgeI(aSrc, aDst: SizeInt; out aData: TEdgeData): Boolean;
    function  IndexOf(constref v: TVertex): SizeInt; inline;
    function  Adjacent(constref aSrc, aDst: TVertex): Boolean; inline;
    function  AdjacentI(aSrc, aDst: SizeInt): Boolean;
  { enumerates indices of adjacent vertices }
    function  AdjVertices(constref aSrc: TVertex): TAdjVertices; inline;
    function  AdjVerticesI(aSrc: SizeInt): TAdjVertices;
  { enumerates incident edges }
    function  IncidentEdges(constref aSrc: TVertex): TIncidentEdges; inline;
    function  IncidentEdgesI(aSrc: SizeInt): TIncidentEdges;
  { enumerates all edges }
    function  Edges: TEdges; inline;
  { returns count of visited vertices; aOnGray calls after vertex visite, aOnWhite calls after vertex found;
    if aOnGray returns True then traversal stops }
    function  DfsTraversal(constref aRoot: TVertex; aOnGray: TOnIntTest = nil; aOnWhite: TOnIntVisit = nil): SizeInt; inline;
    function  DfsTraversalI(aRoot: SizeInt; aOnGray: TOnIntTest = nil; aOnWhite: TOnIntVisit = nil): SizeInt;
  { returns count of visited vertices; aOnGray calls after vertex visite, aOnWhite calls after vertex found;
    if aOnGray returns True then traversal stops}
    function  BfsTraversal(constref aRoot: TVertex; aOnGray: TOnIntTest = nil; aOnWhite: TOnIntVisit = nil): SizeInt; inline;
    function  BfsTraversalI(aRoot: SizeInt; aOnGray: TOnIntTest = nil; aOnWhite: TOnIntVisit = nil): SizeInt;

    function  SimplePathExists(constref aSrc, aDst: TVertex): Boolean; inline;
    function  SimplePathExistsI(aSrc, aDst: SizeInt): Boolean;
  { test whether the graph is bipartite;
    the graph can be disconnected (in this case it consists of a number of connected
    bipartite components and / or several isolated vertices)}
    function  IsBipartite: Boolean;
  { test whether the graph is bipartite; if returns True then information about the vertex
    belonging to the fractions is returned in v(0 or 1) }
    function  IsBipartite(out v: TShortArray): Boolean;
  { returns the length of the shortest path between the aSrc and aDst,
    -1 if the path does not exist }
    function  FindMinPathLen(constref aSrc, aDst: TVertex): SizeInt; inline;
    function  FindMinPathLenI(aSrc, aDst: SizeInt): SizeInt;
  { returns a vector containing in the corresponding components the length of shortest path from aRoot }
    function  FindMinPathLenMap(constref aRoot: TVertex): TIntArray; inline;
    function  FindMinPathLenMapI(aRoot: SizeInt = 0): TIntArray;
  { returns a vector containing chain of indices of found shortest path(empty if path does not exists) }
    function  FindMinPath(constref aSrc, aDst: TVertex): TIntArray; inline;
    function  FindMinPathI(aSrc, aDst: SizeInt): TIntArray;

    property  Title: string read FTitle write FTitle;
    property  VertexCount: SizeInt read GetVertexCount;
    property  EdgeCount: SizeInt read FEdgeCount;
    property  Vertices[aIndex: SizeInt]: TVertex read GetVertex; default;
  end;


implementation
{$B-}{$COPERATORS ON}

{ TDisjointSetUnion }

function TDisjointSetUnion.GetSize: SizeInt;
begin
  Result := System.Length(FList);
end;

procedure TDisjointSetUnion.SetSize(aValue: SizeInt);
var
  OldSize, I: SizeInt;
begin
  OldSize := Size;
  if aValue > OldSize then
    begin
      System.SetLength(FList, aValue);
      for I := OldSize to Pred(aValue) do
        FList[I] := I;
    end;
end;

procedure TDisjointSetUnion.Clear;
begin
  FList := nil;
end;

procedure TDisjointSetUnion.Reset;
var
  I: SizeInt;
begin
  for I := 0 to System.High(FList) do
    FList[I] := I;
end;

function TDisjointSetUnion.Marker(aValue: SizeInt): SizeInt;
begin
  if FList[aValue] = aValue then
    exit(aValue);
  Result := Marker(FList[aValue]);
  FList[aValue] := Result;
end;

function TDisjointSetUnion.InSameSet(L, R: SizeInt): Boolean;
begin
  Result := Marker(L) = Marker(R);
end;

function TDisjointSetUnion.InDiffSets(L, R: SizeInt): Boolean;
begin
  Result := Marker(L) <> Marker(R);
end;

procedure TDisjointSetUnion.Union(L, R: SizeInt);
begin
  L := Marker(L);
  R := Marker(R);
  if Odd(Random(4)) then // random selection ???
    FList[L] := R
  else
    FList[R] := L;
end;

{ TIntPair }

function TIntPair.Key: TIntPair;
begin
  Result := Self;
end;

class function TIntPair.Equal(constref L, R: TIntPair): Boolean;
begin
  Result := (L.Source = R.Source) and (L.Destination = R.Destination);
end;

class function TIntPair.HashCode(constref aValue: TIntPair): SizeInt;
begin
{$IF DEFINED(CPU64)}
  Result := TxxHash32LE.HashQWord(QWord(aValue.Source), QWord(aValue.Destination));
{$ELSEIF DEFINED(CPU32)}
  Result := TxxHash32LE.HashDWord(DWord(aValue.Source), DWord(aValue.Destination));
{$ELSE}
  Result := TxxHash32LE.HasWord(Word(aValue.Source), Word(aValue.Destination));
{$ENDIF}
end;

class function TIntPair.Construct(aSrc, aDst: SizeInt): TIntPair;
begin
  Result.Source := aSrc;
  Result.Destination := aDst;
end;

{ TIntPairHashSet }

procedure TIntPairHashSet.Clear;
begin
  FTable.Clear;
end;

function TIntPairHashSet.Contains(constref aPair: TIntPair): Boolean;
var
  Dummy: SizeInt;
begin
  Result := FTable.Find(aPair, Dummy) <> nil;
end;

function TIntPairHashSet.Add(constref aPair: TIntPair): Boolean;
var
  Dummy: SizeInt;
  p: TTable.PEntry;
begin
  Result := not FTable.FindOrAdd(aPair, p, Dummy);
  if Result then
    p^ := aPair;
end;

{ TGVertexItem.TEnumerator }

function TGVertexItem.TEnumerator.GetCurrent: PAdjItem;
begin
  Result := @FList[FCurrIndex].Item;
end;

function TGVertexItem.TEnumerator.MoveNext: Boolean;
begin
  repeat
    if FCurrIndex >= FLastIndex then
      exit(False);
    Inc(FCurrIndex);
    Result := FList[FCurrIndex].Hash <> 0;
  until Result;
end;

procedure TGVertexItem.TEnumerator.Reset;
begin
  FCurrIndex := -1;
end;

{ TGVertexItem }

function TGVertexItem.GetCapacity: SizeInt;
begin
  Result := System.Length(FNodeList);
end;

procedure TGVertexItem.Rehash(var aNewList: TNodeList);
var
  h, I, J, Mask: SizeInt;
begin
  if Count > 0 then
    begin
      Mask := System.High(aNewList);
      for I := 0 to System.High(FNodeList) do
        begin
          if FNodeList[I].Hash <> 0 then
            begin
              h := FNodeList[I].Hash shr Shift;
              for J := 0 to Mask do
                begin
                  if aNewList[h].Hash = 0 then // -> target node is empty
                    begin
                      aNewList[h] := FNodeList[I];
                      break;
                    end;
                  h := Succ(h) and Mask;       // probe sequence
                end;
            end;
        end;
    end;
end;

procedure TGVertexItem.Resize(aNewCapacity: SizeInt);
var
  List: TNodeList;
begin
  List := NewList(aNewCapacity);
{$IF DEFINED(CPU64)}
  FShift := 64 - BsrQWord(QWord(aNewCapacity));
{$ELSEIF DEFINED(CPU32)}
  FShift := 32 - BsrDWord(DWord(aNewCapacity));
{$ELSE}
  FShift := 16 - BsrWord(Word(aNewCapacity));
{$ENDIF}
  Rehash(List);
  FNodeList := List;
end;

procedure TGVertexItem.Expand;
var
  NewCapacity, OldCapacity: SizeInt;
begin
  OldCapacity := GetCapacity;
  if OldCapacity > 0 then
    begin
      NewCapacity := Math.Min(MAX_CAPACITY, OldCapacity shl 1);
      if NewCapacity > OldCapacity then
        Resize(NewCapacity);
    end
  else
    Resize(INITIAL_SIZE);
end;

function TGVertexItem.DoFind(aValue, aHash: SizeInt): SizeInt;
var
  I, Pos, Mask: SizeInt;
begin
  Mask := System.High(FNodeList);
  Result := SLOT_NOT_FOUND;
  Pos := aHash shr Shift;
  for I := 0 to Mask do
    begin
      if FNodeList[Pos].Hash = 0 then // node empty => key not found
        exit(not Pos)
      else
        if FNodeList[Pos].Item.Key = aValue then
          exit(Pos);                // key found
      Pos := Succ(Pos) and Mask;    // probe sequence
    end;
end;

procedure TGVertexItem.DoRemove(aIndex: SizeInt);
var
  h, Gap, Mask: SizeInt;
begin
  Mask := System.High(FNodeList);
  FNodeList[aIndex] := Default(TNode);;
  Gap := aIndex;
  aIndex := Succ(aIndex) and Mask;
  Dec(FCount);
  repeat
    if FNodeList[aIndex].Hash = 0 then
      break;
    h := FNodeList[aIndex].Hash shr Shift;
    if (h <> aIndex) and (Succ(aIndex - h + Mask) and Mask >= Succ(aIndex - Gap + Mask) and Mask) then
      begin
        FNodeList[Gap] := FNodeList[aIndex];
        FNodeList[aIndex].Hash := 0;
        Gap := aIndex;
      end;
    aIndex := Succ(aIndex) and Mask;
  until False;
end;

class function TGVertexItem.HashCode(aValue: SizeInt): SizeInt;
begin
{$IF DEFINED(CPU64)}
  Result := aValue * SizeInt($9e3779b97f4a7c15);
{$ELSEIF DEFINED(CPU32)}
  Result := aValue * SizeInt($9e3779b9);
{$ELSE}
  Result := aValue * SizeInt($9e37);
{$ENDIF}
end;

class function TGVertexItem.NewList(aCapacity: SizeInt): TNodeList;
begin
  System.SetLength(Result, aCapacity);
  System.FillChar(Result[0], aCapacity * NODE_SIZE, 0);
end;

class constructor TGVertexItem.Init;
begin
{$PUSH}{$J+}
  MAX_CAPACITY := LGUtils.RoundUpTwoPower(MAX_CAPACITY);
{$POP}
end;

class operator TGVertexItem.Initialize(var Item: TGVertexItem);
begin
  Item.FCount := 0;
end;

procedure TGVertexItem.Assign(constref aSrc: TGVertexItem);
begin
  FNodeList := System.Copy(aSrc.FNodeList);
  FCount := aSrc.Count;
  FShift := aSrc.Shift;
end;

function TGVertexItem.GetEnumerator: TEnumerator;
begin
  Result.FLastIndex := System.High(FNodeList);
  if Result.FLastIndex >= 0 then
    Result.FList := @FNodeList[0]
  else
    Result.FList := nil;
  Result.FCurrIndex := -1;
end;

function TGVertexItem.ToArray: TAdjItemArray;
var
  I, J: SizeInt;
begin
  System.SetLength(Result, Count);
  if Count > 0 then
    begin
      I := 0;
      J := 0;
      repeat
        if FNodeList[J].Hash <> 0 then
          begin
            Result[I] := FNodeList[J].Item;
            Inc(I);
          end;
        Inc(J);
      until J >= Capacity;
    end;
end;

function TGVertexItem.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TGVertexItem.NonEmpty: Boolean;
begin
  Result := Count <> 0;
end;

procedure TGVertexItem.Clear;
begin
  FNodeList := nil;
  FCount := 0;
end;

procedure TGVertexItem.MakeEmpty;
var
  I: SizeInt;
begin
  for I := 0 to System.High(FNodeList) do
    FNodeList[I] := Default(TNode);
  FCount := 0;
end;

procedure TGVertexItem.TrimToFit;
var
  NewCapacity: SizeInt;
begin
  if Count = 0 then
    begin
      NewCapacity := LGUtils.RoundUpTwoPower(Count shl 1);
      if NewCapacity < GetCapacity then
        Resize(NewCapacity);
    end
  else
    Clear;
end;

function TGVertexItem.Contains(aDst: SizeInt): Boolean;
begin
  Result := Find(aDst) <> nil;
end;

function TGVertexItem.FindOrAdd(aDst: SizeInt; out e: PAdjItem): Boolean;
var
  Hash, Pos: SizeInt;
begin
  if FNodeList = nil then
    Expand;
  Hash := HashCode(aDst) or USED_FLAG;
  Pos := DoFind(aDst, Hash);
  Result := Pos >= 0; // key found?
  if not Result then   // key not found, will add new slot
    begin
      if Count >= Capacity shr 1 then
        begin
          Expand;
          Pos := DoFind(aDst, Hash);
        end;
      if Pos <> SLOT_NOT_FOUND then
        begin
          Pos := not Pos;
          FNodeList[Pos].Hash := Hash;
          Inc(FCount);
        end
      else
        raise ELGCapacityExceed.CreateFmt(SECapacityExceedFmt, [Succ(Count)]);
    end;
  e := @FNodeList[Pos].Item;
end;

function TGVertexItem.Find(aDst: SizeInt): PAdjItem;
var
  Pos: SizeInt;
begin
  Result := nil;
  if Count > 0 then
    begin
      Pos := DoFind(aDst, HashCode(aDst));
      if Pos >= 0 then
        Result := @FNodeList[Pos].Item;
    end;
end;

function TGVertexItem.FindFirst(out aDst: SizeInt): Boolean;
var
  I: SizeInt;
begin
  if Count > 0 then
    for I := 0 to System.High(FNodeList) do
      if FNodeList[I].Hash <> 0 then
        begin
          aDst := FNodeList[I].Item.Key;
          exit(True);
        end;
  Result := False;
end;

function TGVertexItem.Add(constref aItem: TAdjItem): Boolean;
var
  p: PAdjItem;
begin
  Result := not FindOrAdd(aItem.Key, p);
  if Result then
    p^ := aItem;
end;

function TGVertexItem.Remove(aDst: SizeInt): Boolean;
var
  Pos: SizeInt;
begin
  if Count > 0 then
    begin
      Pos := DoFind(aDst, HashCode(aDst));
      Result := Pos >= 0;
      if Result then
        DoRemove(Pos);
    end
  else
    Result := False;
end;

{ TGVertexHashList.TNode }

procedure TGVertexHashList.TNode.Assign(constref aSrc: TNode);
begin
  Hash := aSrc.Hash;
  Next := aSrc.Next;
  Item.Assign(aSrc.Item);
end;

{ TGVertexHashList }

function TGVertexHashList.GetCapacity: SizeInt;
begin
  Result := System.Length(FNodeList);
end;

function TGVertexHashList.GetItem(aIndex: SizeInt): TVertex;
begin
  CheckIndexRange(aIndex);
  Result := FNodeList[aIndex].Item.Vertex;
end;

function TGVertexHashList.GetItemRef(aIndex: SizeInt): PVertexItem;
begin
  Result := @FNodeList[aIndex].Item;
end;

procedure TGVertexHashList.InitialAlloc;
begin
  System.SetLength(FNodeList, DEFAULT_CONTAINER_CAPACITY);
  System.SetLength(FChainList, DEFAULT_CONTAINER_CAPACITY);
  System.FillChar(FChainList[0], DEFAULT_CONTAINER_CAPACITY * SizeOf(SizeInt), $ff);
end;

procedure TGVertexHashList.Rehash;
var
  I, J, Mask: SizeInt;
begin
  Mask := System.High(FChainList);
  System.FillChar(FChainList[0], Succ(Mask) * SizeOf(SizeInt), $ff);
  for I := 0 to Pred(Count) do
    begin
      J := FNodeList[I].Hash and Mask;
      FNodeList[I].Next := FChainList[J];
      FChainList[J] := I;
    end;
end;

procedure TGVertexHashList.Resize(aNewCapacity: SizeInt);
begin
  System.SetLength(FNodeList, aNewCapacity);
  System.SetLength(FChainList, aNewCapacity);
  Rehash;
end;

procedure TGVertexHashList.Expand;
var
  OldCapacity: SizeInt;
begin
  OldCapacity := Capacity;
  if OldCapacity > 0 then
    begin
      if OldCapacity < MAX_CAPACITY then
        Resize(OldCapacity shl 1)
      else
        raise ELGCapacityExceed.CreateFmt(SECapacityExceedFmt, [OldCapacity shl 1]);
    end
  else
    InitialAlloc;
end;

function TGVertexHashList.Find(constref v: TVertex): SizeInt;
var
  h: SizeInt;
begin
  h := TEqRel.HashCode(v);
  Result := FChainList[h and System.High(FChainList)];
  while Result <> NULL_INDEX do
    begin
      if (FNodeList[Result].Hash = h) and TEqRel.Equal(FNodeList[Result].Item.Vertex, v) then
        exit;
      Result := FNodeList[Result].Next;
    end;
end;

function TGVertexHashList.Find(constref v: TVertex; aHash: SizeInt): SizeInt;
begin
  Result := FChainList[aHash and System.High(FChainList)];
  while Result <> NULL_INDEX do
    begin
      if (FNodeList[Result].Hash = aHash) and TEqRel.Equal(FNodeList[Result].Item.Vertex, v) then
        exit;
      Result := FNodeList[Result].Next;
    end;
end;

function TGVertexHashList.DoAdd(constref v: TVertex; aHash: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  Result := Count;
  FNodeList[Result].Hash := aHash;
  I := aHash and System.High(FNodeList);
  FNodeList[Result].Next := FChainList[I];
  FNodeList[Result].Item.Vertex := v;
  FChainList[I] := Result;
  Inc(FCount);
end;

procedure TGVertexHashList.RemoveFromChain(aIndex: SizeInt);
var
  I, Curr, Prev: SizeInt;
begin
  I := FNodeList[aIndex].Hash and System.High(FNodeList);
  Curr := FChainList[I];
  Prev := NULL_INDEX;
  while Curr <> NULL_INDEX do
    begin
      if Curr = aIndex then
        begin
          if Prev <> NULL_INDEX then
            FNodeList[Prev].Next := FNodeList[Curr].Next
          else
            FChainList[I] := FNodeList[Curr].Next;
          exit;
        end;
      Prev := Curr;
      Curr := FNodeList[Curr].Next;
    end;
end;

procedure TGVertexHashList.DoDelete(aIndex: SizeInt);
begin
  Dec(FCount);
  if aIndex < Count then
    begin
      FNodeList[aIndex].Item := Default(TVertexItem);
      System.Move(FNodeList[Succ(aIndex)], FNodeList[aIndex], (Count - aIndex) * NODE_SIZE);
      System.FillChar(FNodeList[Count].Item, SizeOf(TVertexItem), 0);
      Rehash;
    end
  else   // last element
    begin
      RemoveFromChain(aIndex);
      System.FillChar(FNodeList[Count].Item, SizeOf(TVertexItem), 0);
    end;
end;

function TGVertexHashList.DoRemove(constref v: TVertex): Boolean;
var
  RemoveIdx: SizeInt;
begin
  RemoveIdx := Find(v);
  Result := RemoveIdx >= 0;
  if Result then
    DoDelete(RemoveIdx);
end;

procedure TGVertexHashList.CheckIndexRange(aIndex: SizeInt);
begin
  if (aIndex < 0) or (aIndex >= Count) then
    raise ELGListError.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
end;

class constructor TGVertexHashList.Init;
begin
{$PUSH}{$J+}
  MAX_CAPACITY := LGUtils.RoundUpTwoPower(MAX_CAPACITY);
{$POP}
end;

class operator TGVertexHashList.Initialize(var vl: TGVertexHashList);
begin
  vl.FCount := 0;
end;

class operator TGVertexHashList.Copy(constref aSrc: TGVertexHashList; var aDst: TGVertexHashList);
var
  I: SizeInt;
begin
  aDst.FCount := aSrc.Count;
  if aSrc.Count = 0 then
    exit;
  aDst.FChainList := System.Copy(aSrc.FChainList);
  System.SetLength(aDst.FNodeList, System.Length(aSrc.FNodeList));
  for I := 0 to Pred(aSrc.Count) do
    aDst.FNodeList[I].Assign(aSrc.FNodeList[I]);
  //for I := aSrc.Count do System.High(aDst.FNodeList) do
  //  aDst.FNodeList[I] := Default(TNode);
end;

procedure TGVertexHashList.Clear;
begin
  FNodeList := nil;
  FChainList := nil;
  FCount := 0;
end;

procedure TGVertexHashList.EnsureCapacity(aValue: SizeInt);
begin
  if aValue <= Capacity then
    exit;
  if aValue <= DEFAULT_CONTAINER_CAPACITY then
    aValue := DEFAULT_CONTAINER_CAPACITY
  else
    if aValue <= MAX_CAPACITY then
      aValue := LGUtils.RoundUpTwoPower(aValue)
    else
      raise ELGCapacityExceed.CreateFmt(SECapacityExceedFmt, [aValue]);
  Resize(aValue);
end;

procedure TGVertexHashList.TrimToFit;
var
  I, NewCapacity: SizeInt;
begin
  if Count > 0 then
    begin
      NewCapacity := LGUtils.RoundUpTwoPower(Count shl 1);
      if NewCapacity < Capacity then
        begin
          for I := 0 to Pred(Count) do
            FNodeList[I].Item.TrimToFit;
          Resize(NewCapacity);
        end;
    end
  else
    Clear;
end;

function TGVertexHashList.IndexOf(constref v: TVertex): SizeInt;
begin
  if Count > 0 then
    Result := Find(v)
  else
    Result := NULL_INDEX;
end;

function TGVertexHashList.FindOrAdd(constref v: TVertex; out aIndex: SizeInt): Boolean;
var
  h: SizeInt;
begin
  h := TEqRel.HashCode(v);
  if Count > 0 then
    aIndex := Find(v, h)
  else
    aIndex := NULL_INDEX;
  Result := aIndex >= 0;
  if not Result then
    begin
      if Count = Capacity then
        Expand;
      aIndex := DoAdd(v, h);
    end;
end;

function TGVertexHashList.Remove(constref v: TVertex): Boolean;
begin
  if Count > 0 then
    Result := DoRemove(v)
  else
    Result := False;
end;

procedure TGVertexHashList.Delete(aIndex: SizeInt);
begin
  CheckIndexRange(aIndex);
  DoDelete(aIndex);
end;

{ TGCustomSimpleGraph.TAdjVerticesPtr }

function TGCustomSimpleGraph.TAdjVerticesPtr.GetEnumerator: TVertexItem.TEnumerator;
begin
  Result := FGraph.FVertexList.ItemRefs[FSource]^.GetEnumerator;
end;

{ TGCustomSimpleGraph.TEdge }

procedure TGCustomSimpleGraph.TEdge.Init(aSrc: SizeInt; p: PAdjItem);
begin
  Source := aSrc;
  Destination := p^.Destination;
  Data := p^.Data;
end;

{ TGCustomSimpleGraph.TAdjEnumerator }

function TGCustomSimpleGraph.TAdjEnumerator.GetCurrent: SizeInt;
begin
  Result := FEnum.Current^.Destination;
end;

function TGCustomSimpleGraph.TAdjEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomSimpleGraph.TAdjEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomSimpleGraph.TAdjVertices }

function TGCustomSimpleGraph.TAdjVertices.GetEnumerator: TAdjEnumerator;
begin
  Result.FEnum := FGraph.FVertexList.ItemRefs[FSource]^.GetEnumerator;
end;

{ TGCustomSimpleGraph.TIncidentEnumerator }

function TGCustomSimpleGraph.TIncidentEnumerator.GetCurrent: TIncidentEdge;
var
  p: ^TAdjItem;
begin
  p := FEnum.Current;
  Result.Destination := p^.Destination;
  Result.Data := p^.Data;
end;

function TGCustomSimpleGraph.TIncidentEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomSimpleGraph.TIncidentEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomSimpleGraph.TIncidentEdges }

function TGCustomSimpleGraph.TIncidentEdges.GetEnumerator: TIncidentEnumerator;
begin
  Result.FEnum := FGraph.FVertexList.ItemRefs[FSource]^.GetEnumerator;
end;

{ TGCustomSimpleGraph.TEdgeEnumerator }

function TGCustomSimpleGraph.TEdgeEnumerator.GetCurrent: TEdge;
begin
  Result.Init(FCurrIndex, FEnum.Current);
end;

function TGCustomSimpleGraph.TEdgeEnumerator.MoveNext: Boolean;
begin
  repeat
    if FEnumDone then
      begin
        if FCurrIndex >= FLastIndex then
          exit(False);
        Inc(FCurrIndex);
        FEnum := FList[FCurrIndex].Item.GetEnumerator;
      end;
    Result := FEnum.MoveNext;
    FEnumDone := not Result;
  until Result;
end;

procedure TGCustomSimpleGraph.TEdgeEnumerator.Reset;
begin
  FCurrIndex := -1;
  FEnumDone := True;
end;

{ TGCustomSimpleGraph.TEdges }

function TGCustomSimpleGraph.TEdges.GetEnumerator: TEdgeEnumerator;
begin
  Result.FList := FGraph.FVertexList.FNodeList;
  Result.FLastIndex := Pred(FGraph.FVertexList.Count);
  Result.FCurrIndex := -1;
  Result.FEnumDone := True;
end;

{ TGCustomSimpleGraph }

function TGCustomSimpleGraph.GetVertexCount: SizeInt;
begin
  Result := FVertexList.Count;
end;

function TGCustomSimpleGraph.GetVertex(aIndex: SizeInt): TVertex;
begin
  Result := FVertexList[aIndex];
end;

function TGCustomSimpleGraph.GetEdgeData(aSrc, aDst: SizeInt): PEdgeData;
begin
  Result := @FVertexList.ItemRefs[aSrc]^.Find(aDst)^.Data;
end;

function TGCustomSimpleGraph.AdjVerticesPtr(aSrc: SizeInt): TAdjVerticesPtr;
begin
  FVertexList.CheckIndexRange(aSrc);
  Result.FGraph := Self;
  Result.FSource := aSrc;
end;

class function TGCustomSimpleGraph.ChainFromTree(constref aTree: TIntArray; aIndex: SizeInt): TIntArray;
var
  Stack: TIntStack;
begin
  while aIndex >= 0 do
    begin
      if aIndex < System.Length(aTree) then
        Stack.Push(aIndex)
      else
        raise ELGGraphError.CreateFmt(SEIndexOutOfBoundsFmt,[aIndex]);
      aIndex := aTree[aIndex];
    end;
  Result := Stack.ToArray;
  TIntHelper.Reverse(Result);
end;

procedure TGCustomSimpleGraph.CheckIndexRange(aIndex: SizeInt);
begin
  FVertexList.CheckIndexRange(aIndex);
end;

function TGCustomSimpleGraph.CreateIntVector: TIntArray;
var
  c: SizeInt;
begin
  c := VertexCount;
  System.SetLength(Result, c);
  if c > 0 then
    System.FillChar(Result[0], c * SizeOf(SizeInt), $ff);
end;

function TGCustomSimpleGraph.CreateShortVector: TShortArray;
var
  c: SizeInt;
begin
  c := VertexCount;
  System.SetLength(Result, c);
  if c > 0 then
    System.FillChar(Result[0], c, $ff);
end;

function TGCustomSimpleGraph.CreateHandleVector: THandleArray;
var
  c: SizeInt;
begin
  c := VertexCount;
  System.SetLength(Result, c);
  if c > 0 then
    System.FillChar(Result[0], c * SizeOf(THandle), $ff);
end;

function TGCustomSimpleGraph.IsEmpty: Boolean;
begin
  Result := FVertexList.Count = 0;
end;

function TGCustomSimpleGraph.NonEmpty: Boolean;
begin
  Result := FVertexList.Count <> 0;
end;

procedure TGCustomSimpleGraph.Clear;
begin
  FVertexList.Clear;
  FEdgeCount := 0;
  FTitle := '';
end;

procedure TGCustomSimpleGraph.EnsureCapacity(aValue: SizeInt);
begin
  FVertexList.EnsureCapacity(aValue);
end;

procedure TGCustomSimpleGraph.TrimToFit;
begin
  FVertexList.TrimToFit;
end;

function TGCustomSimpleGraph.ContainsVertex(constref v: TVertex): Boolean;
begin
  Result := FVertexList.IndexOf(v) >= 0;
end;

function TGCustomSimpleGraph.ContainsEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := ContainsEdgeI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGCustomSimpleGraph.ContainsEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  if (aSrc < 0) or (aSrc >= FVertexList.Count) then
    exit(False);
  if (aDst < 0) or (aDst >= FVertexList.Count) then
    exit(False);
  Result := FVertexList.FNodeList[aSrc].Item.Contains(aDst);
end;

function TGCustomSimpleGraph.ContainsEdgeI(aSrc, aDst: SizeInt; out aData: TEdgeData): Boolean;
var
  p: ^TAdjItem;
begin
  if (aSrc < 0) or (aSrc >= FVertexList.Count) then
    exit(False);
  if (aDst < 0) or (aDst >= FVertexList.Count) then
    exit(False);
  p := FVertexList.FNodeList[aSrc].Item.Find(aDst);
  Result := p <> nil;
  if Result then
    aData := p^.Data;
end;

function TGCustomSimpleGraph.IndexOf(constref v: TVertex): SizeInt;
begin
  Result := FVertexList.IndexOf(v);
end;

function TGCustomSimpleGraph.Adjacent(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := AdjacentI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGCustomSimpleGraph.AdjacentI(aSrc, aDst: SizeInt): Boolean;
begin
  FVertexList.CheckIndexRange(aSrc);
  FVertexList.CheckIndexRange(aDst);
  Result := FVertexList.FNodeList[aSrc].Item.Contains(aDst) or
            FVertexList.FNodeList[aDst].Item.Contains(aSrc);
end;

function TGCustomSimpleGraph.AdjVertices(constref aSrc: TVertex): TAdjVertices;
begin
  Result := AdjVerticesI(FVertexList.IndexOf(aSrc));
end;

function TGCustomSimpleGraph.AdjVerticesI(aSrc: SizeInt): TAdjVertices;
begin
  FVertexList.CheckIndexRange(aSrc);
  Result.FGraph := Self;
  Result.FSource := aSrc;
end;

function TGCustomSimpleGraph.IncidentEdges(constref aSrc: TVertex): TIncidentEdges;
begin
  Result := IncidentEdgesI(FVertexList.IndexOf(aSrc));
end;

function TGCustomSimpleGraph.IncidentEdgesI(aSrc: SizeInt): TIncidentEdges;
begin
  FVertexList.CheckIndexRange(aSrc);
  Result.FGraph := Self;
  Result.FSource := aSrc;
end;

function TGCustomSimpleGraph.Edges: TEdges;
begin
  Result.FGraph := Self;
end;

function TGCustomSimpleGraph.DfsTraversal(constref aRoot: TVertex; aOnGray: TOnIntTest;
  aOnWhite: TOnIntVisit): SizeInt;
begin
  Result := DfsTraversalI(FVertexList.IndexOf(aRoot), aOnGray, aOnWhite);
end;

function TGCustomSimpleGraph.DfsTraversalI(aRoot: SizeInt; aOnGray: TOnIntTest;
  aOnWhite: TOnIntVisit): SizeInt;
var
  Visited: TBitVector;
  Stack: TIntStack;
begin
  Result := 0;
  FVertexList.CheckIndexRange(aRoot);
  Visited.Size := VertexCount;
  {%H-}Stack.Push(aRoot);
  repeat
    if not Visited[aRoot] then
      begin
        Inc(Result);
        if Assigned(aOnGray) and aOnGray(aRoot) then
          exit;
        Visited[aRoot] := True;
        for aRoot in AdjVerticesI(aRoot) do
          if not Visited[aRoot] then
            begin
              if Assigned(aOnWhite) then
                aOnWhite(aRoot);
              Stack.Push(aRoot);
            end;
      end;
  until not Stack.TryPop(aRoot);
end;

function TGCustomSimpleGraph.BfsTraversal(constref aRoot: TVertex; aOnGray: TOnIntTest;
  aOnWhite: TOnIntVisit): SizeInt;
begin
  Result := BfsTraversalI(FVertexList.IndexOf(aRoot), aOnGray, aOnWhite);
end;

function TGCustomSimpleGraph.BfsTraversalI(aRoot: SizeInt; aOnGray: TOnIntTest;
  aOnWhite: TOnIntVisit): SizeInt;
var
  Visited: TBitVector;
  Queue: TIntQueue;
begin
  Result := 0;
  FVertexList.CheckIndexRange(aRoot);
  Visited.Size := VertexCount;
  Queue.Enqueue(aRoot);
  while Queue{%H-}.TryDequeue(aRoot) do
    if not Visited[aRoot] then
      begin
        Inc(Result);
        if Assigned(aOnGray) and aOnGray(aRoot) then
          exit;
        Visited[aRoot] := True;
        for aRoot in AdjVerticesI(aRoot) do
          if not Visited[aRoot] then
            begin
              if aOnWhite <> nil then
                aOnWhite(aRoot);
              Queue.Enqueue(aRoot);
            end;
      end;
end;

function TGCustomSimpleGraph.SimplePathExists(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := SimplePathExistsI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGCustomSimpleGraph.SimplePathExistsI(aSrc, aDst: SizeInt): Boolean;
var
  Visited: TBitVector;
  Stack: TIntStack;
begin
  FVertexList.CheckIndexRange(aSrc);
  FVertexList.CheckIndexRange(aDst);
  if aSrc = aDst then
    exit(False);
  Visited.Size := VertexCount;
  {%H-}Stack.Push(aSrc);
  repeat
    if not Visited[aSrc] then
      begin
        if AdjacentI(aSrc, aDst) then
           exit(True);
        Visited[aSrc] := True;
        for aSrc in AdjVerticesI(aSrc) do
          if not Visited[aSrc] then
            Stack.Push(aSrc);
      end;
  until not Stack.TryPop(aSrc);
  Result := False;
end;

function TGCustomSimpleGraph.IsBipartite: Boolean;
var
  v: TShortArray;
begin
  Result := IsBipartite(v);
end;

function TGCustomSimpleGraph.IsBipartite(out v: TShortArray): Boolean;
var
  Visited: TBitVector;
  Stack: TIntStack;
  Curr, I: SizeInt;
  Color: Boolean;
begin
  v := CreateShortVector;
  if VertexCount < 2 then
    exit(False);
  Visited.Size := VertexCount;
  for I := 0 to Pred(System.Length(v)) do
    if not Visited[I] then
      begin
        Curr := I;
        {%H-}Stack.Push(Curr);
        repeat
          if not Visited[Curr] then
            begin
              Visited[Curr] := True;
              if v[Curr] = -1 then
                begin
                  v[Curr] := 0;
                  Color := False;
                end
              else
                Color := Boolean(v[Curr]);
              for Curr in AdjVerticesI(Curr) do
                if not Visited[Curr] then
                  begin
                    Stack.Push(Curr);
                    v[Curr] := Ord(not Color);
                  end
                else
                  if v[Curr] = Ord(Color) then
                    exit(False);
            end;
        until not Stack.TryPop(Curr);
      end;
  Result := True;
end;

function TGCustomSimpleGraph.FindMinPathLen(constref aSrc, aDst: TVertex): SizeInt;
begin
  Result := FindMinPathLenI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aSrc));
end;

function TGCustomSimpleGraph.FindMinPathLenI(aSrc, aDst: SizeInt): SizeInt;
var
  Queue: TIntQueue;
  v: TIntArray;
  d: SizeInt;
begin
  FVertexList.CheckIndexRange(aSrc);
  FVertexList.CheckIndexRange(aDst);
  v := CreateIntVector;
  v[aSrc] := 0;
  Queue.Enqueue(aSrc);
  while Queue{%H-}.TryDequeue(aSrc) do
    for d in AdjVerticesI(aSrc) do
      if v[d] = -1 then
        begin
          Queue.Enqueue(d);
          v[d] := Succ(v[aSrc]);
        end;
  Result := v[aDst];
end;

function TGCustomSimpleGraph.FindMinPathLenMap(constref aRoot: TVertex): TIntArray;
begin
  Result := FindMinPathLenMapI(FVertexList.IndexOf(aRoot));
end;

function TGCustomSimpleGraph.FindMinPathLenMapI(aRoot: SizeInt): TIntArray;
var
  Queue: TIntQueue;
  d: SizeInt;
begin
  FVertexList.CheckIndexRange(aRoot);
  Result := CreateIntVector;
  Result[aRoot] := 0;
  Queue.Enqueue(aRoot);
  while Queue{%H-}.TryDequeue(aRoot) do
    for d in AdjVerticesI(aRoot) do
      if Result[d] = -1 then
        begin
          Queue.Enqueue(d);
          Result[d] := Succ(Result[aRoot]);
        end;
end;

function TGCustomSimpleGraph.FindMinPath(constref aSrc, aDst: TVertex): TIntArray;
begin
  Result := FindMinPathI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGCustomSimpleGraph.FindMinPathI(aSrc, aDst: SizeInt): TIntArray;
var
  v: TIntArray;
  Queue: TIntQueue;
  d: SizeInt;
  Found: Boolean = False;
begin
  FVertexList.CheckIndexRange(aSrc);
  FVertexList.CheckIndexRange(aDst);
  if aSrc = aDst then
    exit(nil);
  v := CreateIntVector;
  {%H-}Queue.Enqueue(aSrc);
  while Queue{%H-}.TryDequeue(aSrc) do
    begin
      if {%H-}aSrc = aDst then
        begin
          Found := True;
          break;
        end;
      for d in AdjVerticesI(aSrc) do
        if v[d] = -1 then
          begin
            Queue.Enqueue(d);
            v[d] := aSrc;
          end;
    end;
  if not Found then
    exit(nil);
  Result := ChainFromTree(v, aDst);
end;

end.

