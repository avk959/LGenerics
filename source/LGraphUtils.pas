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
  //LGHashTable,
  LGStrConst;

type

  ELGraphError     = class(Exception); //???

  TEmptyRec        = record end;
  THandle          = LGUtils.THandle;

  TIntArray        = array of SizeInt;
  TShortArray      = array of ShortInt;
  THandleArray     = array of THandle;
  TIntHelper       = specialize TGNumArrayHelper<SizeInt>;
  TIntVectorHelper = specialize TGComparableVectorHelper<SizeInt>;
  TIntStack        = specialize TGLiteStack<SizeInt>;
  TIntQueue        = specialize TGLiteQueue<SizeInt>;
  TIntVector       = specialize TGLiteVector<SizeInt>;
  PIntVector       = ^TIntVector;

  TOnIntVisit      = procedure (aValue: SizeInt) of object;
  TOnIntTest       = function (aValue: SizeInt): Boolean of object;

  generic TGOnAddEdge<T>       = procedure(constref aSrc, aDst: T; aData: Pointer) of object;
  generic TGOnStreamRead<T>    = function(aStream: TStream): T of object;
  generic TGOnStreamWrite<T>   = procedure(aStream: TStream; constref aValue: T) of object;
  generic TGStreamRead<T>      = function(aStream: TStream): T;
  generic TGStreamWrite<T>     = procedure(aStream: TStream; constref aValue: T);
  generic TGNestStreamRead<T>  = function(aStream: TStream): T is nested;
  generic TGNestStreamWrite<T> = procedure(aStream: TStream; constref aValue: T) is nested;

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

  generic TGAdjItem<TData> = record
    Destination: SizeInt;
    Data: TData;
    constructor Create(aDst: SizeInt; constref aData: TData);
    property Key: SizeInt read Destination;
  end;

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
    MAX_CAPACITY: SizeInt   = SizeInt(MAX_CONTAINER_SIZE div NODE_SIZE);
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
    class function NewList(aCapacity: SizeInt): TNodeList; static;
    property Shift: SizeInt read FShift;
    class constructor Init;
    class operator Initialize(var Item: TGVertexItem);
  public
    Vertex: TVertex;
    FInDegree,
    FCompIndex: SizeInt;
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

  generic TGVertexItem1<TVertex, TData> = record
  public
  type
    TAdjItem      = specialize TGAdjItem<TData>;
    TAdjItemArray = array of TAdjItem;
    PAdjItem      = ^TAdjItem;

  private
  type
    TItemList = array of TAdjItem;

  const
    EXPAND_SIZE = 8;

  public
  type
    TEnumerator = record
    private
      FList: PAdjItem;
      FCurrIndex,
      FLastIndex: SizeInt;
      function  GetCurrent: PAdjItem; inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: PAdjItem read GetCurrent;
    end;

  private
    FItems: TItemList;
    FCount: SizeInt;
    function  GetCapacity: SizeInt; inline;
    procedure Expand; inline;
    function  DoFind(aValue: SizeInt): SizeInt;
    procedure DoRemove(aIndex: SizeInt);
    class operator Initialize(var Item: TGVertexItem1);
  public
    Vertex: TVertex;
    FInDegree,
    FCompIndex: SizeInt;
    procedure Assign(constref aSrc: TGVertexItem1);
    function  GetEnumerator: TEnumerator; inline;
    function  ToArray: TAdjItemArray; inline;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear;
    procedure MakeEmpty;
    procedure TrimToFit; inline;
    function  Contains(aDst: SizeInt): Boolean; inline;
    function  FindOrAdd(aDst: SizeInt; out p: PAdjItem): Boolean; inline;
    function  Find(aDst: SizeInt): PAdjItem;
    function  FindFirst(out aDst: SizeInt): Boolean;
    function  Add(constref aItem: TAdjItem): Boolean;
    function  Remove(aDst: SizeInt): Boolean; inline;
    property  Key: TVertex read Vertex;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
  end;

  generic TGVertexHashList<TVertex, TEdgeData, TEqRel> = record
  public
  type
    TVertexItem = specialize TGVertexItem1<TVertex, TEdgeData>;
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
    procedure SetItem(aIndex: SizeInt; const aValue: TVertex);
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
    property  Items[aIndex: SizeInt]: TVertex read GetItem write SetItem; default;
    property  ItemRefs[aIndex: SizeInt]: PVertexItem read GetItemRef;
  end;

  { TGCustomGraph: simple sparse graph abstract ancestor class based on adjacency lists;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGCustomGraph<TVertex, TEdgeData, TVertexEqRel> = class abstract
  protected
  type
    TVertexList = specialize TGVertexHashList<TVertex, TEdgeData, TVertexEqRel>;
    PVertexList = ^TVertexList;
    TVertexItem = TVertexList.TVertexItem;
    PVertexItem = TVertexList.PVertexItem;
    PEdgeData   = ^TEdgeData;

    TAdjVerticesPtr = record
    private
      FGraph: TGCustomGraph;
      FSource: SizeInt;
    public
      function GetEnumerator: TVertexItem.TEnumerator; inline;
    end;

    TMagic = string[8];

    TStreamHeader = packed record
      Magic: TMagic;
      Version: Byte;
      TitleSize: Word;
      VertexCount,
      EdgeCount: LongInt;
      //title
      //vertices
      //edges: src index, dst index, data
    end;

  const
    LGRAPH_MAGIC: TMagic = 'LGrphTyp';
    CURRENT_VERSION = 1;

  class var
    CFData: TEdgeData;

  var
    FVertexList: TVertexList;
    FEdgeCount: SizeInt;
    FTitle: string;
    function  GetVertexCount: SizeInt; inline;
    function  GetVertex(aIndex: SizeInt): TVertex; inline;
    procedure SetVertex(aIndex: SizeInt; const aValue: TVertex); inline;
    function  GetEdgeDataPtr(aSrc, aDst: SizeInt): PEdgeData; inline;
    function  AdjVerticesPtr(aSrc: SizeInt): TAdjVerticesPtr;
  public
  type
    TAdjItem         = TVertexItem.TAdjItem;
    PAdjItem         = ^TAdjItem;

    TOnReadVertex    = specialize TGOnStreamRead<TVertex>;
    TOnWriteVertex   = specialize TGOnStreamWrite<TVertex>;
    TReadVertex      = specialize TGStreamRead<TVertex>;
    TWriteVertex     = specialize TGStreamWrite<TVertex>;
    TNestReadVertex  = specialize TGNestStreamRead<TVertex>;
    TNestWriteVertex = specialize TGNestStreamWrite<TVertex>;

    TOnAddEdge       = specialize TGOnAddEdge<TVertex>;

    TOnReadData      = specialize TGOnStreamRead<TEdgeData>;
    TOnWriteData     = specialize TGOnStreamWrite<TEdgeData>;
    TReadData        = specialize TGStreamRead<TEdgeData>;
    TWriteData       = specialize TGStreamWrite<TEdgeData>;
    TNestReadData    = specialize TGNestStreamRead<TEdgeData>;
    TNestWriteData   = specialize TGNestStreamWrite<TEdgeData>;

    TIntEdge = record
      Source,
      Destination: SizeInt;
      constructor Create(s, d: SizeInt);
    end;

    TIntEdgeVector = specialize TGLiteVector<TIntEdge>;
    PIntEdgeVector = ^TIntEdgeVector;
    TIntEdgeStack  = specialize TGLiteStack<TIntEdge>;

    TEdge = record
      Source,
      Destination: SizeInt;
      Data:  TEdgeData;
      constructor Create(aSrc: SizeInt; p: PAdjItem);
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
      FGraph: TGCustomGraph;
      FSource: SizeInt;
    public
      constructor Create(aGraph: TGCustomGraph; aSource: SizeInt);
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
      FGraph: TGCustomGraph;
      FSource: SizeInt;
    public
      constructor Create(aGraph: TGCustomGraph; aSource: SizeInt);
      function GetEnumerator: TIncidentEnumerator; inline;
    end;

    TVertexEnumerator = record
    private
      FList: PVertexList;
      FCurrIndex,
      FLastIndex: SizeInt;
      function  GetCurrent: TVertex;
    public
      constructor Create(aGraph: TGCustomGraph);
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: TVertex read GetCurrent;
    end;

    TVertices = record
    private
      FGraph: TGCustomGraph;
    public
      constructor Create(aGraph: TGCustomGraph);
      function GetEnumerator: TVertexEnumerator; inline;
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
      FGraph: TGCustomGraph;
    public
      function GetEnumerator: TEdgeEnumerator;
    end;

  public
    class function ChainFromTree(constref aTree: TIntArray; aIndex: SizeInt): TIntVector; static;
    class function CycleChainFromTree(constref aTree: TIntArray; aFirst, aLast: SizeInt): TIntVector; static;
    constructor Create;
    procedure CheckIndexRange(aIndex: SizeInt); inline;
    function  CreateIntArray(aValue: SizeInt = -1): TIntArray;
    function  CreateShortArray: TShortArray;
    function  CreateHandleArray: THandleArray;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear; virtual;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    procedure TrimToFit; inline;
    function  ContainsVertex(constref aVertex: TVertex): Boolean; inline;
    function  ContainsEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  ContainsEdgeI(aSrc, aDst: SizeInt): Boolean;
    function  ContainsEdgeI(aSrc, aDst: SizeInt; out aData: TEdgeData): Boolean;
    function  IndexOf(constref aVertex: TVertex): SizeInt; inline;
    function  Adjacent(constref aSrc, aDst: TVertex): Boolean; inline;
    function  AdjacentI(aSrc, aDst: SizeInt): Boolean;
  { enumerates indices of adjacent vertices }
    function  AdjVertices(constref aSrc: TVertex): TAdjVertices; inline;
    function  AdjVerticesI(aSrc: SizeInt): TAdjVertices;
  { enumerates incident edges }
    function  IncidentEdges(constref aSrc: TVertex): TIncidentEdges; inline;
    function  IncidentEdgesI(aSrc: SizeInt): TIncidentEdges;
  { enumerates all vertices }
    function  Vertices: TVertices; inline;
  { enumerates all edges }
    function  Edges: TEdges; inline;
    function  GetEdgeData(constref aSrc, aDst: TVertex): TEdgeData; inline;
    function  GetEdgeDataI(aSrc, aDst: SizeInt): TEdgeData; inline;
    procedure SetEdgeData(constref aSrc, aDst: TVertex; constref aValue: TEdgeData); inline;
    procedure SetEdgeDataI(aSrc, aDst: SizeInt; constref aValue: TEdgeData);
  { returns count of visited vertices; aOnGray calls after vertex visite, aOnWhite calls after vertex found;
    if aOnGray returns True then traversal stops }
    function  DfsTraversal(constref aRoot: TVertex; aOnGray: TOnIntTest = nil; aOnWhite: TOnIntVisit = nil): SizeInt; inline;
    function  DfsTraversalI(aRoot: SizeInt; aOnGray: TOnIntTest = nil; aOnWhite: TOnIntVisit = nil): SizeInt;
  { returns count of visited vertices; aOnGray calls after vertex visite, aOnWhite calls after vertex found;
    if aOnGray returns True then traversal stops}
    function  BfsTraversal(constref aRoot: TVertex; aOnGray: TOnIntTest = nil; aOnWhite: TOnIntVisit = nil): SizeInt; inline;
    function  BfsTraversalI(aRoot: SizeInt; aOnGray: TOnIntTest = nil; aOnWhite: TOnIntVisit = nil): SizeInt;
  { test whether the graph is bipartite;
    the graph can be disconnected (in this case it consists of a number of connected
    bipartite components and / or several isolated vertices)}
    function  IsBipartite: Boolean;
  { test whether the graph is bipartite; if returns True then information about the vertex
    belonging to the fractions is returned in v(0 or 1) }
    function  IsBipartite(out v: TShortArray): Boolean;

  { returns the length of the shortest path between the aSrc and aDst(in sense 'edges count'),
    -1 if the path does not exist }
    function  ShortestPathLen(constref aSrc, aDst: TVertex): SizeInt; inline;
    function  ShortestPathLenI(aSrc, aDst: SizeInt): SizeInt;
  { returns a vector containing in the corresponding components the length of shortest path from aRoot
    (in sense 'edges count')}
    function  ShortestPathsMap(constref aRoot: TVertex): TIntArray; inline;
    function  ShortestPathsMapI(aRoot: SizeInt = 0): TIntArray;
  { returns a vector containing chain of indices of found shortest path(in sense 'edges count'),
   (empty if path does not exists) }
    function  ShortestPath(constref aSrc, aDst: TVertex): TIntVector; inline;
    function  ShortestPathI(aSrc, aDst: SizeInt): TIntVector;

    property  Title: string read FTitle write FTitle;
    property  VertexCount: SizeInt read GetVertexCount;
    property  EdgeCount: SizeInt read FEdgeCount;
    property  Items[aIndex: SizeInt]: TVertex read GetVertex write SetVertex; default;
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

{ TGAdjItem }

constructor TGAdjItem.Create(aDst: SizeInt; constref aData: TData);
begin
  Destination := aDst;
  Data := aData;
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
  OldCapacity := Capacity;
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
  Vertex := aSrc.Vertex;
  FInDegree := aSrc.FInDegree;
  FCompIndex := aSrc.FCompIndex;
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
  Result := Pos >= 0;
  if not Result then
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

{ TGVertexItem1.TEnumerator }

function TGVertexItem1.TEnumerator.GetCurrent: PAdjItem;
begin
  Result := @FList[FCurrIndex];
end;

function TGVertexItem1.TEnumerator.MoveNext: Boolean;
begin
  Result := FCurrIndex < FLastIndex;
  FCurrIndex += Ord(Result);
end;

procedure TGVertexItem1.TEnumerator.Reset;
begin
  FCurrIndex := -1;
end;

{ TGVertexItem1 }

function TGVertexItem1.GetCapacity: SizeInt;
begin
  Result := System.Length(FItems);
end;

procedure TGVertexItem1.Expand;
begin
  System.SetLength(FItems, Capacity + EXPAND_SIZE);
end;

function TGVertexItem1.DoFind(aValue: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  for I := 0 to Pred(Count) do
    if FItems[I].Destination = aValue then
      exit(I);
  Result := -1;
end;

procedure TGVertexItem1.DoRemove(aIndex: SizeInt);
begin
  FItems[aIndex] := Default(TAdjItem);
  Dec(FCount);
  System.Move(FItems[Succ(aIndex)], FItems[aIndex], SizeOf(TAdjItem) * (Count - aIndex));
  System.FillChar(FItems[Count], SizeOf(TAdjItem), 0);
end;

class operator TGVertexItem1.Initialize(var Item: TGVertexItem1);
begin
  Item.Clear;
end;

procedure TGVertexItem1.Assign(constref aSrc: TGVertexItem1);
begin
  FItems := System.Copy(aSrc.FItems);
  FCount := aSrc.Count;
  Vertex := aSrc.Vertex;
  FInDegree := aSrc.FInDegree;
  FCompIndex := aSrc.FCompIndex;
end;

function TGVertexItem1.GetEnumerator: TEnumerator;
begin
  Result.FList := Pointer(FItems);
  Result.FLastIndex := Pred(Count);
  Result.FCurrIndex := -1;
end;

function TGVertexItem1.ToArray: TAdjItemArray;
begin
  Result := System.Copy(FItems, 0, Count);
end;

function TGVertexItem1.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TGVertexItem1.NonEmpty: Boolean;
begin
  Result := Count <> 0;
end;

procedure TGVertexItem1.Clear;
begin
  FItems := nil;
  FCount := 0;
end;

procedure TGVertexItem1.MakeEmpty;
var
  I: SizeInt;
begin
  for I := 0 to Pred(Count) do
    FItems[I] := Default(TAdjItem);
  FCount := 0;
end;

procedure TGVertexItem1.TrimToFit;
begin
  System.SetLength(FItems, Count);
end;

function TGVertexItem1.Contains(aDst: SizeInt): Boolean;
begin
  Result := DoFind(aDst) >= 0;
end;

function TGVertexItem1.FindOrAdd(aDst: SizeInt; out p: PAdjItem): Boolean;
var
  Pos: SizeInt;
begin
  Pos := DoFind(aDst);
  Result := Pos >= 0;
  if not Result then
    begin
      if Count = Capacity then
        Expand;
      Pos := Count;
      Inc(FCount);
    end;
  p := @FItems[Pos];
end;

function TGVertexItem1.Find(aDst: SizeInt): PAdjItem;
var
  Pos: SizeInt;
begin
  Pos := DoFind(aDst);
  if Pos >= 0 then
    Result := @FItems[Pos]
  else
    Result := nil;
end;

function TGVertexItem1.FindFirst(out aDst: SizeInt): Boolean;
begin
  Result := Count <> 0;
  if Result then
    aDst := FItems[0].Destination;
end;

function TGVertexItem1.Add(constref aItem: TAdjItem): Boolean;
begin
  Result := DoFind(aItem.Destination) < 0;
  if Result then
    begin
      if Count = Capacity then
        Expand;
      FItems[Count] := aItem;
      Inc(FCount);
    end;
end;

function TGVertexItem1.Remove(aDst: SizeInt): Boolean;
var
  Pos: SizeInt;
begin
  Pos := DoFind(aDst);
  Result := Pos >= 0;
  if Result then
    DoRemove(Pos);
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

procedure TGVertexHashList.SetItem(aIndex: SizeInt; const aValue: TVertex);
var
  I: SizeInt;
begin
  CheckIndexRange(aIndex);
  if TEqRel.Equal(aValue, FNodeList[aIndex].Item.Vertex) then
    exit;
  RemoveFromChain(aIndex);
  //add to new chain
  FNodeList[aIndex].Hash := TEqRel.HashCode(aValue);
  FNodeList[aIndex].Item.Vertex := aValue;
  I := FNodeList[aIndex].Hash and System.High(FNodeList);
  FNodeList[aIndex].Next := FChainList[I];
  FChainList[I] := aIndex;
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

{ TGCustomGraph.TAdjVerticesPtr }

function TGCustomGraph.TAdjVerticesPtr.GetEnumerator: TVertexItem.TEnumerator;
begin
  Result := FGraph.FVertexList.ItemRefs[FSource]^.GetEnumerator;
end;

{ TGCustomGraph.TIntEdge }

constructor TGCustomGraph.TIntEdge.Create(s, d: SizeInt);
begin
  Source := s;
  Destination := d;
end;

{ TGCustomGraph.TEdge }

constructor TGCustomGraph.TEdge.Create(aSrc: SizeInt; p: PAdjItem);
begin
  Source := aSrc;
  Destination := p^.Destination;
  Data := p^.Data;
end;

{ TGCustomGraph.TAdjEnumerator }

function TGCustomGraph.TAdjEnumerator.GetCurrent: SizeInt;
begin
  Result := FEnum.Current^.Destination;
end;

function TGCustomGraph.TAdjEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomGraph.TAdjEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomGraph.TAdjVertices }

constructor TGCustomGraph.TAdjVertices.Create(aGraph: TGCustomGraph; aSource: SizeInt);
begin
  FGraph := aGraph;
  FSource := aSource;
end;

function TGCustomGraph.TAdjVertices.GetEnumerator: TAdjEnumerator;
begin
  Result.FEnum := FGraph.FVertexList.ItemRefs[FSource]^.GetEnumerator;
end;

{ TGCustomGraph.TIncidentEnumerator }

function TGCustomGraph.TIncidentEnumerator.GetCurrent: TIncidentEdge;
var
  p: ^TAdjItem;
begin
  p := FEnum.Current;
  Result.Destination := p^.Destination;
  Result.Data := p^.Data;
end;

function TGCustomGraph.TIncidentEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomGraph.TIncidentEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomGraph.TIncidentEdges }

constructor TGCustomGraph.TIncidentEdges.Create(aGraph: TGCustomGraph; aSource: SizeInt);
begin
  FGraph := aGraph;
  FSource := aSource;
end;

function TGCustomGraph.TIncidentEdges.GetEnumerator: TIncidentEnumerator;
begin
  Result.FEnum := FGraph.FVertexList.ItemRefs[FSource]^.GetEnumerator;
end;

{ TGCustomGraph.TVertexEnumerator }

function TGCustomGraph.TVertexEnumerator.GetCurrent: TVertex;
begin
  Result := FList^.ItemRefs[FCurrIndex]^.Vertex;
end;

constructor TGCustomGraph.TVertexEnumerator.Create(aGraph: TGCustomGraph);
begin
  FList := @aGraph.FVertexList;
  FCurrIndex := -1;
  FLastIndex := Pred(FList^.Count);
end;

function TGCustomGraph.TVertexEnumerator.MoveNext: Boolean;
begin
  if FCurrIndex >= FLastIndex then
    exit(False);
  Inc(FCurrIndex);
  Result := True;
end;

procedure TGCustomGraph.TVertexEnumerator.Reset;
begin
  FCurrIndex := -1;
end;

{ TGCustomGraph.TVertices }

constructor TGCustomGraph.TVertices.Create(aGraph: TGCustomGraph);
begin
  FGraph := aGraph;
end;

function TGCustomGraph.TVertices.GetEnumerator: TVertexEnumerator;
begin
  Result := TVertexEnumerator.Create(FGraph);
end;

{ TGCustomGraph.TEdgeEnumerator }

function TGCustomGraph.TEdgeEnumerator.GetCurrent: TEdge;
begin
  Result := TEdge.Create(FCurrIndex, FEnum.Current);
end;

function TGCustomGraph.TEdgeEnumerator.MoveNext: Boolean;
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

procedure TGCustomGraph.TEdgeEnumerator.Reset;
begin
  FCurrIndex := -1;
  FEnumDone := True;
end;

{ TGCustomGraph.TEdges }

function TGCustomGraph.TEdges.GetEnumerator: TEdgeEnumerator;
begin
  Result.FList := FGraph.FVertexList.FNodeList;
  Result.FLastIndex := Pred(FGraph.FVertexList.Count);
  Result.FCurrIndex := -1;
  Result.FEnumDone := True;
end;

{ TGCustomGraph }

function TGCustomGraph.GetVertexCount: SizeInt;
begin
  Result := FVertexList.Count;
end;

function TGCustomGraph.GetVertex(aIndex: SizeInt): TVertex;
begin
  Result := FVertexList[aIndex];
end;

procedure TGCustomGraph.SetVertex(aIndex: SizeInt; const aValue: TVertex);
begin
  FVertexList[aIndex] := aValue;
end;

function TGCustomGraph.GetEdgeDataPtr(aSrc, aDst: SizeInt): PEdgeData;
begin
  Result := @FVertexList.ItemRefs[aSrc]^.Find(aDst)^.Data;
end;

function TGCustomGraph.AdjVerticesPtr(aSrc: SizeInt): TAdjVerticesPtr;
begin
  FVertexList.CheckIndexRange(aSrc);
  Result.FGraph := Self;
  Result.FSource := aSrc;
end;

class function TGCustomGraph.ChainFromTree(constref aTree: TIntArray; aIndex: SizeInt): TIntVector;
begin
  while aIndex >= 0 do
    begin
      if aIndex < System.Length(aTree) then
        Result.Add(aIndex)
      else
        raise ELGraphError.CreateFmt(SEIndexOutOfBoundsFmt,[aIndex]);
      aIndex := aTree[aIndex];
    end;
  TIntVectorHelper.Reverse(Result);
end;

class function TGCustomGraph.CycleChainFromTree(constref aTree: TIntArray; aFirst, aLast: SizeInt): TIntVector;
var
  I: SizeInt;
begin
  I := aLast;
  while I >= 0 do
    begin
      if I < System.Length(aTree) then
        Result.Add(I)
      else
        raise ELGraphError.CreateFmt(SEIndexOutOfBoundsFmt,[I]);
      if I = aFirst then
        break;
      I := aTree[I];
    end;
  Result.Add(aLast);
  TIntVectorHelper.Reverse(Result);
end;

constructor TGCustomGraph.Create;
begin
  Title := 'Untitled';
end;

procedure TGCustomGraph.CheckIndexRange(aIndex: SizeInt);
begin
  FVertexList.CheckIndexRange(aIndex);
end;

function TGCustomGraph.CreateIntArray(aValue: SizeInt): TIntArray;
var
  c: SizeInt;
begin
  c := VertexCount;
  System.SetLength(Result, c);
  if c > 0 then
{$IF DEFINED(CPU64)}
  System.FillQWord(Result[0], c, QWord(aValue));
{$ELSEIF DEFINED(CPU32)}
  System.FillDWord(Result[0], c, DWord(aValue));
{$ELSE}
  System.FillWord(Result[0], c, Word(aValue));
{$ENDIF}
end;

function TGCustomGraph.CreateShortArray: TShortArray;
var
  c: SizeInt;
begin
  c := VertexCount;
  System.SetLength(Result, c);
  if c > 0 then
    System.FillChar(Result[0], c, $ff);
end;

function TGCustomGraph.CreateHandleArray: THandleArray;
var
  c: SizeInt;
begin
  c := VertexCount;
  System.SetLength(Result, c);
  if c > 0 then
    System.FillChar(Result[0], c * SizeOf(THandle), $ff);
end;

function TGCustomGraph.IsEmpty: Boolean;
begin
  Result := FVertexList.Count = 0;
end;

function TGCustomGraph.NonEmpty: Boolean;
begin
  Result := FVertexList.Count <> 0;
end;

procedure TGCustomGraph.Clear;
begin
  FVertexList.Clear;
  FEdgeCount := 0;
  FTitle := '';
end;

procedure TGCustomGraph.EnsureCapacity(aValue: SizeInt);
begin
  FVertexList.EnsureCapacity(aValue);
end;

procedure TGCustomGraph.TrimToFit;
begin
  FVertexList.TrimToFit;
end;

function TGCustomGraph.ContainsVertex(constref aVertex: TVertex): Boolean;
begin
  Result := FVertexList.IndexOf(aVertex) >= 0;
end;

function TGCustomGraph.ContainsEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := ContainsEdgeI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGCustomGraph.ContainsEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  FVertexList.CheckIndexRange(aSrc);
  FVertexList.CheckIndexRange(aDst);
  Result := FVertexList.ItemRefs[aSrc]^.Contains(aDst);
end;

function TGCustomGraph.ContainsEdgeI(aSrc, aDst: SizeInt; out aData: TEdgeData): Boolean;
var
  p: PAdjItem;
begin
  FVertexList.CheckIndexRange(aSrc);
  FVertexList.CheckIndexRange(aDst);
  p := FVertexList.ItemRefs[aSrc]^.Find(aDst);
  Result := p <> nil;
  if Result then
    aData := p^.Data;
end;

function TGCustomGraph.IndexOf(constref aVertex: TVertex): SizeInt;
begin
  Result := FVertexList.IndexOf(aVertex);
end;

function TGCustomGraph.Adjacent(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := AdjacentI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGCustomGraph.AdjacentI(aSrc, aDst: SizeInt): Boolean;
begin
  FVertexList.CheckIndexRange(aSrc);
  FVertexList.CheckIndexRange(aDst);
  Result := FVertexList.ItemRefs[aSrc]^.Contains(aDst);
end;

function TGCustomGraph.AdjVertices(constref aSrc: TVertex): TAdjVertices;
begin
  Result := AdjVerticesI(FVertexList.IndexOf(aSrc));
end;

function TGCustomGraph.AdjVerticesI(aSrc: SizeInt): TAdjVertices;
begin
  FVertexList.CheckIndexRange(aSrc);
  Result := TAdjVertices.Create(Self, aSrc);
end;

function TGCustomGraph.IncidentEdges(constref aSrc: TVertex): TIncidentEdges;
begin
  Result := IncidentEdgesI(FVertexList.IndexOf(aSrc));
end;

function TGCustomGraph.IncidentEdgesI(aSrc: SizeInt): TIncidentEdges;
begin
  FVertexList.CheckIndexRange(aSrc);
  Result := TIncidentEdges.Create(Self, aSrc);
end;

function TGCustomGraph.Vertices: TVertices;
begin
  Result := TVertices.Create(Self);
end;

function TGCustomGraph.Edges: TEdges;
begin
  Result.FGraph := Self;
end;

function TGCustomGraph.GetEdgeData(constref aSrc, aDst: TVertex): TEdgeData;
begin
  Result := GetEdgeDataI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGCustomGraph.GetEdgeDataI(aSrc, aDst: SizeInt): TEdgeData;
begin
  if not ContainsEdgeI(aSrc, aDst, Result) then
    raise ELGraphError.CreateFmt(SEEdgeNotFoundFmt, [aSrc, aDst]);
end;

procedure TGCustomGraph.SetEdgeData(constref aSrc, aDst: TVertex; constref aValue: TEdgeData);
begin
  SetEdgeDataI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst), aValue);
end;

procedure TGCustomGraph.SetEdgeDataI(aSrc, aDst: SizeInt; constref aValue: TEdgeData);
var
  p: PAdjItem;
begin
  FVertexList.CheckIndexRange(aSrc);
  p := FVertexList.ItemRefs[aSrc]^.Find(aDst);
  if p <> nil then
    p^.Data := aValue
  else
    raise ELGraphError.CreateFmt(SEEdgeNotFoundFmt, [aSrc, aDst]);
end;

function TGCustomGraph.DfsTraversal(constref aRoot: TVertex; aOnGray: TOnIntTest;
  aOnWhite: TOnIntVisit): SizeInt;
begin
  Result := DfsTraversalI(FVertexList.IndexOf(aRoot), aOnGray, aOnWhite);
end;

function TGCustomGraph.DfsTraversalI(aRoot: SizeInt; aOnGray: TOnIntTest; aOnWhite: TOnIntVisit): SizeInt;
var
  Visited: TBitVector;
  Stack: TIntStack;
begin
  Result := 0;
  FVertexList.CheckIndexRange(aRoot);
  Visited.Size := VertexCount;
  if Assigned(aOnWhite) then
    aOnWhite(aRoot);
  repeat
    if not Visited[aRoot] then
      begin
        if Assigned(aOnGray) and aOnGray(aRoot) then
          exit;
        Inc(Result);
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

function TGCustomGraph.BfsTraversal(constref aRoot: TVertex; aOnGray: TOnIntTest;
  aOnWhite: TOnIntVisit): SizeInt;
begin
  Result := BfsTraversalI(FVertexList.IndexOf(aRoot), aOnGray, aOnWhite);
end;

function TGCustomGraph.BfsTraversalI(aRoot: SizeInt; aOnGray: TOnIntTest; aOnWhite: TOnIntVisit): SizeInt;
var
  Visited: TBitVector;
  Queue: TIntQueue;
begin
  Result := 0;
  FVertexList.CheckIndexRange(aRoot);
  Visited.Size := VertexCount;
  if aOnWhite <> nil then
    aOnWhite(aRoot);
  repeat
    if Assigned(aOnGray) and aOnGray(aRoot) then
      exit;
    for aRoot in AdjVerticesI(aRoot) do
      if not Visited[aRoot] then
        begin
          if aOnWhite <> nil then
            aOnWhite(aRoot);
          Inc(Result);
          Visited[aRoot] := True;
          Queue.Enqueue(aRoot);
        end;
  until not Queue.TryDequeue(aRoot);
end;

function TGCustomGraph.IsBipartite: Boolean;
var
  v: TShortArray;
begin
  Result := IsBipartite(v);
end;

function TGCustomGraph.IsBipartite(out v: TShortArray): Boolean;
var
  Visited: TBitVector;
  Stack: TIntStack;
  Curr, I: SizeInt;
  Color: Boolean;
begin
  v := CreateShortArray;
  if VertexCount < 2 then
    exit(False);
  Visited.Size := VertexCount;
  for I := 0 to Pred(System.Length(v)) do
    if not Visited[I] then
      begin
        Curr := I;
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

function TGCustomGraph.ShortestPathLen(constref aSrc, aDst: TVertex): SizeInt;
begin
  Result := ShortestPathLenI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGCustomGraph.ShortestPathLenI(aSrc, aDst: SizeInt): SizeInt;
var
  Queue: TIntQueue;
  Dist: TIntArray;
  Next: SizeInt;
begin
  FVertexList.CheckIndexRange(aSrc);
  FVertexList.CheckIndexRange(aDst);
  Dist := CreateIntArray;
  Dist[aSrc] := 0;
  repeat
    if aSrc = aDst then
      exit(Dist[aSrc]);
    for Next in AdjVerticesI(aSrc) do
      if Dist[Next] = -1 then
        begin
          Queue.Enqueue(Next);
          Dist[Next] := Succ(Dist[aSrc]);
        end;
  until not Queue.TryDequeue(aSrc);
  Result := -1;
end;

function TGCustomGraph.ShortestPathsMap(constref aRoot: TVertex): TIntArray;
begin
  Result := ShortestPathsMapI(FVertexList.IndexOf(aRoot));
end;

function TGCustomGraph.ShortestPathsMapI(aRoot: SizeInt): TIntArray;
var
  Queue: TIntQueue;
  Next: SizeInt;
begin
  FVertexList.CheckIndexRange(aRoot);
  Result := CreateIntArray;
  Result[aRoot] := 0;
  repeat
    for Next in AdjVerticesI(aRoot) do
      if Result[Next] = -1 then
        begin
          Queue.Enqueue(Next);
          Result[Next] := Succ(Result[aRoot]);
        end;
  until not Queue.TryDequeue(aRoot);
end;

function TGCustomGraph.ShortestPath(constref aSrc, aDst: TVertex): TIntVector;
begin
  Result := ShortestPathI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGCustomGraph.ShortestPathI(aSrc, aDst: SizeInt): TIntVector;
var
  Queue: TIntQueue;
  Visited: TBitVector;
  Parents: TIntArray;
  Next: SizeInt;
begin
  FVertexList.CheckIndexRange(aSrc);
  FVertexList.CheckIndexRange(aDst);
  Parents := CreateIntArray;
  Visited.Size := VertexCount;
  Visited[aSrc] := True;
  repeat
    if aSrc = aDst then
      exit(ChainFromTree(Parents, aDst));
    for Next in AdjVerticesI(aSrc) do
      if not Visited[Next] then
        begin
          Visited[Next] := True;
          Queue.Enqueue(Next);
          Parents[Next] := aSrc;
        end;
  until not Queue.TryDequeue(aSrc);
end;

end.

