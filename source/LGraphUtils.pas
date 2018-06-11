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
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}
{$INLINE ON}{$WARN 6058 off : }
interface

uses
  Classes, SysUtils, math,
  LGUtils,
  {%H-}LGHelpers,
  LGArrayHelpers,
  LGStack,
  LGQueue,
  LGDeque,
  LGVector,
  LGHash,
  LGStrConst;

type

  ELGraphError     = class(Exception); //???

  TEmptyRec        = record end;
  THandle          = LGUtils.THandle;

  TIntArray        = array of SizeInt;
  TShortArray      = array of ShortInt;
  THandleArray     = array of THandle;
  TIntHelper       = specialize TGNumArrayHelper<SizeInt>;
  TIntVector       = specialize TGLiteVector<SizeInt>;
  TIntVectorHelper = specialize TGComparableVectorHelper<SizeInt>;
  TIntArrayVector  = specialize TGLiteVector<TIntArray>;
  TIntStack        = specialize TGLiteStack<SizeInt>;
  TIntQueue        = specialize TGLiteQueue<SizeInt>;
  TIntDeque        = specialize TGLiteDeque<SizeInt>;
  PIntVector       = ^TIntVector;

  TOnVisit         = procedure (aValue: SizeInt) of object;
  TOnAccept        = function (aValue: SizeInt): Boolean of object;

  generic TGOnAddEdge<T>       = procedure(constref aSrc, aDst: T; aData: Pointer) of object;
  generic TGOnStreamRead<T>    = procedure(aStream: TStream; out aValue: T) of object;
  generic TGOnStreamWrite<T>   = procedure(aStream: TStream; constref aValue: T) of object;

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

  TVertexColor = 0..3;

const
  vclNone:  TVertexColor = 0;
  vclWhite: TVertexColor = 1;
  vclGray:  TVertexColor = 2;
  vclBlack: TVertexColor = 3;

type
  TColorArray = array of TVertexColor;

  TColorVector = record
  private
  type
    TColorList = array of SizeUInt;
  const
{$IF DEFINED(CPU64)}
    SHIFT = 5;
    MASK  = 31;
{$ELSEIF DEFINED(CPU32)}
    SHIFT = 4;
    MASK  = 15;
{$ELSE}
    SHIFT = 3;
    MASK  = 7;
{$ENDIF}
  var
    FList: TColorList;
    function  GetItem(aIndex: SizeInt): TVertexColor; inline;
    function  GetSize: SizeInt; inline;
    procedure SetItem(aIndex: SizeInt; aValue: TVertexColor); inline;
    procedure SetSize(aValue: SizeInt);
    class operator Copy(constref aSrc: TColorVector; var aDst: TColorVector); inline;
  public
    procedure ClearItems; inline;
    property  Size: SizeInt read GetSize write SetSize;
  { read/write item with (index < 0) or (index >= Size) will raise exception }
    property  Items[aIndex: SizeInt]: TVertexColor read GetItem write SetItem; default;
  end;

  TIntEdge = record
    Source,
    Destination: SizeInt;
    constructor Create(s, d: SizeInt);
  end;

  TIntArrayVectorHelper = specialize TGDelegatedVectorHelper<TIntArray>;
  TIntEdgeVector        = specialize TGLiteVector<TIntEdge>;
  PIntEdgeVector        = ^TIntEdgeVector;
  TIntEdgeArray         = array of TIntEdge;
  TEdgeArrayVector      = specialize TGLiteVector<TIntEdgeArray>;

  generic TGAdjItem<TData> = record
    Destination: SizeInt;
    Data: TData;
    constructor Create(aDst: SizeInt; constref aData: TData);
    property Key: SizeInt read Destination;
  end;

  generic TGHashAdjList<TVertex, TData> = record
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
    class operator Initialize(var aList: TGHashAdjList);
  public
    Vertex: TVertex;
    FInDegree,
    FCompIndex: SizeInt;
    procedure Assign(constref aSrc: TGHashAdjList);
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

  generic TGAdjList<TVertex, TData> = record
  public
  type
    TAdjItem      = specialize TGAdjItem<TData>;
    TAdjItemArray = array of TAdjItem;
    PAdjItem      = ^TAdjItem;

  private
  type
    TAdjItemList = array of TAdjItem;

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
    FList: TAdjItemList;
    FCount: SizeInt;
    function  GetCapacity: SizeInt; inline;
    procedure Expand; inline;
    function  DoFind(aValue: SizeInt): SizeInt;
    procedure DoRemove(aIndex: SizeInt);
    class operator Initialize(var aList: TGAdjList);
  public
    Vertex: TVertex;
    FInDegree,
    FCompIndex: SizeInt;
    procedure Assign(constref aSrc: TGAdjList);
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

  { TGCustomGraph: simple sparse graph abstract ancestor class based on adjacency lists;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGCustomGraph<TVertex, TEdgeData, TVertexEqRel> = class abstract
  public
  type
    TEdgeDataType = TEdgeData;
    PEdgeData     = ^TEdgeData;

  protected
  type
    TAdjList  = specialize TGAdjList<TVertex, TEdgeData>;
    PAdjList  = ^TAdjList;

    TNode = record
      Hash,
      Next: SizeInt;
      Item: TAdjList;
      procedure Assign(constref aSrc: TNode);
    end;
    PNode = ^TNode;

    TNodeList   = array of TNode;
    TChainList  = array of SizeInt;

  const
    NULL_INDEX  = SizeInt(-1);
    NODE_SIZE   = SizeOf(TNode);
    MAX_CAPACITY: SizeInt  = MAX_CONTAINER_SIZE div NODE_SIZE;

  type
    TAdjVerticesPtr = record
    private
      FGraph: TGCustomGraph;
      FSource: SizeInt;
    public
      function GetEnumerator: TAdjList.TEnumerator; inline;
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

    TIntList = record
    const
      EXPAND_SIZE = 8;
    var
      Items: TIntArray;
      Count: SizeInt;
      procedure Expand; inline;
      function  Contains(aValue: SizeInt): Boolean; inline;
      function  Find(aValue: SizeInt): SizeInt;
      function  Add(constref aValue: SizeInt): Boolean;
      class operator Initialize(var aList: TIntList);
    end;

    TSkeleton = record
    private
      function  GetSize: SizeInt; inline;
      procedure SetSize(aValue: SizeInt); inline;
    public
      AdjLists: array of TIntList;
      EdgeCount: SizeInt;
      function ContainsEdge(constref aSrc, aDst: SizeInt): Boolean; inline;
      function AddEdge(constref aSrc, aDst: SizeInt): Boolean;
      property Size: SizeInt read GetSize write SetSize;
      class operator Initialize(var s: TSkeleton);
    end;

  const
    GRAPH_MAGIC: TMagic = 'LGrphTyp';
    CURRENT_VERSION     = 1;

  class var
    CFData: TEdgeData;

  private
    FNodeList: TNodeList;
    FChainList: TChainList;
    FCount,
    FEdgeCount: SizeInt;
    FTitle: string;
    function  GetCapacity: SizeInt; inline;
    function  GetItem(aIndex: SizeInt): TVertex; inline;
    procedure SetItem(aIndex: SizeInt; const aValue: TVertex);
    function  GetAdjList(aIndex: SizeInt): PAdjList; inline;
    procedure InitialAlloc;
    procedure Rehash;
    procedure Resize(aNewCapacity: SizeInt);
    procedure Expand;
    function  DoAdd(constref v: TVertex; aHash: SizeInt): SizeInt;
    procedure RemoveFromChain(aIndex: SizeInt);
    procedure DoDelete(aIndex: SizeInt);
    function  DoRemove(constref v: TVertex): Boolean;
    function  Find(constref v: TVertex): SizeInt;
    function  Find(constref v: TVertex; aHash: SizeInt): SizeInt;
    function  FindOrAdd(constref v: TVertex; out aIndex: SizeInt): Boolean;
    function  Remove(constref v: TVertex): Boolean; inline;
    procedure Delete(aIndex: SizeInt); inline;
    class constructor Init;
  protected
    function  GetEdgeDataPtr(aSrc, aDst: SizeInt): PEdgeData; inline;
    function  AdjVerticesPtr(aSrc: SizeInt): TAdjVerticesPtr;
    function  NonRecDfs(aRoot: SizeInt): SizeInt;
    procedure CheckIndexRange(aIndex: SizeInt);
    property  AdjList[aIndex: SizeInt]: PAdjList read GetAdjList;
  public
  type
    TAdjItem         = TAdjList.TAdjItem;
    PAdjItem         = ^TAdjItem;

    TOnAddEdge       = specialize TGOnAddEdge<TVertex>;

    TOnReadVertex    = specialize TGOnStreamRead<TVertex>;
    TOnWriteVertex   = specialize TGOnStreamWrite<TVertex>;

    TOnReadData      = specialize TGOnStreamRead<TEdgeData>;
    TOnWriteData     = specialize TGOnStreamWrite<TEdgeData>;

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
      FEnum: TAdjList.TEnumerator;
      function  GetCurrent: SizeInt; inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: SizeInt read GetCurrent;
    end;

    TAdjEnumArray = array of TAdjEnumerator;

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
      FEnum: TAdjList.TEnumerator;
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
      FNodeList: PNode;
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
      FList: TNodeList;
      FEnum: TAdjList.TEnumerator;
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
    class function  TreeToChain(constref aTree: TIntArray; aIndex: SizeInt): TIntArray; static;
    class procedure Tree2Chain(constref aTree: TIntArray; aIndex: SizeInt; var v: TIntVector); static;
    class function  TreeToCycle(constref aTree: TIntArray; aFirst, aLast: SizeInt): TIntArray; static;
    class procedure Tree2Cycle(constref aTree: TIntArray;  aFirst, aLast: SizeInt; var v: TIntVector); static;
    class function  TreeToCycleLen(constref aTree: TIntArray; aFirst, aLast: SizeInt): SizeInt; static;
    class property  DefaultEdgeData: TEdgeData read CFData;
    constructor Create;
    constructor Create(aCapacity: SizeInt);
    function  CreateIntArray(aValue: SizeInt = -1): TIntArray;
    function  CreateColorArray: TColorArray;
    function  CreateHandleArray: THandleArray;
    function  CreateAdjEnumArray: TAdjEnumArray;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear; virtual;
    procedure EnsureCapacity(aValue: SizeInt);
    procedure TrimToFit; inline;
    function  ContainsVertex(constref aVertex: TVertex): Boolean; inline;
    function  ContainsEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  ContainsEdgeI(aSrc, aDst: SizeInt): Boolean;
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
    function  GetEdgeData(constref aSrc, aDst: TVertex; out aData: TEdgeData): Boolean; inline;
    function  GetEdgeDataI(aSrc, aDst: SizeInt; out aData: TEdgeData): Boolean; inline;
    function  SetEdgeData(constref aSrc, aDst: TVertex; constref aValue: TEdgeData): Boolean; inline;
    function  SetEdgeDataI(aSrc, aDst: SizeInt; constref aValue: TEdgeData): Boolean;
  { returns count of visited vertices; OnAccept calls after vertex visite, OnNext calls after next vertex found;
    if TOnAccept returns False then traversal stops }
    function  DfsTraversal(constref aRoot: TVertex; OnAccept: TOnAccept = nil; OnFound: TOnVisit = nil): SizeInt; inline;
    function  DfsTraversalI(aRoot: SizeInt; OnAccept: TOnAccept = nil; OnFound: TOnVisit = nil): SizeInt;
  { returns count of visited vertices; OnAccept calls after vertex visite, OnFound calls after vertex found;
    if TOnAccept returns False then traversal stops}
    function  BfsTraversal(constref aRoot: TVertex; OnAccept: TOnAccept = nil; OnFound: TOnVisit = nil): SizeInt; inline;
    function  BfsTraversalI(aRoot: SizeInt; OnAccept: TOnAccept = nil; OnFound: TOnVisit = nil): SizeInt;
  { test whether the graph is bipartite;
    the graph can be disconnected (in this case it consists of a number of connected
    bipartite components and / or several isolated vertices)}
    function  IsBipartite: Boolean;
  { test whether the graph is bipartite; if returns True then information about the vertex
    belonging to the fractions is returned in aColors(vclWhite or vclGray) }
    function  IsBipartite(out aColors: TColorArray): Boolean;
  { returns the length of the shortest path between the aSrc and aDst(in sense 'edges count'),
    -1 if the path does not exist }
    function  ShortestPathLen(constref aSrc, aDst: TVertex): SizeInt; inline;
    function  ShortestPathLenI(aSrc, aDst: SizeInt): SizeInt;
  { returns an array containing in the corresponding components the length of shortest path from aRoot
    (in sense 'edges count'), or -1 if it unreachable }
    function  ShortestPathsMap(constref aRoot: TVertex): TIntArray; inline;
    function  ShortestPathsMapI(aRoot: SizeInt = 0): TIntArray;
  { returns an array containing chain of indices of found shortest path(in sense 'edges count'),
    empty if path does not exists }
    function  ShortestPath(constref aSrc, aDst: TVertex): TIntArray; inline;
    function  ShortestPathI(aSrc, aDst: SizeInt): TIntArray;

    property  Title: string read FTitle write FTitle;
    property  VertexCount: SizeInt read FCount;
    property  EdgeCount: SizeInt read FEdgeCount;
    property  Capacity: SizeInt read GetCapacity;
    property  Items[aIndex: SizeInt]: TVertex read GetItem write SetItem; default;
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

{ TColorVector }

function TColorVector.GetItem(aIndex: SizeInt): TVertexColor;
begin
  if (aIndex >= 0) and (aIndex < (System.Length(FList) shl SHIFT)) then
    Result := FList[aIndex shr SHIFT] shr ((aIndex and MASK) shl 1) and SizeUInt(3)
  else
    raise ELGListError.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
end;

function TColorVector.GetSize: SizeInt;
begin
  Result := System.Length(FList) shl SHIFT;
end;

procedure TColorVector.SetItem(aIndex: SizeInt; aValue: TVertexColor);
begin
  if (aIndex >= 0) and (aIndex < (System.Length(FList) shl SHIFT)) then
    FList[aIndex shr SHIFT] := (FList[aIndex shr SHIFT] and not (SizeUInt(3) shl ((aIndex and MASK) shl 1))) or
                               (SizeUInt(aValue) shl ((aIndex and MASK) shl 1))
  else
    raise ELGListError.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
end;

procedure TColorVector.SetSize(aValue: SizeInt);
var
  OldLen: SizeInt;
begin
  OldLen := Size;
  if aValue > OldLen then
    begin
      aValue := aValue shr SHIFT + Ord(aValue and MASK <> 0);
      System.SetLength(FList, aValue);
      System.FillChar(FList[OldLen], (aValue - OldLen) * SizeOf(SizeUInt), 0);
    end;
end;

class operator TColorVector.Copy(constref aSrc: TColorVector; var aDst: TColorVector);
begin
  aDst.FList := System.Copy(aSrc.FList);
end;

procedure TColorVector.ClearItems;
begin
  if FList <> nil then
    System.FillChar(FList[0], System.Length(FList) * SizeOf(SizeUInt), 0);
end;

{ TIntEdge }

constructor TIntEdge.Create(s, d: SizeInt);
begin
  Source := s;
  Destination := d;
end;

{ TGAdjItem }

constructor TGAdjItem.Create(aDst: SizeInt; constref aData: TData);
begin
  Destination := aDst;
  Data := aData;
end;

{ TGHashAdjList.TEnumerator }

function TGHashAdjList.TEnumerator.GetCurrent: PAdjItem;
begin
  Result := @FList[FCurrIndex].Item;
end;

function TGHashAdjList.TEnumerator.MoveNext: Boolean;
begin
  repeat
    if FCurrIndex >= FLastIndex then
      exit(False);
    Inc(FCurrIndex);
    Result := FList[FCurrIndex].Hash <> 0;
  until Result;
end;

procedure TGHashAdjList.TEnumerator.Reset;
begin
  FCurrIndex := -1;
end;

{ TGHashAdjList }

function TGHashAdjList.GetCapacity: SizeInt;
begin
  Result := System.Length(FNodeList);
end;

procedure TGHashAdjList.Rehash(var aNewList: TNodeList);
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

procedure TGHashAdjList.Resize(aNewCapacity: SizeInt);
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

procedure TGHashAdjList.Expand;
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

function TGHashAdjList.DoFind(aValue, aHash: SizeInt): SizeInt;
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

procedure TGHashAdjList.DoRemove(aIndex: SizeInt);
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

class function TGHashAdjList.HashCode(aValue: SizeInt): SizeInt;
begin
{$IF DEFINED(CPU64)}
  Result := aValue * SizeInt($9e3779b97f4a7c15);
{$ELSEIF DEFINED(CPU32)}
  Result := aValue * SizeInt($9e3779b9);
{$ELSE}
  Result := aValue * SizeInt($9e37);
{$ENDIF}
end;

class function TGHashAdjList.NewList(aCapacity: SizeInt): TNodeList;
begin
  System.SetLength(Result, aCapacity);
  System.FillChar(Result[0], aCapacity * NODE_SIZE, 0);
end;

class constructor TGHashAdjList.Init;
begin
{$PUSH}{$J+}
  MAX_CAPACITY := LGUtils.RoundUpTwoPower(MAX_CAPACITY);
{$POP}
end;

class operator TGHashAdjList.Initialize(var aList: TGHashAdjList);
begin
  aList.FCount := 0;
end;

procedure TGHashAdjList.Assign(constref aSrc: TGHashAdjList);
begin
  FNodeList := System.Copy(aSrc.FNodeList);
  FCount := aSrc.Count;
  FShift := aSrc.Shift;
  Vertex := aSrc.Vertex;
  FInDegree := aSrc.FInDegree;
  FCompIndex := aSrc.FCompIndex;
end;

function TGHashAdjList.GetEnumerator: TEnumerator;
begin
  Result.FLastIndex := System.High(FNodeList);
  if Result.FLastIndex >= 0 then
    Result.FList := @FNodeList[0]
  else
    Result.FList := nil;
  Result.FCurrIndex := -1;
end;

function TGHashAdjList.ToArray: TAdjItemArray;
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

function TGHashAdjList.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TGHashAdjList.NonEmpty: Boolean;
begin
  Result := Count <> 0;
end;

procedure TGHashAdjList.Clear;
begin
  FNodeList := nil;
  FCount := 0;
end;

procedure TGHashAdjList.MakeEmpty;
var
  I: SizeInt;
begin
  for I := 0 to System.High(FNodeList) do
    FNodeList[I] := Default(TNode);
  FCount := 0;
end;

procedure TGHashAdjList.TrimToFit;
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

function TGHashAdjList.Contains(aDst: SizeInt): Boolean;
begin
  Result := Find(aDst) <> nil;
end;

function TGHashAdjList.FindOrAdd(aDst: SizeInt; out e: PAdjItem): Boolean;
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

function TGHashAdjList.Find(aDst: SizeInt): PAdjItem;
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

function TGHashAdjList.FindFirst(out aDst: SizeInt): Boolean;
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

function TGHashAdjList.Add(constref aItem: TAdjItem): Boolean;
var
  p: PAdjItem;
begin
  Result := not FindOrAdd(aItem.Key, p);
  if Result then
    p^ := aItem;
end;

function TGHashAdjList.Remove(aDst: SizeInt): Boolean;
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

{ TGAdjList.TEnumerator }

function TGAdjList.TEnumerator.GetCurrent: PAdjItem;
begin
  Result := @FList[FCurrIndex];
end;

function TGAdjList.TEnumerator.MoveNext: Boolean;
begin
  Result := FCurrIndex < FLastIndex;
  FCurrIndex += Ord(Result);
end;

procedure TGAdjList.TEnumerator.Reset;
begin
  FCurrIndex := -1;
end;

{ TGAdjList }

function TGAdjList.GetCapacity: SizeInt;
begin
  Result := System.Length(FList);
end;

procedure TGAdjList.Expand;
begin
  System.SetLength(FList, Capacity + EXPAND_SIZE);
end;

function TGAdjList.DoFind(aValue: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  for I := 0 to Pred(Count) do
    if FList[I].Destination = aValue then
      exit(I);
  Result := -1;
end;

procedure TGAdjList.DoRemove(aIndex: SizeInt);
begin
  FList[aIndex] := Default(TAdjItem);
  Dec(FCount);
  if aIndex < Count then
    begin
      FList[aIndex] := FList[Count];
      FList[Count] := Default(TAdjItem);
    end;
end;

class operator TGAdjList.Initialize(var aList: TGAdjList);
begin
  aList.Clear;
end;

procedure TGAdjList.Assign(constref aSrc: TGAdjList);
begin
  FList := System.Copy(aSrc.FList);
  FCount := aSrc.Count;
  Vertex := aSrc.Vertex;
  FInDegree := aSrc.FInDegree;
  FCompIndex := aSrc.FCompIndex;
end;

function TGAdjList.GetEnumerator: TEnumerator;
begin
  Result.FList := Pointer(FList);
  Result.FLastIndex := Pred(Count);
  Result.FCurrIndex := -1;
end;

function TGAdjList.ToArray: TAdjItemArray;
begin
  Result := System.Copy(FList, 0, Count);
end;

function TGAdjList.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TGAdjList.NonEmpty: Boolean;
begin
  Result := Count <> 0;
end;

procedure TGAdjList.Clear;
begin
  FList := nil;
  FCount := 0;
end;

procedure TGAdjList.MakeEmpty;
var
  I: SizeInt;
begin
  for I := 0 to Pred(Count) do
    FList[I] := Default(TAdjItem);
  FCount := 0;
end;

procedure TGAdjList.TrimToFit;
begin
  System.SetLength(FList, Count);
end;

function TGAdjList.Contains(aDst: SizeInt): Boolean;
begin
  if Count <> 0 then
    Result := DoFind(aDst) >= 0
  else
    Result := False;
end;

function TGAdjList.FindOrAdd(aDst: SizeInt; out p: PAdjItem): Boolean;
var
  Pos: SizeInt;
begin
  if Count <> 0 then
    Pos := DoFind(aDst)
  else
    Pos := -1;
  Result := Pos >= 0;
  if not Result then
    begin
      if Count = Capacity then
        Expand;
      Pos := Count;
      Inc(FCount);
    end;
  p := @FList[Pos];
end;

function TGAdjList.Find(aDst: SizeInt): PAdjItem;
var
  Pos: SizeInt;
begin
  Result := nil;
  if Count <> 0 then
    begin
      Pos := DoFind(aDst);
      if Pos >= 0 then
        Result := @FList[Pos];
    end;
end;

function TGAdjList.FindFirst(out aDst: SizeInt): Boolean;
begin
  Result := Count <> 0;
  if Result then
    aDst := FList[0].Destination;
end;

function TGAdjList.Add(constref aItem: TAdjItem): Boolean;
begin
  if Count <> 0 then
    Result := DoFind(aItem.Destination) < 0
  else
    Result := True;
  if Result then
    begin
      if Count = Capacity then
        Expand;
      FList[Count] := aItem;
      Inc(FCount);
    end;
end;

function TGAdjList.Remove(aDst: SizeInt): Boolean;
var
  Pos: SizeInt;
begin
  if Count <> 0 then
    begin
      Pos := DoFind(aDst);
      Result := Pos >= 0;
      if Result then
        DoRemove(Pos);
    end
  else
    Result := False;
end;

{ TGCustomGraph.TIntList }

procedure TGCustomGraph.TIntList.Expand;
begin
  System.SetLength(Items, System.Length(Items) + EXPAND_SIZE);
end;

function TGCustomGraph.TIntList.Contains(aValue: SizeInt): Boolean;
begin
  Result := Find(aValue) >= 0;
end;

function TGCustomGraph.TIntList.Find(aValue: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  for I := 0 to Pred(Count) do
    if Items[I] = aValue then
      exit(I);
  Result := -1;
end;

function TGCustomGraph.TIntList.Add(constref aValue: SizeInt): Boolean;
begin
  if Count <> 0 then
    Result := Find(aValue) < 0
  else
    Result := True;
  if Result then
    begin
      if Count = System.Length(Items) then
        Expand;
      Items[Count] := aValue;
      Inc(Count);
    end;
end;

class operator TGCustomGraph.TIntList.Initialize(var aList: TIntList);
begin
  aList.Count := 0;
end;

{ TGCustomGraph.TSkeleton }

function TGCustomGraph.TSkeleton.GetSize: SizeInt;
begin
  Result := System.Length(AdjLists);
end;

procedure TGCustomGraph.TSkeleton.SetSize(aValue: SizeInt);
begin
  if aValue > Size then
    System.SetLength(AdjLists, aValue);
end;

function TGCustomGraph.TSkeleton.ContainsEdge(constref aSrc, aDst: SizeInt): Boolean;
begin
  if (aSrc >= 0) and (aSrc < Size) then
    Result := AdjLists[aSrc].Contains(aDst)
  else
    Result := False;
end;

function TGCustomGraph.TSkeleton.AddEdge(constref aSrc, aDst: SizeInt): Boolean;
begin
  if (aSrc < 0) or (aSrc >= Size) or (aDst < 0) or (aDst >= Size) then
    exit(False);
  Result := AdjLists[aSrc].Add(aDst);
  if Result then
    begin
      AdjLists[aDst].Add(aSrc);
      Inc(EdgeCount);
    end;
end;

class operator TGCustomGraph.TSkeleton.Initialize(var s: TSkeleton);
begin
  s.EdgeCount := 0;
end;

{ TGCustomGraph.TNode }

procedure TGCustomGraph.TNode.Assign(constref aSrc: TNode);
begin
  Hash := aSrc.Hash;
  Next := aSrc.Next;
  Item.Assign(aSrc.Item);
end;

{ TGCustomGraph.TAdjVerticesPtr }

function TGCustomGraph.TAdjVerticesPtr.GetEnumerator: TAdjList.TEnumerator;
begin
  Result := FGraph.AdjList[FSource]^.GetEnumerator;
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
  Result.FEnum := FGraph.AdjList[FSource]^.GetEnumerator;
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
  Result.FEnum := FGraph.AdjList[FSource]^.GetEnumerator;
end;

{ TGCustomGraph.TVertexEnumerator }

function TGCustomGraph.TVertexEnumerator.GetCurrent: TVertex;
begin
  Result := FNodeList[FCurrIndex].Item.Vertex;
end;

constructor TGCustomGraph.TVertexEnumerator.Create(aGraph: TGCustomGraph);
begin
  FNodeList := Pointer(aGraph.FNodeList);
  FCurrIndex := -1;
  FLastIndex := Pred(aGraph.VertexCount);
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
  Result.FList := FGraph.FNodeList;
  Result.FLastIndex := Pred(FGraph.VertexCount);
  Result.FCurrIndex := -1;
  Result.FEnumDone := True;
end;

{ TGCustomGraph }

function TGCustomGraph.GetCapacity: SizeInt;
begin
  Result := System.Length(FNodeList);
end;

function TGCustomGraph.GetItem(aIndex: SizeInt): TVertex;
begin
  CheckIndexRange(aIndex);
  Result := FNodeList[aIndex].Item.Vertex;
end;

procedure TGCustomGraph.SetItem(aIndex: SizeInt; const aValue: TVertex);
var
  I: SizeInt;
begin
  CheckIndexRange(aIndex);
  if TVertexEqRel.Equal(aValue, FNodeList[aIndex].Item.Vertex) then
    exit;
  RemoveFromChain(aIndex);
  //add to new chain
  FNodeList[aIndex].Hash := TVertexEqRel.HashCode(aValue);
  FNodeList[aIndex].Item.Vertex := aValue;
  I := FNodeList[aIndex].Hash and System.High(FNodeList);
  FNodeList[aIndex].Next := FChainList[I];
  FChainList[I] := aIndex;
end;

function TGCustomGraph.GetAdjList(aIndex: SizeInt): PAdjList;
begin
  Result := @FNodeList[aIndex].Item;
end;

procedure TGCustomGraph.InitialAlloc;
begin
  System.SetLength(FNodeList, DEFAULT_CONTAINER_CAPACITY);
  System.SetLength(FChainList, DEFAULT_CONTAINER_CAPACITY);
  System.FillChar(FChainList[0], DEFAULT_CONTAINER_CAPACITY * SizeOf(SizeInt), $ff);
end;

procedure TGCustomGraph.Rehash;
var
  I, J, Mask: SizeInt;
begin
  Mask := System.High(FChainList);
  System.FillChar(FChainList[0], Succ(Mask) * SizeOf(SizeInt), $ff);
  for I := 0 to Pred(VertexCount) do
    begin
      J := FNodeList[I].Hash and Mask;
      FNodeList[I].Next := FChainList[J];
      FChainList[J] := I;
    end;
end;

procedure TGCustomGraph.Resize(aNewCapacity: SizeInt);
begin
  System.SetLength(FNodeList, aNewCapacity);
  System.SetLength(FChainList, aNewCapacity);
  Rehash;
end;

procedure TGCustomGraph.Expand;
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

function TGCustomGraph.DoAdd(constref v: TVertex; aHash: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  Result := VertexCount;
  FNodeList[Result].Hash := aHash;
  I := aHash and System.High(FNodeList);
  FNodeList[Result].Next := FChainList[I];
  FNodeList[Result].Item.Vertex := v;
  FChainList[I] := Result;
  Inc(FCount);
end;

procedure TGCustomGraph.RemoveFromChain(aIndex: SizeInt);
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

procedure TGCustomGraph.DoDelete(aIndex: SizeInt);
begin
  Dec(FCount);
  if aIndex < VertexCount then
    begin
      FNodeList[aIndex].Item := Default(TAdjList);
      System.Move(FNodeList[Succ(aIndex)], FNodeList[aIndex], (VertexCount - aIndex) * NODE_SIZE);
      System.FillChar(FNodeList[VertexCount].Item, SizeOf(TAdjList), 0);
      Rehash;
    end
  else   // last element
    begin
      RemoveFromChain(aIndex);
      System.FillChar(FNodeList[VertexCount].Item, SizeOf(TAdjList), 0);
    end;
end;

function TGCustomGraph.DoRemove(constref v: TVertex): Boolean;
var
  ToRemove: SizeInt;
begin
  ToRemove := Find(v);
  Result := ToRemove >= 0;
  if Result then
    DoDelete(ToRemove);
end;

//class operator TGVertexHashList.Copy(constref aSrc: TGVertexHashList; var aDst: TGVertexHashList);
//var
//  I: SizeInt;
//begin
//  aDst.FCount := aSrc.Count;
//  if aSrc.Count = 0 then
//    exit;
//  aDst.FChainList := System.Copy(aSrc.FChainList);
//  System.SetLength(aDst.FNodeList, System.Length(aSrc.FNodeList));
//  for I := 0 to Pred(aSrc.Count) do
//    aDst.FNodeList[I].Assign(aSrc.FNodeList[I]);
//end;

function TGCustomGraph.Find(constref v: TVertex): SizeInt;
var
  h: SizeInt;
begin
  h := TVertexEqRel.HashCode(v);
  Result := FChainList[h and System.High(FChainList)];
  while Result <> NULL_INDEX do
    begin
      if (FNodeList[Result].Hash = h) and TVertexEqRel.Equal(FNodeList[Result].Item.Vertex, v) then
        exit;
      Result := FNodeList[Result].Next;
    end;
end;

function TGCustomGraph.Find(constref v: TVertex; aHash: SizeInt): SizeInt;
begin
  Result := FChainList[aHash and System.High(FChainList)];
  while Result <> NULL_INDEX do
    begin
      if (FNodeList[Result].Hash = aHash) and TVertexEqRel.Equal(FNodeList[Result].Item.Vertex, v) then
        exit;
      Result := FNodeList[Result].Next;
    end;
end;

function TGCustomGraph.FindOrAdd(constref v: TVertex; out aIndex: SizeInt): Boolean;
var
  h: SizeInt;
begin
  h := TVertexEqRel.HashCode(v);
  if VertexCount > 0 then
    aIndex := Find(v, h)
  else
    aIndex := NULL_INDEX;
  Result := aIndex >= 0;
  if not Result then
    begin
      if VertexCount = Capacity then
        Expand;
      aIndex := DoAdd(v, h);
    end;
end;

function TGCustomGraph.Remove(constref v: TVertex): Boolean;
begin
  if VertexCount > 0 then
    Result := DoRemove(v)
  else
    Result := False;
end;

procedure TGCustomGraph.Delete(aIndex: SizeInt);
begin
  CheckIndexRange(aIndex);
  DoDelete(aIndex);
end;

class constructor TGCustomGraph.Init;
begin
{$PUSH}{$J+}
  MAX_CAPACITY := LGUtils.RoundUpTwoPower(MAX_CAPACITY);
{$POP}
  CFData := Default(TEdgeData);
end;

procedure TGCustomGraph.CheckIndexRange(aIndex: SizeInt);
begin
  if (aIndex < 0) or (aIndex >= VertexCount) then
    raise ELGListError.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
end;

function TGCustomGraph.GetEdgeDataPtr(aSrc, aDst: SizeInt): PEdgeData;
begin
  Result := @AdjList[aSrc]^.Find(aDst)^.Data;
end;

function TGCustomGraph.AdjVerticesPtr(aSrc: SizeInt): TAdjVerticesPtr;
begin
  CheckIndexRange(aSrc);
  Result.FGraph := Self;
  Result.FSource := aSrc;
end;

function TGCustomGraph.NonRecDfs(aRoot: SizeInt): SizeInt;
var
  Stack: TIntStack;
  Visited: TBitVector;
  AdjEnums: TAdjEnumArray;
  Next: SizeInt;
begin
  Visited.Size := VertexCount;
  AdjEnums := CreateAdjEnumArray;
  Visited[aRoot] := True;
  {%H-}Stack.Push(aRoot);
  Result := 1;
  while Stack.TryPeek(aRoot) do
    if AdjEnums[aRoot].MoveNext then
      begin
        Next := AdjEnums[aRoot].Current;
        if not Visited[Next] then
          begin
            //on white
            Visited[Next] := True;
            Stack.Push(Next);
            Inc(Result);
            //on gray
          end;
      end
    else
      begin
        Stack.Pop;
        //on black
      end;
end;

class function TGCustomGraph.TreeToChain(constref aTree: TIntArray; aIndex: SizeInt): TIntArray;
var
  v: TIntVector;
begin
  while aIndex >= 0 do
    begin
      if aIndex < System.Length(aTree) then
        v.Add(aIndex)
      else
        raise ELGraphError.CreateFmt(SEIndexOutOfBoundsFmt,[aIndex]);
      aIndex := aTree[aIndex];
    end;
  Result := v.ToArray;
  TIntHelper.Reverse(Result);
end;

class procedure TGCustomGraph.Tree2Chain(constref aTree: TIntArray; aIndex: SizeInt; var v: TIntVector);
begin
  while aIndex >= 0 do
    begin
      if aIndex < System.Length(aTree) then
        v.Add(aIndex)
      else
        raise ELGraphError.CreateFmt(SEIndexOutOfBoundsFmt,[aIndex]);
      aIndex := aTree[aIndex];
    end;
  v.TrimToFit;
  TIntVectorHelper.Reverse(v);
end;

class function TGCustomGraph.TreeToCycle(constref aTree: TIntArray; aFirst, aLast: SizeInt): TIntArray;
var
  I: SizeInt;
  v: TIntVector;
begin
  I := aLast;
  while I >= 0 do
    begin
      if I < System.Length(aTree) then
        v.Add(I)
      else
        raise ELGraphError.CreateFmt(SEIndexOutOfBoundsFmt,[I]);
      if I = aFirst then
        break;
      I := aTree[I];
    end;
  v.Add(aLast);
  Result := v.ToArray;
  TIntHelper.Reverse(Result);
end;

class procedure TGCustomGraph.Tree2Cycle(constref aTree: TIntArray; aFirst, aLast: SizeInt; var v: TIntVector);
var
  I: SizeInt;
begin
  I := aLast;
  while I >= 0 do
    begin
      if I < System.Length(aTree) then
        v.Add(I)
      else
        raise ELGraphError.CreateFmt(SEIndexOutOfBoundsFmt,[I]);
      if I = aFirst then
        break;
      I := aTree[I];
    end;
  v.Add(aLast);
  v.TrimToFit;
  TIntVectorHelper.Reverse(v);
end;

class function TGCustomGraph.TreeToCycleLen(constref aTree: TIntArray; aFirst, aLast: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  I := aLast;
  while I >= 0 do
    begin
      if I < System.Length(aTree) then
        Inc(Result)
      else
        raise ELGraphError.CreateFmt(SEIndexOutOfBoundsFmt,[I]);
      if I = aFirst then
        break;
      I := aTree[I];
    end;
end;

constructor TGCustomGraph.Create;
begin
  Title := 'Untitled';
end;

constructor TGCustomGraph.Create(aCapacity: SizeInt);
begin
  Title := 'Untitled';
  EnsureCapacity(aCapacity);
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

function TGCustomGraph.CreateColorArray: TColorArray;
var
  c: SizeInt;
begin
  c := VertexCount;
  System.SetLength(Result, c);
  if c > 0 then
    System.FillChar(Result[0], c, 0);
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

function TGCustomGraph.CreateAdjEnumArray: TAdjEnumArray;
var
  I: SizeInt;
begin
  System.SetLength(Result, VertexCount);
  for I := 0 to Pred(VertexCount) do
    Result[I].FEnum := AdjList[I]^.GetEnumerator;
end;

function TGCustomGraph.IsEmpty: Boolean;
begin
  Result := VertexCount = 0;
end;

function TGCustomGraph.NonEmpty: Boolean;
begin
  Result := VertexCount <> 0;
end;

procedure TGCustomGraph.Clear;
begin
  FNodeList := nil;
  FChainList := nil;
  FCount := 0;
  FEdgeCount := 0;
  FTitle := '';
end;

procedure TGCustomGraph.EnsureCapacity(aValue: SizeInt);
begin
  if aValue > Capacity then
    begin
      if aValue <= DEFAULT_CONTAINER_CAPACITY then
        aValue := DEFAULT_CONTAINER_CAPACITY
      else
        if aValue <= MAX_CAPACITY then
          aValue := LGUtils.RoundUpTwoPower(aValue)
        else
          raise ELGCapacityExceed.CreateFmt(SECapacityExceedFmt, [aValue]);
      Resize(aValue);
    end;
end;

procedure TGCustomGraph.TrimToFit;
var
  I, NewCapacity: SizeInt;
begin
  if VertexCount > 0 then
    begin
      NewCapacity := LGUtils.RoundUpTwoPower(VertexCount shl 1);
      if NewCapacity < Capacity then
        begin
          for I := 0 to Pred(VertexCount) do
            FNodeList[I].Item.TrimToFit;
          Resize(NewCapacity);
        end;
    end
  else
    Clear;
end;

function TGCustomGraph.ContainsVertex(constref aVertex: TVertex): Boolean;
begin
  Result := IndexOf(aVertex) >= 0;
end;

function TGCustomGraph.ContainsEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := ContainsEdgeI(IndexOf(aSrc), IndexOf(aDst));
end;

function TGCustomGraph.ContainsEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  Result := AdjList[aSrc]^.Contains(aDst);
end;

function TGCustomGraph.IndexOf(constref aVertex: TVertex): SizeInt;
begin
  if VertexCount > 0 then
    Result := Find(aVertex)
  else
    Result := NULL_INDEX;
end;

function TGCustomGraph.Adjacent(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := AdjacentI(IndexOf(aSrc), IndexOf(aDst));
end;

function TGCustomGraph.AdjacentI(aSrc, aDst: SizeInt): Boolean;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  Result := AdjList[aSrc]^.Contains(aDst);
end;

function TGCustomGraph.AdjVertices(constref aSrc: TVertex): TAdjVertices;
begin
  Result := AdjVerticesI(IndexOf(aSrc));
end;

function TGCustomGraph.AdjVerticesI(aSrc: SizeInt): TAdjVertices;
begin
  CheckIndexRange(aSrc);
  Result := TAdjVertices.Create(Self, aSrc);
end;

function TGCustomGraph.IncidentEdges(constref aSrc: TVertex): TIncidentEdges;
begin
  Result := IncidentEdgesI(IndexOf(aSrc));
end;

function TGCustomGraph.IncidentEdgesI(aSrc: SizeInt): TIncidentEdges;
begin
  CheckIndexRange(aSrc);
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

function TGCustomGraph.GetEdgeData(constref aSrc, aDst: TVertex; out aData: TEdgeData): Boolean;
begin
  Result := GetEdgeDataI(IndexOf(aSrc), IndexOf(aDst), aData);
end;

function TGCustomGraph.GetEdgeDataI(aSrc, aDst: SizeInt; out aData: TEdgeData): Boolean;
var
  p: PAdjItem;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  p := AdjList[aSrc]^.Find(aDst);
  Result := p <> nil;
  if Result then
    aData := p^.Data;
end;

function TGCustomGraph.SetEdgeData(constref aSrc, aDst: TVertex; constref aValue: TEdgeData): Boolean;
begin
  Result := SetEdgeDataI(IndexOf(aSrc), IndexOf(aDst), aValue);
end;

function TGCustomGraph.SetEdgeDataI(aSrc, aDst: SizeInt; constref aValue: TEdgeData): Boolean;
var
  p: PAdjItem;
begin
  CheckIndexRange(aSrc);
  p := AdjList[aSrc]^.Find(aDst);
  Result := p <> nil;
  if Result then
    p^.Data := aValue;
end;

function TGCustomGraph.DfsTraversal(constref aRoot: TVertex; OnAccept: TOnAccept;
  OnFound: TOnVisit): SizeInt;
begin
  Result := DfsTraversalI(IndexOf(aRoot), OnAccept, OnFound);
end;

function TGCustomGraph.DfsTraversalI(aRoot: SizeInt; OnAccept: TOnAccept; OnFound: TOnVisit): SizeInt;
var
  Stack: TIntStack;
  Visited: TBitVector;
  AdjEnums: TAdjEnumArray;
  Next: SizeInt;
begin
  Result := 0;
  CheckIndexRange(aRoot);
  Visited.Size := VertexCount;
  AdjEnums := CreateAdjEnumArray;
  if Assigned(OnFound) then
    OnFound(aRoot);
  Visited[aRoot] := True;
  {%H-}Stack.Push(aRoot);
  Result := 1;
  if Assigned(OnAccept) then
    OnAccept(aRoot);
  while Stack.TryPeek(aRoot) do
    if AdjEnums[aRoot].MoveNext then
      begin
        Next := AdjEnums[aRoot].Current;
        if not Visited[Next] then
          begin
            if Assigned(OnFound) then
              OnFound(Next);
            Visited[Next] := True;
            Stack.Push(Next);
            Inc(Result);
            if Assigned(OnAccept) and not OnAccept(Next) then
              break;
          end;
      end
    else
      Stack.Pop;
end;

function TGCustomGraph.BfsTraversal(constref aRoot: TVertex; OnAccept: TOnAccept; OnFound: TOnVisit): SizeInt;
begin
  Result := BfsTraversalI(IndexOf(aRoot), OnAccept, OnFound);
end;

function TGCustomGraph.BfsTraversalI(aRoot: SizeInt; OnAccept: TOnAccept; OnFound: TOnVisit): SizeInt;
var
  Visited: TBitVector;
  Queue: TIntQueue;
begin
  Result := 0;
  CheckIndexRange(aRoot);
  Visited.Size := VertexCount;
  {%H-}Queue.EnsureCapacity(VertexCount);
  if Assigned(OnFound) then
    OnFound(aRoot);
  repeat
    if Assigned(OnAccept) and not OnAccept(aRoot) then
      exit;
    for aRoot in AdjVerticesI(aRoot) do
      if not Visited[aRoot] then
        begin
          if Assigned(OnFound) then
            OnFound(aRoot);
          Visited[aRoot] := True;
          Inc(Result);
          Queue.Enqueue(aRoot);
        end;
  until not Queue.TryDequeue(aRoot);
end;

function TGCustomGraph.IsBipartite: Boolean;
var
  Colors: TColorArray;
begin
  Result := IsBipartite(Colors);
end;

function TGCustomGraph.IsBipartite(out aColors: TColorArray): Boolean;
var
  Visited: TBitVector;
  Queue: TIntQueue;
  Curr, I: SizeInt;
  CurrColor: TVertexColor;
begin
  aColors := CreateColorArray;
  if VertexCount < 2 then
    exit(False);
  Visited.Size := VertexCount;
  {%H-}Queue.EnsureCapacity(VertexCount);
  for I := 0 to Pred(System.Length(aColors)) do
    if not Visited[I] then
      begin
        Curr := I;
        repeat
          if aColors[Curr] = vclNone then
            aColors[Curr] := vclWhite;
          CurrColor := aColors[Curr];
          for Curr in AdjVerticesI(Curr) do
            if not Visited[Curr] then
              begin
                Visited[Curr] := True;
                Queue.Enqueue(Curr);
                aColors[Curr] := vclBlack - CurrColor;
              end
            else
              if aColors[Curr] = CurrColor then
                begin
                  aColors := nil;
                  exit(False);
                end;
        until not Queue.TryDequeue(Curr);
      end;
  Result := True;
end;

function TGCustomGraph.ShortestPathLen(constref aSrc, aDst: TVertex): SizeInt;
begin
  Result := ShortestPathLenI(IndexOf(aSrc), IndexOf(aDst));
end;

function TGCustomGraph.ShortestPathLenI(aSrc, aDst: SizeInt): SizeInt;
var
  Queue: TIntQueue;
  Dist: TIntArray;
  Next: SizeInt;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
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
  Result := ShortestPathsMapI(IndexOf(aRoot));
end;

function TGCustomGraph.ShortestPathsMapI(aRoot: SizeInt): TIntArray;
var
  Queue: TIntQueue;
  Next: SizeInt;
begin
  CheckIndexRange(aRoot);
  Result := CreateIntArray;
  Result[aRoot] := 0;
  {%H-}Queue.EnsureCapacity(VertexCount);
  repeat
    for Next in AdjVerticesI(aRoot) do
      if Result[Next] = -1 then
        begin
          Queue.Enqueue(Next);
          Result[Next] := Succ(Result[aRoot]);
        end;
  until not Queue.TryDequeue(aRoot);
end;

function TGCustomGraph.ShortestPath(constref aSrc, aDst: TVertex): TIntArray;
begin
  Result := ShortestPathI(IndexOf(aSrc), IndexOf(aDst));
end;

function TGCustomGraph.ShortestPathI(aSrc, aDst: SizeInt): TIntArray;
var
  Queue: TIntQueue;
  Visited: TBitVector;
  Parents: TIntArray;
  Next: SizeInt;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  Parents := CreateIntArray;
  Visited.Size := VertexCount;
  Visited[aSrc] := True;
  repeat
    if aSrc = aDst then
      exit(TreeToChain(Parents, aDst));
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

