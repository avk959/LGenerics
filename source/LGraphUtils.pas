{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
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
  LGHashTable,
  LGHash,
  LGStrConst;

type

  generic TGOnAddEdge<T>     = procedure(constref aSrc, aDst: T; aData: Pointer) of object;
  generic TGOnStreamRead<T>  = procedure(aStream: TStream; out aValue: T) of object;
  generic TGOnStreamWrite<T> = procedure(aStream: TStream; constref aValue: T) of object;

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
  TVertexColor     = 0..3;

const
  vclNone:  TVertexColor = 0;
  vclWhite: TVertexColor = 1;
  vclGray:  TVertexColor = 2;
  vclBlack: TVertexColor = 3;

type
  TColorArray = array of TVertexColor;

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

  TBitVector = record
  private
  type
    TBits = array of SizeUInt;
  var
    FBits: TBits;
    function  GetBit(aIndex: SizeInt): Boolean; inline;
    function  GetSize: SizeInt; inline;
    procedure SetBit(aIndex: SizeInt; aValue: Boolean); inline;
    procedure SetSize(aValue: SizeInt);
  public
    procedure ClearBits; inline;
    procedure ExpandTrue(aSize: SizeInt);
  { size can only grow and is always multiple of BitsizeOf(SizeUInt) }
    property  Size: SizeInt read GetSize write SetSize;
  { indices does not checks }
    property  Bits[aIndex: SizeInt]: Boolean read GetBit write SetBit; default;
  end;

  TSquareBitMatrix = record
  private
  type
    TBits = array of SizeUInt;
  var
    FBits: TBits;
    FSize: SizeUInt;
    function  GetBit(I, J: SizeInt): Boolean; inline;
    function  GetSize: SizeInt; inline;
    procedure SetBit(I, J: SizeInt; aValue: Boolean); inline;
    class operator Initialize(var aMatrix: TSquareBitMatrix);
  public
    class function MaxSize: SizeInt; static; inline;
    constructor Create(aSize: SizeInt);
    procedure ClearBits; inline;
    procedure Clear; inline;
    property  Size: SizeInt read GetSize;
  { indices does not checks }
    property  Bits[I, J: SizeInt]: Boolean read GetBit write SetBit; default;
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
    PAdjItem      = ^TAdjItem;

    TAdjItem      = record
      Destination: SizeInt;
      Data: TEdgeData;
      property Key: SizeInt read Destination;
      constructor Create(aDst: SizeInt; constref aData: TEdgeData);
    end;

    TAdjacencyMatrix = record
    private
      FMatrix: TSquareBitMatrix;
      function  GetSize: SizeInt; inline;
      procedure Clear; inline;
    public
      constructor Create(constref aMatrix: TSquareBitMatrix);
      function  IsEmpty: Boolean; inline;
      function  Adjacent(aSrc, aDst: SizeInt): Boolean; inline;
      property  Size: SizeInt read GetSize;
    end;

  protected
  const
    ADJ_EXPAND_SIZE = 8;

  type

    TAdjList = record
    public
    type
      TEnumerator = record
      private
        pCurr,
        pLast: PAdjItem;
        function  GetCurrent: PAdjItem; inline;
      public
        function  MoveNext: Boolean; inline;
        property  Current: PAdjItem read GetCurrent;
      end;

    private
    type
      TAdjItemArray = array of TAdjItem;

    var
      FList: TAdjItemArray;
      FCount: SizeInt;
      function  GetCapacity: SizeInt; inline;
      procedure Expand; inline;
      function  DoFind(aValue: SizeInt): SizeInt;
      procedure DoRemove(aIndex: SizeInt);
      class operator Initialize(var aList: TAdjList);
      class operator Copy(constref aSrc: TAdjList; var aDst: TAdjList);
    public
      function  GetEnumerator: TEnumerator; inline;
      function  ToArray: TAdjItemArray; inline;
      function  IsEmpty: Boolean; inline;
      function  NonEmpty: Boolean; inline;
      procedure Clear;
      procedure MakeEmpty;
      procedure TrimToFit; inline;
      function  Contains(aDst: SizeInt): Boolean; inline;
      function  ContainsAll(constref aList: TAdjList): Boolean;
      function  FindOrAdd(aDst: SizeInt; out p: PAdjItem): Boolean; inline;
      function  Find(aDst: SizeInt): PAdjItem;
      function  FindFirst(out aDst: SizeInt): Boolean;
      function  Add(constref aItem: TAdjItem): Boolean;
      function  Remove(aDst: SizeInt): Boolean; inline;
      property  Count: SizeInt read FCount;
      property  Capacity: SizeInt read GetCapacity;
    end;

    TNode = record
      Hash,
      Next: SizeInt;
      AdjList: TAdjList;
      Vertex: TVertex;
      Tag: SizeInt;
      procedure Assign(constref aSrc: TNode);
    end;
    PNode = ^TNode;

    TNodeList   = array of TNode;
    TChainList  = array of SizeInt;

  const
    NULL_INDEX  = SizeInt(-1);

  type
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
    private
      FItems: TIntArray;
      FCount: SizeInt;
      procedure Expand; inline;
      function  GetItem(aIndex: SizeInt): SizeInt; inline;
      class operator Initialize(var aList: TIntList);
    public
    type
      TEnumerator = record
      private
        pCurr,
        pLast: PSizeInt;
        function  GetCurrent: SizeInt; inline;
      public
        function  MoveNext: Boolean; inline;
        property  Current: SizeInt read GetCurrent;
      end;

      procedure InitRange(aRange: SizeInt);
      function  GetEnumerator: TEnumerator;
      procedure Assign(constref aList: TIntList);
      procedure Assign(constref aList: TAdjList);
      function  Copy: TIntList; inline;
      function  IsEmpty: Boolean; inline;
      function  NonEmpty: Boolean; inline;
      procedure MakeEmpty; inline;
      function  Contains(aValue: SizeInt): Boolean; inline;
      function  ContainsAll(constref aList: TIntList): Boolean; inline;
      function  Find(aValue: SizeInt): SizeInt;
      function  FindFirst(out aDst: SizeInt): Boolean;
      function  Add(aValue: SizeInt): Boolean;
      function  Remove(aValue: SizeInt): Boolean;
      property  Count: SizeInt read FCount;
      property  Items[aIndex: SizeInt]: SizeInt read GetItem; default;
    end;
    PIntList = ^TIntList;

    TSkeleton = record
    private
      FAdjLists: array of TIntList;
      FEdgeCount: SizeInt;
      FDirected: Boolean;
      function  GetAdjList(aIndex: SizeInt): PIntList; inline;
      function  GetDegree(aIndex: SizeInt): SizeInt; inline;
      function  GetVertexCount: SizeInt; inline;
    public
      constructor Create(aVertCount: SizeInt; aDirected: Boolean = False);
      constructor Create(constref s: TSkeleton);
      function ContainsEdge(aSrc, aDst: SizeInt): Boolean; inline;
      function AddEdge(aSrc, aDst: SizeInt): Boolean;
      function RemoveEdge(aSrc, aDst: SizeInt): Boolean;
      property VertexCount: SizeInt read GetVertexCount;
      property Directed: Boolean read FDirected;
      property EdgeCount: SizeInt read FEdgeCount;
      property Degree[aIndex: SizeInt]: SizeInt read GetDegree;
      property AdjLists[aIndex: SizeInt]: PIntList read GetAdjList; default;
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
    procedure InitialAlloc;
    procedure Rehash;
    procedure Resize(aNewCapacity: SizeInt);
    procedure Expand;
    function  Add(constref v: TVertex; aHash: SizeInt): SizeInt;
    procedure RemoveFromChain(aIndex: SizeInt);
    procedure Delete(aIndex: SizeInt);
    function  Remove(constref v: TVertex): Boolean;
    function  Find(constref v: TVertex): SizeInt;
    function  Find(constref v: TVertex; aHash: SizeInt): SizeInt;
    function  FindOrAdd(constref v: TVertex; out aIndex: SizeInt): Boolean;
    class constructor Init;
  protected
    function  GetEdgeDataPtr(aSrc, aDst: SizeInt): PEdgeData; inline;
    function  NonRecDfs(aRoot: SizeInt): SizeInt;
    procedure CheckIndexRange(aIndex: SizeInt);
    function  CheckPathExists(aSrc, aDst: SizeInt): Boolean;
  public
  type
    TOnAddEdge     = specialize TGOnAddEdge<TVertex>;

    TOnReadVertex  = specialize TGOnStreamRead<TVertex>;
    TOnWriteVertex = specialize TGOnStreamWrite<TVertex>;

    TOnReadData    = specialize TGOnStreamRead<TEdgeData>;
    TOnWriteData   = specialize TGOnStreamWrite<TEdgeData>;

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
    class function  MaxBitMatrixSize: SizeInt; static; inline;
    class function  TreeToChain(constref aTree: TIntArray; aRoot: SizeInt): TIntArray; static;
    class procedure Tree2Chain(constref aTree: TIntArray; aRoot: SizeInt; var v: TIntVector); static;
    class function  TreeToCycle(constref aTree: TIntArray; aFirst, aLast: SizeInt): TIntArray; static;
    class procedure Tree2Cycle(constref aTree: TIntArray;  aFirst, aLast: SizeInt; var v: TIntVector); static;
    class function  Tree2CycleLen(constref aTree: TIntArray; aFirst, aLast: SizeInt): SizeInt; static;
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
  { enumerates indices of adjacent vertices of aVertex }
    function  AdjVertices(constref aVertex: TVertex): TAdjVertices; inline;
    function  AdjVerticesI(aIndex: SizeInt): TAdjVertices;
  { enumerates incident edges of aVertex }
    function  IncidentEdges(constref aVertex: TVertex): TIncidentEdges; inline;
    function  IncidentEdgesI(aIndex: SizeInt): TIncidentEdges;
  { enumerates all vertices }
    function  Vertices: TVertices; inline;
  { enumerates all edges }
    function  Edges: TEdges; inline;
    function  GetEdgeData(constref aSrc, aDst: TVertex; out aData: TEdgeData): Boolean; inline;
    function  GetEdgeDataI(aSrc, aDst: SizeInt; out aData: TEdgeData): Boolean; inline;
    function  SetEdgeData(constref aSrc, aDst: TVertex; constref aValue: TEdgeData): Boolean; inline;
    function  SetEdgeDataI(aSrc, aDst: SizeInt; constref aValue: TEdgeData): Boolean;
  { returns adjacency matrix;
    warning: maximal matrix size limited, see MaxBitMatrixSize }
    function  CreateAdjacencyMatrix: TAdjacencyMatrix;
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
  { returns an array containing chain of vertex indices of found shortest path(in sense 'edges count'),
    empty if path does not exists }
    function  ShortestPath(constref aSrc, aDst: TVertex): TIntArray; inline;
    function  ShortestPathI(aSrc, aDst: SizeInt): TIntArray;
  { returns adjacency matrix of the complement graph;
    warning: maximal matrix size limited, see MaxBitMatrixSize }
    function  Complement: TAdjacencyMatrix;
    property  Title: string read FTitle write FTitle;
    property  VertexCount: SizeInt read FCount;
    property  EdgeCount: SizeInt read FEdgeCount;
    property  Capacity: SizeInt read GetCapacity;
    property  Items[aIndex: SizeInt]: TVertex read GetItem write SetItem; default;
  end;

  generic TGWeighedEdge<TWeight> = record
    Source,
    Destination: SizeInt;
    Weight:  TWeight;
    class operator = (constref L, R: TGWeighedEdge): Boolean; inline;
    class operator <>(constref L, R: TGWeighedEdge): Boolean; inline;
    class operator > (constref L, R: TGWeighedEdge): Boolean; inline;
    class operator < (constref L, R: TGWeighedEdge): Boolean; inline;
    class operator >=(constref L, R: TGWeighedEdge): Boolean; inline;
    class operator <=(constref L, R: TGWeighedEdge): Boolean; inline;
    constructor Create(s, d: SizeInt; w: TWeight);
  end;

  generic TGWeighedItem<TWeight> = record
    Weight: TWeight;
    Index: SizeInt;
    class operator = (constref L, R: TGWeighedItem): Boolean; inline;
    class operator <>(constref L, R: TGWeighedItem): Boolean; inline;
    class operator > (constref L, R: TGWeighedItem): Boolean; inline;
    class operator < (constref L, R: TGWeighedItem): Boolean; inline;
    class operator >=(constref L, R: TGWeighedItem): Boolean; inline;
    class operator <=(constref L, R: TGWeighedItem): Boolean; inline;
    constructor Create(constref w: TWeight; aIndex: SizeInt);
  end;

  generic TGRankWeighedItem<TWeight> = record
    Rank,
    Weight: TWeight;
    Index: SizeInt;
    class operator = (constref L, R: TGRankWeighedItem): Boolean; inline;
    class operator <>(constref L, R: TGRankWeighedItem): Boolean; inline;
    class operator > (constref L, R: TGRankWeighedItem): Boolean; inline;
    class operator < (constref L, R: TGRankWeighedItem): Boolean; inline;
    class operator >=(constref L, R: TGRankWeighedItem): Boolean; inline;
    class operator <=(constref L, R: TGRankWeighedItem): Boolean; inline;
    constructor Create(constref aRank, aWeight: TWeight; aIndex: SizeInt);
  end;

  TDisjointSetUnion = record
  private
    FList: array of SizeInt;
    function  GetSize: SizeInt; inline;
    procedure SetSize(aValue: SizeInt);
  public
    procedure Clear; inline;
    procedure Reset;
  { values related to the same set will have the same Lead }
    function  Lead(aValue: SizeInt): SizeInt;
    function  InSameSet(L, R: SizeInt): Boolean; inline;
    function  InDiffSets(L, R: SizeInt): Boolean; inline;
  { if L and R related to the different sets, these sets will be merged into one with a single Lead }
    procedure Union(L, R: SizeInt);
    property  Size: SizeInt read GetSize write SetSize;
  end;

  TIntSet = record
  private
  type
    TIntEntry = record
      Key: SizeInt;
    end;
    TTable = specialize TGLiteIntHashTable<SizeInt, TIntEntry>;
    PEntry = TTable.PEntry;
  public
  type
    TEnumerator = record
    private
      FEnum: TTable.TEnumerator;
      function  GetCurrent: SizeInt; inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: SizeInt read GetCurrent;
    end;

  private
    FTable: TTable;
    function  GetCount: SizeInt; inline;
  public
    function  GetEnumerator: TEnumerator; inline;
    function  ToArray: TIntArray;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure MakeEmpty; inline;
    procedure Clear; inline;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    function  Contains(aValue: SizeInt): Boolean; inline;
    function  Add(aValue: SizeInt): Boolean;
    function  AddAll(constref a: array of SizeInt): SizeInt;
    function  AddAll(constref s: TIntSet): SizeInt;
    function  Remove(aValue: SizeInt): Boolean; inline;
    property  Count: SizeInt read GetCount;
  end;

  TIntPair = record
  private
    FLess,
    FGreater: SizeInt;
    function GetKey: TIntPair; inline;
  public
    class function HashCode(const aValue: TIntPair): SizeInt; static; inline;
    class function Equal(const L, R: TIntPair): Boolean; static; inline;
    constructor Create(L, R: SizeInt);
    property Left: SizeInt read FLess;
    property Right: SizeInt read FGreater;
    property Key: TIntPair read GetKey;
  end;

  PIntPair = ^TIntPair;

  TIntPairSet = record
  private
  type
    TTable = specialize TGLiteHashTableLP<TIntPair, TIntPair, TIntPair>;
  var
    FTable: TTable;
    function GetCount: SizeInt; inline;
  public
    function Contains(L, R: SizeInt): Boolean; inline;
    function Add(L, R: SizeInt): Boolean;
    function Remove(L, R: SizeInt): Boolean; inline;
    property Count: SizeInt read GetCount;
  end;

  generic TGBinHeapMin<T> = record
  private
  type
    THeap = array of T;

  var
    FHeap: THeap;
    FHandle2Index: TIntArray;
    FIndex2Handle: TIntArray;
    FCount: SizeInt;
    function  GetCapacity: SizeInt; inline;
    procedure Expand;
    procedure FloatUp(aIndex: SizeInt);
    procedure SiftDown(aIndex: SizeInt);
  public
    constructor Create(aSize: SizeInt);
    function  NotUsed(aHandle: SizeInt): Boolean; inline;
    function  TryDequeue(out aValue: T): Boolean;
    procedure Enqueue(constref aValue: T; aHandle: SizeInt);
    procedure Update(aHandle: SizeInt; constref aNewValue: T);
    function  Peek(aHandle: SizeInt): T; inline;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
  end;



implementation
{$B-}{$COPERATORS ON}

const
{$IF DEFINED(CPU64)}
  INT_SIZE_LOG  = 6;
  INT_SIZE_MASK = 63;
{$ELSEIF DEFINED(CPU32)}
  INT_SIZE_LOG  = 5;
  INT_SIZE_MASK = 31;
{$ELSE}
  INT_SIZE_LOG  = 4;
  INT_SIZE_MASK = 15;
{$ENDIF}

{ TIntEdge }

constructor TIntEdge.Create(s, d: SizeInt);
begin
  Source := s;
  Destination := d;
end;

{ TGCustomGraph.TAdjItem }

constructor TGCustomGraph.TAdjItem.Create(aDst: SizeInt; constref aData: TEdgeData);
begin
  Destination := aDst;
  Data := aData;
end;

{ TGCustomGraph.TAdjacencyMatrix }

function TGCustomGraph.TAdjacencyMatrix.GetSize: SizeInt;
begin
  Result := FMatrix.Size;
end;

procedure TGCustomGraph.TAdjacencyMatrix.Clear;
begin
  if FMatrix.Size > 0 then
    FMatrix.Clear;
end;

constructor TGCustomGraph.TAdjacencyMatrix.Create(constref aMatrix: TSquareBitMatrix);
begin
  FMatrix := aMatrix;
end;

function TGCustomGraph.TAdjacencyMatrix.IsEmpty: Boolean;
begin
  Result := FMatrix.Size = 0;
end;

function TGCustomGraph.TAdjacencyMatrix.Adjacent(aSrc, aDst: SizeInt): Boolean;
begin
  Result := FMatrix[aSrc, aDst];
end;

{ TGCustomGraph.TAdjList.TEnumerator }

function TGCustomGraph.TAdjList.TEnumerator.GetCurrent: PAdjItem;
begin
  Result := pCurr;
end;

function TGCustomGraph.TAdjList.TEnumerator.MoveNext: Boolean;
begin
  Result := pCurr < pLast;
  Inc(pCurr, Ord(Result));
end;

{ TGCustomGraph.TAdjList }

function TGCustomGraph.TAdjList.GetCapacity: SizeInt;
begin
  Result := System.Length(FList);
end;

procedure TGCustomGraph.TAdjList.Expand;
begin
  System.SetLength(FList, Capacity + ADJ_EXPAND_SIZE);
end;

function TGCustomGraph.TAdjList.DoFind(aValue: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  for I := 0 to Pred(Count) do
    if FList[I].Destination = aValue then
      exit(I);
  Result := -1;
end;

procedure TGCustomGraph.TAdjList.DoRemove(aIndex: SizeInt);
begin
  FList[aIndex] := Default(TAdjItem);
  Dec(FCount);
  if aIndex < Count then
    begin
      FList[aIndex] := FList[Count];
      FList[Count] := Default(TAdjItem);
    end;
end;

class operator TGCustomGraph.TAdjList.Initialize(var aList: TAdjList);
begin
  aList.FCount := 0;
end;

class operator TGCustomGraph.TAdjList.Copy(constref aSrc: TAdjList; var aDst: TAdjList);
begin
  aDst.FList := System.Copy(aSrc.FList);
  aDst.FCount := aSrc.Count;
end;

function TGCustomGraph.TAdjList.GetEnumerator: TEnumerator;
begin
  Result.pCurr := PAdjItem(Pointer(FList)) - Ord(Count > 0);
  Result.pLast := PAdjItem(Pointer(FList)) + Pred(Count) and (-SizeInt(Count > 0));
end;

function TGCustomGraph.TAdjList.ToArray: TAdjItemArray;
begin
  Result := System.Copy(FList, 0, Count);
end;

function TGCustomGraph.TAdjList.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TGCustomGraph.TAdjList.NonEmpty: Boolean;
begin
  Result := Count <> 0;
end;

procedure TGCustomGraph.TAdjList.Clear;
begin
  FList := nil;
  FCount := 0;
end;

procedure TGCustomGraph.TAdjList.MakeEmpty;
var
  I: SizeInt;
begin
  for I := 0 to Pred(Count) do
    FList[I] := Default(TAdjItem);
  FCount := 0;
end;

procedure TGCustomGraph.TAdjList.TrimToFit;
begin
  System.SetLength(FList, Count);
end;

function TGCustomGraph.TAdjList.Contains(aDst: SizeInt): Boolean;
begin
  if Count <> 0 then
    Result := DoFind(aDst) >= 0
  else
    Result := False;
end;

function TGCustomGraph.TAdjList.ContainsAll(constref aList: TAdjList): Boolean;
var
  I, J, v: SizeInt;
  Found: Boolean;
begin
  for I := 0 to Pred(aList.Count) do
    begin
      Found := False;
      v := aList.FList[I].Key;
      for J := 0 to Pred(Count) do
        if FList[J].Key = v then
          begin
            Found := True;
            break;
          end;
      if not Found then
        exit(False);
    end;
  Result := True;
end;

function TGCustomGraph.TAdjList.FindOrAdd(aDst: SizeInt; out p: PAdjItem): Boolean;
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

function TGCustomGraph.TAdjList.Find(aDst: SizeInt): PAdjItem;
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

function TGCustomGraph.TAdjList.FindFirst(out aDst: SizeInt): Boolean;
begin
  Result := Count <> 0;
  if Result then
    aDst := FList[0].Destination;
end;

function TGCustomGraph.TAdjList.Add(constref aItem: TAdjItem): Boolean;
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

function TGCustomGraph.TAdjList.Remove(aDst: SizeInt): Boolean;
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

{ TGCustomGraph.TIntList.TEnumerator }

function TGCustomGraph.TIntList.TEnumerator.GetCurrent: SizeInt;
begin
  Result := pCurr^;
end;

function TGCustomGraph.TIntList.TEnumerator.MoveNext: Boolean;
begin
  Result := pCurr < pLast;
  Inc(pCurr, Ord(Result));
end;

{ TGCustomGraph.TIntList }

procedure TGCustomGraph.TIntList.Expand;
begin
  System.SetLength(FItems, System.Length(FItems) + ADJ_EXPAND_SIZE);
end;

class operator TGCustomGraph.TIntList.Initialize(var aList: TIntList);
begin
  aList.FCount := 0;
end;

procedure TGCustomGraph.TIntList.InitRange(aRange: SizeInt);
var
  I: SizeInt;
begin
  System.SetLength(FItems, aRange);
  for I := 0 to Pred(aRange) do
    FItems[I] := I;
end;

function TGCustomGraph.TIntList.GetEnumerator: TEnumerator;
begin
  Result.pCurr := PSizeInt(Pointer(FItems)) - Ord(Count > 0);
  Result.pLast := PSizeInt(Pointer(FItems)) + Pred(Count) and (-SizeInt(Count > 0));
end;

procedure TGCustomGraph.TIntList.Assign(constref aList: TIntList);
begin
  FItems := System.Copy(aList.FItems);
  FCount := aList.Count;
end;

procedure TGCustomGraph.TIntList.Assign(constref aList: TAdjList);
var
  I: SizeInt;
begin
  FCount := aList.Count;
  System.SetLength(FItems, FCount);
  for I := 0 to Pred(Count) do
    FItems[I] := aList.FList[I].Destination;
end;

function TGCustomGraph.TIntList.Copy: TIntList;
begin
  Result.Assign(Self);
end;

function TGCustomGraph.TIntList.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TGCustomGraph.TIntList.NonEmpty: Boolean;
begin
  Result := Count <> 0;
end;

procedure TGCustomGraph.TIntList.MakeEmpty;
begin
  FCount := 0;
end;

function TGCustomGraph.TIntList.GetItem(aIndex: SizeInt): SizeInt;
begin
  Result := FItems[aIndex];
end;

function TGCustomGraph.TIntList.Contains(aValue: SizeInt): Boolean;
begin
  Result := Find(aValue) >= 0;
end;

function TGCustomGraph.TIntList.ContainsAll(constref aList: TIntList): Boolean;
var
  I, J, v: SizeInt;
  Found: Boolean;
begin
  for I := 0 to Pred(aList.Count) do
    begin
      Found := False;
      v := aList.FItems[I];
      for J := 0 to Pred(Count) do
        if FItems[J] = v then
          begin
            Found := True;
            break;
          end;
      if not Found then
        exit(False);
    end;
  Result := True;
end;

function TGCustomGraph.TIntList.Find(aValue: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  for I := 0 to Pred(Count) do
    if FItems[I] = aValue then
      exit(I);
  Result := -1;
end;

function TGCustomGraph.TIntList.FindFirst(out aDst: SizeInt): Boolean;
begin
  Result := Count <> 0;
  if Result then
    aDst := FItems[0];
end;

function TGCustomGraph.TIntList.Add(aValue: SizeInt): Boolean;
begin
  if Count <> 0 then
    Result := Find(aValue) < 0
  else
    Result := True;
  if Result then
    begin
      if Count = System.Length(FItems) then
        Expand;
      FItems[Count] := aValue;
      Inc(FCount);
    end;
end;

function TGCustomGraph.TIntList.Remove(aValue: SizeInt): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(Count) do
    if FItems[I] = aValue then
      begin
        Dec(FCount);
        if I < Count then
          FItems[I] := FItems[Count];
        exit(True);
      end;
  Result := False;
end;

{ TGCustomGraph.TSkeleton }

function TGCustomGraph.TSkeleton.GetVertexCount: SizeInt;
begin
  Result := System.Length(FAdjLists);
end;

function TGCustomGraph.TSkeleton.GetAdjList(aIndex: SizeInt): PIntList;
begin
  Result := @FAdjLists[aIndex];
end;

function TGCustomGraph.TSkeleton.GetDegree(aIndex: SizeInt): SizeInt;
begin
  Result := FAdjLists[aIndex].Count;
end;

constructor TGCustomGraph.TSkeleton.Create(aVertCount: SizeInt; aDirected: Boolean);
begin
  System.SetLength(FAdjLists, aVertCount);
  FEdgeCount := 0;
  FDirected := aDirected;
end;

constructor TGCustomGraph.TSkeleton.Create(constref s: TSkeleton);
var
  I: SizeInt;
begin
  System.SetLength(FAdjLists, s.VertexCount);
  FEdgeCount := s.EdgeCount;
  FDirected := s.Directed;
  for I := 0 to Pred(s.VertexCount) do
    FAdjLists[I].Assign(s.FAdjLists[I]);
end;

function TGCustomGraph.TSkeleton.ContainsEdge(aSrc, aDst: SizeInt): Boolean;
begin
  if (aSrc >= 0) and (aSrc < VertexCount) then
    Result := FAdjLists[aSrc].Contains(aDst)
  else
    Result := False;
end;

function TGCustomGraph.TSkeleton.AddEdge(aSrc, aDst: SizeInt): Boolean;
begin
  if (aSrc < 0) or (aSrc >= VertexCount) or (aDst < 0) or (aDst >= VertexCount) then
    exit(False);
  Result := FAdjLists[aSrc].Add(aDst);
  if Result then
    begin
      if not Directed then
        FAdjLists[aDst].Add(aSrc);
      Inc(FEdgeCount);
    end;
end;

function TGCustomGraph.TSkeleton.RemoveEdge(aSrc, aDst: SizeInt): Boolean;
begin
  if aSrc = aDst then
    exit(False);
  Result := FAdjLists[aSrc].Remove(aDst);
  if Result then
    begin
      if not Directed then
        FAdjLists[aDst].Remove(aSrc);
      Dec(FEdgeCount);
    end;
end;

{ TGCustomGraph.TNode }

procedure TGCustomGraph.TNode.Assign(constref aSrc: TNode);
begin
  Hash := aSrc.Hash;
  Next := aSrc.Next;
  AdjList := aSrc.AdjList;
  Vertex := aSrc.Vertex;
  Tag := aSrc.Tag;
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

{ TGCustomGraph.TAdjVertices }

constructor TGCustomGraph.TAdjVertices.Create(aGraph: TGCustomGraph; aSource: SizeInt);
begin
  FGraph := aGraph;
  FSource := aSource;
end;

function TGCustomGraph.TAdjVertices.GetEnumerator: TAdjEnumerator;
begin
  Result.FEnum := FGraph.FNodeList[FSource].AdjList.GetEnumerator;
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

{ TGCustomGraph.TIncidentEdges }

constructor TGCustomGraph.TIncidentEdges.Create(aGraph: TGCustomGraph; aSource: SizeInt);
begin
  FGraph := aGraph;
  FSource := aSource;
end;

function TGCustomGraph.TIncidentEdges.GetEnumerator: TIncidentEnumerator;
begin
  Result.FEnum := FGraph.FNodeList[FSource].AdjList.GetEnumerator;
end;

{ TGCustomGraph.TVertexEnumerator }

function TGCustomGraph.TVertexEnumerator.GetCurrent: TVertex;
begin
  Result := FNodeList[FCurrIndex].Vertex;
end;

constructor TGCustomGraph.TVertexEnumerator.Create(aGraph: TGCustomGraph);
begin
  FNodeList := Pointer(aGraph.FNodeList);
  FLastIndex := Pred(aGraph.VertexCount);
  FCurrIndex := -1;
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
        FEnum := FList[FCurrIndex].AdjList.GetEnumerator;
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
  Result := FNodeList[aIndex].Vertex;
end;

procedure TGCustomGraph.SetItem(aIndex: SizeInt; const aValue: TVertex);
var
  I: SizeInt;
begin
  CheckIndexRange(aIndex);
  if TVertexEqRel.Equal(aValue, FNodeList[aIndex].Vertex) then
    exit;
  RemoveFromChain(aIndex);
  //add to new chain
  FNodeList[aIndex].Hash := TVertexEqRel.HashCode(aValue);
  FNodeList[aIndex].Vertex := aValue;
  I := FNodeList[aIndex].Hash and System.High(FNodeList);
  FNodeList[aIndex].Next := FChainList[I];
  FChainList[I] := aIndex;
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
begin
  if Capacity = 0 then
    begin
      InitialAlloc;
      exit;
    end;
  if Capacity < MAX_POSITIVE_POW2 then
    Resize(Capacity shl 1)
  else
    raise ELGraphError.CreateFmt(SECapacityExceedFmt, [Capacity shl 1]);
end;

function TGCustomGraph.Add(constref v: TVertex; aHash: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  Result := VertexCount;
  FNodeList[Result].Hash := aHash;
  I := aHash and System.High(FNodeList);
  FNodeList[Result].Next := FChainList[I];
  FNodeList[Result].Vertex := v;
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

procedure TGCustomGraph.Delete(aIndex: SizeInt);
begin
  Dec(FCount);
  if aIndex < VertexCount then
    begin
      FNodeList[aIndex].AdjList := Default(TAdjList);
      System.Move(FNodeList[Succ(aIndex)], FNodeList[aIndex], (VertexCount - aIndex) * SizeOf(TNode));
      System.FillChar(FNodeList[VertexCount].AdjList, SizeOf(TAdjList), 0);
      Rehash;
    end
  else   // last element
    begin
      RemoveFromChain(aIndex);
      System.FillChar(FNodeList[VertexCount].AdjList, SizeOf(TAdjList), 0);
    end;
end;

function TGCustomGraph.Remove(constref v: TVertex): Boolean;
var
  ToRemove: SizeInt;
begin
  if NonEmpty then
    begin
      ToRemove := Find(v);
      Result := ToRemove >= 0;
      if Result then
        Delete(ToRemove);
    end
  else
    Result := False;
end;

function TGCustomGraph.Find(constref v: TVertex): SizeInt;
var
  h: SizeInt;
begin
  h := TVertexEqRel.HashCode(v);
  Result := FChainList[h and System.High(FChainList)];
  while Result <> NULL_INDEX do
    begin
      if (FNodeList[Result].Hash = h) and TVertexEqRel.Equal(FNodeList[Result].Vertex, v) then
        exit;
      Result := FNodeList[Result].Next;
    end;
end;

function TGCustomGraph.Find(constref v: TVertex; aHash: SizeInt): SizeInt;
begin
  Result := FChainList[aHash and System.High(FChainList)];
  while Result <> NULL_INDEX do
    begin
      if (FNodeList[Result].Hash = aHash) and TVertexEqRel.Equal(FNodeList[Result].Vertex, v) then
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
      aIndex := Add(v, h);
    end;
end;

class constructor TGCustomGraph.Init;
begin
  CFData := Default(TEdgeData);
end;

procedure TGCustomGraph.CheckIndexRange(aIndex: SizeInt);
begin
  if (aIndex < 0) or (aIndex >= VertexCount) then
    raise ELGListError.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
end;

function TGCustomGraph.CheckPathExists(aSrc, aDst: SizeInt): Boolean;
var
  Queue: TIntQueue;
  Visited: TBitVector;
begin
  if aSrc = aDst then
    exit(True);
  Visited.Size := VertexCount;
  Visited[aSrc] := True;
  repeat
    for aSrc in AdjVerticesI(aSrc) do
      if not Visited[aSrc] then
        begin
          if aSrc = aDst then
            exit(True);
          Visited[aSrc] := True;
          Queue.Enqueue(aSrc);
        end;
  until not Queue{%H-}.TryDequeue(aSrc);
  Result := False;
end;

function TGCustomGraph.GetEdgeDataPtr(aSrc, aDst: SizeInt): PEdgeData;
begin
  Result := @FNodeList[aSrc].AdjList.Find(aDst)^.Data;
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
            Inc(Result);
            Stack.Push(Next);
            //on gray
          end;
      end
    else
      begin
        Stack.Pop;
        //on black
      end;
end;

class function TGCustomGraph.MaxBitMatrixSize: SizeInt;
begin
  Result := TSquareBitMatrix.MaxSize;
end;

class function TGCustomGraph.TreeToChain(constref aTree: TIntArray; aRoot: SizeInt): TIntArray;
var
  v: TIntVector;
begin
  while aRoot >= 0 do
    begin
      if aRoot < System.Length(aTree) then
        v.Add(aRoot)
      else
        raise ELGraphError.CreateFmt(SEIndexOutOfBoundsFmt,[aRoot]);
      aRoot := aTree[aRoot];
    end;
  Result := v.ToArray;
  TIntHelper.Reverse(Result);
end;

class procedure TGCustomGraph.Tree2Chain(constref aTree: TIntArray; aRoot: SizeInt; var v: TIntVector);
begin
  while aRoot >= 0 do
    begin
      if aRoot < System.Length(aTree) then
        v.Add(aRoot)
      else
        raise ELGraphError.CreateFmt(SEIndexOutOfBoundsFmt,[aRoot]);
      aRoot := aTree[aRoot];
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

class function TGCustomGraph.Tree2CycleLen(constref aTree: TIntArray; aFirst, aLast: SizeInt): SizeInt;
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
    Result[I].FEnum := FNodeList[I].AdjList.GetEnumerator;
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
    if aValue < MAX_CONTAINER_SIZE div SizeOf(TNode) then
      Resize(LGUtils.RoundUpTwoPower(aValue))
    else
      raise ELGraphError.CreateFmt(SECapacityExceedFmt, [aValue]);
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
            FNodeList[I].AdjList.TrimToFit;
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
  if (aSrc >= 0) and (aSrc < VertexCount) then
    Result := FNodeList[aSrc].AdjList.Contains(aDst)
  else
    Result := False;
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
  if (aSrc >= 0) and (aSrc < VertexCount) then
    Result := FNodeList[aSrc].AdjList.Contains(aDst)
  else
    Result := False;
end;

function TGCustomGraph.AdjVertices(constref aVertex: TVertex): TAdjVertices;
begin
  Result := AdjVerticesI(IndexOf(aVertex));
end;

function TGCustomGraph.AdjVerticesI(aIndex: SizeInt): TAdjVertices;
begin
  CheckIndexRange(aIndex);
  Result := TAdjVertices.Create(Self, aIndex);
end;

function TGCustomGraph.IncidentEdges(constref aVertex: TVertex): TIncidentEdges;
begin
  Result := IncidentEdgesI(IndexOf(aVertex));
end;

function TGCustomGraph.IncidentEdgesI(aIndex: SizeInt): TIncidentEdges;
begin
  CheckIndexRange(aIndex);
  Result := TIncidentEdges.Create(Self, aIndex);
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
  p := FNodeList[aSrc].AdjList.Find(aDst);
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
  p := FNodeList[aSrc].AdjList.Find(aDst);
  Result := p <> nil;
  if Result then
    p^.Data := aValue;
end;

function TGCustomGraph.CreateAdjacencyMatrix: TAdjacencyMatrix;
var
  m: TSquareBitMatrix;
  s, d: SizeInt;
begin
  if IsEmpty then
    exit(Default(TAdjacencyMatrix));
  m := TSquareBitMatrix.Create(VertexCount);
  for s := 0 to Pred(VertexCount) do
    for d := 0 to Pred(VertexCount) do
      if (s <> d) and FNodeList[s].AdjList.Contains(d) then
        m[s, d] := True;
  Result := TAdjacencyMatrix.Create(m);
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
  until not Queue{%H-}.TryDequeue(aRoot);
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
                aColors[Curr] := vclBlack - CurrColor;
                Queue.Enqueue(Curr);
              end
            else
              if aColors[Curr] = CurrColor then
                begin
                  aColors := nil;
                  exit(False);
                end;
        until not Queue{%H-}.TryDequeue(Curr);
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

function TGCustomGraph.Complement: TAdjacencyMatrix;
var
  m: TSquareBitMatrix;
  s, d: SizeInt;
begin
  if IsEmpty then
    exit(Default(TAdjacencyMatrix));
  m := TSquareBitMatrix.Create(VertexCount);
  for s := 0 to Pred(VertexCount) do
    for d := 0 to Pred(VertexCount) do
      if (s <> d) and not FNodeList[s].AdjList.Contains(d) then
        m[s, d] := True;
  Result := TAdjacencyMatrix.Create(m);
end;

{ TGWeighedEdge }

class operator TGWeighedEdge. = (constref L, R: TGWeighedEdge): Boolean;
begin
  Result := L.Weight = R.Weight;
end;

class operator TGWeighedEdge.<>(constref L, R: TGWeighedEdge): Boolean;
begin
  Result := L.Weight <> R.Weight;
end;

class operator TGWeighedEdge.>(constref L, R: TGWeighedEdge): Boolean;
begin
  Result := L.Weight > R.Weight;
end;

class operator TGWeighedEdge.<(constref L, R: TGWeighedEdge): Boolean;
begin
  Result := L.Weight < R.Weight;
end;

class operator TGWeighedEdge.>=(constref L, R: TGWeighedEdge): Boolean;
begin
  Result := L.Weight >= R.Weight;
end;

class operator TGWeighedEdge.<=(constref L, R: TGWeighedEdge): Boolean;
begin
  Result := L.Weight <= R.Weight;
end;

constructor TGWeighedEdge.Create(s, d: SizeInt; w: TWeight);
begin
  Source := s;
  Destination := d;
  Weight := w;
end;

{ TGWeighedItem }

class operator TGWeighedItem. = (constref L, R: TGWeighedItem): Boolean;
begin
  Result := L.Weight = R.Weight;
end;

class operator TGWeighedItem.<>(constref L, R: TGWeighedItem): Boolean;
begin
  Result := L.Weight <> R.Weight;
end;

class operator TGWeighedItem.>(constref L, R: TGWeighedItem): Boolean;
begin
  Result := L.Weight > R.Weight;
end;

class operator TGWeighedItem.<(constref L, R: TGWeighedItem): Boolean;
begin
  Result := L.Weight < R.Weight;
end;

class operator TGWeighedItem.>=(constref L, R: TGWeighedItem): Boolean;
begin
  Result := L.Weight >= R.Weight;
end;

class operator TGWeighedItem.<=(constref L, R: TGWeighedItem): Boolean;
begin
  Result := L.Weight <= R.Weight;
end;

constructor TGWeighedItem.Create(constref w: TWeight; aIndex: SizeInt);
begin
  Weight := w;
  Index := aIndex;
end;

{ TGRankWeighedItem }

class operator TGRankWeighedItem. = (constref L, R: TGRankWeighedItem): Boolean;
begin
  Result := L.Rank = R.Rank;
end;

class operator TGRankWeighedItem.<>(constref L, R: TGRankWeighedItem): Boolean;
begin
  Result := L.Rank <> R.Rank;
end;

class operator TGRankWeighedItem.>(constref L, R: TGRankWeighedItem): Boolean;
begin
  Result := L.Rank > R.Rank;
end;

class operator TGRankWeighedItem.<(constref L, R: TGRankWeighedItem): Boolean;
begin
  Result := L.Rank < R.Rank;
end;

class operator TGRankWeighedItem.>=(constref L, R: TGRankWeighedItem): Boolean;
begin
  Result := L.Rank >= R.Rank;
end;

class operator TGRankWeighedItem.<=(constref L, R: TGRankWeighedItem): Boolean;
begin
  Result := L.Rank <= R.Rank;
end;

constructor TGRankWeighedItem.Create(constref aRank, aWeight: TWeight; aIndex: SizeInt);
begin
  Rank := aRank;
  Weight := aWeight;
  Index := aIndex;
end;

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

function TDisjointSetUnion.Lead(aValue: SizeInt): SizeInt;
begin
  if FList[aValue] = aValue then
    exit(aValue);
  Result := Lead(FList[aValue]);
  FList[aValue] := Result;
end;

function TDisjointSetUnion.InSameSet(L, R: SizeInt): Boolean;
begin
  Result := Lead(L) = Lead(R);
end;

function TDisjointSetUnion.InDiffSets(L, R: SizeInt): Boolean;
begin
  Result := Lead(L) <> Lead(R);
end;

procedure TDisjointSetUnion.Union(L, R: SizeInt);
begin
  L := Lead(L);
  R := Lead(R);
  if Odd(Random(4)) then // random selection
    FList[L] := R
  else
    FList[R] := L;
end;

{ TBitVector }

function TBitVector.GetBit(aIndex: SizeInt): Boolean;
begin
  Result := (FBits[aIndex shr INT_SIZE_LOG] and (SizeUInt(1) shl (aIndex and INT_SIZE_MASK))) <> 0;
end;

function TBitVector.GetSize: SizeInt;
begin
  Result := System.Length(FBits) shl INT_SIZE_LOG;
end;

procedure TBitVector.SetBit(aIndex: SizeInt; aValue: Boolean);
begin
  if aValue then
    FBits[aIndex shr INT_SIZE_LOG] :=
    FBits[aIndex shr INT_SIZE_LOG] or (SizeUInt(1) shl (aIndex and INT_SIZE_MASK))
  else
    FBits[aIndex shr INT_SIZE_LOG] :=
    FBits[aIndex shr INT_SIZE_LOG] and not (SizeUInt(1) shl (aIndex and INT_SIZE_MASK));
end;

procedure TBitVector.SetSize(aValue: SizeInt);
var
  OldLen: SizeInt;
begin
  OldLen := Size;
  if aValue > OldLen then
    begin
      aValue := Succ(aValue shr INT_SIZE_LOG);
      System.SetLength(FBits, aValue);
      System.FillChar(FBits[OldLen], (aValue - OldLen) * SizeOf(SizeUInt), 0);
    end;
end;

procedure TBitVector.ClearBits;
begin
  if FBits <> nil then
    System.FillChar(FBits[0], System.Length(FBits) * SizeOf(SizeUInt), 0);
end;

procedure TBitVector.ExpandTrue(aSize: SizeInt);
var
  OldLen: SizeInt;
begin
  OldLen := Size;
  if aSize > OldLen then
    begin
      aSize := Succ(aSize shr INT_SIZE_LOG);
      System.SetLength(FBits, aSize);
      System.FillChar(FBits[OldLen], (aSize - OldLen) * SizeOf(SizeUInt), $ff);
    end;
end;

{ TSquareBitMatrix }

function TSquareBitMatrix.GetBit(I, J: SizeInt): Boolean;
begin
  Result := (FBits[(SizeUInt(I) * FSize + SizeUInt(J)) shr INT_SIZE_LOG] and
            (SizeUInt(1) shl ((SizeUInt(I) * FSize + SizeUInt(J)) and INT_SIZE_MASK))) <> 0
end;

function TSquareBitMatrix.GetSize: SizeInt;
begin
  Result := FSize;
end;

procedure TSquareBitMatrix.SetBit(I, J: SizeInt; aValue: Boolean);
begin
  if aValue then
    FBits[(SizeUInt(I) * FSize + SizeUInt(J)) shr INT_SIZE_LOG] :=
    FBits[(SizeUInt(I) * FSize + SizeUInt(J)) shr INT_SIZE_LOG] or
          (SizeUInt(1) shl ((SizeUInt(I) * FSize + SizeUInt(J)) and INT_SIZE_MASK))
  else
    FBits[(SizeUInt(I) * FSize + SizeUInt(J)) shr INT_SIZE_LOG] :=
    FBits[(SizeUInt(I) * FSize + SizeUInt(J)) shr INT_SIZE_LOG] and not
          (SizeUInt(1) shl ((SizeUInt(I) * FSize + SizeUInt(J)) and INT_SIZE_MASK));
end;

class operator TSquareBitMatrix.Initialize(var aMatrix: TSquareBitMatrix);
begin
  aMatrix.Clear;
end;

class function TSquareBitMatrix.MaxSize: SizeInt;
begin
  Result := Trunc(Sqrt(High(SizeUInt)));
end;

constructor TSquareBitMatrix.Create(aSize: SizeInt);
var
  s: SizeInt;
begin
  if aSize > 0 then
    if aSize <= MaxSize then
      begin
        FSize := aSize;
        s := Succ((FSize * FSize) shr INT_SIZE_LOG);
        System.SetLength(FBits, s);
        System.FillChar(FBits[0], s * SizeOf(SizeUInt), 0);
      end
    else
      raise ELGraphError.CreateFmt(SEMatrixSizeExceedFmt, [aSize]);
end;

procedure TSquareBitMatrix.ClearBits;
begin
  System.FillChar(FBits[0], System.Length(FBits) * SizeOf(SizeUInt), 0);
end;

procedure TSquareBitMatrix.Clear;
begin
  FBits := nil;
  FSize := 0;
end;

{ TIntSet.TEnumerator }

function TIntSet.TEnumerator.GetCurrent: SizeInt;
begin
  Result := FEnum.Current^.Key;
end;

function TIntSet.TEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TIntSet.TEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TIntSet }

function TIntSet.GetCount: SizeInt;
begin
  Result := FTable.Count;
end;

function TIntSet.GetEnumerator: TEnumerator;
begin
  Result.FEnum := FTable.GetEnumerator
end;

function TIntSet.ToArray: TIntArray;
var
  p: PEntry;
  I: SizeInt = 0;
begin
  System.SetLength(Result, Count);
  for p in FTable do
    begin
      Result[I] := p^.Key;
      Inc(I);
    end;
end;

function TIntSet.IsEmpty: Boolean;
begin
  Result := FTable.Count = 0;
end;

function TIntSet.NonEmpty: Boolean;
begin
  Result := FTable.Count <> 0;
end;

procedure TIntSet.MakeEmpty;
begin
  FTable.MakeEmpty;
end;

procedure TIntSet.Clear;
begin
  FTable.Clear;
end;

procedure TIntSet.EnsureCapacity(aValue: SizeInt);
begin
  FTable.EnsureCapacity(aValue);
end;

function TIntSet.Contains(aValue: SizeInt): Boolean;
var
  Dummy: SizeInt;
begin
  Result := FTable.Find(aValue, Dummy) <> nil;
end;

function TIntSet.Add(aValue: SizeInt): Boolean;
var
  Dummy: SizeInt;
  p: PEntry;
begin
  Result := not FTable.FindOrAdd(aValue, p, Dummy);
  if Result then
    p^.Key := aValue;
end;

function TIntSet.AddAll(constref a: array of SizeInt): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I in a do
    Result += Ord(Add(I));
end;

function TIntSet.AddAll(constref s: TIntSet): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I in s do
    Result += Ord(Add(I));
end;

function TIntSet.Remove(aValue: SizeInt): Boolean;
begin
  Result := FTable.Remove(aValue);
end;

{ TIntPair }

function TIntPair.GetKey: TIntPair;
begin
  Result := Self;
end;

class function TIntPair.HashCode(const aValue: TIntPair): SizeInt;
begin
{$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  {$IF DEFINED (CPU64)}
      Result := TxxHash32LE.HashGuid(TGuid(aValue));
  {$ELSEIF DEFINED (CPU32)}
     Result := TxxHash32LE.HashQWord(QWord(aValue));
  {$ELSE }
     Result := TxxHash32LE.HashDWord(DWord(aValue));
  {$ENDIF }
{$Else FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := TxxHash32LE.HashBuf(@aValue, SizeOf(aValue));
{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
end;

class function TIntPair.Equal(const L, R: TIntPair): Boolean;
begin
  Result := (L.Left = R.Left) and (L.Right = R.Right);
end;

constructor TIntPair.Create(L, R: SizeInt);
begin
  if L <= R then
    begin
      FLess := L;
      FGreater := R;
    end
  else
    begin
      FLess := R;
      FGreater := L;
    end;
end;

{ TIntPairSet }

function TIntPairSet.GetCount: SizeInt;
begin
  Result := FTable.Count;
end;

function TIntPairSet.Contains(L, R: SizeInt): Boolean;
var
  Dummy: SizeInt;
begin
  Result := FTable.Find(TIntPair.Create(L, R), Dummy) <> nil;
end;

function TIntPairSet.Add(L, R: SizeInt): Boolean;
var
  Dummy: SizeInt;
  p: PIntPair;
  v: TIntPair;
begin
  v := TIntPair.Create(L, R);
  Result := not FTable.FindOrAdd(v, p, Dummy);
  if Result then
    p^ := v;
end;

function TIntPairSet.Remove(L, R: SizeInt): Boolean;
begin
  Result := FTable.Remove(TIntPair.Create(L, R));
end;

{ TGBinHeapMin }

function TGBinHeapMin.GetCapacity: SizeInt;
begin
  Result := System.Length(FIndex2Handle);
end;

procedure TGBinHeapMin.Expand;
begin
  System.SetLength(FHeap, System.Length(FHeap) shl 1);
  System.SetLength(FIndex2Handle, System.Length(FIndex2Handle) shl 1);
end;

procedure TGBinHeapMin.FloatUp(aIndex: SizeInt);
var
  CurrIdx, ParentIdx, HandleIdx: SizeInt;
  v: T;
begin
  if aIndex > 0 then
    begin
      CurrIdx := aIndex;
      ParentIdx := Pred(aIndex) shr 1;
      v := FHeap[aIndex];
      HandleIdx := FIndex2Handle[aIndex];
      while (CurrIdx > 0) and (v < FHeap[ParentIdx]) do
        begin
          FHeap[CurrIdx] := FHeap[ParentIdx];
          FHandle2Index[FIndex2Handle[ParentIdx]] := CurrIdx;
          FIndex2Handle[CurrIdx] := FIndex2Handle[ParentIdx];
          CurrIdx := ParentIdx;
          ParentIdx := Pred(ParentIdx) shr 1;
        end;
      FHeap[CurrIdx] := v;
      FHandle2Index[HandleIdx] := CurrIdx;
      FIndex2Handle[CurrIdx] := HandleIdx;
    end;
end;

procedure TGBinHeapMin.SiftDown(aIndex: SizeInt);
var
  CurrIdx, NextIdx, HighIdx, HandleIdx: SizeInt;
  v: T;
begin
  HighIdx := Pred(Count);
  if HighIdx > 0 then
    begin
      CurrIdx := aIndex;
      NextIdx := Succ(aIndex shl 1);
      v := FHeap[aIndex];
      HandleIdx := FIndex2Handle[aIndex];
      while NextIdx <= HighIdx do
        begin
          if (Succ(NextIdx) <= HighIdx) and (FHeap[NextIdx] > FHeap[Succ(NextIdx)]) then
            Inc(NextIdx);
          FHeap[CurrIdx] := FHeap[NextIdx];
          FHandle2Index[FIndex2Handle[NextIdx]] := CurrIdx;
          FIndex2Handle[CurrIdx] := FIndex2Handle[NextIdx];
          CurrIdx := NextIdx;
          NextIdx := Succ(NextIdx shl 1);
        end;
      NextIdx := Pred(CurrIdx) shr 1;
      while (CurrIdx > 0) and (v < FHeap[NextIdx]) do
        begin
          FHeap[CurrIdx] := FHeap[NextIdx];
          FHandle2Index[FIndex2Handle[NextIdx]] := CurrIdx;
          FIndex2Handle[CurrIdx] := FIndex2Handle[NextIdx];
          CurrIdx := NextIdx;
          NextIdx := Pred(NextIdx) shr 1;
        end;
      FHeap[CurrIdx] := v;
      FHandle2Index[HandleIdx] := CurrIdx;
      FIndex2Handle[CurrIdx] := HandleIdx;
    end;
end;

constructor TGBinHeapMin.Create(aSize: SizeInt);
begin
  if aSize > 0 then
    begin
      FCount := 0;
      System.SetLength(FHandle2Index, aSize);
      System.FillChar(FHandle2Index[0], aSize * SizeOf(SizeInt), $ff);
      System.SetLength(FHeap, 4096 div SizeOf(SizeInt));
      System.SetLength(FIndex2Handle, 4096 div SizeOf(SizeInt));
    end;
end;

function TGBinHeapMin.NotUsed(aHandle: SizeInt): Boolean;
begin
  Result := FHandle2Index[aHandle] = -1;
end;

function TGBinHeapMin.TryDequeue(out aValue: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    begin
      Dec(FCount);
      aValue := FHeap[0];
      FHeap[0] := FHeap[Count];
      FHandle2Index[FIndex2Handle[Count]] := 0;
      FIndex2Handle[0] := FIndex2Handle[Count];
      FHeap[Count] := Default(T);
      SiftDown(0);
    end;
end;

procedure TGBinHeapMin.Enqueue(constref aValue: T; aHandle: SizeInt);
var
  InsertIdx: SizeInt;
begin
  if Count = Capacity then
    Expand;
  InsertIdx := Count;
  Inc(FCount);
  FHeap[InsertIdx] := aValue;
  FHandle2Index[aHandle] := InsertIdx;
  FIndex2Handle[InsertIdx] := aHandle;
  FloatUp(InsertIdx);
end;

procedure TGBinHeapMin.Update(aHandle: SizeInt; constref aNewValue: T);
var
  I: SizeInt;
begin
  I := FHandle2Index[aHandle];
  if aNewValue < FHeap[I] then
    begin
      FHeap[I] := aNewValue;
      FloatUp(I);
    end
  else
    if aNewValue > FHeap[I] then
      begin
        FHeap[I] := aNewValue;
        SiftDown(I);
      end;
end;

function TGBinHeapMin.Peek(aHandle: SizeInt): T;
begin
  Result := FHeap[FHandle2Index[aHandle]];
end;

end.

