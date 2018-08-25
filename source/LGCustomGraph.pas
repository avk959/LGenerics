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
unit LGCustomGraph;

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

  EGraphError      = class(Exception); //???

  TEmptyRec        = record end;

  TIntArray        = array of SizeInt;
  TShortArray      = array of ShortInt;
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
  TOnFindSet       = procedure(constref aSet: TIntArray; var aCancel: Boolean) of object;
  TVertexColor     = type Byte;

const
  vcNone:  TVertexColor = 0;
  vcWhite: TVertexColor = 1;
  vcGray:  TVertexColor = 2;
  vcBlack: TVertexColor = 3;

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

  TGraphMagic           = string[8];

const
  GRAPH_MAGIC: TGraphMagic = 'LGrphTyp';
  GRAPH_HEADER_VERSION     = 2;
  ADJLIST_EXPAND_SIZE      = 8;

type

  generic TGAdjItem<T> = record
    Destination: SizeInt;
    Data: T;
    property Key: SizeInt read Destination;
    constructor Create(aDst: SizeInt; constref aData: T);
  end;

  generic TGAdjList<T> = record   //for internal use only
  public
  type
    TAdjItem = specialize TGAdjItem<T>;
    PAdjItem = ^TAdjItem;

    TEnumerator = record
    private
      pCurr,
      pLast: PAdjItem;
      function  GetCurrent: PAdjItem; inline;
      procedure SetCurrent(aValue: PAdjItem); inline;
    public
      function  MoveNext: Boolean; inline;
      property  Current: PAdjItem read GetCurrent write SetCurrent; // ???
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
    class operator Initialize(var aList: TGAdjList);
    class operator Copy(constref aSrc: TGAdjList; var aDst: TGAdjList);
  public
    function  GetEnumerator: TEnumerator; inline;
    function  ToArray: TAdjItemArray; inline;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear;
    procedure MakeEmpty;
    procedure TrimToFit; inline;
    function  Contains(aDst: SizeInt): Boolean; inline;
    function  ContainsAll(constref aList: TGAdjList): Boolean;
    function  FindOrAdd(aDst: SizeInt; out p: PAdjItem): Boolean; inline;
    function  Find(aDst: SizeInt): PAdjItem;
    function  FindFirst(out aValue: SizeInt): Boolean;
    function  Add(constref aItem: TAdjItem): Boolean;
    function  Remove(aDst: SizeInt): Boolean; inline;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
  end;

  { TGCustomGraph: simple sparse graph abstract ancestor class based on adjacency lists;
      functor TEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGCustomGraph<TVertex, TEdgeData, TEqRel> = class abstract
  protected
  type
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
    { clears all bits }
      procedure ClearBits; inline;
    { sets all new bits }
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

    TBits256 = record
    public
    const
      BITNESS      = 256;
      BIT_PER_LIMB = BitsizeOf(SizeUInt);
      LIMB_COUNT = BITNESS div BIT_PER_LIMB;

    type
      PBits256 = ^TBits256;

      TEnumerator = record
      private
        FValue: PBits256;
        FBitIndex,
        FLimbIndex: SizeInt;
        FCurrLimb: SizeUInt;
        FInCycle: Boolean;
        function GetCurrent: SizeInt; inline;
        function FindFirst: Boolean;
      public
        function MoveNext: Boolean;
        property Current: SizeInt read GetCurrent;
      end;

      TReverseEnumerator = record
      private
        FValue: PBits256;
        FBitIndex,
        FLimbIndex: SizeInt;
        FCurrLimb: SizeUInt;
        FInCycle: Boolean;
        function GetCurrent: SizeInt; inline;
        function FindFirst: Boolean;
      public
        function MoveNext: Boolean;
        property Current: SizeInt read GetCurrent;
      end;

      TReverse = record
      private
        FValue: PBits256;
      public
        function GetEnumerator: TReverseEnumerator; inline;
      end;

    private
    type
      TBits = array[0..Pred(LIMB_COUNT)] of SizeUInt;

    var
      FBits: TBits;
      function  GetBit(aIndex: SizeInt): Boolean; inline;
      procedure SetBit(aIndex: SizeInt; aValue: Boolean); inline;
      class function  BsfValue(aValue: SizeUInt): SizeInt; static; inline;
      class function  BsrValue(aValue: SizeUInt): SizeInt; static; inline;
      class procedure ClearBit(aIndex: SizeInt; var aValue: SizeUInt); static; inline;
    public
      function  GetEnumerator: TEnumerator; inline;
      function  Reverse: TReverse; inline;
      procedure InitRange(aRange: SizeInt);
      procedure InitZero; inline;
    { returns an array containing the indices of the set bits }
      function  ToArray: TIntArray;
      function  IsEmpty: Boolean;
      function  NonEmpty: Boolean; inline;
    { returns index of the least significant bit }
      function  Bsf: SizeInt; inline;
    { returns index of the most significant bit }
      function  Bsr: SizeInt; inline;
      function  Intersecting(constref aValue: TBits256): Boolean;
    { returns the number of bits in the intersection with aValue }
      function  IntersectionPop(constref aValue: TBits256): SizeInt;
      function  Contains(constref aValue: TBits256): Boolean;
    { returns the number of bits that will be added when union with aValue }
      function  JoinGain(constref aValue: TBits256): SizeInt;
      procedure Join(constref aValue: TBits256);
      function  Union(constref aValue: TBits256): TBits256; inline;
      procedure Subtract(constref aValue: TBits256); inline;
      function  Difference(constref aValue: TBits256): TBits256; inline;
      procedure Intersect(constref aValue: TBits256); inline;
      function  Intersection(constref aValue: TBits256): TBits256; inline;
    { returns count of set bits }
      function  PopCount: SizeInt; inline;
      property  Bits[aIndex: SizeInt]: Boolean read GetBit write SetBit; default;
    end;

    TBitMatrix256 = array of TBits256;
    TBoolMatrix   = array of TBoolVector;

  public
  type
    TSpecEdgeData = TEdgeData;
    PEdgeData     = ^TEdgeData;
    TAdjItem      = specialize TGAdjItem<TEdgeData>;
    PAdjItem      = ^TAdjItem;

    TAdjacencyMatrix = record
    private
      FMatrix: TSquareBitMatrix;
      function  GetSize: SizeInt; inline;
    public
      constructor Create(constref aMatrix: TSquareBitMatrix);
      function  IsEmpty: Boolean; inline;
      function  Adjacent(aSrc, aDst: SizeInt): Boolean; inline;
      property  Size: SizeInt read GetSize;
    end;

  protected
  type
    TAdjList = specialize TGAdjList<TEdgeData>;
    PAdjList = ^TAdjList;

    TIntSet = record
    private
      FItems: TIntArray;
      FCount: SizeInt;
      procedure Expand; inline;
      function  GetItem(aIndex: SizeInt): SizeInt; inline;
      class operator Initialize(var aList: TIntSet);
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
      function  GetEnumerator: TEnumerator; inline;
      function  ToArray: TIntArray; inline;
      procedure Assign(constref aValue: TIntSet);
      procedure AssignArray(constref a: TIntArray);
      procedure AssignList(aList: PAdjList);
      function  IsEmpty: Boolean; inline;
      function  NonEmpty: Boolean; inline;
      procedure MakeEmpty; inline;
      function  FindFirst(out aValue: SizeInt): Boolean;
      function  Contains(aValue: SizeInt): Boolean; inline;
      function  ContainsAny(constref aValue: TIntSet): Boolean;
      function  ContainsAll(constref aValue: TIntSet): Boolean;
      function  Find(aValue: SizeInt): SizeInt;
      function  Add(aValue: SizeInt): Boolean;
      function  Add(constref a: TIntArray): SizeInt;
      function  Join(constref aValue: TIntSet): SizeInt;
      procedure Push(aValue: SizeInt); inline;
      function  Pop: SizeInt; inline;
      function  TryPop(out aValue: SizeInt): Boolean; inline;
      function  Last: SizeInt; inline;
    { preserves the order of the elements }
      procedure Subtract(constref aValue: TIntSet);
      procedure Subtract(PList: PAdjList);
      function  Difference(constref aValue: TIntSet): TIntSet; inline;
    { preserves the order of the elements }
      procedure Intersect(constref aValue: TIntSet);
      function  Intersection(constref aValue: TIntSet): TIntSet; inline;
    { returns the number of elements in the intersection with aValue }
      function  IntersectionCount(constref aValue: TIntSet): SizeInt;
    { returns the number of elements in the intersection with PList }
      function  IntersectionCount(PList: PAdjList): SizeInt;
      function  Remove(aValue: SizeInt): Boolean;
    { preserves the order of the elements }
      procedure Delete(aValue: SizeInt);
      procedure Reverse; inline;
      property  Count: SizeInt read FCount;
      property  Items[aIndex: SizeInt]: SizeInt read GetItem; default;
    end;
    PIntSet = ^TIntSet;

    TSkeleton = record
    private
      FAdjLists: array of TIntSet;
      FEdgeCount: SizeInt;
      FDirected: Boolean;
      function  GetAdjList(aIndex: SizeInt): PIntSet; inline;
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
      property AdjLists[aIndex: SizeInt]: PIntSet read GetAdjList; default;
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

    TStreamHeader = packed record
      Magic: TGraphMagic;
      Version: Byte;
      TitleLen,
      DescriptionLen: Word;
      VertexCount,
      EdgeCount: LongInt;
      //title as utf8string
      //description as utf8string
      //vertices
      //edges: src index, dst index as little endian LongInt, data
    end;

  class var
    CFData: TEdgeData;


  private
    FNodeList: TNodeList;
    FChainList: TChainList;
    FCount,
    FEdgeCount: SizeInt;
    FTitle: string;
    FDescription: TStrings;
    function  GetCapacity: SizeInt; inline;
    function  GetItem(aIndex: SizeInt): TVertex; inline;
    function  GetAdjList(aIndex: SizeInt): PAdjList; inline;
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
  public
  type
    TAdjEnumerator = record
    private
      FEnum: TAdjList.TEnumerator;
      function  GetCurrent: SizeInt; inline;
    public
      function  MoveNext: Boolean; inline;
      property  Current: SizeInt read GetCurrent;
    end;

    TAdjEnumArray = array of TAdjEnumerator;
  protected
    function  GetEdgeDataPtr(aSrc, aDst: SizeInt): PEdgeData; inline;
    function  NonRecDfs(aRoot: SizeInt): SizeInt;
    procedure CheckIndexRange(aIndex: SizeInt);
    function  CheckPathExists(aSrc, aDst: SizeInt): Boolean;
    function  CreateBoolMatrix: TBoolMatrix;
    function  CreateIntArray(aValue: SizeInt = -1): TIntArray;
    function  CreateIntArray(aLen, aValue: SizeInt): TIntArray;
    function  CreateIntArrayRange: TIntArray;
    function  CreateColorArray: TColorArray;
    function  CreateAdjEnumArray: TAdjEnumArray;
    property  AdjLists[aIndex: SizeInt]: PAdjList read GetAdjList;
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
      constructor Create(aSrc, aDst: SizeInt; constref d: TEdgeData);
    end;

    TIncidentEdge = record
      Destination: SizeInt;
      Data:  TEdgeData;
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
      FList: PNode;
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
{**********************************************************************************************************
  auxiliary utilities
***********************************************************************************************************}

    class function MaxBitMatrixSize: SizeInt; static; inline;
    class function TreePathTo(constref aTree: TIntArray; aValue: SizeInt): TIntArray; static;
    class function TreePathFromTo(constref aTree: TIntArray; aFrom, aTo: SizeInt): TIntArray; static;
    class function TreePathLen(constref aTree: TIntArray; aFrom, aTo: SizeInt): SizeInt; static;
    class property DefaultEdgeData: TEdgeData read CFData;

{**********************************************************************************************************
  class management utilities
***********************************************************************************************************}

    constructor Create;
    constructor Create(aCapacity: SizeInt);
    destructor Destroy; override;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear; virtual;
    procedure EnsureCapacity(aValue: SizeInt);
    procedure TrimToFit; inline;

{**********************************************************************************************************
  structural management utilities
***********************************************************************************************************}

    function ContainsVertex(constref aVertex: TVertex): Boolean; inline;
    function ContainsEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function ContainsEdgeI(aSrc, aDst: SizeInt): Boolean;
    function IndexOf(constref aVertex: TVertex): SizeInt; inline;
    function Adjacent(constref aSrc, aDst: TVertex): Boolean; inline;
    function AdjacentI(aSrc, aDst: SizeInt): Boolean;
  { enumerates indices of adjacent vertices of aVertex }
    function AdjVertices(constref aVertex: TVertex): TAdjVertices; inline;
    function AdjVerticesI(aIndex: SizeInt): TAdjVertices;
  { enumerates incident edges of aVertex }
    function IncidentEdges(constref aVertex: TVertex): TIncidentEdges; inline;
    function IncidentEdgesI(aIndex: SizeInt): TIncidentEdges;
  { enumerates all vertices }
    function Vertices: TVertices; inline;
  { enumerates all edges }
    function Edges: TEdges; inline;
    function GetEdgeData(constref aSrc, aDst: TVertex; out aData: TEdgeData): Boolean; inline;
    function GetEdgeDataI(aSrc, aDst: SizeInt; out aData: TEdgeData): Boolean; inline;
    function SetEdgeData(constref aSrc, aDst: TVertex; constref aValue: TEdgeData): Boolean; inline;
    function SetEdgeDataI(aSrc, aDst: SizeInt; constref aValue: TEdgeData): Boolean;
  { returns adjacency matrix;
    warning: maximum matrix size limited, see MaxBitMatrixSize }
    function CreateAdjacencyMatrix: TAdjacencyMatrix;
  { test whether the graph is bipartite;
    the graph can be disconnected (in this case it consists of a number of connected
    bipartite components and / or several isolated vertices)}
    function IsBipartite: Boolean;
  { test whether the graph is bipartite; if returns True then information about the vertex
    belonging to the fractions is returned in aColors(vcWhite or vcGray) }
    function IsBipartite(out aColors: TColorArray): Boolean;
  { test whether the graph is bipartite; if returns True then aWhites and aGrays will contain
    indices of correspondig vertices }
    function IsBipartite(out aWhites, aGrays: TIntArray): Boolean;

{**********************************************************************************************************
  traversal utilities
***********************************************************************************************************}

  { returns count of visited vertices; OnAccept calls after vertex visite, OnNext calls after next vertex found;
    if TOnAccept returns False then traversal stops }
    function DfsTraversal(constref aRoot: TVertex; OnAccept: TOnAccept = nil; OnFound: TOnVisit = nil): SizeInt; inline;
    function DfsTraversalI(aRoot: SizeInt; OnAccept: TOnAccept = nil; OnFound: TOnVisit = nil): SizeInt;
  { returns count of visited vertices; OnAccept calls after vertex visite, OnFound calls after vertex found;
    if TOnAccept returns False then traversal stops}
    function BfsTraversal(constref aRoot: TVertex; OnAccept: TOnAccept = nil; OnFound: TOnVisit = nil): SizeInt; inline;
    function BfsTraversalI(aRoot: SizeInt; OnAccept: TOnAccept = nil; OnFound: TOnVisit = nil): SizeInt;

{**********************************************************************************************************
  shortest path problem utilities
***********************************************************************************************************}

  { returns the length of the shortest path between the aSrc and aDst(in sense 'edges count'),
    -1 if the path does not exist }
    function ShortestPathLen(constref aSrc, aDst: TVertex): SizeInt; inline;
    function ShortestPathLenI(aSrc, aDst: SizeInt): SizeInt;
  { returns an array containing in the corresponding components the length of the shortest path from aSrc
    (in sense 'edges count'), or -1 if it unreachable }
    function ShortestPathsMap(constref aSrc: TVertex): TIntArray; inline;
    function ShortestPathsMapI(aSrc: SizeInt = 0): TIntArray;
  { returns an array containing chain of vertex indices of found shortest path(in sense 'edges count'),
    empty if path does not exists }
    function ShortestPath(constref aSrc, aDst: TVertex): TIntArray; inline;
    function ShortestPathI(aSrc, aDst: SizeInt): TIntArray;

{**********************************************************************************************************
  properties
***********************************************************************************************************}

    property Title: string read FTitle write FTitle;
    property Description: TStrings read FDescription;
    property VertexCount: SizeInt read FCount;
    property EdgeCount: SizeInt read FEdgeCount;
    property Capacity: SizeInt read GetCapacity;
    property Items[aIndex: SizeInt]: TVertex read GetItem write SetItem; default;
  end;

  TDisjointSetUnion = record
  private
    FList: array of SizeInt;
    function  GetSize: SizeInt; inline;
    procedure SetSize(aValue: SizeInt);
  public
    procedure Clear; inline;
    procedure Reset;
  { values related to the same subset will have the same Tag }
    function  Tag(aValue: SizeInt): SizeInt;
    function  InSameSet(L, R: SizeInt): Boolean; inline;
    function  InDiffSets(L, R: SizeInt): Boolean; inline;
  { returns True and merges L and R, if L and R related to the different subsets, False otherwise }
    function  Join(L, R: SizeInt): Boolean;
    property  Size: SizeInt read GetSize write SetSize;
  end;

  TIntHashSet = record
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
    function  AddAll(constref s: TIntHashSet): SizeInt;
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
    procedure Clear; inline;
    function  Contains(L, R: SizeInt): Boolean; inline;
    function  Add(L, R: SizeInt): Boolean;
    function  Remove(L, R: SizeInt): Boolean; inline;
    property  Count: SizeInt read GetCount;
  end;

  generic TGBinHeapMin<T> = record // for internal use only
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

  generic TGPairHeap<T> = record // for internal use only
  private
  type
    PNode = ^TNode;
    TNode = record
      Prev,
      Child,
      Sibling: PNode;
      Data: T;
      function AddChild(aNode: PNode): PNode; inline;
    end;

    TNodeList = array of TNode;

  var
    FNodeList: TNodeList;
    FRoot: PNode;
    FCount: SizeInt;
    function  GetCapacity: SizeInt; inline;
    function  NewNode(constref aValue: T; aHandle: SizeInt): PNode; inline;
    function  DequeueItem: T;
    procedure RootMerge(aNode: PNode); inline;
    class function  NodeMerge(L, R: PNode): PNode; static;
    class function  TwoPassMerge(aNode: PNode): PNode; static;
    class procedure CutNode(aNode: PNode); static; inline;
  public
    constructor Create(aSize: SizeInt);
    function  NotUsed(aHandle: SizeInt): Boolean; inline;
    function  TryDequeue(out aValue: T): Boolean; inline;
    procedure Enqueue(constref aValue: T; aHandle: SizeInt); inline;
    procedure Update(aHandle: SizeInt; constref aNewValue: T); inline;
    function  Peek(aHandle: SizeInt): T; inline;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
  end;

  generic TGWeightedPathHelper<TVertex, TWeight, TEdgeData, TEqRel> = class
  public
  type
    TWeightArray  = array of TWeight;

  strict private
  class var
    CFInfiniteWeight,
    CFNegInfiniteWeight,
    CFZeroWeight: TWeight;

    class constructor Init;
    class function CreateAndFill(constref aValue: TWeight; aSize: SizeInt): TWeightArray; static;
  protected
  type
    TWeightEdge = record
      Source,
      Destination: SizeInt;
      Weight:  TWeight;
      class operator = (constref L, R: TWeightEdge): Boolean; inline;
      class operator <>(constref L, R: TWeightEdge): Boolean; inline;
      class operator > (constref L, R: TWeightEdge): Boolean; inline;
      class operator < (constref L, R: TWeightEdge): Boolean; inline;
      class operator >=(constref L, R: TWeightEdge): Boolean; inline;
      class operator <=(constref L, R: TWeightEdge): Boolean; inline;
      constructor Create(s, d: SizeInt; w: TWeight);
    end;

    TWeightItem = record
      Weight: TWeight;
      Index: SizeInt;
      class operator = (constref L, R: TWeightItem): Boolean; inline;
      class operator <>(constref L, R: TWeightItem): Boolean; inline;
      class operator > (constref L, R: TWeightItem): Boolean; inline;
      class operator < (constref L, R: TWeightItem): Boolean; inline;
      class operator >=(constref L, R: TWeightItem): Boolean; inline;
      class operator <=(constref L, R: TWeightItem): Boolean; inline;
      constructor Create(constref w: TWeight; aIndex: SizeInt);
    end;

    TRankItem = record
      Rank,
      Weight: TWeight;
      Index: SizeInt;
      class operator = (constref L, R: TRankItem): Boolean; inline;
      class operator <>(constref L, R: TRankItem): Boolean; inline;
      class operator > (constref L, R: TRankItem): Boolean; inline;
      class operator < (constref L, R: TRankItem): Boolean; inline;
      class operator >=(constref L, R: TRankItem): Boolean; inline;
      class operator <=(constref L, R: TRankItem): Boolean; inline;
      constructor Create(constref aRank, aWeight: TWeight; aIndex: SizeInt);
    end;

    TGraph       = specialize TGCustomGraph<TVertex, TEdgeData, TEqRel>;
    TEstimate    = function(constref aSrc, aDst: TVertex): TWeight;
    TPairingHeap = specialize TGPairHeap<TWeightItem>;
    TBinHeap     = specialize TGBinHeapMin<TWeightItem>;
    TAStarHeap   = specialize TGPairHeap<TRankItem>;
    //TAStarHeap   = specialize TGBinHeapMin<TRankItem>;
    TEdgeArray   = array of TWeightEdge;

  { Dijkstra's algorithm: single-source shortest paths problem for non-negative weights  }
    class function  DijkstraSssp(g: TGraph; aSrc: SizeInt): TWeightArray;
    class function  DijkstraSssp(g: TGraph; aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray; static;
  { Dijkstra's pathfinding algorithm }
    class function  DijkstraPath(g: TGraph; aSrc, aDst: SizeInt): TWeight; static;
    class function  DijkstraPath(g: TGraph; aSrc, aDst: SizeInt; out aWeight: TWeight): TIntArray; static;
  { A* pathfinding algorithm }
    class function  AStar(g: TGraph; aSrc, aDst: SizeInt; out aWeight: TWeight; aEst: TEstimate): TIntArray; static;
  { Bellman-Ford algorithm: single-source shortest paths problem for any weights  }
    class function  FordBellman(g: TGraph; aSrc: SizeInt; out aWeights: TWeightArray): Boolean; static;
    class function  FordBellman(g: TGraph; aSrc: SizeInt; out aPaths: TIntArray; out aWeights: TWeightArray): Boolean;
                    static;
  { fills array with InfiniteWeight }
    class function  CreateWeightArray(aLen: SizeInt): TWeightArray; static; inline;
  { fills array with InfiniteWeight NegInfiniteWeight }
    class function  CreateWeightArrayNI(aLen: SizeInt): TWeightArray; static; inline;
  { fills array with ZeroWeight }
    class function  CreateWeightArrayZ(aLen: SizeInt): TWeightArray; static; inline;

    class property InfiniteWeight: TWeight read CFInfiniteWeight;
    class property NegInfiniteWeight: TWeight read CFNegInfiniteWeight;
    class property ZeroWeight: TWeight read CFZeroWeight;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGAdjItem }

constructor TGAdjItem.Create(aDst: SizeInt; constref aData: T);
begin
  Destination := aDst;
  Data := aData;
end;

{ TGAdjList.TEnumerator }

function TGAdjList.TEnumerator.GetCurrent: PAdjItem;
begin
  Result := pCurr;
end;

procedure TGAdjList.TEnumerator.SetCurrent(aValue: PAdjItem);
begin
  pCurr := aValue;
end;

function TGAdjList.TEnumerator.MoveNext: Boolean;
begin
  Result := pCurr < pLast;
  pCurr += Ord(Result);
end;

{ TGAdjList }

function TGAdjList.GetCapacity: SizeInt;
begin
  Result := System.Length(FList);
end;

procedure TGAdjList.Expand;
begin
  System.SetLength(FList, Capacity + ADJLIST_EXPAND_SIZE);
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
  aList.FCount := 0;
end;

class operator TGAdjList.Copy(constref aSrc: TGAdjList; var aDst: TGAdjList);
begin
  aDst.FList := System.Copy(aSrc.FList);
  aDst.FCount := aSrc.Count;
end;

function TGAdjList.GetEnumerator: TEnumerator;
begin
  Result.pCurr := PAdjItem(Pointer(FList)) - Ord(Count > 0);
  Result.pLast := PAdjItem(Pointer(FList)) + Pred(Count) and (-SizeInt(Count > 0));
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

function TGAdjList.ContainsAll(constref aList: TGAdjList): Boolean;
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

function TGAdjList.FindFirst(out aValue: SizeInt): Boolean;
begin
  Result := Count <> 0;
  if Result then
    aValue := FList[0].Destination;
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

{ TIntEdge }

constructor TIntEdge.Create(s, d: SizeInt);
begin
  Source := s;
  Destination := d;
end;

{ TGCustomGraph.TBitVector }

function TGCustomGraph.TBitVector.GetBit(aIndex: SizeInt): Boolean;
begin
  Result := (FBits[aIndex shr INT_SIZE_LOG] and (SizeUInt(1) shl (aIndex and INT_SIZE_MASK))) <> 0;
end;

function TGCustomGraph.TBitVector.GetSize: SizeInt;
begin
  Result := System.Length(FBits) shl INT_SIZE_LOG;
end;

procedure TGCustomGraph.TBitVector.SetBit(aIndex: SizeInt; aValue: Boolean);
begin
  if aValue then
    FBits[aIndex shr INT_SIZE_LOG] :=
    FBits[aIndex shr INT_SIZE_LOG] or (SizeUInt(1) shl (aIndex and INT_SIZE_MASK))
  else
    FBits[aIndex shr INT_SIZE_LOG] :=
    FBits[aIndex shr INT_SIZE_LOG] and not (SizeUInt(1) shl (aIndex and INT_SIZE_MASK));
end;

procedure TGCustomGraph.TBitVector.SetSize(aValue: SizeInt);
var
  OldLen: SizeInt;
begin
  OldLen := Size;
  if aValue > OldLen then
    begin
      aValue := aValue shr INT_SIZE_LOG + Ord(aValue and INT_SIZE_MASK <> 0);
      System.SetLength(FBits, aValue);
      System.FillChar(FBits[OldLen], (aValue - OldLen) * SizeOf(SizeUInt), 0);
    end;
end;

procedure TGCustomGraph.TBitVector.ClearBits;
begin
  if FBits <> nil then
    System.FillChar(FBits[0], System.Length(FBits) * SizeOf(SizeUInt), 0);
end;

procedure TGCustomGraph.TBitVector.ExpandTrue(aSize: SizeInt);
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

{ TGCustomGraph.TSquareBitMatrix }

function TGCustomGraph.TSquareBitMatrix.GetBit(I, J: SizeInt): Boolean;
begin
  Result := (FBits[(SizeUInt(I) * FSize + SizeUInt(J)) shr INT_SIZE_LOG] and
            (SizeUInt(1) shl ((SizeUInt(I) * FSize + SizeUInt(J)) and INT_SIZE_MASK))) <> 0
end;

function TGCustomGraph.TSquareBitMatrix.GetSize: SizeInt;
begin
  Result := FSize;
end;

procedure TGCustomGraph.TSquareBitMatrix.SetBit(I, J: SizeInt; aValue: Boolean);
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

class operator TGCustomGraph.TSquareBitMatrix.Initialize(var aMatrix: TSquareBitMatrix);
begin
  aMatrix.Clear;
end;

class function TGCustomGraph.TSquareBitMatrix.MaxSize: SizeInt;
begin
  Result := Trunc(Sqrt(High(SizeUInt)));
end;

constructor TGCustomGraph.TSquareBitMatrix.Create(aSize: SizeInt);
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
      raise EGraphError.CreateFmt(SEBitMatrixSizeExceedFmt, [aSize]);
end;

procedure TGCustomGraph.TSquareBitMatrix.ClearBits;
begin
  System.FillChar(FBits[0], System.Length(FBits) * SizeOf(SizeUInt), 0);
end;

procedure TGCustomGraph.TSquareBitMatrix.Clear;
begin
  FBits := nil;
  FSize := 0;
end;

{ TGCustomGraph.TBits256.TEnumerator }

function TGCustomGraph.TBits256.TEnumerator.GetCurrent: SizeInt;
begin
  Result := FLimbIndex shl INT_SIZE_LOG + FBitIndex;
end;

function TGCustomGraph.TBits256.TEnumerator.FindFirst: Boolean;
var
  I: SizeInt;
begin
  I := FValue^.Bsf;
  if I >= 0 then
    begin
      FLimbIndex := I shr INT_SIZE_LOG;
      FBitIndex := I and INT_SIZE_MASK;
      FCurrLimb := FValue^.FBits[FLimbIndex];
      TBits256.ClearBit(FBitIndex, FCurrLimb);
      Result := True;
    end
  else
    begin
      FLimbIndex := LIMB_COUNT;
      FBitIndex := BIT_PER_LIMB;
      Result := False;
    end;
end;

function TGCustomGraph.TBits256.TEnumerator.MoveNext: Boolean;
begin
  if FInCycle then
    repeat
      FBitIndex := TBits256.BsfValue(FCurrLimb);
      Result := FBitIndex >= 0;
      if Result then
        TBits256.ClearBit(FBitIndex, FCurrLimb)
      else
        begin
          if FLimbIndex >= Pred(LIMB_COUNT) then
            exit(False);
          Inc(FLimbIndex);
          FCurrLimb := FValue^.FBits[FLimbIndex];
        end;
    until Result
  else
    begin
      Result := FindFirst;
      FInCycle := True;
    end;
end;

{ TGCustomGraph.TBits256.TReverseEnumerator }

function TGCustomGraph.TBits256.TReverseEnumerator.GetCurrent: SizeInt;
begin
  Result := FLimbIndex shl INT_SIZE_LOG + FBitIndex;
end;

function TGCustomGraph.TBits256.TReverseEnumerator.FindFirst: Boolean;
var
  I: SizeInt;
begin
  I := FValue^.Bsr;
  if I >= 0 then
    begin
      FLimbIndex := I shr INT_SIZE_LOG;
      FBitIndex := I and INT_SIZE_MASK;
      FCurrLimb := FValue^.FBits[FLimbIndex];
      TBits256.ClearBit(FBitIndex, FCurrLimb);
      Result := True;
    end
  else
    begin
      FLimbIndex := -1;
      FBitIndex := BIT_PER_LIMB;
      Result := False;
    end;
end;

function TGCustomGraph.TBits256.TReverseEnumerator.MoveNext: Boolean;
begin
  if FInCycle then
    repeat
      FBitIndex := TBits256.BsrValue(FCurrLimb);
      Result := FBitIndex >= 0;
      if Result then
        TBits256.ClearBit(FBitIndex, FCurrLimb)
      else
        begin
          if FLimbIndex <= 0 then
            exit(False);
          Dec(FLimbIndex);
          FCurrLimb := FValue^.FBits[FLimbIndex];
        end;
    until Result
  else
    begin
      Result := FindFirst;
      FInCycle := True;
    end;
end;

{ TGCustomGraph.TBits256.TReverse }

function TGCustomGraph.TBits256.TReverse.GetEnumerator: TReverseEnumerator;
begin
  Result.FValue := FValue;
  Result.FInCycle := False;
end;

{ TGCustomGraph.TBits256 }

function TGCustomGraph.TBits256.GetBit(aIndex: SizeInt): Boolean;
begin
  Result := (FBits[aIndex shr INT_SIZE_LOG] and (SizeUInt(1) shl (aIndex and INT_SIZE_MASK))) <> 0;
end;

procedure TGCustomGraph.TBits256.SetBit(aIndex: SizeInt; aValue: Boolean);
begin
  if aValue then
    FBits[aIndex shr INT_SIZE_LOG] :=
      FBits[aIndex shr INT_SIZE_LOG] or (SizeUInt(1) shl (aIndex and INT_SIZE_MASK))
  else
    FBits[aIndex shr INT_SIZE_LOG] :=
      FBits[aIndex shr INT_SIZE_LOG] and not (SizeUInt(1) shl (aIndex and INT_SIZE_MASK));
end;

class function TGCustomGraph.TBits256.BsfValue(aValue: SizeUInt): SizeInt;
begin
{$IF DEFINED(CPU64)}
  Result := ShortInt(BsfQWord(aValue));
{$ELSEIF DEFINED(CPU32)}
  Result := ShortInt(BsfDWord(aValue));
{$ELSE}
  Result := ShortInt(BsfWord(aValue));
{$ENDIF}
end;

class function TGCustomGraph.TBits256.BsrValue(aValue: SizeUInt): SizeInt;
begin
{$IF DEFINED(CPU64)}
  Result := ShortInt(BsrQWord(aValue));
{$ELSEIF DEFINED(CPU32)}
  Result := ShortInt(BsrDWord(aValue));
{$ELSE}
  Result := ShortInt(BsrWord(aValue));
{$ENDIF}
end;

class procedure TGCustomGraph.TBits256.ClearBit(aIndex: SizeInt; var aValue: SizeUInt);
begin
  aValue := aValue and not (SizeUInt(1) shl aIndex);
end;

function TGCustomGraph.TBits256.GetEnumerator: TEnumerator;
begin
  Result.FValue := @Self;
  Result.FInCycle := False;
end;

function TGCustomGraph.TBits256.Reverse: TReverse;
begin
  Result.FValue := @Self;
end;

procedure TGCustomGraph.TBits256.InitRange(aRange: SizeInt);
var
  msb: SizeInt;
begin
  InitZero;
  if aRange > 0 then
    begin
      msb := aRange and INT_SIZE_MASK;
      aRange := aRange shr INT_SIZE_LOG  + Ord(msb <> 0);
      System.FillChar(FBits[0], aRange * SizeOf(SizeUInt), $ff);
      if msb <> 0 then
        FBits[Pred(aRange)] := FBits[Pred(aRange)] shr (BitsizeOf(SizeUint) - msb);
    end;
end;

procedure TGCustomGraph.TBits256.InitZero;
begin
{$IF DEFINED(CPU64)}
  System.FillQWord(FBits[0], LIMB_COUNT, 0);
{$ELSEIF DEFINED(CPU32)}
  System.FillDWord(FBits[0], LIMB_COUNT, 0);
{$ELSE}
  System.FillWord(FBits[0], LIMB_COUNT, 0);
{$ENDIF}
end;

function TGCustomGraph.TBits256.ToArray: TIntArray;
var
  I, Pos: SizeInt;
begin
  System.SetLength(Result, PopCount);
  Pos := 0;
  for I in Self do
    begin
      Result[Pos] := I;
      Inc(Pos);
    end;
end;

function TGCustomGraph.TBits256.IsEmpty: Boolean;
var
  I: SizeUInt;
begin
  for I in FBits do
    if I <> 0 then
      exit(False);
  Result := True;
end;

function TGCustomGraph.TBits256.NonEmpty: Boolean;
begin
  Result := not IsEmpty;
end;

function TGCustomGraph.TBits256.Bsf: SizeInt;
var
  I: SizeInt;
begin
  for I := 0 to Pred(LIMB_COUNT) do
    if FBits[I] <> 0 then
      exit(I shl INT_SIZE_LOG + BsfValue(FBits[I]));
  Result := -1;
end;

function TGCustomGraph.TBits256.Bsr: SizeInt;
var
  I: SizeInt;
begin
  for I := Pred(LIMB_COUNT) downto 0 do
    if FBits[I] <> 0 then
      exit(I shl INT_SIZE_LOG + BsrValue(FBits[I]));
  Result := -1;
end;

function TGCustomGraph.TBits256.Intersecting(constref aValue: TBits256): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(LIMB_COUNT) do
    if FBits[I] and aValue.FBits[I] <> 0 then
      exit(True);
  Result := False;
end;

function TGCustomGraph.TBits256.IntersectionPop(constref aValue: TBits256): SizeInt;
{$IF DEFINED(CPU64)}
begin
  Result := SizeInt(PopCnt(FBits[0] and aValue.FBits[0])) +
            SizeInt(PopCnt(FBits[1] and aValue.FBits[1])) +
            SizeInt(PopCnt(FBits[2] and aValue.FBits[2])) +
            SizeInt(PopCnt(FBits[3] and aValue.FBits[3]));
{$ELSEIF DEFINED(CPU32)}
begin
  Result := SizeInt(PopCnt(FBits[0] and aValue.FBits[0])) +
            SizeInt(PopCnt(FBits[1] and aValue.FBits[1])) +
            SizeInt(PopCnt(FBits[2] and aValue.FBits[2])) +
            SizeInt(PopCnt(FBits[3] and aValue.FBits[3])) +
            SizeInt(PopCnt(FBits[4] and aValue.FBits[4])) +
            SizeInt(PopCnt(FBits[5] and aValue.FBits[5])) +
            SizeInt(PopCnt(FBits[6] and aValue.FBits[6])) +
            SizeInt(PopCnt(FBits[7] and aValue.FBits[7]));
{$ELSE }
var
  I: SizeInt;
begin
  Result := 0;
  for I := 0 to Pred(LIMB_COUNT) do
    Result += SizeInt(PopCnt(FBits[I] and aValue.FBits[I]));
{$ENDIF }
end;

function TGCustomGraph.TBits256.Contains(constref aValue: TBits256): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(LIMB_COUNT) do
    if FBits[I] or aValue.FBits[I] <> FBits[I] then
      exit(False);
  Result := True;
end;

function TGCustomGraph.TBits256.JoinGain(constref aValue: TBits256): SizeInt;
{$IF DEFINED(CPU64)}
begin
  Result := SizeInt(PopCnt(not FBits[0] and aValue.FBits[0])) +
            SizeInt(PopCnt(not FBits[1] and aValue.FBits[1])) +
            SizeInt(PopCnt(not FBits[2] and aValue.FBits[2])) +
            SizeInt(PopCnt(not FBits[3] and aValue.FBits[3]));
{$ELSEIF DEFINED(CPU32)}
begin
  Result := SizeInt(PopCnt(not FBits[0] and aValue.FBits[0])) +
            SizeInt(PopCnt(not FBits[1] and aValue.FBits[1])) +
            SizeInt(PopCnt(not FBits[2] and aValue.FBits[2])) +
            SizeInt(PopCnt(not FBits[3] and aValue.FBits[3])) +
            SizeInt(PopCnt(not FBits[4] and aValue.FBits[4])) +
            SizeInt(PopCnt(not FBits[5] and aValue.FBits[5])) +
            SizeInt(PopCnt(not FBits[6] and aValue.FBits[6])) +
            SizeInt(PopCnt(not FBits[7] and aValue.FBits[7]));
{$ELSE }
var
  I: SizeInt;
begin
  Result := 0;
  for I := 0 to Pred(LIMB_COUNT) do
    Result += SizeInt(PopCnt(not FBits[I] and aValue.FBits[I]));
{$ENDIF }
end;

procedure TGCustomGraph.TBits256.Join(constref aValue: TBits256);
{$IF DEFINED(CPU64)}
begin
  FBits[0] := FBits[0] or aValue.FBits[0];
  FBits[1] := FBits[1] or aValue.FBits[1];
  FBits[2] := FBits[2] or aValue.FBits[2];
  FBits[3] := FBits[3] or aValue.FBits[3];
{$ELSEIF DEFINED(CPU32)}
begin
  FBits[0] := FBits[0] or aValue.FBits[0];
  FBits[1] := FBits[1] or aValue.FBits[1];
  FBits[2] := FBits[2] or aValue.FBits[2];
  FBits[3] := FBits[3] or aValue.FBits[3];
  FBits[4] := FBits[4] or aValue.FBits[4];
  FBits[5] := FBits[5] or aValue.FBits[5];
  FBits[6] := FBits[6] or aValue.FBits[6];
  FBits[7] := FBits[7] or aValue.FBits[7];
{$ELSE }
var
  I: SizeInt;
begin
  for I := 0 to Pred(LIMB_COUNT) do
    FBits[I] := FBits[I] or aValue.FBits[I];
{$ENDIF }
end;

function TGCustomGraph.TBits256.Union(constref aValue: TBits256): TBits256;
begin
  Result := Self;
  Result.Join(aValue);
end;

procedure TGCustomGraph.TBits256.Subtract(constref aValue: TBits256);
{$IF DEFINED(CPU64)}
begin
  FBits[0] := FBits[0] and not aValue.FBits[0];
  FBits[1] := FBits[1] and not aValue.FBits[1];
  FBits[2] := FBits[2] and not aValue.FBits[2];
  FBits[3] := FBits[3] and not aValue.FBits[3];
{$ELSEIF DEFINED(CPU32)}
begin
  FBits[0] := FBits[0] and not aValue.FBits[0];
  FBits[1] := FBits[1] and not aValue.FBits[1];
  FBits[2] := FBits[2] and not aValue.FBits[2];
  FBits[3] := FBits[3] and not aValue.FBits[3];
  FBits[4] := FBits[4] and not aValue.FBits[4];
  FBits[5] := FBits[5] and not aValue.FBits[5];
  FBits[6] := FBits[6] and not aValue.FBits[6];
  FBits[7] := FBits[7] and not aValue.FBits[7];
{$ELSE }
var
  I: SizeInt;
begin
  for I := 0 to Pred(LIMB_COUNT) do
    FBits[I] := FBits[I] and not aValue.FBits[I];
{$ENDIF }
end;

function TGCustomGraph.TBits256.Difference(constref aValue: TBits256): TBits256;
begin
  Result := Self;
  Result.Subtract(aValue);
end;

procedure TGCustomGraph.TBits256.Intersect(constref aValue: TBits256);
{$IF DEFINED(CPU64)}
begin
  FBits[0] := FBits[0] and aValue.FBits[0];
  FBits[1] := FBits[1] and aValue.FBits[1];
  FBits[2] := FBits[2] and aValue.FBits[2];
  FBits[3] := FBits[3] and aValue.FBits[3];
{$ELSEIF DEFINED(CPU32)}
begin
  FBits[0] := FBits[0] and aValue.FBits[0];
  FBits[1] := FBits[1] and aValue.FBits[1];
  FBits[2] := FBits[2] and aValue.FBits[2];
  FBits[3] := FBits[3] and aValue.FBits[3];
  FBits[4] := FBits[4] and aValue.FBits[4];
  FBits[5] := FBits[5] and aValue.FBits[5];
  FBits[6] := FBits[6] and aValue.FBits[6];
  FBits[7] := FBits[7] and aValue.FBits[7];
{$ELSE }
var
  I: SizeInt;
begin
  for I := 0 to Pred(LIMB_COUNT) do
    FBits[I] := FBits[I] and aValue.FBits[I];
{$ENDIF }
end;

function TGCustomGraph.TBits256.Intersection(constref aValue: TBits256): TBits256;
begin
  Result := Self;
  Result.Intersect(aValue);
end;

function TGCustomGraph.TBits256.PopCount: SizeInt;
{$IF DEFINED(CPU64)}
begin
  Result := SizeInt(PopCnt(FBits[0])) + SizeInt(PopCnt(FBits[1])) +
            SizeInt(PopCnt(FBits[2])) + SizeInt(PopCnt(FBits[3]));
{$ELSEIF DEFINED(CPU32)}
begin
  Result := SizeInt(PopCnt(FBits[0])) + SizeInt(PopCnt(FBits[1])) +
            SizeInt(PopCnt(FBits[2])) + SizeInt(PopCnt(FBits[3])) +
            SizeInt(PopCnt(FBits[4])) + SizeInt(PopCnt(FBits[5])) +
            SizeInt(PopCnt(FBits[6])) + SizeInt(PopCnt(FBits[7]));
{$ELSE }
var
  I: SizeUInt;
begin
  Result := 0;
  for I in FBits do
    Result += SizeInt(PopCnt(I));
{$ENDIF }
end;

{ TGCustomGraph.TAdjacencyMatrix }

function TGCustomGraph.TAdjacencyMatrix.GetSize: SizeInt;
begin
  Result := FMatrix.FSize;
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
  if SizeUInt(aSrc) < SizeUInt(FMatrix.FSize) then
      if SizeUInt(aDst) < SizeUInt(FMatrix.FSize) then
        Result := FMatrix{%H-}[aSrc, aDst]
      else
        raise EGraphError.CreateFmt(SEIndexOutOfBoundsFmt, [aDst])
  else
    raise EGraphError.CreateFmt(SEIndexOutOfBoundsFmt, [aSrc])
end;

{ TGCustomGraph.TIntSet.TEnumerator }

function TGCustomGraph.TIntSet.TEnumerator.GetCurrent: SizeInt;
begin
  Result := pCurr^;
end;

function TGCustomGraph.TIntSet.TEnumerator.MoveNext: Boolean;
begin
  Result := pCurr < pLast;
  pCurr += Ord(Result);
end;

{ TIntSet }

procedure TGCustomGraph.TIntSet.Expand;
begin
  System.SetLength(FItems, System.Length(FItems) + ADJLIST_EXPAND_SIZE);
end;

class operator TGCustomGraph.TIntSet.Initialize(var aList: TIntSet);
begin
  aList.FCount := 0;
end;

procedure TGCustomGraph.TIntSet.InitRange(aRange: SizeInt);
var
  I: SizeInt;
begin
  System.SetLength(FItems, aRange);
  if aRange > 0 then
    begin
      for I := 0 to Pred(aRange) do
        FItems[I] := I;
      FCount := aRange;
    end
  else
    FCount := 0;
end;

function TGCustomGraph.TIntSet.GetEnumerator: TEnumerator;
begin
  Result.pCurr := PSizeInt(Pointer(FItems)) - Ord(Count > 0);
  Result.pLast := PSizeInt(Pointer(FItems)) + (Pred(Count) and (-SizeInt(Count > 0)));
end;

function TGCustomGraph.TIntSet.ToArray: TIntArray;
begin
  Result := System.Copy(FItems, 0, Count);
end;

procedure TGCustomGraph.TIntSet.Assign(constref aValue: TIntSet);
begin
  FCount := aValue.Count;
  if Count <> 0 then
    FItems := System.Copy(aValue.FItems, 0, Count);
end;

procedure TGCustomGraph.TIntSet.AssignArray(constref a: TIntArray);
begin
  FItems := System.Copy(a);
  FCount := System.Length(FItems);
end;

procedure TGCustomGraph.TIntSet.AssignList(aList: PAdjList);
var
  p: PAdjItem;
  I: SizeInt = 0;
begin
  FCount := aList^.Count;
  System.SetLength(FItems, Count);
  for p in aList^ do
    begin
      FItems[I] := p^.Destination;
      Inc(I);
    end;
end;

function TGCustomGraph.TIntSet.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TGCustomGraph.TIntSet.NonEmpty: Boolean;
begin
  Result := Count <> 0;
end;

procedure TGCustomGraph.TIntSet.MakeEmpty;
begin
  FCount := 0;
end;

function TGCustomGraph.TIntSet.FindFirst(out aValue: SizeInt): Boolean;
begin
  Result := Count <> 0;
  if Result then
    aValue := FItems[Pred(Count)]
end;

function TGCustomGraph.TIntSet.GetItem(aIndex: SizeInt): SizeInt;
begin
  Result := FItems[aIndex];
end;

function TGCustomGraph.TIntSet.Contains(aValue: SizeInt): Boolean;
begin
  Result := Find(aValue) >= 0;
end;

function TGCustomGraph.TIntSet.ContainsAny(constref aValue: TIntSet): Boolean;
var
  I: SizeInt;
begin
  if NonEmpty then
    for I in aValue do
      if Contains(I) then
        exit(True);
  Result := False;
end;

function TGCustomGraph.TIntSet.ContainsAll(constref aValue: TIntSet): Boolean;
var
  I: SizeInt;
begin
  if Count >= aValue.Count then
    begin
      for I in aValue do
        if not Contains(I) then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

function TGCustomGraph.TIntSet.Find(aValue: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  for I := 0 to Pred(Count) do
    if FItems[I] = aValue then
      exit(I);
  Result := -1;
end;

function TGCustomGraph.TIntSet.Add(aValue: SizeInt): Boolean;
begin
  Result := Find(aValue) < 0;
  if Result then
    begin
      if Count = System.Length(FItems) then
        Expand;
      FItems[Count] := aValue;
      Inc(FCount);
    end;
end;

function TGCustomGraph.TIntSet.Add(constref a: TIntArray): SizeInt;
var
  I: SizeInt;
begin
  Result := Count;
  for I in a do
    Add(I);
  Result := Count - Result;
end;

function TGCustomGraph.TIntSet.Join(constref aValue: TIntSet): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I in aValue do
    Result += Ord(Add(I));
end;

procedure TGCustomGraph.TIntSet.Push(aValue: SizeInt);
begin
  if Count = System.Length(FItems) then
    Expand;
  FItems[Count] := aValue;
  Inc(FCount);
end;

function TGCustomGraph.TIntSet.Pop: SizeInt;
begin
  if Count <> 0 then
    begin
      Dec(FCount);
      Result := FItems[Count];
    end
  else
    begin
      EGraphError.Create(SECantAccessEmpty);
      Result := -1;
    end;
end;

function TGCustomGraph.TIntSet.TryPop(out aValue: SizeInt): Boolean;
begin
  Result := Count <> 0;
  if Result then
    begin
      Dec(FCount);
      aValue := FItems[Count];
    end;
end;

function TGCustomGraph.TIntSet.Last: SizeInt;
begin
  if Count <> 0 then
    Result := FItems[Count]
  else
    begin
      EGraphError.Create(SECantAccessEmpty);
      Result := -1;
    end;
end;

procedure TGCustomGraph.TIntSet.Subtract(constref aValue: TIntSet);
var
  I, Pos: SizeInt;
begin
  if aValue.NonEmpty then
    begin
      Pos := 0;
      for I := 0 to Pred(Count) do
        if aValue.Contains(FItems[I]) then
          Dec(FCount)
        else
          begin
            FItems[Pos] := FItems[I];
            Inc(Pos);
          end;
    end;
end;

procedure TGCustomGraph.TIntSet.Subtract(PList: PAdjList);
var
  I, Pos: SizeInt;
begin
  if PList^.NonEmpty then
    begin
      Pos := 0;
      for I := 0 to Pred(Count) do
        if PList^.Contains(FItems[I]) then
          Dec(FCount)
        else
          begin
            FItems[Pos] := FItems[I];
            Inc(Pos);
          end;
    end;
end;

function TGCustomGraph.TIntSet.Difference(constref aValue: TIntSet): TIntSet;
begin
  Result.Assign(Self);
  Result.Subtract(aValue);
end;

procedure TGCustomGraph.TIntSet.Intersect(constref aValue: TIntSet);
var
  I, Pos: SizeInt;
begin
  if aValue.NonEmpty then
    begin
      Pos := 0;
      for I := 0 to Pred(Count) do
        if aValue.Contains(FItems[I]) then
          begin
            FItems[Pos] := FItems[I];
            Inc(Pos);
          end
        else
          Dec(FCount);
    end
  else
    MakeEmpty;
end;

function TGCustomGraph.TIntSet.Intersection(constref aValue: TIntSet): TIntSet;
begin
  Result.Assign(Self);
  Result.Intersect(aValue);
end;

function TGCustomGraph.TIntSet.IntersectionCount(constref aValue: TIntSet): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I in aValue do
    Result += Ord(Contains(I));
end;

function TGCustomGraph.TIntSet.IntersectionCount(PList: PAdjList): SizeInt;
var
  p: PAdjItem;
begin
  Result := 0;
  for p in PList^ do
    Result += Ord(Contains(p^.Destination));
end;

function TGCustomGraph.TIntSet.Remove(aValue: SizeInt): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(Count) do
    if FItems[I] = aValue then
      begin
        Dec(FCount);
        FItems[I] := FItems[Count];
        exit(True);
      end;
  Result := False;
end;

procedure TGCustomGraph.TIntSet.Delete(aValue: SizeInt);
var
  I: SizeInt;
begin
  for I := 0 to Pred(Count) do
    if FItems[I] = aValue then
      begin
        Dec(FCount);
        if I < Count then
          System.Move(FItems[Succ(I)], FItems[I], (Count - I) * SizeOf(SizeInt));
        exit;
      end;
end;

procedure TGCustomGraph.TIntSet.Reverse;
begin
  if Count <> 0 then
    TIntHelper.Reverse(FItems[0..Pred(Count)]);
end;

{ TGCustomGraph.TSkeleton }

function TGCustomGraph.TSkeleton.GetVertexCount: SizeInt;
begin
  Result := System.Length(FAdjLists);
end;

function TGCustomGraph.TSkeleton.GetAdjList(aIndex: SizeInt): PIntSet;
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

constructor TGCustomGraph.TEdge.Create(aSrc, aDst: SizeInt; constref d: TEdgeData);
begin
  Source := aSrc;
  Destination := aDst;
  Data := d;
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
  Result.FEnum := FGraph.AdjLists[FSource]^.GetEnumerator;
end;

{ TGCustomGraph.TIncidentEnumerator }

function TGCustomGraph.TIncidentEnumerator.GetCurrent: TIncidentEdge;
var
  p: PAdjItem;
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
  Result.FEnum := FGraph.AdjLists[FSource]^.GetEnumerator;
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
  Result.FList := Pointer(FGraph.FNodeList);
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

function TGCustomGraph.GetAdjList(aIndex: SizeInt): PAdjList;
begin
  Result := @FNodeList[aIndex].AdjList;
end;

procedure TGCustomGraph.SetItem(aIndex: SizeInt; const aValue: TVertex);
var
  I: SizeInt;
begin
  CheckIndexRange(aIndex);
  if TEqRel.Equal(aValue, FNodeList[aIndex].Vertex) then
    exit;
  RemoveFromChain(aIndex);
  //add to new chain
  FNodeList[aIndex].Hash := TEqRel.HashCode(aValue);
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
    raise EGraphError.CreateFmt(SECapacityExceedFmt, [Capacity shl 1]);
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
  if {%H-}NonEmpty then
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
  h := TEqRel.HashCode(v);
  Result := FChainList[h and System.High(FChainList)];
  while Result <> NULL_INDEX do
    begin
      if (FNodeList[Result].Hash = h) and TEqRel.Equal(FNodeList[Result].Vertex, v) then
        exit;
      Result := FNodeList[Result].Next;
    end;
end;

function TGCustomGraph.Find(constref v: TVertex; aHash: SizeInt): SizeInt;
begin
  Result := FChainList[aHash and System.High(FChainList)];
  while Result <> NULL_INDEX do
    begin
      if (FNodeList[Result].Hash = aHash) and TEqRel.Equal(FNodeList[Result].Vertex, v) then
        exit;
      Result := FNodeList[Result].Next;
    end;
end;

function TGCustomGraph.FindOrAdd(constref v: TVertex; out aIndex: SizeInt): Boolean;
var
  h: SizeInt;
begin
  h := TEqRel.HashCode(v);
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

procedure TGCustomGraph.CheckIndexRange(aIndex: SizeInt);
begin
  if SizeUInt(aIndex) >= SizeUInt(VertexCount) then
    raise EGraphError.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
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

function TGCustomGraph.CreateBoolMatrix: TBoolMatrix;
var
  I, J: SizeInt;
begin
  System.SetLength(Result, VertexCount);
  for I := 0 to System.High(Result) do
    begin
      Result[I].Size := VertexCount;
      for J in AdjVerticesI(I) do
        Result[I][J] := True;
    end;
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

function TGCustomGraph.CreateIntArray(aLen, aValue: SizeInt): TIntArray;
begin
  System.SetLength(Result, aLen);
  if aLen > 0 then
{$IF DEFINED(CPU64)}
  System.FillQWord(Result[0], aLen, QWord(aValue));
{$ELSEIF DEFINED(CPU32)}
  System.FillDWord(Result[0], aLen, DWord(aValue));
{$ELSE}
  System.FillWord(Result[0], aLen, Word(aValue));
{$ENDIF}
end;

function TGCustomGraph.CreateIntArrayRange: TIntArray;
var
  I: SizeInt;
begin
  System.SetLength(Result, VertexCount);
  for I := 0 to Pred(VertexCount) do
    Result[I] := I;
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

function TGCustomGraph.CreateAdjEnumArray: TAdjEnumArray;
var
  I: SizeInt;
begin
  System.SetLength(Result, VertexCount);
  for I := 0 to Pred(VertexCount) do
    Result[I].FEnum := AdjLists[I]^.GetEnumerator;
end;

class function TGCustomGraph.MaxBitMatrixSize: SizeInt;
begin
  Result := TSquareBitMatrix.MaxSize;
end;

class function TGCustomGraph.TreePathTo(constref aTree: TIntArray; aValue: SizeInt): TIntArray;
var
  v: TIntVector;
begin
  while aValue >= 0 do
    begin
      if aValue < System.Length(aTree) then
        v.Add(aValue)
      else
        raise EGraphError.CreateFmt(SEIndexOutOfBoundsFmt,[aValue]);
      aValue := aTree[aValue];
    end;
  Result := v.ToArray;
  TIntHelper.Reverse(Result);
end;

class function TGCustomGraph.TreePathFromTo(constref aTree: TIntArray; aFrom, aTo: SizeInt): TIntArray;
var
  I: SizeInt;
  v: TIntVector;
begin
  I := aTo;
  while I >= 0 do
    begin
      if I < System.Length(aTree) then
        v.Add(I)
      else
        raise EGraphError.CreateFmt(SEIndexOutOfBoundsFmt,[I]);
      if I = aFrom then
        break;
      I := aTree[I];
    end;
  v.Add(aTo);
  Result := v.ToArray;
  TIntHelper.Reverse(Result);
end;

class function TGCustomGraph.TreePathLen(constref aTree: TIntArray; aFrom, aTo: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  I := aTo;
  while I >= 0 do
    begin
      if I < System.Length(aTree) then
        Inc(Result)
      else
        raise EGraphError.CreateFmt(SEIndexOutOfBoundsFmt,[I]);
      if I = aFrom then
        break;
      I := aTree[I];
    end;
end;

constructor TGCustomGraph.Create;
begin
  FDescription := TStringList.Create;
  Title := 'Untitled';
end;

constructor TGCustomGraph.Create(aCapacity: SizeInt);
begin
  Create;
  EnsureCapacity(aCapacity);
end;

destructor TGCustomGraph.Destroy;
begin
  FDescription.Free;
  inherited;
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
  FDescription.Clear;
end;

procedure TGCustomGraph.EnsureCapacity(aValue: SizeInt);
begin
  if aValue > Capacity then
    if aValue < MAX_CONTAINER_SIZE div SizeOf(TNode) then
      Resize(LGUtils.RoundUpTwoPower(aValue))
    else
      raise EGraphError.CreateFmt(SECapacityExceedFmt, [aValue]);
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
    Result := AdjLists[aSrc]^.Contains(aDst)
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
    Result := AdjLists[aSrc]^.Contains(aDst)
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
  p := AdjLists[aSrc]^.Find(aDst);
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
  p := AdjLists[aSrc]^.Find(aDst);
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
      if (s <> d) and AdjLists[s]^.Contains(d) then
        m[s, d] := True;
  Result := TAdjacencyMatrix.Create(m);
end;

function TGCustomGraph.IsBipartite: Boolean;
var
  Colors: TColorArray;
begin
  Result := IsBipartite(Colors);
end;

function TGCustomGraph.IsBipartite(out aColors: TColorArray): Boolean;
var
  Queue: TIntQueue;
  Curr, Next, I: SizeInt;
  CurrColor: TVertexColor;
begin
  if VertexCount < 2 then
    exit(False);
  aColors := CreateColorArray;
  for I := 0 to System.High(aColors) do
    if aColors[I] = vcNone then
      begin
        Curr := I;
        aColors[I] := vcWhite;
        repeat
          CurrColor := aColors[Curr];
          for Next in AdjVerticesI(Curr) do
            if aColors[Next] = vcNone then
              begin
                aColors[Next] := vcBlack - CurrColor;
                Queue.Enqueue(Next);
              end
            else
              if aColors[Next] = CurrColor then
                begin
                  aColors := nil;
                  exit(False);
                end;
        until not Queue{%H-}.TryDequeue(Curr);
      end;
  Result := True;
end;

function TGCustomGraph.IsBipartite(out aWhites, aGrays: TIntArray): Boolean;
var
  Colors: TColorArray;
  WhiteIdx, GrayIdx, I: SizeInt;
  CurrColor: TVertexColor;
begin
  Result := IsBipartite(Colors);
  if not Result then
    exit;
  aWhites := CreateIntArray;
  aGrays := CreateIntArray;
  WhiteIdx := 0;
  GrayIdx := 0;
  I := 0;
  for CurrColor in Colors do
    begin
      if CurrColor = vcWhite then
        begin
          aWhites[WhiteIdx] := I;
          Inc(WhiteIdx);
        end
      else
        begin
          aGrays[GrayIdx] := I;
          Inc(GrayIdx);
        end;
      Inc(I);
    end;
  System.SetLength(aWhites, WhiteIdx);
  System.SetLength(aGrays, GrayIdx);
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
  Queue: TIntQueue;
  Visited: TBitVector;
begin
  Result := 0;
  CheckIndexRange(aRoot);
  Visited.Size := VertexCount;
  if Assigned(OnFound) then
    OnFound(aRoot);
  Visited[aRoot] := True;
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
  until not Queue{%H-}.TryDequeue(aSrc);
  Result := -1;
end;

function TGCustomGraph.ShortestPathsMap(constref aSrc: TVertex): TIntArray;
begin
  Result := ShortestPathsMapI(IndexOf(aSrc));
end;

function TGCustomGraph.ShortestPathsMapI(aSrc: SizeInt): TIntArray;
var
  Queue: TIntQueue;
  Next: SizeInt;
begin
  CheckIndexRange(aSrc);
  Result := CreateIntArray;
  Result[aSrc] := 0;
  repeat
    for Next in AdjVerticesI(aSrc) do
      if Result[Next] = -1 then
        begin
          Queue.Enqueue(Next);
          Result[Next] := Succ(Result[aSrc]);
        end;
  until not Queue{%H-}.TryDequeue(aSrc);
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
      exit(TreePathTo(Parents, aDst));
    for Next in AdjVerticesI(aSrc) do
      if not Visited[Next] then
        begin
          Visited[Next] := True;
          Queue.Enqueue(Next);
          Parents[Next] := aSrc;
        end;
  until not Queue{%H-}.TryDequeue(aSrc);
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

function TDisjointSetUnion.Tag(aValue: SizeInt): SizeInt;
begin
  if FList[aValue] = aValue then
    exit(aValue);
  Result := Tag(FList[aValue]);
  FList[aValue] := Result;
end;

function TDisjointSetUnion.InSameSet(L, R: SizeInt): Boolean;
begin
  Result := Tag(L) = Tag(R);
end;

function TDisjointSetUnion.InDiffSets(L, R: SizeInt): Boolean;
begin
  Result := Tag(L) <> Tag(R);
end;

function TDisjointSetUnion.Join(L, R: SizeInt): Boolean;
begin
  L := Tag(L);
  R := Tag(R);
  if L = R then
    exit(False);
  if NextRandomBoolean then
    FList[L] := R
  else
    FList[R] := L;
  Result := True;
end;

{ TIntHashSet.TEnumerator }

function TIntHashSet.TEnumerator.GetCurrent: SizeInt;
begin
  Result := FEnum.Current^.Key;
end;

function TIntHashSet.TEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TIntHashSet.TEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TIntHashSet }

function TIntHashSet.GetCount: SizeInt;
begin
  Result := FTable.Count;
end;

function TIntHashSet.GetEnumerator: TEnumerator;
begin
  Result.FEnum := FTable.GetEnumerator
end;

function TIntHashSet.ToArray: TIntArray;
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

function TIntHashSet.IsEmpty: Boolean;
begin
  Result := FTable.Count = 0;
end;

function TIntHashSet.NonEmpty: Boolean;
begin
  Result := FTable.Count <> 0;
end;

procedure TIntHashSet.MakeEmpty;
begin
  FTable.MakeEmpty;
end;

procedure TIntHashSet.Clear;
begin
  FTable.Clear;
end;

procedure TIntHashSet.EnsureCapacity(aValue: SizeInt);
begin
  FTable.EnsureCapacity(aValue);
end;

function TIntHashSet.Contains(aValue: SizeInt): Boolean;
begin
  Result := FTable.Contains(aValue);
end;

function TIntHashSet.Add(aValue: SizeInt): Boolean;
var
  p: PEntry;
begin
  Result := not FTable.FindOrAdd(aValue, p);
  if Result then
    p^.Key := aValue;
end;

function TIntHashSet.AddAll(constref a: array of SizeInt): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I in a do
    Result += Ord(Add(I));
end;

function TIntHashSet.AddAll(constref s: TIntHashSet): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I in s do
    Result += Ord(Add(I));
end;

function TIntHashSet.Remove(aValue: SizeInt): Boolean;
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

procedure TIntPairSet.Clear;
begin
  FTable.Clear;
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

{ TGPairHeap.TNode }

function TGPairHeap.TNode.AddChild(aNode: PNode): PNode;
begin
  Result := @Self;
  aNode^.Prev := Result;
  Sibling :=  aNode^.Sibling;
  if Sibling <> nil then
    Sibling^.Prev := @Self;
  aNode^.Sibling := Child;
  if Child <> nil then
    Child^.Prev := aNode;
  Child := aNode;
end;

{ TGPairHeap }

function TGPairHeap.GetCapacity: SizeInt;
begin
  Result := System.Length(FNodeList);
end;

function TGPairHeap.NewNode(constref aValue: T; aHandle: SizeInt): PNode;
begin
  Result := @FNodeList[aHandle];
  Inc(FCount);
  Result^.Data := aValue;
  Result^.Prev := nil;
  Result^.Child := nil;
  Result^.Sibling := nil;
end;

function TGPairHeap.DequeueItem: T;
begin
  Result := FRoot^.Data;
  Dec(FCount);
  FRoot := TwoPassMerge(FRoot^.Child);
  if FRoot <> nil then
    FRoot^.Prev := nil;
end;

procedure TGPairHeap.RootMerge(aNode: PNode);
begin
  FRoot := NodeMerge(FRoot, aNode);
  if FRoot <> nil then
    FRoot^.Prev := nil;
end;

class function TGPairHeap.NodeMerge(L, R: PNode): PNode;
begin
  if L <> nil then
    if R <> nil then
      if L^.Data <= R^.Data then
        Result := L^.AddChild(R)
      else
        Result := R^.AddChild(L)
    else
      Result := L
  else
    Result := R;
end;

class function TGPairHeap.TwoPassMerge(aNode: PNode): PNode;
var
  CurrNode, NextNode: PNode;
begin
  Result := nil;
  while (aNode <> nil) and (aNode^.Sibling <> nil) do
    begin
      NextNode := aNode^.Sibling;
      CurrNode := aNode;
      aNode := NextNode^.Sibling;
      NextNode^.Sibling := nil;
      CurrNode^.Sibling := nil;
      Result := NodeMerge(Result, NodeMerge(CurrNode, NextNode));
    end;
  Result := NodeMerge(Result, aNode);
end;

class procedure TGPairHeap.CutNode(aNode: PNode);
begin
  if aNode^.Sibling <> nil then
    aNode^.Sibling^.Prev := aNode^.Prev;
  if aNode^.Prev^.Child = aNode then
    aNode^.Prev^.Child := aNode^.Sibling
  else
    aNode^.Prev^.Sibling := aNode^.Sibling;
  aNode^.Sibling := nil;
end;

constructor TGPairHeap.Create(aSize: SizeInt);
var
  I: SizeInt;
begin
  if aSize > 0 then
    begin
      System.SetLength(FNodeList, aSize);
      for I := 0 to Pred(aSize) do
        FNodeList[I].Prev := PNode(SizeUInt(-1));
    end;
  FRoot := nil;
  FCount := 0;
end;

function TGPairHeap.NotUsed(aHandle: SizeInt): Boolean;
begin
  Result := FNodeList[aHandle].Prev = PNode(SizeUInt(-1));
end;

function TGPairHeap.TryDequeue(out aValue: T): Boolean;
begin
  Result := Count <> 0;
  if Result then
    aValue := DequeueItem;
end;

procedure TGPairHeap.Enqueue(constref aValue: T; aHandle: SizeInt);
begin
  RootMerge(NewNode(aValue, aHandle));
end;

procedure TGPairHeap.Update(aHandle: SizeInt; constref aNewValue: T);
var
  Node: PNode;
begin
  Node := @FNodeList[aHandle];
  if aNewValue < Node^.Data then
    begin
      Node^.Data := aNewValue;
      if Node <> FRoot then
        begin
          CutNode(Node);
          RootMerge(Node);
        end;
    end;
end;

function TGPairHeap.Peek(aHandle: SizeInt): T;
begin
  Result := FNodeList[aHandle].Data;
end;

{ TGWeightedPathHelper.TWeightEdge }

class operator TGWeightedPathHelper.TWeightEdge. = (constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight = R.Weight;
end;

class operator TGWeightedPathHelper.TWeightEdge.<>(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight <> R.Weight;
end;

class operator TGWeightedPathHelper.TWeightEdge.>(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight > R.Weight;
end;

class operator TGWeightedPathHelper.TWeightEdge.<(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight < R.Weight;
end;

class operator TGWeightedPathHelper.TWeightEdge.>=(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight >= R.Weight;
end;

class operator TGWeightedPathHelper.TWeightEdge.<=(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight <= R.Weight;
end;

constructor TGWeightedPathHelper.TWeightEdge.Create(s, d: SizeInt; w: TWeight);
begin
  Source := s;
  Destination := d;
  Weight := w;
end;

{ TGWeightedPathHelper.TWeightItem }

class operator TGWeightedPathHelper.TWeightItem. = (constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight = R.Weight;
end;

class operator TGWeightedPathHelper.TWeightItem.<>(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight <> R.Weight;
end;

class operator TGWeightedPathHelper.TWeightItem.>(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight > R.Weight;
end;

class operator TGWeightedPathHelper.TWeightItem.<(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight < R.Weight;
end;

class operator TGWeightedPathHelper.TWeightItem.>=(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight >= R.Weight;
end;

class operator TGWeightedPathHelper.TWeightItem.<=(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight <= R.Weight;
end;

constructor TGWeightedPathHelper.TWeightItem.Create(constref w: TWeight; aIndex: SizeInt);
begin
  Weight := w;
  Index := aIndex;
end;

{ TGWeightedPathHelper.TRankItem }

class operator TGWeightedPathHelper.TRankItem. = (constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank = R.Rank;
end;

class operator TGWeightedPathHelper.TRankItem.<>(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank <> R.Rank;
end;

class operator TGWeightedPathHelper.TRankItem.>(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank > R.Rank;
end;

class operator TGWeightedPathHelper.TRankItem.<(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank < R.Rank;
end;

class operator TGWeightedPathHelper.TRankItem.>=(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank >= R.Rank;
end;

class operator TGWeightedPathHelper.TRankItem.<=(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank <= R.Rank;
end;

constructor TGWeightedPathHelper.TRankItem.Create(constref aRank, aWeight: TWeight; aIndex: SizeInt);
begin
  Rank := aRank;
  Weight := aWeight;
  Index := aIndex;
end;

{ TGWeightedPathHelper }

class constructor TGWeightedPathHelper.Init;
begin
  CFInfiniteWeight := TWeight.MaxValue;
  CFNegInfiniteWeight := TWeight.MinValue;
  CFZeroWeight := Default(TWeight);
end;

class function TGWeightedPathHelper.CreateAndFill(constref aValue: TWeight; aSize: SizeInt): TWeightArray;
var
  I: SizeInt;
begin
  System.SetLength(Result, aSize);
  for I := 0 to Pred(aSize) do
    Result[I] := aValue;
end;

class function TGWeightedPathHelper.DijkstraSssp(g: TGraph; aSrc: SizeInt): TWeightArray;
var
  Visited: TGraph.TBitVector;
  Queue: TPairingHeap;
  Relaxed: TWeight;
  Item: TWeightItem;
  p: TGraph.PAdjItem;
begin
  Result := CreateWeightArray(g.VertexCount);
  Queue := TPairingHeap.Create(g.VertexCount);
  Visited.Size := g.VertexCount;
  Queue.Enqueue(TWeightItem.Create(ZeroWeight, aSrc), aSrc);
  while Queue.TryDequeue(Item) do
    begin
      Visited[{%H-}Item.Index] := True;
      Result[Item.Index] := Item.Weight;
      for p in g.AdjLists[Item.Index]^ do
        if Queue.NotUsed(p^.Key) then
          Queue.Enqueue(TWeightItem.Create(p^.Data.Weight + Item.Weight, p^.Key), p^.Key)
        else
          if not Visited[p^.Key] then
            begin
              Relaxed := p^.Data.Weight + Item.Weight;
              if Relaxed < Queue.Peek(p^.Key).Weight then
                Queue.Update(p^.Key, TWeightItem.Create(Relaxed, p^.Key));
            end;
    end;
end;

class function TGWeightedPathHelper.DijkstraSssp(g: TGraph; aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
var
  Visited: TGraph.TBitVector;
  Queue: TPairingHeap;
  Relaxed: TWeight;
  Item: TWeightItem;
  p: TGraph.PAdjItem;
begin
  Result := CreateWeightArray(g.VertexCount);
  Queue := TPairingHeap.Create(g.VertexCount);
  aPathTree := g.CreateIntArray;
  Visited.Size := g.VertexCount;
  Queue.Enqueue(TWeightItem.Create(ZeroWeight, aSrc), aSrc);
  while Queue.TryDequeue(Item) do
    begin
      Visited[{%H-}Item.Index] := True;
      Result[Item.Index] := Item.Weight;
      for p in g.AdjLists[Item.Index]^ do
        if Queue.NotUsed(p^.Key) then
          begin
            Queue.Enqueue(TWeightItem.Create(p^.Data.Weight + Item.Weight, p^.Key), p^.Key);
            aPathTree[p^.Key] := Item.Index;
          end
        else
          if not Visited[p^.Key] then
            begin
              Relaxed := p^.Data.Weight + Item.Weight;
              if Relaxed < Queue.Peek(p^.Key).Weight then
                begin
                  Queue.Update(p^.Key, TWeightItem.Create(Relaxed, p^.Key));
                  aPathTree[p^.Key] := Item.Index;
                end;
            end;
    end;
end;

class function TGWeightedPathHelper.DijkstraPath(g: TGraph; aSrc, aDst: SizeInt): TWeight;
var
  Visited: TGraph.TBitVector;
  Queue: TBinHeap;
  Relaxed: TWeight;
  Item: TWeightItem;
  p: TGraph.PAdjItem;
begin
  Queue := TBinHeap.Create(g.VertexCount);
  Visited.Size := g.VertexCount;
  Queue.Enqueue(TWeightItem.Create(ZeroWeight, aSrc), aSrc);
  while Queue.TryDequeue(Item) do
    begin
      if Item.Index = aDst then
        exit(Item.Weight);
      Visited[Item.Index] := True;
      for p in g.AdjLists[Item.Index]^ do
        if Queue.NotUsed(p^.Key) then
          Queue.Enqueue(TWeightItem.Create(p^.Data.Weight + Item.Weight, p^.Key), p^.Key)
        else
          if not Visited[p^.Key] then
            begin
              Relaxed := p^.Data.Weight + Item.Weight;
              if Relaxed < Queue.Peek(p^.Key).Weight then
                Queue.Update(p^.Key, TWeightItem.Create(Relaxed, p^.Key));
            end
    end;
  Result := InfiniteWeight;
end;

class function TGWeightedPathHelper.DijkstraPath(g: TGraph; aSrc, aDst: SizeInt; out aWeight: TWeight): TIntArray;
var
  Visited: TGraph.TBitVector;
  Queue: TBinHeap;
  Tree: TIntArray;
  Relaxed: TWeight;
  Item: TWeightItem;
  p: TGraph.PAdjItem;
{%H-}begin
  Queue := TBinHeap.Create(g.VertexCount);
  Tree := g.CreateIntArray;
  Visited.Size := g.VertexCount;
  Queue.Enqueue(TWeightItem.Create(ZeroWeight, aSrc), aSrc);
  while {%H-}Queue.TryDequeue(Item) do
    begin
      if Item.Index = aDst then
        begin
          aWeight := Item.Weight;
          exit(g.TreePathTo(Tree, aDst));
        end;
      Visited[Item.Index] := True;
      for p in g.AdjLists[Item.Index]^ do
        begin
          if Queue.NotUsed(p^.Key) then
            begin
              Queue.Enqueue(TWeightItem.Create(p^.Data.Weight + Item.Weight, p^.Key), p^.Key);
              Tree[p^.Key] := Item.Index;
            end
          else
            if not Visited[p^.Key] then
              begin
                Relaxed := p^.Data.Weight + Item.Weight;
                if Relaxed < Queue.Peek(p^.Key).Weight then
                  begin
                    Queue.Update(p^.Key, TWeightItem.Create(Relaxed, p^.Key));
                    Tree[p^.Key] := Item.Index;
                  end;
              end;
        end;
    end;
  aWeight := InfiniteWeight;
end;

class function TGWeightedPathHelper.AStar(g: TGraph; aSrc, aDst: SizeInt; out aWeight: TWeight;
  aEst: TEstimate): TIntArray;
var
  Visited: TGraph.TBitVector;
  Queue: TAStarHeap;
  Tree: TIntArray;
  Relaxed: TWeight;
  Item: TRankItem;
  p: TGraph.PAdjItem;
{%H-}begin
  Queue := TAStarHeap.Create(g.VertexCount);
  Tree := g.CreateIntArray;
  Visited.Size := g.VertexCount;
  Queue.Enqueue(TRankItem.Create(aEst(g.Items[aSrc], g.Items[aDst]), ZeroWeight, aSrc), aSrc);
  while Queue.TryDequeue(Item) do
    begin
      if {%H-}Item.Index = aDst then
        begin
          aWeight := Item.Weight;
          exit(g.TreePathTo(Tree, aDst));
        end;
      Visited[Item.Index] := True;
      for p in g.AdjLists[Item.Index]^ do
        begin
          if Queue.NotUsed(p^.Key) then
            begin
              Relaxed := p^.Data.Weight + Item.Weight;
              Queue.Enqueue(TRankItem.Create(
                Relaxed + aEst(g.Items[p^.Key], g.Items[aDst]), Relaxed, p^.Key), p^.Key);
              Tree[p^.Key] := Item.Index;
            end
          else
            if not Visited[p^.Key] then
              begin
                Relaxed := Item.Weight + p^.Data.Weight;
                if Relaxed < Queue.Peek(p^.Key).Weight then
                  begin
                    Queue.Update(p^.Key, TRankItem.Create(
                      Relaxed + aEst(g.Items[p^.Key], g.Items[aDst]), Relaxed, p^.Key));
                    Tree[p^.Key] := Item.Index;
                  end;
              end;
        end;
    end;
  aWeight := InfiniteWeight;
end;

class function TGWeightedPathHelper.FordBellman(g: TGraph; aSrc: SizeInt; out aWeights: TWeightArray): Boolean;
var
  Edge: TGraph.TEdge;
  Enum: TGraph.TEdgeEnumerator;
  RelaxValue: TWeight;
  I: SizeInt;
  Relaxed: Boolean = False;
begin
  aWeights := CreateWeightArray(g.VertexCount);
  Enum := g.Edges.GetEnumerator;
  aWeights[aSrc] := ZeroWeight;
  for I := 1 to g.VertexCount do
    begin
      Relaxed := False;
      while Enum.MoveNext do
        begin
          Edge := Enum.Current;
          if aWeights[Edge.Source] < InfiniteWeight then
            begin
              RelaxValue := aWeights[Edge.Source] + Edge.Data.Weight;
              if RelaxValue < aWeights[Edge.Destination] then
                begin
                  aWeights[Edge.Destination] := RelaxValue;
                  Relaxed := True;
                end;
            end;
        end;
      if not Relaxed then
        break;
      Enum.Reset;
    end;
  Result := not Relaxed;
  if not Result then
    aWeights := nil;
end;

class function TGWeightedPathHelper.FordBellman(g: TGraph; aSrc: SizeInt; out aPaths: TIntArray; out
  aWeights: TWeightArray): Boolean;
var
  Edge: TGraph.TEdge;
  Enum: TGraph.TEdgeEnumerator;
  v: TIntVector;
  RelaxValue: TWeight;
  I: SizeInt;
  J: SizeInt = -1;
begin
  aWeights := CreateWeightArray(g.VertexCount);
  aPaths := g.CreateIntArray;
  Enum := g.Edges.GetEnumerator;
  aWeights[aSrc] := ZeroWeight;
  for I := 1 to g.VertexCount do
    begin
      J := -1;
      while Enum.MoveNext do
        begin
          Edge := Enum.Current;
          if aWeights[Edge.Source] < InfiniteWeight then
            begin
              RelaxValue := aWeights[Edge.Source] + Edge.Data.Weight;
              if RelaxValue < aWeights[Edge.Destination] then
                begin
                  aWeights[Edge.Destination] := RelaxValue;
                  aPaths[Edge.Destination] := Edge.Source;
                  J := Edge.Destination;
                end;
            end;
        end;
      if J = -1 then
        break;
      Enum.Reset;
    end;

  Result := J = -1;

  if not Result then
    begin
      for I := 1 to g.VertexCount do
        J := aPaths[J];
      I := J;
      v.Add(J);
      repeat
        I := aPaths[I];
        v.Add(I);
      until I = J;
      aPaths := v.ToArray;
      aWeights := nil;
    end;
end;

class function TGWeightedPathHelper.CreateWeightArray(aLen: SizeInt): TWeightArray;
begin
  Result := CreateAndFill(InfiniteWeight, aLen);
end;

class function TGWeightedPathHelper.CreateWeightArrayNI(aLen: SizeInt): TWeightArray;
begin
  Result := CreateAndFill(NegInfiniteWeight, aLen);
end;

class function TGWeightedPathHelper.CreateWeightArrayZ(aLen: SizeInt): TWeightArray;
begin
  Result := CreateAndFill(ZeroWeight, aLen);
end;

end.

