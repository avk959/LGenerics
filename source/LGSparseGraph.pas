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
unit LGSparseGraph;

{$mode objfpc}{$H+}
{$MODESWITCH TYPEHELPERS}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}
{$INLINE ON}{$WARN 6058 off : }

interface

uses
  Classes, SysUtils, DateUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGArrayHelpers,
  LGStack,
  LGQueue,
  LGDeque,
  LGVector,
  LGHashTable,
  LGHash,
  LGHashMap,
  LGStrConst;

type

  EGraphError      = class(Exception); //???
  TEmptyRec        = record end;

  TIntArray        = array of SizeInt;
  TShortArray      = array of ShortInt;
  TIntHelper       = specialize TGOrdinalArrayHelper<SizeInt>;
  TIntVector       = specialize TGLiteVector<SizeInt>;
  PIntVector       = ^TIntVector;
  TIntVectorHelper = specialize TGComparableVectorHelper<SizeInt>;
  TIntArrayVector  = specialize TGLiteVector<TIntArray>;
  PIntArrayVector  = ^TIntArrayVector;
  TIntVectorArray  = array of TIntVector;
  TIntStack        = specialize TGLiteStack<SizeInt>;
  TIntQueue        = specialize TGLiteQueue<SizeInt>;
  TIntDeque        = specialize TGLiteDeque<SizeInt>;

  TOnVisit         = procedure (aSender: TObject; aIndex: SizeInt) of object;
  TOnAccept        = function (aSender: TObject; aIndex: SizeInt): Boolean of object;
  TOnFindSet       = procedure(const aSet: TIntArray; var aCancel: Boolean) of object;
  TCost            = Int64;
  TVertexColor     = type Byte;

const
  MAX_COST = High(TCost);
  MIN_COST = Low(TCost);

  vcNone:  TVertexColor = 0;
  vcWhite: TVertexColor = 1;
  vcGray:  TVertexColor = 2;
  vcBlack: TVertexColor = 3;

type
  TColorArray = array of TVertexColor;

  TIntEdge = packed record
    Source,
    Destination: SizeInt;
    class function HashCode(constref aValue: TIntEdge): SizeInt; static; inline;
    class function Equal(constref L, R: TIntEdge): Boolean; static; inline;
    constructor Create(aSrc, aDst: SizeInt);
    function Key: TIntEdge; inline;
  end;

  TCostEdge = record
    Source,
    Destination: SizeInt;
    Cost: TCost;
    constructor Create(aSrc, aDst: SizeInt; aCost: TCost);
    function Edge: TIntEdge; inline;
  end;

  TEdgeCostMap          = specialize TGLiteHashMapLP<TIntEdge, TCost, TIntEdge>;
  TIntArrayVectorHelper = specialize TGDelegatedVectorHelper<TIntArray>;
  TIntEdgeVector        = specialize TGLiteVector<TIntEdge>;
  TIntEdgeArray         = array of TIntEdge;
  TEdgeArrayVector      = specialize TGLiteVector<TIntEdgeArray>;
  TCostEdgeArray        = array of TCostEdge;

  TGraphMagic           = string[8];

const
  GRAPH_MAGIC: TGraphMagic = 'LGrphTyp';
  GRAPH_HEADER_VERSION     = 2;
  GRAPH_ADJLIST_GROW       = 8;
  DENSE_CUTOFF             = 0.7;  //???
  JOHNSON_CUTOFF           = 0.2;  //???

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

  { TGSparseGraph: simple sparse graph abstract ancestor class based on adjacency lists;
      functor TEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGSparseGraph<TVertex, TEdgeData, TEqRel> = class abstract
  protected
    {$I SparseGraphBitHelpH.inc}
  public
  type
    TSpecEdgeData  = TEdgeData;
    TAdjItem       = specialize TGAdjItem<TEdgeData>;
    PAdjItem       = ^TAdjItem;
    TVertexArray   = array of TVertex;
    TOnAddEdge     = procedure(constref aSrc, aDst: TVertex; var aData: TEdgeData) of object;
    TOnReadVertex  = procedure(aStream: TStream; out aValue: TVertex) of object;
    TOnWriteVertex = procedure(aStream: TStream; constref aValue: TVertex) of object;
    TOnReadData    = procedure(aStream: TStream; out aValue: TEdgeData) of object;
    TOnWriteData   = procedure(aStream: TStream; constref aValue: TEdgeData) of object;

    TAdjacencyMatrix = record
    private
      FMatrix: TSquareBitMatrix;
      function GetSize: SizeInt; inline;
    public
      constructor Create(constref aMatrix: TSquareBitMatrix);
      function IsEmpty: Boolean; inline;
      function Adjacent(aSrc, aDst: SizeInt): Boolean; inline;
      property Size: SizeInt read GetSize;
    end;

  protected
  type
    PEdgeData          = ^TEdgeData;
    TAdjList           = specialize TGAdjList<TEdgeData>;
    PAdjList           = ^TAdjList;
    TAdjItemEnumerator = TAdjList.TEnumerator;
    TAdjEnumArrayEx    = array of TAdjItemEnumerator;

    {$I SparseGraphIntSetH.inc}

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
  public
  type
    TAdjEnumerator = record
    private
      FEnum: TAdjItemEnumerator;
      function  GetCurrent: SizeInt; inline;
    public
      function  MoveNext: Boolean; inline;
      property  Current: SizeInt read GetCurrent;
    end;

    TAdjEnumArray = array of TAdjEnumerator;
  protected
    function  GetEdgeDataPtr(aSrc, aDst: SizeInt): PEdgeData; inline;
    procedure CheckIndexRange(aIndex: SizeInt);
    function  CheckPathExists(aSrc, aDst: SizeInt): Boolean;
    function  CreateBoolMatrix: TBoolMatrix;
    function  CreateIntArray(aLength, aValue: SizeInt): TIntArray; inline;
    function  CreateIntArray(aValue: SizeInt = -1): TIntArray; inline;
    function  CreateIntArrayRange: TIntArray; inline;
    function  CreateColorArray: TColorArray;
    function  CreateAdjEnumArray: TAdjEnumArray;
    function  CreateAdjEnumArrayEx: TAdjEnumArrayEx;
    function  PathToNearestFrom(aSrc: SizeInt; constref aTargets: TIntArray): TIntArray;
    procedure AssignVertexList(aGraph: TGSparseGraph; const aList: TIntArray);
    procedure AssignTree(aGraph: TGSparseGraph; const aTree: TIntArray);
    procedure AssignEdges(aGraph: TGSparseGraph; const aEdges: TIntEdgeArray);
    function  DoAddVertex(constref aVertex: TVertex; out aIndex: SizeInt): Boolean; virtual; abstract;
    procedure DoRemoveVertex(aIndex: SizeInt); virtual; abstract;
    function  DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean; virtual; abstract;
    function  DoRemoveEdge(aSrc, aDst: SizeInt): Boolean; virtual; abstract;
    function  DoSetEdgeData(aSrc, aDst: SizeInt; constref aValue: TEdgeData): Boolean; virtual; abstract;
    procedure DoWriteEdges(aStream: TStream; aOnWriteData: TOnWriteData); virtual; abstract;
    property  AdjLists[aIndex: SizeInt]: PAdjList read GetAdjList;
  public
  type
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
      FGraph: TGSparseGraph;
      FSource: SizeInt;
    public
      constructor Create(aGraph: TGSparseGraph; aSource: SizeInt);
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
      FGraph: TGSparseGraph;
      FSource: SizeInt;
    public
      constructor Create(aGraph: TGSparseGraph; aSource: SizeInt);
      function GetEnumerator: TIncidentEnumerator; inline;
    end;

    TVertexEnumerator = record
    private
      FNodeList: PNode;
      FCurrIndex,
      FLastIndex: SizeInt;
      function  GetCurrent: TVertex;
    public
      constructor Create(aGraph: TGSparseGraph);
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: TVertex read GetCurrent;
    end;

    TVertices = record
    private
      FGraph: TGSparseGraph;
    public
      constructor Create(aGraph: TGSparseGraph);
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
      FGraph: TGSparseGraph;
    public
      function GetEnumerator: TEdgeEnumerator;
    end;

  public
{**********************************************************************************************************
  auxiliary utilities
***********************************************************************************************************}
    class function cMin(L, R: TCost): TCost; static; inline;
    class function cMax(L, R: TCost): TCost; static; inline;
    class function MaxBitMatrixSize: SizeInt; static; inline;
    class function TreePathTo(const aTree: TIntArray; aValue: SizeInt): TIntArray; static;
    class function TreePathFromTo(const aTree: TIntArray; aFrom, aTo: SizeInt): TIntArray; static;
    class function TreePathLen(const aTree: TIntArray; aFrom, aTo: SizeInt): SizeInt; static;
{**********************************************************************************************************
  class management utilities
***********************************************************************************************************}
    constructor Create;
    destructor Destroy; override;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear; virtual;
    procedure EnsureCapacity(aValue: SizeInt);
    procedure TrimToFit; inline;
  { saves graph in its own binary format }
    procedure SaveToStream(aStream: TStream; aOnWriteVertex: TOnWriteVertex; aOnWriteData: TOnWriteData);
    procedure LoadFromStream(aStream: TStream; aOnReadVertex: TOnReadVertex; aOnReadData: TOnReadData);
    procedure SaveToFile(const aFileName: string; aOnWriteVertex: TOnWriteVertex; aOnWriteData: TOnWriteData);
    procedure LoadFromFile(const aFileName: string; aOnReadVertex: TOnReadVertex; aOnReadData: TOnReadData);
{**********************************************************************************************************
  structural management utilities
***********************************************************************************************************}
  { returns True and vertex index, if it was added, False otherwise }
    function  AddVertex(constref aVertex: TVertex; out aIndex: SizeInt): Boolean; inline;
    function  AddVertex(constref aVertex: TVertex): Boolean; inline;
  { returns count of added vertices }
    function  AddVertices(const aVertices: TVertexArray): SizeInt;
  { removes vertex aVertex from graph; raises EGraphError if not contains aVertex }
    procedure RemoveVertex(constref aVertex: TVertex); inline;
    procedure RemoveVertexI(aIndex: SizeInt);
    function  ContainsVertex(constref aVertex: TVertex): Boolean; inline;
  { if does not contain aSrc or aDst vertices, they will be added;
    returns True if the edge is added, False, if such an edge already exists }
    function  AddEdge(constref aSrc, aDst: TVertex; aData: TEdgeData): Boolean;
  { adds edge with default data }
    function  AddEdge(constref aSrc, aDst: TVertex): Boolean; inline;
  { returns True if the edge is added, False, if such an edge already exists;
    raises EGraphError if aSrc or aDst out of range }
    function  AddEdgeI(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  AddEdgeI(aSrc, aDst: SizeInt): Boolean; inline;
  { if contains an edge (aSrc, aDst) then removes it and returns True,
    otherwise returns False }
    function  RemoveEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  RemoveEdgeI(aSrc, aDst: SizeInt): Boolean; inline;
  { if contains an edge (aSrc, aDst) then contracts it and returns True(vertex aSrc remains, aDst is removed),
    otherwise returns False }
    function  ContractEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  ContractEdgeI(aSrc, aDst: SizeInt): Boolean;
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
    function  IndexPath2VertexPath(const aIdxPath: TIntArray): TVertexArray;
    function  VertexPath2IndexPath(const aVertPath: TVertexArray): TIntArray;
  { returns adjacency matrix;
    warning: maximum matrix size limited, see MaxBitMatrixSize }
    function  CreateAdjacencyMatrix: TAdjacencyMatrix;
  { test whether the graph is bipartite;
    the graph can be disconnected (in this case it consists of a number of connected
    bipartite components and / or several isolated vertices)}
    function  IsBipartite: Boolean;
  { test whether the graph is bipartite; if returns True then information about the vertex
    belonging to the fractions is returned in aColors(vcWhite or vcGray) }
    function  IsBipartite(out aColors: TColorArray): Boolean;
  { test whether the graph is bipartite; if returns True then aWhites and aGrays will contain
    indices of correspondig vertices }
    function  IsBipartite(out aWhites, aGrays: TIntArray): Boolean;
{**********************************************************************************************************
  matching utilities
***********************************************************************************************************}

  { returns True if aMatch is maximal matching }
    function IsMaxMatching(const aMatch: TIntEdgeArray): Boolean;
  { returns True if aMatch is perfect matching }
    function IsPerfectMatching(const aMatch: TIntEdgeArray): Boolean;
{**********************************************************************************************************
  traversal utilities
***********************************************************************************************************}

  { returns count of visited vertices during the DFS traversal;
    aOnFound calls after next vertex found, aOnDone calls after vertex done;
    if aOnFound returns False then traversal stops }
    function DfsTraversal(constref aRoot: TVertex; aOnFound: TOnAccept = nil; aOnDone: TOnVisit = nil): SizeInt; inline;
    function DfsTraversalI(aRoot: SizeInt; aOnFound: TOnAccept = nil; aOnDone: TOnVisit = nil): SizeInt;
  { returns the DFS traversal tree(forest, if not connected) started from vertex with index 0;
    each element of Result contains the index of its parent in tree(or -1 if it is root) }
    function DfsTree: TIntArray;
  { returns count of visited vertices during the BFS traversal;
    aOnFound calls after vertex found; if aOnFound returns False then traversal stops}
    function BfsTraversal(constref aRoot: TVertex; aOnFound: TOnAccept = nil; aOnDone: TOnVisit = nil): SizeInt; inline;
    function BfsTraversalI(aRoot: SizeInt; aOnFound: TOnAccept = nil; aOnDone: TOnVisit = nil): SizeInt;
  { returns the BFS traversal tree(forest, if not connected) started from vertex with index 0;
    each element of Result contains the index of its parent (or -1 if it is root) }
    function BfsTree: TIntArray;

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
    function ShortestPathsMapI(aSrc: SizeInt): TIntArray;
    function ShortestPathsMap(constref aSrc: TVertex; out aPathTree: TIntArray): TIntArray; inline;
    function ShortestPathsMapI(aSrc: SizeInt; out aPathTree: TIntArray): TIntArray;
  { returns an array containing chain of vertex indices of found shortest path(in sense 'edges count'),
    empty if path does not exists }
    function ShortestPath(constref aSrc, aDst: TVertex): TIntArray; inline;
    function ShortestPathI(aSrc, aDst: SizeInt): TIntArray;
  { returns the eccentricity of the aVertex;
    returns High(SizeInt), if exists any vertex unreachable from aVertex }
    function Eccentricity(constref aVertex: TVertex): SizeInt; inline;
    function EccentricityI(aIndex: SizeInt): SizeInt;
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

  { TGAbstractDotWriter: abstract writer into Graphviz dot format }
  generic TGAbstractDotWriter<TVertex, TEdgeData, TEqRel> = class abstract
  public
  type
    TWriteDirection = (wdTopToBottom, wdLeftToWrite);
    TGraph          = specialize TGSparseGraph<TVertex, TEdgeData, TEqRel>;
    TOnStartWrite   = function(aGraph: TGraph): utf8string of object;
    TOnWriteVertex  = function(aGraph: TGraph; aIndex: SizeInt): utf8string of object;
    TOnWriteEdge    = function(aGraph: TGraph; constref aEdge: TGraph.TEdge): utf8string of object;

  protected
  const
    DIRECTS: array[TWriteDirection] of utf8string = ('rankdir=TB;', 'rankdir=LR;');
  var
    FGraphMark,
    FEdgeMark: utf8string;
    FDirection: TWriteDirection;
    FOnStartWrite: TOnStartWrite;
    FOnWriteVertex: TOnWriteVertex;
    FOnWriteEdge: TOnWriteEdge;
    function Graph2Dot(aGraph: TGraph): utf8string; virtual; abstract;
    function DefaultWriteEdge({%H-}aGraph: TGraph; constref aEdge: TGraph.TEdge): utf8string; virtual;
  public
    procedure SaveToStream(aGraph: TGraph; aStream: TStream);
    procedure SaveToFile(aGraph: TGraph; const aFileName: string);
    property Direction: TWriteDirection read FDirection write FDirection;
    property OnStartWrite: TOnStartWrite read FOnStartWrite write FOnStartWrite;
    property OnWriteVertex: TOnWriteVertex read FOnWriteVertex write FOnWriteVertex;
    property OnWriteEdge: TOnWriteEdge read FOnWriteEdge write FOnWriteEdge;
  end;

  TTspMatrixState = (tmsProper, tmsTrivial, tmsNonSquare, tmsNegElement);

  { TGTspHelper: some algorithms for TSP;
    warning: for signed integer types only }
  generic TGTspHelper<T> = class
  public
  type
    PItem      = ^T;
    TArray     = array of T;
    TTspMatrix = array of array of T;
  protected
  type
    TOnTourReady = procedure(const m: TTspMatrix; var aTour: TIntArray; var aCost: T);

    { TBbTsp: branch and bound TSP algorithm;
      Little, Murty, Sweeney, and Karel "An Algorithm for Traveling Salesman Problem";
      Syslo, Deo, Kowalik "Discrete Optimization Algorithms: With Pascal Programs";
      advanced matrix reduction:
      Костюк Ю.Л. "Эффективная реализация алгоритма решения задачи коммивояжёра методом ветвей и границ" }
    TBbTsp = object
    protected
    type
      TMinData = record
        Value: T;
        ZeroFlag: Boolean;
        procedure Clear; inline;
      end;

      PMinData    = ^TMinData;
      PInt        = PInteger;
      TMinArray   = array of TMinData;
      TArray      = array of T;
      TBoolMatrix = array of TBoolVector;

    const
      ADV_CUTOFF = 4;

    var
      FMatrix: TArray;
      FZeros: TBoolMatrix;
      FForwardTour,
      FBackTour,
      FBestTour: array of Integer;
      FRowMin,
      FColMin: TMinArray;
      FMatrixSize,
      FTimeOut: Integer;
      FUpBound: T;
      FStartTime: TDateTime;
      FIsMetric,
      FCancelled: Boolean;
      procedure Init(const m: TTspMatrix; const aTour: TIntArray; aTimeOut: Integer);
      function  TimeOut: Boolean; inline;
      function  Reduce(aSize: Integer; aCost: T; aRows, aCols: PInt; aRowRed, aColRed: PItem): T;
      function  ReduceA(aSize: Integer; aCost: T; aRows, aCols: PInt; aRowRed, aColRed: PItem): T;
      function  SelectNext(aSize: Integer; aRows, aCols: PInt; out aRowIdx, aColIdx: Integer): T;
      procedure Search(aSize: Integer; aCost: T; aRows, aCols: PInt);
      procedure CopyBest(var aTour: TIntArray; out aCost: T);
    public
      function  Execute(const m: TTspMatrix; aTimeOut: Integer; var aTour: TIntArray; out aCost: T): Boolean;
      property  IsMetric: Boolean read FIsMetric write FIsMetric;
    end;

    { TApproxBbTsp }
    TApproxBbTsp = object(TBbTsp)
    protected
      Factor: Double;
      function  Reduce(aSize: Integer; aCost: T; aRows, aCols: PInt; aRowRed, aColRed: PItem): T;
      function  ReduceA(aSize: Integer; aCost: T; aRows, aCols: PInt; aRowRed, aColRed: PItem): T;
      procedure Search(aSize: Integer; aCost: T; aRows, aCols: PInt);
    public
      function  Execute(const m: TTspMatrix; aEps: Double; aTimeOut: Integer; var aTour: TIntArray;
                out aCost: T): Boolean;
    end;

  { TLs3Opt: 3-opt local search algorithm for the traveling salesman problem;
    Syslo, Deo, Kowalik "Discrete Optimization Algorithms: With Pascal Programs"; }
    TLs3Opt = record
    strict private
    type
      TSwap  = record
        X1, X2, Y1, Y2, Z1, Z2: SizeInt;
        Gain: T;
        IsAsymm: Boolean;
      end;

    var
      Matrix: TTspMatrix;
      CurrTour: TIntArray;
      procedure PickSwapKind(var aSwap: TSwap);
      procedure Reverse(aFirst, aLast: SizeInt);
      procedure Execute(var aCost: T);
    public
      procedure OptPath(const m: TTspMatrix; var aTour: TIntArray; var aCost: T);
      procedure OptEdges(const m: TTspMatrix; var aTour: TIntArray; var aCost: T);
    end;

    class function  vMin(L, R: T): T; static; inline;
  { returns True if matrix m is symmetric;
    raises exception if m is not proper matrix }
    class function  CheckMatrixProper(const m: TTspMatrix): Boolean; static;
  { assumes aTour is closed path;
    cyclic shifts aTour so that element aSrc becomes the first;
    does not checks if aSrc exists in aTour }
    class procedure NormalizeTour(aSrc: SizeInt; var aTour: TIntArray); static;
  { 2-opt local search; assumes aTour is closed path;
    does not checks not matrix nor path }
    class procedure Ls2Opt(const m: TTspMatrix; var aTour: TIntArray; var aCost: T); static;
  { 3-opt local search; assumes aTour is closed path;
    does not checks not matrix nor path }
    class procedure Ls3OptPath(const m: TTspMatrix; var aTour: TIntArray; var aCost: T); static;
  { 3-opt local search; assumes aTour is edge set(index - source, value - target);
    does not checks not matrix nor aTour }
    class procedure Ls3OptEdges(const m: TTspMatrix; var aTour: TIntArray; var aCost: T); static;
  { best of farthest insertion starting from every vertex; does not checks matrix;
    Syslo, Deo, Kowalik "Discrete Optimization Algorithms: With Pascal Programs"  }
    class function GreedyFInsTsp(const m: TTspMatrix; aOnReady: TOnTourReady; out aCost: T): TIntArray; static;
  { best of nearest neighbour, starting from every vertex; does not checks matrix }
    class function GreedyNearNeighb(const m: TTspMatrix; aOnReady: TOnTourReady; out aCost: T): TIntArray; static;
  public
    class function GetMatrixState(const m: TTspMatrix; out aIsSymm: Boolean): TTspMatrixState; static;
  { returns total cost of TS tour specified by aTour;
    warning: does not checks not matrix not tour }
    class function GetTotalCost(const m: TTspMatrix; const aTour: TIntArray): T; static;
  { best of farthest insertion starting from every vertex;
    will raise EGraphError if m is not proper TSP matrix }
    class function FindGreedyFast(const m: TTspMatrix; out aCost: T): TIntArray; static;
  { best of nearest neighbour starting from every vertex;
    will raise EGraphError if m is not proper TSP matrix }
    class function FindGreedyFastNn(const m: TTspMatrix; out aCost: T): TIntArray; static;
  { best of nearest neighbour + 2-opt local search starting from every vertex +
    3-opt local search at the end; applicable only for symmetric matrices;
    returns best of nearest neighbour starting from every vertex, if matrix is asymmetric;
    will raise EGraphError if m is not proper TSP matrix }
    class function FindGreedy2Opt(const m: TTspMatrix; out aCost: T): TIntArray; static;
  { best of farthest insertion starting from every vertex + 3-opt local search at the end;
    applicable only for symmetric matrices;
    returns best of nearest neighbour starting from every vertex, if matrix is asymmetric;
    will raise EGraphError if m is not proper TSP matrix }
    class function FindGreedy3Opt(const m: TTspMatrix; out aCost: T): TIntArray; static;
  { best of farthest insertion + 3-opt local search, starting from every vertex;
    applicable only for symmetric matrices;
    returns best of nearest neighbour starting from every vertex, if matrix is asymmetric;
    will raise EGraphError if m is not proper TSP matrix }
    class function FindSlowGreedy3Opt(const m: TTspMatrix; out aCost: T): TIntArray; static;
  { exact branch and bound algorithm for TSP;
    aTimeOut specifies the timeout in seconds; at the end of the timeout,
    will be returned False and the best recent solution;
    will raise EGraphError if m is not proper TSP matrix }
    class function FindExact(const m: TTspMatrix; out aTour: TIntArray; out aCost: T;
                   aTimeOut: Integer = WAIT_INFINITE): Boolean; static;
  { suboptimal branch and bound algorithm for TSP;
    aTimeOut specifies the timeout in seconds; at the end of the timeout,
    will be returned False and the best recent solution, otherwise
    returns solution of a given guaranteed accuracy, specified with param Accuracy;
    will raise EGraphError if m is not proper TSP matrix }
    class function FindApprox(const m: TTspMatrix; Accuracy: Double; out aTour: TIntArray; out aCost: T;
                   aTimeOut: Integer = WAIT_INFINITE): Boolean; static;
  end;

  { TGMetricTspHelper: for signed integer types only }
  generic TGMetricTspHelper<T> = class(specialize TGTspHelper<T>)
    class function FindExact(const m: TTspMatrix; out aTour: TIntArray; out aCost: T;
                   aTimeOut: Integer = WAIT_INFINITE): Boolean; static;
    class function FindApprox(const m: TTspMatrix; Accuracy: Double; out aTour: TIntArray; out aCost: T;
                   aTimeOut: Integer = WAIT_INFINITE): Boolean; static;
  end;

  generic TGPoint2D<T> = record
    X, Y: T;
    constructor Create(aX, aY: T);
    class function Equal(constref L, R: TGPoint2D): Boolean; static; inline;
    class function HashCode(constref aPoint: TGPoint2D): SizeInt; static; inline;
    function Distance(constref aPoint: TGPoint2D): ValReal; inline;
  end;

  generic TGPoint3D<T> = record
    X, Y, Z: T;
    class function Equal(constref L, R: TGPoint3D): Boolean; static; inline;
    class function HashCode(constref aPoint: TGPoint3D): SizeInt; static; inline;
    constructor Create(aX, aY, aZ: T);
    function Distance(constref aPoint: TGPoint3D): ValReal; inline;
  end;

  {$I SparseGraphHelpH.inc}

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}

uses
  bufstream;

{ TIntEdge }

class function TIntEdge.HashCode(constref aValue: TIntEdge): SizeInt;
begin
{$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  {$IF DEFINED (CPU64)}
    Result := TxxHash32LE.HashGuid(TGuid(aValue));
  {$ELSEIF DEFINED (CPU32)}
    Result := TxxHash32LE.HashQWord(QWord(aValue));
  {$ELSE }
    Result := TxxHash32LE.HashDWord(DWord(aValue));
  {$ENDIF }
{$ElSE FPC_REQUIRES_PROPER_ALIGNMENT}
    Result := TxxHash32LE.HashBuf(@aValue, SizeOf(aValue));
{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
end;

class function TIntEdge.Equal(constref L, R: TIntEdge): Boolean;
begin
  Result := (L.Source = R.Source) and (L.Destination = R.Destination);
end;

constructor TIntEdge.Create(aSrc, aDst: SizeInt);
begin
  Source := aSrc;
  Destination := aDst;
end;

function TIntEdge.Key: TIntEdge;
begin
  Result := Self;
end;

{ TCostEdge }

constructor TCostEdge.Create(aSrc, aDst: SizeInt; aCost: TCost);
begin
  Source := aSrc;
  Destination := aDst;
  Cost := aCost;
end;

function TCostEdge.Edge: TIntEdge;
begin
  Result := TIntEdge.Create(Source, Destination);
end;

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
  System.SetLength(FList, Capacity + GRAPH_ADJLIST_GROW);
end;

function TGAdjList.DoFind(aValue: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  for I := 0 to Pred(Count) do
    if FList[I].Destination = aValue then
      exit(I);
  Result := NULL_INDEX;
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
    Pos := NULL_INDEX;
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
  if Count > 0 then
    Result := DoFind(aItem.Destination) = NULL_INDEX
  else
    Result := True;
  if Result then
    begin
      if Count >= Capacity then
        Expand;
      FList[Count] := aItem;
      Inc(FCount);
    end;
end;

function TGAdjList.Remove(aDst: SizeInt): Boolean;
var
  Pos: SizeInt;
begin
  if Count > 0 then
    begin
      Pos := DoFind(aDst);
      Result := Pos >= 0;
      if Result then
        DoRemove(Pos);
    end
  else
    Result := False;
end;

{$I SparseGraphBitHelp.inc}

{ TGSparseGraph.TAdjacencyMatrix }

function TGSparseGraph.TAdjacencyMatrix.GetSize: SizeInt;
begin
  Result := FMatrix.FSize;
end;

constructor TGSparseGraph.TAdjacencyMatrix.Create(constref aMatrix: TSquareBitMatrix);
begin
  FMatrix := aMatrix;
end;

function TGSparseGraph.TAdjacencyMatrix.IsEmpty: Boolean;
begin
  Result := FMatrix.Size = 0;
end;

function TGSparseGraph.TAdjacencyMatrix.Adjacent(aSrc, aDst: SizeInt): Boolean;
begin
  if SizeUInt(aSrc) < SizeUInt(FMatrix.FSize) then
      if SizeUInt(aDst) < SizeUInt(FMatrix.FSize) then
        Result := FMatrix{%H-}[aSrc, aDst]
      else
        raise EGraphError.CreateFmt(SEIndexOutOfBoundsFmt, [aDst])
  else
    raise EGraphError.CreateFmt(SEIndexOutOfBoundsFmt, [aSrc])
end;

{$I SparseGraphIntSet.inc}

{ TGSparseGraph.TNode }

procedure TGSparseGraph.TNode.Assign(constref aSrc: TNode);
begin
  Hash := aSrc.Hash;
  Next := aSrc.Next;
  AdjList := aSrc.AdjList;
  Vertex := aSrc.Vertex;
  Tag := aSrc.Tag;
end;

{ TGSparseGraph.TEdge }

constructor TGSparseGraph.TEdge.Create(aSrc: SizeInt; p: PAdjItem);
begin
  Source := aSrc;
  Destination := p^.Destination;
  Data := p^.Data;
end;

constructor TGSparseGraph.TEdge.Create(aSrc, aDst: SizeInt; constref d: TEdgeData);
begin
  Source := aSrc;
  Destination := aDst;
  Data := d;
end;

{ TGSparseGraph.TAdjEnumerator }

function TGSparseGraph.TAdjEnumerator.GetCurrent: SizeInt;
begin
  Result := FEnum.Current^.Destination;
end;

function TGSparseGraph.TAdjEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

{ TGSparseGraph.TAdjVertices }

constructor TGSparseGraph.TAdjVertices.Create(aGraph: TGSparseGraph; aSource: SizeInt);
begin
  FGraph := aGraph;
  FSource := aSource;
end;

function TGSparseGraph.TAdjVertices.GetEnumerator: TAdjEnumerator;
begin
  Result.FEnum := FGraph.AdjLists[FSource]^.GetEnumerator;
end;

{ TGSparseGraph.TIncidentEnumerator }

function TGSparseGraph.TIncidentEnumerator.GetCurrent: TIncidentEdge;
var
  p: PAdjItem;
begin
  p := FEnum.Current;
  Result.Destination := p^.Destination;
  Result.Data := p^.Data;
end;

function TGSparseGraph.TIncidentEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

{ TGSparseGraph.TIncidentEdges }

constructor TGSparseGraph.TIncidentEdges.Create(aGraph: TGSparseGraph; aSource: SizeInt);
begin
  FGraph := aGraph;
  FSource := aSource;
end;

function TGSparseGraph.TIncidentEdges.GetEnumerator: TIncidentEnumerator;
begin
  Result.FEnum := FGraph.AdjLists[FSource]^.GetEnumerator;
end;

{ TGSparseGraph.TVertexEnumerator }

function TGSparseGraph.TVertexEnumerator.GetCurrent: TVertex;
begin
  Result := FNodeList[FCurrIndex].Vertex;
end;

constructor TGSparseGraph.TVertexEnumerator.Create(aGraph: TGSparseGraph);
begin
  FNodeList := Pointer(aGraph.FNodeList);
  FLastIndex := Pred(aGraph.VertexCount);
  FCurrIndex := -1;
end;

function TGSparseGraph.TVertexEnumerator.MoveNext: Boolean;
begin
  if FCurrIndex >= FLastIndex then
    exit(False);
  Inc(FCurrIndex);
  Result := True;
end;

procedure TGSparseGraph.TVertexEnumerator.Reset;
begin
  FCurrIndex := -1;
end;

{ TGSparseGraph.TVertices }

constructor TGSparseGraph.TVertices.Create(aGraph: TGSparseGraph);
begin
  FGraph := aGraph;
end;

function TGSparseGraph.TVertices.GetEnumerator: TVertexEnumerator;
begin
  Result := TVertexEnumerator.Create(FGraph);
end;

{ TGSparseGraph.TEdgeEnumerator }

function TGSparseGraph.TEdgeEnumerator.GetCurrent: TEdge;
begin
  Result := TEdge.Create(FCurrIndex, FEnum.Current);
end;

function TGSparseGraph.TEdgeEnumerator.MoveNext: Boolean;
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

procedure TGSparseGraph.TEdgeEnumerator.Reset;
begin
  FCurrIndex := -1;
  FEnumDone := True;
end;

{ TGSparseGraph.TEdges }

function TGSparseGraph.TEdges.GetEnumerator: TEdgeEnumerator;
begin
  Result.FList := Pointer(FGraph.FNodeList);
  Result.FLastIndex := Pred(FGraph.VertexCount);
  Result.FCurrIndex := -1;
  Result.FEnumDone := True;
end;

{ TGSparseGraph }

function TGSparseGraph.GetCapacity: SizeInt;
begin
  Result := System.Length(FNodeList);
end;

function TGSparseGraph.GetItem(aIndex: SizeInt): TVertex;
begin
  CheckIndexRange(aIndex);
  Result := FNodeList[aIndex].Vertex;
end;

function TGSparseGraph.GetAdjList(aIndex: SizeInt): PAdjList;
begin
  Result := @FNodeList[aIndex].AdjList;
end;

procedure TGSparseGraph.SetItem(aIndex: SizeInt; const aValue: TVertex);
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

procedure TGSparseGraph.InitialAlloc;
begin
  System.SetLength(FNodeList, DEFAULT_CONTAINER_CAPACITY);
  System.SetLength(FChainList, DEFAULT_CONTAINER_CAPACITY);
  System.FillChar(FChainList[0], DEFAULT_CONTAINER_CAPACITY * SizeOf(SizeInt), $ff);
end;

procedure TGSparseGraph.Rehash;
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

procedure TGSparseGraph.Resize(aNewCapacity: SizeInt);
begin
  System.SetLength(FNodeList, aNewCapacity);
  System.SetLength(FChainList, aNewCapacity);
  Rehash;
end;

procedure TGSparseGraph.Expand;
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

function TGSparseGraph.Add(constref v: TVertex; aHash: SizeInt): SizeInt;
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

procedure TGSparseGraph.RemoveFromChain(aIndex: SizeInt);
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

procedure TGSparseGraph.Delete(aIndex: SizeInt);
begin
  Dec(FCount);
  Finalize(FNodeList[aIndex].AdjList);
  if aIndex < VertexCount then
    begin
      System.Move(FNodeList[Succ(aIndex)], FNodeList[aIndex], (VertexCount - aIndex) * SizeOf(TNode));
      System.FillChar(FNodeList[VertexCount], SizeOf(TNode), 0);
      Rehash;
    end
  else   // last element
    begin
      RemoveFromChain(aIndex);
      System.FillChar(FNodeList[VertexCount], SizeOf(TNode), 0);
    end;
end;

function TGSparseGraph.Remove(constref v: TVertex): Boolean;
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

function TGSparseGraph.Find(constref v: TVertex): SizeInt;
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

function TGSparseGraph.Find(constref v: TVertex; aHash: SizeInt): SizeInt;
begin
  Result := FChainList[aHash and System.High(FChainList)];
  while Result <> NULL_INDEX do
    begin
      if (FNodeList[Result].Hash = aHash) and TEqRel.Equal(FNodeList[Result].Vertex, v) then
        exit;
      Result := FNodeList[Result].Next;
    end;
end;

function TGSparseGraph.FindOrAdd(constref v: TVertex; out aIndex: SizeInt): Boolean;
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

function TGSparseGraph.GetEdgeDataPtr(aSrc, aDst: SizeInt): PEdgeData;
begin
  Result := @FNodeList[aSrc].AdjList.Find(aDst)^.Data;
end;

procedure TGSparseGraph.CheckIndexRange(aIndex: SizeInt);
begin
  if SizeUInt(aIndex) >= SizeUInt(VertexCount) then
    raise EGraphError.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
end;

function TGSparseGraph.CheckPathExists(aSrc, aDst: SizeInt): Boolean;
var
  Queue: TIntArray;
  Visited: TBitVector;
  p: PAdjItem;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  System.SetLength(Queue, VertexCount);
  Visited.Size := VertexCount;
  Queue[qTail] := aSrc;
  Inc(qTail);
  while qHead < qTail do
    begin
      aSrc := Queue[qHead];
      Inc(qHead);
      if aSrc = aDst then
        exit(True);
      for p in AdjLists[aSrc]^ do
        if not Visited[p^.Destination] then
          begin
            Queue[qTail] := p^.Destination;
            Inc(qTail);
            Visited[p^.Destination] := True;
          end;
    end;
  Result := False;
end;

function TGSparseGraph.CreateBoolMatrix: TBoolMatrix;
var
  I: SizeInt;
  p: PAdjItem;
begin
  System.SetLength(Result, VertexCount);
  for I := 0 to System.High(Result) do
    begin
      Result[I].Size := VertexCount;
      for p in AdjLists[I]^ do
        Result[I][p^.Key] := True;
    end;
end;

function TGSparseGraph.CreateIntArray(aLength, aValue: SizeInt): TIntArray;
begin
  Result := TIntArray.Construct(aLength, aValue);
end;

function TGSparseGraph.CreateIntArray(aValue: SizeInt): TIntArray;
begin
  Result := TIntArray.Construct(VertexCount, aValue);
end;

function TGSparseGraph.CreateIntArrayRange: TIntArray;
begin
  Result := TIntHelper.CreateRange(0, Pred(VertexCount));
end;

function TGSparseGraph.CreateColorArray: TColorArray;
begin
  System.SetLength(Result, VertexCount);
  System.FillChar(Pointer(Result)^, VertexCount, 0);
end;

function TGSparseGraph.CreateAdjEnumArray: TAdjEnumArray;
var
  I: SizeInt;
begin
  System.SetLength(Result, VertexCount);
  for I := 0 to Pred(VertexCount) do
    Result[I].FEnum := AdjLists[I]^.GetEnumerator;
end;

function TGSparseGraph.CreateAdjEnumArrayEx: TAdjEnumArrayEx;
var
  I: SizeInt;
begin
  System.SetLength(Result, VertexCount);
  for I := 0 to Pred(VertexCount) do
    Result[I] := AdjLists[I]^.GetEnumerator;
end;

function TGSparseGraph.PathToNearestFrom(aSrc: SizeInt; constref aTargets: TIntArray): TIntArray;
var
  Dist,
  Parents: TIntArray;
  Curr, d, Nearest: SizeInt;
begin
  if aTargets = nil then
    exit([]);
  Dist := ShortestPathsMapI(aSrc, Parents);
  d := VertexCount;
  Nearest := NULL_INDEX;
  for Curr in aTargets do
    if Dist[Curr] < d then
      begin
        Nearest := Curr;
        d := Dist[Curr];
      end;
  if Nearest <> NULL_INDEX then
    Result := TreePathTo(Parents, Nearest)
  else
    Result := [];
end;

procedure TGSparseGraph.AssignVertexList(aGraph: TGSparseGraph; const aList: TIntArray);
var
  VertSet: TBitVector;
  I: SizeInt;
  p: PAdjItem;
begin
  Clear;
  VertSet.Size := aGraph.VertexCount;
  for I in aList do
    begin
      {%H-}AddVertex(aGraph[I]);
      VertSet[I] := True;
    end;
  for I in aList do
    for p in aGraph.AdjLists[I]^ do
      if VertSet[p^.Key] then
        AddEdge(aGraph[I], aGraph[p^.Key], p^.Data);
end;

procedure TGSparseGraph.AssignTree(aGraph: TGSparseGraph; const aTree: TIntArray);
var
  I, Src: SizeInt;
begin
  Clear;
  for I := 0 to Pred(System.Length(aTree)) do
    begin
      {%H-}AddVertex(aGraph[I]);
      Src := aTree[I];
      if Src <> -1 then
        AddEdge(aGraph[Src], aGraph[I], aGraph.GetEdgeDataPtr(Src, I)^);
    end;
end;

procedure TGSparseGraph.AssignEdges(aGraph: TGSparseGraph; const aEdges: TIntEdgeArray);
var
  e: TIntEdge;
begin
  Clear;
  for e in aEdges do
    AddEdge(aGraph[e.Source], aGraph[e.Destination], aGraph.GetEdgeDataPtr(e.Source, e.Destination)^);
end;

class function TGSparseGraph.cMin(L, R: TCost): TCost;
begin
  if L <= R then
    Result := L
  else
    Result := R;
end;

class function TGSparseGraph.cMax(L, R: TCost): TCost;
begin
  if L >= R then
    Result := L
  else
    Result := R;
end;

class function TGSparseGraph.MaxBitMatrixSize: SizeInt;
begin
  Result := TSquareBitMatrix.MaxSize;
end;

class function TGSparseGraph.TreePathTo(const aTree: TIntArray; aValue: SizeInt): TIntArray;
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

class function TGSparseGraph.TreePathFromTo(const aTree: TIntArray; aFrom, aTo: SizeInt): TIntArray;
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

class function TGSparseGraph.TreePathLen(const aTree: TIntArray; aFrom, aTo: SizeInt): SizeInt;
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

constructor TGSparseGraph.Create;
begin
  FDescription := TStringList.Create;
  FDescription.WriteBOM := False;
  FDescription.SkipLastLineBreak := True;
  FDescription.DefaultEncoding := TEncoding.UTF8;
  Title := 'Untitled';
end;

destructor TGSparseGraph.Destroy;
begin
  FDescription.Free;
  inherited;
end;

function TGSparseGraph.IsEmpty: Boolean;
begin
  Result := VertexCount = 0;
end;

function TGSparseGraph.NonEmpty: Boolean;
begin
  Result := VertexCount <> 0;
end;

procedure TGSparseGraph.Clear;
begin
  FNodeList := nil;
  FChainList := nil;
  FCount := 0;
  FEdgeCount := 0;
  FTitle := '';
  FDescription.Clear;
end;

procedure TGSparseGraph.EnsureCapacity(aValue: SizeInt);
begin
  if aValue > Capacity then
    if aValue < MAX_CONTAINER_SIZE div SizeOf(TNode) then
      Resize(LGUtils.RoundUpTwoPower(aValue))
    else
      raise EGraphError.CreateFmt(SECapacityExceedFmt, [aValue]);
end;

procedure TGSparseGraph.TrimToFit;
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

procedure TGSparseGraph.SaveToStream(aStream: TStream; aOnWriteVertex: TOnWriteVertex; aOnWriteData: TOnWriteData);
var
  Header: TStreamHeader;
  I: Integer;
  gTitle, Descr: utf8string;
  wbs: TWriteBufStream;
begin
  if not Assigned(aOnWriteVertex) then
    raise EGraphError.Create(SEStreamWriteVertMissed);
  if not Assigned(aOnWriteData) then
    raise EGraphError.Create(SEStreamWriteDataMissed);
{$IFDEF CPU64}
  if VertexCount > System.High(Integer) then
    raise EGraphError.CreateFmt(SEStreamSizeExceedFmt, [VertexCount]);
{$ENDIF CPU64}
  wbs := TWriteBufStream.Create(aStream);
  try
    //write header
    Header.Magic := GRAPH_MAGIC;
    Header.Version := GRAPH_HEADER_VERSION;
    gTitle := Title;
    Header.TitleLen := System.Length(gTitle);
    Descr := Description.Text;
    Header.DescriptionLen := System.Length(Descr);
    Header.VertexCount := VertexCount;
    Header.EdgeCount := EdgeCount;
    wbs.WriteBuffer(Header, SizeOf(Header));
    //write title
    if Header.TitleLen > 0 then
      wbs.WriteBuffer(Pointer(gTitle)^, Header.TitleLen);
    //write description
    if Header.DescriptionLen > 0 then
      wbs.WriteBuffer(Pointer(Descr)^, Header.DescriptionLen);
    //write Items, but does not save any info about connected
    //this should allow transfer data between directed/undirected graphs ???
    for I := 0 to Pred(Header.VertexCount) do
      aOnWriteVertex(wbs, FNodeList[I].Vertex);
    //write edges
    DoWriteEdges(wbs, aOnWriteData);
  finally
    wbs.Free;
  end;
end;

procedure TGSparseGraph.LoadFromStream(aStream: TStream; aOnReadVertex: TOnReadVertex; aOnReadData: TOnReadData);
var
  Header: TStreamHeader;
  s, d: Integer;
  I, Ind: SizeInt;
  Data: TEdgeData;
  Vertex: TVertex;
  gTitle, Descr: utf8string;
  rbs: TReadBufStream;
begin
  if not Assigned(aOnReadVertex) then
    raise EGraphError.Create(SEStreamReadVertMissed);
  if not Assigned(aOnReadData) then
    raise EGraphError.Create(SEStreamReadDataMissed);
  rbs := TReadBufStream.Create(aStream);
  try
    //read header
    rbs.ReadBuffer(Header, SizeOf(Header));
    if Header.Magic <> GRAPH_MAGIC then
      raise EGraphError.Create(SEUnknownGraphStreamFmt);
    if Header.Version > GRAPH_HEADER_VERSION then
      raise EGraphError.Create(SEUnsuppGraphFmtVersion);
    Clear;
    EnsureCapacity(Header.VertexCount);
    //read title
    if Header.TitleLen > 0 then
      begin
        System.SetLength(gTitle, Header.TitleLen);
        rbs.ReadBuffer(Pointer(gTitle)^, Header.TitleLen);
        FTitle := gTitle;
      end;
    //read description
    if Header.DescriptionLen > 0 then
      begin
        System.SetLength(Descr, Header.DescriptionLen);
        rbs.ReadBuffer(Pointer(Descr)^, Header.DescriptionLen);
        Description.Text := Descr;
      end;
    //read Items
    for I := 0 to Pred(Header.VertexCount) do
      begin
        aOnReadVertex(rbs, Vertex);
        if not AddVertex(Vertex, Ind) then
          raise EGraphError.Create(SEGraphStreamCorrupt);
        if Ind <> I then
          raise EGraphError.Create(SEGraphStreamReadIntern);
      end;
    //read edges
    Data := Default(TEdgeData);
    for I := 0 to Pred(Header.EdgeCount) do
      begin
        rbs.ReadBuffer(s, SizeOf(s));
        rbs.ReadBuffer(d, SizeOf(d));
        aOnReadData(rbs, Data);
        AddEdgeI(LEToN(s), LEToN(d), Data);
      end;
  finally
    rbs.Free;
  end;
end;

procedure TGSparseGraph.SaveToFile(const aFileName: string; aOnWriteVertex: TOnWriteVertex;
  aOnWriteData: TOnWriteData);
var
  fs: TStream;
begin
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(fs, aOnWriteVertex, aOnWriteData);
  finally
    fs.Free;
  end;
end;

procedure TGSparseGraph.LoadFromFile(const aFileName: string; aOnReadVertex: TOnReadVertex;
  aOnReadData: TOnReadData);
var
  fs: TStream;
begin
  fs := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs, aOnReadVertex, aOnReadData);
  finally
    fs.Free;
  end;
end;

function TGSparseGraph.AddVertex(constref aVertex: TVertex; out aIndex: SizeInt): Boolean;
begin
  Result := DoAddVertex(aVertex, aIndex);
end;

function TGSparseGraph.AddVertex(constref aVertex: TVertex): Boolean;
var
  Dummy: SizeInt;
begin
  Result := AddVertex(aVertex, Dummy);
end;

function TGSparseGraph.AddVertices(const aVertices: TVertexArray): SizeInt;
var
  v: TVertex;
begin
  Result := VertexCount;
  for v in aVertices do
    AddVertex(v);
  Result := VertexCount - Result;
end;

procedure TGSparseGraph.RemoveVertex(constref aVertex: TVertex);
begin
  RemoveVertexI(IndexOf(aVertex));
end;

procedure TGSparseGraph.RemoveVertexI(aIndex: SizeInt);
begin
  CheckIndexRange(aIndex);
  DoRemoveVertex(aIndex);
end;

function TGSparseGraph.ContainsVertex(constref aVertex: TVertex): Boolean;
begin
  Result := IndexOf(aVertex) >= 0;
end;

function TGSparseGraph.AddEdge(constref aSrc, aDst: TVertex; aData: TEdgeData): Boolean;
var
  SrcIdx, DstIdx: SizeInt;
begin
  AddVertex(aSrc, SrcIdx);
  AddVertex(aDst, DstIdx);
  Result := DoAddEdge(SrcIdx, DstIdx, aData);
end;

function TGSparseGraph.AddEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := AddEdge(aSrc, aDst, Default(TEdgeData));
end;

function TGSparseGraph.AddEdgeI(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  Result := DoAddEdge(aSrc, aDst, aData);
end;

function TGSparseGraph.AddEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := AddEdgeI(aSrc, aDst, Default(TEdgeData));
end;

function TGSparseGraph.RemoveEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := RemoveEdgeI(IndexOf(aSrc), IndexOf(aDst));
end;

function TGSparseGraph.RemoveEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  if (SizeUInt(aSrc) < SizeUInt(VertexCount)) and (SizeUInt(aDst) < SizeUInt(VertexCount)) then
    Result := DoRemoveEdge(aSrc, aDst)
  else
    Result := False;
end;

function TGSparseGraph.ContractEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := ContractEdgeI(IndexOf(aSrc), IndexOf(aDst));
end;

function TGSparseGraph.ContractEdgeI(aSrc, aDst: SizeInt): Boolean;
var
  DstEdges: array of TIncidentEdge;
  e: TIncidentEdge;
  I, Deg: SizeInt;
  pList: PAdjList;
  p: PAdjItem;
begin
  if not RemoveEdgeI(aSrc, aDst) then
    exit(False);
  pList := AdjLists[aDst];
  Deg := pList^.Count;
  if Deg > 0 then
    begin
      I := 0;
      System.SetLength(DstEdges, Deg);
      for p in pList^ do
        begin
          DstEdges[I].Destination := p^.Destination;
          DstEdges[I].Data := p^.Data;
          Inc(I);
        end;
      for e in DstEdges do
        begin
          DoRemoveEdge(aDst, e.Destination);
          DoAddEdge(aSrc, e.Destination, e.Data);
        end;
    end;
  DoRemoveVertex(aDst);
  Result := True;
end;

function TGSparseGraph.ContainsEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := ContainsEdgeI(IndexOf(aSrc), IndexOf(aDst));
end;

function TGSparseGraph.ContainsEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  if SizeUInt(aSrc) < SizeUInt(VertexCount) then
    Result := AdjLists[aSrc]^.Contains(aDst)
  else
    Result := False;
end;

function TGSparseGraph.IndexOf(constref aVertex: TVertex): SizeInt;
begin
  if VertexCount > 0 then
    Result := Find(aVertex)
  else
    Result := NULL_INDEX;
end;

function TGSparseGraph.Adjacent(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := AdjacentI(IndexOf(aSrc), IndexOf(aDst));
end;

function TGSparseGraph.AdjacentI(aSrc, aDst: SizeInt): Boolean;
begin
  if SizeUInt(aSrc) < SizeUInt(VertexCount) then
    Result := AdjLists[aSrc]^.Contains(aDst)
  else
    Result := False;
end;

function TGSparseGraph.AdjVertices(constref aVertex: TVertex): TAdjVertices;
begin
  Result := AdjVerticesI(IndexOf(aVertex));
end;

function TGSparseGraph.AdjVerticesI(aIndex: SizeInt): TAdjVertices;
begin
  CheckIndexRange(aIndex);
  Result := TAdjVertices.Create(Self, aIndex);
end;

function TGSparseGraph.IncidentEdges(constref aVertex: TVertex): TIncidentEdges;
begin
  Result := IncidentEdgesI(IndexOf(aVertex));
end;

function TGSparseGraph.IncidentEdgesI(aIndex: SizeInt): TIncidentEdges;
begin
  CheckIndexRange(aIndex);
  Result := TIncidentEdges.Create(Self, aIndex);
end;

function TGSparseGraph.Vertices: TVertices;
begin
  Result := TVertices.Create(Self);
end;

function TGSparseGraph.Edges: TEdges;
begin
  Result.FGraph := Self;
end;

function TGSparseGraph.GetEdgeData(constref aSrc, aDst: TVertex; out aData: TEdgeData): Boolean;
begin
  Result := GetEdgeDataI(IndexOf(aSrc), IndexOf(aDst), aData);
end;

function TGSparseGraph.GetEdgeDataI(aSrc, aDst: SizeInt; out aData: TEdgeData): Boolean;
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

function TGSparseGraph.SetEdgeData(constref aSrc, aDst: TVertex; constref aValue: TEdgeData): Boolean;
begin
  Result := SetEdgeDataI(IndexOf(aSrc), IndexOf(aDst), aValue);
end;

function TGSparseGraph.SetEdgeDataI(aSrc, aDst: SizeInt; constref aValue: TEdgeData): Boolean;
begin
  CheckIndexRange(aSrc);
  Result := DoSetEdgeData(aSrc, aDst, aValue);
end;

function TGSparseGraph.IndexPath2VertexPath(const aIdxPath: TIntArray): TVertexArray;
var
  I: SizeInt;
begin
  System.SetLength(Result, aIdxPath.Length);
  for I := 0 to Pred(aIdxPath.Length) do
    Result[I] := Items[aIdxPath[I]];
end;

function TGSparseGraph.VertexPath2IndexPath(const aVertPath: TVertexArray): TIntArray;
var
  I: SizeInt;
begin
  Result{%H-}.Length := System.Length(aVertPath);
  for I := 0 to Pred(Result.Length) do
    Result[I] := IndexOf(aVertPath[I]);
end;

function TGSparseGraph.CreateAdjacencyMatrix: TAdjacencyMatrix;
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

function TGSparseGraph.IsBipartite: Boolean;
var
  Colors: TColorArray;
begin
  Result := IsBipartite(Colors);
end;

function TGSparseGraph.IsBipartite(out aColors: TColorArray): Boolean;
var
  Queue: TIntArray;
  Curr, I: SizeInt;
  p: PAdjItem;
  CurrColor: TVertexColor;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  if VertexCount < 2 then
    exit(False);
  System.SetLength(Queue, VertexCount);
  aColors := CreateColorArray;
  for I := 0 to System.High(aColors) do
    if aColors[I] = vcNone then
      begin
        Curr := I;
        aColors[I] := vcWhite;
        Queue[qTail] := I;
        Inc(qTail);
        while qHead < qTail do
          begin
            Curr := Queue[qHead];
            Inc(qHead);
            CurrColor := aColors[Curr];
            for p in AdjLists[Curr]^ do
              if aColors[p^.Destination] = vcNone then
                begin
                  aColors[p^.Destination] := vcBlack - CurrColor;
                  Queue[qTail] := p^.Destination;
                  Inc(qTail);
                end
              else
                if aColors[p^.Destination] = CurrColor then
                  begin
                    aColors := nil;
                    exit(False);
                  end;
          end;
      end;
  Result := True;
end;

function TGSparseGraph.IsBipartite(out aWhites, aGrays: TIntArray): Boolean;
var
  Colors: TColorArray;
  WhiteIdx, GrayIdx, I: SizeInt;
  CurrColor: TVertexColor;
begin
  Result := IsBipartite(Colors);
  if not Result then
    exit;
  System.SetLength(aWhites, VertexCount);
  System.SetLength(aGrays, VertexCount);
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

function TGSparseGraph.IsMaxMatching(const aMatch: TIntEdgeArray): Boolean;
var
  vFree: TBoolVector;
  e: TIntEdge;
  I, J: SizeInt;
begin
  if VertexCount < 2 then
    exit(False);
  if System.Length(aMatch) = 0 then
    exit(False);
  vFree.InitRange(VertexCount);
  for e in aMatch do
    begin
      if SizeUInt(e.Source) >= SizeUInt(VertexCount) then //contains garbage
        exit(False);
      if SizeUInt(e.Destination) >= SizeUInt(VertexCount) then //contains garbage
        exit(False);
      if e.Source = e.Destination then //contains garbage
        exit(False);
      if not AdjLists[e.Source]^.Contains(e.Destination) then //contains garbage
        exit(False);
      if not vFree[e.Source] then  //contains adjacent edges -> not matching
        exit(False);
      vFree[e.Source] := False;
      if not vFree[e.Destination] then  //contains adjacent edges -> not matching
        exit(False);
      vFree[e.Destination] := False;
    end;
  for I in vFree do
    for J in AdjVerticesI(I) do
      if vFree[J] then  // is not maximal
        exit(False);
  Result := True;
end;

function TGSparseGraph.IsPerfectMatching(const aMatch: TIntEdgeArray): Boolean;
var
  vFree: TBoolVector;
  e: TIntEdge;
begin
  if (VertexCount < 2) or Odd(VertexCount) then
    exit(False);
  if System.Length(aMatch) <> VertexCount div 2 then
    exit(False);
  vFree.InitRange(VertexCount);
  for e in aMatch do
    begin
      if SizeUInt(e.Source) >= SizeUInt(VertexCount) then //contains garbage
        exit(False);
      if SizeUInt(e.Destination) >= SizeUInt(VertexCount) then //contains garbage
        exit(False);
      if e.Source = e.Destination then //contains garbage
        exit(False);
      if not AdjLists[e.Source]^.Contains(e.Destination) then //contains garbage
        exit(False);
      if not vFree[e.Source] then  //contains adjacent edges -> not matching
        exit(False);
      vFree[e.Source] := False;
      if not vFree[e.Destination] then  //contains adjacent edges -> not matching
        exit(False);
      vFree[e.Destination] := False;
    end;
  Result := vFree.IsEmpty;
end;

function TGSparseGraph.DfsTraversal(constref aRoot: TVertex; aOnFound: TOnAccept; aOnDone: TOnVisit): SizeInt;
begin
  Result := DfsTraversalI(IndexOf(aRoot), aOnFound, aOnDone);
end;

function TGSparseGraph.DfsTraversalI(aRoot: SizeInt; aOnFound: TOnAccept; aOnDone: TOnVisit): SizeInt;
var
  Stack: TIntArray;
  Visited: TBitVector;
  AdjEnums: TAdjEnumArray;
  Next: SizeInt;
  sTop: SizeInt = -1;
begin
  Result := 0;
  CheckIndexRange(aRoot);
  Inc(Result);
  if Assigned(aOnFound) and not aOnFound(Self, aRoot) then
    exit;
  Visited.Size := VertexCount;
  AdjEnums := CreateAdjEnumArray;
  {%H-}Stack := CreateIntArray;
  Visited[aRoot] := True;
  Inc(sTop);
  Stack[sTop] := aRoot;
  while sTop >= 0 do
    begin
      aRoot := Stack[sTop];
      if AdjEnums[aRoot].MoveNext then
        begin
          Next := AdjEnums[aRoot].Current;
          if not Visited[Next] then
            begin
              Inc(Result);
              if Assigned(aOnFound) and not aOnFound(Self, Next) then
                exit;
              Visited[Next] := True;
              Inc(sTop);
              Stack[sTop] := Next;
            end;
        end
      else
        begin
          if Assigned(aOnDone) then
            aOnDone(Self, Stack[sTop]);
          Dec(sTop);
        end;
    end;
end;

function TGSparseGraph.DfsTree: TIntArray;
var
  Stack: TSimpleStack;
  AdjEnums: TAdjEnumArray;
  Visited: TBitVector;
  I, Curr, Next: SizeInt;
begin
  if IsEmpty then
    exit(nil);
  Stack := TSimpleStack.Create(VertexCount);
  Result := CreateIntArray;
  AdjEnums := CreateAdjEnumArray;
  Visited.Size := VertexCount;
  for I := 0 to Pred(VertexCount) do
    if not Visited[I] then
      begin
        {%H-}Stack.Push(I);
        Visited[I] := True;
        while Stack.TryPeek(Curr) do
          if AdjEnums[{%H-}Curr].MoveNext then
            begin
              Next := AdjEnums[Curr].Current;
              if not Visited[Next] then
                begin
                  Result[Next] := Curr;
                  Visited[Next] := True;
                  Stack.Push(Next);
                end;
            end
          else
            Stack.Pop;
      end;
end;

function TGSparseGraph.BfsTraversal(constref aRoot: TVertex; aOnFound: TOnAccept; aOnDone: TOnVisit): SizeInt;
begin
  Result := BfsTraversalI(IndexOf(aRoot), aOnFound, aOnDone);
end;

function TGSparseGraph.BfsTraversalI(aRoot: SizeInt; aOnFound: TOnAccept; aOnDone: TOnVisit): SizeInt;
var
  Queue: TIntArray;
  Visited: TBitVector;
  p: PAdjItem;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  Result := 0;
  CheckIndexRange(aRoot);
  Inc(Result);
  if Assigned(aOnFound) and not aOnFound(Self, aRoot) then
    exit;
  Visited.Size := VertexCount;
  Queue.Length := VertexCount;
  Visited[aRoot] := True;
  Queue[qTail] := aRoot;
  Inc(qTail);
  while qHead < qTail do
    begin
      aRoot := Queue[qHead];
      Inc(qHead);
      for p in AdjLists[aRoot]^ do
        if not Visited[p^.Destination] then
          begin
            Inc(Result);
            if Assigned(aOnFound) and not aOnFound(Self, p^.Destination) then
              exit;
            Queue[qTail] := p^.Destination;
            Inc(qTail);
            Visited[p^.Destination] := True;
          end;
      if Assigned(aOnDone) then
        aOnDone(Self, aRoot);
    end;
end;

function TGSparseGraph.BfsTree: TIntArray;
var
  Queue: TIntArray;
  Visited: TBitVector;
  I, Curr, Next: SizeInt;
  p: PAdjItem;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  if IsEmpty then
    exit(nil);
  Queue.Length := VertexCount;
  Visited.Size := VertexCount;
  Result := CreateIntArray;
  for I := 0 to Pred(VertexCount) do
    if not Visited[I] then
      begin
        Queue[qTail] := I;
        Inc(qTail);
        Visited[I] := True;
        while qHead < qTail do
          begin
            Curr := Queue[qHead];
            Inc(qHead);
            for p in AdjLists[Curr]^ do
              begin
                Next := p^.Destination;
                if not Visited[Next] then
                  begin
                    Result[Next] := Curr;
                    Visited[Next] := True;
                    Queue[qTail] := Next;
                    Inc(qTail);
                  end;
              end;
          end;
      end;
end;

function TGSparseGraph.ShortestPathLen(constref aSrc, aDst: TVertex): SizeInt;
begin
  Result := ShortestPathLenI(IndexOf(aSrc), IndexOf(aDst));
end;

function TGSparseGraph.ShortestPathLenI(aSrc, aDst: SizeInt): SizeInt;
var
  Queue: TIntArray;
  Dist: TIntArray;
  d: SizeInt;
  p: PAdjItem;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  System.SetLength(Queue, VertexCount);
  {%H-}Dist := CreateIntArray;
  Dist[aSrc] := 0;
  Queue[qTail] := aSrc;
  Inc(qTail);
  while qHead < qTail do
    begin
      aSrc := Queue[qHead];
      Inc(qHead);
      if aSrc = aDst then
        exit(Dist[aSrc]);
      d := Succ(Dist[aSrc]);
      for p in AdjLists[aSrc]^ do
        if Dist[p^.Destination] = NULL_INDEX then
          begin
            Queue[qTail] := p^.Destination;
            Inc(qTail);
            Dist[p^.Destination] := d;
          end;
    end;
  Result := NULL_INDEX;
end;

function TGSparseGraph.ShortestPathsMap(constref aSrc: TVertex): TIntArray;
begin
  Result := ShortestPathsMapI(IndexOf(aSrc));
end;

function TGSparseGraph.ShortestPathsMapI(aSrc: SizeInt): TIntArray;
var
  Queue: TIntArray;
  d: SizeInt;
  p: PAdjItem;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  CheckIndexRange(aSrc);
  System.SetLength(Queue, VertexCount);
  Result := CreateIntArray;
  Result[aSrc] := 0;
  Queue[qTail] := aSrc;
  Inc(qTail);
  while qHead < qTail do
    begin
      aSrc := Queue[qHead];
      Inc(qHead);
      d := Succ(Result[aSrc]);
      for p in AdjLists[aSrc]^ do
        if Result[p^.Destination] = NULL_INDEX then
          begin
            Queue[qTail] := p^.Destination;
            Inc(qTail);
            Result[p^.Destination] := d;
          end;
    end;
end;

function TGSparseGraph.ShortestPathsMap(constref aSrc: TVertex; out aPathTree: TIntArray): TIntArray;
begin
  Result := ShortestPathsMapI(IndexOf(aSrc), aPathTree);
end;

function TGSparseGraph.ShortestPathsMapI(aSrc: SizeInt; out aPathTree: TIntArray): TIntArray;
var
  Queue: TIntArray;
  d: SizeInt;
  p: PAdjItem;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  CheckIndexRange(aSrc);
  System.SetLength(Queue, VertexCount);
  Result := CreateIntArray;
  aPathTree := CreateIntArray;
  Result[aSrc] := 0;
  Queue[qTail] := aSrc;
  Inc(qTail);
  while qHead < qTail do
    begin
      aSrc := Queue[qHead];
      Inc(qHead);
      d := Succ(Result[aSrc]);
      for p in AdjLists[aSrc]^ do
        if Result[p^.Destination] = NULL_INDEX then
          begin
            Queue[qTail] := p^.Destination;
            Inc(qTail);
            Result[p^.Destination] := d;
            aPathTree[p^.Destination] := aSrc;
          end;
    end;
end;

function TGSparseGraph.ShortestPath(constref aSrc, aDst: TVertex): TIntArray;
begin
  Result := ShortestPathI(IndexOf(aSrc), IndexOf(aDst));
end;

function TGSparseGraph.ShortestPathI(aSrc, aDst: SizeInt): TIntArray;
var
  Queue: TIntArray;
  Parents: TIntArray;
  Curr: SizeInt;
  p: PAdjItem;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  System.SetLength(Queue, VertexCount);
  Parents := CreateIntArray;
  Queue[qTail] := aSrc;
  Inc(qTail);
  Parents[aSrc] := aSrc;
  while qHead < qTail do
    begin
      Curr := Queue[qHead];
      Inc(qHead);
      if Curr = aDst then
        begin
          Parents[aSrc] := NULL_INDEX;
          exit(TreePathTo(Parents, aDst));
        end;
      for p in AdjLists[Curr]^ do
        if Parents[p^.Destination] = NULL_INDEX then
          begin
            Queue[qTail] := p^.Destination;
            Inc(qTail);
            Parents[p^.Destination] := Curr;
          end;
    end;
  Result := nil;
end;

function TGSparseGraph.Eccentricity(constref aVertex: TVertex): SizeInt;
begin
  Result := EccentricityI(IndexOf(aVertex));
end;

function TGSparseGraph.EccentricityI(aIndex: SizeInt): SizeInt;
var
  Dist: TIntArray;
  I: SizeInt;
begin
  Dist := ShortestPathsMapI(aIndex);
  Result := 0;
  for I := 0 to System.High(Dist) do
    if Dist[I] = NULL_INDEX then
      exit(High(SizeInt))
    else
      if (Dist[I] > Result) then
        Result := Dist[I];
end;

{ TGAbstractDotWriter }

function TGAbstractDotWriter.DefaultWriteEdge(aGraph: TGraph; constref aEdge: TGraph.TEdge): utf8string;
begin
  Result := IntToStr(aEdge.Source) + FEdgeMark + IntToStr(aEdge.Destination);
end;

procedure TGAbstractDotWriter.SaveToStream(aGraph: TGraph; aStream: TStream);
var
  Dot: utf8string;
begin
  Dot := Graph2Dot(aGraph);
  aStream.WriteBuffer(Pointer(Dot)^, System.Length(Dot));
end;

procedure TGAbstractDotWriter.SaveToFile(aGraph: TGraph; const aFileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(aGraph, fs);
  finally
    fs.Free;
  end;
end;

{ TGTspHelper.TBbTsp.TMinData }

procedure TGTspHelper.TBbTsp.TMinData.Clear;
begin
  Value := T.INF_VALUE;
  ZeroFlag := False;
end;

{ TGTspHelper.TBbTsp }

procedure TGTspHelper.TBbTsp.Init(const m: TTspMatrix; const aTour: TIntArray; aTimeOut: Integer);
var
  I, J: Integer;
begin
  FMatrixSize := System.Length(m);
  System.SetLength(FMatrix, FMatrixSize * FMatrixSize);
  for I := 0 to Pred(FMatrixSize) do
    for J := 0 to Pred(FMatrixSize) do
      FMatrix[I * FMatrixSize + J] := m[I, J];
  for I := 0 to Pred(FMatrixSize) do
    FMatrix[I * FMatrixSize + I] := T.INF_VALUE;
  if aTour.Length = Succ(FMatrixSize) then
    begin
      System.SetLength(FBestTour, FMatrixSize);
      FUpBound := 0;
      for I := 0 to Pred(FMatrixSize) do
        begin
          FBestTour[aTour[I]] := aTour[Succ(I)];
          FUpBound += m[aTour[I], aTour[Succ(I)]];
        end;
    end
  else
    begin
      FBestTour := nil;
      FUpBound := T.INF_VALUE;
    end;
  FTimeOut := aTimeOut and System.High(Integer);
  System.SetLength(FForwardTour, FMatrixSize);
  System.FillChar(Pointer(FForwardTour)^, FMatrixSize * SizeOf(Integer), $ff);
  FBackTour := System.Copy(FForwardTour);
  System.SetLength(FRowMin, FMatrixSize);
  System.SetLength(FColMin, FMatrixSize);
  System.SetLength(FZeros, FMatrixSize);
  for I := 0 to Pred(FMatrixSize) do
    FZeros[I].Size := FMatrixSize;
  FStartTime := Now;
  FCancelled := False;
end;

function TGTspHelper.TBbTsp.TimeOut: Boolean;
begin
  FCancelled := FCancelled or (SecondsBetween(Now, FStartTime) >= FTimeOut);
  Result := FCancelled;
end;

function TGTspHelper.TBbTsp.Reduce(aSize: Integer; aCost: T; aRows, aCols: PInt; aRowRed, aColRed: PItem): T;
var
  I, J, Curr, MxSize: Integer;
  MinVal, CurrVal: T;
  m: PItem;
begin
  m := PItem(FMatrix);
  MxSize := FMatrixSize;
  Result := aCost;
  for I := 0 to Pred(aSize) do
    begin
      aRowRed[I] := T(0);
      aColRed[I] := T(0);
    end;
  //////////////////
  for I := 0 to Pred(aSize) do  // reduce rows
    begin
      MinVal := T.INF_VALUE;
      for J := 0 to Pred(aSize) do
        begin
          MinVal := vMin(MinVal, m[aRows[I] * MxSize + aCols[J]]);
          if MinVal <= T(0) then
            break;
        end;
      if (MinVal <= T(0)) or (MinVal = T.INF_VALUE) then
        continue;
      for J := 0 to Pred(aSize) do
        if m[aRows[I] * MxSize + aCols[J]] < T.INF_VALUE then
          m[aRows[I] * MxSize + aCols[J]] -= MinVal;
      Result += MinVal;
      aRowRed[I] := MinVal;
      if Result >= FUpBound then
        exit;
    end;
  //////////////////
  for J := 0 to Pred(aSize) do  // reduce columns
    begin
      MinVal := T.INF_VALUE;
      for I := 0 to Pred(aSize) do
        begin
          MinVal := vMin(MinVal, m[aRows[I] * MxSize + aCols[J]]);
          if MinVal <= T(0) then
            break;
        end;
      if (MinVal <= T(0)) or (MinVal = T.INF_VALUE) then
        continue;
      for I := 0 to Pred(aSize) do
        if m[aRows[I] * MxSize + aCols[J]] < T.INF_VALUE then
          m[aRows[I] * MxSize + aCols[J]] -= MinVal;
      Result += MinVal;
      aColRed[J] := MinVal;
      if Result >= FUpBound then
        exit;
    end;
end;

function TGTspHelper.TBbTsp.ReduceA(aSize: Integer; aCost: T; aRows, aCols: PInt; aRowRed, aColRed: PItem): T;
var
  I, J, K, MxSize, ZeroCount: Integer;
  MinVal, CurrVal: T;
  RowMin, ColMin: PMinData;
  m: PItem;
begin
  Result := Reduce(aSize, aCost, aRows, aCols, aRowRed, aColRed);
  if (aSize <= ADV_CUTOFF) or (Result >= FUpBound) then
    exit;
  m := PItem(FMatrix);
  RowMin := PMinData(FRowMin);
  ColMin := PMinData(FColMin);
  MxSize := FMatrixSize;
  //////////////////
  for I := 0 to Pred(aSize) do
    begin
      RowMin[I].Clear;
      ColMin[I].Clear;
      FZeros[I].ClearBits;
    end;
  //////////////////
  for I := 0 to Pred(aSize) do
    for J := 0 to Pred(aSize) do
      begin
        CurrVal := m[aRows[I] * MxSize + aCols[J]];
        if CurrVal > T(0) then
          if CurrVal < RowMin[I].Value then
            RowMin[I].Value := CurrVal else
        else
          if RowMin[I].ZeroFlag then
            begin
              RowMin[I].Value := T(0);
              FZeros[J][I] := False;
              break;
            end
          else
            begin
              RowMin[I].ZeroFlag := True;
              FZeros[J][I] := True;
            end;
      end;
  ///////////////
  for J := 0 to Pred(aSize) do
    begin
      ZeroCount := FZeros[J].PopCount;
      if ZeroCount > 1 then
        begin
          MinVal := T.INF_VALUE;
          for I in FZeros[J] do
            if RowMin[I].Value < MinVal then
              MinVal := RowMin[I].Value;
          if (MinVal <= T(0)) or (MinVal = T.INF_VALUE) then
            continue;
          for I := 0 to Pred(aSize) do
            if m[aRows[I] * MxSize + aCols[J]] < T.INF_VALUE then
              m[aRows[I] * MxSize + aCols[J]] += MinVal;
          aColRed[J] -= MinVal;
          for I in FZeros[J] do
            begin
              for K := 0 to Pred(aSize) do
                if m[aRows[I] * MxSize + aCols[K]] < T.INF_VALUE then
                  m[aRows[I] * MxSize + aCols[K]] -= MinVal;
              aRowRed[I] += MinVal;
            end;
          Result += MinVal * Pred(ZeroCount);
          if Result >= FUpBound then
            exit;
        end;
    end;
  //////////////
  for I := 0 to Pred(aSize) do
    FZeros[I].ClearBits;
  //////////////
  for J := 0 to Pred(aSize) do
    for I := 0 to Pred(aSize) do
      begin
        CurrVal := m[aRows[I] * MxSize + aCols[J]];
        if CurrVal > T(0) then
          if CurrVal < ColMin[J].Value then
            ColMin[J].Value := CurrVal else
        else
          if ColMin[J].ZeroFlag then
            begin
              ColMin[J].Value := T(0);
              FZeros[I][J] := False;
              break;
            end
          else
            begin
              ColMin[J].ZeroFlag := True;
              FZeros[I][J] := True;
            end;
      end;
  /////////////
  for I := 0 to Pred(aSize) do
    begin
      ZeroCount := FZeros[I].PopCount;
      if ZeroCount > 1 then
        begin
          MinVal := T.INF_VALUE;
          for J in FZeros[I] do
            if ColMin[J].Value < MinVal then
              MinVal := ColMin[J].Value;
          if (MinVal <= T(0)) or (MinVal = T.INF_VALUE) then
            continue;
          for J := 0 to Pred(aSize) do
            if m[aRows[I] * MxSize + aCols[J]] < T.INF_VALUE then
              m[aRows[I] * MxSize + aCols[J]] += MinVal;
          aRowRed[I] -= MinVal;
          for J in FZeros[I] do
            begin
              for K := 0 to Pred(aSize) do
                if m[aRows[K] * MxSize + aCols[J]] < T.INF_VALUE then
                  m[aRows[K] * MxSize + aCols[J]] -= MinVal;
              aColRed[J] += MinVal;
            end;
          Result += MinVal * Pred(ZeroCount);
          if Result >= FUpBound then
            exit;
        end;
    end;
end;

function TGTspHelper.TBbTsp.SelectNext(aSize: Integer; aRows, aCols: PInt; out aRowIdx, aColIdx: Integer): T;
var
  I, J, MxSize: Integer;
  MinVal, CurrVal: T;
  RowMin, ColMin: PMinData;
  m: PItem;
begin
  m := PItem(FMatrix);
  RowMin := PMinData(FRowMin);
  ColMin := PMinData(FColMin);
  MxSize := FMatrixSize;
  ////////////////////
  for I := 0 to Pred(aSize) do
    begin
      RowMin[I].Clear;
      ColMin[I].Clear;
      FZeros[I].ClearBits;
    end;
  /////////////////////////
  for I := 0 to Pred(aSize) do
    for J := 0 to Pred(aSize) do
      begin
        CurrVal := m[aRows[I] * MxSize + aCols[J]];
        if CurrVal > T(0) then
          begin
            if CurrVal < RowMin[I].Value then
              RowMin[I].Value := CurrVal;
            if CurrVal < ColMin[J].Value then
              ColMin[J].Value := CurrVal;
          end
        else
          begin
            FZeros[I][J] := True;
            if RowMin[I].ZeroFlag then
              RowMin[I].Value := T(0)
            else
              RowMin[I].ZeroFlag := True;
            if ColMin[J].ZeroFlag then
              ColMin[J].Value := T(0)
            else
              ColMin[J].ZeroFlag := True;
          end;
      end;
  ///////////////////////
  Result := T.NEGINF_VALUE;
  aRowIdx := NULL_INDEX;
  aColIdx := NULL_INDEX;
  ///////////////////////
  for I := 0 to Pred(aSize) do
    for J in FZeros[I] do
      begin
        CurrVal := RowMin[I].Value + ColMin[J].Value;
        if CurrVal > Result then
          begin
            Result := CurrVal;
            aRowIdx := I;
            aColIdx := J;
          end;
      end;
end;

procedure TGTspHelper.TBbTsp.Search(aSize: Integer; aCost: T; aRows, aCols: PInt);
var
  RowsReduce, ColsReduce: TArray;
  I, J, Row, Col, SaveRow, SaveCol, FirstRow, LastCol, MxSize: Integer;
  LowBound, SaveValue: T;
  m: PItem;
begin
  if TimeOut then
    exit;
  m := PItem(FMatrix);
  MxSize := FMatrixSize;
  System.SetLength(RowsReduce, aSize);
  System.SetLength(ColsReduce, aSize);
  if IsMetric then
    aCost := Reduce(aSize, aCost, aRows, aCols, PItem(RowsReduce), PItem(ColsReduce))
  else
    aCost := ReduceA(aSize, aCost, aRows, aCols, PItem(RowsReduce), PItem(ColsReduce));
  if aCost < FUpBound then
    if aSize > 2 then
      begin
        LowBound := aCost + SelectNext(aSize, aRows, aCols, Row, Col);
        SaveRow := aRows[Row];
        SaveCol := aCols[Col];
        FirstRow := SaveRow;
        LastCol := SaveCol;
        FForwardTour[SaveRow] := SaveCol;
        FBackTour[SaveCol] := SaveRow;
        while FForwardTour[LastCol] <> NULL_INDEX do
          LastCol := FForwardTour[LastCol];
        while FBackTour[FirstRow] <> NULL_INDEX do
          FirstRow := FBackTour[FirstRow];
        SaveValue := m[LastCol * MxSize + FirstRow];
        m[LastCol * MxSize + FirstRow] := T.INF_VALUE;
        for I := Row to aSize - 2 do // remove Row
          aRows[I] := aRows[Succ(I)];
        for J := Col to aSize - 2 do // remove Col
          aCols[J] := aCols[Succ(J)];
        ///////////////
        Search(Pred(aSize), aCost, aRows, aCols);
        ///////////////  restore values
        for I := aSize - 2 downto  Row do //restore Row
          aRows[Succ(I)] := aRows[I];
        aRows[Row] := SaveRow;
        for J := aSize - 2 downto  Col do //restore Col
          aCols[Succ(J)] := aCols[J];
        aCols[Col] := SaveCol;
        m[LastCol * MxSize + FirstRow] := SaveValue;
        FForwardTour[SaveRow] := NULL_INDEX;
        FBackTour[SaveCol] := NULL_INDEX;
        ////////////////
        if LowBound < FUpBound then
          begin
            m[SaveRow * MxSize + SaveCol] := T.INF_VALUE;
            //////////
            Search(aSize, aCost, aRows, aCols);
            //////////
            m[SaveRow * MxSize + SaveCol] := T(0);
          end;
      end
    else
      begin
        FBestTour := System.Copy(FForwardTour);
        Col := Ord(m[aRows[0] * MxSize + aCols[0]] < T.INF_VALUE);
        FBestTour[aRows[0]] := aCols[1 - Col];
        FBestTour[aRows[1]] := aCols[Col];
        FUpBound := aCost;
      end;
  for I := 0 to Pred(aSize) do      // restore matrix
    for J := 0 to Pred(aSize) do
      begin
        Col := aRows[I] * MxSize + aCols[J];
        SaveValue := m[Col];
        if SaveValue < T.INF_VALUE then
          m[Col] := SaveValue + RowsReduce[I] + ColsReduce[J];
      end;
end;

procedure TGTspHelper.TBbTsp.CopyBest(var aTour: TIntArray; out aCost: T);
var
  I, J: Integer;
begin
  aCost := FUpBound;
  if aCost < T.INF_VALUE then
    begin
      aTour.Length := Succ(FMatrixSize);
      J := 0;
      for I := 0 to Pred(FMatrixSize) do
        begin
          aTour[I] := J;
          J := FBestTour[J];
        end;
      aTour[FMatrixSize] := J;
    end;
end;

function TGTspHelper.TBbTsp.Execute(const m: TTspMatrix; aTimeOut: Integer; var aTour: TIntArray;
  out aCost: T): Boolean;
var
  Cols, Rows: array of Integer;
  I: Integer;
begin
  Init(m, aTour, aTimeOut);
  System.SetLength(Rows, FMatrixSize);
  for I := 0 to Pred(FMatrixSize) do
    Rows[I] := I;
  Cols := System.Copy(Rows);
  Search(FMatrixSize, T(0), PInt(Rows), PInt(Cols));
  CopyBest(aTour, aCost);
  Result := not FCancelled;
end;

{ TGTspHelper.TApproxBbTsp }

function TGTspHelper.TApproxBbTsp.Reduce(aSize: Integer; aCost: T; aRows, aCols: PInt; aRowRed, aColRed: PItem): T;
var
  I, J, Curr, MxSize: Integer;
  MinVal, CurrVal: T;
  m: PItem;
begin
  m := PItem(FMatrix);
  MxSize := FMatrixSize;
  Result := aCost;
  for I := 0 to Pred(aSize) do
    begin
      aRowRed[I] := T(0);
      aColRed[I] := T(0);
    end;
  //////////////////
  for I := 0 to Pred(aSize) do  // reduce rows
    begin
      MinVal := T.INF_VALUE;
      for J := 0 to Pred(aSize) do
        begin
          MinVal := vMin(MinVal, m[aRows[I] * MxSize + aCols[J]]);
          if MinVal <= T(0) then
            break;
        end;
      if (MinVal <= T(0)) or (MinVal = T.INF_VALUE) then
        continue;
      for J := 0 to Pred(aSize) do
        if m[aRows[I] * MxSize + aCols[J]] < T.INF_VALUE then
          m[aRows[I] * MxSize + aCols[J]] -= MinVal;
      Result += MinVal;
      aRowRed[I] := MinVal;
      if Result * Factor >= FUpBound then
        exit;
    end;
  //////////////////
  for J := 0 to Pred(aSize) do  // reduce columns
    begin
      MinVal := T.INF_VALUE;
      for I := 0 to Pred(aSize) do
        begin
          MinVal := vMin(MinVal, m[aRows[I] * MxSize + aCols[J]]);
          if MinVal <= T(0) then
            break;
        end;
      if (MinVal <= T(0)) or (MinVal = T.INF_VALUE) then
        continue;
      for I := 0 to Pred(aSize) do
        if m[aRows[I] * MxSize + aCols[J]] < T.INF_VALUE then
          m[aRows[I] * MxSize + aCols[J]] -= MinVal;
      Result += MinVal;
      aColRed[J] := MinVal;
      if Result * Factor >= FUpBound then
        exit;
    end;
end;

function TGTspHelper.TApproxBbTsp.ReduceA(aSize: Integer; aCost: T; aRows, aCols: PInt; aRowRed, aColRed: PItem): T;
var
  I, J, K, MxSize, ZeroCount: Integer;
  MinVal, CurrVal: T;
  RowMin, ColMin: PMinData;
  m: PItem;
begin
  Result := Reduce(aSize, aCost, aRows, aCols, aRowRed, aColRed);
  if (aSize <= ADV_CUTOFF) or (Result >= FUpBound) then
    exit;
  m := PItem(FMatrix);
  RowMin := PMinData(FRowMin);
  ColMin := PMinData(FColMin);
  MxSize := FMatrixSize;
  //////////////////
  for I := 0 to Pred(aSize) do
    begin
      RowMin[I].Clear;
      ColMin[I].Clear;
      FZeros[I].ClearBits;
    end;
  //////////////////
  for I := 0 to Pred(aSize) do
    for J := 0 to Pred(aSize) do
      begin
        CurrVal := m[aRows[I] * MxSize + aCols[J]];
        if CurrVal > T(0) then
          if CurrVal < RowMin[I].Value then
            RowMin[I].Value := CurrVal else
        else
          if RowMin[I].ZeroFlag then
            begin
              RowMin[I].Value := T(0);
              FZeros[J][I] := False;
              break;
            end
          else
            begin
              RowMin[I].ZeroFlag := True;
              FZeros[J][I] := True;
            end;
      end;
  ///////////////
  for J := 0 to Pred(aSize) do
    begin
      ZeroCount := FZeros[J].PopCount;
      if ZeroCount > 1 then
        begin
          MinVal := T.INF_VALUE;
          for I in FZeros[J] do
            if RowMin[I].Value < MinVal then
              MinVal := RowMin[I].Value;
          if (MinVal <= T(0)) or (MinVal = T.INF_VALUE) then
            continue;
          for I := 0 to Pred(aSize) do
            if m[aRows[I] * MxSize + aCols[J]] < T.INF_VALUE then
              m[aRows[I] * MxSize + aCols[J]] += MinVal;
          aColRed[J] -= MinVal;
          for I in FZeros[J] do
            begin
              for K := 0 to Pred(aSize) do
                if m[aRows[I] * MxSize + aCols[K]] < T.INF_VALUE then
                  m[aRows[I] * MxSize + aCols[K]] -= MinVal;
              aRowRed[I] += MinVal;
            end;
          Result += MinVal * Pred(ZeroCount);
          if Result * Factor >= FUpBound then
            exit;
        end;
    end;
  //////////////
  for I := 0 to Pred(aSize) do
    FZeros[I].ClearBits;
  //////////////
  for J := 0 to Pred(aSize) do
    for I := 0 to Pred(aSize) do
      begin
        CurrVal := m[aRows[I] * MxSize + aCols[J]];
        if CurrVal > T(0) then
          if CurrVal < ColMin[J].Value then
            ColMin[J].Value := CurrVal else
        else
          if ColMin[J].ZeroFlag then
            begin
              ColMin[J].Value := T(0);
              FZeros[I][J] := False;
              break;
            end
          else
            begin
              ColMin[J].ZeroFlag := True;
              FZeros[I][J] := True;
            end;
      end;
  /////////////
  for I := 0 to Pred(aSize) do
    begin
      ZeroCount := FZeros[I].PopCount;
      if ZeroCount > 1 then
        begin
          MinVal := T.INF_VALUE;
          for J in FZeros[I] do
            if ColMin[J].Value < MinVal then
              MinVal := ColMin[J].Value;
          if (MinVal <= T(0)) or (MinVal = T.INF_VALUE) then
            continue;
          for J := 0 to Pred(aSize) do
            if m[aRows[I] * MxSize + aCols[J]] < T.INF_VALUE then
              m[aRows[I] * MxSize + aCols[J]] += MinVal;
          aRowRed[I] -= MinVal;
          for J in FZeros[I] do
            begin
              for K := 0 to Pred(aSize) do
                if m[aRows[K] * MxSize + aCols[J]] < T.INF_VALUE then
                  m[aRows[K] * MxSize + aCols[J]] -= MinVal;
              aColRed[J] += MinVal;
            end;
          Result += MinVal * Pred(ZeroCount);
          if Result * Factor >= FUpBound then
            exit;
        end;
    end;
end;

procedure TGTspHelper.TApproxBbTsp.Search(aSize: Integer; aCost: T; aRows, aCols: PInt);
var
  RowsReduce, ColsReduce: TArray;
  I, J, Row, Col, SaveRow, SaveCol, FirstRow, LastCol, MxSize: Integer;
  LowBound, SaveValue: T;
  m: PItem;
begin
  if TimeOut then
    exit;
  m := PItem(FMatrix);
  MxSize := FMatrixSize;
  System.SetLength(RowsReduce, aSize);
  System.SetLength(ColsReduce, aSize);
  if IsMetric then
    aCost := Reduce(aSize, aCost, aRows, aCols, PItem(RowsReduce), PItem(ColsReduce))
  else
    aCost := ReduceA(aSize, aCost, aRows, aCols, PItem(RowsReduce), PItem(ColsReduce));
  if aCost * Factor < FUpBound then
    if aSize > 2 then
      begin
        LowBound := aCost + SelectNext(aSize, aRows, aCols, Row, Col);
        SaveRow := aRows[Row];
        SaveCol := aCols[Col];
        FirstRow := SaveRow;
        LastCol := SaveCol;
        FForwardTour[SaveRow] := SaveCol;
        FBackTour[SaveCol] := SaveRow;
        while FForwardTour[LastCol] <> NULL_INDEX do
          LastCol := FForwardTour[LastCol];
        while FBackTour[FirstRow] <> NULL_INDEX do
          FirstRow := FBackTour[FirstRow];
        SaveValue := m[LastCol * MxSize + FirstRow];
        m[LastCol * MxSize + FirstRow] := T.INF_VALUE;
        for I := Row to aSize - 2 do // remove Row
          aRows[I] := aRows[Succ(I)];
        for J := Col to aSize - 2 do // remove Col
          aCols[J] := aCols[Succ(J)];
        ///////////////
        Search(Pred(aSize), aCost, aRows, aCols);
        ///////////////  restore values
        for I := aSize - 2 downto  Row do //restore Row
          aRows[Succ(I)] := aRows[I];
        aRows[Row] := SaveRow;
        for J := aSize - 2 downto  Col do //restore Col
          aCols[Succ(J)] := aCols[J];
        aCols[Col] := SaveCol;
        m[LastCol * MxSize + FirstRow] := SaveValue;
        FForwardTour[SaveRow] := NULL_INDEX;
        FBackTour[SaveCol] := NULL_INDEX;
        ////////////////
        if LowBound * Factor < FUpBound then
          begin
            m[SaveRow * MxSize + SaveCol] := T.INF_VALUE;
            //////////
            Search(aSize, aCost, aRows, aCols);
            //////////
            m[SaveRow * MxSize + SaveCol] := T(0);
          end;
      end
    else
      begin
        FBestTour := System.Copy(FForwardTour);
        Col := Ord(m[aRows[0] * MxSize + aCols[0]] < T.INF_VALUE);
        FBestTour[aRows[0]] := aCols[1 - Col];
        FBestTour[aRows[1]] := aCols[Col];
        FUpBound := aCost;
      end;
  for I := 0 to Pred(aSize) do      // restore matrix
    for J := 0 to Pred(aSize) do
      begin
        Col := aRows[I] * MxSize + aCols[J];
        SaveValue := m[Col];
        if SaveValue < T.INF_VALUE then
          m[Col] := SaveValue + RowsReduce[I] + ColsReduce[J];
      end;
end;

function TGTspHelper.TApproxBbTsp.Execute(const m: TTspMatrix; aEps: Double; aTimeOut: Integer;
  var aTour: TIntArray; out aCost: T): Boolean;
var
  Cols, Rows: array of Integer;
  I: Integer;
begin
  Factor := Double(1.0) + aEps;
  Init(m, aTour, aTimeOut);
  System.SetLength(Rows, FMatrixSize);
  for I := 0 to Pred(FMatrixSize) do
    Rows[I] := I;
  Cols := System.Copy(Rows);
  Search(FMatrixSize, T(0), PInt(Rows), PInt(Cols));
  CopyBest(aTour, aCost);
  Result := not FCancelled;
end;

{ TGTspHelper.TLs3Opt }

procedure TGTspHelper.TLs3Opt.PickSwapKind(var aSwap: TSwap);
var
  OldCost, MaxGain: T;
begin
  aSwap.Gain := 0;
  OldCost := Matrix[aSwap.X1, aSwap.X2] + Matrix[aSwap.Y1, aSwap.Y2] + Matrix[aSwap.Z1, aSwap.Z2];
  MaxGain := OldCost - (Matrix[aSwap.Y1, aSwap.X1] + Matrix[aSwap.Z1, aSwap.X2] + Matrix[aSwap.Z2, aSwap.Y2]);
  if MaxGain > aSwap.Gain then
    begin
     aSwap.Gain := MaxGain;
     aSwap.IsAsymm := True;
    end;
  MaxGain := OldCost - (Matrix[aSwap.X1, aSwap.Y2] + Matrix[aSwap.Z1, aSwap.X2] + Matrix[aSwap.Y1, aSwap.Z2]);
  if MaxGain > aSwap.Gain then
    begin
      aSwap.Gain := MaxGain;
      aSwap.IsAsymm := False;
    end;
end;

procedure TGTspHelper.TLs3Opt.Reverse(aFirst, aLast: SizeInt);
var
  Head, Next: SizeInt;
begin
  if aFirst <> aLast then
    begin
      Next := CurrTour[aFirst];
      repeat
        Head := CurrTour[Next];
        CurrTour[Next] := aFirst;
        aFirst := Next;
        Next := Head;
      until aFirst = aLast;
    end;
end;

procedure TGTspHelper.TLs3Opt.Execute(var aCost: T);
var
  Best, Curr: TSwap;
  Len, I, J, K: SizeInt;
begin
  Len := CurrTour.Length;
  repeat
    Best.Gain := 0;
    Curr.X1 := 0;
    for I := 0 to Pred(Len) do
      begin
        Curr.X2 := CurrTour[Curr.X1];
        Curr.Y1 := Curr.X2;
        for J := 1 to Len - 4 do
          begin
            Curr.Y2 := CurrTour[Curr.Y1];
            Curr.Z1 := CurrTour[Curr.Y2];
            for K := J + 2 to Len - 2 do
              begin
                Curr.Z2 := CurrTour[Curr.Z1];
                PickSwapKind(Curr);
                if Curr.Gain > Best.Gain then
                  Best := Curr;
                Curr.Z1 := Curr.Z2;
              end;
            Curr.Y1 := Curr.Y2;
          end;
        Curr.X1 := Curr.X2;
      end;
    if Best.Gain > 0 then
      begin
        if Best.IsAsymm then
          begin
            Reverse(Best.Z2, Best.X1);
            CurrTour[Best.Y1] := Best.X1;
            CurrTour[Best.Z2] := Best.Y2
          end
        else
          begin
            CurrTour[Best.X1] := Best.Y2;
            CurrTour[Best.Y1] := Best.Z2;
          end;
        CurrTour[Best.Z1] := Best.X2;
        aCost -= Best.Gain;
      end;
  until Best.Gain = 0;
end;

procedure TGTspHelper.TLs3Opt.OptPath(const m: TTspMatrix; var aTour: TIntArray; var aCost: T);
var
  I, J, Len: SizeInt;
begin
  Len := System.Length(m);
  Matrix := m;
  CurrTour.Length := Len;
  for I := 0 to Pred(Len) do
    CurrTour[aTour[I]] := aTour[Succ(I)];
  Execute(aCost);
  J := 0;
  for I := 0 to Pred(Len) do
    begin
      aTour[I] := J;
      J := CurrTour[J];
    end;
  aTour[Len] := J;
end;

procedure TGTspHelper.TLs3Opt.OptEdges(const m: TTspMatrix; var aTour: TIntArray; var aCost: T);
begin
  CurrTour := aTour.Copy;
  Matrix := m;
  Execute(aCost);
  aTour := CurrTour;
end;

{ TGTspHelper }

class function TGTspHelper.vMin(L, R: T): T;
begin
  if L <= R then
    Result := L
  else
    Result := R;
end;

class function TGTspHelper.CheckMatrixProper(const m: TTspMatrix): Boolean;
var
  I, J, Size: SizeInt;
begin
  Size := System.Length(m);
  if Size < 2 then
    raise EGraphError.Create(SEInputMatrixTrivial);
  for I := 0 to Pred(Size) do
    if System.Length(m[I]) <> Size then
      raise EGraphError.Create(SENonSquareInputMatrix);
  Result := True;
  for I := 0 to Pred(Size) do
    for J := 0 to Pred(Size) do
      if I <> J then
        begin
          if m[I, J] < T(0) then
            raise EGraphError.Create(SEInputMatrixNegElem);
          if I > J then
            Result := Result and (m[I, J] = m[J, I]);
        end;
end;

class procedure TGTspHelper.NormalizeTour(aSrc: SizeInt; var aTour: TIntArray);
var
  I: SizeInt = 0;
begin
  while aTour[I] <> aSrc do
    Inc(I);
  TIntHelper.RotateLeft(aTour[0..Pred(System.High(aTour))], I);
  aTour[System.High(aTour)] := aTour[0];
end;

class procedure TGTspHelper.Ls2Opt(const m: TTspMatrix; var aTour: TIntArray; var aCost: T);
var
  I, J, L, R, Len: SizeInt;
  Cost, Gain, MaxGain: T;
begin
  Len := System.High(aTour);
  repeat
    MaxGain := T(0);
    L := NULL_INDEX;
    R := NULL_INDEX;
    for I := 0 to Len - 3 do
      begin
        Cost := m[aTour[I], aTour[Succ(I)]];
        for J := I + 2 to Pred(Len) do
          begin
            Gain := Cost + m[aTour[J], aTour[J+1]] - m[aTour[I], aTour[J]] - m[aTour[I+1], aTour[J+1]];
            if Gain > MaxGain then
              begin
                MaxGain := Gain;
                L := I;
                R := J;
              end;
          end;
      end;
    if MaxGain > T(0) then
      TIntHelper.Reverse(aTour[L+1..R]);
  until MaxGain <= T(0);
  aCost := GetTotalCost(m, aTour);
end;

class procedure TGTspHelper.Ls3OptPath(const m: TTspMatrix; var aTour: TIntArray; var aCost: T);
var
  Opt: TLs3Opt;
begin
  Opt.OptPath(m, aTour, aCost);
end;

class procedure TGTspHelper.Ls3OptEdges(const m: TTspMatrix; var aTour: TIntArray; var aCost: T);
var
  Opt: TLs3Opt;
begin
  Opt.OptEdges(m, aTour, aCost);
end;

class function TGTspHelper.GreedyFInsTsp(const m: TTspMatrix; aOnReady: TOnTourReady; out aCost: T): TIntArray;
var
  Tour: TIntArray;
  CurrRow: TArray;
  Unvisit: TBoolVector;
  Len, I, J, K, Source, Target, Curr, Next, Farthest: SizeInt;
  InsCost, MaxCost, Cost, TotalCost: T;
begin
  Len := System.Length(m);
  aCost := T.INF_VALUE;
  Tour.Length := Len;
  Result{%H-}.Length := Succ(Len);
  for K := 0 to Pred(Len) do
    begin
      Unvisit.InitRange(Len);
      Tour[K] := K;
      Unvisit[K] := False;
      CurrRow := System.Copy(m[K]);
      TotalCost := 0;
      MaxCost := T.NEGINF_VALUE;
      for J in Unvisit do
        if CurrRow[J] > MaxCost then
          begin
            MaxCost := CurrRow[J];
            Farthest := J;
          end;
      for I := 2 to Len do
        begin
          InsCost := T.INF_VALUE;
          Curr := K;
          for J := 0 to I do
            begin
              Next := Tour[Curr];
              Cost := m[Curr, Farthest] + m[Farthest, Next] - m[Curr, Next];
              if Cost < InsCost then
                begin
                  InsCost := Cost;
                  Source := Curr;
                  Target := Next;
                end;
              Curr := Next;
            end;
          Tour[Farthest] := Target;
          Tour[Source] := Farthest;
          TotalCost += InsCost;
          Unvisit[Farthest] := False;
          MaxCost := T.NEGINF_VALUE;
          for J in Unvisit do
            begin
              Cost := m[Farthest, J];
              if Cost < CurrRow[J] then
                CurrRow[J] := Cost;
              if CurrRow[J] > MaxCost then
                begin
                  MaxCost := CurrRow[J];
                  Next := J;
                end;
            end;
          Farthest := Next;
        end;
      if aOnReady <> nil then
        aOnReady(m, Tour, TotalCost);
      if TotalCost < aCost then
        begin
          aCost := TotalCost;
          J := 0;
          for I := 0 to Pred(Len) do
            begin
              Result[I] := J;
              J := Tour[J];
            end;
          Result[Len] := J;
        end;
    end;
end;

class function TGTspHelper.GreedyNearNeighb(const m: TTspMatrix; aOnReady: TOnTourReady; out aCost: T): TIntArray;
var
  Tour: TIntArray;
  Unvisit: TBoolVector;
  I, J, K, Curr, Next, Len: SizeInt;
  MinCost, CurrCost, TotalCost: T;
begin
  Len := System.Length(m);
  Result := nil;
  aCost := T.INF_VALUE;
  {%H-}Tour.Length := Succ(Len);
  for K := 0 to Pred(Len) do
    begin
      Unvisit.InitRange(Len);
      Tour[0] := K;
      Unvisit[K] := False;
      Curr := K;
      I := 1;
      while Unvisit.NonEmpty do
        begin
          MinCost := T.INF_VALUE;
          for J in Unvisit do
            begin
              CurrCost := m[Curr, J];
              if CurrCost < MinCost then
                begin
                  MinCost := CurrCost;
                  Next := J;
                end;
            end;
          Curr := Next;
          Tour[I] := Next;
          Unvisit[Next] := False;
          Inc(I);
        end;
      Tour[I] := K;
      TotalCost := GetTotalCost(m, Tour);
      if aOnReady <> nil then
        aOnReady(m, Tour, TotalCost);
      if TotalCost < aCost then
        begin
          aCost := TotalCost;
          Result := System.Copy(Tour);
        end;
    end;
end;

class function TGTspHelper.GetMatrixState(const m: TTspMatrix; out aIsSymm: Boolean): TTspMatrixState;
var
  I, J, Size: SizeInt;
begin
  Size := System.Length(m);
  if Size < 2 then  // trivial
    exit(tmsTrivial);
  for I := 0 to Pred(Size) do
    if System.Length(m[I]) <> Size then // non square
      exit(tmsNonSquare);
  aIsSymm := True;
  for I := 0 to Pred(Size) do
    for J := 0 to Pred(Size) do
      if I <> J then
        begin
          if m[I, J] < T(0) then // negative element
            exit(tmsNegElement);
          if I > J then
            aIsSymm := aIsSymm and (m[I, J] = m[J, I]);
        end;
  Result := tmsProper;
end;

class function TGTspHelper.GetTotalCost(const m: TTspMatrix; const aTour: TIntArray): T;
var
  I: SizeInt;
begin
  Result := T(0);
  for I := 0 to Pred(System.High(aTour)) do
    Result += m[aTour[I], aTour[Succ(I)]];
end;

class function TGTspHelper.FindGreedyFast(const m: TTspMatrix; out aCost: T): TIntArray;
var
  Symm: Boolean;
begin
  Symm := CheckMatrixProper(m);
  Result := GreedyFInsTsp(m, nil, aCost);
  if Symm then
    Ls2Opt(m, Result, aCost);
end;

class function TGTspHelper.FindGreedyFastNn(const m: TTspMatrix; out aCost: T): TIntArray;
var
  Symm: Boolean;
begin
  Symm := CheckMatrixProper(m);
  Result := GreedyNearNeighb(m, nil, aCost);
  NormalizeTour(0, Result);
  if Symm then
    Ls2Opt(m, Result, aCost);
end;

class function TGTspHelper.FindGreedy2Opt(const m: TTspMatrix; out aCost: T): TIntArray;
begin
  if CheckMatrixProper(m) then
    begin
      Result := GreedyNearNeighb(m, @Ls2Opt, aCost);
      NormalizeTour(0, Result);
      Ls3OptPath(m, Result, aCost);
    end
  else
    begin
      Result := GreedyNearNeighb(m, nil, aCost);
      NormalizeTour(0, Result);
    end;
end;

class function TGTspHelper.FindGreedy3Opt(const m: TTspMatrix; out aCost: T): TIntArray;
begin
  if CheckMatrixProper(m) then
    begin
      Result := GreedyFInsTsp(m, nil, aCost);
      Ls3OptPath(m, Result, aCost);
    end
  else
    begin
      Result := GreedyNearNeighb(m, nil, aCost);
      NormalizeTour(0, Result);
    end;
end;

class function TGTspHelper.FindSlowGreedy3Opt(const m: TTspMatrix; out aCost: T): TIntArray;
begin
  if CheckMatrixProper(m) then
    Result := GreedyFInsTsp(m, @Ls3OptEdges, aCost)
  else
    begin
      Result := GreedyNearNeighb(m, nil, aCost);
      NormalizeTour(0, Result);
    end;
end;

{ TGMetricTspHelper }

class function TGTspHelper.FindExact(const m: TTspMatrix; out aTour: TIntArray; out aCost: T;
  aTimeOut: Integer): Boolean;
var
  Helper: TBbTsp;
  Symm: Boolean;
begin
  Symm := CheckMatrixProper(m);
  Helper.IsMetric := False;
  if Symm then
    begin
      aTour := GreedyFInsTsp(m, nil, aCost);
      Ls3OptEdges(m, aTour, aCost);
      Result := Helper.Execute(m, aTimeOut, aTour, aCost);
      if not Result then
        Ls3OptPath(m, aTour, aCost);
    end
  else
    begin
      aTour := GreedyNearNeighb(m, nil, aCost);
      NormalizeTour(0, aTour);
      Result := Helper.Execute(m, aTimeOut, aTour, aCost);
    end;
end;

class function TGTspHelper.FindApprox(const m: TTspMatrix; Accuracy: Double; out aTour: TIntArray; out aCost: T;
  aTimeOut: Integer): Boolean;
var
  Helper: TApproxBbTsp;
  Symm: Boolean;
begin
  Symm := CheckMatrixProper(m);
  Helper.IsMetric := False;
  if Symm then
    begin
      aTour := GreedyFInsTsp(m, nil, aCost);
      Ls3OptEdges(m, aTour, aCost);
      Result := Helper.Execute(m, Accuracy, aTimeOut, aTour, aCost);
      if not Result then
        Ls3OptPath(m, aTour, aCost);
    end
  else
    begin
      aTour := GreedyNearNeighb(m, nil, aCost);
      NormalizeTour(0, aTour);
      Result := Helper.Execute(m, Accuracy, aTimeOut, aTour, aCost);
    end;
end;

{ TGMetricTspHelper }

class function TGMetricTspHelper.FindExact(const m: TTspMatrix; out aTour: TIntArray; out aCost: T;
  aTimeOut: Integer): Boolean;
var
  Helper: TBbTsp;
  Symm: Boolean;
begin
  Symm := CheckMatrixProper(m);
  Helper.IsMetric := True;
  if Symm then
    begin
      aTour := GreedyFInsTsp(m, nil, aCost);
      Ls3OptEdges(m, aTour, aCost);
      Result := Helper.Execute(m, aTimeOut, aTour, aCost);
      if not Result then
        Ls3OptPath(m, aTour, aCost);
    end
  else
    begin
      aTour := GreedyNearNeighb(m, nil, aCost);
      NormalizeTour(0, aTour);
      Result := Helper.Execute(m, aTimeOut, aTour, aCost);
    end;
end;

class function TGMetricTspHelper.FindApprox(const m: TTspMatrix; Accuracy: Double; out aTour: TIntArray; out
  aCost: T; aTimeOut: Integer): Boolean;
var
  Helper: TApproxBbTsp;
  Symm: Boolean;
begin
  Symm := CheckMatrixProper(m);
  Helper.IsMetric := True;
  if Symm then
    begin
      aTour := GreedyFInsTsp(m, nil, aCost);
      Ls3OptEdges(m, aTour, aCost);
      Result := Helper.Execute(m, Accuracy, aTimeOut, aTour, aCost);
      if not Result then
        Ls3OptPath(m, aTour, aCost);
    end
  else
    begin
      aTour := GreedyNearNeighb(m, nil, aCost);
      NormalizeTour(0, aTour);
      Result := Helper.Execute(m, Accuracy, aTimeOut, aTour, aCost);
    end;
end;

{ TGPoint2D }

class function TGPoint2D.Equal(constref L, R: TGPoint2D): Boolean;
begin
  Result := (L.X = R.X) and (L.Y = R.Y);
end;

class function TGPoint2D.HashCode(constref aPoint: TGPoint2D): SizeInt;
begin
  Result := TxxHash32LE.HashBuf(@aPoint, SizeOf(aPoint));
end;

constructor TGPoint2D.Create(aX, aY: T);
begin
  X := aX;
  Y := aY;
end;

function TGPoint2D.Distance(constref aPoint: TGPoint2D): ValReal;
begin
  Result := Sqrt((ValReal(aPoint.X) - ValReal(X)) * (ValReal(aPoint.X) - ValReal(X)) +
                 (ValReal(aPoint.Y) - ValReal(Y)) * (ValReal(aPoint.Y) - ValReal(Y)));
end;

{ TGPoint3D }

class function TGPoint3D.Equal(constref L, R: TGPoint3D): Boolean;
begin
  Result := (L.X = R.X) and (L.Y = R.Y) and (L.Z = R.Z);
end;

class function TGPoint3D.HashCode(constref aPoint: TGPoint3D): SizeInt;
begin
  Result := TxxHash32LE.HashBuf(@aPoint, SizeOf(aPoint));
end;

constructor TGPoint3D.Create(aX, aY, aZ: T);
begin
  X := aX;
  Y := aY;
  Z := aZ;
end;

function TGPoint3D.Distance(constref aPoint: TGPoint3D): ValReal;
begin
  Result := Sqrt((ValReal(aPoint.X) - ValReal(X)) * (ValReal(aPoint.X) - ValReal(X)) +
                 (ValReal(aPoint.Y) - ValReal(Y)) * (ValReal(aPoint.Y) - ValReal(Y)) +
                 (ValReal(aPoint.Z) - ValReal(Z)) * (ValReal(aPoint.Z) - ValReal(Z)));
end;

{ TIntArrayHelper }

function TIntArrayHelper.GetLenght: SizeInt;
begin
  Result := System.Length(Self);
end;

procedure TIntArrayHelper.SetLength(aValue: SizeInt);
begin
  System.SetLength(Self, aValue);
end;

class function TIntArrayHelper.Construct(aLength: SizeInt; aInitValue: SizeInt): TIntArray;
begin
  System.SetLength(Result, aLength);
{$IF DEFINED(CPU64)}
  System.FillQWord(Pointer(Result)^, aLength, QWord(aInitValue));
{$ELSEIF DEFINED(CPU32)}
  System.FillDWord(Pointer(Result)^, aLength, DWord(aInitValue));
{$ELSE}
  System.FillWord(Pointer(Result)^, aLength, Word(aInitValue));
{$ENDIF}
end;

function TIntArrayHelper.IsEmpty: Boolean;
begin
  Result := Length = 0;
end;

function TIntArrayHelper.Copy: TIntArray;
begin
  Result := System.Copy(Self);
end;

{ TGSimpleWeight }

constructor TGSimpleWeight.Create(aValue: T);
begin
  Weight := aValue;
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

{ TIntValue }

constructor TIntValue.Create(aValue: SizeInt);
begin
  Value := aValue;
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

{ TOrdIntPair }

class function TOrdIntPair.HashCode(constref aValue: TOrdIntPair): SizeInt;
begin
{$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  {$IF DEFINED (CPU64)}
    Result := TxxHash32LE.HashGuid(TGuid(aValue));
  {$ELSEIF DEFINED (CPU32)}
    Result := TxxHash32LE.HashQWord(QWord(aValue));
  {$ELSE }
    Result := TxxHash32LE.HashDWord(DWord(aValue));
  {$ENDIF }
{$ElSE FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := TxxHash32LE.HashBuf(@aValue, SizeOf(aValue));
{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
end;

class function TOrdIntPair.Equal(constref L, R: TOrdIntPair): Boolean;
begin
  Result := (L.Left = R.Left) and (L.Right = R.Right);
end;

constructor TOrdIntPair.Create(L, R: SizeInt);
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

function TOrdIntPair.Key: TOrdIntPair;
begin
  Result := Self;
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
  Result := FTable.Find(TOrdIntPair.Create(L, R), Dummy) <> nil;
end;

function TIntPairSet.Add(L, R: SizeInt): Boolean;
var
  Dummy: SizeInt;
  p: POrdIntPair;
  v: TOrdIntPair;
begin
  v := TOrdIntPair.Create(L, R);
  Result := not FTable.FindOrAdd(v, p, Dummy);
  if Result then
    p^ := v;
end;

function TIntPairSet.Remove(L, R: SizeInt): Boolean;
begin
  Result := FTable.Remove(TOrdIntPair.Create(L, R));
end;

{ TIntNode }

class operator TIntNode. = (constref L, R: TIntNode): Boolean;
begin
  Result := L.Data = R.Data;
end;

class operator TIntNode.<>(constref L, R: TIntNode): Boolean;
begin
  Result := L.Data <> R.Data;
end;

class operator TIntNode.>(constref L, R: TIntNode): Boolean;
begin
  Result := L.Data > R.Data;
end;

class operator TIntNode.<(constref L, R: TIntNode): Boolean;
begin
  Result := L.Data < R.Data;
end;

class operator TIntNode.>=(constref L, R: TIntNode): Boolean;
begin
  Result := L.Data >= R.Data;
end;

class operator TIntNode.<=(constref L, R: TIntNode): Boolean;
begin
  Result := L.Data <= R.Data;
end;

constructor TIntNode.Create(aIndex, aData: SizeInt);
begin
  Index := aIndex;
  Data := aData;
end;

{ TGBinHeapMin }

function TGBinHeapMin.GetCapacity: SizeInt;
begin
  Result := System.Length(FIndex2Handle);
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
  FCount := 0;
  if aSize > 0 then
    begin
      System.SetLength(FHandle2Index, aSize);
      System.SetLength(FHeap, aSize);
      System.SetLength(FIndex2Handle, aSize);
    end;
end;

procedure TGBinHeapMin.MakeEmpty;
begin
  FCount := 0;
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

procedure TGBinHeapMin.Enqueue(aHandle: SizeInt; constref aValue: T);
var
  InsertIdx: SizeInt;
begin
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

function TGBinHeapMin.HeadPtr(aHandle: SizeInt): PItem;
begin
  Result := @FHeap[FHandle2Index[aHandle]];
end;

{ TGPairHeapMin.TNode }

function TGPairHeapMin.TNode.AddChild(aNode: PNode): PNode;
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

{ TGPairHeapMin }

function TGPairHeapMin.GetCapacity: SizeInt;
begin
  Result := System.Length(FNodeList);
end;

function TGPairHeapMin.NewNode(constref aValue: T; aHandle: SizeInt): PNode;
begin
  Result := @FNodeList[aHandle];
  Inc(FCount);
  Result^.Data := aValue;
  Result^.Prev := nil;
  Result^.Child := nil;
  Result^.Sibling := nil;
end;

function TGPairHeapMin.DequeueItem: T;
begin
  Result := FRoot^.Data;
  Dec(FCount);
  FRoot := TwoPassMerge(FRoot^.Child);
  if FRoot <> nil then
    FRoot^.Prev := nil;
end;

procedure TGPairHeapMin.RootMerge(aNode: PNode);
begin
  FRoot := NodeMerge(FRoot, aNode);
  if FRoot <> nil then
    FRoot^.Prev := nil;
end;

procedure TGPairHeapMin.ExtractNode(aNode: PNode);
begin
  if aNode <> FRoot then
    begin
      CutNode(aNode);
      RootMerge(TwoPassMerge(aNode^.Child));
    end
  else
    begin
      FRoot := TwoPassMerge(FRoot^.Child);
      if FRoot <> nil then
        FRoot^.Prev := nil;
    end;
  Dec(FCount);
end;

class function TGPairHeapMin.NodeMerge(L, R: PNode): PNode;
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

class function TGPairHeapMin.TwoPassMerge(aNode: PNode): PNode;
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

class procedure TGPairHeapMin.CutNode(aNode: PNode);
begin
  if aNode^.Sibling <> nil then
    aNode^.Sibling^.Prev := aNode^.Prev;
  if aNode^.Prev^.Child = aNode then
    aNode^.Prev^.Child := aNode^.Sibling
  else
    aNode^.Prev^.Sibling := aNode^.Sibling;
  aNode^.Sibling := nil;
end;

constructor TGPairHeapMin.Create(aSize: SizeInt);
begin
  System.SetLength(FNodeList, aSize);
  MakeEmpty;
end;

procedure TGPairHeapMin.MakeEmpty;
begin
  FRoot := nil;
  FCount := 0;
end;

function TGPairHeapMin.TryDequeue(out aValue: T): Boolean;
begin
  Result := Count <> 0;
  if Result then
    aValue := DequeueItem;
end;

function TGPairHeapMin.Dequeue: T;
begin
  if Count > 0 then
    Result := DequeueItem
  else
    raise ELGAccessEmpty.Create(SECantAccessEmpty);
end;

procedure TGPairHeapMin.Enqueue(aHandle: SizeInt; constref aValue: T);
begin
  RootMerge(NewNode(aValue, aHandle));
end;

procedure TGPairHeapMin.Update(aHandle: SizeInt; constref aNewValue: T);
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

procedure TGPairHeapMin.Remove(aHandle: SizeInt);
begin
  ExtractNode(@FNodeList[aHandle]);
end;

function TGPairHeapMin.Peek(aHandle: SizeInt): T;
begin
  Result := FNodeList[aHandle].Data;
end;

function TGPairHeapMin.HeadPtr(aHandle: SizeInt): PItem;
begin
  Result := @FNodeList[aHandle].Data;
end;

{ TGPairHeapMax.TNode }

function TGPairHeapMax.TNode.AddChild(aNode: PNode): PNode;
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

{ TGPairHeapMax }

function TGPairHeapMax.GetCapacity: SizeInt;
begin
  Result := System.Length(FNodeList);
end;

function TGPairHeapMax.NewNode(constref aValue: T; aHandle: SizeInt): PNode;
begin
  Result := @FNodeList[aHandle];
  Inc(FCount);
  Result^.Data := aValue;
  Result^.Prev := nil;
  Result^.Child := nil;
  Result^.Sibling := nil;
end;

function TGPairHeapMax.DequeueItem: T;
begin
  Result := FRoot^.Data;
  Dec(FCount);
  FRoot := TwoPassMerge(FRoot^.Child);
  if FRoot <> nil then
    FRoot^.Prev := nil;
end;

procedure TGPairHeapMax.RootMerge(aNode: PNode);
begin
  FRoot := NodeMerge(FRoot, aNode);
  if FRoot <> nil then
    FRoot^.Prev := nil;
end;

class function TGPairHeapMax.NodeMerge(L, R: PNode): PNode;
begin
  if L <> nil then
    if R <> nil then
      if L^.Data >= R^.Data then
        Result := L^.AddChild(R)
      else
        Result := R^.AddChild(L)
    else
      Result := L
  else
    Result := R;
end;

class function TGPairHeapMax.TwoPassMerge(aNode: PNode): PNode;
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

class procedure TGPairHeapMax.CutNode(aNode: PNode);
begin
  if aNode^.Sibling <> nil then
    aNode^.Sibling^.Prev := aNode^.Prev;
  if aNode^.Prev^.Child = aNode then
    aNode^.Prev^.Child := aNode^.Sibling
  else
    aNode^.Prev^.Sibling := aNode^.Sibling;
  aNode^.Sibling := nil;
end;

constructor TGPairHeapMax.Create(aSize: SizeInt);
begin
  System.SetLength(FNodeList, aSize);
  MakeEmpty;
end;

procedure TGPairHeapMax.MakeEmpty;
begin
  FRoot := nil;
  FCount := 0;
end;

function TGPairHeapMax.TryDequeue(out aValue: T): Boolean;
begin
  Result := Count <> 0;
  if Result then
    aValue := DequeueItem;
end;

function TGPairHeapMax.Dequeue: T;
begin
  if Count > 0 then
    Result := DequeueItem
  else
    raise ELGAccessEmpty.Create(SECantAccessEmpty);
end;

procedure TGPairHeapMax.Enqueue(aHandle: SizeInt; constref aValue: T);
begin
  RootMerge(NewNode(aValue, aHandle));
end;

procedure TGPairHeapMax.Update(aHandle: SizeInt; constref aNewValue: T);
var
  Node: PNode;
begin
  Node := @FNodeList[aHandle];
  if aNewValue > Node^.Data then
    begin
      Node^.Data := aNewValue;
      if Node <> FRoot then
        begin
          CutNode(Node);
          RootMerge(Node);
        end;
    end;
end;

function TGPairHeapMax.Peek(aHandle: SizeInt): T;
begin
  Result := FNodeList[aHandle].Data;
end;

function TGPairHeapMax.HeadPtr(aHandle: SizeInt): PItem;
begin
  Result := @FNodeList[aHandle].Data;
end;

{ TGJoinableHashList }

function TGJoinableHashList.GetCount: SizeInt;
begin
  Result := FTable.Count;
end;

function TGJoinableHashList.GetEnumerator: TEnumerator;
begin
  Result := FTable.GetEnumerator;
end;

procedure TGJoinableHashList.EnsureCapacity(aValue: SizeInt);
begin
  FTable.EnsureCapacity(aValue);
end;

procedure TGJoinableHashList.Add(constref aValue: TEntry);
var
  p: PEntry;
begin
  if FTable.FindOrAdd(aValue.Key, p) then
    p^.Weight += aValue.Weight
  else
    p^ := aValue;
end;

procedure TGJoinableHashList.AddAll(constref aList: TGJoinableHashList);
var
  p: PEntry;
begin
  for p in aList do
    Add(p^);
end;

procedure TGJoinableHashList.Remove(aValue: SizeInt);
begin
  FTable.Remove(aValue);
end;

{ TSimpleStack }

function TSimpleStack.GetCapacity: SizeInt;
begin
  Result := System.Length(Items);
end;

function TSimpleStack.GetCount: SizeInt;
begin
  Result := Succ(Top);
end;

constructor TSimpleStack.Create(aSize: SizeInt);
begin
  System.SetLength(Items, aSize);
  Top := NULL_INDEX;
end;

function TSimpleStack.ToArray: TIntArray;
begin
  Result := System.Copy(Items, 0, Count);;
end;

function TSimpleStack.IsEmpty: Boolean;
begin
  Result := Top < 0;
end;

function TSimpleStack.NonEmpty: Boolean;
begin
  Result := Top >= 0;
end;

procedure TSimpleStack.MakeEmpty;
begin
  Top := NULL_INDEX;
end;

procedure TSimpleStack.Push(aValue: SizeInt);
begin
  Inc(Top);
  Items[Top] := aValue;
end;

function TSimpleStack.Pop: SizeInt;
begin
  Result := Items[Top];
  Dec(Top);
end;

function TSimpleStack.TryPop(out aValue: SizeInt): Boolean;
begin
  Result := Top >= 0;
  if Result then
    aValue := Pop;
end;

function TSimpleStack.Peek: SizeInt;
begin
  Result := Items[Top];
end;

function TSimpleStack.TryPeek(out aValue: SizeInt): Boolean;
begin
  Result := Top >= 0;
  if Result then
    aValue := Peek;
end;

{ TGWeightHelper.TWeightEdge }

class operator TGWeightHelper.TWeightEdge. = (constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight = R.Weight;
end;

class operator TGWeightHelper.TWeightEdge.<>(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight <> R.Weight;
end;

class operator TGWeightHelper.TWeightEdge.>(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight > R.Weight;
end;

class operator TGWeightHelper.TWeightEdge.<(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight < R.Weight;
end;

class operator TGWeightHelper.TWeightEdge.>=(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight >= R.Weight;
end;

class operator TGWeightHelper.TWeightEdge.<=(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight <= R.Weight;
end;

constructor TGWeightHelper.TWeightEdge.Create(s, d: SizeInt; w: TWeight);
begin
  Source := s;
  Destination := d;
  Weight := w;
end;

function TGWeightHelper.TWeightEdge.Edge: TIntEdge;
begin
  Result := TIntEdge.Create(Source, Destination);
end;

{ TCostItem }

class operator TCostItem. = (constref L, R: TCostItem): Boolean;
begin
  Result := L.Cost = R.Cost;
end;

class operator TCostItem.<>(constref L, R: TCostItem): Boolean;
begin
  Result := L.Cost <> R.Cost;
end;

class operator TCostItem.>(constref L, R: TCostItem): Boolean;
begin
  Result := L.Cost > R.Cost;
end;

class operator TCostItem.<(constref L, R: TCostItem): Boolean;
begin
  Result := L.Cost < R.Cost;
end;

class operator TCostItem.>=(constref L, R: TCostItem): Boolean;
begin
  Result := L.Cost >= R.Cost;
end;

class operator TCostItem.<=(constref L, R: TCostItem): Boolean;
begin
  Result := L.Cost <= R.Cost;
end;

constructor TCostItem.Create(aIndex: SizeInt; aCost: TCost);
begin
  Index := aIndex;
  Cost := aCost;
end;

{ TGWeightHelper.TWeightItem }

class operator TGWeightHelper.TWeightItem. = (constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight = R.Weight;
end;

class operator TGWeightHelper.TWeightItem.<>(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight <> R.Weight;
end;

class operator TGWeightHelper.TWeightItem.>(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight > R.Weight;
end;

class operator TGWeightHelper.TWeightItem.<(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight < R.Weight;
end;

class operator TGWeightHelper.TWeightItem.>=(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight >= R.Weight;
end;

class operator TGWeightHelper.TWeightItem.<=(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight <= R.Weight;
end;

constructor TGWeightHelper.TWeightItem.Create(aIndex: SizeInt; w: TWeight);
begin
  Index := aIndex;
  Weight := w;
end;

{ TGWeightHelper.TRankItem }

class operator TGWeightHelper.TRankItem. = (constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank = R.Rank;
end;

class operator TGWeightHelper.TRankItem.<>(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank <> R.Rank;
end;

class operator TGWeightHelper.TRankItem.>(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank > R.Rank;
end;

class operator TGWeightHelper.TRankItem.<(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank < R.Rank;
end;

class operator TGWeightHelper.TRankItem.>=(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank >= R.Rank;
end;

class operator TGWeightHelper.TRankItem.<=(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank <= R.Rank;
end;

constructor TGWeightHelper.TRankItem.Create(aIndex: SizeInt; aRank, aWeight: TWeight);
begin
  Index := aIndex;
  Rank := aRank;
  Weight := aWeight;
end;

{ TGWeightHelper.TApspCell }

constructor TGWeightHelper.TApspCell.Create(aWeight: TWeight; aSource: SizeInt);
begin
  Weight := aWeight;
  Predecessor := aSource;
end;

{ TGWeightHelper.THungarian }

procedure TGWeightHelper.THungarian.Match(aNode, aMate: SizeInt);
begin
  FMates[aNode] := aMate;
  FMates[aMate] := aNode;
end;

procedure TGWeightHelper.THungarian.Init(aGraph: TGraph; const w, g: TIntArray);
var
  I: SizeInt;
  ew: TWeight;
  p: TGraph.PAdjItem;
begin
  FGraph := aGraph;
  FMatchCount := 0;
  FWhites.Size := aGraph.VertexCount;
  if w.Length <= g.Length then
    for I in w do
      FWhites[I] := True
  else
    for I in g do
      FWhites[I] := True;

  FPhi := CreateWeightArrayZ(aGraph.VertexCount);
  for I in FWhites do
    begin
      ew := TWeight.INF_VALUE;
      for p in aGraph.AdjLists[I]^ do
        if p^.Data.Weight < ew then
          ew := p^.Data.Weight;
      FPhi[I] := ew;
    end;

  FMates := aGraph.CreateIntArray;
  FParents := aGraph.CreateIntArray;
  FQueue := aGraph.CreateIntArray;
  FVisited.Size := aGraph.VertexCount;
end;

procedure TGWeightHelper.THungarian.InitMax(aGraph: TGraph; const w, g: TIntArray);
var
  I: SizeInt;
  ew: TWeight;
  p: TGraph.PAdjItem;
begin
  FGraph := aGraph;
  FMatchCount := 0;
  FWhites.Size := aGraph.VertexCount;
  if w.Length <= g.Length then
    for I in w do
      FWhites[I] := True
  else
    for I in g do
      FWhites[I] := True;

  FPhi := CreateWeightArrayZ(aGraph.VertexCount);
  for I in FWhites do
    begin
      ew := TWeight.NEGINF_VALUE;
      for p in aGraph.AdjLists[I]^ do
        if p^.Data.Weight > ew then
          ew := p^.Data.Weight;
      FPhi[I] := ew;
    end;

  FMates := aGraph.CreateIntArray;
  FParents := aGraph.CreateIntArray;
  FQueue := aGraph.CreateIntArray;
  FVisited.Size := aGraph.VertexCount;
end;
{$PUSH}{$MACRO ON}
{$DEFINE EnqueueNext :=
begin
  FParents[Next] := Curr;
  FQueue[qTail] := Next;
  Inc(qTail);
end
}
function TGWeightHelper.THungarian.FindAugmentPathMin(aRoot: SizeInt; var aDelta: TWeight): SizeInt;
var
  Curr, Next: SizeInt;
  CurrPhi, Cost: TWeight;
  p: TGraph.PAdjItem;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  FQueue[qTail] := aRoot;
  Inc(qTail);
  while qHead < qTail do
    begin
      Curr := FQueue[qHead];
      Inc(qHead);
      FVisited[Curr] := True;
      CurrPhi := FPhi[Curr];
      if FWhites[Curr] then
        for p in FGraph.AdjLists[Curr]^ do
          begin
            Next := p^.Destination;
            if (FMates[Curr] = Next) or (FParents[Next] <> NULL_INDEX) then
              continue;
            Cost := p^.Data.Weight + FPhi[Next] - CurrPhi;
            if Cost = 0 then
              begin
                if FMates[Next] = NULL_INDEX then
                  begin
                    FParents[Next] := Curr;
                    exit(Next);
                  end
                else
                  if not FVisited[Next] then
                    EnqueueNext;
              end
            else
              if Cost < aDelta then
                aDelta := Cost;
          end
      else
        begin
          Next := FMates[Curr];
          EnqueueNext;
        end;
    end;
  Result := NULL_INDEX;
end;

function TGWeightHelper.THungarian.FindAugmentPathMax(aRoot: SizeInt; var aDelta: TWeight): SizeInt;
var
  Curr, Next: SizeInt;
  CurrPhi, Cost: TWeight;
  p: TGraph.PAdjItem;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  FQueue[qTail] := aRoot;
  Inc(qTail);
  while qHead < qTail do
    begin
      Curr := FQueue[qHead];
      Inc(qHead);
      FVisited[Curr] := True;
      CurrPhi := FPhi[Curr];
      if FWhites[Curr] then
        for p in FGraph.AdjLists[Curr]^ do
          begin
            Next := p^.Destination;
            if (FMates[Curr] = Next) or (FParents[Next] <> NULL_INDEX) then
              continue;
            Cost := p^.Data.Weight + FPhi[Next] - CurrPhi;
            if Cost = 0 then
              begin
                if FMates[Next] = NULL_INDEX then
                  begin
                    FParents[Next] := Curr;
                    exit(Next);
                  end
                else
                  if not FVisited[Next] then
                    EnqueueNext;
              end
            else
              if Cost > aDelta then
                aDelta := Cost;
          end
      else
        begin
          Next := FMates[Curr];
          EnqueueNext;
        end;
    end;
  Result := NULL_INDEX;
end;
{$UNDEF EnqueueNext}{$POP}
procedure TGWeightHelper.THungarian.AlternatePath(aRoot: SizeInt);
var
  Mate, Next: SizeInt;
begin
  repeat
    Mate := FParents[aRoot];
    Next := FMates[Mate];
    Match(aRoot, Mate);
    aRoot := Next;
  until aRoot = NULL_INDEX;
end;

function TGWeightHelper.THungarian.TryAugmentMin(var aDelta: TWeight): SizeInt;
var
  I, Last: SizeInt;
begin
  aDelta := TWeight.INF_VALUE;
  Result := 0;
  System.FillChar(Pointer(FParents)^, FParents.Length * SizeOf(SizeInt), $ff);
  FVisited.ClearBits;
  for I in FWhites do
    if FMates[I] = NULL_INDEX then
      begin
        Last := FindAugmentPathMin(I, aDelta);
        if Last <> NULL_INDEX then
          begin
            AlternatePath(Last);
            Inc(Result);
          end;
      end;
end;

function TGWeightHelper.THungarian.TryAugmentMax(var aDelta: TWeight): SizeInt;
var
  I, Last: SizeInt;
begin
  aDelta := TWeight.NEGINF_VALUE;
  Result := 0;
  System.FillChar(Pointer(FParents)^, FParents.Length * SizeOf(SizeInt), $ff);
  FVisited.ClearBits;
  for I in FWhites do
    if FMates[I] = NULL_INDEX then
      begin
        Last := FindAugmentPathMax(I, aDelta);
        if Last <> NULL_INDEX then
          begin
            AlternatePath(Last);
            Inc(Result);
          end;
      end;
end;

procedure TGWeightHelper.THungarian.CorrectPhi(aDelta: TWeight);
var
  I: SizeInt;
begin
  for I in FVisited do
    FPhi[I] += aDelta;
end;

procedure TGWeightHelper.THungarian.ExecuteMin;
var
  Count: SizeInt;
  Delta: TWeight;
begin
  Delta := TWeight.INF_VALUE;
  repeat
    repeat
      Count := TryAugmentMin(Delta);
      FMatchCount += Count;
    until Count = 0;
    if not (Delta < TWeight.INF_VALUE) then
      break;
    CorrectPhi(Delta);
  until False;
end;

procedure TGWeightHelper.THungarian.ExecuteMax;
var
  Count: SizeInt;
  Delta: TWeight;
begin
  Delta := TWeight.NEGINF_VALUE;
  repeat
    repeat
      Count := TryAugmentMax(Delta);
      FMatchCount += Count;
    until Count = 0;
    if not (Delta > TWeight.NEGINF_VALUE) then
      break;
    CorrectPhi(Delta);
  until False;
end;

function TGWeightHelper.THungarian.CreateEdges: TEdgeArray;
var
  I, J, Mate: SizeInt;
begin
  System.SetLength(Result, FMatchCount);
  J := 0;
  for I in FWhites do
    begin
      Mate := FMates[I];
      if Mate <> NULL_INDEX then
        begin
          Result[J] := TWeightEdge.Create(I, Mate, FGraph.AdjLists[I]^.Find(Mate)^.Data.Weight);
          Inc(J);
        end;
    end;
end;

function TGWeightHelper.THungarian.MinWeightMatching(aGraph: TGraph; const w, g: TIntArray): TEdgeArray;
begin
  Init(aGraph, w, g);
  ExecuteMin;
  Result := CreateEdges;
end;

function TGWeightHelper.THungarian.MaxWeightMatching(aGraph: TGraph; const w, g: TIntArray): TEdgeArray;
begin
  InitMax(aGraph, w, g);
  ExecuteMax;
  Result := CreateEdges;
end;

{ TGWeightHelper.TBfmt.TArc }

constructor TGWeightHelper.TBfmt.TArc.Create(aTarget: PNode; aWeight: TWeight);
begin
  Target := aTarget;
  Weight := aWeight;
end;

{ TGWeightHelper.TBfmt }

procedure TGWeightHelper.TBfmt.CopyGraph(aDirected: Boolean);
var
  CurrArcIdx: TIntArray;
  I, J: SizeInt;
  p: TGraph.PAdjItem;
begin
  System.SetLength(CurrArcIdx, FNodeCount);
  J := 0;
  for I := 0 to System.High(CurrArcIdx) do
    begin
      CurrArcIdx[I] := J;
      J += FGraph.AdjLists[I]^.Count;
    end;

  System.SetLength(Nodes, Succ(FNodeCount));
  if aDirected then
    System.SetLength(FArcs, Succ(FGraph.EdgeCount))
  else
    System.SetLength(FArcs, Succ(FGraph.EdgeCount * 2));

  for I := 0 to Pred(FNodeCount) do
    Nodes[I].FirstArc := @FArcs[CurrArcIdx[I]];

  for I := 0 to Pred(FNodeCount) do
    for p in FGraph.AdjLists[I]^ do
      begin
        FArcs[CurrArcIdx[I]] := TArc.Create(@Nodes[p^.Key], p^.Data.Weight);
        Inc(CurrArcIdx[I]);
      end;
  CurrArcIdx := nil;

  FArcs[System.High(FArcs)] :=
    TArc.Create(@Nodes[FNodeCount], 0);
  //sentinel node
  Nodes[FNodeCount].FirstArc := @FArcs[System.High(FArcs)];
  Nodes[FNodeCount].Weight := 0;
  Nodes[FNodeCount].TreePrev := nil;
  Nodes[FNodeCount].TreeNext := nil;
  Nodes[FNodeCount].Parent := nil;
  Nodes[FNodeCount].Level := NULL_INDEX;
end;

procedure TGWeightHelper.TBfmt.SsspInit(aSrc: SizeInt);
var
  I: SizeInt;
begin
  for I := 0 to Pred(FNodeCount) do
    with Nodes[I] do
      begin
        Weight := TWeight.INF_VALUE;
        TreePrev := nil;
        TreeNext := nil;
        Parent := nil;
        Level := NULL_INDEX;
      end;
  Nodes[aSrc].Weight := 0;
  Nodes[aSrc].TreePrev := @Nodes[aSrc];
  Nodes[aSrc].TreeNext := @Nodes[aSrc];
  Nodes[aSrc].Parent := @Nodes[aSrc];
end;

constructor TGWeightHelper.TBfmt.Create(aGraph: TGraph; aDirected: Boolean);
begin
  FGraph := aGraph;
  FNodeCount := aGraph.VertexCount;
  CopyGraph(aDirected);
  System.SetLength(FQueue, FNodeCount);
  FInQueue.Size := FNodeCount;
  FActive.Size := FNodeCount;
end;

function TGWeightHelper.TBfmt.IndexOf(aNode: PNode): SizeInt;
begin
  Result := aNode - PNode(Nodes);
end;

procedure TGWeightHelper.TBfmt.Sssp(aSrc: SizeInt);
var
  CurrNode, NextNode, PrevNode, PostNode, TestNode: PNode;
  CurrArc: PArc;
  NodeCount, I, Level: SizeInt;
  CurrWeight: TWeight;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  NodeCount := FNodeCount;
  SsspInit(aSrc);
  FActive[aSrc] := True;
  FQueue[qTail] := @Nodes[aSrc];
  Inc(qTail);
  while qHead <> qTail do
    begin
      CurrNode := FQueue[qHead];
      Inc(qHead);
      if qHead = NodeCount then
        qHead := 0;
      I := IndexOf(CurrNode);
      FInQueue[I] := False;
      if not FActive[I] then
        continue;
      FActive[I] := False;
      CurrArc := CurrNode^.FirstArc;
      CurrWeight := CurrNode^.Weight;
      while CurrArc < (CurrNode + 1)^.FirstArc do
        begin
          NextNode := CurrArc^.Target;
          if NextNode^.Weight > CurrWeight + CurrArc^.Weight then
            begin
              NextNode^.Weight := CurrWeight + CurrArc^.Weight;
              if NextNode^.TreePrev <> nil then
                begin
                  PrevNode := NextNode^.TreePrev;
                  TestNode := NextNode;
                  Level := 0;
                  repeat
                    Level += TestNode^.Level;
                    TestNode^.TreePrev := nil;
                    TestNode^.Level := NULL_INDEX;
                    FActive[IndexOf(TestNode)] := False;
                    TestNode := TestNode^.TreeNext;
                  until Level < 0;
                  Dec(NextNode^.Parent^.Level);
                  PrevNode^.TreeNext := TestNode;
                  TestNode^.TreePrev := PrevNode;
                end;
              NextNode^.Parent := CurrNode;
              Inc(CurrNode^.Level);
              PostNode := CurrNode^.TreeNext;
              CurrNode^.TreeNext := NextNode;
              NextNode^.TreePrev := CurrNode;
              NextNode^.TreeNext := PostNode;
              PostNode^.TreePrev := NextNode;
              I := IndexOf(NextNode);
              if not FInQueue[I] then
                begin
                  FQueue[qTail] := NextNode;
                  Inc(qTail);
                  if qTail = NodeCount then
                    qTail := 0;
                  FInQueue[I] := True;
                end;
              FActive[I] := True;
            end;
          Inc(CurrArc);
        end;
    end;
end;

{ TGWeightHelper }

class function TGWeightHelper.CreateAndFill(aValue: TWeight; aSize: SizeInt): TWeightArray;
var
  I: SizeInt;
begin
  System.SetLength(Result, aSize);
  for I := 0 to Pred(aSize) do
    Result[I] := aValue;
end;

class procedure TGWeightHelper.Fill(var a: TWeightArray; aValue: TWeight);
var
  I: SizeInt;
begin
  for I := 0 to System.High(a) do
    a[I] := aValue;
end;

class function TGWeightHelper.wMax(L, R: TWeight): TWeight;
begin
  if L >= R then
    Result := L
  else
    Result := R;
end;

class function TGWeightHelper.wMin(L, R: TWeight): TWeight;
begin
  if L <= R then
    Result := L
  else
    Result := R;
end;

class function TGWeightHelper.ExtractCycle(aRoot, aLen: SizeInt; constref aTree: TIntArray): TIntArray;
var
  v: TIntVector;
  I, J: SizeInt;
begin
  for I := 1 to aLen do
    aRoot := aTree[aRoot];
  I := aRoot;
  v.Add(aRoot);
  repeat
    I := aTree[I];
    v.Add(I);
  until I = aRoot;
  System.SetLength(Result, v.Count);
  J := 0;
  for I in v.Reverse do
    begin
      Result[J] := I;
      Inc(J);
    end;
end;

class function TGWeightHelper.IsMaxMatching(g: TGraph; const aMatch: TEdgeArray): Boolean;
var
  vFree: TBoolVector;
  e: TWeightEdge;
  I, J, vCount: SizeInt;
begin
  if g.VertexCount < 2 then
    exit(False);
  if System.Length(aMatch) = 0 then
    exit(False);
  vCount := g.VertexCount;
  vFree.InitRange(vCount);
  for e in aMatch do
    begin
      if SizeUInt(e.Source) >= SizeUInt(vCount) then //contains garbage
        exit(False);
      if SizeUInt(e.Destination) >= SizeUInt(vCount) then //contains garbage
        exit(False);
      if e.Source = e.Destination then //contains garbage
        exit(False);
      if not g.AdjLists[e.Source]^.Contains(e.Destination) then //contains garbage
        exit(False);
      if not vFree[e.Source] then  //contains adjacent edges -> not matching
        exit(False);
      vFree[e.Source] := False;
      if not vFree[e.Destination] then  //contains adjacent edges -> not matching
        exit(False);
      vFree[e.Destination] := False;
    end;
  for I in vFree do
    for J in g.AdjVerticesI(I) do
      if vFree[J] then  // is not maximal
        exit(False);
  Result := True;
end;

class function TGWeightHelper.IsPerfectMatching(g: TGraph; const aMatch: TEdgeArray): Boolean;
var
  vFree: TBoolVector;
  e: TWeightEdge;
  vCount: SizeInt;
begin
  if (g.VertexCount < 2) or Odd(g.VertexCount) then
    exit(False);
  if System.Length(aMatch) <> g.VertexCount div 2 then
    exit(False);
  vCount := g.VertexCount;
  vFree.InitRange(vCount);
  for e in aMatch do
    begin
      if SizeUInt(e.Source) >= SizeUInt(vCount) then //contains garbage
        exit(False);
      if SizeUInt(e.Destination) >= SizeUInt(vCount) then //contains garbage
        exit(False);
      if e.Source = e.Destination then //contains garbage
        exit(False);
      if not g.AdjLists[e.Source]^.Contains(e.Destination) then //contains garbage
        exit(False);
      if not vFree[e.Source] then  //contains adjacent edges -> not matching
        exit(False);
      vFree[e.Source] := False;
      if not vFree[e.Destination] then  //contains adjacent edges -> not matching
        exit(False);
      vFree[e.Destination] := False;
    end;
  Result := vFree.IsEmpty;
end;

class function TGWeightHelper.DijkstraSssp(g: TGraph; aSrc: SizeInt): TWeightArray;
var
  Queue: TPairHeap;
  Reached,
  InQueue: TGraph.TBitVector;
  Item: TWeightItem;
  p: TGraph.PAdjItem;
begin
  Result := CreateWeightArray(g.VertexCount);
  Queue := TPairHeap.Create(g.VertexCount);
  Reached.Size := g.VertexCount;
  InQueue.Size := g.VertexCount;
  Item := TWeightItem.Create(aSrc, 0);
  repeat
    Result[Item.Index] := Item.Weight;
    Reached[Item.Index] := True;
    for p in g.AdjLists[Item.Index]^ do
      if not Reached[p^.Key] then
        if not InQueue[p^.Key] then
          begin
            Queue.Enqueue(p^.Key, TWeightItem.Create(p^.Key, p^.Data.Weight + Item.Weight));
            InQueue[p^.Key] := True;
          end
        else
          if p^.Data.Weight + Item.Weight < Queue.HeadPtr(p^.Key)^.Weight then
            Queue.Update(p^.Key, TWeightItem.Create(p^.Key, p^.Data.Weight + Item.Weight));
  until not Queue.TryDequeue(Item);
end;

class function TGWeightHelper.DijkstraSssp(g: TGraph; aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
var
  Queue: TPairHeap;
  Reached,
  InQueue: TGraph.TBitVector;
  Item: TWeightItem;
  p: TGraph.PAdjItem;
begin
  Result := CreateWeightArray(g.VertexCount);
  Queue := TPairHeap.Create(g.VertexCount);
  aPathTree := g.CreateIntArray;
  Reached.Size := g.VertexCount;
  InQueue.Size := g.VertexCount;
  Item := TWeightItem.Create(aSrc, 0);
  repeat
    Result[Item.Index] := Item.Weight;
    Reached[Item.Index] := True;
    for p in g.AdjLists[Item.Index]^ do
      if not Reached[p^.Key] then
        if not InQueue[p^.Key] then
          begin
            Queue.Enqueue(p^.Key, TWeightItem.Create(p^.Key, p^.Data.Weight + Item.Weight));
            aPathTree[p^.Key] := Item.Index;
            InQueue[p^.Key] := True;
          end
        else
          if p^.Data.Weight + Item.Weight < Queue.HeadPtr(p^.Key)^.Weight then
            begin
              Queue.Update(p^.Key, TWeightItem.Create(p^.Key, p^.Data.Weight + Item.Weight));
              aPathTree[p^.Key] := Item.Index;
            end;
  until not Queue.TryDequeue(Item);
end;

class function TGWeightHelper.DijkstraPath(g: TGraph; aSrc, aDst: SizeInt): TWeight;
var
  Queue: TBinHeap;
  Reached,
  InQueue: TGraph.TBitVector;
  Item: TWeightItem;
  p: TGraph.PAdjItem;
begin
  Queue := TBinHeap.Create(g.VertexCount);
  Reached.Size := g.VertexCount;
  InQueue.Size := g.VertexCount;
  Item := TWeightItem.Create(aSrc, 0);
  repeat
    if Item.Index = aDst then
      exit(Item.Weight);
    Reached[Item.Index] := True;
    for p in g.AdjLists[Item.Index]^ do
      if not Reached[p^.Key] then
        if not InQueue[p^.Key] then
          begin
            Queue.Enqueue(p^.Key, TWeightItem.Create(p^.Key, p^.Data.Weight + Item.Weight));
            InQueue[p^.Key] := True;
          end
        else
          if p^.Data.Weight + Item.Weight < Queue.HeadPtr(p^.Key)^.Weight then
            Queue.Update(p^.Key, TWeightItem.Create(p^.Key, p^.Data.Weight + Item.Weight));
  until not Queue.TryDequeue(Item);
  Result := TWeight.INF_VALUE;
end;

class function TGWeightHelper.DijkstraPath(g: TGraph; aSrc, aDst: SizeInt; out aWeight: TWeight): TIntArray;
var
  Queue: TBinHeap;
  Parents: TIntArray;
  Reached,
  InQueue: TGraph.TBitVector;
  Item: TWeightItem;
  p: TGraph.PAdjItem;
begin
  Queue := TBinHeap.Create(g.VertexCount);
  Parents := g.CreateIntArray;
  Reached.Size := g.VertexCount;
  InQueue.Size := g.VertexCount;
  Item := TWeightItem.Create(aSrc, 0);
  repeat
    if Item.Index = aDst then
      begin
        aWeight := Item.Weight;
        exit(g.TreePathTo(Parents, aDst));
      end;
    Reached[Item.Index] := True;
    for p in g.AdjLists[Item.Index]^ do
      if not Reached[p^.Key] then
        if not InQueue[p^.Key] then
          begin
            Queue.Enqueue(p^.Key, TWeightItem.Create(p^.Key, p^.Data.Weight + Item.Weight));
            Parents[p^.Key] := Item.Index;
            InQueue[p^.Key] := True;
          end
        else
          if p^.Data.Weight + Item.Weight < Queue.HeadPtr(p^.Key)^.Weight then
            begin
              Queue.Update(p^.Key, TWeightItem.Create(p^.Key, p^.Data.Weight + Item.Weight));
              Parents[p^.Key] := Item.Index;
            end;
  until not Queue.TryDequeue(Item);
  aWeight := TWeight.INF_VALUE;
  Result := [];
end;

class function TGWeightHelper.AStar(g: TGraph; aSrc, aDst: SizeInt; out aWeight: TWeight;
  aEst: TEstimate): TIntArray;
var
  Queue: TAStarHeap;
  Parents: TIntArray;
  Reached,
  InQueue: TGraph.TBitVector;
  Item: TRankItem;
  Relax: TWeight;
  p: TGraph.PAdjItem;
begin
  Queue := TAStarHeap.Create(g.VertexCount);
  Parents := g.CreateIntArray;
  Reached.Size := g.VertexCount;
  InQueue.Size := g.VertexCount;
  Item := TRankItem.Create(aSrc, aEst(g.Items[aSrc], g.Items[aDst]), 0);
  repeat
    if {%H-}Item.Index = aDst then
      begin
        aWeight := Item.Weight;
        exit(g.TreePathTo(Parents, aDst));
      end;
    Reached[Item.Index] := True;
    for p in g.AdjLists[Item.Index]^ do
      if not Reached[p^.Key] then
        begin
          Relax := p^.Data.Weight + Item.Weight;
          if not InQueue[p^.Key] then
            begin
              Queue.Enqueue(p^.Key, TRankItem.Create(
                p^.Key, Relax + aEst(g.Items[p^.Key], g.Items[aDst]), Relax));
              Parents[p^.Key] := Item.Index;
              InQueue[p^.Key] := True;
            end
          else
            if Relax < Queue.HeadPtr(p^.Key)^.Weight then
              begin
                Queue.Update(p^.Key, TRankItem.Create(
                  p^.Key, Relax + aEst(g.Items[p^.Key], g.Items[aDst]), Relax));
                Parents[p^.Key] := Item.Index;
              end;
        end;
  until not Queue.TryDequeue(Item);
  aWeight := TWeight.INF_VALUE;
  Result := [];
end;

class function TGWeightHelper.SpfaBase(g: TGraph; aSrc: SizeInt; out aTree: TIntArray;
  out aWeights: TWeightArray): SizeInt;
var
  Queue: TIntDeque;
  Dist: TIntArray;
  InQueue: TGraph.TBitVector;
  Curr, Next, Top, d, VertCount: SizeInt;
  p: TGraph.PAdjItem;
begin
  VertCount := g.VertexCount;
  aWeights := CreateWeightArray(VertCount);
  Dist := g.CreateIntArray;
  aTree := g.CreateIntArray;
  {%H-}Queue.EnsureCapacity(VertCount);
  InQueue.Size := VertCount;
  aWeights[aSrc] := 0;
  Dist[aSrc] := 0;
  Curr := aSrc;
  repeat
    InQueue[Curr] := False;
    if (aTree[Curr] <> NULL_INDEX) and InQueue[aTree[Curr]] then
      continue;
    d := Succ(Dist[Curr]);
    for p in g.AdjLists[Curr]^ do
      begin
        Next := p^.Destination;
        if aWeights[Curr] + p^.Data.Weight < aWeights[Next] then
          //todo: need some kind of protection from overflow ?
          begin
            aWeights[Next] := aWeights[Curr] + p^.Data.Weight;
            aTree[Next] := Curr;
            if (Next = aSrc) or (d >= VertCount) then
              exit(Next);
            Dist[Next] := d;
            if not InQueue[Next] then
              begin
                if Queue.TryPeekFirst(Top) and (aWeights[Next] < aWeights[{%H-}Top]) then
                  Queue.PushFirst(Next)
                else
                  Queue.PushLast(Next);
                InQueue[Next] := True;
              end;
          end;
      end;
  until not Queue{%H-}.TryPopFirst(Curr);
  Result := NULL_INDEX;
end;

class function TGWeightHelper.Spfa2Base(g: TGraph; aSrc: SizeInt; out aTree: TIntArray;
  out aWeights: TWeightArray): SizeInt;
var
  v1, v2: TBoolVector;
  Dist: TIntArray;
  Curr, Next, VertCount, d: SizeInt;
  CurrPass, NextPass: ^TBoolVector;
  p: TGraph.PAdjItem;
begin
  VertCount := g.VertexCount;
  aWeights := CreateWeightArray(VertCount);
  Dist := g.CreateIntArray;
  aTree := g.CreateIntArray;
  v1.Size := VertCount;
  v2.Size := VertCount;
  aWeights[aSrc] := 0;
  v2[aSrc] := True;
  Dist[aSrc] := 0;
  CurrPass := @v1;
  NextPass := @v2;
  repeat
    p := Pointer(CurrPass);
    CurrPass := NextPass;
    NextPass := Pointer(p);
    for Curr in CurrPass^ do
      begin
        CurrPass^[Curr] := False;
        d := Succ(Dist[Curr]);
        for p in g.AdjLists[Curr]^ do
          begin
            Next := p^.Destination;
            if aWeights[Curr] + p^.Data.Weight < aWeights[Next] then
              begin
                aWeights[Next] := aWeights[Curr] + p^.Data.Weight;
                aTree[Next] := Curr;
                if (Next = aSrc) or (d >= VertCount) then
                  exit(Next);
                Dist[Next] := d;
                NextPass^[Next] := True;
              end;
          end;
      end;
  until NextPass^.IsEmpty;
  Result := NULL_INDEX;
end;

class function TGWeightHelper.BfmtBase(g: TGraph; aSrc: SizeInt; out aParents: TIntArray;
  out aWeights: TWeightArray): SizeInt;
var
  Queue, TreePrev, TreeNext, Level: TIntArray;
  InQueue, Active: TGraph.TBitVector;
  Curr, Next, Prev, Post, Test, CurrLevel, vCount: SizeInt;
  CurrWeight: TWeight;
  p: TGraph.PAdjItem;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  vCount := g.VertexCount;
  Queue.Length := vCount;
  aParents := g.CreateIntArray;
  TreePrev := g.CreateIntArray;
  TreeNext := g.CreateIntArray;
  Level := g.CreateIntArray;
  InQueue.Size := vCount;
  Active.Size := vCount;
  aWeights := CreateWeightArray(vCount);
  aWeights[aSrc] := 0;
  aParents[aSrc] := aSrc;
  TreePrev[aSrc] := aSrc;
  TreeNext[aSrc] := aSrc;
  Active[aSrc] := True;
  Queue[qTail] := aSrc;
  Inc(qTail);
  while qHead <> qTail do
    begin
      Curr := Queue[qHead];
      Inc(qHead);
      if qHead = vCount then
        qHead := 0;
      InQueue[Curr] := False;
      if not Active[Curr] then
        continue;
      Active[Curr] := False;
      CurrWeight := aWeights[Curr];
      for p in g.AdjLists[Curr]^ do
        begin
          Next := p^.Destination;
          if aWeights[Next] > CurrWeight + p^.Data.Weight then
            begin
              aWeights[Next] := CurrWeight + p^.Data.Weight;
              if TreePrev[Next] <> NULL_INDEX then
                begin
                  Prev := TreePrev[Next];
                  Test := Next;
                  CurrLevel := 0;
                  repeat
                    if Test = Curr then
                      begin
                        aParents[Next] := Curr;
                        exit(Next);
                      end;
                    CurrLevel += Level[Test];
                    TreePrev[Test] := NULL_INDEX;
                    Level[Test] := NULL_INDEX;
                    Active[Test] := False;
                    Test := TreeNext[Test];
                  until CurrLevel < 0;
                  Dec(Level[aParents[Next]]);
                  TreeNext[Prev] := Test;
                  TreePrev[Test] := Prev;
                end;
              aParents[Next] := Curr;
              Inc(Level[Curr]);
              Post := TreeNext[Curr];
              TreeNext[Curr] := Next;
              TreePrev[Next] := Curr;
              TreeNext[Next] := Post;
              TreePrev[Post] := Next;
              if not InQueue[Next] then
                begin
                  Queue[qTail] := Next;
                  Inc(qTail);
                  if qTail = vCount then
                    qTail := 0;
                  InQueue[Next] := True;
                end;
              Active[Next] := True;
            end;
        end;
    end;
  aParents[aSrc] := NULL_INDEX;
  Result := NULL_INDEX;
end;

class function TGWeightHelper.BfmtReweight(g: TGraph; out aWeights: TWeightArray): SizeInt;
var
  Queue, Parents, TreePrev, TreeNext, Level: TIntArray;
  InQueue, Active: TGraph.TBitVector;
  Curr, Next, Prev, Post, Test, CurrLevel, vCount: SizeInt;
  CurrWeight: TWeight;
  p: TGraph.PAdjItem;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  vCount := Succ(g.VertexCount);
  Test := g.VertexCount;
  Queue.Length := vCount;
  Parents := g.CreateIntArray(vCount, NULL_INDEX);
  TreePrev := g.CreateIntArray(vCount, NULL_INDEX);
  TreeNext := g.CreateIntArray(vCount, NULL_INDEX);
  Level := g.CreateIntArray(vCount, NULL_INDEX);
  InQueue.Size := vCount;
  Active.Size := vCount;
  aWeights := CreateWeightArrayZ(vCount);
  Parents[Test] := Test;
  TreePrev[Test] := Test;
  TreeNext[Test] := Test;
  for Curr := 0 to Pred(Test) do
    begin
      Parents[Curr] := Pred(vCount);
      TreePrev[Curr] := Pred(vCount);
      InQueue[Curr] := True;
      Active[Curr] := True;
      Queue[qTail] := Curr;
      Inc(qTail);
    end;
  while qHead <> qTail do
    begin
      Curr := Queue[qHead];
      Inc(qHead);
      if qHead = vCount then
        qHead := 0;
      InQueue[Curr] := False;
      if not Active[Curr] then
        continue;
      Active[Curr] := False;
      CurrWeight := aWeights[Curr];
      for p in g.AdjLists[Curr]^ do
        begin
          Next := p^.Destination;
          if aWeights[Next] > CurrWeight + p^.Data.Weight then
            begin
              aWeights[Next] := CurrWeight + p^.Data.Weight;
              if TreePrev[Next] <> NULL_INDEX then
                begin
                  Prev := TreePrev[Next];
                  Test := Next;
                  CurrLevel := 0;
                  repeat
                    if Test = Curr then
                      begin
                        aWeights := nil;
                        exit(Next);
                      end;
                    CurrLevel += Level[Test];
                    TreePrev[Test] := NULL_INDEX;
                    Level[Test] := NULL_INDEX;
                    Active[Test] := False;
                    Test := TreeNext[Test];
                  until CurrLevel < 0;
                  Dec(Level[Parents[Next]]);
                  TreeNext[Prev] := Test;
                  TreePrev[Test] := Prev;
                end;
              Parents[Next] := Curr;
              Inc(Level[Curr]);
              Post := TreeNext[Curr];
              TreeNext[Curr] := Next;
              TreePrev[Next] := Curr;
              TreeNext[Next] := Post;
              TreePrev[Post] := Next;
              if not InQueue[Next] then
                begin
                  Queue[qTail] := Next;
                  Inc(qTail);
                  if qTail = vCount then
                    qTail := 0;
                  InQueue[Next] := True;
                end;
              Active[Next] := True;
            end;
        end;
    end;
  System.SetLength(aWeights, Pred(vCount));
  Result := NULL_INDEX;
end;

class function TGWeightHelper.NegCycleDetect(g: TGraph; aSrc: SizeInt): TIntArray;
var
  Parents: TIntArray;
  Weights: TWeightArray;
  Cycle: SizeInt;
begin
  Cycle := BfmtBase(g, aSrc, Parents, Weights);
  if Cycle <> NULL_INDEX then
    Result := ExtractCycle(Cycle, g.VertexCount, Parents)
  else
    Result := nil;
end;

class function TGWeightHelper.BfmtSssp(g: TGraph; aSrc: SizeInt; out aWeights: TWeightArray): Boolean;
var
  Parents: TIntArray;
begin
  Result := BfmtBase(g, aSrc, Parents, aWeights) = NULL_INDEX;
  if not Result then
    aWeights := nil;
end;

class function TGWeightHelper.BfmtSssp(g: TGraph; aSrc: SizeInt; out aPaths: TIntArray;
  out aWeights: TWeightArray): Boolean;
var
  Cycle: SizeInt;
begin
  Cycle := BfmtBase(g, aSrc, aPaths, aWeights);
  Result := Cycle = NULL_INDEX;
  if not Result then
    begin
      aWeights := nil;
      aPaths := ExtractCycle(Cycle, g.VertexCount, aPaths);
    end;
end;

class function TGWeightHelper.BfmtPath(g: TGraph; aSrc, aDst: SizeInt; out aPath: TIntArray;
  out aWeight: TWeight): Boolean;
var
  Weights: TWeightArray;
begin
  aWeight := TWeight.INF_VALUE;
  if BfmtSssp(g, aSrc, aPath, Weights) then
    begin
      Result := aPath[aDst] <> NULL_INDEX;
      if Result then
        begin
          aWeight := Weights[aDst];
          aPath := g.TreePathTo(aPath, aDst);
        end
      else
        aWeight := TWeight.INF_VALUE;
    end
  else
    begin
      Result := False;
      aWeight := 0;
    end;
end;

class function TGWeightHelper.FloydApsp(aGraph: TGraph; out aPaths: TApspMatrix): Boolean;
var
  I, J, K: SizeInt;
  L, R, W: TWeight;
begin
  aPaths := CreateAPSPMatrix(aGraph);
  for K := 0 to Pred(aGraph.VertexCount) do
    for I := 0 to Pred(aGraph.VertexCount) do
      for J := 0 to Pred(aGraph.VertexCount) do
        begin
          L := aPaths[I, K].Weight;
          R := aPaths[K, J].Weight;
          if (L < TWeight.INF_VALUE) and (R < TWeight.INF_VALUE) then
            begin
              W := L + R;
              if W < aPaths[I, J].Weight then
                if I <> J then
                  begin
                    aPaths[I, J].Weight := W;
                    aPaths[I, J].Predecessor := aPaths[K, J].Predecessor;
                  end
                else
                  begin
                    aPaths := [[TApspCell.Create(0, aPaths[K, J].Predecessor)]]; /////////////
                    exit(False);
                  end;
            end;
        end;
  Result := True;
end;

class function TGWeightHelper.JohnsonApsp(aGraph: TGraph; out aPaths: TApspMatrix): Boolean;
var
  Queue: TPairHeap;
  Parents: TIntArray;
  Phi, Weights: TWeightArray;
  Reached, InQueue: TGraph.TBitVector;
  Item: TWeightItem;
  Relax: TWeight;
  I, J, VertCount: SizeInt;
  p: TGraph.PAdjItem;
begin
  I := BfmtReweight(aGraph, Phi);
  if I >= 0 then
    begin
      aPaths := [[TApspCell.Create(0, I)]];
      exit(False);
    end;
  VertCount := aGraph.VertexCount;
  Parents.Length := VertCount;
  System.SetLength(Weights, VertCount);
  Queue := TPairHeap.Create(VertCount);
  Reached.Size := VertCount;
  InQueue.Size := VertCount;
  System.SetLength(aPaths, VertCount, VertCount);
  for I := 0 to Pred(VertCount) do
    begin
      System.FillChar(Pointer(Parents)^, VertCount * SizeOf(SizeInt), $ff);
      Fill(Weights, TWeight.INF_VALUE);
      Item := TWeightItem.Create(I, 0);
      Parents[I] := I;
      repeat
        Weights[Item.Index] := Item.Weight;
        Reached[Item.Index] := True;
        InQueue[Item.Index] := False;
        for p in aGraph.AdjLists[Item.Index]^ do
          begin
            Relax := Item.Weight + p^.Data.Weight + Phi[Item.Index] - Phi[p^.Key];
            if not Reached[p^.Key] then
              if not InQueue[p^.Key] then
                begin
                  Queue.Enqueue(p^.Key, TWeightItem.Create(p^.Key, Relax));
                  Parents[p^.Key] := Item.Index;
                  InQueue[p^.Key] := True;
                end
              else
                if Relax < Queue.HeadPtr(p^.Key)^.Weight then
                  begin
                    Queue.Update(p^.Key, TWeightItem.Create(p^.Key, Relax));
                    Parents[p^.Key] := Item.Index;
                  end;
          end;
      until not Queue.TryDequeue(Item);
      for J := 0 to Pred(VertCount) do
        aPaths[I, J] := TApspCell.Create(Weights[J] + Phi[J] - Phi[I], Parents[J]);
      Reached.ClearBits;
    end;
  Result := True;
end;

class function TGWeightHelper.BfmtApsp(aGraph: TGraph; aDirect: Boolean; out aPaths: TApspMatrix): Boolean;
var
  Bfmt: TBfmt;
  Weights: TWeightArray;
  I, J, VertCount: SizeInt;
begin
  I := BfmtReweight(aGraph, Weights);
  if I >= 0 then
    begin
      aPaths := [[TApspCell.Create(0, I)]];
      exit(False);
    end;
  Weights := nil;
  VertCount := aGraph.VertexCount;
  Bfmt := TBfmt.Create(aGraph, aDirect);
  System.SetLength(aPaths, VertCount, VertCount);
  for I := 0 to Pred(VertCount) do
    begin
      Bfmt.Sssp(I);
      with Bfmt do
        for J := 0 to Pred(VertCount) do
          aPaths[I, J] := TApspCell.Create(Nodes[J].Weight, IndexOf(Nodes[J].Parent));
    end;
  Result := True;
end;

class function TGWeightHelper.CreateWeightArray(aLen: SizeInt): TWeightArray;
begin
  Result := CreateAndFill(TWeight.INF_VALUE, aLen);
end;

class function TGWeightHelper.CreateWeightArrayNI(aLen: SizeInt): TWeightArray;
begin
  Result := CreateAndFill(TWeight.NEGINF_VALUE, aLen);
end;

class function TGWeightHelper.CreateWeightArrayZ(aLen: SizeInt): TWeightArray;
begin
  Result := CreateAndFill(0, aLen);
end;

class procedure TGWeightHelper.ResizeAndFill(var a: TWeightArray; aLen: SizeInt; aValue: TWeight);
var
  I: SizeInt;
begin
  System.SetLength(a, aLen);
  for I := 0 to Pred(aLen) do
    a[I] := aValue;
end;

class function TGWeightHelper.CreateWeightsMatrix(aGraph: TGraph): TWeightMatrix;
var
  Empties: TBoolVector;
  I, J, VertCount: SizeInt;
  p: TGraph.PAdjItem;
begin
  VertCount := aGraph.VertexCount;
  System.SetLength(Result, VertCount, VertCount);
  for I := 0 to Pred(VertCount) do
    begin
      Empties.InitRange(VertCount);
      Result[I, I] := 0;
      Empties[I] := False;
      for p in aGraph.AdjLists[I]^ do
        begin
          Result[I, p^.Key] := p^.Data.Weight;
          Empties[p^.Key] := False;
        end;
      for J in Empties do
        Result[I, J] := TWeight.INF_VALUE;
    end;
end;

class function TGWeightHelper.CreateAPSPMatrix(aGraph: TGraph): TApspMatrix;
var
  Empties: TBoolVector;
  I, J, VertCount: SizeInt;
  p: TGraph.PAdjItem;
begin
  VertCount := aGraph.VertexCount;
  System.SetLength(Result, VertCount, VertCount);
  for I := 0 to Pred(VertCount) do
    begin
      Empties.InitRange(VertCount);
      Result[I, I] := TApspCell.Create(0, I);
      Empties[I] := False;
      for p in aGraph.AdjLists[I]^ do
        begin
          Result[I, p^.Key] := TApspCell.Create(p^.Data.Weight, I);
          Empties[p^.Key] := False;
        end;
      for J in Empties do
        Result[I, J] := TApspCell.Create(TWeight.INF_VALUE, I);
    end;
end;

class function TGWeightHelper.ExtractMinPath(aSrc, aDst: SizeInt; const aMatrix: TApspMatrix): TIntArray;
var
  Stack: TIntStack;
begin
  if aMatrix[aSrc, aDst].Weight < TWeight.INF_VALUE then
    repeat
      {%H-}Stack.Push(aDst);
      aDst := aMatrix[aSrc, aDst].Predecessor;
    until aDst = aSrc;
  Stack.Push(aDst);
  Result{%H-}.Length := Stack.Count;
  aDst := 0;
  for aSrc in Stack.Reverse do
    begin
      Result[aDst] := aSrc;
      Inc(aDst);
    end;
end;

class function TGWeightHelper.MinWeightMatchingB(aGraph: TGraph; const w, g: TIntArray): TEdgeArray;
var
  Helper: THungarian;
begin
  Result := Helper.MinWeightMatching(aGraph, w, g);
end;

class function TGWeightHelper.MaxWeightMatchingB(aGraph: TGraph; const w, g: TIntArray): TEdgeArray;
var
  Helper: THungarian;
begin
  Result := Helper.MaxWeightMatching(aGraph, w, g);
end;

end.

