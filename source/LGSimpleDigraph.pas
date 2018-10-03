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
unit LGSimpleDigraph;

{$mode objfpc}{$H+}
{$INLINE ON}{$WARN 6058 off : }
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, math,
  LGUtils,
  {%H-}LGHelpers,
  LGQueue,
  LGDeque,
  LGVector,
  LGHashMap,
  LGCustomGraph,
  LGStrConst;

type
  TSortOrder = LGUtils.TSortOrder;

  { TGSimpleDiGraph implements simple sparse directed graph based on adjacency lists;

      functor TEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGSimpleDiGraph<TVertex, TEdgeData, TEqRel> = class(specialize TGCustomGraph<TVertex, TEdgeData, TEqRel>)
  protected
  type
    TReachabilityMatrix = record
    private
      FMatrix: TSquareBitMatrix;
      FIds: TIntArray;
      function  GetSize: SizeInt; inline;
      procedure Clear; inline;
    public
      constructor Create(constref aMatrix: TSquareBitMatrix; constref aIds: TIntArray);
      function  IsEmpty: Boolean; inline;
      function  Reachable(aSrc, aDst: SizeInt): Boolean; inline;
      property  Size: SizeInt read GetSize;
    end;

  protected
    FReachabilityMatrix: TReachabilityMatrix;
    function  GetReachabilityValid: Boolean; inline;
    function  GetDensity: Double; inline;
    procedure DoRemoveVertex(aIndex: SizeInt);
    function  DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
    function  CreateSkeleton: TSkeleton;
    function  FindCycle(aRoot: SizeInt; out aCycle: TIntArray): Boolean;
    function  CycleExists: Boolean;
    function  TopoSort: TIntArray;
    function  GetDagLongestPaths(aSrc: SizeInt): TIntArray;
    function  GetDagLongestPaths(aSrc: SizeInt; out aTree: TIntArray): TIntArray;
    function  SearchForStrongComponents(out aIds: TIntArray): SizeInt;
    function  GetReachabilityMatrix(constref aScIds: TIntArray; aScCount: SizeInt): TReachabilityMatrix;
  public
{**********************************************************************************************************
  class management utilities
***********************************************************************************************************}

    procedure Clear; override;
    function  Clone: TGSimpleDiGraph;
    function  Reverse: TGSimpleDiGraph;
  { saves graph in its own binary format }
    procedure SaveToStream(aStream: TStream; aWriteVertex: TOnWriteVertex; aWriteData: TOnWriteData);
    procedure LoadFromStream(aStream: TStream; aReadVertex: TOnReadVertex; aReadData: TOnReadData);
    procedure SaveToFile(const aFileName: string; aWriteVertex: TOnWriteVertex; aWriteData: TOnWriteData);
    procedure LoadFromFile(const aFileName: string; aReadVertex: TOnReadVertex; aReadData: TOnReadData);

{**********************************************************************************************************
  structural management utilities
***********************************************************************************************************}

  { returns True and vertex index, if it was added, False otherwise }
    function  AddVertex(constref aVertex: TVertex; out aIndex: SizeInt): Boolean;
    function  AddVertex(constref aVertex: TVertex): Boolean; inline;
    procedure RemoveVertex(constref aVertex: TVertex); inline;
    procedure RemoveVertexI(aIndex: SizeInt);
  { returns True if the edge is added, False, if such an edge already exists }
    function  AddEdge(constref aSrc, aDst: TVertex; aData: TEdgeData): Boolean;
    function  AddEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  AddEdgeI(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  AddEdgeI(aSrc, aDst: SizeInt): Boolean; inline;
    function  RemoveEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  RemoveEdgeI(aSrc, aDst: SizeInt): Boolean;
    function  InDegree(constref aVertex: TVertex): SizeInt; inline;
    function  InDegreeI(aIndex: SizeInt): SizeInt;
    function  OutDegree(constref aVertex: TVertex): SizeInt; inline;
    function  OutDegreeI(aIndex: SizeInt): SizeInt;
    function  Degree(constref aVertex: TVertex): SizeInt; inline;
    function  DegreeI(aIndex: SizeInt): SizeInt;
    function  Isolated(constref aVertex: TVertex): Boolean; inline;
    function  IsolatedI(aIndex: SizeInt): Boolean; inline;
    function  IsSource(constref aVertex: TVertex): Boolean; inline;
    function  IsSourceI(aIndex: SizeInt): Boolean;
    function  IsSink(constref aVertex: TVertex): Boolean; inline;
    function  IsSinkI(aIndex: SizeInt): Boolean;
    function  SourceCount: SizeInt;
    function  SinkCount: SizeInt;
  { checks whether the aDst is reachable from the aSrc(each vertex is reachable from itself) }
    function  PathExists(constref aSrc, aDst: TVertex): Boolean; inline;
    function  PathExistsI(aSrc, aDst: SizeInt): Boolean;
  { checks whether exists any cycle in subgraph that reachable from a aRoot;
    if True then aCycle will contain indices of the vertices of the cycle }
    function  ContainsCycle(constref aRoot: TVertex; out aCycle: TIntArray): Boolean; inline;
    function  ContainsCycleI(aRoot: SizeInt; out aCycle: TIntArray): Boolean;
    function  ContainsEulerianCircuit: Boolean;
    function  FindEulerianCircuit(out aCircuit: TIntVector): Boolean;
  { returns count of the strong connected components; the corresponding element of the
    aCompIds will contain its component index(used Gabow's algorithm) }
    function  FindStrongComponents(out aCompIds: TIntArray): SizeInt;
  { creates internal reachability matrix }
    procedure FillReachabilityMatrix;
  { creates internal reachability matrix using pre-calculated results of FindStrongComponents }
    procedure FillReachabilityMatrix(constref aScIds: TIntArray; aScCount: SizeInt);

{**********************************************************************************************************
  DAG utilities
***********************************************************************************************************}

  { returns array of vertex indices in topological order, without any acyclic checks }
    function  TopologicalSort(aOrder: TSortOrder = soAsc): TIntArray;
    function  IsDag: Boolean;
  { for an acyclic graph returns an array containing in the corresponding components the length of
    the longest path from aSrc to it (in sense 'edges count'), or -1 if it is unreachable from aSrc }
    function  DagLongestPathsMap(constref aSrc: TVertex): TIntArray; inline;
    function  DagLongestPathsMapI(aSrc: SizeInt): TIntArray;
  { same as above and in aPathTree returns paths }
    function  DagLongestPathsMap(constref aSrc: TVertex; out aPathTree: TIntArray): TIntArray; inline;
    function  DagLongestPathsMapI(aSrc: SizeInt; out aPathTree: TIntArray): TIntArray;
  { for an acyclic graph returns an array containing in the corresponding components the length of
    the longest path starting with it(in sense 'edges count') }
    function  DagLongesPaths: TIntArray;

{**********************************************************************************************************
  properties
***********************************************************************************************************}

    property  ReachabilityValid: Boolean read GetReachabilityValid;
    property  Density: Double read GetDensity;
  end;

  { TGWeightedDiGraph implements simple sparse directed weighted graph based on adjacency lists;

      functor TEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean;

      TEdgeData must provide field/property/function Weight: TWeight;

      TWeight must have defined comparision operators and properties MinValue, MaxValue,
      which used as infinity weight values;
      Default(TWeight) used as zero weight value }
  generic TGWeightedDiGraph<TVertex, TWeight, TEdgeData, TEqRel> = class(
     specialize TGSimpleDiGraph<TVertex, TEdgeData, TEqRel>)
  protected
  type
    TPathHelper = specialize TGWeightPathHelper<TVertex, TWeight, TEdgeData, TEqRel>;

  public
  type
    TWeightItem  = TPathHelper.TWeightItem;
    TWeightArray = TPathHelper.TWeightArray;
    TEstimate    = TPathHelper.TEstimate;
    TWeightEdge  = TPathHelper.TWeightEdge;
    TEdgeArray   = array of TWeightEdge;

  protected
    function CreateEdgeArray: TEdgeArray;
    function GetDagMaxPaths(aSrc: SizeInt): TWeightArray;
    function GetDagMaxPaths(aSrc: SizeInt; out aTree: TIntArray): TWeightArray;
  public
{**********************************************************************************************************
  auxiliary utilities
***********************************************************************************************************}

    class function wMin(L, R: TWeight): TWeight; static; inline;
    class function wMax(L, R: TWeight): TWeight; static; inline;
    class function InfWeight: TWeight; static; inline;
    class function NegInfWeight: TWeight; static; inline;
    class function ZeroWeight: TWeight; static; inline;
  { returns True if exists arc with negative weight }
    function ContainsNegWeightEdge: Boolean;
  { checks whether exists any cycle of negative weight in subgraph that reachable from a aRoot;
    if True then aCycle will contain indices of the vertices of the cycle }
    function ContainsNegCycle(constref aRoot: TVertex; out aCycle: TIntArray): Boolean; inline;
    function ContainsNegCycleI(aRootIdx: SizeInt; out aCycle: TIntArray): Boolean;

{**********************************************************************************************************
  class management utilities
***********************************************************************************************************}

    function Clone: TGWeightedDiGraph;
    function Reverse: TGWeightedDiGraph;

{**********************************************************************************************************
  shortest path problem utilities
***********************************************************************************************************}

  { finds all paths of minimal weight from a given vertex to the remaining vertices in the same
    connected component(SSSP), the weights of all edges must be nonnegative;
    the result contains in the corresponding component the weight of the path to the vertex or
    InfWeight if the vertex is unreachable; used Dijkstra's algorithm  }
    function MinPathsMap(constref aSrc: TVertex): TWeightArray; inline;
    function MinPathsMapI(aSrc: SizeInt): TWeightArray;
  { same as above and in aPathTree returns paths }
    function MinPathsMap(constref aSrc: TVertex; out aPathTree: TIntArray): TWeightArray; inline;
    function MinPathsMapI(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
  { finds the path of minimal weight from a aSrc to aDst if it exists(pathfinding);
    the weights of all edges must be nonnegative;
    returns path weight or InfWeight if the vertex is unreachable; used Dijkstra's algorithm  }
    function MinPathWeight(constref aSrc, aDst: TVertex): TWeight; inline;
    function MinPathWeightI(aSrc, aDst: SizeInt): TWeight;
  { returns the vertex path of minimal weight from a aSrc to aDst, if exists, and its weight in aWeight }
    function MinPath(constref aSrc, aDst: TVertex; out aWeight: TWeight): TIntArray; inline;
    function MinPathI(aSrc, aDst: SizeInt; out aWeight: TWeight): TIntArray;
  { finds the path of minimal weight from a aSrc to aDst if it exists;
    the weights of all edges must be nonnegative; used A* algorithm if aEst <> nil }
    function MinPathAStar(constref aSrc, aDst: TVertex; out aWeight: TWeight; aEst: TEstimate): TIntArray; inline;
    function MinPathAStarI(aSrc, aDst: SizeInt; out aWeight: TWeight; aEst: TEstimate): TIntArray;
  { returns False if exists negative weighted cycle, otherwise returns the vertex path
    of minimal weight from a aSrc to aDst in aPath, if exists, and its weight in aWeight;
    to distinguish 'unreachable' and 'negative cycle': in case negative cycle aWeight returns ZeroWeight,
    but InfWeight if aDst unreachable; used BFMT algorithm }
    function FindMinPath(constref aSrc, aDst: TVertex; out aPath: TIntArray; out aWeight: TWeight): Boolean; inline;
    function FindMinPathI(aSrc, aDst: SizeInt; out aPath: TIntArray; out aWeight: TWeight): Boolean;
  { finds all paths of minimal weight from a given vertex to the remaining vertices in the same
    connected component(SSSP), the weights of the edges can be negative;
    returns False and empty aWeights if there is a negative weight cycle, otherwise
    aWeights will contain in the corresponding component the weight of the minimum path to the vertex or
    InfWeight if the vertex is unreachable; used BFMT algorithm  }
    function FindMinPathsMap(constref aSrc: TVertex; out aWeights: TWeightArray): Boolean; inline;
    function FindMinPathsMapI(aSrc: SizeInt; out aWeights: TWeightArray): Boolean;
  { same as above and in aPaths returns paths,
    if there is a negative weight cycle, then aPaths will contain that cycle }
    function FindMinPathsMap(constref aSrc: TVertex; out aPaths: TIntArray; out aWeights: TWeightArray): Boolean; inline;
    function FindMinPathsMapI(aSrc: SizeInt; out aPaths: TIntArray; out aWeights: TWeightArray): Boolean;

{**********************************************************************************************************
  DAG utilities
***********************************************************************************************************}

  { for an acyclic graph returns an array containing in the corresponding components the maximal weight of
    the path from aSrc to it, or NegInfWeight if it is unreachable from aSrc }
    function DagMaxPathsMap(constref aSrc: TVertex): TWeightArray; inline;
    function DagMaxPathsMapI(aSrc: SizeInt): TWeightArray;
  { same as above and in aPathTree returns paths }
    function DagMaxPathsMap(constref aSrc: TVertex; out aPathTree: TIntArray): TWeightArray; inline;
    function DagMaxPathsMapI(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
  { for an acyclic graph returns an array containing in the corresponding components the maximal weight of
    the path starting with it }
    function DagMaxPaths: TWeightArray;
  end;


  { TGIntWeightDiGraph specializes TWeight with Int64 }
  generic TGIntWeightDiGraph<TVertex, TEdgeData, TEqRel> = class(
     specialize TGWeightedDiGraph<TVertex, Int64, TEdgeData, TEqRel>)
  public
  type
    TWeight = Int64;

  protected
  const
    MaxWeight     = High(Int64);
    MinWeight     = Low(Int64);
    MaxCost       = High(TCost);
    MinCost       = Low(TCost);

  type
    TWeightArcMap = specialize TGLiteHashMapLP<TIntEdge, TWeight, TIntEdge>;

    { THPrHelper: an implementation of the high-level push-relabel method;
      B.V. Cherkassky, A.V. Goldberg: On Implementing Push-Relabel Method for the Maximum Flow Problem. }
    THPrHelper = record
    private
    type
      PNode = ^TNode;
      PArc  = ^TArc;

      TArc = record
        Target: PNode;       // pointer to target node
        Reverse: PArc;       // pointer to opposite arc
        ResCap: TWeight;     // residual capacity
        IsForward: Boolean;
        constructor Create(aTarget: PNode; aReverse: PArc; aCap: TWeight);
        constructor CreateReverse(aTarget: PNode; aReverse: PArc);
        function  IsSaturated: Boolean; inline;
        function  IsResidual: Boolean; inline;
        procedure Push(aFlow: TWeight); inline;
      end;

      TNode = record
      private
        function  GetColor: TVertexColor; inline;
        procedure SetColor(aValue: TVertexColor); inline;
      public
        FirstArc,            // pointer to first incident arc
        CurrentArc: PArc;    // pointer to current incident arc
        LevelNext,           // next node in level list
        LevelPrev: PNode;    // previous node in level list
        Excess: TWeight;     // excess at the node
        Distance: SizeInt;   // distance from the sink
        procedure ResetCurrent; inline;
        property  OrderNext: PNode read LevelNext write LevelNext;  // for dfs
        property  Parent: PNode read LevelPrev write LevelPrev;     // for dfs
        property  Color: TVertexColor read GetColor write SetColor; // for dfs
      end;


      TLevel = record
        TopActive,          // head of singly linked list of the nodes with positive excess
        TopIdle: PNode;     // head of doubly linked list of the nodes with zero excess
        function  IsEmpty: Boolean; inline;
        procedure AddActive(aNode: PNode); inline;
        procedure AddIdle(aNode: PNode); inline;
        procedure Activate(aNode: PNode); inline;
        procedure RemoveIdle(aNode: PNode); inline;
        procedure Clear(aLabel: SizeInt);
      end;

    var
      FNodes: array of TNode;
      FArcs: array of TArc;
      FLevels: array of TLevel;
      FCaps: TWeightArray;
      FQueue: array of PNode;
      FSource,
      FSink: PNode;
      FNodeCount,
      FMaxLevel,                // maximal level
      FMaxActiveLevel,          // maximal level with excessed node
      FMinActiveLevel: SizeInt; // minimal level with excessed node
      procedure CreateResudualGraph(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt);
      procedure CreateResudualGraphCap(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; aReqFlow: TWeight);
      procedure ClearLabels; inline;
      procedure GlobalRelabel;
      procedure RemoveGap(aLayer: SizeInt);
      function  Push(aNode: PNode): Boolean;
      procedure Relabel(aNode: PNode);
      procedure HiLevelPushRelabel;
      function  CreateEdges: TEdgeArray;
      function  RecoverFlow: TEdgeArray;
    public
      function  GetMaxFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt): TWeight;
      function  GetMaxFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; out a: TEdgeArray): TWeight;
      function  GetMinCut(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; out s: TIntArray): TWeight;
      function  GetFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; aReqFlow: TWeight;
                out m: TWeightArcMap): TWeight;
    end;

    { TDinitzHelper: implementation of scaling Dinitz's maxflow algorithm }
    TDinitzHelper = record
    private
    type
      PNode = ^TNode;
      PArc  = ^TArc;

      TArc = record
        Target: PNode;       // pointer to target node
        Reverse: PArc;       // pointer to opposite arc
        ResCap: TWeight;     // residual capacity
        IsForward: Boolean;
        constructor Create(aTarget: PNode; aReverse: PArc; aCap: TWeight);
        constructor CreateReverse(aTarget: PNode; aReverse: PArc);
        procedure Push(aFlow: TWeight); inline;
      end;

      TNode = record
      private
        FirstArc,            // pointer to first incident arc
        CurrentArc: PArc;    // pointer to current incident arc
        Distance: SizeInt;   // distance from the source
        procedure ResetCurrent; inline;
      end;

    var
      FNodes: array of TNode;
      FArcs: array of TArc;
      FQueue: array of PNode;
      FSource,
      FSink: PNode;
      FMaxCap: TWeight;
      FScaleFactor,
      FNodeCount: SizeInt;
      procedure CreateResudualGraph(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt);
      procedure ClearLabels; inline;
      function  Relabel: Boolean;
      function  Dfs(aRoot: PNode; aFlow: TWeight): TWeight;
      function  DinitzMaxFlow: TWeight;
      function  CreateEdges(aGraph: TGIntWeightDiGraph): TEdgeArray;
    public
      function  GetMaxFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt): TWeight;
      function  GetMaxFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; out a: TEdgeArray): TWeight;
      function  GetMinCut(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; out s: TIntArray): TWeight;
    end;

    { TBgMcfHelper: simpliest mincost-flow algorithm: shortest augmenting paths(Busacker and Gowen?) }
    TBgMcfHelper = record
    private
    type
      PNode  = ^TNode;
      PArc   = ^TArc;

      TArc = record
        Target: PNode;       // pointer to target node
        Reverse: PArc;       // pointer to opposite arc
        ResCap: TWeight;     // residual capacity
        Cost: TCost;
        IsForward: Boolean;
        constructor Create(aTarget: PNode; aReverse: PArc; aCap: TWeight; aCost: TCost);
        constructor CreateReverse(aTarget: PNode; aReverse: PArc; aCost: TCost);
        function  IsResidual: Boolean; inline;
        procedure Push(aFlow: TWeight); inline;
      end;

      TNode = record
      private
        FirstArc,            // pointer to first incident arc
        PathArc: PArc;       // pointer to incoming path arc
        Parent: PNode;
        Price: TCost;
        PathMinCap: TWeight; // munimum capacity on path
      end;

      TDeque = specialize TGLiteDeque<PNode>;
      THeap  = specialize TGPairHeapMin<TCostItem>;

    var
      FNodes: array of TNode;
      FArcs: array of TArc;
      FQueue: THeap;
      FInQueue: TBitVector;
      FReached: TBoolVector;
      FSource,
      FSink: PNode;
      FRequestFlow: TWeight;
      FNodeCount: SizeInt;
      procedure CreateResudualGraph(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; aReqFlow: TWeight;
                var aCosts: TEdgeCostMap);
      procedure SearchInit; inline;
      function  FindNegCycle(out aMinCap: TWeight): Boolean;
      function  FindShortestPath(out aMinCap: TWeight): Boolean;
      procedure FlowIn(aFlow: TWeight);
      function  MinCostFlow: TWeight;
      function  GetTotalCost: TCost;
      function  CreateEdges(out aTotalCost: TCost): TEdgeArray;
    public
      function  GetMinCostFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; aReqFlow: TWeight;
                var aCosts: TEdgeCostMap; out aTotalCost: TCost): TWeight;
      function  GetMinCostFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; aReqFlow: TWeight;
                var aCosts: TEdgeCostMap; out aTotalCost: TCost; out a: TEdgeArray): TWeight;
    end;

    { TCsMcfHelper: basic implementation of cost scaling mincost-flow algorithm;
        A.V.Goldberg and M.Kharitonov. "On Implementing Scaling Push-Relabel Algorithms for the
        Minimum-Cost Flow Problem";
      todo: need to implement crutial heurictics }
    TCsMcfHelper = record
    private
    type
      PNode  = ^TNode;
      PArc   = ^TArc;

      TArc = record
        Target: PNode;       // pointer to target node
        Reverse: PArc;       // pointer to opposite arc
        ResCap: TWeight;     // residual capacity
        Cost: TCost;
        IsForward: Boolean;
        constructor Create(aTarget: PNode; aReverse: PArc; aCap: TWeight; aCost: TCost);
        constructor CreateReverse(aTarget: PNode; aReverse: PArc; aCost: TCost);
        procedure Push(aFlow: TWeight); inline;
      end;

      TNode = record
      private
        FirstArc,            // pointer to first incident arc
        CurrArc: PArc;       // pointer to current incident arc
        Price: TCost;
        Excess: TWeight;     // excess at the node
        procedure ResetCurrent; inline;
      end;

      TQueue = specialize TGLiteQueue<PNode>;

    var
      FNodes: array of TNode;
      FArcs: array of TArc;
      FQueue: TQueue;
      FInQueue: TBitVector;
      FGraph: TGIntWeightDiGraph;
      FSource,
      FSink: PNode;
      FRequestFlow,
      FResultFlow: TWeight;
      Factor,
      FEpsilon,
      FAlpha: TCost;
      FNodeCount: SizeInt;
      procedure CreateResidualGraph(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; aReqFlow: TWeight;
                var aCosts: TEdgeCostMap);
      function  NegCycleExists: Boolean;
      procedure InitSolution(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; out m: TWeightArcMap);
      function  InitSuccess: Boolean;
      procedure InitPhase;
      procedure Discharge(aNode: PNode);
      procedure Done;
      procedure OptimizeFlow;
      function  GetTotalCost: TCost;
      function  CreateEdges(out aTotalCost: TCost): TEdgeArray;
    public
      function  GetMinCostFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; aReqFlow: TWeight;
                var aCosts: TEdgeCostMap; out aTotalCost: TCost): TWeight;
      function  GetMinCostFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; aReqFlow: TWeight;
                var aCosts: TEdgeCostMap; out aTotalCost: TCost; out a: TEdgeArray): TWeight;
    end;

     function IsCostArrayCorrect(constref aCosts: TCostEdgeArray; out aMap: TEdgeCostMap): Boolean;
  public
{**********************************************************************************************************
  networks utilities treat the weight of the arc as its capacity
***********************************************************************************************************}
  type
    TNetworkState = (nsOk, nsTrivial, nsInvalidSource, nsInvalidSink, nsNegCapacity, nsSinkUnreachable);

    function GetNetworkState(constref aSource, aSink: TVertex): TNetworkState; inline;
    function GetNetworkStateI(aSrcIndex, aSinkIndex: SizeInt): TNetworkState;
  { returns state of the network with aSource as source and aSink as sink;
    returns maximum flow through the network in aFlow, if result = nsOk, 0 otherwise;
    used PR algorithm }
    function FindMaxFlowPr(constref aSource, aSink: TVertex; out aFlow: TWeight): TNetworkState; inline;
    function FindMaxFlowPrI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight): TNetworkState;
  { returns state of network with aSource as source and aSink as sink;
    returns maximum flow through the network in aFlow and flows through the arcs
    in array a, if result = nsOk, 0 and nil otherwise; used PR algorithm }
    function FindMaxFlowPr(constref aSource, aSink: TVertex; out aFlow: TWeight; out a: TEdgeArray): TNetworkState;
             inline;
    function FindMaxFlowPrI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight; out a: TEdgeArray): TNetworkState;
  { returns state of the network with aSource as source and aSink as sink;
    returns maximum flow through the network in aFlow, if result = nsOk, 0 otherwise;
    used Dinitz's algorithm }
    function FindMaxFlowD(constref aSource, aSink: TVertex; out aFlow: TWeight): TNetworkState; inline;
    function FindMaxFlowDI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight): TNetworkState;
  { rreturns state of network with aSource as source and aSink as sink;
    returns maximum flow through the network in aFlow and flows through the arcs
    in array a, if result = nsOk, 0 and nil otherwise; used Dinitz's algorithm }
    function FindMaxFlowD(constref aSource, aSink: TVertex; out aFlow: TWeight; out a: TEdgeArray): TNetworkState;
             inline;
    function FindMaxFlowDI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight; out a: TEdgeArray): TNetworkState;
  {  }
    function IsFeasibleFlow(constref aSource, aSink: TVertex; constref a: TEdgeArray): Boolean;
    function IsFeasibleFlowI(aSrcIndex, aSinkIndex: SizeInt; constref a: TEdgeArray): Boolean;

  type
    //s-t vertex partition
    TStCut = record
      S,
      T: TIntArray;
    end;

  { returns state of the network with aSource as source and aSink as sink;
    returns value of the minimum cut in aValue and vertex partition in aCut,
    if result = nsOk, otherwise 0 and empty partition; used PR algorithm }
    function FindMinSTCutPr(constref aSource, aSink: TVertex; out aValue: TWeight; out aCut: TStCut): TNetworkState;
    function FindMinSTCutPrI(aSrcIndex, aSinkIndex: SizeInt; out aValue: TWeight; out aCut: TStCut): TNetworkState;
  { returns state of the network with aSource as source and aSink as sink;
    returns value of the minimum cut in aValue and vertex partition in aCut,
    if result = nsOk, otherwise 0 and empty partition; used Dinitz's algorithm }
    function FindMinSTCutD(constref aSource, aSink: TVertex; out aValue: TWeight; out aCut: TStCut): TNetworkState;
    function FindMinSTCutDI(aSrcIndex, aSinkIndex: SizeInt; out aValue: TWeight; out aCut: TStCut): TNetworkState;

  { returns True if arc costs are correct(negative costs allows), False otherwise }
    function IsValidCostArray(constref aCosts: TCostEdgeArray): Boolean;
  { param aReqFlow specifies the required flow > 0 (can be MaxWeight);
    returns False if network is not correct or arc costs are not correct or network
    contains negative cycle or aNeedFlow < 1,
    otherwise returns True, flow = min(aReqFlow, maxflow) in aReqFlow and
    total flow cost in aTotalCost }
    function FindMinCostFlow(constref aSource, aSink: TVertex; constref aCosts: TCostEdgeArray;
                             var aReqFlow: TWeight; out aTotalCost: TCost): Boolean; inline;
    function FindMinCostFlowI(aSrcIndex, aSinkIndex: SizeInt; constref aCosts: TCostEdgeArray;
                              var aReqFlow: TWeight; out aTotalCost: TCost): Boolean;
  { same as above and in addition returns flows through the arcs in array a }
    function FindMinCostFlow(constref aSource, aSink: TVertex; constref aCosts: TCostEdgeArray;
                             var aReqFlow: TWeight; out aTotalCost: TCost; out a: TEdgeArray): Boolean; inline;
    function FindMinCostFlowI(aSrcIndex, aSinkIndex: SizeInt; constref aCosts: TCostEdgeArray;
                              var aReqFlow: TWeight; out aTotalCost: TCost; out aArcFlows: TEdgeArray): Boolean;
  { param aReqFlow specifies the required flow > 0 (can be MaxWeight);
    returns False if network is not correct or arc costs are not correct or network
    contains negative cycle or aNeedFlow < 1,
    otherwise returns True, flow = min(aReqFlow, maxflow) in aReqFlow and
    total flow cost in aTotalCost; used cost scaling algorithm }
    function FindMinCostFlowCs(constref aSource, aSink: TVertex; constref aCosts: TCostEdgeArray;
                             var aReqFlow: TWeight; out aTotalCost: TCost): Boolean; inline;
    function FindMinCostFlowCsI(aSrcIndex, aSinkIndex: SizeInt; constref aCosts: TCostEdgeArray;
                              var aReqFlow: TWeight; out aTotalCost: TCost): Boolean;
  { same as above and in addition returns flows through the arcs in array a }
    function FindMinCostFlowCs(constref aSource, aSink: TVertex; constref aCosts: TCostEdgeArray;
                             var aReqFlow: TWeight; out aTotalCost: TCost; out a: TEdgeArray): Boolean; inline;
    function FindMinCostFlowCsI(aSrcIndex, aSinkIndex: SizeInt; constref aCosts: TCostEdgeArray;
                                var aReqFlow: TWeight; out aTotalCost: TCost; out aArcFlows: TEdgeArray): Boolean;
  end;

implementation
{$B-}{$COPERATORS ON}
uses
  bufstream;

{ TGSimpleDiGraph.TReachabilityMatrix }

function TGSimpleDiGraph.TReachabilityMatrix.GetSize: SizeInt;
begin
  Result := FMatrix.Size;
end;

procedure TGSimpleDiGraph.TReachabilityMatrix.Clear;
begin
  if FMatrix.Size > 0 then
    begin
      FMatrix.Clear;
      FIds := nil;
    end;
end;

constructor TGSimpleDiGraph.TReachabilityMatrix.Create(constref aMatrix: TSquareBitMatrix; constref aIds: TIntArray);
begin
  FMatrix := aMatrix;
  FIds := aIds;
end;

function TGSimpleDiGraph.TReachabilityMatrix.IsEmpty: Boolean;
begin
  Result := FMatrix.Size = 0;
end;

function TGSimpleDiGraph.TReachabilityMatrix.Reachable(aSrc, aDst: SizeInt): Boolean;
begin
  Result := FMatrix[FIds[aSrc], FIds[aDst]];
end;

{ TGSimpleDiGraph }

function TGSimpleDiGraph.GetDensity: Double;
begin
  if NonEmpty then
    Result := Double(EdgeCount)/(Double(VertexCount) * Double(Pred(VertexCount)))
  else
    Result := 0.0;
end;

function TGSimpleDiGraph.GetReachabilityValid: Boolean;
begin
  Result := NonEmpty and not FReachabilityMatrix.IsEmpty;
end;

procedure TGSimpleDiGraph.DoRemoveVertex(aIndex: SizeInt);
var
  I, J: SizeInt;
  p: ^TAdjItem;
  CurrEdges: TAdjList.TAdjItemArray;
begin
  FEdgeCount -= FNodeList[aIndex].AdjList.Count;
  for p in FNodeList[aIndex].AdjList do
    Dec(FNodeList[p^.Destination].Tag);
  Delete(aIndex);
  for I := 0 to Pred(VertexCount) do
    begin
      CurrEdges := FNodeList[I].AdjList.ToArray;
      FNodeList[I].AdjList.MakeEmpty;
      for J := 0 to System.High(CurrEdges) do
        begin
          if CurrEdges[J].Destination <> aIndex then
            begin
              if CurrEdges[J].Destination > aIndex then
                Dec(CurrEdges[J].Destination);
              FNodeList[I].AdjList.Add(CurrEdges[J]);
            end;
        end;
    end;
  FReachabilityMatrix.Clear;
end;

function TGSimpleDiGraph.DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
begin
  if aSrc = aDst then
    exit(False);
  Result := FNodeList[aSrc].AdjList.Add(TAdjItem.Create(aDst, aData));
  if Result then
    begin
      Inc(FNodeList[aDst].Tag);
      Inc(FEdgeCount);
      FReachabilityMatrix.Clear;
    end;
end;

function TGSimpleDiGraph.DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
begin
  if aSrc = aDst then
    exit(False);
  Result := FNodeList[aSrc].AdjList.Remove(aDst);
  if Result then
    begin
      Dec(FNodeList[aDst].Tag);
      Dec(FEdgeCount);
      FReachabilityMatrix.Clear;
    end;
end;

function TGSimpleDiGraph.CreateSkeleton: TSkeleton;
var
  I: SizeInt;
begin
  Result := TSkeleton.Create(VertexCount, True);
  Result.FEdgeCount := EdgeCount;
  for I := 0 to Pred(VertexCount) do
    Result[I]^.AssignList(AdjLists[I]);
end;

function TGSimpleDiGraph.FindCycle(aRoot: SizeInt; out aCycle: TIntArray): Boolean;
var
  Stack: TSimpleStack;
  AdjEnums: TAdjEnumArray;
  InStack: TBitVector;
  PreOrd, Parents: TIntArray;
  Counter, Next: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  Stack := TSimpleStack.Create(VertexCount);
  PreOrd := CreateIntArray;
  Parents := CreateIntArray;
  InStack.Size := VertexCount;
  PreOrd[aRoot] := 0;
  Counter := 1;
  Stack.Push(aRoot);
  while Stack.TryPeek(aRoot) do
    if AdjEnums[aRoot].MoveNext then
      begin
        Next := AdjEnums[aRoot].Current;
        if PreOrd[Next] = -1 then
          begin
            Parents[Next] := aRoot;
            PreOrd[Next] := Counter;
            InStack[Next] := True;
            Inc(Counter);
            Stack.Push(Next);
          end
        else
          if (PreOrd[aRoot] >= PreOrd[Next]) and InStack[Next] then
            begin
              aCycle := TreePathFromTo(Parents, Next, aRoot);
              exit(True);
            end;
      end
    else
      InStack[Stack.Pop{%H-}] := False;
  Result := False;
end;

function TGSimpleDiGraph.CycleExists: Boolean;
var
  Stack: TSimpleStack;
  AdjEnums: TAdjEnumArray;
  InStack: TBitVector;
  PreOrd: TIntArray;
  Counter, I, Curr, Next: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  Stack := TSimpleStack.Create(VertexCount);
  PreOrd := CreateIntArray;
  InStack.Size := VertexCount;
  Counter := 0;
  for I := 0 to Pred(VertexCount) do
    if PreOrd[I] = -1 then
      begin
        PreOrd[I] := Counter;
        Inc(Counter);
        Stack.Push(I);
        while Stack.TryPeek(Curr) do
          if AdjEnums[{%H-}Curr].MoveNext then
            begin
              Next := AdjEnums[Curr].Current;
              if PreOrd[Next] = -1 then
                begin
                  PreOrd[Next] := Counter;
                  InStack[Next] := True;
                  Inc(Counter);
                  Stack.Push(Next);
                end
              else
                if (PreOrd[Curr] >= PreOrd[Next]) and InStack[Next] then
                  exit(True);
            end
          else
            InStack[Stack.Pop{%H-}] := False;
      end;
  Result := False;
end;

function TGSimpleDiGraph.TopoSort: TIntArray;
var
  Stack: TSimpleStack;
  AdjEnums: TAdjEnumArray;
  Visited: TBitVector;
  Counter, I, Curr, Next: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  Stack := TSimpleStack.Create(VertexCount);
  Result := CreateIntArray;
  Visited.Size := VertexCount;
  Counter := Pred(VertexCount);
  for I := 0 to Pred(VertexCount) do
    if not Visited[I] then
      begin
        Visited[I] := True;
        Stack.Push(I);
        while Stack.TryPeek(Curr) do
          if AdjEnums[{%H-}Curr].MoveNext then
            begin
              Next := AdjEnums[Curr].Current;
              if not Visited[Next] then
                begin
                  Visited[Next] := True;
                  Stack.Push(Next);
                end;
            end
          else
            begin
              Result[Counter] := Stack.Pop;
              Dec(Counter);
            end;
      end;
end;

function TGSimpleDiGraph.GetDagLongestPaths(aSrc: SizeInt): TIntArray;
var
  Stack: TSimpleStack;
  AdjEnums: TAdjEnumArray;
  Visited: TBitVector;
  d, Curr, Next: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  Stack := TSimpleStack.Create(VertexCount);
  Result := CreateIntArray;
  Visited.Size := VertexCount;
  Visited[aSrc] := True;
  Result[aSrc] := 0;
  Stack.Push(aSrc);
  while Stack.TryPeek(Curr) do
    if AdjEnums[{%H-}Curr].MoveNext then
      begin
        Next := AdjEnums[Curr].Current;
        if not Visited[Next] then
          begin
            Visited[Next] := True;
            d := Succ(Result[Curr]);
            if d > Result[Next] then
              Result[Next] := d;
            Stack.Push(Next);
          end;
      end
    else
      Stack.Pop;
end;

function TGSimpleDiGraph.GetDagLongestPaths(aSrc: SizeInt; out aTree: TIntArray): TIntArray;
var
  Stack: TSimpleStack;
  AdjEnums: TAdjEnumArray;
  Visited: TBitVector;
  d, Curr, Next: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  Stack := TSimpleStack.Create(VertexCount);
  Result := CreateIntArray;
  aTree := CreateIntArray;
  Visited.Size := VertexCount;
  Visited[aSrc] := True;
  Result[aSrc] := 0;
  {%H-}Stack.Push(aSrc);
  while Stack.TryPeek(Curr) do
    if AdjEnums[{%H-}Curr].MoveNext then
      begin
        Next := AdjEnums[Curr].Current;
        if not Visited[Next] then
          begin
            Visited[Next] := True;
            d := Succ(Result[Curr]);
            if d > Result[Next] then
              begin
                Result[Next] := d;
                aTree[Next] := Curr;
              end;
            Stack.Push(Next);
          end;
      end
    else
      Stack.Pop;
end;

function TGSimpleDiGraph.SearchForStrongComponents(out aIds: TIntArray): SizeInt;
var
  Stack, VtxStack, PathStack: TSimpleStack;
  AdjEnums: TAdjEnumArray;
  PreOrd: TIntArray;
  I, Counter, Curr, Next: SizeInt;
begin
  Stack := TSimpleStack.Create(VertexCount);
  VtxStack := TSimpleStack.Create(VertexCount);
  PathStack := TSimpleStack.Create(VertexCount);
  PreOrd := CreateIntArray;
  aIds := CreateIntArray;
  AdjEnums := CreateAdjEnumArray;
  Counter := 0;
  Result := 0;
  for I := 0 to Pred(VertexCount) do
    if PreOrd[I] = -1 then
      begin
        PreOrd[I] := Counter;
        Inc(Counter);
        Stack.Push(I);
        VtxStack.Push(I);
        PathStack.Push(I);
        while Stack.TryPeek(Curr) do
          if AdjEnums[{%H-}Curr].MoveNext then
            begin
              Next := AdjEnums[Curr].Current;
              if PreOrd[Next] = -1 then
                begin
                  PreOrd[Next] := Counter;
                  Inc(Counter);
                  Stack.Push(Next);
                  VtxStack.Push(Next);
                  PathStack.Push(Next);
                end
              else
                if aIds[Next] = -1 then
                  while PreOrd[PathStack.Peek] > PreOrd[Next] do
                    PathStack.Pop;
            end
          else
            begin
              Curr := Stack.Pop;
              if PathStack.Peek = Curr then
                begin
                  PathStack.Pop;
                  repeat
                    Next := VtxStack.Pop;
                    aIds[Next] := Result;
                  until Next = Curr;
                  Inc(Result);
                end;
            end;
      end;
end;

function TGSimpleDiGraph.GetReachabilityMatrix(constref aScIds: TIntArray; aScCount: SizeInt): TReachabilityMatrix;
var
  Stack: TSimpleStack;
  Visited, IdVisited: TBitVector;
  IdParents, IdOrd: TIntArray;
  m: TSquareBitMatrix;
  Pairs: TIntPairSet;
  AdjEnums: TAdjEnumArray;
  I, J, Counter, Curr, Next, CurrId, NextId: SizeInt;
begin
  Stack := TSimpleStack.Create(VertexCount);
  IdParents := CreateIntArray(aScCount, -1);
  IdOrd := CreateIntArray(aScCount, -1);
  Visited.Size := VertexCount;
  IdVisited.Size := aScCount;
  AdjEnums := CreateAdjEnumArray;
  Counter := 0;
  m := TSquareBitMatrix.Create(aScCount);
  for I := 0 to Pred(VertexCount) do
    if not Visited[I] then
      begin
        Visited[I] := True;
        Stack.Push(I);
        if IdOrd[aScIds[I]] = -1 then
          begin
            IdOrd[aScIds[I]] := Counter;
            Inc(Counter);
          end;
        while Stack.TryPeek(Curr) do
          begin
            CurrId := aScIds[{%H-}Curr];
            if AdjEnums[{%H-}Curr].MoveNext then
              begin
                Next := AdjEnums[Curr].Current;
                NextId := aScIds[Next];
                m[CurrId, NextId] := True;
                if IdOrd[CurrId] < IdOrd[NextId] then
                  continue;
                if not Visited[Next] then
                  begin
                    Visited[Next] := True;
                    Stack.Push(Next);
                    if IdOrd[NextId] = -1 then
                      begin
                        IdOrd[NextId] := Counter;
                        IdParents[NextId] := CurrId;
                        Inc(Counter);
                      end
                  end
                    else
                      if Pairs.Add(IdOrd[CurrId], IdOrd[NextId]) then
                        for J := 0 to Pred(aScCount) do
                          if m[NextId, J] then
                            m[CurrId, J] := True;
              end
            else
              begin
                Next := aScIds[Stack.Pop];
                if not IdVisited[Next] then
                  begin
                    IdVisited[Next] := True;
                    Curr := IdParents[Next];
                    if Curr <> -1 then
                      for J := 0 to Pred(aScCount) do
                        if m[Next, J] then
                          m[Curr, J] := True;
                  end;
              end;
          end;
      end;
  Result := TReachabilityMatrix.Create(m, aScIds);
end;

procedure TGSimpleDiGraph.Clear;
begin
  inherited;
  FReachabilityMatrix.Clear;
end;

function TGSimpleDiGraph.Clone: TGSimpleDiGraph;
var
  I: SizeInt;
begin
  Result := TGSimpleDiGraph.Create;
  Result.FCount := VertexCount;
  Result.FEdgeCount := EdgeCount;
  Result.FTitle := Title;
  if NonEmpty then
    begin
      Result.FChainList := System.Copy(FChainList);
      System.SetLength(Result.FNodeList, System.Length(FNodeList));
      for I := 0 to Pred(VertexCount) do
        Result.FNodeList[I].Assign(FNodeList[I]);
    end;
end;

function TGSimpleDiGraph.Reverse: TGSimpleDiGraph;
var
  e: TEdge;
  v: TVertex;
begin
  Result := TGSimpleDiGraph.Create;
  for v in Vertices do
    Result.AddVertex(v);
  for e in Edges do
    Result.AddEdgeI(e.Destination, e.Source, e.Data);
end;

procedure TGSimpleDiGraph.SaveToStream(aStream: TStream; aWriteVertex: TOnWriteVertex; aWriteData: TOnWriteData);
var
  Header: TStreamHeader;
  s, d: Integer;
  Edge: TEdge;
  gTitle, Descr: utf8string;
  wbs: TWriteBufStream;
begin
  if not Assigned(aWriteVertex) then
    raise EGraphError.Create(SEWriteCallbackMissed);
{$IFDEF CPU64}
  if VertexCount > System.High(Integer) then
    raise ELGraphError.CreateFmt(SEStreamSizeExceedFmt, [VertexCount]);
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
    for s := 0 to Pred(Header.VertexCount) do
      aWriteVertex(wbs, FNodeList[s].Vertex);
    //write edges
    for Edge in Edges do
      begin
        s := Edge.Source;
        d := Edge.Destination;
        wbs.WriteBuffer(NtoLE(s), SizeOf(s));
        wbs.WriteBuffer(NtoLE(d), SizeOf(d));
        if Assigned(aWriteData) then
          aWriteData(wbs, Edge.Data);
      end;
  finally
    wbs.Free;
  end;
end;

procedure TGSimpleDiGraph.LoadFromStream(aStream: TStream; aReadVertex: TOnReadVertex; aReadData: TOnReadData);
var
  Header: TStreamHeader;
  s, d: Integer;
  I, Ind: SizeInt;
  Data: TEdgeData;
  Vertex: TVertex;
  gTitle, Descr: utf8string;
  rbs: TReadBufStream;
begin
  if not Assigned(aReadVertex) then
    raise EGraphError.Create(SEReadCallbackMissed);
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
        aReadVertex(rbs, Vertex);
        if not AddVertex(Vertex, Ind) then
          raise EGraphError.Create(SEGraphStreamCorrupt);
        if Ind <> I then
          raise EGraphError.Create(SEGraphStreamReadIntern);
      end;
    //read edges
    Data := DefaultEdgeData;
    for I := 0 to Pred(Header.EdgeCount) do
      begin
        rbs.ReadBuffer(s, SizeOf(s));
        rbs.ReadBuffer(d, SizeOf(d));
        if Assigned(aReadData) then
          aReadData(rbs, Data);
        AddEdgeI(LEToN(s), LEToN(d), Data);
      end;
  finally
    rbs.Free;
  end;
end;

procedure TGSimpleDiGraph.SaveToFile(const aFileName: string; aWriteVertex: TOnWriteVertex;
  aWriteData: TOnWriteData);
var
  fs: TStream;
begin
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(fs, aWriteVertex, aWriteData);
  finally
    fs.Free;
  end;
end;

procedure TGSimpleDiGraph.LoadFromFile(const aFileName: string; aReadVertex: TOnReadVertex; aReadData: TOnReadData);
var
  fs: TStream;
begin
  fs := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs, aReadVertex, aReadData);
  finally
    fs.Free;
  end;
end;

function TGSimpleDiGraph.AddVertex(constref aVertex: TVertex; out aIndex: SizeInt): Boolean;
begin
  Result := not FindOrAdd(aVertex, aIndex);
  if Result then
    begin
      FNodeList[aIndex].Tag := 0;
      FReachabilityMatrix.Clear;
    end;
end;

function TGSimpleDiGraph.AddVertex(constref aVertex: TVertex): Boolean;
var
  Dummy: SizeInt;
begin
  Result := AddVertex(aVertex, Dummy);
end;

procedure TGSimpleDiGraph.RemoveVertex(constref aVertex: TVertex);
begin
  RemoveVertexI(IndexOf(aVertex));
end;

procedure TGSimpleDiGraph.RemoveVertexI(aIndex: SizeInt);
begin
  CheckIndexRange(aIndex);
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
  Result := AddEdge(aSrc, aDst, DefaultEdgeData);
end;

function TGSimpleDiGraph.AddEdgeI(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  Result := DoAddEdge(aSrc, aDst, aData);
end;

function TGSimpleDiGraph.AddEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := AddEdgeI(aSrc, aDst, DefaultEdgeData);
end;

function TGSimpleDiGraph.RemoveEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := RemoveEdgeI(IndexOf(aSrc), IndexOf(aDst));
end;

function TGSimpleDiGraph.RemoveEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  Result := DoRemoveEdge(aSrc, aDst);
end;

function TGSimpleDiGraph.InDegree(constref aVertex: TVertex): SizeInt;
begin
  Result := InDegreeI(IndexOf(aVertex));
end;

function TGSimpleDiGraph.InDegreeI(aIndex: SizeInt): SizeInt;
begin
  CheckIndexRange(aIndex);
  Result := FNodeList[aIndex].Tag;
end;

function TGSimpleDiGraph.OutDegree(constref aVertex: TVertex): SizeInt;
begin
  Result := OutDegreeI(IndexOf(aVertex));
end;

function TGSimpleDiGraph.OutDegreeI(aIndex: SizeInt): SizeInt;
begin
  CheckIndexRange(aIndex);
  Result := FNodeList[aIndex].AdjList.Count;
end;

function TGSimpleDiGraph.Degree(constref aVertex: TVertex): SizeInt;
begin
  Result := DegreeI(IndexOf(aVertex));
end;

function TGSimpleDiGraph.DegreeI(aIndex: SizeInt): SizeInt;
begin
  CheckIndexRange(aIndex);
  Result := FNodeList[aIndex].AdjList.Count + FNodeList[aIndex].Tag;
end;

function TGSimpleDiGraph.Isolated(constref aVertex: TVertex): Boolean;
begin
  Result := Degree(aVertex) = 0;
end;

function TGSimpleDiGraph.IsolatedI(aIndex: SizeInt): Boolean;
begin
  Result := DegreeI(aIndex) = 0;
end;

function TGSimpleDiGraph.IsSource(constref aVertex: TVertex): Boolean;
begin
  Result := IsSourceI(IndexOf(aVertex));
end;

function TGSimpleDiGraph.IsSourceI(aIndex: SizeInt): Boolean;
begin
  CheckIndexRange(aIndex);
  Result := (FNodeList[aIndex].AdjList.Count <> 0) and (FNodeList[aIndex].Tag = 0);
end;

function TGSimpleDiGraph.IsSink(constref aVertex: TVertex): Boolean;
begin
  Result := IsSinkI(IndexOf(aVertex));
end;

function TGSimpleDiGraph.IsSinkI(aIndex: SizeInt): Boolean;
begin
  CheckIndexRange(aIndex);
  Result := (FNodeList[aIndex].AdjList.Count = 0) and (FNodeList[aIndex].Tag <> 0);
end;

function TGSimpleDiGraph.SourceCount: SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I := 0 to Pred(VertexCount) do
    if (FNodeList[I].AdjList.Count <> 0) and (FNodeList[I].Tag = 0) then
      Inc(Result);
end;

function TGSimpleDiGraph.SinkCount: SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I := 0 to Pred(VertexCount) do
    if (FNodeList[I].AdjList.Count = 0) and (FNodeList[I].Tag <> 0) then
      Inc(Result);
end;

function TGSimpleDiGraph.PathExists(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := PathExistsI(IndexOf(aSrc), IndexOf(aDst));
end;

function TGSimpleDiGraph.PathExistsI(aSrc, aDst: SizeInt): Boolean;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  if aSrc = aDst then
    exit(True);
  if ReachabilityValid then
    exit(FReachabilityMatrix.Reachable(aSrc, aDst));
  Result := CheckPathExists(aSrc, aDst);
end;

function TGSimpleDiGraph.ContainsCycle(constref aRoot: TVertex; out aCycle: TIntArray): Boolean;
begin
  Result := ContainsCycleI(IndexOf(aRoot), aCycle);
end;

function TGSimpleDiGraph.ContainsCycleI(aRoot: SizeInt; out aCycle: TIntArray): Boolean;
begin
  CheckIndexRange(aRoot);
  if VertexCount < 2 then
    exit(False);
  aCycle := nil;
  FindCycle(aRoot, aCycle);
  Result := System.Length(aCycle) <> 0;
end;

function TGSimpleDiGraph.ContainsEulerianCircuit: Boolean;
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

function TGSimpleDiGraph.FindEulerianCircuit(out aCircuit: TIntVector): Boolean;
var
  g: TSkeleton;
  Stack: TIntStack;
  s, d: SizeInt;
begin
  if not ContainsEulerianCircuit then
    exit(False);
  g := CreateSkeleton;
  s := 0;
  while g.Degree[s] = 0 do
    Inc(s);
  aCircuit.Add(s);
  repeat
    while g[s]^.FindFirst(d) do
      begin
        {%H-}Stack.Push(s);
        g.RemoveEdge(s, d);
        s := d;
      end;
    if not Stack.TryPop(s) then
      break;
    aCircuit.Add(s);
  until False;
  Result := aCircuit.Count > 0;
  if Result then
    TIntVectorHelper.Reverse(aCircuit);
end;

function TGSimpleDiGraph.FindStrongComponents(out aCompIds: TIntArray): SizeInt;
begin
  if IsEmpty then
    exit(0);
  if VertexCount = 1 then
    begin
      aCompIds := [0];
      exit(1);
    end;
  if ReachabilityValid then
    begin
      aCompIds := System.Copy(FReachabilityMatrix.FIds);
      exit(FReachabilityMatrix.Size);
    end;
  Result := SearchForStrongComponents(aCompIds);
end;

procedure TGSimpleDiGraph.FillReachabilityMatrix;
var
  Ids: TIntArray;
  ScCount: SizeInt;
begin
  if IsEmpty or ReachabilityValid then
    exit;
  ScCount := SearchForStrongComponents(Ids);
  FillReachabilityMatrix(Ids, ScCount);
end;

procedure TGSimpleDiGraph.FillReachabilityMatrix(constref aScIds: TIntArray; aScCount: SizeInt);
var
  m: TSquareBitMatrix;
begin
  if aScCount = 1 then
    begin
      m := TSquareBitMatrix.Create(aScCount);
      m[0, 0] := True;
      FReachabilityMatrix := TReachabilityMatrix.Create(m, aScIds);
      exit;
    end;
  FReachabilityMatrix := GetReachabilityMatrix(aScIds, aScCount);
end;

function TGSimpleDiGraph.TopologicalSort(aOrder: TSortOrder): TIntArray;
begin
  if IsEmpty then
    exit(nil);
  Result := TopoSort;
  if aOrder = soDesc then
    TIntHelper.Reverse(Result);
end;

function TGSimpleDiGraph.IsDag: Boolean;
var
  Dummy: TIntArray = nil;
  I: SizeInt;
begin
  if IsEmpty then
    exit(False);
  if VertexCount = 1 then
    exit(True);
  Result := not CycleExists;
end;

function TGSimpleDiGraph.DagLongestPathsMap(constref aSrc: TVertex): TIntArray;
begin
  Result := DagLongestPathsMapI(IndexOf(aSrc));
end;

function TGSimpleDiGraph.DagLongestPathsMapI(aSrc: SizeInt): TIntArray;
begin
  CheckIndexRange(aSrc);
  Result := GetDagLongestPaths(aSrc);
end;

function TGSimpleDiGraph.DagLongestPathsMap(constref aSrc: TVertex; out aPathTree: TIntArray): TIntArray;
begin
  Result := DagLongestPathsMapI(IndexOf(aSrc), aPathTree);
end;

function TGSimpleDiGraph.DagLongestPathsMapI(aSrc: SizeInt; out aPathTree: TIntArray): TIntArray;
begin
  CheckIndexRange(aSrc);
  Result := GetDagLongestPaths(aSrc, aPathTree);
end;

function TGSimpleDiGraph.DagLongesPaths: TIntArray;
var
  TopoOrd: TIntArray;
  I, J, d: SizeInt;
begin
  TopoOrd := TopologicalSort(soDesc);
  Result := CreateIntArray(0);
  for I := 1 to Pred(VertexCount) do
    for J := 0 to Pred(I) do
      if AdjacentI(TopoOrd[I], TopoOrd[J]) then
        begin
          d := Succ(Result[TopoOrd[J]]);
          if d > Result[TopoOrd[I]] then
            Result[TopoOrd[I]] := d;
        end;
end;

{ TGWeightedDiGraph }

function TGWeightedDiGraph.CreateEdgeArray: TEdgeArray;
var
  I: SizeInt = 0;
  e: TEdge;
begin
  System.SetLength(Result, EdgeCount);
  for e in Edges do
    begin
      Result[I] := TWeightEdge.Create(e.Source, e.Destination, e.Data.Weight);
      Inc(I);
    end;
end;

function TGWeightedDiGraph.GetDagMaxPaths(aSrc: SizeInt): TWeightArray;
var
  Stack: TSimpleStack;
  AdjEnums: TAdjEnumArray;
  Visited: TBitVector;
  Curr, Next: SizeInt;
  w: TWeight;
begin
  AdjEnums := CreateAdjEnumArray;
  Stack := TSimpleStack.Create(VertexCount);
  Result := TPathHelper.CreateWeightArrayNI(VertexCount);
  Visited.Size := VertexCount;
  Visited[aSrc] := True;
  Result[aSrc] := ZeroWeight;
  Stack.Push(aSrc);
  while Stack.TryPeek(Curr) do
    if AdjEnums[{%H-}Curr].MoveNext then
      begin
        Next := AdjEnums[Curr].Current;
        if not Visited[Next] then
          begin
            Visited[Next] := True;
            Stack.Push(Next);
          end;
        w := Result[Curr] + GetEdgeDataPtr(Curr, Next)^.Weight;
        if w > Result[Next] then
          Result[Next] := w;
      end
    else
      Stack.Pop;
end;

function TGWeightedDiGraph.GetDagMaxPaths(aSrc: SizeInt; out aTree: TIntArray): TWeightArray;
var
  Stack: TSimpleStack;
  AdjEnums: TAdjEnumArray;
  Visited: TBitVector;
  Curr, Next: SizeInt;
  w: TWeight;
begin
  AdjEnums := CreateAdjEnumArray;
  Stack := TSimpleStack.Create(VertexCount);
  Result := TPathHelper.CreateWeightArrayNI(VertexCount);
  aTree := CreateIntArray;
  Visited.Size := VertexCount;
  Visited[aSrc] := True;
  Result[aSrc] := ZeroWeight;
  {%H-}Stack.Push(aSrc);
  while Stack.TryPeek(Curr) do
    if AdjEnums[{%H-}Curr].MoveNext then
      begin
        Next := AdjEnums[Curr].Current;
        if not Visited[Next] then
          begin
            Visited[Next] := True;
            Stack.Push(Next);
          end;
        w := Result[Curr] + GetEdgeDataPtr(Curr, Next)^.Weight;
        if w > Result[Next] then
          begin
            Result[Next] := w;
            aTree[Next] := Curr;
          end;
      end
    else
      Stack.Pop;
end;

class function TGWeightedDiGraph.wMin(L, R: TWeight): TWeight;
begin
  if L <= R then
    Result := L
  else
    Result := R;
end;

class function TGWeightedDiGraph.wMax(L, R: TWeight): TWeight;
begin
  if L >= R then
    Result := L
  else
    Result := R;
end;

class function TGWeightedDiGraph.InfWeight: TWeight;
begin
  Result := TWeight.MaxValue;
end;

class function TGWeightedDiGraph.NegInfWeight: TWeight;
begin
  Result := TWeight.MinValue;
end;

class function TGWeightedDiGraph.ZeroWeight: TWeight;
begin
  Result := Default(TWeight);
end;

function TGWeightedDiGraph.ContainsNegWeightEdge: Boolean;
var
  e: TEdge;
begin
  for e in Edges do
    if e.Data.Weight < ZeroWeight then
      exit(True);
  Result := False;
end;

function TGWeightedDiGraph.ContainsNegCycle(constref aRoot: TVertex; out aCycle: TIntArray): Boolean;
begin
  Result := ContainsNegCycleI(IndexOf(aRoot), aCycle);
end;

function TGWeightedDiGraph.ContainsNegCycleI(aRootIdx: SizeInt; out aCycle: TIntArray): Boolean;
begin
  CheckIndexRange(aRootIdx);
  aCycle := TPathHelper.NegDetect(Self, aRootIdx);
  Result := aCycle <> nil;
end;

function TGWeightedDiGraph.Clone: TGWeightedDiGraph;
begin
  Result := inherited Clone as TGWeightedDiGraph;
end;

function TGWeightedDiGraph.Reverse: TGWeightedDiGraph;
begin
  Result := inherited Reverse as TGWeightedDiGraph;
end;

function TGWeightedDiGraph.MinPathsMap(constref aSrc: TVertex): TWeightArray;
begin
  Result := MinPathsMapI(IndexOf(aSrc));
end;

function TGWeightedDiGraph.MinPathsMapI(aSrc: SizeInt): TWeightArray;
begin
  CheckIndexRange(aSrc);
  Result := TPathHelper.DijkstraSssp(Self, aSrc);
end;

function TGWeightedDiGraph.MinPathsMap(constref aSrc: TVertex; out aPathTree: TIntArray): TWeightArray;
begin
  Result := MinPathsMapI(IndexOf(aSrc), aPathTree);
end;

function TGWeightedDiGraph.MinPathsMapI(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
begin
  CheckIndexRange(aSrc);
  Result := TPathHelper.DijkstraSssp(Self, aSrc, aPathTree);
end;

function TGWeightedDiGraph.MinPathWeight(constref aSrc, aDst: TVertex): TWeight;
begin
  Result := MinPathWeightI(IndexOf(aSrc), IndexOf(aDst));
end;

function TGWeightedDiGraph.MinPathWeightI(aSrc, aDst: SizeInt): TWeight;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  Result := TPathHelper.DijkstraPath(Self, aSrc, aDst);
end;

function TGWeightedDiGraph.MinPath(constref aSrc, aDst: TVertex; out aWeight: TWeight): TIntArray;
begin
  Result := MinPathI(IndexOf(aSrc), IndexOf(aDst), aWeight);
end;

function TGWeightedDiGraph.MinPathI(aSrc, aDst: SizeInt; out aWeight: TWeight): TIntArray;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  Result := TPathHelper.DijkstraPath(Self, aSrc, aDst, aWeight);
end;

function TGWeightedDiGraph.MinPathAStar(constref aSrc, aDst: TVertex; out aWeight: TWeight;
  aEst: TEstimate): TIntArray;
begin
  Result := MinPathAStarI(IndexOf(aSrc), IndexOf(aDst), aWeight, aEst);
end;

function TGWeightedDiGraph.MinPathAStarI(aSrc, aDst: SizeInt; out aWeight: TWeight; aEst: TEstimate): TIntArray;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  if aEst <> nil then
    Result := TPathHelper.AStar(Self, aSrc, aDst, aWeight, aEst)
  else
    Result := TPathHelper.DijkstraPath(Self, aSrc, aDst, aWeight);
end;

function TGWeightedDiGraph.FindMinPath(constref aSrc, aDst: TVertex; out aPath: TIntArray;
  out aWeight: TWeight): Boolean;
begin
  Result := FindMinPathI(IndexOf(aSrc), IndexOf(aDst), aPath, aWeight);
end;

function TGWeightedDiGraph.FindMinPathI(aSrc, aDst: SizeInt; out aPath: TIntArray; out aWeight: TWeight): Boolean;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  Result := TPathHelper.BfmtPath(Self, aSrc, aDst, aPath, aWeight);
end;

function TGWeightedDiGraph.FindMinPathsMap(constref aSrc: TVertex; out aWeights: TWeightArray): Boolean;
begin
  Result := FindMinPathsMapI(IndexOf(aSrc), aWeights);
end;

function TGWeightedDiGraph.FindMinPathsMapI(aSrc: SizeInt; out aWeights: TWeightArray): Boolean;
begin
  CheckIndexRange(aSrc);
  Result := TPathHelper.BfmtSssp(Self, aSrc, aWeights);
end;

function TGWeightedDiGraph.FindMinPathsMap(constref aSrc: TVertex; out aPaths: TIntArray;
  out aWeights: TWeightArray): Boolean;
begin
  Result := FindMinPathsMapI(IndexOf(aSrc), aPaths, aWeights);
end;

function TGWeightedDiGraph.FindMinPathsMapI(aSrc: SizeInt; out aPaths: TIntArray;
  out aWeights: TWeightArray): Boolean;
begin
  CheckIndexRange(aSrc);
  Result := TPathHelper.BfmtSssp(Self, aSrc, aPaths, aWeights);
end;

function TGWeightedDiGraph.DagMaxPathsMap(constref aSrc: TVertex): TWeightArray;
begin
  Result := DagMaxPathsMapI(IndexOf(aSrc));
end;

function TGWeightedDiGraph.DagMaxPathsMapI(aSrc: SizeInt): TWeightArray;
begin
  CheckIndexRange(aSrc);
  Result := GetDagMaxPaths(aSrc);
end;

function TGWeightedDiGraph.DagMaxPathsMap(constref aSrc: TVertex; out aPathTree: TIntArray): TWeightArray;
begin
  Result := DagMaxPathsMapI(IndexOf(aSrc), aPathTree);
end;

function TGWeightedDiGraph.DagMaxPathsMapI(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
var
  I: SizeInt;
begin
  CheckIndexRange(aSrc);
  Result := GetDagMaxPaths(aSrc, aPathTree);
end;

function TGWeightedDiGraph.DagMaxPaths: TWeightArray;
var
  TopoOrd: TIntArray;
  I, J: SizeInt;
  w: TWeight;
begin
  TopoOrd := TopologicalSort(soDesc);
  Result := TPathHelper.CreateWeightArrayZ(VertexCount);
  for I := 1 to Pred(VertexCount) do
    for J := 0 to Pred(I) do
      if AdjacentI(TopoOrd[I], TopoOrd[J]) then
        begin
          w := Result[TopoOrd[J]] + GetEdgeDataPtr(TopoOrd[I], TopoOrd[J])^.Weight;
          if w > Result[TopoOrd[I]] then
            Result[TopoOrd[I]] := w;
        end;
end;

{ TGIntWeightDiGraph.THPrHelper.TFlowData }

constructor TGIntWeightDiGraph.THPrHelper.TArc.Create(aTarget: PNode; aReverse: PArc; aCap: TWeight);
begin
  Target := aTarget;
  Reverse := aReverse;
  ResCap := aCap;
  IsForward := True;
end;

constructor TGIntWeightDiGraph.THPrHelper.TArc.CreateReverse(aTarget: PNode; aReverse: PArc);
begin
  Target := aTarget;
  Reverse := aReverse;
  ResCap := 0;
  IsForward := False;
end;

function TGIntWeightDiGraph.THPrHelper.TArc.IsSaturated: Boolean;
begin
  Result := ResCap = 0;
end;

function TGIntWeightDiGraph.THPrHelper.TArc.IsResidual: Boolean;
begin
  Result := ResCap > 0;
end;

procedure TGIntWeightDiGraph.THPrHelper.TArc.Push(aFlow: TWeight);
begin
  ResCap -= aFlow;
  Target^.Excess += aFlow;
  Reverse^.ResCap += aFlow;
  Reverse^.Target^.Excess -= aFlow;
end;

{ TGIntWeightDiGraph.THPrHelper.TNode }

function TGIntWeightDiGraph.THPrHelper.TNode.GetColor: TVertexColor;
begin
  Result := TVertexColor(Distance);
end;

procedure TGIntWeightDiGraph.THPrHelper.TNode.SetColor(aValue: TVertexColor);
begin
  Distance := SizeInt(aValue);
end;

procedure TGIntWeightDiGraph.THPrHelper.TNode.ResetCurrent;
begin
  CurrentArc := FirstArc;
end;

{ TGIntWeightDiGraph.THPrHelper.TLevel }

function TGIntWeightDiGraph.THPrHelper.TLevel.IsEmpty: Boolean;
begin
  Result := (TopActive = nil) and (TopIdle = nil);
end;

procedure TGIntWeightDiGraph.THPrHelper.TLevel.AddActive(aNode: PNode);
begin
  aNode^.LevelNext := TopActive;
  TopActive := aNode;
end;

procedure TGIntWeightDiGraph.THPrHelper.TLevel.AddIdle(aNode: PNode);
var
  Next: PNode;
begin
  Next := TopIdle;
  TopIdle := aNode;
  aNode^.LevelNext := Next;
  if Next <> nil then
    Next^.LevelPrev := aNode;
end;

procedure TGIntWeightDiGraph.THPrHelper.TLevel.Activate(aNode: PNode);
begin
  RemoveIdle(aNode);
  AddActive(aNode);
end;

procedure TGIntWeightDiGraph.THPrHelper.TLevel.RemoveIdle(aNode: PNode);
var
  Next, Prev: PNode;
begin
  Next := aNode^.LevelNext;
  if TopIdle = aNode then // is on head of the list
    TopIdle := Next
  else
    begin
      Prev := aNode^.LevelPrev;
      Prev^.LevelNext := aNode^.LevelNext;
      if Next <> nil then
        Next^.LevelPrev := Prev;
    end;
end;

procedure TGIntWeightDiGraph.THPrHelper.TLevel.Clear(aLabel: SizeInt);
var
  Next: PNode;
  I: SizeInt;
begin
  Next := TopActive;
  while Next <> nil do
    begin
      Next^.Distance := aLabel;
      Next := Next^.LevelNext;
    end;
  TopActive := nil;
  Next := TopIdle;
  while Next <> nil do
    begin
      Next^.Distance := aLabel;
      Next := Next^.LevelNext;
    end;
  TopIdle  := nil;
end;

{ TGIntWeightDiGraph.THPrHelper }

procedure TGIntWeightDiGraph.THPrHelper.CreateResudualGraph(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt);
var
  CurrArcIdx: TIntArray;
  I, J: SizeInt;
  p: PAdjItem;
begin
  FNodeCount := aGraph.VertexCount;
  System.SetLength(CurrArcIdx, FNodeCount);
  J := 0;
  for I := 0 to System.High(CurrArcIdx) do
    begin
      CurrArcIdx[I] := J;
      J += aGraph.DegreeI(I);
    end;

  System.SetLength(FNodes, Succ(FNodeCount));
  System.SetLength(FArcs, Succ(aGraph.EdgeCount * 2));
  FSource := @FNodes[aSource];
  FSink := @FNodes[aSink];

  for I := 0 to Pred(FNodeCount) do
    begin
      FNodes[I].FirstArc := @FArcs[CurrArcIdx[I]];
      FNodes[I].Excess := ZeroWeight;
    end;

  for I := 0 to Pred(FNodeCount) do
    for p in aGraph.AdjLists[I]^ do
      begin
        J := p^.Destination;
        FArcs[CurrArcIdx[I]] := TArc.Create(@FNodes[J], @FArcs[CurrArcIdx[J]], p^.Data.Weight);
        FArcs[CurrArcIdx[J]] := TArc.CreateReverse(@FNodes[I], @FArcs[CurrArcIdx[I]]);
        Inc(CurrArcIdx[I]);
        Inc(CurrArcIdx[J]);
      end;

  CurrArcIdx := nil;

  FArcs[System.High(FArcs)] :=
    TArc.Create(@FNodes[FNodeCount], @FArcs[System.High(FArcs)], 0);
  //sentinel node
  FNodes[FNodeCount].FirstArc := @FArcs[System.High(FArcs)];
  FNodes[FNodeCount].CurrentArc := @FArcs[System.High(FArcs)];
  FNodes[FNodeCount].LevelNext := nil;
  FNodes[FNodeCount].LevelPrev := nil;
  FNodes[FNodeCount].Excess := 0;
  FNodes[FNodeCount].Distance := FNodeCount;

  FSource^.Excess := MaxWeight;
  System.SetLength(FLevels, FNodeCount);
  FMaxLevel := System.High(FLevels);
  System.SetLength(FQueue, FNodeCount);
end;

procedure TGIntWeightDiGraph.THPrHelper.CreateResudualGraphCap(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight);
var
  CurrArcIdx: TIntArray;
  I, J: SizeInt;
  p: PAdjItem;
begin
  //almost same as above, but also stores capacities of the arcs;
  FNodeCount := aGraph.VertexCount;
  System.SetLength(CurrArcIdx, FNodeCount);
  CurrArcIdx[0] := 0;
  J := aGraph.DegreeI(0);

  for I := 1 to System.High(CurrArcIdx) do
    begin
      CurrArcIdx[I] := J;
      J += aGraph.DegreeI(I);
    end;

  System.SetLength(FNodes, Succ(FNodeCount));
  System.SetLength(FArcs, Succ(aGraph.EdgeCount * 2));
  System.SetLength(FCaps, aGraph.EdgeCount * 2);
  FSource := @FNodes[aSource];
  FSink := @FNodes[aSink];

  for I := 0 to Pred(FNodeCount) do
    begin
      FNodes[I].FirstArc := @FArcs[CurrArcIdx[I]];
      FNodes[I].Excess := ZeroWeight;
    end;

  for I := 0 to Pred(FNodeCount) do
    for p in aGraph.AdjLists[I]^ do
      begin
        J := p^.Destination;
        FCaps[CurrArcIdx[I]] := p^.Data.Weight;
        FCaps[CurrArcIdx[J]] := ZeroWeight;
        FArcs[CurrArcIdx[I]] := TArc.Create(@FNodes[J], @FArcs[CurrArcIdx[J]], p^.Data.Weight);
        FArcs[CurrArcIdx[J]] := TArc.CreateReverse(@FNodes[I], @FArcs[CurrArcIdx[I]]);
        Inc(CurrArcIdx[I]);
        Inc(CurrArcIdx[J]);
      end;

  CurrArcIdx := nil;

  FArcs[System.High(FArcs)] :=
    TArc.Create(@FNodes[FNodeCount], @FArcs[System.High(FArcs)], 0);
  //sentinel node
  FNodes[FNodeCount].FirstArc := @FArcs[System.High(FArcs)];
  FNodes[FNodeCount].CurrentArc := @FArcs[System.High(FArcs)];
  FNodes[FNodeCount].LevelNext := nil;
  FNodes[FNodeCount].LevelPrev := nil;
  FNodes[FNodeCount].Excess := 0;
  FNodes[FNodeCount].Distance := FNodeCount;

  FSource^.Excess := aReqFlow;
  System.SetLength(FLevels, FNodeCount);
  FMaxLevel := System.High(FLevels);
  System.SetLength(FQueue, FNodeCount);
end;

procedure TGIntWeightDiGraph.THPrHelper.ClearLabels;
var
  I: SizeInt;
begin
  for I := 0 to  System.High(FNodes) do
    FNodes[I].Distance := FNodeCount;
end;

procedure TGIntWeightDiGraph.THPrHelper.GlobalRelabel;
var
  CurrNode, NextNode: PNode;
  CurrArc: PArc;
  Dist: SizeInt;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  System.FillChar(Pointer(FLevels)^, Succ(FMaxLevel) * SizeOf(TLevel), 0);
  FMaxLevel := 0;
  FMaxActiveLevel := NULL_INDEX;
  FMinActiveLevel := FNodeCount;
  ClearLabels;
  FSink^.Distance := 0;
  CurrNode := FSink;
  FQueue[qTail] := FSink;
  Inc(qTail);
  while qHead < qTail do
    begin
      CurrNode := FQueue[qHead];
      Inc(qHead);
      Dist := Succ(CurrNode^.Distance);
      CurrArc := CurrNode^.FirstArc;
      while CurrArc < (CurrNode + 1)^.FirstArc do
        begin
          NextNode := CurrArc^.Target;
          if (NextNode^.Distance = FNodeCount) and CurrArc^.Reverse^.IsResidual then
            begin
              NextNode^.Distance := Dist;
              NextNode^.ResetCurrent;
              if Dist > FMaxLevel then
                FMaxLevel := Dist;
              if NextNode^.Excess > 0 then
                begin
                  FLevels[Dist].AddActive(NextNode);
                  if Dist > FMaxActiveLevel then
                    FMaxActiveLevel := Dist;
                  if Dist < FMinActiveLevel then
                    FMinActiveLevel := Dist;
                end
              else
                FLevels[Dist].AddIdle(NextNode);
              FQueue[qTail] := NextNode;
              Inc(qTail);
            end;
          Inc(CurrArc);
        end;
    end;
end;

procedure TGIntWeightDiGraph.THPrHelper.RemoveGap(aLayer: SizeInt);
var
  I: SizeInt;
begin
  for I := Succ(aLayer) to FMaxLevel do
    FLevels[I].Clear(FNodeCount);
  FMaxActiveLevel := Pred(aLayer);
  FMaxLevel := FMaxActiveLevel;
end;

function TGIntWeightDiGraph.THPrHelper.Push(aNode: PNode): Boolean;
var
  CurrArc: PArc;
  NextNode: PNode;
  Dist: SizeInt;
begin
  Dist := Pred(aNode^.Distance);
  while aNode^.CurrentArc < (aNode + 1)^.FirstArc do
    begin
      CurrArc := aNode^.CurrentArc;
      NextNode := CurrArc^.Target;
      if (NextNode^.Distance = Dist) and CurrArc^.IsResidual then
        //arc is not saturated and target belongs to the next layer -> arc is admissible
        begin
          if (Dist > 0) and (NextNode^.Excess = 0) then //-> NextNode in idle list
            begin
              FLevels[Dist].Activate(NextNode);
              if Dist < FMinActiveLevel then
                FMinActiveLevel := Dist;
            end;
          CurrArc^.Push(wMin(aNode^.Excess, CurrArc^.ResCap));
          if aNode^.Excess = 0 then
            break;
        end;
      Inc(aNode^.CurrentArc);
    end;
  Result := aNode^.CurrentArc < (aNode + 1)^.FirstArc;
end;

procedure TGIntWeightDiGraph.THPrHelper.Relabel(aNode: PNode);
var
  CurrArc: PArc;
  MinArc: PArc = nil;
  Dist: SizeInt;
begin
  Dist := FNodeCount;
  aNode^.Distance := FNodeCount;
  CurrArc := aNode^.FirstArc;
  while CurrArc < (aNode + 1)^.FirstArc do
    begin
      if CurrArc^.IsResidual and (CurrArc^.Target^.Distance < Dist) then
        begin
          Dist := CurrArc^.Target^.Distance;
          MinArc := CurrArc;
        end;
      Inc(CurrArc);
    end;
  Inc(Dist);
  if Dist < FNodeCount then
    begin
      aNode^.Distance := Dist;
      aNode^.CurrentArc := MinArc;
      if Dist > FMaxLevel then
        FMaxLevel := Dist;
      if aNode^.Excess > 0 then
        begin
          FLevels[Dist].AddActive(aNode);
          if Dist > FMaxActiveLevel then
            FMaxActiveLevel := Dist;
          if Dist < FMinActiveLevel then
            FMinActiveLevel := Dist;
        end
      else
        FLevels[Dist].AddIdle(aNode);
    end;
end;

procedure TGIntWeightDiGraph.THPrHelper.HiLevelPushRelabel;
var
  CurrNode: PNode;
  GlobalRelableTreshold, OldMaxActive: SizeInt;
  RelableTimes: SizeInt = 0;
begin
  GlobalRelabel;
  GlobalRelableTreshold := FNodeCount;
  while FMaxActiveLevel >= FMinActiveLevel do
    begin
      CurrNode := FLevels[FMaxActiveLevel].TopActive;
      if CurrNode <> nil then
        begin
          OldMaxActive := FMaxActiveLevel;
          FLevels[OldMaxActive].TopActive := CurrNode^.LevelNext;
          if not Push(CurrNode) then
            begin
              Relabel(CurrNode);
              Inc(RelableTimes);
              if FLevels[OldMaxActive].IsEmpty then
                RemoveGap(OldMaxActive);
              if RelableTimes = GlobalRelableTreshold then
                begin
                  GlobalRelabel;
                  RelableTimes := 0;
                end;
            end
          else
            FLevels[OldMaxActive].AddIdle(CurrNode);
        end
      else
        Dec(FMaxActiveLevel);
    end;
end;

function TGIntWeightDiGraph.THPrHelper.CreateEdges: TEdgeArray;
var
  I, J: SizeInt;
  CurrArc: PArc;
begin
  System.SetLength(Result, Pred(System.Length(FArcs)) div 2);
  J := 0;
  for I := 0 to Pred(FNodeCount) do
    begin
      CurrArc := FNodes[I].FirstArc;
      while CurrArc < FNodes[Succ(I)].FirstArc do
        begin
          if CurrArc^.IsForward then
            begin
              Result[J] := TWeightEdge.Create(I, CurrArc^.Target - PNode(FNodes), CurrArc^.Reverse^.ResCap);
              Inc(J);
            end;
          Inc(CurrArc);
        end;
    end;
end;

function TGIntWeightDiGraph.THPrHelper.RecoverFlow: TEdgeArray;
var
  CurrNode, NextNode, RootNode, RestartNode: PNode;
  StackTop: PNode = nil;
  StackBottom: PNode = nil;
  CurrArc: PArc;
  Delta: TWeight;
begin
  CurrNode := Pointer(FNodes);
  while CurrNode < PNode(FNodes) + FNodeCount do
    begin
      CurrNode^.Color := vcWhite;
      CurrNode^.Parent := nil;
      CurrNode^.ResetCurrent;
      CurrArc := CurrNode^.FirstArc;
      while CurrArc < (CurrNode + 1)^.FirstArc do
        begin
          if CurrArc^.Target = CurrNode then
            CurrArc^.ResCap := FCaps[CurrArc - PArc(FArcs)];
          Inc(CurrArc);
        end;
      Inc(CurrNode);
    end;

  CurrNode := Pointer(FNodes);
  while CurrNode < PNode(FNodes) + FNodeCount do
    begin
      if (CurrNode^.Color = vcWhite) and (CurrNode^.Excess > 0) and
         (CurrNode <> FSource) and (CurrNode <> FSink) then
           begin
             RootNode := CurrNode;
             RootNode^.Color := vcGray;
             repeat
               while CurrNode^.CurrentArc < (CurrNode + 1)^.FirstArc do
                 begin
                   CurrArc := CurrNode^.CurrentArc;
                   if (FCaps[CurrArc - PArc(FArcs)] = 0) and CurrArc^.IsResidual and
                      (CurrArc^.Target <> FSource) and (CurrArc^.Target <> FSink) then
                     begin
                       NextNode := CurrArc^.Target;
                       if NextNode^.Color = vcWhite then
                         begin
                           NextNode^.Color := vcGray;
                           NextNode^.Parent := CurrNode;
                           CurrNode := NextNode;
                           break;
                         end
                       else
                         if NextNode^.Color = vcGray then
                           begin
                             //
                             Delta := CurrArc^.ResCap;
                             while True do
                               begin
                                 Delta := wMin(Delta, NextNode^.CurrentArc^.ResCap);
                                 if NextNode = CurrNode then
                                   break
                                 else
                                   NextNode := NextNode^.CurrentArc^.Target;
                               end;
                             //
                             NextNode := CurrNode;
                             while True do
                               begin
                                 CurrArc := NextNode^.CurrentArc;
                                 CurrArc^.ResCap -= Delta;
                                 CurrArc^.Reverse^.ResCap += Delta;
                                 NextNode := CurrArc^.Target;
                                 if NextNode = CurrNode then
                                   break;
                               end;
                             //
                             RestartNode := CurrNode;
                             NextNode := CurrNode^.CurrentArc^.Target;
                             while NextNode <> CurrNode do
                               begin
                                 CurrArc := NextNode^.CurrentArc;
                                 if (NextNode^.Color = vcWhite) or CurrArc^.IsSaturated then
                                   begin
                                     NextNode^.CurrentArc^.Target^.Color := vcWhite;
                                     if NextNode^.Color <> vcWhite then
                                       RestartNode := NextNode;
                                   end;
                                 NextNode := CurrArc^.Target;
                               end;
                             //
                             if RestartNode <> CurrNode then
                               begin
                                 CurrNode := RestartNode;
                                 Inc(CurrNode^.CurrentArc);
                                 break;
                               end;
                             //
                           end;
                     end;
                   Inc(CurrNode^.CurrentArc);
                 end;
               //
               if CurrNode^.CurrentArc >= (CurrNode + 1)^.FirstArc then
                 begin
                   CurrNode^.Color := vcBlack;
                   if CurrNode <> FSource then
                     if StackBottom = nil then
                       begin
                         StackBottom := CurrNode;
                         StackTop := CurrNode;
                       end
                     else
                       begin
                         CurrNode^.OrderNext := StackTop;
                         StackTop := CurrNode;
                       end;
                   if CurrNode <> RootNode then
                     begin
                       CurrNode := CurrNode^.Parent;
                       Inc(CurrNode^.CurrentArc);
                     end
                   else
                     break;
                 end;
             until False;
           end;
      Inc(CurrNode);
    end;

  if StackBottom <> nil then
    begin
      CurrNode := StackTop;
      repeat
        CurrArc := CurrNode^.FirstArc;
        while CurrNode^.Excess > 0 do
          begin
            if (FCaps[CurrArc - PArc(FArcs)] = 0) and CurrArc^.IsResidual then
              CurrArc^.Push(wMin(CurrNode^.Excess, CurrArc^.ResCap));
            Inc(CurrArc);
          end;
        if CurrNode = StackBottom then
          break
        else
          CurrNode := CurrNode^.OrderNext;
      until False;
    end;
  Result := CreateEdges;
end;

function TGIntWeightDiGraph.THPrHelper.GetMaxFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt): TWeight;
begin
  CreateResudualGraph(aGraph, aSource, aSink);
  HiLevelPushRelabel;
  Result := FSink^.Excess;
end;

function TGIntWeightDiGraph.THPrHelper.GetMaxFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  out a: TEdgeArray): TWeight;
begin
  CreateResudualGraphCap(aGraph, aSource, aSink, MaxWeight);
  HiLevelPushRelabel;
  FLevels := nil;
  Result := FSink^.Excess;
  a := RecoverFlow;
end;

function TGIntWeightDiGraph.THPrHelper.GetMinCut(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  out s: TIntArray): TWeight;
var
  I, J: SizeInt;
begin
  CreateResudualGraph(aGraph, aSource, aSink);
  HiLevelPushRelabel;
  FLevels := nil;
  Result := FSink^.Excess;
  System.SetLength(s, ARRAY_INITIAL_SIZE);
  J := 0;
  for I := 0 to System.High(FNodes) do
    if FNodes[I].Distance = FNodeCount then
      begin
        if System.Length(s) = J then
          System.SetLength(s, J shl 1);
        s[J] := I;
        Inc(J);
      end;
  System.SetLength(s, J);
end;

function TGIntWeightDiGraph.THPrHelper.GetFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight; out m: TWeightArcMap): TWeight;
var
  e: TWeightEdge;
begin
  CreateResudualGraphCap(aGraph, aSource, aSink, aReqFlow);
  HiLevelPushRelabel;
  FLevels := nil;
  Result := FSink^.Excess;
  for e in RecoverFlow do
    m.Add(TIntEdge.Create(e.Source, e.Destination), e.Weight);
end;

{ TGIntWeightDiGraph.TDinitzHelper.TArc }

constructor TGIntWeightDiGraph.TDinitzHelper.TArc.Create(aTarget: PNode; aReverse: PArc; aCap: TWeight);
begin
  Target := aTarget;
  Reverse := aReverse;
  ResCap := aCap;
  IsForward := True;
end;

constructor TGIntWeightDiGraph.TDinitzHelper.TArc.CreateReverse(aTarget: PNode; aReverse: PArc);
begin
  Target := aTarget;
  Reverse := aReverse;
  ResCap := 0;
  IsForward := False;
end;

procedure TGIntWeightDiGraph.TDinitzHelper.TArc.Push(aFlow: TWeight);
begin
  ResCap -= aFlow;
  Reverse^.ResCap += aFlow;
end;

{ TGIntWeightDiGraph.TDinitzHelper.TNode }

procedure TGIntWeightDiGraph.TDinitzHelper.TNode.ResetCurrent;
begin
  CurrentArc := FirstArc;
end;

{ TGIntWeightDiGraph.TDinitzHelper }

procedure TGIntWeightDiGraph.TDinitzHelper.CreateResudualGraph(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt);
var
  CurrArcIdx: TIntArray;
  I, J: SizeInt;
  p: PAdjItem;
  Cap: TWeight;
begin
  FScaleFactor := 8;
  FNodeCount := aGraph.VertexCount;
  System.SetLength(CurrArcIdx, FNodeCount);
  J := 0;
  for I := 0 to System.High(CurrArcIdx) do
    begin
      CurrArcIdx[I] := J;
      J += aGraph.DegreeI(I);
    end;

  System.SetLength(FNodes, Succ(FNodeCount));
  FSource := @FNodes[aSource];
  FSink := @FNodes[aSink];
  System.SetLength(FArcs, Succ(aGraph.EdgeCount * 2));

  for I := 0 to Pred(FNodeCount) do
    FNodes[I].FirstArc := @FArcs[CurrArcIdx[I]];

  FMaxCap := 0;
  for I := 0 to Pred(FNodeCount) do
    for p in aGraph.AdjLists[I]^ do
      begin
        J := p^.Destination;
        Cap := p^.Data.Weight;
        if Cap > FMaxCap then
          FMaxCap := Cap;
        FArcs[CurrArcIdx[I]] := TArc.Create(@FNodes[J], @FArcs[CurrArcIdx[J]], Cap);
        FArcs[CurrArcIdx[J]] := TArc.CreateReverse(@FNodes[I], @FArcs[CurrArcIdx[I]]);
        Inc(CurrArcIdx[I]);
        Inc(CurrArcIdx[J]);
      end;

  CurrArcIdx := nil;

  FArcs[System.High(FArcs)] :=
    TArc.Create(@FNodes[FNodeCount], @FArcs[System.High(FArcs)], 0);
  //sentinel node
  FNodes[FNodeCount].FirstArc := @FArcs[System.High(FArcs)];
  FNodes[FNodeCount].CurrentArc := @FArcs[System.High(FArcs)];
  FNodes[FNodeCount].Distance := NULL_INDEX;

  System.SetLength(FQueue, aGraph.VertexCount);
end;

procedure TGIntWeightDiGraph.TDinitzHelper.ClearLabels;
var
  I: SizeInt;
begin
  for I := 0 to Pred(FNodeCount) do
    FNodes[I].Distance := NULL_INDEX;
  FSource^.Distance := 0;
  FSource^.ResetCurrent;
end;

function TGIntWeightDiGraph.TDinitzHelper.Relabel: Boolean;
var
  Curr, Next: PNode;
  CurrArc: PArc;
  Dist: SizeInt;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  ClearLabels;
  FQueue[qTail] := FSource;
  Inc(qTail);
  while (qHead < qTail) and (FSink^.Distance = NULL_INDEX) do
    begin
      Curr := FQueue[qHead];
      Inc(qHead);
      Dist := Succ(Curr^.Distance);
      CurrArc := Curr^.FirstArc;
      while CurrArc < (Curr + 1)^.FirstArc do
        begin
          Next := CurrArc^.Target;
          if (Next^.Distance = NULL_INDEX) and (CurrArc^.ResCap > FMaxCap) then
            begin
              Next^.ResetCurrent;
              Next^.Distance := Dist;
              FQueue[qTail] := Next;
              Inc(qTail);
            end;
          Inc(CurrArc);
        end;
    end;
  Result := FSink^.Distance <> NULL_INDEX;
end;

function TGIntWeightDiGraph.TDinitzHelper.Dfs(aRoot: PNode; aFlow: TWeight): TWeight;
var
  Flow: TWeight;
begin
  //todo: non-recursive dfs
  if aFlow > 0 then
    if aRoot <> FSink then
      while aRoot^.CurrentArc < (aRoot + 1)^.FirstArc do
        begin
          if aRoot^.CurrentArc^.Target^.Distance = Succ(aRoot^.Distance) then
            begin
              Flow := Dfs(aRoot^.CurrentArc^.Target, wMin(aFlow, aRoot^.CurrentArc^.ResCap));
              if Flow > 0 then
                begin
                  aRoot^.CurrentArc^.Push(Flow);
                  exit(Flow);
                end;
            end;
          Inc(aRoot^.CurrentArc);
        end
    else
      exit(aFlow);
  Result := 0;
end;

function TGIntWeightDiGraph.TDinitzHelper.DinitzMaxFlow: TWeight;
var
  Flow: TWeight;
begin
  Result := 0;
  while FMaxCap >= 1 do
    begin
      FMaxCap := FMaxCap div FScaleFactor;
      while Relabel do
        repeat
          Flow := Dfs(FSource, InfWeight);
          Result += Flow;
        until Flow = 0;
    end;
end;

function TGIntWeightDiGraph.TDinitzHelper.CreateEdges(aGraph: TGIntWeightDiGraph): TEdgeArray;
var
  I, J: SizeInt;
  CurrArc: PArc;
begin
  System.SetLength(Result, aGraph.EdgeCount);
  J := 0;
  for I := 0 to Pred(FNodeCount) do
    begin
      CurrArc := FNodes[I].FirstArc;
      while CurrArc < FNodes[Succ(I)].FirstArc do
        begin
          if CurrArc^.IsForward then
            begin
              Result[J] :=
                TWeightEdge.Create(I, CurrArc^.Target - PNode(FNodes), CurrArc^.Reverse^.ResCap);
              Inc(J);
            end;
          Inc(CurrArc);
        end;
    end;
end;

function TGIntWeightDiGraph.TDinitzHelper.GetMaxFlow(aGraph: TGIntWeightDiGraph; aSource,
  aSink: SizeInt): TWeight;
begin
  CreateResudualGraph(aGraph, aSource, aSink);
  Result := DinitzMaxFlow;
end;

function TGIntWeightDiGraph.TDinitzHelper.GetMaxFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  out a: TEdgeArray): TWeight;
begin
  CreateResudualGraph(aGraph, aSource, aSink);
  Result := DinitzMaxFlow;
  a := CreateEdges(aGraph);
end;

function TGIntWeightDiGraph.TDinitzHelper.GetMinCut(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  out s: TIntArray): TWeight;
var
  I, J: SizeInt;
begin
  CreateResudualGraph(aGraph, aSource, aSink);
  Result := DinitzMaxFlow;
  System.SetLength(s, ARRAY_INITIAL_SIZE);
  J := 0;
  for I := 0 to System.High(FNodes) do
    if FNodes[I].Distance <> NULL_INDEX then
      begin
        if System.Length(s) = J then
          System.SetLength(s, J shl 1);
        s[J] := I;
        Inc(J);
      end;
  System.SetLength(s, J);
end;

{ TGIntWeightDiGraph.TBgMcfHelper.TArc }

constructor TGIntWeightDiGraph.TBgMcfHelper.TArc.Create(aTarget: PNode; aReverse: PArc; aCap: TWeight; aCost: TCost);
begin
  Target := aTarget;
  Reverse := aReverse;
  ResCap := aCap;
  Cost := aCost;
  IsForward := True;
end;

constructor TGIntWeightDiGraph.TBgMcfHelper.TArc.CreateReverse(aTarget: PNode; aReverse: PArc; aCost: TCost);
begin
  Target := aTarget;
  Reverse := aReverse;
  ResCap := 0;
  Cost := -aCost;
  IsForward := False;
end;

function TGIntWeightDiGraph.TBgMcfHelper.TArc.IsResidual: Boolean;
begin
  Result := ResCap > 0;
end;

procedure TGIntWeightDiGraph.TBgMcfHelper.TArc.Push(aFlow: TWeight);
begin
  ResCap -= aFlow;
  Reverse^.ResCap += aFlow;
end;

{ TGIntWeightDiGraph.TBgMcfHelper }

procedure TGIntWeightDiGraph.TBgMcfHelper.CreateResudualGraph(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight; var aCosts: TEdgeCostMap);
var
  CurrArcIdx: TIntArray;
  I, J: SizeInt;
  c: TCost;
  p: PAdjItem;
begin
  FNodeCount := aGraph.VertexCount;
  FRequestFlow := aReqFlow;
  System.SetLength(CurrArcIdx, FNodeCount);
  J := 0;
  for I := 0 to System.High(CurrArcIdx) do
    begin
      CurrArcIdx[I] := J;
      J += aGraph.DegreeI(I);
    end;

  System.SetLength(FNodes, Succ(FNodeCount));
  FSource := @FNodes[aSource];
  FSink := @FNodes[aSink];
  System.SetLength(FArcs, Succ(aGraph.EdgeCount * 2));

  for I := 0 to Pred(FNodeCount) do
    FNodes[I].FirstArc := @FArcs[CurrArcIdx[I]];

  for I := 0 to Pred(FNodeCount) do
    for p in aGraph.AdjLists[I]^ do
      begin
        J := p^.Destination;
        c := aCosts[TIntEdge.Create(I, J)];
        FArcs[CurrArcIdx[I]] := TArc.Create(@FNodes[J], @FArcs[CurrArcIdx[J]], p^.Data.Weight, c);
        FArcs[CurrArcIdx[J]] := TArc.CreateReverse(@FNodes[I], @FArcs[CurrArcIdx[I]], c);
        Inc(CurrArcIdx[I]);
        Inc(CurrArcIdx[J]);
      end;

  CurrArcIdx := nil;
  Finalize(aCosts);

  FArcs[System.High(FArcs)] :=
    TArc.Create(@FNodes[FNodeCount], @FArcs[System.High(FArcs)], 0, 0);
  //sentinel node
  FNodes[FNodeCount].FirstArc := @FArcs[System.High(FArcs)];
  FNodes[FNodeCount].PathArc := nil;
  FNodes[FNodeCount].Parent := nil;
  FNodes[FNodeCount].Price := 0;
  FNodes[FNodeCount].PathMinCap := 0;

  FInQueue.Size := FNodeCount;
  FReached.Size := FNodeCount;
  FQueue := THeap.Create(FNodeCount);
end;

procedure TGIntWeightDiGraph.TBgMcfHelper.SearchInit;
var
  I: SizeInt;
begin
  for I := 0 to Pred(FNodeCount) do
    FNodes[I].Price := MaxCost;
  FSource^.Price := 0;
  FSource^.PathMinCap := MaxWeight;
  FSink^.Parent := nil;
end;

function TGIntWeightDiGraph.TBgMcfHelper.FindNegCycle(out aMinCap: TWeight): Boolean;
var
  Queue: TDeque;
  Dist: TIntArray;
  CurrNode, NextNode, TopNode: PNode;
  CurrArc: PArc;
  d: SizeInt;
begin
  SearchInit;
  System.SetLength(Dist, FNodeCount);
  CurrNode := FSource;
  Dist[FSource - PNode(FNodes)] := 0;
  aMinCap := 0;
  repeat
    FInQueue[CurrNode - PNode(FNodes)] := False;
    if (CurrNode^.Parent <> nil) and FInQueue[CurrNode^.Parent - PNode(FNodes)] then
      continue;
    d := Succ(Dist[CurrNode - PNode(FNodes)]);
    CurrArc := CurrNode^.FirstArc;
    while CurrArc < (CurrNode + 1)^.FirstArc do
      begin
        if CurrArc^.IsResidual then
          begin
            NextNode := CurrArc^.Target;
            if CurrNode^.Price + CurrArc^.Cost < NextNode^.Price then
              begin
                NextNode^.Price := CurrNode^.Price + CurrArc^.Cost;
                NextNode^.PathMinCap := wMin(CurrNode^.PathMinCap, CurrArc^.ResCap);
                NextNode^.Parent := CurrNode;
                NextNode^.PathArc := CurrArc;
                if (NextNode = FSource) or (d >= FNodeCount) then
                  exit(True);
                Dist[NextNode - PNode(FNodes)] := d;
                if not FInQueue[NextNode - PNode(FNodes)] then
                  begin
                    if Queue.TryPeekFirst(TopNode) and (NextNode^.Price <{%H-}TopNode^.Price) then
                      Queue.PushFirst(NextNode)
                    else
                      Queue.PushLast(NextNode);
                    FInQueue[NextNode - PNode(FNodes)] := True;
                  end;
              end;
          end;
        Inc(CurrArc);
      end;
  until not Queue.TryPopFirst(CurrNode);
  Result := FSink^.Parent = nil;
  if not Result then
    aMinCap := FSink^.PathMinCap;
end;

function TGIntWeightDiGraph.TBgMcfHelper.FindShortestPath(out aMinCap: TWeight): Boolean;
var
  CurrNode, NextNode, TopNode: PNode;
  CurrArc: PArc;
  Item: TCostItem;
  Price: TCost;
  I: SizeInt;
begin
  FInQueue.ClearBits;
  FReached.ClearBits;
  FQueue.MakeEmpty;
  FSource^.PathArc := nil;
  FSource^.PathMinCap := MaxWeight;
  aMinCap := 0;
  Item := TCostItem.Create(FSource - PNode(FNodes), 0);
  repeat
    CurrNode := @FNodes[Item.Index];
    FNodes[Item.Index].Price += Item.Cost;
    FReached[Item.Index] := True;
    if CurrNode = FSink then
      break;
    CurrArc := CurrNode^.FirstArc;
    while CurrArc < (CurrNode + 1)^.FirstArc do
      begin
        if CurrArc^.IsResidual then
          begin
            NextNode := CurrArc^.Target;
            if not FReached[NextNode - PNode(FNodes)] then
              begin
                I := NextNode - PNode(FNodes);
                Price := CurrNode^.Price + CurrArc^.Cost - NextNode^.Price;
                if not FInQueue[I] then
                  begin
                    NextNode^.PathMinCap := wMin(CurrNode^.PathMinCap, CurrArc^.ResCap);
                    NextNode^.Parent := CurrNode;
                    NextNode^.PathArc := CurrArc;
                    FQueue.Enqueue(I, TCostItem.Create(I, Price));
                    FInQueue[I] := True;
                  end
                else
                  if Price < FQueue.HeadPtr(I)^.Cost then
                    begin
                      NextNode^.PathMinCap := wMin(CurrNode^.PathMinCap, CurrArc^.ResCap);
                      NextNode^.Parent := CurrNode;
                      NextNode^.PathArc := CurrArc;
                      FQueue.Update(I, TCostItem.Create(I, Price));
                    end;
              end;
          end;
        Inc(CurrArc);
      end;
  until not FQueue.TryDequeue(Item);

  Result := FReached[FSink - PNode(FNodes)];
  if Result then
    begin
      for I in FReached do
        FNodes[I].Price -= Item.Cost;
      aMinCap := FSink^.PathMinCap;
    end;
end;

procedure TGIntWeightDiGraph.TBgMcfHelper.FlowIn(aFlow: TWeight);
var
  ParentNode: PNode;
  CurrArc: PArc;
begin
  ParentNode := FSink^.Parent;
  CurrArc := FSink^.PathArc;
  while CurrArc <> nil do
    begin
      CurrArc^.Push(aFlow);
      CurrArc := ParentNode^.PathArc;
      ParentNode := ParentNode^.Parent;
    end;
end;

function TGIntWeightDiGraph.TBgMcfHelper.MinCostFlow: TWeight;
var
  Flow: TWeight;
begin
  Result := 0;
  if FindNegCycle(Flow) then
    exit(0);
  Flow := wMin(Flow, FRequestFlow);
  repeat
    FlowIn(Flow);
    Result += Flow;
    Flow := 0;
    if FindShortestPath(Flow) then
      Flow := wMin(Flow, FRequestFlow - Result);
  until Flow = 0;
end;

function TGIntWeightDiGraph.TBgMcfHelper.GetTotalCost: TCost;
var
  I: SizeInt;
  CurrArc: PArc;
begin
  Result := 0;
  for I := 0 to Pred(FNodeCount) do
    begin
      CurrArc := FNodes[I].FirstArc;
      while CurrArc < FNodes[Succ(I)].FirstArc do
        begin
          if CurrArc^.IsForward then
            Result += CurrArc^.Reverse^.ResCap * CurrArc^.Cost;
          Inc(CurrArc);
        end;
    end;
end;

function TGIntWeightDiGraph.TBgMcfHelper.CreateEdges(out aTotalCost: TCost): TEdgeArray;
var
  I, J, Dst: SizeInt;
  CurrArc: PArc;
  w: TWeight;
begin
  System.SetLength(Result, Pred(System.Length(FArcs)) shr 1);
  J := 0;
  aTotalCost := 0;
  for I := 0 to Pred(FNodeCount) do
    begin
      CurrArc := FNodes[I].FirstArc;
      while CurrArc < FNodes[Succ(I)].FirstArc do
        begin
          if CurrArc^.IsForward then
            begin
              Dst := CurrArc^.Target - PNode(FNodes);
              w := CurrArc^.Reverse^.ResCap;
              aTotalCost += w * CurrArc^.Cost;
              Result[J] := TWeightEdge.Create(I, Dst, w);
              Inc(J);
            end;
          Inc(CurrArc);
        end;
    end;
end;

function TGIntWeightDiGraph.TBgMcfHelper.GetMinCostFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight; var aCosts: TEdgeCostMap; out aTotalCost: TCost): TWeight;
begin
  if aReqFlow <= 0 then
    exit(0);
  CreateResudualGraph(aGraph, aSource, aSink, aReqFlow, aCosts);
  Result := MinCostFlow;
  if Result > 0 then
    aTotalCost := GetTotalCost;
end;

function TGIntWeightDiGraph.TBgMcfHelper.GetMinCostFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight; var aCosts: TEdgeCostMap; out aTotalCost: TCost; out a: TEdgeArray): TWeight;
begin
  if aReqFlow <= 0 then
    exit(0);
  CreateResudualGraph(aGraph, aSource, aSink, aReqFlow, aCosts);
  Result := MinCostFlow;
  if Result > 0 then
    a := CreateEdges(aTotalCost);
end;

{ TGIntWeightDiGraph.TCsMcfHelper.TArc }

constructor TGIntWeightDiGraph.TCsMcfHelper.TArc.Create(aTarget: PNode; aReverse: PArc; aCap: TWeight;
  aCost: TCost);
begin
  Target := aTarget;
  Reverse := aReverse;
  ResCap := aCap;
  Cost := aCost;
  IsForward := True;
end;

constructor TGIntWeightDiGraph.TCsMcfHelper.TArc.CreateReverse(aTarget: PNode; aReverse: PArc; aCost: TCost);
begin
  Target := aTarget;
  Reverse := aReverse;
  ResCap := 0;
  Cost := -aCost;
  IsForward := False;
end;

procedure TGIntWeightDiGraph.TCsMcfHelper.TArc.Push(aFlow: TWeight);
begin
  ResCap -= aFlow;
  Target^.Excess += aFlow;
  Reverse^.ResCap += aFlow;
  Reverse^.Target^.Excess -= aFlow;
end;

{ TGIntWeightDiGraph.TCsMcfHelper.TNode }

procedure TGIntWeightDiGraph.TCsMcfHelper.TNode.ResetCurrent;
begin
  CurrArc := FirstArc;
end;

{ TGIntWeightDiGraph.TCsMcfHelper }

procedure TGIntWeightDiGraph.TCsMcfHelper.CreateResidualGraph(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight; var aCosts: TEdgeCostMap);
var
  CurrArcIdx: TIntArray;
  I, J: SizeInt;
  c: TCost;
  p: PAdjItem;
begin
  FAlpha := 8;
  FNodeCount := aGraph.VertexCount;
  FGraph := aGraph;
  Factor := FAlpha * FNodeCount;
  FRequestFlow := aReqFlow;
  System.SetLength(CurrArcIdx, FNodeCount);
  J := 0;
  for I := 0 to System.High(CurrArcIdx) do
    begin
      CurrArcIdx[I] := J;
      J += aGraph.DegreeI(I);
    end;

  System.SetLength(FNodes, Succ(FNodeCount));
  FSource := @FNodes[aSource];
  FSink := @FNodes[aSink];
  System.SetLength(FArcs, Succ(aGraph.EdgeCount * 2));

  for I := 0 to Pred(FNodeCount) do
    begin
      FNodes[I].FirstArc := @FArcs[CurrArcIdx[I]];
      FNodes[I].CurrArc := @FArcs[CurrArcIdx[I]];
      FNodes[I].Price := 0;
      FNodes[I].Excess := 0;
    end;

  for I := 0 to Pred(FNodeCount) do
    for p in aGraph.AdjLists[I]^ do
      begin
        J := p^.Destination;
        c := aCosts[TIntEdge.Create(I, J)];
        FArcs[CurrArcIdx[I]] := TArc.Create(@FNodes[J], @FArcs[CurrArcIdx[J]], p^.Data.Weight, c);
        FArcs[CurrArcIdx[J]] := TArc.CreateReverse(@FNodes[I], @FArcs[CurrArcIdx[I]], c);
        Inc(CurrArcIdx[I]);
        Inc(CurrArcIdx[J]);
      end;

  CurrArcIdx := nil;
  Finalize(aCosts);

  FArcs[System.High(FArcs)] :=
    TArc.Create(@FNodes[FNodeCount], @FArcs[System.High(FArcs)], 0, 0);
  //sentinel node
  FNodes[FNodeCount].FirstArc := @FArcs[System.High(FArcs)];
  FNodes[FNodeCount].CurrArc :=  @FArcs[System.High(FArcs)];
  FNodes[FNodeCount].Price := 0;
  FNodes[FNodeCount].Excess := 0;

  FInQueue.Size := FNodeCount;
  FQueue.EnsureCapacity(FNodeCount);
end;

function TGIntWeightDiGraph.TCsMcfHelper.NegCycleExists: Boolean;
var
  Dist: TIntArray;
  CurrNode, NextNode: PNode;
  CurrArc: PArc;
  d: SizeInt;
begin
  System.SetLength(Dist, FNodeCount);
  CurrNode := FSource;
  Dist[FSource - PNode(FNodes)] := 0;
  repeat
    FInQueue[CurrNode - PNode(FNodes)] := False;
    d := Succ(Dist[CurrNode - PNode(FNodes)]);
    CurrArc := CurrNode^.FirstArc;
    while CurrArc < (CurrNode + 1)^.FirstArc do
      begin
        if CurrArc^.ResCap > 0 then
          begin
            NextNode := CurrArc^.Target;
            if CurrNode^.Price + CurrArc^.Cost < NextNode^.Price then
              begin
                NextNode^.Price := CurrNode^.Price + CurrArc^.Cost;
                if (NextNode = FSource) or (d >= FNodeCount) then
                  exit(True);
                Dist[NextNode - PNode(FNodes)] := d;
                if not FInQueue[NextNode - PNode(FNodes)] then
                  begin
                    FQueue.Enqueue(NextNode);
                    FInQueue[NextNode - PNode(FNodes)] := True;
                  end;

              end;
          end;
        Inc(CurrArc);
      end;
  until not FQueue.TryDequeue(CurrNode);
  Result := False;
end;

procedure TGIntWeightDiGraph.TCsMcfHelper.InitSolution(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; out
  m: TWeightArcMap);
var
  Helper: THPrHelper;
begin
  FResultFlow := Helper.GetFlow(aGraph, aSource, aSink, FRequestFlow, m);
end;

function TGIntWeightDiGraph.TCsMcfHelper.InitSuccess: Boolean;
var
  ArcMap: TWeightArcMap;
  CurrArc: PArc;
  I, Src, Dst: SizeInt;
  cAbs: TCost;
begin
  FResultFlow := 0;
  if NegCycleExists then
    exit(False);
  InitSolution(FGraph, FSource - PNode(FNodes), FSink - PNode(FNodes), ArcMap);
  if FResultFlow = 0 then
    exit(False);
  for I := 0 to Pred(FNodeCount) do
    FNodes[I].Price := 0;

  FSource^.Excess += FResultFlow;
  FSink^.Excess -= FResultFlow;
  FEpsilon := 0;

  for I := 0 to Pred(System.High(FArcs)) do
    begin
      FArcs[I].Cost *= Factor;
      if FArcs[I].IsForward then
        begin
          cAbs := Abs(FArcs[I].Cost);
          if cAbs > FEpsilon then
            FEpsilon := cAbs;
          Src := FArcs[I].Reverse^.Target - PNode(FNodes);
          Dst := FArcs[I].Target - PNode(FNodes);
          FArcs[I].Push(ArcMap[TIntEdge.Create(Src, Dst)]);
        end;
    end;
  if FEpsilon < 1 then
    FEpsilon := 1;

  Finalize(ArcMap);
  Result := True;
end;

procedure TGIntWeightDiGraph.TCsMcfHelper.InitPhase;
var
  I: SizeInt;
  NextNode: PNode;
  CurrArc: PArc;
begin
  for I := 0 to Pred(FNodeCount) do
    begin
      CurrArc := FNodes[I].FirstArc;
      FNodes[I].ResetCurrent;
      while CurrArc < FNodes[Succ(I)].FirstArc do
        begin
          NextNode := CurrArc^.Target;
          if (CurrArc^.ResCap > 0) and (FNodes[I].Price + CurrArc^.Cost - NextNode^.Price < 0) then
            begin
              CurrArc^.Push(CurrArc^.ResCap);
              if NextNode^.Excess > 0 then
                FQueue.Enqueue(NextNode);
            end;
          Inc(CurrArc);
        end;
    end;
end;

procedure TGIntWeightDiGraph.TCsMcfHelper.Discharge(aNode: PNode);
var
  CurrArc, NextArc: PArc;
  MinPrice: TCost;
begin
  while aNode^.Excess > 0 do
    begin
      while aNode^.CurrArc < (aNode + 1)^.FirstArc do
        begin
          CurrArc := aNode^.CurrArc;
          if (CurrArc^.ResCap > 0) and (aNode^.Price + CurrArc^.Cost - CurrArc^.Target^.Price < 0 ) then
            begin
              CurrArc^.Push(wMin(aNode^.Excess, CurrArc^.ResCap));
              if CurrArc^.Target^.Excess > 0 then
                FQueue.Enqueue(CurrArc^.Target);
            end;
          Inc(aNode^.CurrArc);
        end;
      if aNode^.Excess > 0 then
        begin
          CurrArc := aNode^.FirstArc;
          aNode^.ResetCurrent;
          MinPrice := MaxCost;
          while CurrArc < (aNode + 1)^.FirstArc do
            begin
              if (CurrArc^.ResCap > 0) and (aNode^.Price + CurrArc^.Cost - CurrArc^.Target^.Price < MinPrice) then
                MinPrice := aNode^.Price + CurrArc^.Cost - CurrArc^.Target^.Price;
              Inc(CurrArc);
            end;
          aNode^.Price -= MinPrice + FEpsilon;
        end;
    end;
end;

procedure TGIntWeightDiGraph.TCsMcfHelper.Done;
var
  I: SizeInt;
begin
  for I := 0 to Pred(System.High(FArcs)) do
    if FArcs[I].IsForward then
      FArcs[I].Cost := FArcs[I].Cost div Factor;
end;

procedure TGIntWeightDiGraph.TCsMcfHelper.OptimizeFlow;
var
  Node: PNode;
begin
  while True do
    begin
      FEpsilon := Math.Max(FEpsilon div FAlpha, 1);
      InitPhase;
      while FQueue.TryDequeue(Node) do
        Discharge(Node);
      if FEpsilon = 1 then
        break;
    end;
  Done;
end;

function TGIntWeightDiGraph.TCsMcfHelper.GetTotalCost: TCost;
var
  I: SizeInt;
  CurrArc: PArc;
begin
  Result := 0;
  for I := 0 to Pred(FNodeCount) do
    begin
      CurrArc := FNodes[I].FirstArc;
      while CurrArc < FNodes[Succ(I)].FirstArc do
        begin
          if CurrArc^.IsForward then
            Result += CurrArc^.Reverse^.ResCap * CurrArc^.Cost;
          Inc(CurrArc);
        end;
    end;
end;

function TGIntWeightDiGraph.TCsMcfHelper.CreateEdges(out aTotalCost: TCost): TEdgeArray;
var
  I, J, Dst: SizeInt;
  CurrArc: PArc;
  w: TWeight;
begin
  System.SetLength(Result, Pred(System.Length(FArcs)) shr 1);
  J := 0;
  aTotalCost := 0;
  for I := 0 to Pred(FNodeCount) do
    begin
      CurrArc := FNodes[I].FirstArc;
      while CurrArc < FNodes[Succ(I)].FirstArc do
        begin
          if CurrArc^.IsForward then
            begin
              Dst := CurrArc^.Target - PNode(FNodes);
              w := CurrArc^.Reverse^.ResCap;
              aTotalCost += w * CurrArc^.Cost;
              Result[J] := TWeightEdge.Create(I, Dst, w);
              Inc(J);
            end;
          Inc(CurrArc);
        end;
    end;
end;

function TGIntWeightDiGraph.TCsMcfHelper.GetMinCostFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight; var aCosts: TEdgeCostMap; out aTotalCost: TCost): TWeight;
begin
  if aReqFlow <= 0 then
    exit(0);
  CreateResidualGraph(aGraph, aSource, aSink, aReqFlow, aCosts);
  if not InitSuccess then
    exit(0);
  OptimizeFlow;
  Result := FResultFlow;
  aTotalCost := GetTotalCost;
end;

function TGIntWeightDiGraph.TCsMcfHelper.GetMinCostFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight; var aCosts: TEdgeCostMap; out aTotalCost: TCost; out a: TEdgeArray): TWeight;
begin
  if aReqFlow <= 0 then
    exit(0);
  CreateResidualGraph(aGraph, aSource, aSink, aReqFlow, aCosts);
  if not InitSuccess then
    exit(0);
  OptimizeFlow;
  Result := FResultFlow;
  a := CreateEdges(aTotalCost);
end;

{ TGIntWeightDiGraph }

function TGIntWeightDiGraph.IsCostArrayCorrect(constref aCosts: TCostEdgeArray; out aMap: TEdgeCostMap): Boolean;
var
  Arc: TCostEdge;
  vCount: SizeUInt;
begin
  aMap.Clear;
  if IsEmpty then
    exit(aCosts = nil);
  if System.Length(aCosts) <> EdgeCount then
    exit(False);
  vCount := SizeUInt(VertexCount);
  aMap.EnsureCapacity(EdgeCount);
  for Arc in aCosts do
    begin
      if (SizeUInt(Arc.Source) >= vCount) or (SizeUInt(Arc.Destination) >= vCount) then //invalid arc
        exit(False);
      if not AdjLists[Arc.Source]^.Contains(Arc.Destination) then //no such arc
        exit(False);
      if not aMap.Add(TIntEdge.Create(Arc.Source, Arc.Destination), Arc.Cost) then //contains duplicates
        exit(False);
    end;
  Result := True;
end;

function TGIntWeightDiGraph.GetNetworkState(constref aSource, aSink: TVertex): TNetworkState;
begin
  Result := GetNetworkStateI(IndexOf(aSource), IndexOf(aSink));
end;

function TGIntWeightDiGraph.GetNetworkStateI(aSrcIndex, aSinkIndex: SizeInt): TNetworkState;
var
  Queue: TIntArray;
  Visited: TBitVector;
  Curr: SizeInt;
  p: PAdjItem;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
  SinkFound: Boolean = False;
begin
  CheckIndexRange(aSrcIndex);
  CheckIndexRange(aSinkIndex);
  if VertexCount < 2 then
    exit(nsTrivial);
  if not IsSourceI(aSrcIndex) then
    exit(nsInvalidSource);
  if not IsSinkI(aSinkIndex) then
    exit(nsInvalidSink);
  Queue := CreateIntArray;
  Visited.Size := VertexCount;
  Visited[aSrcIndex] := True;
  Queue[qTail] := aSrcIndex;
  Inc(qTail);
  while qHead < qTail do
    begin
      Curr := Queue[qHead];
      Inc(qHead);
      for p in AdjLists[Curr]^ do
        begin
          if p^.Data.Weight < 0 then // network should not contains arcs with negative capacity
            exit(nsNegCapacity);
          if not Visited[p^.Destination] and (p^.Data.Weight > 0) then
            begin
              Queue[qTail] := p^.Destination;
              Inc(qTail);
              Visited[p^.Destination] := True;
              SinkFound := SinkFound or (p^.Destination = aSinkIndex);
            end;
        end;
    end;
  if not SinkFound then // sink should be reachable from the source
    exit(nsSinkUnreachable);
  Result := nsOk;
end;

function TGIntWeightDiGraph.FindMaxFlowPr(constref aSource, aSink: TVertex; out aFlow: TWeight): TNetworkState;
begin
  Result := FindMaxFlowPrI(IndexOf(aSource), IndexOf(aSink), aFlow);
end;

function TGIntWeightDiGraph.FindMaxFlowPrI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight): TNetworkState;
var
  Helper: THPrHelper;
begin
  aFlow := 0;
  Result := GetNetworkStateI(aSrcIndex, aSinkIndex);
  if Result <> nsOk then
    exit;
  aFlow := Helper.GetMaxFlow(Self, aSrcIndex, aSinkIndex);
end;

function TGIntWeightDiGraph.FindMaxFlowPr(constref aSource, aSink: TVertex; out aFlow: TWeight;
  out a: TEdgeArray): TNetworkState;
begin
  Result := FindMaxFlowPrI(IndexOf(aSource), IndexOf(aSink), aFlow, a);
end;

function TGIntWeightDiGraph.FindMaxFlowPrI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight;
  out a: TEdgeArray): TNetworkState;
var
  Helper: THPrHelper;
begin
  aFlow := 0;
  a := nil;
  Result := GetNetworkStateI(aSrcIndex, aSinkIndex);
  if Result <> nsOk then
    exit;
  aFlow := Helper.GetMaxFlow(Self, aSrcIndex, aSinkIndex, a);
end;

function TGIntWeightDiGraph.FindMaxFlowD(constref aSource, aSink: TVertex; out aFlow: TWeight): TNetworkState;
begin
  Result := FindMaxFlowDI(IndexOf(aSource), IndexOf(aSink), aFlow);
end;

function TGIntWeightDiGraph.FindMaxFlowDI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight): TNetworkState;
var
  Helper: TDinitzHelper;
begin
  aFlow := 0;
  Result := GetNetworkStateI(aSrcIndex, aSinkIndex);
  if Result <> nsOk then
    exit;
  aFlow := Helper.GetMaxFlow(Self, aSrcIndex, aSinkIndex);
end;

function TGIntWeightDiGraph.FindMaxFlowD(constref aSource, aSink: TVertex; out aFlow: TWeight;
  out a: TEdgeArray): TNetworkState;
begin
  Result := FindMaxFlowDI(IndexOf(aSource), IndexOf(aSink), aFlow, a);
end;

function TGIntWeightDiGraph.FindMaxFlowDI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight;
  out a: TEdgeArray): TNetworkState;
var
  Helper: TDinitzHelper;
begin
  aFlow := 0;
  a := nil;
  Result := GetNetworkStateI(aSrcIndex, aSinkIndex);
  if Result <> nsOk then
    exit;
  aFlow := Helper.GetMaxFlow(Self, aSrcIndex, aSinkIndex, a);
end;

function TGIntWeightDiGraph.IsFeasibleFlow(constref aSource, aSink: TVertex; constref a: TEdgeArray): Boolean;
begin
  Result := IsFeasibleFlowI(IndexOf(aSource), IndexOf(aSink), a);
end;

function TGIntWeightDiGraph.IsFeasibleFlowI(aSrcIndex, aSinkIndex: SizeInt; constref a: TEdgeArray): Boolean;
var
  v: array of TWeight;
  e: TWeightEdge;
  d: TEdgeData;
  I: SizeInt;
begin
  CheckIndexRange(aSrcIndex);
  CheckIndexRange(aSinkIndex);
  if System.Length(a) <> EdgeCount then
    exit(False);
  v := TPathHelper.CreateWeightArrayZ(VertexCount);
  for e in a do
    begin
      if not GetEdgeDataI(e.Source, e.Destination, d) then
        exit(False);
      if e.Weight > d.Weight then
        exit(False);
      v[e.Source] -= e.Weight;
      v[e.Destination] += e.Weight;
    end;
  for I := 0 to System.High(v) do
    if (I <> aSrcIndex) and (I <> aSinkIndex) then
      if v[I] <> 0 then
        exit(False);
  Result := True;
end;

function TGIntWeightDiGraph.FindMinSTCutPr(constref aSource, aSink: TVertex; out aValue: TWeight;
  out aCut: TStCut): TNetworkState;
begin
  Result := FindMinSTCutPrI(IndexOf(aSource), IndexOf(aSink), aValue, aCut);
end;

function TGIntWeightDiGraph.FindMinSTCutPrI(aSrcIndex, aSinkIndex: SizeInt; out aValue: TWeight;
  out aCut: TStCut): TNetworkState;
var
  Helper: THPrHelper;
  TmpSet: TBoolVector;
  I: SizeInt;
begin
  aValue := 0;
  aCut.S := [];
  aCut.T := [];
  CheckIndexRange(aSrcIndex);
  CheckIndexRange(aSinkIndex);
  Result := GetNetworkStateI(aSrcIndex, aSinkIndex);
  if Result <> nsOk then
    exit;
  aValue := Helper.GetMinCut(Self, aSrcIndex, aSinkIndex, aCut.S);
  TmpSet.InitRange(VertexCount);
  for I in aCut.S do
    TmpSet[I] := False;
  aCut.T := TmpSet.ToArray;
end;

function TGIntWeightDiGraph.FindMinSTCutD(constref aSource, aSink: TVertex; out aValue: TWeight;
  out aCut: TStCut): TNetworkState;
begin
  Result := FindMinSTCutDI(IndexOf(aSource), IndexOf(aSink), aValue, aCut);
end;

function TGIntWeightDiGraph.FindMinSTCutDI(aSrcIndex, aSinkIndex: SizeInt; out aValue: TWeight;
  out aCut: TStCut): TNetworkState;
var
  Helper: TDinitzHelper;
  TmpSet: TBoolVector;
  I: SizeInt;
begin
  aValue := 0;
  aCut.S := [];
  aCut.T := [];
  CheckIndexRange(aSrcIndex);
  CheckIndexRange(aSinkIndex);
  Result := GetNetworkStateI(aSrcIndex, aSinkIndex);
  if Result <> nsOk then
    exit;
  aValue := Helper.GetMinCut(Self, aSrcIndex, aSinkIndex, aCut.S);
  TmpSet.InitRange(VertexCount);
  for I in aCut.S do
    TmpSet[I] := False;
  aCut.T := TmpSet.ToArray;
end;

function TGIntWeightDiGraph.IsValidCostArray(constref aCosts: TCostEdgeArray): Boolean;
var
  m: TEdgeCostMap;
begin
  Result := IsCostArrayCorrect(aCosts, m);
end;

function TGIntWeightDiGraph.FindMinCostFlow(constref aSource, aSink: TVertex; constref aCosts: TCostEdgeArray;
  var aReqFlow: TWeight; out aTotalCost: TCost): Boolean;
begin
  Result := FindMinCostFlowI(IndexOf(aSource), IndexOf(aSink), aCosts, aReqFlow, aTotalCost);
end;

function TGIntWeightDiGraph.FindMinCostFlowI(aSrcIndex, aSinkIndex: SizeInt; constref aCosts: TCostEdgeArray;
  var aReqFlow: TWeight; out aTotalCost: TCost): Boolean;
var
  Helper: TBgMcfHelper;
  CostMap: TEdgeCostMap;
begin
  aTotalCost := 0;
  if aReqFlow < 1 then
    exit(False);
  if GetNetworkStateI(aSrcIndex, aSinkIndex) <> nsOk then
    exit(False);
  if not IsCostArrayCorrect(aCosts, CostMap) then
    exit(False);
  aReqFlow := Helper.GetMinCostFlow(Self, aSrcIndex, aSinkIndex, aReqFlow, CostMap, aTotalCost);
  Result := aReqFlow <> 0;
end;

function TGIntWeightDiGraph.FindMinCostFlow(constref aSource, aSink: TVertex; constref aCosts: TCostEdgeArray;
  var aReqFlow: TWeight; out aTotalCost: TCost; out a: TEdgeArray): Boolean;
begin
  Result := FindMinCostFlowI(IndexOf(aSource), IndexOf(aSink), aCosts, aReqFlow, aTotalCost, a);
end;

function TGIntWeightDiGraph.FindMinCostFlowI(aSrcIndex, aSinkIndex: SizeInt; constref aCosts: TCostEdgeArray;
  var aReqFlow: TWeight; out aTotalCost: TCost; out aArcFlows: TEdgeArray): Boolean;
var
  Helper: TBgMcfHelper;
  CostMap: TEdgeCostMap;
begin
  aTotalCost := 0;
  aArcFlows := nil;
  if aReqFlow < 1 then
    exit(False);
  if GetNetworkStateI(aSrcIndex, aSinkIndex) <> nsOk then
    exit(False);
  if not IsCostArrayCorrect(aCosts, CostMap) then
    exit(False);
  aReqFlow := Helper.GetMinCostFlow(Self, aSrcIndex, aSinkIndex, aReqFlow, CostMap, aTotalCost, aArcFlows);
  Result := aReqFlow <> 0;
end;

function TGIntWeightDiGraph.FindMinCostFlowCs(constref aSource, aSink: TVertex; constref aCosts: TCostEdgeArray;
  var aReqFlow: TWeight; out aTotalCost: TCost): Boolean;
begin
  Result := FindMinCostFlowCsI(IndexOf(aSource), IndexOf(aSink), aCosts, aReqFlow, aTotalCost);
end;

function TGIntWeightDiGraph.FindMinCostFlowCsI(aSrcIndex, aSinkIndex: SizeInt; constref aCosts: TCostEdgeArray;
  var aReqFlow: TWeight; out aTotalCost: TCost): Boolean;
var
  Helper: TCsMcfHelper;
  CostMap: TEdgeCostMap;
begin
  aTotalCost := 0;
  if aReqFlow < 1 then
    exit(False);
  if GetNetworkStateI(aSrcIndex, aSinkIndex) <> nsOk then
    exit(False);
  if not IsCostArrayCorrect(aCosts, CostMap) then
    exit(False);
  aReqFlow := Helper.GetMinCostFlow(Self, aSrcIndex, aSinkIndex, aReqFlow, CostMap, aTotalCost);
  Result := aReqFlow <> 0;
end;

function TGIntWeightDiGraph.FindMinCostFlowCs(constref aSource, aSink: TVertex; constref aCosts: TCostEdgeArray;
  var aReqFlow: TWeight; out aTotalCost: TCost; out a: TEdgeArray): Boolean;
begin
  Result := FindMinCostFlowCsI(IndexOf(aSource), IndexOf(aSink), aCosts, aReqFlow, aTotalCost, a);
end;

function TGIntWeightDiGraph.FindMinCostFlowCsI(aSrcIndex, aSinkIndex: SizeInt; constref aCosts: TCostEdgeArray;
  var aReqFlow: TWeight; out aTotalCost: TCost; out aArcFlows: TEdgeArray): Boolean;
var
  Helper: TCsMcfHelper;
  CostMap: TEdgeCostMap;
begin
  aTotalCost := 0;
  aArcFlows := nil;
  if aReqFlow < 1 then
    exit(False);
  if GetNetworkStateI(aSrcIndex, aSinkIndex) <> nsOk then
    exit(False);
  if not IsCostArrayCorrect(aCosts, CostMap) then
    exit(False);
  aReqFlow := Helper.GetMinCostFlow(Self, aSrcIndex, aSinkIndex, aReqFlow, CostMap, aTotalCost, aArcFlows);
  Result := aReqFlow <> 0;
end;

end.

