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
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGVector,
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
  { checks whether exists any cycle in subgraph that reachable from a Root;
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
  private
  type
    TPathHelper = specialize TGWeightedPathHelper<TVertex, TWeight, TEdgeData, TEqRel>;

  public
  type
    TWeightArray = TPathHelper.TWeightArray;
    TEstimate    = TPathHelper.TEstimate;
    TWeightEdge  = TPathHelper.TWeightEdge;
    TEdgeArray   = array of TWeightEdge;

  protected
  type
    { THPrfHelper: an efficient implementation of the push-relabel method for the maximum flow;
      see "On Implementing Push-Relabel Method for the Maximum Flow Problem"
          by B.V. Cherkassky and A.V. Goldberg;
      this is freepascal port of H_PRF developed by Boris Cherkassky and Andrew Goldberg. }
    THPrfHelper = record
    private
    type
      PNode = ^TNode;
      PArc  = ^TArc;

      TArc = record
        ResidualCap: TWeight;
        Target: PNode;       // pointer to target node
        Reverse: PArc;       // pointer to opposite arc
        constructor Create(constref c: TWeight; aTarget: PNode; aReverse: PArc);
        constructor CreateReverse(aTarget: PNode; aReverse: PArc);
        function  Saturated: Boolean; inline;
        function  HasResidual: Boolean; inline;
        procedure Push(constref aFlow: TWeight); inline;
      end;

      TNode = record
      private
        function  GetColor: TVertexColor; inline;
        procedure SetColor(aValue: TVertexColor); inline;
      public
        FirstArc,            // pointer to first incident arc in arcs array
        CurrentArc,          // pointer to current incident arc in arcs array
        LastArc: PArc;       // pointer to last incident arc in arcs array
        LayerNext,           // next node in layer list
        LayerPrev: PNode;    // previous node in layer list
        Distance: SizeInt;   // distance from the sink
        Excess: TWeight;     // excess at the node
        procedure ResetCurrent; inline;
        function  HasExcess: Boolean; inline;
        property  OrderNext: PNode read LayerNext write LayerNext;  // for dfs
        property  Parent: PNode read LayerPrev write LayerPrev;     // for dfs
        property  Color: TVertexColor read GetColor write SetColor; // for dfs
      end;


      TLayer = record
        TopActive,          // head of singly linked list of the nodes with positive excess
        TopIdle: PNode;     // head of doubly linked list of the nodes with zero excess
        function  IsEmpty: Boolean; inline;
        procedure AddActive(aNode: PNode); inline;
        procedure AddIdle(aNode: PNode); inline;
        procedure ActivateIdle(aNode: PNode); inline;
        procedure RemoveIdle(aNode: PNode); inline;
        procedure Clear(aLabel: SizeInt);
      end;

    var
      FNodes: array of TNode;
      FArcs: array of TArc;
      FLayers: array of TLayer;
      FCaps: TWeightArray;
      FQueue: array of PNode;
      FSource,
      FSink: PNode;
      FNodeCount,
      FMaxLayer,                // maximal layer
      FMaxActiveLayer,          // maximal layer with excessed node
      FMinActiveLayer: SizeInt; // minimal layer with excessed node
      procedure Init(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt);
      procedure Init2(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt);
      procedure ClearLabels; inline;
      procedure GlobalRelabel;
      procedure RemoveGap(aLayer: SizeInt);
      function  Push(aNode: PNode): Boolean;
      procedure Relabel(aNode: PNode);
      procedure HiLevelPushRelabel;
      function  CreateEdges: TEdgeArray;
      function  PreflowToFlow: TEdgeArray; //flow recovering
    public
      function  GetMaxFlow(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt): TWeight;
      function  GetMaxFlow(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt; out a: TEdgeArray): TWeight;
      function  GetMinCut(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt; out s: TIntArray): TWeight;
    end;

    { TDinitzHelper: implementation of Dinitz's maxflow algorithm }
    TDinitzHelper = record
    type
      PNode = ^TNode;
      PArc  = ^TArc;

      TArc = record
        ResidualCap: TWeight;
        Target: PNode;       // pointer to target node
        Reverse: PArc;       // pointer to opposite arc
        IsReal: Boolean;
        constructor Create(constref c: TWeight; aTarget: PNode; aReverse: PArc);
        constructor CreateReverse(aTarget: PNode; aReverse: PArc);
        function  HasResidual: Boolean; inline;
        procedure Push(constref aFlow: TWeight); inline;
      end;

      TNode = record
      private
        FirstArc,            // pointer to first incident arc in arcs array
        CurrentArc,          // pointer to current incident arc in arcs array
        LastArc: PArc;       // pointer to last incident arc in arcs array
        Distance: SizeInt;   // distance from the source
        procedure ResetCurrent; inline;
        function  NonLabeled: Boolean; inline;
        function  Labeled: Boolean; inline;
      end;

    var
      FNodes: array of TNode;
      FArcs: array of TArc;
      FQueue: array of PNode;
      FSource,
      FSink: PNode;
      procedure Init(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt);
      procedure ClearLabels; inline;
      function  Bfs: Boolean;
      function  Dfs(aRoot: PNode; var aFlow: TWeight): Boolean;
      function  FindMaxFlow: TWeight;
      function  CreateEdges(aGraph: TGWeightedDiGraph): TEdgeArray;
    public
      function  GetMaxFlow(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt): TWeight;
      function  GetMaxFlow(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt; out a: TEdgeArray): TWeight;
      function  GetMinCut(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt; out s: TIntArray): TWeight;
    end;

    function CreateEdgeArray: TEdgeArray;
    function GetDagMaxPaths(aSrc: SizeInt): TWeightArray;
    function GetDagMaxPaths(aSrc: SizeInt; out aTree: TIntArray): TWeightArray;

  public
{**********************************************************************************************************
  auxiliary utilities
***********************************************************************************************************}

    class function Min(constref L, R: TWeight): TWeight; static; inline;
    class function InfiniteWeight: TWeight; static; inline;
    class function NegInfiniteWeight: TWeight; static; inline;
    class function ZeroWeight: TWeight; static; inline;
  { returns True if exists edge with negative weight }
    function ContainsNegWeightEdge: Boolean;

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
    InfiniteWeight if the vertex is unreachable; used Dijkstra's algorithm  }
    function MinPathsMap(constref aSrc: TVertex): TWeightArray; inline;
    function MinPathsMapI(aSrc: SizeInt): TWeightArray;
  { same as above and in aPathTree returns paths }
    function MinPathsMap(constref aSrc: TVertex; out aPathTree: TIntArray): TWeightArray; inline;
    function MinPathsMapI(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
  { finds the path of minimal weight from a aSrc to aDst if it exists(pathfinding);
    the weights of all edges must be nonnegative;
    returns path weight or InfiniteWeight if the vertex is unreachable; used Dijkstra's algorithm  }
    function MinPathWeight(constref aSrc, aDst: TVertex): TWeight; inline;
    function MinPathWeightI(aSrc, aDst: SizeInt): TWeight;
  { returns the vertex path of minimal weight from a aSrc to aDst, if exists, and its weight in aWeight }
    function MinPath(constref aSrc, aDst: TVertex; out aWeight: TWeight): TIntArray; inline;
    function MinPathI(aSrc, aDst: SizeInt; out aWeight: TWeight): TIntArray;
  { finds the path of minimal weight from a aSrc to aDst if it exists;
    the weights of all edges must be nonnegative; used A* algorithm if aEst <> nil }
    function MinPathAStar(constref aSrc, aDst: TVertex; out aWeight: TWeight; aEst: TEstimate): TIntArray; inline;
    function MinPathAStarI(aSrc, aDst: SizeInt; out aWeight: TWeight; aEst: TEstimate): TIntArray;
  { finds all paths of minimal weight from a given vertex to the remaining vertices in the same
    connected component(SSSP), the weights of the edges can be negative;
    returns False and empty aWeights if there is a negative weight cycle, otherwise
    aWeights will contain in the corresponding component the weight of the minimum path to the vertex or
    InfiniteWeight if the vertex is unreachable; used Bellman–Ford algorithm  }
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
    the path from aSrc to it, or NegInfiniteWeight if it is unreachable from aSrc }
    function DagMaxPathsMap(constref aSrc: TVertex): TWeightArray; inline;
    function DagMaxPathsMapI(aSrc: SizeInt): TWeightArray;
  { same as above and in aPathTree returns paths }
    function DagMaxPathsMap(constref aSrc: TVertex; out aPathTree: TIntArray): TWeightArray; inline;
    function DagMaxPathsMapI(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
  { for an acyclic graph returns an array containing in the corresponding components the maximal weight of
    the path starting with it }
    function DagMaxPaths: TWeightArray;

{**********************************************************************************************************
  networks utilities treat the weight of the arc as its capacity
***********************************************************************************************************}

  type
    TNetworkState = (nwsValid, nwsTrivial, nwsInvalidSource, nwsInvalidSink, nwsAntiParallelArc,
                     nwsNegArcCapacity, nwsSourceOverflow, nwsSinkUnreachable);

    function GetNetworkState(constref aSource, aSink: TVertex): TNetworkState; inline;
    function GetNetworkStateI(aSrcIndex, aSinkIndex: SizeInt): TNetworkState;
  { returns False if GetNetworkState <> nwsValid, used PR algorithm }
    function FindMaxFlowPr(constref aSource, aSink: TVertex; out aFlow: TWeight): Boolean; inline;
    function FindMaxFlowPrI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight): Boolean;
  { returns False if GetNetworkState <> nwsValid, used Dinitz's algorithm }
    function FindMaxFlowD(constref aSource, aSink: TVertex; out aFlow: TWeight): Boolean; inline;
    function FindMaxFlowDI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight): Boolean;
  { returns False if GetNetworkState <> nwsValid, returns flows through the arcs in array a;
    used Dinitz's algorithm }
    function FindMaxFlowD(constref aSource, aSink: TVertex; out aFlow: TWeight; out a: TEdgeArray): Boolean; inline;
    function FindMaxFlowDI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight; out a: TEdgeArray): Boolean;
  { warning: does not checks network state, used PR algorithm }
    function GetMaxFlowPr(constref aSource, aSink: TVertex): TWeight; inline;
    function GetMaxFlowPrI(aSrcIndex, aSinkIndex: SizeInt): TWeight;
  { warning: does not checks network state, used Dinitz's algorithm }
    function GetMaxFlowD(constref aSource, aSink: TVertex): TWeight; inline;
    function GetMaxFlowDI(aSrcIndex, aSinkIndex: SizeInt): TWeight;
  { returns flows through the arcs in array a, used Dinitz's algorithm
    warning: does not checks network state }
    function GetMaxFlowD(constref aSource, aSink: TVertex; out a: TEdgeArray): TWeight; inline;
    function GetMaxFlowDI(aSrcIndex, aSinkIndex: SizeInt; out a: TEdgeArray): TWeight;

  { warning: works correctly only for integer capacities ??? }
    function IsFlowFeasible(constref aSource, aSink: TVertex; constref a: TEdgeArray): Boolean;
    function IsFlowFeasibleI(aSrcIndex, aSinkIndex: SizeInt; constref a: TEdgeArray): Boolean;

  type
    //s-t vertex partition
    TStCut = record
      S,
      T: TIntArray;
    end;

  { returns False if GetNetworkState <> nwsValid, used PR algorithm }
    function FindMinSTCutPr(constref aSource, aSink: TVertex; out aValue: TWeight; out aCut: TStCut): Boolean;
    function FindMinSTCutPrI(aSrcIndex, aSinkIndex: SizeInt; out aValue: TWeight; out aCut: TStCut): Boolean;
  { returns False if GetNetworkState <> nwsValid, used Dinitz's algorithm }
    function FindMinSTCutD(constref aSource, aSink: TVertex; out aValue: TWeight; out aCut: TStCut): Boolean;
    function FindMinSTCutDI(aSrcIndex, aSinkIndex: SizeInt; out aValue: TWeight; out aCut: TStCut): Boolean;
  { warning: does not checks network state, used PR algorithm }
    function GetMinSTCutPr(constref aSource, aSink: TVertex; out aCut: TStCut): TWeight; inline;
    function GetMinSTCutPrI(aSrcIndex, aSinkIndex: SizeInt; out aCut: TStCut): TWeight;
  { warning: does not checks network state, used Dinitz's algorithm }
    function GetMinSTCutD(constref aSource, aSink: TVertex; out aCut: TStCut): TWeight; inline;
    function GetMinSTCutDI(aSrcIndex, aSinkIndex: SizeInt; out aCut: TStCut): TWeight;

  end;


  { TGIntWeightedDiGraph assumes TWeight is integer type }
  generic TGIntWeightedDiGraph<TVertex, TWeight, TEdgeData, TEqRel> = class(
     specialize TGWeightedDiGraph<TVertex, TWeight, TEdgeData, TEqRel>)
{**********************************************************************************************************
  networks utilities treat the weight of the arc as its capacity
***********************************************************************************************************}

  { returns False if GetNetworkState <> nwsValid, returns flows through the arcs in array a;
    used PR algorithm }
    function FindMaxFlowPr(constref aSource, aSink: TVertex; out aFlow: TWeight; out a: TEdgeArray): Boolean; inline;
    function FindMaxFlowPrI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight; out a: TEdgeArray): Boolean;
  { returns flows through the arcs in array a, used PR algorithm;
    warning: does not checks network state }
    function GetMaxFlowPr(constref aSource, aSink: TVertex; out a: TEdgeArray): TWeight; inline;
    function GetMaxFlowPrI(aSrcIndex, aSinkIndex: SizeInt; out a: TEdgeArray): TWeight;
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
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  InStack: TBitVector;
  PreOrd, Parents: TIntArray;
  Counter, Next: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  PreOrd := CreateIntArray;
  Parents := CreateIntArray;
  InStack.Size := VertexCount;
  PreOrd[aRoot] := 0;
  Counter := 1;
  {%H-}Stack.Push(aRoot);
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
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  InStack: TBitVector;
  PreOrd: TIntArray;
  Counter, I, Curr, Next: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  PreOrd := CreateIntArray;
  InStack.Size := VertexCount;
  Counter := 0;
  for I := 0 to Pred(VertexCount) do
    if PreOrd[I] = -1 then
      begin
        PreOrd[I] := Counter;
        Inc(Counter);
        {%H-}Stack.Push(I);
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
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  Visited: TBitVector;
  Counter, I, Curr, Next: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  Result := CreateIntArray;
  Visited.Size := VertexCount;
  Counter := Pred(VertexCount);
  for I := 0 to Pred(VertexCount) do
    if not Visited[I] then
      begin
        Visited[I] := True;
        {%H-}Stack.Push(I);
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
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  Visited: TBitVector;
  d, Curr, Next: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  Result := CreateIntArray;
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
              Result[Next] := d;
            Stack.Push(Next);
          end;
      end
    else
      Stack.Pop;
end;

function TGSimpleDiGraph.GetDagLongestPaths(aSrc: SizeInt; out aTree: TIntArray): TIntArray;
var
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  Visited: TBitVector;
  d, Curr, Next: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
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
  Stack, VtxStack, PathStack: TIntStack;
  AdjEnums: TAdjEnumArray;
  PreOrd: TIntArray;
  I, Counter, Curr, Next: SizeInt;
begin
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
        {%H-}Stack.Push(I);
        VtxStack{%H-}.Push(I);
        PathStack{%H-}.Push(I);
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
  Stack: TIntStack;
  Visited, IdVisited: TBitVector;
  IdParents, IdOrd: TIntArray;
  m: TSquareBitMatrix;
  Pairs: TIntPairSet;
  AdjEnums: TAdjEnumArray;
  I, J, Counter, Curr, Next, CurrId, NextId: SizeInt;
begin
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
        {%H-}Stack.Push(I);
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

{ TGWeightedDiGraph.THPrfHelper.TFlowData }

constructor TGWeightedDiGraph.THPrfHelper.TArc.Create(constref c: TWeight; aTarget: PNode; aReverse: PArc);
begin
  ResidualCap := c;
  Target := aTarget;
  Reverse := aReverse;
end;

constructor TGWeightedDiGraph.THPrfHelper.TArc.CreateReverse(aTarget: PNode; aReverse: PArc);
begin
  ResidualCap := ZeroWeight;
  Target := aTarget;
  Reverse := aReverse;
end;

function TGWeightedDiGraph.THPrfHelper.TArc.Saturated: Boolean;
begin
  Result := ResidualCap <= ZeroWeight;
end;

function TGWeightedDiGraph.THPrfHelper.TArc.HasResidual: Boolean;
begin
  Result := ResidualCap > ZeroWeight;
end;

procedure TGWeightedDiGraph.THPrfHelper.TArc.Push(constref aFlow: TWeight);
begin
  ResidualCap -= aFlow;
  Reverse^.ResidualCap += aFlow;
  Target^.Excess += aFlow;
  Reverse^.Target^.Excess -= aFlow;
end;

{ TGWeightedDiGraph.THPrfHelper.TNode }

function TGWeightedDiGraph.THPrfHelper.TNode.GetColor: TVertexColor;
begin
  Result := TVertexColor(Distance);
end;

procedure TGWeightedDiGraph.THPrfHelper.TNode.SetColor(aValue: TVertexColor);
begin
  Distance := SizeInt(aValue);
end;

procedure TGWeightedDiGraph.THPrfHelper.TNode.ResetCurrent;
begin
  CurrentArc := FirstArc;
end;

function TGWeightedDiGraph.THPrfHelper.TNode.HasExcess: Boolean;
begin
  Result := Excess > ZeroWeight;
end;

{ TGWeightedDiGraph.THPrfHelper.TLayer }

function TGWeightedDiGraph.THPrfHelper.TLayer.IsEmpty: Boolean;
begin
  Result := (TopActive = nil) and (TopIdle = nil);
end;

procedure TGWeightedDiGraph.THPrfHelper.TLayer.AddActive(aNode: PNode);
begin
  aNode^.LayerNext := TopActive;
  TopActive := aNode;
end;

procedure TGWeightedDiGraph.THPrfHelper.TLayer.AddIdle(aNode: PNode);
var
  Next: PNode;
begin
  Next := TopIdle;
  TopIdle := aNode;
  aNode^.LayerNext := Next;
  if Next <> nil then
    Next^.LayerPrev := aNode;
end;

procedure TGWeightedDiGraph.THPrfHelper.TLayer.ActivateIdle(aNode: PNode);
begin
  RemoveIdle(aNode);
  AddActive(aNode);
end;

procedure TGWeightedDiGraph.THPrfHelper.TLayer.RemoveIdle(aNode: PNode);
var
  Next, Prev: PNode;
begin
  Next := aNode^.LayerNext;
  if TopIdle = aNode then // is on head of the list
    TopIdle := Next
  else
    begin
      Prev := aNode^.LayerPrev;
      Prev^.LayerNext := aNode^.LayerNext;
      if Next <> nil then
        Next^.LayerPrev := Prev;
    end;
end;

procedure TGWeightedDiGraph.THPrfHelper.TLayer.Clear(aLabel: SizeInt);
var
  Next: PNode;
  I: SizeInt;
begin
  Next := TopActive;
  while Next <> nil do
    begin
      Next^.Distance := aLabel;
      Next := Next^.LayerNext;
    end;
  TopActive := nil;
  Next := TopIdle;
  while Next <> nil do
    begin
      Next^.Distance := aLabel;
      Next := Next^.LayerNext;
    end;
  TopIdle  := nil;
end;

{ TGWeightedDiGraph.THPrfHelper }

procedure TGWeightedDiGraph.THPrfHelper.Init(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt);
var
  CurrArcIdx: TIntArray;
  I, J: SizeInt;
  p: PAdjItem;
begin
  //transform graph into internal representation - residual graph;
  FNodeCount := aGraph.VertexCount;
  System.SetLength(CurrArcIdx, FNodeCount);
  J := 0;
  for I := 0 to System.High(CurrArcIdx) do
    begin
      CurrArcIdx[I] := J;
      J += aGraph.DegreeI(I);
    end;

  System.SetLength(FNodes, FNodeCount);
  //all arcs stored in the single array
  System.SetLength(FArcs, aGraph.EdgeCount * 2);
  FSource := @FNodes[aSource];
  FSink := @FNodes[aSink];

  for I := 0 to System.High(FNodes) do
    begin
      FNodes[I].FirstArc := @FArcs[CurrArcIdx[I]];
      FNodes[I].Excess := ZeroWeight;
    end;

  for I := 0 to System.High(FNodes) do
    for p in aGraph.AdjLists[I]^ do
      begin
        J := p^.Destination;
        FArcs[CurrArcIdx[I]] := TArc.Create(p^.Data.Weight, @FNodes[J], @FArcs[CurrArcIdx[J]]);
        FArcs[CurrArcIdx[J]] := TArc.CreateReverse(@FNodes[I], @FArcs[CurrArcIdx[I]]);
        Inc(CurrArcIdx[I]);
        Inc(CurrArcIdx[J]);
      end;

  for I := 0 to System.High(FNodes) do
    FNodes[I].LastArc := @FArcs[Pred(CurrArcIdx[I])];

  CurrArcIdx := nil;

  FSource^.Excess := InfiniteWeight;
  System.SetLength(FLayers, FNodeCount);
  FMaxLayer := System.High(FLayers);
  System.SetLength(FQueue, FNodeCount);
end;

procedure TGWeightedDiGraph.THPrfHelper.Init2(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt);
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

  System.SetLength(FNodes, FNodeCount);
  System.SetLength(FArcs, aGraph.EdgeCount * 2);
  System.SetLength(FCaps, aGraph.EdgeCount * 2);
  FSource := @FNodes[aSource];
  FSink := @FNodes[aSink];

  for I := 0 to System.High(FNodes) do
    begin
      FNodes[I].FirstArc := @FArcs[CurrArcIdx[I]];
      FNodes[I].Excess := ZeroWeight;
    end;

  for I := 0 to System.High(FNodes) do
    for p in aGraph.AdjLists[I]^ do
      begin
        J := p^.Destination;
        FCaps[CurrArcIdx[I]] := p^.Data.Weight;
        FCaps[CurrArcIdx[J]] := ZeroWeight;
        FArcs[CurrArcIdx[I]] := TArc.Create(p^.Data.Weight, @FNodes[J], @FArcs[CurrArcIdx[J]]);
        FArcs[CurrArcIdx[J]] := TArc.CreateReverse(@FNodes[I], @FArcs[CurrArcIdx[I]]);
        Inc(CurrArcIdx[I]);
        Inc(CurrArcIdx[J]);
      end;

  for I := 0 to System.High(FNodes) do
    FNodes[I].LastArc := @FArcs[Pred(CurrArcIdx[I])];

  CurrArcIdx := nil;

  FSource^.Excess := InfiniteWeight;
  System.SetLength(FLayers, FNodeCount);
  FMaxLayer := System.High(FLayers);
  System.SetLength(FQueue, FNodeCount);
end;

procedure TGWeightedDiGraph.THPrfHelper.ClearLabels;
var
  I: SizeInt;
begin
  for I := 0 to  System.High(FNodes) do
    FNodes[I].Distance := FNodeCount;
end;

procedure TGWeightedDiGraph.THPrfHelper.GlobalRelabel;
var
  CurrNode, NextNode: PNode;
  CurrArc: PArc;
  Dist: SizeInt;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  System.FillChar(Pointer(FLayers)^, Succ(FMaxLayer) * SizeOf(TLayer), 0);
  FMaxLayer := 0;
  FMaxActiveLayer := NULL_INDEX;
  FMinActiveLayer := FNodeCount;
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
      while CurrArc <= CurrNode^.LastArc do
        begin
          NextNode := CurrArc^.Target;
          if (NextNode^.Distance = FNodeCount) and CurrArc^.Reverse^.HasResidual then
            begin
              NextNode^.Distance := Dist;
              NextNode^.ResetCurrent;
              if Dist > FMaxLayer then
                FMaxLayer := Dist;
              if NextNode^.HasExcess then
                begin
                  FLayers[Dist].AddActive(NextNode);
                  if Dist > FMaxActiveLayer then
                    FMaxActiveLayer := Dist;
                  if Dist < FMinActiveLayer then
                    FMinActiveLayer := Dist;
                end
              else
                FLayers[Dist].AddIdle(NextNode);
              FQueue[qTail] := NextNode;
              Inc(qTail);
            end;
          Inc(CurrArc);
        end;
    end;
end;

procedure TGWeightedDiGraph.THPrfHelper.RemoveGap(aLayer: SizeInt);
var
  I: SizeInt;
begin
  for I := Succ(aLayer) to FMaxLayer do
    FLayers[I].Clear(FNodeCount);
  FMaxActiveLayer := Pred(aLayer);
  FMaxLayer := FMaxActiveLayer;
end;

function TGWeightedDiGraph.THPrfHelper.Push(aNode: PNode): Boolean;
var
  CurrArc: PArc;
  NextNode: PNode;
  Dist: SizeInt;
begin
  Dist := Pred(aNode^.Distance);
  while aNode^.CurrentArc <= aNode^.LastArc do
    begin
      CurrArc := aNode^.CurrentArc;
      NextNode := CurrArc^.Target;
      if (NextNode^.Distance = Dist) and CurrArc^.HasResidual then
        //arc is not saturated and target belongs to the next layer -> arc is admissible
        begin
          if (Dist > 0) and not NextNode^.HasExcess then //NextNode in idle list
            begin
              FLayers[Dist].ActivateIdle(NextNode);
              if Dist < FMinActiveLayer then
                FMinActiveLayer := Dist;
            end;
          CurrArc^.Push(Min(aNode^.Excess, CurrArc^.ResidualCap));
          if not aNode^.HasExcess then
            break;
        end;
      Inc(aNode^.CurrentArc);
    end;
  Result := aNode^.CurrentArc <= aNode^.LastArc;
end;

procedure TGWeightedDiGraph.THPrfHelper.Relabel(aNode: PNode);
var
  CurrArc: PArc;
  MinArc: PArc = nil;
  Dist: SizeInt;
begin
  Dist := FNodeCount;
  aNode^.Distance := FNodeCount;
  CurrArc := aNode^.FirstArc;
  while CurrArc <= aNode^.LastArc do
    begin
      if CurrArc^.HasResidual and (CurrArc^.Target^.Distance < Dist) then
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
      if Dist > FMaxLayer then
        FMaxLayer := Dist;
      if aNode^.HasExcess then
        begin
          FLayers[Dist].AddActive(aNode);
          if Dist > FMaxActiveLayer then
            FMaxActiveLayer := Dist;
          if Dist < FMinActiveLayer then
            FMinActiveLayer := Dist;
        end
      else
        FLayers[Dist].AddIdle(aNode);
    end;
end;

procedure TGWeightedDiGraph.THPrfHelper.HiLevelPushRelabel;
var
  CurrNode: PNode;
  GlobalRelableTreshold, OldMaxActive: SizeInt;
  RelableTimes: SizeInt = 0;
begin
  GlobalRelabel;
  GlobalRelableTreshold := FNodeCount;
  while FMaxActiveLayer >= FMinActiveLayer do
    begin
      CurrNode := FLayers[FMaxActiveLayer].TopActive;
      if CurrNode <> nil then
        begin
          OldMaxActive := FMaxActiveLayer;
          FLayers[OldMaxActive].TopActive := CurrNode^.LayerNext;
          if not Push(CurrNode) then
            begin
              Relabel(CurrNode);
              Inc(RelableTimes);
              if FLayers[OldMaxActive].IsEmpty then
                RemoveGap(OldMaxActive);
              if RelableTimes > GlobalRelableTreshold then
                begin
                  GlobalRelabel;
                  RelableTimes := 0;
                end;
            end
          else
            FLayers[OldMaxActive].AddIdle(CurrNode);
        end
      else
        Dec(FMaxActiveLayer);
    end;
end;

function TGWeightedDiGraph.THPrfHelper.CreateEdges: TEdgeArray;
var
  I, J: SizeInt;
  CurrArc: PArc;
  Cap: TWeight;
begin
  System.SetLength(Result, System.Length(FArcs) div 2);
  J := 0;
  for I := 0 to System.High(FNodes) do
    begin
      CurrArc := FNodes[I].FirstArc;
      while CurrArc <= FNodes[I].LastArc do
        begin
          Cap := FCaps[CurrArc - PArc(FArcs)];
          if Cap > ZeroWeight then
            begin
              Result[J] := TWeightEdge.Create(
                I, SizeInt(CurrArc^.Target - PNode(FNodes)), Cap - CurrArc^.ResidualCap);
              Inc(J);
            end;
          Inc(CurrArc);
        end;
    end;
end;

function TGWeightedDiGraph.THPrfHelper.PreflowToFlow: TEdgeArray;
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
      while CurrArc <= CurrNode^.LastArc do
        begin
          if CurrArc^.Target = CurrNode then
            CurrArc^.ResidualCap := FCaps[CurrArc - PArc(FArcs)];
          Inc(CurrArc);
        end;
      Inc(CurrNode);
    end;

  CurrNode := Pointer(FNodes);
  while CurrNode < PNode(FNodes) + FNodeCount do
    begin
      if (CurrNode^.Color = vcWhite) and CurrNode^.HasExcess and
         (CurrNode <> FSource) and (CurrNode <> FSink) then
           begin
             RootNode := CurrNode;
             RootNode^.Color := vcGray;
             repeat
               while CurrNode^.CurrentArc <= CurrNode^.LastArc do
                 begin
                   CurrArc := CurrNode^.CurrentArc;
                   if (FCaps[CurrArc - PArc(FArcs)] <= ZeroWeight) and CurrArc^.HasResidual and
                      (CurrArc^.Target = FSource) and (CurrArc^.Target = FSink) then
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
                             Delta := CurrArc^.ResidualCap;
                             while True do
                               begin
                                 Delta := Min(Delta, NextNode^.CurrentArc^.ResidualCap);
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
                                 CurrArc^.ResidualCap -= Delta;
                                 CurrArc^.Reverse^.ResidualCap += Delta;
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
                                 if (NextNode^.Color = vcWhite) or CurrArc^.Saturated then
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
               if CurrNode^.CurrentArc > CurrNode^.LastArc then
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
        while CurrNode^.HasExcess do
          begin
            if (FCaps[CurrArc - PArc(FArcs)] <= ZeroWeight) and CurrArc^.HasResidual then
              CurrArc^.Push(Min(CurrNode^.Excess, CurrArc^.ResidualCap));
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

function TGWeightedDiGraph.THPrfHelper.GetMaxFlow(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt): TWeight;
begin
  Init(aGraph, aSource, aSink);
  HiLevelPushRelabel;
  Result := FSink^.Excess;
end;

function TGWeightedDiGraph.THPrfHelper.GetMaxFlow(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt;
  out a: TEdgeArray): TWeight;
begin
  Init2(aGraph, aSource, aSink);
  HiLevelPushRelabel;
  FLayers := nil;
  Result := FSink^.Excess;
  a := PreflowToFlow;
end;

function TGWeightedDiGraph.THPrfHelper.GetMinCut(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt;
  out s: TIntArray): TWeight;
var
  I, J: SizeInt;
begin
  Init(aGraph, aSource, aSink);
  HiLevelPushRelabel;
  FLayers := nil;
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

{ TGWeightedDiGraph.TDinitzHelper.TArc }

constructor TGWeightedDiGraph.TDinitzHelper.TArc.Create(constref c: TWeight; aTarget: PNode; aReverse: PArc);
begin
  ResidualCap := c;
  Target := aTarget;
  Reverse := aReverse;
  IsReal := True;
end;

constructor TGWeightedDiGraph.TDinitzHelper.TArc.CreateReverse(aTarget: PNode; aReverse: PArc);
begin
  ResidualCap := ZeroWeight;
  Target := aTarget;
  Reverse := aReverse;
  IsReal := False;
end;

function TGWeightedDiGraph.TDinitzHelper.TArc.HasResidual: Boolean;
begin
  Result := ResidualCap > ZeroWeight;
end;

procedure TGWeightedDiGraph.TDinitzHelper.TArc.Push(constref aFlow: TWeight);
begin
  ResidualCap -= aFlow;
  Reverse^.ResidualCap += aFlow;
end;

{ TGWeightedDiGraph.TDinitzHelper.TNode }

procedure TGWeightedDiGraph.TDinitzHelper.TNode.ResetCurrent;
begin
  CurrentArc := FirstArc;
end;

function TGWeightedDiGraph.TDinitzHelper.TNode.NonLabeled: Boolean;
begin
  Result := Distance = NULL_INDEX;
end;

function TGWeightedDiGraph.TDinitzHelper.TNode.Labeled: Boolean;
begin
  Result := Distance <> NULL_INDEX;
end;

{ TGWeightedDiGraph.TDinitzHelper }

procedure TGWeightedDiGraph.TDinitzHelper.Init(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt);
var
  CurrArcIdx: TIntArray;
  I, J: SizeInt;
  p: PAdjItem;
begin
  System.SetLength(CurrArcIdx, aGraph.VertexCount);
  J := 0;
  for I := 0 to System.High(CurrArcIdx) do
    begin
      CurrArcIdx[I] := J;
      J += aGraph.DegreeI(I);
    end;

  System.SetLength(FNodes, aGraph.VertexCount);
  FSource := @FNodes[aSource];
  FSink := @FNodes[aSink];
  System.SetLength(FArcs, aGraph.EdgeCount * 2);

  for I := 0 to System.High(FNodes) do
    FNodes[I].FirstArc := @FArcs[CurrArcIdx[I]];

  for I := 0 to System.High(FNodes) do
    for p in aGraph.AdjLists[I]^ do
      begin
        J := p^.Destination;
        FArcs[CurrArcIdx[I]] := TArc.Create(p^.Data.Weight, @FNodes[J], @FArcs[CurrArcIdx[J]]);
        FArcs[CurrArcIdx[J]] := TArc.CreateReverse(@FNodes[I], @FArcs[CurrArcIdx[I]]);
        Inc(CurrArcIdx[I]);
        Inc(CurrArcIdx[J]);
      end;

  for I := 0 to System.High(FNodes) do
    FNodes[I].LastArc := @FArcs[Pred(CurrArcIdx[I])];

  CurrArcIdx := nil;
  System.SetLength(FQueue, aGraph.VertexCount);
end;

procedure TGWeightedDiGraph.TDinitzHelper.ClearLabels;
var
  I: SizeInt;
begin
  for I := 0 to  System.High(FNodes) do
    FNodes[I].Distance := NULL_INDEX;
end;

function TGWeightedDiGraph.TDinitzHelper.Bfs: Boolean;
var
  Curr, Next: PNode;
  CurrArc: PArc;
  Dist: SizeInt;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  ClearLabels;
  FSource^.Distance := 0;
  FSource^.ResetCurrent;
  FQueue[qTail] := FSource;
  Inc(qTail);
  while (qHead < qTail) and (FSink^.Distance = NULL_INDEX) do
    begin
      Curr := FQueue[qHead];
      Inc(qHead);
      Dist := Succ(Curr^.Distance);
      CurrArc := Curr^.FirstArc;
      while CurrArc <= Curr^.LastArc do
        begin
          Next := CurrArc^.Target;
          if Next^.NonLabeled and CurrArc^.HasResidual then
            begin
              Next^.ResetCurrent;
              Next^.Distance := Dist;
              FQueue[qTail] := Next;
              Inc(qTail);
            end;
          Inc(CurrArc);
        end;
    end;
  Result := FSink^.Labeled;
end;

function TGWeightedDiGraph.TDinitzHelper.Dfs(aRoot: PNode; var aFlow: TWeight): Boolean;
var
  Flow: TWeight;
begin
  if aFlow > ZeroWeight then
    begin
      if aRoot = FSink then
        exit(True);
      while aRoot^.CurrentArc <= aRoot^.LastArc do
        begin
          if aRoot^.CurrentArc^.Target^.Distance = Succ(aRoot^.Distance) then
            begin
              Flow := Min(aFlow, aRoot^.CurrentArc^.ResidualCap);
              if Dfs(aRoot^.CurrentArc^.Target, Flow) then
                begin
                  aRoot^.CurrentArc^.Push(Flow);
                  aFlow := Flow;
                  exit(True);
                end;
            end;
          Inc(aRoot^.CurrentArc);
        end;
    end;
  Result := False;
end;

function TGWeightedDiGraph.TDinitzHelper.FindMaxFlow: TWeight;
var
  Flow: TWeight;
begin
  Flow := InfiniteWeight;
  Result := ZeroWeight;
  while Bfs do
    while Dfs(FSource, Flow) do
      begin
        Result += Flow;
        Flow := InfiniteWeight;
      end;
end;

function TGWeightedDiGraph.TDinitzHelper.CreateEdges(aGraph: TGWeightedDiGraph): TEdgeArray;
var
  I, J, Dst: SizeInt;
  CurrArc: PArc;
  d: TEdgeData;
begin
  System.SetLength(Result, aGraph.EdgeCount);
  J := 0;
  d := Default(TEdgeData);
  for I := 0 to System.High(FNodes) do
    begin
      CurrArc := FNodes[I].FirstArc;
      while CurrArc <= FNodes[I].LastArc do
        begin
          if CurrArc^.IsReal then
            begin
              Dst := SizeInt(CurrArc^.Target - PNode(FNodes));
              aGraph.GetEdgeDataI(I, Dst, d);
              Result[J] := TWeightEdge.Create(I, Dst, d.Weight - CurrArc^.ResidualCap);
              Inc(J);
            end;
          Inc(CurrArc);
        end;
    end;
end;

function TGWeightedDiGraph.TDinitzHelper.GetMaxFlow(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt): TWeight;
begin
  Init(aGraph, aSource, aSink);
  Result := FindMaxFlow;
end;

function TGWeightedDiGraph.TDinitzHelper.GetMaxFlow(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt;
  out a: TEdgeArray): TWeight;
begin
  Init(aGraph, aSource, aSink);
  Result := FindMaxFlow;
  a := CreateEdges(aGraph);
end;

function TGWeightedDiGraph.TDinitzHelper.GetMinCut(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt;
  out s: TIntArray): TWeight;
var
  I, J: SizeInt;
begin
  Init(aGraph, aSource, aSink);
  Result := FindMaxFlow;
  System.SetLength(s, ARRAY_INITIAL_SIZE);
  J := 0;
  for I := 0 to System.High(FNodes) do
    if FNodes[I].Labeled then
      begin
        if System.Length(s) = J then
          System.SetLength(s, J shl 1);
        s[J] := I;
        Inc(J);
      end;
  System.SetLength(s, J);
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
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  Visited: TBitVector;
  Curr, Next: SizeInt;
  w: TWeight;
begin
  AdjEnums := CreateAdjEnumArray;
  Result := TPathHelper.CreateWeightArrayNI(VertexCount);
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
          Result[Next] := w;
      end
    else
      Stack.Pop;
end;

function TGWeightedDiGraph.GetDagMaxPaths(aSrc: SizeInt; out aTree: TIntArray): TWeightArray;
var
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  Visited: TBitVector;
  Curr, Next: SizeInt;
  w: TWeight;
begin
  AdjEnums := CreateAdjEnumArray;
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

class function TGWeightedDiGraph.Min(constref L, R: TWeight): TWeight;
begin
  if L <= R then
    Result := L
  else
    Result := R;
end;

class function TGWeightedDiGraph.InfiniteWeight: TWeight;
begin
  Result := TWeight.MaxValue;
end;

class function TGWeightedDiGraph.NegInfiniteWeight: TWeight;
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

function TGWeightedDiGraph.FindMinPathsMap(constref aSrc: TVertex; out aWeights: TWeightArray): Boolean;
begin
  Result := FindMinPathsMapI(IndexOf(aSrc), aWeights);
end;

function TGWeightedDiGraph.FindMinPathsMapI(aSrc: SizeInt; out aWeights: TWeightArray): Boolean;
begin
  CheckIndexRange(aSrc);
  Result := TPathHelper.FordBellman(Self, aSrc, aWeights);
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
  Result := TPathHelper.FordBellman(Self, aSrc, aPaths, aWeights);
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

function TGWeightedDiGraph.GetNetworkState(constref aSource, aSink: TVertex): TNetworkState;
begin
  Result := GetNetworkStateI(IndexOf(aSource), IndexOf(aSink));
end;

function TGWeightedDiGraph.GetNetworkStateI(aSrcIndex, aSinkIndex: SizeInt): TNetworkState;
var
  Queue: TIntQueue;
  Visited: TBitVector;
  Curr, Next: SizeInt;
  w: TWeight;
  p: PAdjItem;
  HasOverflow: Boolean = False;
  SinkFound: Boolean = False;
begin
  CheckIndexRange(aSrcIndex);
  CheckIndexRange(aSinkIndex);
  if VertexCount < 2 then
    exit(nwsTrivial);
  if not IsSourceI(aSrcIndex) then
    exit(nwsInvalidSource);
  if not IsSinkI(aSinkIndex) then
    exit(nwsInvalidSink);
  Visited.Size := VertexCount;
  Visited[aSrcIndex] := True;
  Curr := aSrcIndex;
  repeat
    for p in AdjLists[Curr]^ do
      begin
        Next := p^.Destination;
        if AdjLists[Next]^.Contains(Curr) then  // network should not contain antiparallel arcs
          exit(nwsAntiParallelArc);
        if p^.Data.Weight < ZeroWeight then // network should not contain arc with negative capacity
          exit(nwsNegArcCapacity);
        if not Visited[Next] then
          begin
            Visited[Next] := True;
            Queue.Enqueue(Next);
            SinkFound := SinkFound or (Next = aSinkIndex);
          end;
      end;
  until not Queue{%H-}.TryDequeue(Curr);
  if not SinkFound then // sink must be reachable from the source
    exit(nwsSinkUnreachable);
  w := ZeroWeight;
  {$PUSH}{$Q+}
  for p in AdjLists[aSrcIndex]^ do
    begin
      try
        w += p^.Data.Weight;
      except
        HasOverflow := True;
      end;
    end;
  {$POP}
  if HasOverflow then //total capacity of edges incident to the source exceeds InfiniteWeight
    exit(nwsSourceOverflow);
  Result := nwsValid;
end;

function TGWeightedDiGraph.FindMaxFlowPr(constref aSource, aSink: TVertex; out aFlow: TWeight): Boolean;
begin
  Result := FindMaxFlowPrI(IndexOf(aSource), IndexOf(aSink), aFlow);
end;

function TGWeightedDiGraph.FindMaxFlowPrI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight): Boolean;
var
  Helper: THPrfHelper;
begin
  if GetNetworkStateI(aSrcIndex, aSinkIndex) <> nwsValid then
    exit(False);
  aFlow := Helper.GetMaxFlow(Self, aSrcIndex, aSinkIndex);
  Result := True;
end;

function TGWeightedDiGraph.FindMaxFlowD(constref aSource, aSink: TVertex; out aFlow: TWeight): Boolean;
begin
  Result := FindMaxFlowDI(IndexOf(aSource), IndexOf(aSink), aFlow);
end;

function TGWeightedDiGraph.FindMaxFlowDI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight): Boolean;
var
  Helper: TDinitzHelper;
begin
  if GetNetworkStateI(aSrcIndex, aSinkIndex) <> nwsValid then
    exit(False);
  aFlow := Helper.GetMaxFlow(Self, aSrcIndex, aSinkIndex);
  Result := True;
end;

function TGWeightedDiGraph.FindMaxFlowD(constref aSource, aSink: TVertex; out aFlow: TWeight;
  out a: TEdgeArray): Boolean;
begin
  Result := FindMaxFlowDI(IndexOf(aSource), IndexOf(aSink), aFlow, a);
end;

function TGWeightedDiGraph.FindMaxFlowDI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight;
  out a: TEdgeArray): Boolean;
var
  Helper: TDinitzHelper;
begin
  if GetNetworkStateI(aSrcIndex, aSinkIndex) <> nwsValid then
    exit(False);
  aFlow := Helper.GetMaxFlow(Self, aSrcIndex, aSinkIndex, a);
  Result := True;
end;

function TGWeightedDiGraph.GetMaxFlowPr(constref aSource, aSink: TVertex): TWeight;
begin
  Result := GetMaxFlowPrI(IndexOf(aSource), IndexOf(aSink));
end;

function TGWeightedDiGraph.GetMaxFlowPrI(aSrcIndex, aSinkIndex: SizeInt): TWeight;
var
  Helper: THPrfHelper;
begin
  CheckIndexRange(aSrcIndex);
  CheckIndexRange(aSinkIndex);
  Result := Helper.GetMaxFlow(Self, aSrcIndex, aSinkIndex);
end;

function TGWeightedDiGraph.GetMaxFlowD(constref aSource, aSink: TVertex): TWeight;
begin
  Result := GetMaxFlowDI(IndexOf(aSource), IndexOf(aSink));
end;

function TGWeightedDiGraph.GetMaxFlowDI(aSrcIndex, aSinkIndex: SizeInt): TWeight;
var
  Helper: TDinitzHelper;
begin
  CheckIndexRange(aSrcIndex);
  CheckIndexRange(aSinkIndex);
  Result := Helper.GetMaxFlow(Self, aSrcIndex, aSinkIndex);
end;

function TGWeightedDiGraph.GetMaxFlowD(constref aSource, aSink: TVertex; out a: TEdgeArray): TWeight;
begin
  Result := GetMaxFlowDI(IndexOf(aSource), IndexOf(aSink), a);
end;

function TGWeightedDiGraph.GetMaxFlowDI(aSrcIndex, aSinkIndex: SizeInt; out a: TEdgeArray): TWeight;
var
  Helper: TDinitzHelper;
begin
  CheckIndexRange(aSrcIndex);
  CheckIndexRange(aSinkIndex);
  Result := Helper.GetMaxFlow(Self, aSrcIndex, aSinkIndex, a);
end;

function TGWeightedDiGraph.IsFlowFeasible(constref aSource, aSink: TVertex; constref a: TEdgeArray): Boolean;
begin
  Result := IsFlowFeasibleI(IndexOf(aSource), IndexOf(aSink), a);
end;

function TGWeightedDiGraph.IsFlowFeasibleI(aSrcIndex, aSinkIndex: SizeInt; constref a: TEdgeArray): Boolean;
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
      if v[I] <> ZeroWeight then
        exit(False);
  Result := True;
end;

function TGWeightedDiGraph.FindMinSTCutPr(constref aSource, aSink: TVertex; out aValue: TWeight;
  out aCut: TStCut): Boolean;
begin
  Result := FindMinSTCutPrI(IndexOf(aSource), IndexOf(aSink), aValue, aCut);
end;

function TGWeightedDiGraph.FindMinSTCutPrI(aSrcIndex, aSinkIndex: SizeInt; out aValue: TWeight;
  out aCut: TStCut): Boolean;
begin
  if GetNetworkStateI(aSrcIndex, aSinkIndex) <> nwsValid then
    exit(False);
  aValue := GetMinSTCutPrI(aSrcIndex, aSinkIndex, aCut);
  Result := True;
end;

function TGWeightedDiGraph.FindMinSTCutD(constref aSource, aSink: TVertex; out aValue: TWeight;
  out aCut: TStCut): Boolean;
begin
  Result := FindMinSTCutDI(IndexOf(aSource), IndexOf(aSink), aValue, aCut);
end;

function TGWeightedDiGraph.FindMinSTCutDI(aSrcIndex, aSinkIndex: SizeInt; out aValue: TWeight;
  out aCut: TStCut): Boolean;
begin
  if GetNetworkStateI(aSrcIndex, aSinkIndex) <> nwsValid then
    exit(False);
  aValue := GetMinSTCutDI(aSrcIndex, aSinkIndex, aCut);
  Result := True;
end;

function TGWeightedDiGraph.GetMinSTCutPr(constref aSource, aSink: TVertex; out aCut: TStCut): TWeight;
begin
  Result := GetMinSTCutPrI(IndexOf(aSource), IndexOf(aSink), aCut);
end;

function TGWeightedDiGraph.GetMinSTCutPrI(aSrcIndex, aSinkIndex: SizeInt; out aCut: TStCut): TWeight;
var
  Helper: THPrfHelper;
  TmpSet: TBoolVector;
  I: SizeInt;
begin
  CheckIndexRange(aSrcIndex);
  CheckIndexRange(aSinkIndex);
  Result := Helper.GetMinCut(Self, aSrcIndex, aSinkIndex, aCut.S);
  TmpSet.InitRange(VertexCount);
  for I in aCut.S do
    TmpSet[I] := False;
  aCut.T := TmpSet.ToArray;
end;

function TGWeightedDiGraph.GetMinSTCutD(constref aSource, aSink: TVertex; out aCut: TStCut): TWeight;
begin
  Result := GetMinSTCutDI(IndexOf(aSource), IndexOf(aSink), aCut);
end;

function TGWeightedDiGraph.GetMinSTCutDI(aSrcIndex, aSinkIndex: SizeInt; out aCut: TStCut): TWeight;
var
  Helper: TDinitzHelper;
  TmpSet: TBoolVector;
  I: SizeInt;
begin
  CheckIndexRange(aSrcIndex);
  CheckIndexRange(aSinkIndex);
  Result := Helper.GetMinCut(Self, aSrcIndex, aSinkIndex, aCut.S);
  TmpSet.InitRange(VertexCount);
  for I in aCut.S do
    TmpSet[I] := False;
  aCut.T := TmpSet.ToArray;
end;

{ TGIntWeightedDiGraph }

function TGIntWeightedDiGraph.FindMaxFlowPr(constref aSource, aSink: TVertex; out aFlow: TWeight;
  out a: TEdgeArray): Boolean;
begin
  Result := FindMaxFlowPrI(IndexOf(aSource), IndexOf(aSink), aFlow, a);
end;

function TGIntWeightedDiGraph.FindMaxFlowPrI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight;
  out a: TEdgeArray): Boolean;
var
  Helper: THPrfHelper;
begin
  if GetNetworkStateI(aSrcIndex, aSinkIndex) <> nwsValid then
    exit(False);
  aFlow := Helper.GetMaxFlow(Self, aSrcIndex, aSinkIndex, a);
  Result := True;
end;

function TGIntWeightedDiGraph.GetMaxFlowPr(constref aSource, aSink: TVertex; out a: TEdgeArray): TWeight;
begin
  Result := GetMaxFlowPrI(IndexOf(aSource), IndexOf(aSink), a);
end;

function TGIntWeightedDiGraph.GetMaxFlowPrI(aSrcIndex, aSinkIndex: SizeInt; out a: TEdgeArray): TWeight;
var
  Helper: THPrfHelper;
begin
  CheckIndexRange(aSrcIndex);
  CheckIndexRange(aSinkIndex);
  Result := Helper.GetMaxFlow(Self, aSrcIndex, aSinkIndex, a);
end;

end.

