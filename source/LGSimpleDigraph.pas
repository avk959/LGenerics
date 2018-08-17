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
  LGArrayHelpers,
  LGCustomGraph,
  LGQueue,
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
    TMaxFlowHelper = record
    private
    type
      PNode = ^TNode;
      PArc  = ^TArc;

      TArc = record
        ResidualCap: TWeight;
        Target: PNode;       // pointer to target node
        ReverseArc: PArc;    // pointer to opposite arc
        constructor Create(constref c: TWeight; aTarget, aOpposite: Pointer);
        constructor CreateReverse(aTarget, aOpposite: Pointer);
      end;

      TNode = record
        FirstArc,            // pointer to first ajacent arcs in arc array
        CurrentArc,          // pointer to current arc in arcs array
        LastArc: PArc;       // pointer to last ajacent arc in arcs array
        LayerNext,           // next node in layer-list
        LayerPrev: PNode;    // previous node in layer-list
        Distance: SizeInt;   // distance from the sink
        Excess: TWeight;     // excess at the node
        procedure ResetCurrent; inline;
      end;

      TLayer = record
        TopExceeded,         // 1st node with positive excess
        TopTransit: PNode;   // 1st node with zero excess
      end;

      TQueue = specialize TGLiteQueue<PNode>;

    var
      FNodes: array of TNode;
      FArcs: array of TArc;
      FLayers: array of TLayer;
      FSource,
      FSink: PNode;
      FNodeCount,
      FMaxLayer,                // maximal layer
      FMaxExcessLayer,          // maximal layer with excess node
      FMinExcessLayer: SizeInt; // minimal layer with excess node
      procedure Init(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt);
      procedure AddToExceeded(aNode: PNode; aLayer: SizeInt); inline;
      procedure RemoveFromExceeded(aNode: PNode; aLayer: SizeInt); inline;
      procedure AddToTransit(aNode: PNode; aLayer: SizeInt); inline;
      procedure RemoveFromTransit(aNode: PNode; aLayer: SizeInt); inline;
      procedure GlobalRelabel;
      procedure Gap;
      function  Push(aNode: PNode): Boolean;
      function  Relabel(aNode: PNode): Boolean;
      procedure HighLevelPushRelabel;
      function  ToFlow: TEdgeArray;
    public
      function  GetMaxFlow(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt): TWeight;
      function  GetMaxFlow(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt; out a: TEdgeArray): TWeight;
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
    function ContainsNegWeighedEdge: Boolean;

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
  networks utilities
***********************************************************************************************************}

    function NetworkState(constref aSource, aSink: TVertex): TNetworkState; inline;
    function NetworkStateI(aSrcIndex, aSinkIndex: SizeInt): TNetworkState;
    function FindMaxFlow(constref aSource, aSink: TVertex; out aFlow: TWeight): Boolean; inline;
    function FindMaxFlow(constref aSource, aSink: TVertex; out aFlow: TWeight; out a: TEdgeArray): Boolean; inline;
    function FindMaxFlowI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight): Boolean;
    function FindMaxFlowI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight; out a: TEdgeArray): Boolean;
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
      InStack[Stack.Pop] := False;
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
            InStack[Stack.Pop] := False;
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

{ TGWeightedDiGraph.TMaxFlowHelper.TFlowData }

constructor TGWeightedDiGraph.TMaxFlowHelper.TArc.Create(constref c: TWeight; aTarget, aOpposite: Pointer);
begin
  ResidualCap := c;
  Target := aTarget;
  ReverseArc := aOpposite;
end;

constructor TGWeightedDiGraph.TMaxFlowHelper.TArc.CreateReverse(aTarget, aOpposite: Pointer);
begin
  ResidualCap := ZeroWeight;
  Target := aTarget;
  ReverseArc := aOpposite;
end;

{ TGWeightedDiGraph.TMaxFlowHelper.TNode }

procedure TGWeightedDiGraph.TMaxFlowHelper.TNode.ResetCurrent;
begin
  CurrentArc := FirstArc;
end;

{ TGWeightedDiGraph.TMaxFlowHelper }

procedure TGWeightedDiGraph.TMaxFlowHelper.Init(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt);
var
  CurrArcIdx: array of SizeInt;
  I, J: SizeInt;
  p: PAdjItem;
begin
  //transform graph into internal representation;
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
end;

procedure TGWeightedDiGraph.TMaxFlowHelper.AddToExceeded(aNode: PNode; aLayer: SizeInt);
var
  Prev: PNode;
begin
  Prev := FLayers[aLayer].TopExceeded;
  FLayers[aLayer].TopExceeded := aNode;
  aNode^.LayerPrev := Prev;
  if Prev <> nil then
    Prev^.LayerNext := aNode;
  aNode^.LayerNext := nil;
  if aLayer > FMaxExcessLayer then
    FMaxExcessLayer := aLayer;
  if aLayer < FMinExcessLayer then
    FMinExcessLayer := aLayer;
end;

procedure TGWeightedDiGraph.TMaxFlowHelper.RemoveFromExceeded(aNode: PNode; aLayer: SizeInt);
var
  Prev: PNode;
begin
  Prev := aNode^.LayerPrev;
  if FLayers[aLayer].TopExceeded = aNode then //on the top of list
    FLayers[aLayer].TopExceeded := Prev
  else
    if Prev <> nil then
      Prev^.LayerNext := aNode^.LayerNext;
end;

procedure TGWeightedDiGraph.TMaxFlowHelper.AddToTransit(aNode: PNode; aLayer: SizeInt);
var
  Prev: PNode;
begin
  Prev := FLayers[aLayer].TopTransit;
  FLayers[aLayer].TopTransit := aNode;
  aNode^.LayerPrev := Prev;
  if Prev <> nil then
    Prev^.LayerNext := aNode;
  aNode^.LayerNext := nil;
end;

procedure TGWeightedDiGraph.TMaxFlowHelper.RemoveFromTransit(aNode: PNode; aLayer: SizeInt);
var
  Prev: PNode;
begin
  Prev := aNode^.LayerPrev;
  if FLayers[aLayer].TopTransit = aNode then //on the top of list
    FLayers[aLayer].TopTransit := Prev
  else
    if Prev <> nil then
      Prev^.LayerNext := aNode^.LayerNext
end;

procedure TGWeightedDiGraph.TMaxFlowHelper.GlobalRelabel;
var
  Queue: TQueue;
  CurrNode, NextNode: PNode;
  CurrArc: PArc;
  I, Dist: SizeInt;
begin
  System.FillChar(Pointer(FLayers)^, Succ(FMaxLayer) * SizeOf(TLayer), 0);
  FMaxExcessLayer := NULL_INDEX;
  FMinExcessLayer := FNodeCount;
  FMaxLayer := NULL_INDEX;
  for I := 0 to  System.High(FNodes) do
    FNodes[I].Distance := FNodeCount;
  FSink^.Distance := 0;
  CurrNode := FSink;
  repeat
    Dist := Succ(CurrNode^.Distance);
    CurrArc := CurrNode^.FirstArc;
    CurrNode^.ResetCurrent;
    while CurrArc <= CurrNode^.LastArc do
      begin
        NextNode := CurrArc^.Target;
        if (NextNode^.Distance = FNodeCount) and (CurrArc^.ReverseArc^.ResidualCap > ZeroWeight) then
          begin
            Queue.Enqueue(NextNode);
            NextNode^.Distance := Dist;
            if Dist > FMaxLayer then
              FMaxLayer := Dist;
            if NextNode^.Excess > ZeroWeight then
              AddToExceeded(NextNode, Dist)
            else
              AddToTransit(NextNode, Dist);
          end;
        Inc(CurrArc);
      end;
  until not Queue{%H-}.TryDequeue(CurrNode);
end;

procedure TGWeightedDiGraph.TMaxFlowHelper.Gap;
var
  CurrNode: PNode;
  I: SizeInt;
begin
  for I := Succ(FMaxExcessLayer) to FMaxLayer do
    begin
      CurrNode := FLayers[I].TopExceeded;
      while CurrNode <> nil do
        begin
          CurrNode^.Distance := FNodeCount;
          CurrNode := CurrNode^.LayerPrev;
        end;
      CurrNode := FLayers[I].TopTransit;
      while CurrNode <> nil do
        begin
          CurrNode^.Distance := FNodeCount;
          CurrNode := CurrNode^.LayerPrev;
        end;
    end;
  FMaxLayer := Pred(FMaxExcessLayer);
  FMaxExcessLayer := Pred(FMaxExcessLayer);
end;

function TGWeightedDiGraph.TMaxFlowHelper.Push(aNode: PNode): Boolean;
var
  CurrArc: PArc;
  NextNode: PNode;
  Dist: SizeInt;
  f: TWeight;
begin
  Dist := Pred(aNode^.Distance);
  Result := False;
  while aNode^.CurrentArc <= aNode^.LastArc do
    begin
      CurrArc := aNode^.CurrentArc;
      NextNode := CurrArc^.Target;
      f := CurrArc^.ResidualCap;
      if (NextNode^.Distance = Dist) and (f > ZeroWeight) then
        //arc is not saturated and target belongs to the next layer -> arc is admissible
        begin
          f := Min(aNode^.Excess, f);
          aNode^.Excess -= f;
          CurrArc^.ResidualCap -= f;
          CurrArc^.ReverseArc^.ResidualCap += f;
          if (Dist > 0) and (NextNode^.Excess <= ZeroWeight) then //in transit list
            begin
              RemoveFromTransit(NextNode, Dist);
              AddToExceeded(NextNode, Dist);
            end;
          NextNode^.Excess += f;
          if aNode^.Excess <= ZeroWeight then
            begin
              RemoveFromExceeded(aNode, Succ(Dist));
              AddToTransit(aNode, Succ(Dist));
              Result := True;
            end;
        end;
      Inc(aNode^.CurrentArc);
      if Result then
        break;
    end;
end;

function TGWeightedDiGraph.TMaxFlowHelper.Relabel(aNode: PNode): Boolean;
var
  CurrArc, MinArc: PArc;
  Dist, MinDist: SizeInt;
begin
  MinArc := nil;
  MinDist := FNodeCount;
  CurrArc := aNode^.FirstArc;
  while CurrArc <= aNode^.LastArc do
    begin
      Dist := CurrArc^.Target^.Distance;
      if Dist < MinDist then
        begin
          MinDist := Dist;
          MinArc := CurrArc;
        end;
      Inc(CurrArc);
    end;
  Dist := Succ(MinDist);
  Result := (Dist <> aNode^.Distance) and (Dist < FNodeCount);
  if Result then
    begin
      if Dist > FMaxLayer then
        FMaxLayer := Dist;
      aNode^.Distance := Dist;
      aNode^.CurrentArc := MinArc;
      AddToExceeded(aNode, Dist);
    end;
end;

procedure TGWeightedDiGraph.TMaxFlowHelper.HighLevelPushRelabel;
var
  CurrNode: PNode;
  RelableTreshold: SizeInt;
  RelableCount: SizeInt = 0;
begin
  GlobalRelabel;
  RelableTreshold := FNodeCount;
  while FMaxExcessLayer >= FMinExcessLayer do
    begin
      CurrNode := FLayers[FMaxExcessLayer].TopExceeded;
      if CurrNode = nil then
        begin
          Dec(FMaxExcessLayer);
          continue;
        end;
      if not Push(CurrNode) then
        if Relabel(CurrNode) then
          begin
            Inc(RelableCount);
            if (FLayers[FMaxExcessLayer].TopExceeded = nil) and (FLayers[FMaxExcessLayer].TopTransit = nil) then
              Gap;
            if RelableCount > RelableTreshold then
              begin
                GlobalRelabel;
                RelableCount := 0;
              end;
          end
        else
          begin
            GlobalRelabel;
            RelableCount := 0;
          end;
    end;
end;

function TGWeightedDiGraph.TMaxFlowHelper.ToFlow: TEdgeArray;
begin
  //not implemented yet
end;

function TGWeightedDiGraph.TMaxFlowHelper.GetMaxFlow(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt): TWeight;
begin
  Init(aGraph, aSource, aSink);
  HighLevelPushRelabel;
  Result := FNodes[aSink].Excess;
end;

function TGWeightedDiGraph.TMaxFlowHelper.GetMaxFlow(aGraph: TGWeightedDiGraph; aSource, aSink: SizeInt;
  out a: TEdgeArray): TWeight;
begin
  Init(aGraph, aSource, aSink);
  HighLevelPushRelabel;
  Result := FNodes[aSink].Excess;
  FLayers := nil;
  a := ToFlow;
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
  Result := TPathHelper.InfiniteWeight;
end;

class function TGWeightedDiGraph.NegInfiniteWeight: TWeight;
begin
  Result := TPathHelper.NegInfiniteWeight;
end;

class function TGWeightedDiGraph.ZeroWeight: TWeight;
begin
  Result := TPathHelper.ZeroWeight;
end;

function TGWeightedDiGraph.ContainsNegWeighedEdge: Boolean;
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
  Result := MinPathAStarI(IndexOf(aSrc), IndexOf(aSrc), aWeight, aEst);
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

function TGWeightedDiGraph.NetworkState(constref aSource, aSink: TVertex): TNetworkState;
begin
  Result := NetworkStateI(IndexOf(aSource), IndexOf(aSink));
end;

function TGWeightedDiGraph.NetworkStateI(aSrcIndex, aSinkIndex: SizeInt): TNetworkState;
var
  Queue: TIntQueue;
  Visited: TBitVector;
  Curr, Next: SizeInt;
  w: TWeight;
  p: PEdgeData;
  HasOverflow: Boolean = False;
  SinkFound: Boolean = False;
begin
  CheckIndexRange(aSrcIndex);
  CheckIndexRange(aSinkIndex);
  if VertexCount < 2 then
    exit(nwsTrivial); // network is trivial
  if not IsSourceI(aSrcIndex) then
    exit(nwsInvalidSource);
  if not IsSinkI(aSinkIndex) then
    exit(nwsInvalidSink);
  w := ZeroWeight;
  Visited.Size := VertexCount;
  Visited[aSrcIndex] := True;
  Curr := aSrcIndex;
  repeat
    for Next in AdjVerticesI(Curr) do
      begin
        if AdjacentI(Next, Curr) then  // network should not contain antiparallel arcs
          exit(nwsAntiParallelArc);
        p := GetEdgeDataPtr(Curr, Next);
        if p^.Weight < ZeroWeight then // network should not contain arcs with negative capacity
          exit(nwsNegArcCapacity);
        if Curr = aSrcIndex then
          {$PUSH}{$Q+}
          try
            w += p^.Weight;
          except
            HasOverflow := True;
          end;
          {$POP}
        if HasOverflow then //total capacity of edges incident to the source exceeds InfiniteWeight
          exit(nwsSourceOverflow);
        if not Visited[Next] then
          begin
            Visited[Next] := True;
            SinkFound := SinkFound or (Next = aSinkIndex);
            Queue.Enqueue(Next);
          end;
      end;
  until not Queue{%H-}.TryDequeue(Curr);
  if not SinkFound then //sink must be reachable from the source
    exit(nwsSinkUnreachable);
  Result := nwsValid;
end;

function TGWeightedDiGraph.FindMaxFlow(constref aSource, aSink: TVertex; out aFlow: TWeight): Boolean;
begin
  Result := FindMaxFlowI(IndexOf(aSource), IndexOf(aSink), aFlow);
end;

function TGWeightedDiGraph.FindMaxFlow(constref aSource, aSink: TVertex; out aFlow: TWeight;
  out a: TEdgeArray): Boolean;
begin
  Result := FindMaxFlowI(IndexOf(aSource), IndexOf(aSink), aFlow, a);
end;

function TGWeightedDiGraph.FindMaxFlowI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight): Boolean;
var
  Helper: TMaxFlowHelper;
begin
  if NetworkStateI(aSrcIndex, aSinkIndex) <> nwsValid then
    exit(False);
  aFlow := Helper.GetMaxFlow(Self, aSrcIndex, aSinkIndex);
  Result := True;
end;

function TGWeightedDiGraph.FindMaxFlowI(aSrcIndex, aSinkIndex: SizeInt; out aFlow: TWeight;
  out a: TEdgeArray): Boolean;
var
  Helper: TMaxFlowHelper;
begin
  if NetworkStateI(aSrcIndex, aSinkIndex) <> nwsValid then
    exit(False);
  aFlow := Helper.GetMaxFlow(Self, aSrcIndex, aSinkIndex, a);
  Result := True;
end;

end.

