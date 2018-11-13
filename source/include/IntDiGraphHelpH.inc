{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
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

type
  TWeightArcMap = specialize TGLiteHashMapLP<TIntEdge, TWeight, TIntEdge>;

  { THPrHelper: an implementation of the high-level push-relabel method;
    B.V. Cherkassky, A.V. Goldberg: On Implementing Push-Relabel Method for the Maximum Flow Problem. }
  THPrHelper = record
  strict private
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
      procedure Reset; inline;
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
    procedure ClearLabels;
    procedure GlobalRelabel;
    procedure RemoveGap(aLayer: SizeInt);
    function  Push(aNode: PNode): Boolean;
    procedure Relabel(aNode: PNode);
    procedure Execute;
    function  CreateEdges: TEdgeArray;
    function  RecoverFlow: TEdgeArray;
  public
    function  GetMaxFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt): TWeight;
    function  GetMaxFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; out a: TEdgeArray): TWeight;
    function  GetMinCut(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; out s: TIntArray): TWeight;
    function  GetFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; aReqFlow: TWeight;
              out m: TWeightArcMap): TWeight;
  end;

  { TDinitzHelper: implementation of capacity scaling Dinitz's maxflow algorithm }
  TDinitzHelper = record
  strict private
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
      procedure Reset; inline;
    end;

  const
    MIN_SCALE = 6;

  var
    FNodes: array of TNode;
    FArcs: array of TArc;
    FQueue: array of PNode;
    FSource,
    FSink: PNode;
    FEpsilon: TWeight;
    FScaleFactor,
    FNodeCount: SizeInt;
    procedure CreateResudualGraph(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt);
    procedure ClearLabels;
    function  BuildLevelGraph: Boolean;
    function  FindBlockingFlow(aRoot: PNode; aFlow: TWeight): TWeight;
    function  Execute: TWeight;
    function  CreateEdges(aGraph: TGIntWeightDiGraph): TEdgeArray;
  public
    function  GetMaxFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt): TWeight;
    function  GetMaxFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; out a: TEdgeArray): TWeight;
    function  GetMinCut(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; out s: TIntArray): TWeight;
  end;

  { TSspMcfHelper: Busacker-Gowen successive shortest path minimum-cost flow algorithm with
    potentials; Syslo, Deo, Kowalik "Discrete Optimization Algorithms: With Pascal Programs" }
  TSspMcfHelper = record
  strict private
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
      PathArc: PArc;       // pointer to incoming path arc
      Parent: PNode;
      Price: TCost;
      PathMinCap: TWeight; // munimum capacity on path
    end;
    THeap  = specialize TGPairHeapMin<TCostItem>;

  var
    FNodes: array of TNode;
    FArcs: array of TArc;
    FQueue: THeap;
    FInQueue: TBitVector;
    FReached: TBoolVector;
    FSource,
    FSink: PNode;
    FGraph: TGIntWeightDiGraph;
    FRequestFlow: TWeight;
    FNodeCount: SizeInt;
    procedure CreateResudualGraph(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; aReqFlow: TWeight;
              var aCosts: TEdgeCostMap);
    procedure SearchInit;
    function  ReweightGraph: Boolean;
    function  FindAugmentPath: Boolean;
    function  PushFlow(aRestFlow: TWeight): TWeight;
    function  Execute: TWeight;
    function  GetTotalCost: TCost;
    function  CreateEdges(out aTotalCost: TCost): TEdgeArray;
  public
    function  GetMinCostFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; aReqFlow: TWeight;
              var aCosts: TEdgeCostMap; out aTotalCost: TCost): TWeight;
    function  GetMinCostFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; aReqFlow: TWeight;
              var aCosts: TEdgeCostMap; out aTotalCost: TCost; out a: TEdgeArray): TWeight;
  end;

  { TCsMcfHelper: basic implementation of cost scaling mincost flow algorithm;
      A.V.Goldberg and M.Kharitonov. "On Implementing Scaling Push-Relabel Algorithms for the
      Minimum-Cost Flow Problem";
    todo: more crucial heurictics }
  TCsMcfHelper = record
  strict private
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
      procedure PushAll; inline;
    end;

    TNode = record
    private
      FirstArc,            // pointer to first incident arc
      CurrArc: PArc;       // pointer to current incident arc
      Price: TCost;
      Excess: TWeight;     // excess at the node
      procedure Reset; inline;
    end;
    TQueue = specialize TGLiteQueue<PNode>;

  const
    ALPHA: TCost = TCost(8);

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
    FScaleFactor,
    FEpsilon: TCost;
    FNodeCount: SizeInt;
    procedure CreateResidualGraph(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; aReqFlow: TWeight;
              var aCosts: TEdgeCostMap);
    function  IndexOf(aNode: PNode): SizeInt; inline;
    function  PriceUpdate: Boolean;
    procedure FindRequiredFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; out m: TWeightArcMap);
    function  FindInitSolution: Boolean;
    procedure InitPhase;
    procedure Discharge(aNode: PNode);
    procedure RemoveScaleFactor;
    procedure Execute;
    function  GetTotalCost: TCost;
    function  CreateEdges(out aTotalCost: TCost): TEdgeArray;
  public
    function  GetMinCostFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; aReqFlow: TWeight;
              var aCosts: TEdgeCostMap; out aTotalCost: TCost): TWeight;
    function  GetMinCostFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; aReqFlow: TWeight;
              var aCosts: TEdgeCostMap; out aTotalCost: TCost; out a: TEdgeArray): TWeight;
  end;



