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
  TBPCliqueIsHelper = record // BP -> bit-parallel
  strict private
    FMatrix: TBoolMatrix;
    FCurrSet: TBoolVector;
    FNodes,
    FRecentBest: TIntArray;
    FOnFind: TOnFindSet;
    FStartTime: TDateTime;
    FTimeOut: Integer;
    FCancelled: Boolean;
    procedure Colorize(constref aCand: TBoolVector; var aColOrd, aColors: TIntArray);//aka BB_Color
    procedure Extend(var aCand: TBoolVector); // in Bron-Kerbosch terminlogy
    procedure Extend(var aSub, aCand: TBoolVector);
    procedure FillMatrix(aGraph: TGSimpleGraph; aComplement: Boolean);
    procedure SortMatrixByWidth(aGraph: TGSimpleGraph; aComplement: Boolean);
    procedure SortMatrixByDegree(aGraph: TGSimpleGraph; aComplement: Boolean);
  public
  { some variant of BB-MaxClique -
      San Segundo, P, Rodriguez-Losada, D., Jimenez, A.:
        "An exact bit-parallel algorithm for the maximum clique problem",
      Patrick Prosser: "Exact Algorithms for Maximum Clique: a computational study." }
    function  MaxClique(aGraph: TGSimpleGraph; aTimeOut: Integer; out aExact: Boolean): TIntArray;
  { executes MaxClique upon complement graph }
    function  MaxIS(aGraph: TGSimpleGraph; aTimeOut: Integer; out aExact: Boolean): TIntArray;
  { something like Tomita's Cliques on bit strings, except pivot maximizing -
      Etsuji Tomitaa, Akira Tanakaa, Haruhisa Takahashi:
        "The worst-case time complexity for generating all maximal cliques and
         computational experiments. }
    procedure ListCliques(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
  { executes ListCliques upon complement graph }
    procedure ListIS(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
  end;

  TBPCliqueIsHelper256 = record // BP -> bit-parallel
  strict private
    FMatrix: TBitMatrix256;
    FCurrSet: TBits256;
    FNodes,
    FRecentBest: TIntArray;
    FOnFind: TOnFindSet;
    FStartTime: TDateTime;
    FTimeOut: Integer;
    FCancelled: Boolean;
    procedure Colorize(constref aCand: TBits256; var aColOrd, aColors: TIntArray);
    procedure Extend(var aCand: TBits256);
    procedure Extend(var aSub, aCand: TBits256);
    procedure FillMatrix(aGraph: TGSimpleGraph; aComplement: Boolean);
    procedure SortMatrixByWidth(aGraph: TGSimpleGraph; aComplement: Boolean);
    procedure SortMatrixByDegree(aGraph: TGSimpleGraph; aComplement: Boolean);
  public
    function  MaxClique(aGraph: TGSimpleGraph; aTimeOut: Integer; out aExact: Boolean): TIntArray;
    function  MaxIS(aGraph: TGSimpleGraph; aTimeOut: Integer; out aExact: Boolean): TIntArray;
    procedure ListCliques(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
    procedure ListIS(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
  end;

  TCliqueHelper = record
  strict private
    FMatrix: TSkeleton;
    FCurrSet: TIntSet;
    FNodes,
    FRecentBest: TIntArray;
    FOnFind: TOnFindSet;
    FStartTime: TDateTime;
    FTimeOut: Integer;
    FCancelled: Boolean;
    procedure Colorize(constref aCand: TIntSet; var aColOrd, aColors: TIntArray);
    procedure Extend(var aCand: TIntSet);
    procedure Extend(var aSub, aCand: TIntSet);
  public
    function  MaxClique(aGraph: TGSimpleGraph; aTimeOut: Integer; out aExact: Boolean): TIntArray;
    procedure ListCliques(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
  end;

  TBPDomSetHelper = record // BP -> bit-parallel
  strict private
    FMatrix: TBoolMatrix;
    FNodes,
    FRecentBest: TIntArray;
    FStartTime: TDateTime;
    FTimeOut: Integer;
    FCancelled: Boolean;
    procedure FillMatrix(aGraph: TGSimpleGraph);
    procedure Extend(constref aSub, aCand: TBoolVector);
  public
    function  MinDomSet(aGraph: TGSimpleGraph; aTimeOut: Integer; out aExact: Boolean): TIntArray;
  end;

  TBPDomSetHelper256 = record // BP -> bit-parallel
  strict private
    FMatrix: TBitMatrix256;
    FNodes,
    FRecentBest: TIntArray;
    FStartTime: TDateTime;
    FTimeOut: Integer;
    FCancelled: Boolean;
    procedure FillMatrix(aGraph: TGSimpleGraph);
    procedure Extend(constref aSub, aCand: TBits256);
  public
    function  MinDomSet(aGraph: TGSimpleGraph; aTimeOut: Integer; out aExact: Boolean): TIntArray;
  end;

  TDomSetHelper = record
  strict private
    FMatrix: TSkeleton;
    FRecentBest: TIntArray;
    FStartTime: TDateTime;
    FTimeOut: Integer;
    FCancelled: Boolean;
    procedure Extend(constref aSub, aCand: TIntSet);
  public
    function  MinDomSet(aGraph: TGSimpleGraph; aTimeOut: Integer; out aExact: Boolean): TIntArray;
  end;

  { TExactColor: a DSATUR-based branch and bound algorithm inspired by
    https://mat.tepper.cmu.edu/COLOR/solvers/trick.c by Michael Trick }
  TExactColor = record
  strict private
  type
    TNode = record
      Saturation,
      Degree: SizeInt;
      AdjColors: array of Integer;
      constructor Create(aDegree, aUpperBound: SizeInt);
      procedure NeighbPickColor(aColor: SizeInt); inline;
      procedure NeighbDropColor(aColor: SizeInt); inline;
      function  Available(aColor: Integer): Boolean; inline;
    end;

  var
    FNodes: array of TNode;
    FAchromatic: TBoolVector;
    FColorMap,
    FRecentBest: TIntArray;
    FGraph: TGSimpleGraph;
    FLowBound,
    FUpBound,
    FNodeCount: SizeInt;
    FStartTime: TDateTime;
    FTimeOut: Integer;
    FCancelled: Boolean;
    function  TimeOut: Boolean; inline;
    function  InitLowBound: SizeInt;
    procedure InitComplete;
    procedure PickColor(aIndex, aColor: SizeInt);
    procedure DropColor(aIndex, aColor: SizeInt);
    function  SelectNext(out aNode: SizeInt): Boolean;
    procedure CheckSolution(aMaxColor: SizeInt); inline;
    function  DSatur(aMaxColor: SizeInt): SizeInt;
  public
    function Colorize(aGraph: TGSimpleGraph; aTimeOut: Integer; out aColors: TIntArray; out aExact: Boolean): SizeInt;
    function IsColorable(aGraph: TGSimpleGraph; aK: SizeInt; aTimeOut: Integer; out aColors: TIntArray): TTriLean;
    function Complete(aGraph: TGSimpleGraph; aK: SizeInt; aTimeOut: Integer; var aColors: TIntArray): Boolean;
  end;

  { TGreedyColorRlf }
  TGreedyColorRlf = record
  strict private
  type
    TNode = record
      Index,
      WDegree,
      Degree: SizeInt;
      class operator > (constref L, R: TNode): Boolean; inline;
      class operator < (constref L, R: TNode): Boolean; inline;
      class operator >=(constref L, R: TNode): Boolean; inline;
      class operator <=(constref L, R: TNode): Boolean; inline;
      constructor Create(aIndex, aDegree: SizeInt);
    end;
    TQueue = specialize TGPairHeapMax<TNode>;
  public
    function Colorize(aGraph: TGSimpleGraph; out aColors: TIntArray): SizeInt;
  end;

  { THamiltonCycles }
  THamiltonCycles = record
  strict private
    FMatrix: TBoolMatrix;
    FVacant: TBoolVector;
    FStack: TSimpleStack;
    FCycles: PIntArrayVector;
    FRoot,
    FNodeCount,
    FWishCount,
    FFoundCount: SizeInt;
    FStartTime: TDateTime;
    FTimeOut: Integer;
    FDone,
    FCancelled: Boolean;
    procedure Init(aGraph: TGSimpleGraph; aRoot, aCount: SizeInt; aTimeOut: Integer; pv: PIntArrayVector);
    function  TimeToFinish: Boolean; inline;
    function  SelectMin(constref v: TBoolVector; out aValue: SizeInt): Boolean;
    procedure CheckIsCycle(aNode: SizeInt); inline;
    procedure DoSearch(aNode: SizeInt);
    procedure SearchFor;
  public
    function  Find(aGraph: TGSimpleGraph; aRoot, aCount: SizeInt; aTimeOut: Integer;
              pv: PIntArrayVector): Boolean;
  end;

  { THKMatch: Hopcroft–Karp algorithm for maximum cardinality matching
    for bipartite graph - see en.wikipedia.org/wiki/Hopcroft–Karp_algorithm }
  THKMatch = record
  strict private
  type
    TArc = record
      Target: SizeInt; // index of target node
    end;

    TNode = record
      FirstArc,        // index of first incident arc in arcs array
      Distance,
      Mate: SizeInt;   // index of matched node
    end;

  const
    INF_DIST = High(SizeInt);
  var
    FNodes: array of TNode;
    FArcs: array of TArc;
    FWhites: array of SizeInt;
    FQueue: TIntArray;
    FNodeCount,
    FDummy: SizeInt;  // index of dummy node
    procedure Init(aGraph: TGSimpleGraph; constref w, g: TIntArray);
    function  Bfs: Boolean;
    function  Dfs(aRoot: SizeInt): Boolean;
    function  HopcroftKarp: TIntEdgeArray;
  public
    function  MaxMatching(aGraph: TGSimpleGraph; constref w, g: TIntArray): TIntEdgeArray;
  end;

  { TBfsMatch: simple BFS matching algorithm for bipartite graph }
  TBfsMatch = record
  strict private
    FGraph: TGSimpleGraph;
    FMates,
    FParents,
    FQueue: TIntArray;
    FVisited: TBitVector;
    FWhites: TBoolVector;
    FMatchCount: SizeInt;
    procedure Match(aNode, aMate: SizeInt); inline;
    procedure Init(aGraph: TGSimpleGraph; constref w, g: TIntArray);
    function  FindAugmentPath(aRoot: SizeInt): SizeInt;
    procedure AlternatePath(aRoot: SizeInt);
    procedure BfsMatch;
    function  CreateEdges: TIntEdgeArray;
  public
    function  MaxMatching(aGraph: TGSimpleGraph; constref w, g: TIntArray): TIntEdgeArray;
  end;

{ TEdMatch: Edmonds algorithm for maximum cardinality matching }
  TEdMatch = record
  strict private
    FGraph: TGSimpleGraph;
    FMates,
    FBase,
    FParents,
    FQueue: TIntArray;
    FVisited,
    FLcaUsed,
    FBlossoms: TBitVector;
    FMatchCount: SizeInt;
    procedure Match(aNode, aMate: SizeInt); inline;
    procedure ClearBase; inline;
    procedure ClearParents; inline;
    function  Lca(L, R: SizeInt): SizeInt;
    procedure MarkPath(aNode, aBloss, aChild: SizeInt);
    function  FindAugmentPath(aRoot: SizeInt; out aLast: SizeInt): Boolean;
    procedure AlternatePath(aRoot: SizeInt);
    procedure EdMatch;
    procedure Init(aGraph: TGSimpleGraph);
  public
    function  MaxMatching(aGraph: TGSimpleGraph): TIntEdgeArray;
  end;

  { TPcMatch: Pape and Conradt general matching algorithm from
    Syslo, Deo, Kowalik "Discrete Optimization Algorithms: With Pascal Programs" }
  TPcMatch = record
  strict private
    FGraph: TGSimpleGraph;
    FMates,
    FGrannies,
    FQueue: TIntArray;
    FInTree: TBitVector;
    FMatchCount: SizeInt;
    procedure Match(aNode, aMate: SizeInt); inline;
    procedure FindAugmentPath(aRoot: SizeInt);
    procedure Init(aGraph: TGSimpleGraph);
  public
    function  MaxMatching(aGraph: TGSimpleGraph): TIntEdgeArray;
  end;

  { TNiMinCut: some implemenation of Nagamochi-Ibaraki minimum cut algorithm }
  TNiMinCut = record
  strict private
  type
    TNiEdge = record
      Target,
      Weight,
      ScanRank: SizeInt;
      Scanned: Boolean;
      constructor Create(aTarget, aWeight: SizeInt);
      property Key: SizeInt read Target;
    end;

    PNiEdge    = ^TNiEdge;
    TNiAdjList = specialize TGJoinableHashList<TNiEdge>;
    TQueue     = specialize TGPairHeapMax<TIntNode>;
    TEdgeQueue = specialize TGLiteQueue<TIntOrdPair>;

  var
    FGraph: array of TNiAdjList;
    FCuts: array of TIntSet;
    FQueue: TQueue;
    FEdgeQueue: TEdgeQueue;
    FExistNodes,
    FInQueue: TBoolVector;
    FBestSet: TIntSet;
    FBestCut: SizeInt;
    procedure ClearMarks; inline;
    procedure Init(aGraph: TGSimpleGraph);
    procedure Init2(aGraph: TGSimpleGraph);
    procedure ShrinkEdge(aSource, aTarget: SizeInt);
    procedure ScanFirstSearch;
    procedure Shrink;
  public
    function  GetMinCut(aGraph: TGSimpleGraph): SizeInt;
    function  GetMinCut(aGraph: TGSimpleGraph; out aCut: TIntSet): SizeInt;
  end;

