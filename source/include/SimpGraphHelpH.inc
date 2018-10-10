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
  private
    FMatrix: TBoolMatrix;
    FCurrSet: TBoolVector;
    FNodes,
    FRecentBest: TIntArray;
    FOnFind: TOnFindSet;
    FStartTime: TDateTime;
    FTimeOut: Integer;
    FCanceled: Boolean;
    procedure Recolor(constref aCand: TBoolVector; var aColOrd, aColors: TIntArray);//aka BB_Color
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
  private
    FMatrix: TBitMatrix256;
    FCurrSet: TBits256;
    FNodes,
    FRecentBest: TIntArray;
    FOnFind: TOnFindSet;
    FStartTime: TDateTime;
    FTimeOut: Integer;
    FCanceled: Boolean;
    procedure Recolor(constref aCand: TBits256; var aColOrd, aColors: TIntArray);
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
  private
    FMatrix: TSkeleton;
    FCurrSet: TIntSet;
    FNodes,
    FRecentBest: TIntArray;
    FOnFind: TOnFindSet;
    FStartTime: TDateTime;
    FTimeOut: Integer;
    FCanceled: Boolean;
    procedure Recolor(constref aCand: TIntSet; var aColOrd, aColors: TIntArray);
    procedure Extend(var aCand: TIntSet);
    procedure Extend(var aSub, aCand: TIntSet);
  public
    function  MaxClique(aGraph: TGSimpleGraph; aTimeOut: Integer; out aExact: Boolean): TIntArray;
    procedure ListCliques(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
  end;

  TBPDomSetHelper = record // BP -> bit-parallel
  private
    FMatrix: TBoolMatrix;
    FNodes,
    FRecentBest: TIntArray;
    FStartTime: TDateTime;
    FTimeOut: Integer;
    FCanceled: Boolean;
    procedure FillMatrix(aGraph: TGSimpleGraph);
    procedure Extend(constref aSub, aCand: TBoolVector);
  public
    function  MinDomSet(aGraph: TGSimpleGraph; aTimeOut: Integer; out aExact: Boolean): TIntArray;
  end;

  TBPDomSetHelper256 = record // BP -> bit-parallel
  private
    FMatrix: TBitMatrix256;
    FNodes,
    FRecentBest: TIntArray;
    FStartTime: TDateTime;
    FTimeOut: Integer;
    FCanceled: Boolean;
    procedure FillMatrix(aGraph: TGSimpleGraph);
    procedure Extend(constref aSub, aCand: TBits256);
  public
    function  MinDomSet(aGraph: TGSimpleGraph; aTimeOut: Integer; out aExact: Boolean): TIntArray;
  end;

  TDomSetHelper = record
  private
    FMatrix: TSkeleton;
    FRecentBest: TIntArray;
    FStartTime: TDateTime;
    FTimeOut: Integer;
    FCancel: Boolean;
    procedure Extend(constref aSub, aCand: TIntSet);
  public
    function  MinDomSet(aGraph: TGSimpleGraph; aTimeOut: Integer; out aExact: Boolean): TIntArray;
  end;

  { TVColorHelper }
  TVColorHelper = record
  private
    FCurrent,
    FRecentBest,
    FColCounts,
    FCurrSeq: TIntArray;
    FColLists: array of TBoolVector;
    FGraph: TGSimpleGraph;
    FMinColCount,
    FBestColCount,
    FNodeCount: SizeInt;
    FStartTime: TDateTime;
    FTimeOut: Integer;
    FCancel: Boolean;
    procedure BackwardStep();
    procedure RefineColoring;
  public
    function GetChromNumber(aGraph: TGSimpleGraph; aTimeOut: Integer; out aColors: TIntArray; out aExact: Boolean): SizeInt;
  end;

{ THKMatch: Hopcroft–Karp algorithm for maximum cardinality matching
    for bipartite graph - see en.wikipedia.org/wiki/Hopcroft–Karp_algorithm }
  THKMatch = record
  private
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
    function  GetMaxMatch(aGraph: TGSimpleGraph; constref w, g: TIntArray): TIntEdgeArray;
  end;

  { TBfsMatch: simple BFS matching algorithm for bipartite graph }
  TBfsMatch = record
   private
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
     function  GetMaxMatch(aGraph: TGSimpleGraph; constref w, g: TIntArray): TIntEdgeArray;
   end;

{ TEdMatch: Edmonds algorithm for maximum cardinality matching }
  TEdMatch = record
  private
    FGraph: TGSimpleGraph;
    FMates,
    FBase,
    FParents,
    FQueue: TIntArray;
    FVisited,
    FLcaUsed,
    FBlossoms: TBitVector;
    FMatchCount: SizeInt;
    procedure Match(aIndex, aMate: SizeInt); inline;
    procedure ClearBase; inline;
    procedure ClearParents; inline;
    function  Lca(L, R: SizeInt): SizeInt;
    procedure MarkPath(aValue, aBloss, aChild: SizeInt);
    function  FindAugmentPath(aRoot: SizeInt; out aLast: SizeInt): Boolean;
    procedure AlternatePath(aRoot: SizeInt);
    procedure EdMatch;
    procedure Init(aGraph: TGSimpleGraph);
  public
    function  GetMaxMatch(aGraph: TGSimpleGraph): TIntEdgeArray;
  end;

  { TPcMatch: Pape and Conradt general matching algorithm from
    Syslo, Deo, Kowalik "Discrete Optimization Algorithms: With Pascal Programs" }
  TPcMatch = record
  private
    FGraph: TGSimpleGraph;
    FMates,
    FGrannies,
    FQueue: TIntArray;
    FInTree: TBitVector;
    FMatchCount: SizeInt;
    procedure DoMatch(aValue, aMate: SizeInt); inline;
    procedure TryAugment(aRoot: SizeInt);
    procedure Init(aGraph: TGSimpleGraph);
  public
    function  GetMaxMatch(aGraph: TGSimpleGraph): TIntEdgeArray;
  end;

  { TNIMinCut: some implemenation of Nagamochi-Ibaraki minimum cut algorithm }
  TNIMinCut = record
  private
  type
    TNIEdge = record
      Target,
      Weight,
      ScanRank: SizeInt;
      Scanned: Boolean;
      constructor Create(aTarget: SizeInt; constref w: SizeInt);
      property Key: SizeInt read Target;
    end;

    PECEdge    = ^TNIEdge;
    TECAdjList = specialize TGJoinableHashList<TNIEdge>;
    TPairHeap  = specialize TGPairHeapMax<TINode>;
    TEdgeQueue = specialize TGLiteQueue<TIntPair>;

  var
    FGraph: array of TECAdjList;
    FCuts: array of TIntSet;
    FQueue: TPairHeap;
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

