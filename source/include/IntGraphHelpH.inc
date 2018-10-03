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
  { TKuhnMatch: Kuhn weighted matching algorithm for bipartite graph }
  TKuhnMatch = record
   private
     FGraph: TGIntWeightGraph;
     FMates,
     FParents,
     FQueue: array of SizeInt;
     FPots: array of TWeight;
     FWhites,
     FVisited: TBoolVector;
     FMatchCount: SizeInt;
     procedure Match(aNode, aMate: SizeInt); inline;
     procedure ClearParents; inline;
     procedure Init(aGraph: TGIntWeightGraph; constref w, g: TIntArray);
     procedure InitMax(aGraph: TGIntWeightGraph; constref w, g: TIntArray);
     function  FindAugmentPath(aRoot: SizeInt; var aDelta: TWeight): SizeInt;
     function  FindAugmentPathMax(aRoot: SizeInt; var aDelta: TWeight): SizeInt;
     procedure AlternatePath(aRoot: SizeInt);
     function  TryMatch(var aDelta: TWeight): SizeInt;
     function  TryMatchMax(var aDelta: TWeight): SizeInt;
     procedure CorrectPots(aDelta: TWeight); inline;
     procedure KuhnMatch;
     procedure KuhnMatchMax;
     function  CreateEdges: TEdgeArray;
   public
     function  GetMinWeightMatch(aGraph: TGIntWeightGraph; constref w, g: TIntArray): TEdgeArray;
     function  GetMaxWeightMatch(aGraph: TGIntWeightGraph; constref w, g: TIntArray): TEdgeArray;
   end;

  TSWAdjList = specialize TGJoinableHashList<TWeightItem>;

  { TNIMinCutHelper: some implemenation of Nagamochi-Ibaraki minimum cut algorithm:
      H.Nagamochi and T.Ibaraki. "Computing Edge-Connectivity in Multigraphs and Capacitated Graphs" }
  TNIMinCutHelper = record
  private
  type
    TNiEdge = record
      Target: SizeInt;
      Weight,
      ScanRank: TWeight;
      Scanned: Boolean;
      constructor Create(aTarget: SizeInt; w: TWeight);
      property Key: SizeInt read Target;
    end;

    PNiEdge    = ^TNiEdge;
    TNiAdjList = specialize TGJoinableHashList<TNiEdge>;
    TEdgeQueue = specialize TGLiteQueue<TIntPair>;

  var
    FGraph: array of TNiAdjList;
    FCuts: array of TIntSet;
    FQueue: TPairHeapMax;
    FEdgeQueue: TEdgeQueue;
    FExistNodes,
    FInQueue: TBoolVector;
    FBestSet: TIntSet;
    FBestCut: TWeight;
    procedure ClearMarks;
    procedure Init(aGraph: TGIntWeightGraph);
    procedure Init2(aGraph: TGIntWeightGraph);
    procedure ShrinkEdge(aSource, aTarget: SizeInt);
    procedure ScanFirstSearch;
    procedure Shrink;
  public
    function  GetMinCut(aGraph: TGIntWeightGraph): TWeight;
    function  GetMinCut(aGraph: TGIntWeightGraph; out aCut: TIntSet): TWeight;
  end;
