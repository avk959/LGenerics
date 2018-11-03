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

{ TGIntWeightGraph.TKuhnMatch }

procedure TGIntWeightGraph.TKuhnMatch.Match(aNode, aMate: SizeInt);
begin
  FMates[aNode] := aMate;
  FMates[aMate] := aNode;
end;

procedure TGIntWeightGraph.TKuhnMatch.ClearParents;
begin
  System.FillChar(Pointer(FParents)^, System.Length(FParents) * SizeOf(SizeUint), $ff);
end;

procedure TGIntWeightGraph.TKuhnMatch.Init(aGraph: TGIntWeightGraph; constref w, g: TIntArray);
var
  I: SizeInt;
  p: PAdjItem;
  ew: TWeight;
begin
  FGraph := aGraph;
  FMatchCount := 0;
  FWhites.Size := aGraph.VertexCount;
  if System.Length(w) <= System.Length(g) then
    for I in w do
      FWhites[I] := True
  else
    for I in g do
      FWhites[I] := True;

  FPhi := TPathHelper.CreateWeightArrayZ(aGraph.VertexCount);
  for I in FWhites do
    begin
      ew := MAX_WEIGHT;
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

procedure TGIntWeightGraph.TKuhnMatch.InitMax(aGraph: TGIntWeightGraph; constref w, g: TIntArray);
var
  I: SizeInt;
  p: PAdjItem;
  ew: TWeight;
begin
  FGraph := aGraph;
  FMatchCount := 0;
  FWhites.Size := aGraph.VertexCount;
  if System.Length(w) <= System.Length(g) then
    for I in w do
      FWhites[I] := True
  else
    for I in g do
      FWhites[I] := True;

  FPhi := TPathHelper.CreateWeightArrayZ(aGraph.VertexCount);
  for I in FWhites do
    begin
      ew := MIN_WEIGHT;
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
function TGIntWeightGraph.TKuhnMatch.FindAugmentPath(aRoot: SizeInt; var aDelta: TWeight): SizeInt;
var
  Curr, Next: SizeInt;
  p: PAdjItem;
  Cost: TWeight;
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
      if FWhites[Curr] then
        for p in FGraph.AdjLists[Curr]^ do
          begin
            Next := p^.Destination;
            if (FMates[Curr] = Next) or (FParents[Next] <> NULL_INDEX) then
              continue;
            Cost := p^.Data.Weight + FPhi[Next] - FPhi[Curr];
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

function TGIntWeightGraph.TKuhnMatch.FindAugmentPathMax(aRoot: SizeInt; var aDelta: TWeight): SizeInt;
var
  Curr, Next: SizeInt;
  p: PAdjItem;
  Cost: TWeight;
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
      if FWhites[Curr] then
        for p in FGraph.AdjLists[Curr]^ do
          begin
            Next := p^.Destination;
            if (FMates[Curr] = Next) or (FParents[Next] <> NULL_INDEX) then
              continue;
            Cost := p^.Data.Weight + FPhi[Next] - FPhi[Curr];
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
procedure TGIntWeightGraph.TKuhnMatch.AlternatePath(aRoot: SizeInt);
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

function TGIntWeightGraph.TKuhnMatch.TryAugment(var aDelta: TWeight): SizeInt;
var
  I, Last: SizeInt;
begin
  aDelta := MAX_WEIGHT;
  FVisited.ClearBits;
  ClearParents;
  Result := 0;
  for I in FWhites do
    if FMates[I] = NULL_INDEX then
      begin
        Last := FindAugmentPath(I, aDelta);
        if Last <> NULL_INDEX then
          begin
            AlternatePath(Last);
            Inc(Result);
          end;
      end;
end;

function TGIntWeightGraph.TKuhnMatch.TryAugmentMax(var aDelta: TWeight): SizeInt;
var
  I, Last: SizeInt;
begin
  aDelta := MIN_WEIGHT;
  FVisited.ClearBits;
  ClearParents;
  Result := 0;
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

procedure TGIntWeightGraph.TKuhnMatch.CorrectPhi(aDelta: TWeight);
var
  I: SizeInt;
begin
  for I in FVisited do
    FPhi[I] += aDelta;
end;

procedure TGIntWeightGraph.TKuhnMatch.FindMatching;
var
  Matched: SizeInt;
  Delta: TWeight;
begin
  Delta := MAX_WEIGHT;
  repeat
    repeat
      Matched := TryAugment(Delta);
      FMatchCount += Matched;
    until Matched = 0;
    if Delta = MAX_WEIGHT then
      break;
    CorrectPhi(Delta);
  until False;
end;

procedure TGIntWeightGraph.TKuhnMatch.FindMaxMatching;
var
  Matched: SizeInt;
  Delta: TWeight;
begin
  Delta := MIN_WEIGHT;
  repeat
    repeat
      Matched := TryAugmentMax(Delta);
      FMatchCount += Matched;
    until Matched = 0;
    if Delta = MIN_WEIGHT then
      break;
    CorrectPhi(Delta);
  until False;
end;

function TGIntWeightGraph.TKuhnMatch.CreateEdges: TEdgeArray;
var
  I, J: SizeInt;
  d: TEdgeData;
begin
  System.SetLength(Result, FMatchCount);
  J := 0;
  d := Default(TEdgeData);
  for I in FWhites do
    if FMates[I] <> NULL_INDEX then
      begin
        FGraph.GetEdgeDataI(I, FMates[I], d);
        Result[J] := TWeightEdge.Create(I, FMates[I], d.Weight);
        Inc(J);
      end;
end;

function TGIntWeightGraph.TKuhnMatch.GetMinWeightMatch(aGraph: TGIntWeightGraph; constref w, g: TIntArray): TEdgeArray;
begin
  Init(aGraph, w, g);
  FindMatching;
  Result := CreateEdges;
end;

function TGIntWeightGraph.TKuhnMatch.GetMaxWeightMatch(aGraph: TGIntWeightGraph; constref w, g: TIntArray): TEdgeArray;
begin
  InitMax(aGraph, w, g);
  FindMaxMatching;
  Result := CreateEdges;
end;

{ TGIntWeightGraph.TNIMinCutHelper.TNiEdge }

constructor TGIntWeightGraph.TNIMinCutHelper.TNiEdge.Create(aTarget: SizeInt; w: TWeight);
begin
  Target := aTarget;
  Weight := w;
end;

{ TGIntWeightGraph.TNIMinCutHelper }

procedure TGIntWeightGraph.TNIMinCutHelper.ClearMarks;
var
  I: SizeInt;
  p: TNiAdjList.PEntry;
begin
  for I in FExistNodes do
    for p in FGraph[I] do
      p^.Scanned := False;
end;

procedure TGIntWeightGraph.TNIMinCutHelper.Init(aGraph: TGIntWeightGraph);
var
  I: SizeInt;
  p: PAdjItem;
begin
  System.SetLength(FGraph, aGraph.VertexCount);
  for I := 0 to Pred(aGraph.VertexCount) do
    begin
      FGraph[I].EnsureCapacity(aGraph.DegreeI(I));
      for p in aGraph.AdjLists[I]^ do
        FGraph[I].Add(TNiEdge.Create(p^.Destination, p^.Data.Weight));
    end;
  FQueue := TPairHeapMax.Create(aGraph.VertexCount);
  FExistNodes.InitRange(aGraph.VertexCount);
  FInQueue.Size := aGraph.VertexCount;
  FBestCut := MAX_WEIGHT;
  FCuts := nil;
end;

procedure TGIntWeightGraph.TNIMinCutHelper.Init2(aGraph: TGIntWeightGraph);
var
  I: SizeInt;
  p: PAdjItem;
begin
  System.SetLength(FGraph, aGraph.VertexCount);
  for I := 0 to Pred(aGraph.VertexCount) do
    begin
      FGraph[I].EnsureCapacity(aGraph.DegreeI(I));
      for p in aGraph.AdjLists[I]^ do
        FGraph[I].Add(TNiEdge.Create(p^.Destination, p^.Data.Weight));
    end;
  System.SetLength(FCuts, aGraph.VertexCount);
  for I := 0 to Pred(aGraph.VertexCount) do
    FCuts[I].Add(I);
  FQueue := TPairHeapMax.Create(aGraph.VertexCount);
  FExistNodes.InitRange(aGraph.VertexCount);
  FInQueue.Size := aGraph.VertexCount;
  FBestCut := MAX_WEIGHT;
end;

procedure TGIntWeightGraph.TNIMinCutHelper.ShrinkEdge(aSource, aTarget: SizeInt);
var
  I: SizeInt;
  p: PNiEdge;
  Edge: TNiEdge;
begin
  FGraph[aSource].Remove(aTarget);
  FGraph[aTarget].Remove(aSource);
  FGraph[aSource].AddAll(FGraph[aTarget]);
  for p in FGraph[aTarget] do
    begin
      I := p^.Target;
      Edge := p^;
      FGraph[I].Remove(aTarget);
      Edge.Target := aSource;
      FGraph[I].Add(Edge);
    end;
  Finalize(FGraph[aTarget]);
  FExistNodes[aTarget] := False;
  if FCuts <> nil then
    begin
      while FCuts[aTarget].TryPop(I) do
        FCuts[aSource].Push(I);
      Finalize(FCuts[aTarget]);
    end;
end;

procedure TGIntWeightGraph.TNIMinCutHelper.ScanFirstSearch;
var
  I: SizeInt;
  p: PNiEdge;
  Item: TWeightItem;
begin
  ClearMarks;
  FInQueue.Join(FExistNodes);
  for I in FExistNodes do
    FQueue.Enqueue(I, TWeightItem.Create(I, ZeroWeight));
  while FQueue.Count > 1 do
    begin
      I := FQueue.Dequeue.Index;
      FInQueue[I] := False;
      for p in FGraph[I] do
        if FInQueue[p^.Target] then
          begin
            Item := FQueue.Peek(p^.Target);
            Item.Weight += p^.Weight;
            FQueue.Update(p^.Target, Item);
            p^.Scanned := True;
            p^.ScanRank := Item.Weight;
          end;
    end;
  Item := FQueue.Dequeue;
  FInQueue[Item.Index] := False;
  if Item.Weight < FBestCut then
    begin
      FBestCut := Item.Weight;
      if FCuts <> nil then
        FBestSet.Assign(FCuts[Item.Index]);
    end;
end;

procedure TGIntWeightGraph.TNIMinCutHelper.Shrink;
var
  I: SizeInt;
  p: PNiEdge;
  Pair: TIntOrdPair;
begin
  ScanFirstSearch;
  for I in FExistNodes do
    for p in FGraph[I] do
      if p^.Scanned and (p^.ScanRank >= FBestCut) then
        FEdgeQueue.Enqueue(TIntOrdPair.Create(I, p^.Target));
  while FEdgeQueue.TryDequeue(Pair) do
    if FExistNodes[Pair.Left] and FExistNodes[Pair.Right] then
      ShrinkEdge(Pair.Left, Pair.Right);
end;

function TGIntWeightGraph.TNIMinCutHelper.GetMinCut(aGraph: TGIntWeightGraph): TWeight;
begin
  Init(aGraph);
  while FExistNodes.PopCount >= 2 do
    Shrink;
  Result := FBestCut;
end;

function TGIntWeightGraph.TNIMinCutHelper.GetMinCut(aGraph: TGIntWeightGraph; out aCut: TIntSet): TWeight;
begin
  Init2(aGraph);
  while FExistNodes.PopCount >= 2 do
    Shrink;
  Result := FBestCut;
  aCut.Assign(FBestSet);
end;

