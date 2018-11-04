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
    FQueue.Enqueue(I, TWeightItem.Create(I, 0));
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

