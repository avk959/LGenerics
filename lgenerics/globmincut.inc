{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*                                                                           *
*   Copyright(c) 2018-2022 A.Koverdyaev(avk)                                *
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

{ TGInt64Net.TNIMinCutHelper.TNiEdge }

constructor TGInt64Net.TNIMinCutHelper.TNiEdge.Create(aTarget: SizeInt; w: TWeight; aRev, aNext: PNiEdge);
begin
  Target := aTarget;
  Weight := w;
  Reverse := aRev;
  Next := aNext;
end;

{ TGInt64Net.TNIMinCutHelper }

function TGInt64Net.TNIMinCutHelper.Init(aGraph: TGInt64Net; aCutNeeded: Boolean): Boolean;
var
  EdgeIdx: TIntArray;
  I, Tmp: SizeInt;
  w: TWeight;
  p: PAdjItem;
begin
  FNodeCount := aGraph.VertexCount;
  EdgeIdx.Length := FNodeCount;
  System.SetLength(FNodes, FNodeCount);
  System.SetLength(FEdges, aGraph.EdgeCount * 2);
  Tmp := 0;
  for I := 0 to Pred(FNodeCount) do
    begin
      EdgeIdx[I] := Tmp;
      FNodes[I].AdjHead := @FEdges[Tmp];
      Tmp += aGraph.AdjLists[I]^.Count;
      FNodes[I].AdjTail := @FEdges[Pred(Tmp)];
    end;
  for I := 0 to Pred(FNodeCount) do
    for p in aGraph.AdjLists[I]^ do
      if p^.Key > I then
        begin
          w := p^.Data.Weight;
          if w < 0 then exit(False);
          FEdges[EdgeIdx[I]] :=
            TNiEdge.Create(p^.Key, w, @FEdges[EdgeIdx[p^.Key]], @FEdges[Succ(EdgeIdx[I])]);
          FEdges[EdgeIdx[p^.Key]] :=
            TNiEdge.Create(I, w, @FEdges[EdgeIdx[I]], @FEdges[Succ(EdgeIdx[p^.Key])]);
          Inc(EdgeIdx[I]);
          Inc(EdgeIdx[p^.Key])
        end;
  for I := 0 to Pred(FNodeCount) do
    FNodes[I].AdjTail^.Next := nil;
  if aCutNeeded then
    begin
      System.SetLength(FCuts, FNodeCount);
      for I := 0 to Pred(FNodeCount) do
        FCuts[I].Push(I);
    end
  else
    FCuts := nil;
  FScanned.Capacity := aGraph.EdgeCount * 2;
  FQueue := TQueue.Create(FNodeCount);
  FExistNodes.InitRange(FNodeCount);
  FInQueue.Capacity := FNodeCount;
  FBestCut := MAX_WEIGHT;
  Result := True;
end;

procedure TGInt64Net.TNIMinCutHelper.ContractEdge(aSource, aTarget: SizeInt);
var
  I: SizeInt;
  p: PNiEdge;
begin
  p := FNodes[aTarget].AdjHead;
  while p <> nil do
    begin
      p^.Reverse^.Target := aSource;
      p := p^.Next;
    end;
  FNodes[aSource].AdjTail^.Next := FNodes[aTarget].AdjHead;
  FNodes[aSource].AdjTail := FNodes[aTarget].AdjTail;
  FNodes[aTarget].AdjHead := nil;
  FExistNodes.UncBits[aTarget] := False;
  Dec(FNodeCount);
  if FCuts <> nil then
    begin
      I := 0;
      while FCuts[aTarget].TryPop(I) do
        FCuts[aSource].Push(I);
      FCuts[aTarget].Clear;
    end;
end;

procedure TGInt64Net.TNIMinCutHelper.ScanFirstSearch;
var
  Item: TWeightItem;
  I: SizeInt;
  p: PNiEdge;
begin
  FScanned.ClearBits;
  FInQueue.Join(FExistNodes);
  for I in FExistNodes do
    FQueue.Enqueue(I, TWeightItem.Create(I, 0));
  while FQueue.Count > 1 do
    begin
      I := FQueue.Dequeue.Index;
      FInQueue.UncBits[I] := False;
      p := FNodes[I].AdjHead;
      while p <> nil do
        begin
          if not FScanned.UncBits[p - PNiEdge(FEdges)] and FInQueue.UncBits[p^.Target] then
            begin
              FScanned.UncBits[p - PNiEdge(FEdges)] := True;
              FScanned.UncBits[p^.Reverse - PNiEdge(FEdges)] := True;
              Item := FQueue.GetItem(p^.Target);
              Item.Weight += p^.Weight;
              p^.Rank := Item.Weight;
              p^.Reverse^.Rank := Item.Weight;
              FQueue.Update(p^.Target, Item);
            end;
          p := p^.Next;
        end;
    end;
  Item := FQueue.Dequeue;
  FInQueue.UncBits[Item.Index] := False;
  if Item.Weight < FBestCut then
    begin
      FBestCut := Item.Weight;
      if FCuts <> nil then
        FBestSet.Assign(FCuts[Item.Index]);
    end;
end;

procedure TGInt64Net.TNIMinCutHelper.Shrink;
var
  I: SizeInt;
  p: PNiEdge;
  Edge: TIntEdge;
begin
  ScanFirstSearch;
  for I in FExistNodes do
    begin
      p := FNodes[I].AdjHead;
      while p <> nil do begin
        if FScanned.UncBits[p - PNiEdge(FEdges)] and (p^.Rank >= FBestCut) then
          begin
            FEdgeQueue.Enqueue(TIntEdge.Create(I, p^.Target));
            FScanned.UncBits[p^.Reverse - PNiEdge(FEdges)] := False;
          end;
        p := p^.Next;
      end;
    end;
  Edge := Default(TIntEdge);
  while FEdgeQueue.TryDequeue(Edge) do
    if FExistNodes.UncBits[Edge.Source] and FExistNodes.UncBits[Edge.Destination] then
      ContractEdge(Edge.Source, Edge.Destination);
end;

function TGInt64Net.TNIMinCutHelper.GetMinCut(aGraph: TGInt64Net): TWeight;
begin
  if not Init(aGraph, False) then
    exit(MIN_WEIGHT);
  while FNodeCount >= 2 do
    Shrink;
  Result := FBestCut;
end;

function TGInt64Net.TNIMinCutHelper.GetMinCut(aGraph: TGInt64Net; out aCut: TIntSet): TWeight;
begin
  if not Init(aGraph, True) then
    exit(MIN_WEIGHT);
  while FNodeCount >= 2 do
    Shrink;
  Result := FBestCut;
  aCut.Assign(FBestSet);
end;

