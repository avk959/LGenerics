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

{ TDisjointSetUnion }

function TDisjointSetUnion.GetSize: SizeInt;
begin
  Result := System.Length(FList);
end;

procedure TDisjointSetUnion.SetSize(aValue: SizeInt);
var
  OldSize, I: SizeInt;
begin
  OldSize := Size;
  if aValue > OldSize then
    begin
      System.SetLength(FList, aValue);
      for I := OldSize to Pred(aValue) do
        FList[I] := I;
    end;
end;

procedure TDisjointSetUnion.Clear;
begin
  FList := nil;
end;

procedure TDisjointSetUnion.Reset;
var
  I: SizeInt;
begin
  for I := 0 to System.High(FList) do
    FList[I] := I;
end;

function TDisjointSetUnion.Tag(aValue: SizeInt): SizeInt;
begin
  if FList[aValue] = aValue then
    exit(aValue);
  Result := Tag(FList[aValue]);
  FList[aValue] := Result;
end;

function TDisjointSetUnion.InSameSet(L, R: SizeInt): Boolean;
begin
  Result := Tag(L) = Tag(R);
end;

function TDisjointSetUnion.InDiffSets(L, R: SizeInt): Boolean;
begin
  Result := Tag(L) <> Tag(R);
end;

function TDisjointSetUnion.Join(L, R: SizeInt): Boolean;
begin
  L := Tag(L);
  R := Tag(R);
  if L = R then
    exit(False);
  if NextRandomBoolean then
    FList[L] := R
  else
    FList[R] := L;
  Result := True;
end;

{ TIntValue }

constructor TIntValue.Create(aValue: SizeInt);
begin
  Value := aValue;
end;

{ TIntHashSet.TEnumerator }

function TIntHashSet.TEnumerator.GetCurrent: SizeInt;
begin
  Result := FEnum.Current^.Key;
end;

function TIntHashSet.TEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TIntHashSet.TEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TIntHashSet }

function TIntHashSet.GetCount: SizeInt;
begin
  Result := FTable.Count;
end;

function TIntHashSet.GetEnumerator: TEnumerator;
begin
  Result.FEnum := FTable.GetEnumerator
end;

function TIntHashSet.ToArray: TIntArray;
var
  p: PEntry;
  I: SizeInt = 0;
begin
  System.SetLength(Result, Count);
  for p in FTable do
    begin
      Result[I] := p^.Key;
      Inc(I);
    end;
end;

function TIntHashSet.IsEmpty: Boolean;
begin
  Result := FTable.Count = 0;
end;

function TIntHashSet.NonEmpty: Boolean;
begin
  Result := FTable.Count <> 0;
end;

procedure TIntHashSet.MakeEmpty;
begin
  FTable.MakeEmpty;
end;

procedure TIntHashSet.Clear;
begin
  FTable.Clear;
end;

procedure TIntHashSet.EnsureCapacity(aValue: SizeInt);
begin
  FTable.EnsureCapacity(aValue);
end;

function TIntHashSet.Contains(aValue: SizeInt): Boolean;
begin
  Result := FTable.Contains(aValue);
end;

function TIntHashSet.Add(aValue: SizeInt): Boolean;
var
  p: PEntry;
begin
  Result := not FTable.FindOrAdd(aValue, p);
  if Result then
    p^.Key := aValue;
end;

function TIntHashSet.AddAll(constref a: array of SizeInt): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I in a do
    Result += Ord(Add(I));
end;

function TIntHashSet.AddAll(constref s: TIntHashSet): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I in s do
    Result += Ord(Add(I));
end;

function TIntHashSet.Remove(aValue: SizeInt): Boolean;
begin
  Result := FTable.Remove(aValue);
end;

{ TIntPair }

class function TIntPair.HashCode(constref aValue: TIntPair): SizeInt;
begin
{$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  {$IF DEFINED (CPU64)}
    Result := TxxHash32LE.HashGuid(TGuid(aValue));
  {$ELSEIF DEFINED (CPU32)}
    Result := TxxHash32LE.HashQWord(QWord(aValue));
  {$ELSE }
    Result := TxxHash32LE.HashDWord(DWord(aValue));
  {$ENDIF }
{$ElSE FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := TxxHash32LE.HashBuf(@aValue, SizeOf(aValue));
{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
end;

class function TIntPair.Equal(constref L, R: TIntPair): Boolean;
begin
  Result := (L.Left = R.Left) and (L.Right = R.Right);
end;

constructor TIntPair.Create(L, R: SizeInt);
begin
  if L <= R then
    begin
      FLess := L;
      FGreater := R;
    end
  else
    begin
      FLess := R;
      FGreater := L;
    end;
end;

function TIntPair.Key: TIntPair;
begin
  Result := Self;
end;

{ TIntPairSet }

function TIntPairSet.GetCount: SizeInt;
begin
  Result := FTable.Count;
end;

procedure TIntPairSet.Clear;
begin
  FTable.Clear;
end;

function TIntPairSet.Contains(L, R: SizeInt): Boolean;
var
  Dummy: SizeInt;
begin
  Result := FTable.Find(TIntPair.Create(L, R), Dummy) <> nil;
end;

function TIntPairSet.Add(L, R: SizeInt): Boolean;
var
  Dummy: SizeInt;
  p: PIntPair;
  v: TIntPair;
begin
  v := TIntPair.Create(L, R);
  Result := not FTable.FindOrAdd(v, p, Dummy);
  if Result then
    p^ := v;
end;

function TIntPairSet.Remove(L, R: SizeInt): Boolean;
begin
  Result := FTable.Remove(TIntPair.Create(L, R));
end;

{ TINode }

class operator TINode. = (constref L, R: TINode): Boolean;
begin
  Result := L.Data = R.Data;
end;

class operator TINode.<>(constref L, R: TINode): Boolean;
begin
  Result := L.Data <> R.Data;
end;

class operator TINode.>(constref L, R: TINode): Boolean;
begin
  Result := L.Data > R.Data;
end;

class operator TINode.<(constref L, R: TINode): Boolean;
begin
  Result := L.Data < R.Data;
end;

class operator TINode.>=(constref L, R: TINode): Boolean;
begin
  Result := L.Data >= R.Data;
end;

class operator TINode.<=(constref L, R: TINode): Boolean;
begin
  Result := L.Data <= R.Data;
end;

constructor TINode.Create(aIndex, aData: SizeInt);
begin
  Index := aIndex;
  Data := aData;
end;

{ TGBinHeapMin }

function TGBinHeapMin.GetCapacity: SizeInt;
begin
  Result := System.Length(FIndex2Handle);
end;

procedure TGBinHeapMin.FloatUp(aIndex: SizeInt);
var
  CurrIdx, ParentIdx, HandleIdx: SizeInt;
  v: T;
begin
  if aIndex > 0 then
    begin
      CurrIdx := aIndex;
      ParentIdx := Pred(aIndex) shr 1;
      v := FHeap[aIndex];
      HandleIdx := FIndex2Handle[aIndex];
      while (CurrIdx > 0) and (v < FHeap[ParentIdx]) do
        begin
          FHeap[CurrIdx] := FHeap[ParentIdx];
          FHandle2Index[FIndex2Handle[ParentIdx]] := CurrIdx;
          FIndex2Handle[CurrIdx] := FIndex2Handle[ParentIdx];
          CurrIdx := ParentIdx;
          ParentIdx := Pred(ParentIdx) shr 1;
        end;
      FHeap[CurrIdx] := v;
      FHandle2Index[HandleIdx] := CurrIdx;
      FIndex2Handle[CurrIdx] := HandleIdx;
    end;
end;

procedure TGBinHeapMin.SiftDown(aIndex: SizeInt);
var
  CurrIdx, NextIdx, HighIdx, HandleIdx: SizeInt;
  v: T;
begin
  HighIdx := Pred(Count);
  if HighIdx > 0 then
    begin
      CurrIdx := aIndex;
      NextIdx := Succ(aIndex shl 1);
      v := FHeap[aIndex];
      HandleIdx := FIndex2Handle[aIndex];
      while NextIdx <= HighIdx do
        begin
          if (Succ(NextIdx) <= HighIdx) and (FHeap[NextIdx] > FHeap[Succ(NextIdx)]) then
            Inc(NextIdx);
          FHeap[CurrIdx] := FHeap[NextIdx];
          FHandle2Index[FIndex2Handle[NextIdx]] := CurrIdx;
          FIndex2Handle[CurrIdx] := FIndex2Handle[NextIdx];
          CurrIdx := NextIdx;
          NextIdx := Succ(NextIdx shl 1);
        end;
      NextIdx := Pred(CurrIdx) shr 1;
      while (CurrIdx > 0) and (v < FHeap[NextIdx]) do
        begin
          FHeap[CurrIdx] := FHeap[NextIdx];
          FHandle2Index[FIndex2Handle[NextIdx]] := CurrIdx;
          FIndex2Handle[CurrIdx] := FIndex2Handle[NextIdx];
          CurrIdx := NextIdx;
          NextIdx := Pred(NextIdx) shr 1;
        end;
      FHeap[CurrIdx] := v;
      FHandle2Index[HandleIdx] := CurrIdx;
      FIndex2Handle[CurrIdx] := HandleIdx;
    end;
end;

constructor TGBinHeapMin.Create(aSize: SizeInt);
begin
  FCount := 0;
  if aSize > 0 then
    begin
      System.SetLength(FHandle2Index, aSize);
      System.SetLength(FHeap, aSize);
      System.SetLength(FIndex2Handle, aSize);
    end;
end;

procedure TGBinHeapMin.MakeEmpty;
begin
  FCount := 0;
end;

function TGBinHeapMin.TryDequeue(out aValue: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    begin
      Dec(FCount);
      aValue := FHeap[0];
      FHeap[0] := FHeap[Count];
      FHandle2Index[FIndex2Handle[Count]] := 0;
      FIndex2Handle[0] := FIndex2Handle[Count];
      FHeap[Count] := Default(T);
      SiftDown(0);
    end;
end;

procedure TGBinHeapMin.Enqueue(aHandle: SizeInt; constref aValue: T);
var
  InsertIdx: SizeInt;
begin
  InsertIdx := Count;
  Inc(FCount);
  FHeap[InsertIdx] := aValue;
  FHandle2Index[aHandle] := InsertIdx;
  FIndex2Handle[InsertIdx] := aHandle;
  FloatUp(InsertIdx);
end;

procedure TGBinHeapMin.Update(aHandle: SizeInt; constref aNewValue: T);
var
  I: SizeInt;
begin
  I := FHandle2Index[aHandle];
  if aNewValue < FHeap[I] then
    begin
      FHeap[I] := aNewValue;
      FloatUp(I);
    end
  else
    if aNewValue > FHeap[I] then
      begin
        FHeap[I] := aNewValue;
        SiftDown(I);
      end;
end;

function TGBinHeapMin.Peek(aHandle: SizeInt): T;
begin
  Result := FHeap[FHandle2Index[aHandle]];
end;

{ TGPairHeapMin.TNode }

function TGPairHeapMin.TNode.AddChild(aNode: PNode): PNode;
begin
  Result := @Self;
  aNode^.Prev := Result;
  Sibling :=  aNode^.Sibling;
  if Sibling <> nil then
    Sibling^.Prev := @Self;
  aNode^.Sibling := Child;
  if Child <> nil then
    Child^.Prev := aNode;
  Child := aNode;
end;

{ TGPairHeapMin }

function TGPairHeapMin.GetCapacity: SizeInt;
begin
  Result := System.Length(FNodeList);
end;

function TGPairHeapMin.NewNode(constref aValue: T; aHandle: SizeInt): PNode;
begin
  Result := @FNodeList[aHandle];
  Inc(FCount);
  Result^.Data := aValue;
  Result^.Prev := nil;
  Result^.Child := nil;
  Result^.Sibling := nil;
end;

function TGPairHeapMin.DequeueItem: T;
begin
  Result := FRoot^.Data;
  Dec(FCount);
  FRoot := TwoPassMerge(FRoot^.Child);
  if FRoot <> nil then
    FRoot^.Prev := nil;
end;

procedure TGPairHeapMin.RootMerge(aNode: PNode);
begin
  FRoot := NodeMerge(FRoot, aNode);
  if FRoot <> nil then
    FRoot^.Prev := nil;
end;

procedure TGPairHeapMin.ExtractNode(aNode: PNode);
begin
  if aNode <> FRoot then
    begin
      CutNode(aNode);
      RootMerge(TwoPassMerge(aNode^.Child));
    end
  else
    begin
      FRoot := TwoPassMerge(FRoot^.Child);
      if FRoot <> nil then
        FRoot^.Prev := nil;
    end;
  Dec(FCount);
end;

class function TGPairHeapMin.NodeMerge(L, R: PNode): PNode;
begin
  if L <> nil then
    if R <> nil then
      if L^.Data <= R^.Data then
        Result := L^.AddChild(R)
      else
        Result := R^.AddChild(L)
    else
      Result := L
  else
    Result := R;
end;

class function TGPairHeapMin.TwoPassMerge(aNode: PNode): PNode;
var
  CurrNode, NextNode: PNode;
begin
  Result := nil;
  while (aNode <> nil) and (aNode^.Sibling <> nil) do
    begin
      NextNode := aNode^.Sibling;
      CurrNode := aNode;
      aNode := NextNode^.Sibling;
      NextNode^.Sibling := nil;
      CurrNode^.Sibling := nil;
      Result := NodeMerge(Result, NodeMerge(CurrNode, NextNode));
    end;
  Result := NodeMerge(Result, aNode);
end;

class procedure TGPairHeapMin.CutNode(aNode: PNode);
begin
  if aNode^.Sibling <> nil then
    aNode^.Sibling^.Prev := aNode^.Prev;
  if aNode^.Prev^.Child = aNode then
    aNode^.Prev^.Child := aNode^.Sibling
  else
    aNode^.Prev^.Sibling := aNode^.Sibling;
  aNode^.Sibling := nil;
end;

constructor TGPairHeapMin.Create(aSize: SizeInt);
begin
  System.SetLength(FNodeList, aSize);
  MakeEmpty;
end;

procedure TGPairHeapMin.MakeEmpty;
begin
  FRoot := nil;
  FCount := 0;
end;

function TGPairHeapMin.TryDequeue(out aValue: T): Boolean;
begin
  Result := Count <> 0;
  if Result then
    aValue := DequeueItem;
end;

function TGPairHeapMin.Dequeue: T;
begin
  if Count > 0 then
    Result := DequeueItem
  else
    raise ELGAccessEmpty.Create(SECantAccessEmpty);
end;

procedure TGPairHeapMin.Enqueue(aHandle: SizeInt; constref aValue: T);
begin
  RootMerge(NewNode(aValue, aHandle));
end;

procedure TGPairHeapMin.Update(aHandle: SizeInt; constref aNewValue: T);
var
  Node: PNode;
begin
  Node := @FNodeList[aHandle];
  if aNewValue < Node^.Data then
    begin
      Node^.Data := aNewValue;
      if Node <> FRoot then
        begin
          CutNode(Node);
          RootMerge(Node);
        end;
    end;
end;

procedure TGPairHeapMin.Remove(aHandle: SizeInt);
begin
  ExtractNode(@FNodeList[aHandle]);
end;

function TGPairHeapMin.Peek(aHandle: SizeInt): T;
begin
  Result := FNodeList[aHandle].Data;
end;

{ TGPairHeapMax.TNode }

function TGPairHeapMax.TNode.AddChild(aNode: PNode): PNode;
begin
  Result := @Self;
  aNode^.Prev := Result;
  Sibling :=  aNode^.Sibling;
  if Sibling <> nil then
    Sibling^.Prev := @Self;
  aNode^.Sibling := Child;
  if Child <> nil then
    Child^.Prev := aNode;
  Child := aNode;
end;

{ TGPairHeapMax }

function TGPairHeapMax.GetCapacity: SizeInt;
begin
  Result := System.Length(FNodeList);
end;

function TGPairHeapMax.NewNode(constref aValue: T; aHandle: SizeInt): PNode;
begin
  Result := @FNodeList[aHandle];
  Inc(FCount);
  Result^.Data := aValue;
  Result^.Prev := nil;
  Result^.Child := nil;
  Result^.Sibling := nil;
end;

function TGPairHeapMax.DequeueItem: T;
begin
  Result := FRoot^.Data;
  Dec(FCount);
  FRoot := TwoPassMerge(FRoot^.Child);
  if FRoot <> nil then
    FRoot^.Prev := nil;
end;

procedure TGPairHeapMax.RootMerge(aNode: PNode);
begin
  FRoot := NodeMerge(FRoot, aNode);
  if FRoot <> nil then
    FRoot^.Prev := nil;
end;

class function TGPairHeapMax.NodeMerge(L, R: PNode): PNode;
begin
  if L <> nil then
    if R <> nil then
      if L^.Data >= R^.Data then
        Result := L^.AddChild(R)
      else
        Result := R^.AddChild(L)
    else
      Result := L
  else
    Result := R;
end;

class function TGPairHeapMax.TwoPassMerge(aNode: PNode): PNode;
var
  CurrNode, NextNode: PNode;
begin
  Result := nil;
  while (aNode <> nil) and (aNode^.Sibling <> nil) do
    begin
      NextNode := aNode^.Sibling;
      CurrNode := aNode;
      aNode := NextNode^.Sibling;
      NextNode^.Sibling := nil;
      CurrNode^.Sibling := nil;
      Result := NodeMerge(Result, NodeMerge(CurrNode, NextNode));
    end;
  Result := NodeMerge(Result, aNode);
end;

class procedure TGPairHeapMax.CutNode(aNode: PNode);
begin
  if aNode^.Sibling <> nil then
    aNode^.Sibling^.Prev := aNode^.Prev;
  if aNode^.Prev^.Child = aNode then
    aNode^.Prev^.Child := aNode^.Sibling
  else
    aNode^.Prev^.Sibling := aNode^.Sibling;
  aNode^.Sibling := nil;
end;

constructor TGPairHeapMax.Create(aSize: SizeInt);
begin
  System.SetLength(FNodeList, aSize);
  MakeEmpty;
end;

procedure TGPairHeapMax.MakeEmpty;
begin
  FRoot := nil;
  FCount := 0;
end;

function TGPairHeapMax.TryDequeue(out aValue: T): Boolean;
begin
  Result := Count <> 0;
  if Result then
    aValue := DequeueItem;
end;

function TGPairHeapMax.Dequeue: T;
begin
  if Count > 0 then
    Result := DequeueItem
  else
    raise ELGAccessEmpty.Create(SECantAccessEmpty);
end;

procedure TGPairHeapMax.Enqueue(aHandle: SizeInt; constref aValue: T);
begin
  RootMerge(NewNode(aValue, aHandle));
end;

procedure TGPairHeapMax.Update(aHandle: SizeInt; constref aNewValue: T);
var
  Node: PNode;
begin
  Node := @FNodeList[aHandle];
  if aNewValue > Node^.Data then
    begin
      Node^.Data := aNewValue;
      if Node <> FRoot then
        begin
          CutNode(Node);
          RootMerge(Node);
        end;
    end;
end;

function TGPairHeapMax.Peek(aHandle: SizeInt): T;
begin
  Result := FNodeList[aHandle].Data;
end;

{ TGJoinableHashList }

function TGJoinableHashList.GetCount: SizeInt;
begin
  Result := FTable.Count;
end;

function TGJoinableHashList.GetEnumerator: TEnumerator;
begin
  Result := FTable.GetEnumerator;
end;

procedure TGJoinableHashList.EnsureCapacity(aValue: SizeInt);
begin
  FTable.EnsureCapacity(aValue);
end;

procedure TGJoinableHashList.Add(constref aValue: TEntry);
var
  p: PEntry;
begin
  if FTable.FindOrAdd(aValue.Key, p) then
    p^.Weight += aValue.Weight
  else
    p^ := aValue;
end;

procedure TGJoinableHashList.AddAll(constref aList: TGJoinableHashList);
var
  p: PEntry;
begin
  for p in aList do
    Add(p^);
end;

procedure TGJoinableHashList.Remove(aValue: SizeInt);
begin
  FTable.Remove(aValue);
end;

{ TGSimpleWeight }

constructor TGSimpleWeight.Create(aValue: T);
begin
  Weight := aValue;
end;

{ TSimpleStack }

function TSimpleStack.GetCapacity: SizeInt;
begin
  Result := System.Length(Items);
end;

constructor TSimpleStack.Create(aSize: SizeInt);
begin
  System.SetLength(Items, aSize);
  Top := NULL_INDEX;
end;

function TSimpleStack.IsEmpty: Boolean;
begin
  Result := Top < 0;
end;

function TSimpleStack.NonEmpty: Boolean;
begin
  Result := Top >= 0;
end;

procedure TSimpleStack.Push(aValue: SizeInt);
begin
  Inc(Top);
  Items[Top] := aValue;
end;

function TSimpleStack.Pop: SizeInt;
begin
  Result := Items[Top];
  Dec(Top);
end;

function TSimpleStack.TryPop(out aValue: SizeInt): Boolean;
begin
  Result := Top >= 0;
  if Result then
    aValue := Pop;
end;

function TSimpleStack.Peek: SizeInt;
begin
  Result := Items[Top];
end;

function TSimpleStack.TryPeek(out aValue: SizeInt): Boolean;
begin
  Result := Top >= 0;
  if Result then
    aValue := Peek;
end;

{ TGWeightPathHelper.TWeightEdge }

class operator TGWeightPathHelper.TWeightEdge. = (constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight = R.Weight;
end;

class operator TGWeightPathHelper.TWeightEdge.<>(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight <> R.Weight;
end;

class operator TGWeightPathHelper.TWeightEdge.>(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight > R.Weight;
end;

class operator TGWeightPathHelper.TWeightEdge.<(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight < R.Weight;
end;

class operator TGWeightPathHelper.TWeightEdge.>=(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight >= R.Weight;
end;

class operator TGWeightPathHelper.TWeightEdge.<=(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight <= R.Weight;
end;

constructor TGWeightPathHelper.TWeightEdge.Create(s, d: SizeInt; w: TWeight);
begin
  Source := s;
  Destination := d;
  Weight := w;
end;

{ TGWeightPathHelper.TWeightItem }

class operator TGWeightPathHelper.TWeightItem. = (constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight = R.Weight;
end;

class operator TGWeightPathHelper.TWeightItem.<>(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight <> R.Weight;
end;

class operator TGWeightPathHelper.TWeightItem.>(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight > R.Weight;
end;

class operator TGWeightPathHelper.TWeightItem.<(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight < R.Weight;
end;

class operator TGWeightPathHelper.TWeightItem.>=(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight >= R.Weight;
end;

class operator TGWeightPathHelper.TWeightItem.<=(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight <= R.Weight;
end;

constructor TGWeightPathHelper.TWeightItem.Create(aIndex: SizeInt; w: TWeight);
begin
  Index := aIndex;
  Weight := w;
end;

{ TGWeightPathHelper.TRankItem }

class operator TGWeightPathHelper.TRankItem. = (constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank = R.Rank;
end;

class operator TGWeightPathHelper.TRankItem.<>(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank <> R.Rank;
end;

class operator TGWeightPathHelper.TRankItem.>(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank > R.Rank;
end;

class operator TGWeightPathHelper.TRankItem.<(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank < R.Rank;
end;

class operator TGWeightPathHelper.TRankItem.>=(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank >= R.Rank;
end;

class operator TGWeightPathHelper.TRankItem.<=(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank <= R.Rank;
end;

constructor TGWeightPathHelper.TRankItem.Create(aIndex: SizeInt; aRank, aWeight: TWeight);
begin
  Index := aIndex;
  Rank := aRank;
  Weight := aWeight;
end;

{ TGWeightPathHelper }

class constructor TGWeightPathHelper.Init;
begin
  CFInfWeight := TWeight.MaxValue;
  CFNegInfWeight := TWeight.MinValue;
  CFZeroWeight := Default(TWeight);
end;

class function TGWeightPathHelper.CreateAndFill(aValue: TWeight; aSize: SizeInt): TWeightArray;
var
  I: SizeInt;
begin
  System.SetLength(Result, aSize);
  for I := 0 to Pred(aSize) do
    Result[I] := aValue;
end;

class function TGWeightPathHelper.wMax(L, R: TWeight): TWeight;
begin
  if L >= R then
    Result := L
  else
    Result := R;
end;

class function TGWeightPathHelper.ExtractCycle(aRoot, aLen: SizeInt; constref aTree: TIntArray): TIntArray;
var
  v: TIntVector;
  I, J: SizeInt;
begin
  for I := 1 to aLen do
    aRoot := aTree[aRoot];
  I := aRoot;
  v.Add(aRoot);
  repeat
    I := aTree[I];
    v.Add(I);
  until I = aRoot;
  System.SetLength(Result, v.Count);
  J := 0;
  for I in v.Reverse do
    begin
      Result[J] := I;
      Inc(J);
    end;
end;

class function TGWeightPathHelper.DijkstraSssp(g: TGraph; aSrc: SizeInt): TWeightArray;
var
  Queue: TPairHeap;
  Reached,
  InQueue: TGraph.TBitVector;
  Item: TWeightItem;
  p: TGraph.PAdjItem;
begin
  Result := CreateWeightArray(g.VertexCount);
  Queue := TPairHeap.Create(g.VertexCount);
  Reached.Size := g.VertexCount;
  InQueue.Size := g.VertexCount;
  Item := TWeightItem.Create(aSrc, ZeroWeight);
  repeat
    Result[Item.Index] := Item.Weight;
    Reached[Item.Index] := True;
    for p in g.AdjLists[Item.Index]^ do
      if not Reached[p^.Key] then
        if not InQueue[p^.Key] then
          begin
            Queue.Enqueue(p^.Key, TWeightItem.Create(p^.Key, p^.Data.Weight + Item.Weight));
            InQueue[p^.Key] := True;
          end
        else
          if p^.Data.Weight + Item.Weight < Queue.Peek(p^.Key).Weight then
            Queue.Update(p^.Key, TWeightItem.Create(p^.Key, p^.Data.Weight + Item.Weight));
  until not Queue.TryDequeue(Item);
end;

class function TGWeightPathHelper.DijkstraSssp(g: TGraph; aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
var
  Queue: TPairHeap;
  Reached,
  InQueue: TGraph.TBitVector;
  Item: TWeightItem;
  p: TGraph.PAdjItem;
begin
  Result := CreateWeightArray(g.VertexCount);
  Queue := TPairHeap.Create(g.VertexCount);
  aPathTree := g.CreateIntArray;
  Reached.Size := g.VertexCount;
  InQueue.Size := g.VertexCount;
  Item := TWeightItem.Create(aSrc, ZeroWeight);
  repeat
    Result[Item.Index] := Item.Weight;
    Reached[Item.Index] := True;
    for p in g.AdjLists[Item.Index]^ do
      if not Reached[p^.Key] then
        if not InQueue[p^.Key] then
          begin
            Queue.Enqueue(p^.Key, TWeightItem.Create(p^.Key, p^.Data.Weight + Item.Weight));
            aPathTree[p^.Key] := Item.Index;
            InQueue[p^.Key] := True;
          end
        else
          if p^.Data.Weight + Item.Weight < Queue.Peek(p^.Key).Weight then
            begin
              Queue.Update(p^.Key, TWeightItem.Create(p^.Key, p^.Data.Weight + Item.Weight));
              aPathTree[p^.Key] := Item.Index;
            end;
  until not Queue.TryDequeue(Item);
end;

class function TGWeightPathHelper.DijkstraPath(g: TGraph; aSrc, aDst: SizeInt): TWeight;
var
  Queue: TBinHeap;
  Reached,
  InQueue: TGraph.TBitVector;
  Item: TWeightItem;
  p: TGraph.PAdjItem;
begin
  Queue := TBinHeap.Create(g.VertexCount);
  Reached.Size := g.VertexCount;
  InQueue.Size := g.VertexCount;
  Item := TWeightItem.Create(aSrc, ZeroWeight);
  repeat
    if Item.Index = aDst then
      exit(Item.Weight);
    Reached[Item.Index] := True;
    for p in g.AdjLists[Item.Index]^ do
      if not Reached[p^.Key] then
        if not InQueue[p^.Key] then
          begin
            Queue.Enqueue(p^.Key, TWeightItem.Create(p^.Key, p^.Data.Weight + Item.Weight));
            InQueue[p^.Key] := True;
          end
        else
          if p^.Data.Weight + Item.Weight < Queue.Peek(p^.Key).Weight then
            Queue.Update(p^.Key, TWeightItem.Create(p^.Key, p^.Data.Weight + Item.Weight));
  until not Queue.TryDequeue(Item);
  Result := InfWeight;
end;

class function TGWeightPathHelper.DijkstraPath(g: TGraph; aSrc, aDst: SizeInt; out aWeight: TWeight): TIntArray;
var
  Queue: TBinHeap;
  Parents: TIntArray;
  Reached,
  InQueue: TGraph.TBitVector;
  Item: TWeightItem;
  p: TGraph.PAdjItem;
begin
  Queue := TBinHeap.Create(g.VertexCount);
  Parents := g.CreateIntArray;
  Reached.Size := g.VertexCount;
  InQueue.Size := g.VertexCount;
  Item := TWeightItem.Create(aSrc, ZeroWeight);
  repeat
    if Item.Index = aDst then
      begin
        aWeight := Item.Weight;
        exit(g.TreePathTo(Parents, aDst));
      end;
    Reached[Item.Index] := True;
    for p in g.AdjLists[Item.Index]^ do
      if not Reached[p^.Key] then
        if not InQueue[p^.Key] then
          begin
            Queue.Enqueue(p^.Key, TWeightItem.Create(p^.Key, p^.Data.Weight + Item.Weight));
            Parents[p^.Key] := Item.Index;
            InQueue[p^.Key] := True;
          end
        else
          if p^.Data.Weight + Item.Weight < Queue.Peek(p^.Key).Weight then
            begin
              Queue.Update(p^.Key, TWeightItem.Create(p^.Key, p^.Data.Weight + Item.Weight));
              Parents[p^.Key] := Item.Index;
            end;
  until not Queue.TryDequeue(Item);
  aWeight := InfWeight;
  Result := [];
end;

class function TGWeightPathHelper.AStar(g: TGraph; aSrc, aDst: SizeInt; out aWeight: TWeight;
  aEst: TEstimate): TIntArray;
var
  Queue: TAStarHeap;
  Parents: TIntArray;
  Reached,
  InQueue: TGraph.TBitVector;
  Item: TRankItem;
  Relax: TWeight;
  p: TGraph.PAdjItem;
begin
  Queue := TAStarHeap.Create(g.VertexCount);
  Parents := g.CreateIntArray;
  Reached.Size := g.VertexCount;
  InQueue.Size := g.VertexCount;
  Item := TRankItem.Create(aSrc, aEst(g.Items[aSrc], g.Items[aDst]), ZeroWeight);
  repeat
    if {%H-}Item.Index = aDst then
      begin
        aWeight := Item.Weight;
        exit(g.TreePathTo(Parents, aDst));
      end;
    Reached[Item.Index] := True;
    for p in g.AdjLists[Item.Index]^ do
      if not Reached[p^.Key] then
        begin
          Relax := p^.Data.Weight + Item.Weight;
          if not InQueue[p^.Key] then
            begin
              Queue.Enqueue(p^.Key, TRankItem.Create(
                p^.Key, Relax + aEst(g.Items[p^.Key], g.Items[aDst]), Relax));
              Parents[p^.Key] := Item.Index;
              InQueue[p^.Key] := True;
            end
          else
            if Relax < Queue.Peek(p^.Key).Weight then
              begin
                Queue.Update(p^.Key, TRankItem.Create(
                  p^.Key, Relax + aEst(g.Items[p^.Key], g.Items[aDst]), Relax));
                Parents[p^.Key] := Item.Index;
              end;
        end;
  until not Queue.TryDequeue(Item);
  aWeight := InfWeight;
  Result := [];
end;

class function TGWeightPathHelper.SpfaBase(g: TGraph; aSrc: SizeInt; out aTree: TIntArray;
  out aWeights: TWeightArray): SizeInt;
var
  Queue: TIntDeque;
  Dist: TIntArray;
  InQueue: TGraph.TBitVector;
  Curr, Next, Top, VertCount: SizeInt;
  p: TGraph.PAdjItem;
begin
  VertCount := g.VertexCount;
  aWeights := CreateWeightArray(VertCount);
  Dist := g.CreateIntArray;
  aTree := g.CreateIntArray;
  {%H-}Queue.EnsureCapacity(VertCount);
  InQueue.Size := VertCount;
  aWeights[aSrc] := ZeroWeight;
  Dist[aSrc] := 0;
  Curr := aSrc;
  repeat
    InQueue[Curr] := False;
    if (aTree[Curr] <> NULL_INDEX) and InQueue[aTree[Curr]] then
      continue;
    for p in g.AdjLists[Curr]^ do
      begin
        Next := p^.Destination;
        if aWeights[Curr] + p^.Data.Weight < aWeights[Next] then
          //todo: need some kind of protection from overflow ???
          begin
            aWeights[Next] := aWeights[Curr] + p^.Data.Weight;
            aTree[Next] := Curr;
            if (Next = aSrc) or (Succ(Dist[Curr]) >= VertCount) then
              exit(Next);
            Dist[Next] := Succ(Dist[Curr]);
            if not InQueue[Next] then
              begin
                if Queue.TryPeekFirst(Top) and (aWeights[Next] < aWeights[{%H-}Top]) then
                  Queue.PushFirst(Next)
                else
                  Queue.PushLast(Next);
                InQueue[Next] := True;
              end;
          end;
      end;
  until not Queue{%H-}.TryPopFirst(Curr);
  Result := NULL_INDEX;
end;

class function TGWeightPathHelper.Spfa2Base(g: TGraph; aSrc: SizeInt; out aTree: TIntArray;
  out aWeights: TWeightArray): SizeInt;
var
  v1, v2: TBoolVector;
  Dist: TIntArray;
  Curr, Next, VertCount: SizeInt;
  CurrPass, NextPass: ^TBoolVector;
  p: TGraph.PAdjItem;
begin
  VertCount := g.VertexCount;
  aWeights := CreateWeightArray(VertCount);
  Dist := g.CreateIntArray;
  aTree := g.CreateIntArray;
  v1.Size := VertCount;
  v2.Size := VertCount;
  aWeights[aSrc] := ZeroWeight;
  v2[aSrc] := True;
  Dist[aSrc] := 0;
  CurrPass := @v1;
  NextPass := @v2;
  repeat
    p := Pointer(CurrPass);
    CurrPass := NextPass;
    NextPass := Pointer(p);
    for Curr in CurrPass^ do
      begin
        CurrPass^[Curr] := False;
        for p in g.AdjLists[Curr]^ do
          begin
            Next := p^.Destination;
            if aWeights[Curr] + p^.Data.Weight < aWeights[Next] then
              begin
                aWeights[Next] := aWeights[Curr] + p^.Data.Weight;
                aTree[Next] := Curr;
                if (Next = aSrc) or (Succ(Dist[Curr]) >= VertCount) then
                  exit(Next);
                Dist[Next] := Succ(Dist[Curr]);
                NextPass^[Next] := True;
              end;
          end;
      end;
  until NextPass^.IsEmpty;
  Result := NULL_INDEX;
end;

class function TGWeightPathHelper.BfmtBase(g: TGraph; aSrc: SizeInt; out aParents: TIntArray;
  out aWeights: TWeightArray): SizeInt;
var
  Queue, TreePrev, TreeNext, Level: TIntArray;
  InQueue, Active: TGraph.TBitVector;
  Curr, Next, Prev, Post, Test, CurrLevel, vCount, qCount: SizeInt;
  p: TGraph.PAdjItem;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  Queue := g.CreateIntArray;
  aParents := g.CreateIntArray;
  TreePrev := g.CreateIntArray;
  TreeNext := g.CreateIntArray;
  Level := g.CreateIntArray;
  vCount := g.VertexCount;
  qCount := 0;
  InQueue.Size := vCount;
  Active.Size := vCount;
  aWeights := CreateWeightArray(vCount);
  aWeights[aSrc] := ZeroWeight;
  aParents[aSrc] := aSrc;
  TreePrev[aSrc] := aSrc;
  TreeNext[aSrc] := aSrc;
  Active[aSrc] := True;
  Queue[qTail] := aSrc;
  Inc(qTail);
  Inc(qCount);
  while qHead <> qTail do
    begin
      Curr := Queue[qHead];
      Inc(qHead);
      Dec(qCount);
      if qHead = vCount then
        qHead := 0;
      InQueue[Curr] := False;
      if not Active[Curr] then
        continue;
      Active[Curr] := False;
      for p in g.AdjLists[Curr]^ do
        begin
          Next := p^.Destination;
          if aWeights[Next] > aWeights[Curr] + p^.Data.Weight then
            begin
              aWeights[Next] := aWeights[Curr] + p^.Data.Weight;
              if TreePrev[Next] <> NULL_INDEX then
                begin
                  Prev := TreePrev[Next];
                  Test := Next;
                  CurrLevel := 0;
                  repeat
                    if Test = Curr then
                      begin
                        aParents[Next] := Curr;
                        exit(Next);
                      end;
                    CurrLevel += Level[Test];
                    TreePrev[Test] := NULL_INDEX;
                    Level[Test] := NULL_INDEX;
                    Active[Test] := False;
                    Test := TreeNext[Test];
                  until CurrLevel < 0;
                  Dec(Level[aParents[Next]]);
                  TreeNext[Prev] := Test;
                  TreePrev[Test] := Prev;
                end;
              aParents[Next] := Curr;
              Inc(Level[Curr]);
              Post := TreeNext[Curr];
              TreeNext[Curr] := Next;
              TreePrev[Next] := Curr;
              TreeNext[Next] := Post;
              TreePrev[Post] := Next;
              if not InQueue[Next] then
                begin
                  if (qCount <> 0) and (aWeights[Next] < aWeights[Queue[qHead]]) then
                    begin
                      Dec(qHead);
                      if qHead = NULL_INDEX then
                        qHead := Pred(vCount);
                      Queue[qHead] := Next;
                    end
                  else
                    begin
                      Queue[qTail] := Next;
                      Inc(qTail);
                      if qTail = vCount then
                        qTail := 0;
                    end;
                  Inc(qCount);
                  InQueue[Next] := True;
                end;
              Active[Next] := True;
            end;
        end;
    end;
  aParents[aSrc] := NULL_INDEX;
  Result := NULL_INDEX;
end;

class function TGWeightPathHelper.NegDetect(g: TGraph; aSrc: SizeInt): TIntArray;
var
  Parents: TIntArray;
  Weights: TWeightArray;
  Cycle: SizeInt;
begin
  Cycle := BfmtBase(g, aSrc, Parents, Weights);
  if Cycle <> NULL_INDEX then
    Result := ExtractCycle(Cycle, g.VertexCount, Parents)
  else
    Result := nil;
end;

class function TGWeightPathHelper.BfmtSssp(g: TGraph; aSrc: SizeInt; out aWeights: TWeightArray): Boolean;
var
  Parents: TIntArray;
begin
  Result := BfmtBase(g, aSrc, Parents, aWeights) = NULL_INDEX;
  if not Result then
    aWeights := nil;
end;

class function TGWeightPathHelper.BfmtSssp(g: TGraph; aSrc: SizeInt; out aPaths: TIntArray;
  out aWeights: TWeightArray): Boolean;
var
  Cycle: SizeInt;
begin
  Cycle := BfmtBase(g, aSrc, aPaths, aWeights);
  Result := Cycle = NULL_INDEX;
  if not Result then
    begin
      aWeights := nil;
      aPaths := ExtractCycle(Cycle, g.VertexCount, aPaths);
    end;
end;

class function TGWeightPathHelper.BfmtPath(g: TGraph; aSrc, aDst: SizeInt; out aPath: TIntArray;
  out aWeight: TWeight): Boolean;
var
  Weights: TWeightArray;
begin
  aWeight := InfWeight;
  if BfmtSssp(g, aSrc, aPath, Weights) then
    begin
      Result := aPath[aDst] <> NULL_INDEX;
      if Result then
        begin
          aWeight := Weights[aDst];
          aPath := g.TreePathTo(aPath, aDst);
        end
      else
        aWeight := InfWeight;
    end
  else
    begin
      Result := False;
      aWeight := ZeroWeight;
    end;
end;

class function TGWeightPathHelper.CreateWeightArray(aLen: SizeInt): TWeightArray;
begin
  Result := CreateAndFill(InfWeight, aLen);
end;

class function TGWeightPathHelper.CreateWeightArrayNI(aLen: SizeInt): TWeightArray;
begin
  Result := CreateAndFill(NegInfWeight, aLen);
end;

class function TGWeightPathHelper.CreateWeightArrayZ(aLen: SizeInt): TWeightArray;
begin
  Result := CreateAndFill(ZeroWeight, aLen);
end;
