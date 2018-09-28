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

{ TGCustomGraph.TIntSet.TEnumerator }

function TGCustomGraph.TIntSet.TEnumerator.GetCurrent: SizeInt;
begin
  Result := pCurr^;
end;

function TGCustomGraph.TIntSet.TEnumerator.MoveNext: Boolean;
begin
  Result := pCurr < pLast;
  pCurr += Ord(Result);
end;

{ TIntSet }

procedure TGCustomGraph.TIntSet.Expand;
begin
  System.SetLength(FItems, System.Length(FItems) + GRAPH_ADJLIST_GROW);
end;

class operator TGCustomGraph.TIntSet.Initialize(var aList: TIntSet);
begin
  aList.FCount := 0;
end;

procedure TGCustomGraph.TIntSet.InitRange(aRange: SizeInt);
var
  I: SizeInt;
begin
  System.SetLength(FItems, aRange);
  if aRange > 0 then
    begin
      for I := 0 to Pred(aRange) do
        FItems[I] := I;
      FCount := aRange;
    end
  else
    FCount := 0;
end;

function TGCustomGraph.TIntSet.GetEnumerator: TEnumerator;
begin
  Result.pCurr := PSizeInt(Pointer(FItems)) - Ord(Count > 0);
  Result.pLast := PSizeInt(Pointer(FItems)) + (Pred(Count) and (-SizeInt(Count > 0)));
end;

function TGCustomGraph.TIntSet.ToArray: TIntArray;
begin
  Result := System.Copy(FItems, 0, Count);
end;

procedure TGCustomGraph.TIntSet.Assign(constref aValue: TIntSet);
begin
  FCount := aValue.Count;
  if Count <> 0 then
    FItems := System.Copy(aValue.FItems, 0, Count);
end;

procedure TGCustomGraph.TIntSet.AssignArray(constref a: TIntArray);
begin
  FItems := System.Copy(a);
  FCount := System.Length(FItems);
end;

procedure TGCustomGraph.TIntSet.AssignList(aList: PAdjList);
var
  p: PAdjItem;
  I: SizeInt = 0;
begin
  FCount := aList^.Count;
  System.SetLength(FItems, Count);
  for p in aList^ do
    begin
      FItems[I] := p^.Destination;
      Inc(I);
    end;
end;

function TGCustomGraph.TIntSet.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TGCustomGraph.TIntSet.NonEmpty: Boolean;
begin
  Result := Count <> 0;
end;

procedure TGCustomGraph.TIntSet.MakeEmpty;
begin
  FCount := 0;
end;

function TGCustomGraph.TIntSet.FindFirst(out aValue: SizeInt): Boolean;
begin
  Result := Count <> 0;
  if Result then
    aValue := FItems[Pred(Count)]
end;

function TGCustomGraph.TIntSet.GetItem(aIndex: SizeInt): SizeInt;
begin
  Result := FItems[aIndex];
end;

function TGCustomGraph.TIntSet.Contains(aValue: SizeInt): Boolean;
begin
  Result := Find(aValue) >= 0;
end;

function TGCustomGraph.TIntSet.ContainsAny(constref aValue: TIntSet): Boolean;
var
  I: SizeInt;
begin
  if NonEmpty then
    for I in aValue do
      if Contains(I) then
        exit(True);
  Result := False;
end;

function TGCustomGraph.TIntSet.ContainsAll(constref aValue: TIntSet): Boolean;
var
  I: SizeInt;
begin
  if Count >= aValue.Count then
    begin
      for I in aValue do
        if not Contains(I) then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

function TGCustomGraph.TIntSet.Find(aValue: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  for I := 0 to Pred(Count) do
    if FItems[I] = aValue then
      exit(I);
  Result := -1;
end;

function TGCustomGraph.TIntSet.Add(aValue: SizeInt): Boolean;
begin
  Result := Find(aValue) < 0;
  if Result then
    begin
      if Count = System.Length(FItems) then
        Expand;
      FItems[Count] := aValue;
      Inc(FCount);
    end;
end;

function TGCustomGraph.TIntSet.Add(constref a: TIntArray): SizeInt;
var
  I: SizeInt;
begin
  Result := Count;
  for I in a do
    Add(I);
  Result := Count - Result;
end;

function TGCustomGraph.TIntSet.Join(constref aValue: TIntSet): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I in aValue do
    Result += Ord(Add(I));
end;

procedure TGCustomGraph.TIntSet.Push(aValue: SizeInt);
begin
  if Count = System.Length(FItems) then
    Expand;
  FItems[Count] := aValue;
  Inc(FCount);
end;

function TGCustomGraph.TIntSet.Pop: SizeInt;
begin
  if Count <> 0 then
    begin
      Dec(FCount);
      Result := FItems[Count];
    end
  else
    begin
      EGraphError.Create(SECantAccessEmpty);
      Result := -1;
    end;
end;

function TGCustomGraph.TIntSet.TryPop(out aValue: SizeInt): Boolean;
begin
  Result := Count <> 0;
  if Result then
    begin
      Dec(FCount);
      aValue := FItems[Count];
    end;
end;

function TGCustomGraph.TIntSet.Last: SizeInt;
begin
  if Count <> 0 then
    Result := FItems[Count]
  else
    begin
      EGraphError.Create(SECantAccessEmpty);
      Result := -1;
    end;
end;

procedure TGCustomGraph.TIntSet.Subtract(constref aValue: TIntSet);
var
  I, Pos: SizeInt;
begin
  if aValue.NonEmpty then
    begin
      Pos := 0;
      for I := 0 to Pred(Count) do
        if aValue.Contains(FItems[I]) then
          Dec(FCount)
        else
          begin
            FItems[Pos] := FItems[I];
            Inc(Pos);
          end;
    end;
end;

procedure TGCustomGraph.TIntSet.Subtract(PList: PAdjList);
var
  I, Pos: SizeInt;
begin
  if PList^.NonEmpty then
    begin
      Pos := 0;
      for I := 0 to Pred(Count) do
        if PList^.Contains(FItems[I]) then
          Dec(FCount)
        else
          begin
            FItems[Pos] := FItems[I];
            Inc(Pos);
          end;
    end;
end;

function TGCustomGraph.TIntSet.Difference(constref aValue: TIntSet): TIntSet;
begin
  Result.Assign(Self);
  Result.Subtract(aValue);
end;

procedure TGCustomGraph.TIntSet.Intersect(constref aValue: TIntSet);
var
  I, Pos: SizeInt;
begin
  if aValue.NonEmpty then
    begin
      Pos := 0;
      for I := 0 to Pred(Count) do
        if aValue.Contains(FItems[I]) then
          begin
            FItems[Pos] := FItems[I];
            Inc(Pos);
          end
        else
          Dec(FCount);
    end
  else
    MakeEmpty;
end;

function TGCustomGraph.TIntSet.Intersection(constref aValue: TIntSet): TIntSet;
begin
  Result.Assign(Self);
  Result.Intersect(aValue);
end;

function TGCustomGraph.TIntSet.IntersectionCount(constref aValue: TIntSet): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I in aValue do
    Result += Ord(Contains(I));
end;

function TGCustomGraph.TIntSet.IntersectionCount(PList: PAdjList): SizeInt;
var
  p: PAdjItem;
begin
  Result := 0;
  for p in PList^ do
    Result += Ord(Contains(p^.Destination));
end;

function TGCustomGraph.TIntSet.Remove(aValue: SizeInt): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(Count) do
    if FItems[I] = aValue then
      begin
        Dec(FCount);
        FItems[I] := FItems[Count];
        exit(True);
      end;
  Result := False;
end;

procedure TGCustomGraph.TIntSet.Delete(aValue: SizeInt);
var
  I: SizeInt;
begin
  for I := 0 to Pred(Count) do
    if FItems[I] = aValue then
      begin
        Dec(FCount);
        if I < Count then
          System.Move(FItems[Succ(I)], FItems[I], (Count - I) * SizeOf(SizeInt));
        exit;
      end;
end;

procedure TGCustomGraph.TIntSet.Reverse;
begin
  if Count <> 0 then
    TIntHelper.Reverse(FItems[0..Pred(Count)]);
end;

{ TGCustomGraph.TSkeleton }

function TGCustomGraph.TSkeleton.GetVertexCount: SizeInt;
begin
  Result := System.Length(FAdjLists);
end;

function TGCustomGraph.TSkeleton.GetAdjList(aIndex: SizeInt): PIntSet;
begin
  Result := @FAdjLists[aIndex];
end;

function TGCustomGraph.TSkeleton.GetDegree(aIndex: SizeInt): SizeInt;
begin
  Result := FAdjLists[aIndex].Count;
end;

constructor TGCustomGraph.TSkeleton.Create(aVertCount: SizeInt; aDirected: Boolean);
begin
  System.SetLength(FAdjLists, aVertCount);
  FEdgeCount := 0;
  FDirected := aDirected;
end;

constructor TGCustomGraph.TSkeleton.Create(constref s: TSkeleton);
var
  I: SizeInt;
begin
  System.SetLength(FAdjLists, s.VertexCount);
  FEdgeCount := s.EdgeCount;
  FDirected := s.Directed;
  for I := 0 to Pred(s.VertexCount) do
    FAdjLists[I].Assign(s.FAdjLists[I]);
end;

function TGCustomGraph.TSkeleton.ContainsEdge(aSrc, aDst: SizeInt): Boolean;
begin
  if (aSrc >= 0) and (aSrc < VertexCount) then
    Result := FAdjLists[aSrc].Contains(aDst)
  else
    Result := False;
end;

function TGCustomGraph.TSkeleton.AddEdge(aSrc, aDst: SizeInt): Boolean;
begin
  if (aSrc < 0) or (aSrc >= VertexCount) or (aDst < 0) or (aDst >= VertexCount) then
    exit(False);
  Result := FAdjLists[aSrc].Add(aDst);
  if Result then
    begin
      if not Directed then
        FAdjLists[aDst].Add(aSrc);
      Inc(FEdgeCount);
    end;
end;

function TGCustomGraph.TSkeleton.RemoveEdge(aSrc, aDst: SizeInt): Boolean;
begin
  if aSrc = aDst then
    exit(False);
  Result := FAdjLists[aSrc].Remove(aDst);
  if Result then
    begin
      if not Directed then
        FAdjLists[aDst].Remove(aSrc);
      Dec(FEdgeCount);
    end;
end;

