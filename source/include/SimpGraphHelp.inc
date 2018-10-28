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

{ TGSimpleGraph.TBPCliqueIsHelper }

procedure TGSimpleGraph.TBPCliqueIsHelper.Colorize(constref aCand: TBoolVector; var aColOrd, aColors: TIntArray);
var
  P, Q: TBoolVector;
  I, J, ColorClass, PCount: SizeInt;
begin
  P := aCand;
  ColorClass := 0;
  I := 0;
  PCount := P.PopCount;
  while PCount > 0 do
    begin
      Inc(ColorClass);
      Q := P;
      while Q.NonEmpty do
        begin
          J := Q.Bsf;
          P[J] := False;
          Q[J] := False;
          Q.Subtract(FMatrix[J]);
          aColOrd[I] := J;
          aColors[I] := ColorClass;
          Inc(I);
          Dec(PCount);
        end;
    end;
end;

procedure TGSimpleGraph.TBPCliqueIsHelper.Extend(var aCand: TBoolVector);
var
  NewCand: TBoolVector;
  ColOrd, Colors: TIntArray;
  I, J, ItemCount: SizeInt;
begin
  if aCand.NonEmpty then
    begin
      if SecondsBetween(Now, FStartTime) >= FTimeOut then
        begin
          FCanceled := True;
          exit;
        end;
      ItemCount := aCand.PopCount;
      ColOrd.Length := ItemCount;
      Colors.Length := ItemCount;
      Colorize(aCand, ColOrd, Colors);
      for I := Pred(ItemCount) downto 0 do
        begin
          if Colors[I] + FCurrSet.PopCount <= FRecentBest.Length then
            exit;
          J := ColOrd[I];
          aCand[J] := False;
          FCurrSet[FNodes[J]] := True;
          NewCand := aCand.Intersection(FMatrix[J]);
          Extend(NewCand);
          FCurrSet[FNodes[J]] := False;
        end;
    end
  else
    if FCurrSet.PopCount > FRecentBest.Length then
      FRecentBest := FCurrSet.ToArray;
end;

procedure TGSimpleGraph.TBPCliqueIsHelper.Extend(var aSub, aCand: TBoolVector);
var
  NewSub, NewCand: TBoolVector;
  I: SizeInt;
begin
  if FCanceled then
    exit;
  if aSub.NonEmpty then
    begin
      if aCand.NonEmpty then
        for I in aCand.Difference(FMatrix[aSub.Bsf]) do
          begin
            aCand[I] := False;
            NewCand := aCand.Intersection(FMatrix[I]);
            NewSub := aSub.Intersection(FMatrix[I]);
            FCurrSet[FNodes[I]] := True;
            Extend(NewSub, NewCand);
            FCurrSet[FNodes[I]] := False;
          end;
    end
  else
    FOnFind(FCurrSet.ToArray, FCanceled);
end;

procedure TGSimpleGraph.TBPCliqueIsHelper.FillMatrix(aGraph: TGSimpleGraph; aComplement: Boolean);
var
  I, J: SizeInt;
  p: PAdjList;
begin
  System.SetLength(FMatrix, aGraph.VertexCount);
  for I := 0 to Pred(aGraph.VertexCount) do
    begin
      FMatrix[I].Size := aGraph.VertexCount;
      p := aGraph.AdjLists[FNodes[I]];
      for J := 0 to Pred(aGraph.VertexCount) do
        if (I <> J) and (p^.Contains(FNodes[J]) xor aComplement) then
          FMatrix[I][J] := True;
    end;
end;

procedure TGSimpleGraph.TBPCliqueIsHelper.SortMatrixByWidth(aGraph: TGSimpleGraph; aComplement: Boolean);
begin
  if aComplement then
    FNodes := aGraph.SortComplementByWidth
  else
    FNodes := aGraph.SortNodesByWidth(soDesc);
  FillMatrix(aGraph, aComplement);
end;

procedure TGSimpleGraph.TBPCliqueIsHelper.SortMatrixByDegree(aGraph: TGSimpleGraph; aComplement: Boolean);
begin
  if aComplement then
    FNodes := aGraph.SortNodesByDegree(soAsc)
  else
    FNodes := aGraph.SortNodesByDegree(soDesc);
  FillMatrix(aGraph, aComplement);
end;

function TGSimpleGraph.TBPCliqueIsHelper.MaxClique(aGraph: TGSimpleGraph; aTimeOut: Integer;
  out aExact: Boolean): TIntArray;
var
  Cand: TBoolVector;
begin
  FStartTime := Now;
  FTimeOut := aTimeOut and System.High(Integer);
  FCanceled := False;
  SortMatrixByWidth(aGraph, False);
  FRecentBest := aGraph.GreedyMaxClique;
  Cand.InitRange(aGraph.VertexCount);
  FCurrSet.Size := aGraph.VertexCount;
  Extend(Cand);
  Result := FRecentBest;
  aExact := not FCanceled;
end;

function TGSimpleGraph.TBPCliqueIsHelper.MaxIS(aGraph: TGSimpleGraph; aTimeOut: Integer;
  out aExact: Boolean): TIntArray;
var
  Cand: TBoolVector;
begin
  FTimeOut := aTimeOut and System.High(Integer);
  FCanceled := False;
  FStartTime := Now;
  SortMatrixByWidth(aGraph, True);
  FRecentBest := aGraph.GreedyMIS;
  Cand.InitRange(aGraph.VertexCount);
  FCurrSet.Size := aGraph.VertexCount;
  Extend(Cand);
  Result := FRecentBest;
  aExact := not FCanceled;
end;

procedure TGSimpleGraph.TBPCliqueIsHelper.ListCliques(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
var
  Sub, Cand: TBoolVector;
begin
  SortMatrixByDegree(aGraph, False);
  Sub.InitRange(aGraph.VertexCount);
  Cand.InitRange(aGraph.VertexCount);
  FCurrSet.Size := aGraph.VertexCount;
  FOnFind := aOnFind;
  FCanceled := False;
  Extend(Sub, Cand);
end;

procedure TGSimpleGraph.TBPCliqueIsHelper.ListIS(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
var
  Sub, Cand: TBoolVector;
begin
  SortMatrixByDegree(aGraph, True);
  Sub.InitRange(aGraph.VertexCount);
  Cand.InitRange(aGraph.VertexCount);
  FCurrSet.Size := aGraph.VertexCount;
  FOnFind := aOnFind;
  FCanceled := False;
  Extend(Sub, Cand);
end;

{ TGSimpleGraph.TBPCliqueIsHelper256 }

procedure TGSimpleGraph.TBPCliqueIsHelper256.Colorize(constref aCand: TBits256; var aColOrd, aColors: TIntArray);
var
  P, Q: TBits256;
  I, J, ColorClass, PCount: SizeInt;
begin
  P := aCand;
  ColorClass := 0;
  I := 0;
  PCount := P.PopCount;
  while PCount > 0 do
    begin
      Inc(ColorClass);
      Q := P;
      while Q.NonEmpty do
        begin
          J := Q.Bsf;
          P[J] := False;
          Q[J] := False;
          Q.Subtract(FMatrix[J]);
          aColOrd[I] := J;
          aColors[I] := ColorClass;
          Inc(I);
          Dec(PCount);
        end;
    end;
end;

procedure TGSimpleGraph.TBPCliqueIsHelper256.Extend(var aCand: TBits256);
var
  NewCand: TBits256;
  ColOrd, Colors: TIntArray;
  I, J, ItemCount: SizeInt;
begin
  if aCand.NonEmpty then
    begin
      if SecondsBetween(Now, FStartTime) >= FTimeOut then
        begin
          FCanceled := True;
          exit;
        end;
      ItemCount := aCand.PopCount;
      ColOrd.Length := ItemCount;
      Colors.Length := ItemCount;
      Colorize(aCand, ColOrd, Colors);
      for I := Pred(ItemCount) downto 0 do
        begin
          if Colors[I] + FCurrSet.PopCount <= FRecentBest.Length then
            exit;
          J := ColOrd[I];
          aCand[J] := False;
          FCurrSet[FNodes[J]] := True;
          NewCand := aCand.Intersection(FMatrix[J]);
          Extend(NewCand);
          FCurrSet[FNodes[J]] := False;
        end;
    end
  else
    if FCurrSet.PopCount > FRecentBest.Length then
      FRecentBest := FCurrSet.ToArray;
end;

procedure TGSimpleGraph.TBPCliqueIsHelper256.Extend(var aSub, aCand: TBits256);
var
  NewSub, NewCand: TBits256;
  I: SizeInt;
begin
  if FCanceled then
    exit;
  if aSub.NonEmpty then
    begin
      if aCand.NonEmpty then
        for I in aCand.Difference(FMatrix[aSub.Bsf]) do
          begin
            aCand[I] := False;
            NewCand := aCand.Intersection(FMatrix[I]);
            NewSub := aSub.Intersection(FMatrix[I]);
            FCurrSet[FNodes[I]] := True;
            Extend(NewSub, NewCand);
            FCurrSet[FNodes[I]] := False;
          end;
    end
  else
    FOnFind(FCurrSet.ToArray, FCanceled);
end;

procedure TGSimpleGraph.TBPCliqueIsHelper256.FillMatrix(aGraph: TGSimpleGraph; aComplement: Boolean);
var
  I, J: SizeInt;
  pA: PAdjList;
begin
  System.SetLength(FMatrix, aGraph.VertexCount);
  for I := 0 to Pred(aGraph.VertexCount) do
    begin
      pA := aGraph.AdjLists[FNodes[I]];
      FMatrix[I].InitZero;
      for J := 0 to Pred(aGraph.VertexCount) do
        if (I <> J) and (pA^.Contains(FNodes[J]) xor aComplement) then
          FMatrix[I][J] := True;
    end;
end;

procedure TGSimpleGraph.TBPCliqueIsHelper256.SortMatrixByWidth(aGraph: TGSimpleGraph; aComplement: Boolean);
begin
  if aComplement then
    FNodes := aGraph.SortComplementByWidth
  else
    FNodes := aGraph.SortNodesByWidth(soDesc);
  FillMatrix(aGraph, aComplement);
end;

procedure TGSimpleGraph.TBPCliqueIsHelper256.SortMatrixByDegree(aGraph: TGSimpleGraph; aComplement: Boolean);
begin
  if aComplement then
    FNodes := aGraph.SortNodesByDegree(soAsc)
  else
    FNodes := aGraph.SortNodesByDegree(soDesc);
  FillMatrix(aGraph, aComplement);
end;

function TGSimpleGraph.TBPCliqueIsHelper256.MaxClique(aGraph: TGSimpleGraph; aTimeOut: Integer;
  out aExact: Boolean): TIntArray;
var
  Cand: TBits256;
begin
  FTimeOut := aTimeOut and System.High(Integer);
  FCanceled := False;
  FStartTime := Now;
  SortMatrixByWidth(aGraph, False);
  FRecentBest := aGraph.GreedyMaxClique;
  Cand.InitRange(aGraph.VertexCount);
  FCurrSet.InitZero;
  Extend(Cand);
  Result := FRecentBest;
  aExact := not FCanceled;
end;

function TGSimpleGraph.TBPCliqueIsHelper256.MaxIS(aGraph: TGSimpleGraph; aTimeOut: Integer;
  out aExact: Boolean): TIntArray;
var
  Cand: TBits256;
begin
  FStartTime := Now;
  FTimeOut := aTimeOut and System.High(Integer);
  FCanceled := False;
  SortMatrixByWidth(aGraph, True);
  FRecentBest := aGraph.GreedyMIS;
  Cand.InitRange(aGraph.VertexCount);
  FCurrSet.InitZero;
  Extend(Cand);
  Result := FRecentBest;
  aExact := not FCanceled;
end;

procedure TGSimpleGraph.TBPCliqueIsHelper256.ListCliques(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
var
  Sub, Cand: TBits256;
begin
  SortMatrixByDegree(aGraph, False);
  Sub.InitRange(aGraph.VertexCount);
  Cand.InitRange(aGraph.VertexCount);
  FCurrSet.InitZero;
  FOnFind := aOnFind;
  FCanceled := False;
  Extend(Sub, Cand);
end;

procedure TGSimpleGraph.TBPCliqueIsHelper256.ListIS(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
var
  Sub, Cand: TBits256;
begin
  SortMatrixByDegree(aGraph, True);
  Sub.InitRange(aGraph.VertexCount);
  Cand.InitRange(aGraph.VertexCount);
  FCurrSet.InitZero;
  FOnFind := aOnFind;
  FCanceled := False;
  Extend(Sub, Cand);
end;

procedure TGSimpleGraph.TCliqueHelper.Colorize(constref aCand: TIntSet; var aColOrd, aColors: TIntArray);
var
  P, Q: TIntSet;
  I, J, ColorClass: SizeInt;
begin
  P.Assign(aCand);
  P.Reverse;
  ColorClass := 0;
  I := 0;
  while P.NonEmpty do
    begin
      Inc(ColorClass);
      Q.Assign(P);
      while Q.NonEmpty do
        begin
          J := Q.Pop;
          P.Delete(J);
          Q.Subtract(FMatrix[J]^);
          aColOrd[I] := J;
          aColors[I] := ColorClass;
          Inc(I);
        end;
    end;
end;

procedure TGSimpleGraph.TCliqueHelper.Extend(var aCand: TIntSet);
var
  NewCand: TIntSet;
  ColOrd, Colors: TIntArray;
  I, J: SizeInt;
begin
  if aCand.NonEmpty then
    begin
      if SecondsBetween(Now, FStartTime) >= FTimeOut then
        begin
          FCanceled := True;
          exit;
        end;
      ColOrd.Length := aCand.Count;
      Colors.Length := aCand.Count;
      Colorize(aCand, ColOrd, Colors);
      for I := Pred(aCand.Count) downto 0 do
        begin
          if Colors[I] + FCurrSet.Count <= FRecentBest.Length then
            exit;
          J := ColOrd[I];
          aCand.Delete(J);
          FCurrSet.Push(FNodes[J]);
          NewCand := aCand.Intersection(FMatrix[J]^);
          Extend(NewCand);
          FCurrSet.Pop;
        end;
    end
  else
    if FCurrSet.Count > FRecentBest.Length then
      FRecentBest := FCurrSet.ToArray;
end;

procedure TGSimpleGraph.TCliqueHelper.Extend(var aSub, aCand: TIntSet);
var
  NewSub, NewCand: TIntSet;
  I: SizeInt;
begin
  if FCanceled then
    exit;
  if aSub.NonEmpty then
    begin
      if aCand.NonEmpty then
        for I in aCand.Difference(FMatrix[aSub[0]]^) do
          begin
            aCand.Delete(I);
            NewCand := aCand.Intersection(FMatrix[I]^);
            NewSub := aSub.Intersection(FMatrix[I]^);
            FCurrSet.Push(FNodes[I]);
            Extend(NewSub, NewCand);
            FCurrSet.Pop;
          end;
    end
  else
    FOnFind(FCurrSet.ToArray, FCanceled);
end;

function TGSimpleGraph.TCliqueHelper.MaxClique(aGraph: TGSimpleGraph; aTimeOut: Integer;
  out aExact: Boolean): TIntArray;
var
  Cand: TIntSet;
begin
  FTimeOut := aTimeOut and System.High(Integer);
  FCanceled := False;
  FStartTime := Now;
  FNodes := aGraph.SortNodesByWidth(soDesc);
  FMatrix := aGraph.CreateSkeleton;
  FRecentBest := aGraph.GreedyMaxClique;
  Cand.InitRange(aGraph.VertexCount);
  Extend(Cand);
  Result := FRecentBest;
  aExact := not FCanceled;
end;

procedure TGSimpleGraph.TCliqueHelper.ListCliques(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
var
  Sub, Cand: TIntSet;
begin
  FNodes := aGraph.SortNodesByDegree(soDesc);
  FMatrix := aGraph.CreateSkeleton;
  Sub.InitRange(aGraph.VertexCount);
  Cand.InitRange(aGraph.VertexCount);
  FOnFind := aOnFind;
  FCanceled := False;
  Extend(Sub, Cand);
end;

{ TGSimpleGraph.TBPDomSetHelper }

procedure TGSimpleGraph.TBPDomSetHelper.FillMatrix(aGraph: TGSimpleGraph);
var
  I, J: SizeInt;
  pA: PAdjList;
begin
  FNodes := aGraph.SortNodesByDegree(soDesc);
  System.SetLength(FMatrix, System.Length(FNodes));
  for I := 0 to System.High(FNodes) do
    begin
      FMatrix[I].Size := System.Length(FNodes);
      pA := aGraph.AdjLists[FNodes[I]];
      for J := 0 to System.High(FNodes) do
        if (I <> J) and pA^.Contains(FNodes[J]) then
          FMatrix[I][J] := True;
    end;
end;

procedure TGSimpleGraph.TBPDomSetHelper.Extend(constref aSub, aCand: TBoolVector);
var
  NewSub, NewCand: TBoolVector;
  I, J: SizeInt;
begin
  if aSub.NonEmpty then
    begin
      if aCand.PopCount >= System.High(FRecentBest) then
        exit;
      if SecondsBetween(Now, FStartTime) >= FTimeOut then
        begin
          FCanceled := True;
          exit;
        end;
      I := aSub.Bsf;
      NewCand := aCand;
      NewCand[FNodes[I]] := True;
      NewSub := aSub.Difference(FMatrix[I]);
      NewSub[I] := False;
      Extend(NewSub, NewCand);
      if aCand.PopCount >= System.High(FRecentBest) then
        exit;
      NewCand[FNodes[I]] := False;
      for J in aSub.Intersection(FMatrix[I]) do
        begin
          NewCand[FNodes[J]] := True;
          NewSub := aSub.Difference(FMatrix[J]);
          NewSub[J] := False;
          Extend(NewSub, NewCand);
          if NewCand.PopCount >= System.High(FRecentBest) then
            exit;
          NewCand[FNodes[I]] := True;
          NewSub.Subtract(FMatrix[I]);
          Extend(NewSub, NewCand);
          if NewCand.PopCount >= System.Length(FRecentBest) then
            exit;
          NewCand[FNodes[I]] := False;
          NewCand[FNodes[J]] := False;
        end;
    end
  else
    if aCand.PopCount < System.Length(FRecentBest) then
      FRecentBest := aCand.ToArray;
end;

function TGSimpleGraph.TBPDomSetHelper.MinDomSet(aGraph: TGSimpleGraph; aTimeOut: Integer;
  out aExact: Boolean): TIntArray;
var
  Sub, Cand: TBoolVector;
begin
  FTimeOut := aTimeOut and System.High(Integer);
  FCanceled := False;
  FStartTime := Now;
  FillMatrix(aGraph);
  FRecentBest := aGraph.GreedyMDS;
  Cand.Size := aGraph.VertexCount;
  Sub.InitRange(aGraph.VertexCount);
  Extend(Sub, Cand);
  aExact := not FCanceled;
  Result := FRecentBest;
end;

{ TGSimpleGraph.TBPDomSetHelper256 }

procedure TGSimpleGraph.TBPDomSetHelper256.FillMatrix(aGraph: TGSimpleGraph);
var
  I, J: SizeInt;
  pA: PAdjList;
begin
  FNodes := aGraph.SortNodesByDegree(soDesc);
  System.SetLength(FMatrix, System.Length(FNodes));
  for I := 0 to Pred(System.High(FNodes)) do
    begin
      FMatrix[I].InitZero;
      pA := aGraph.AdjLists[FNodes[I]];
      for J := 0 to System.High(FNodes) do
        if (I <> J) and pA^.Contains(FNodes[J]) then
          FMatrix[I][J] := True;
    end;
end;

procedure TGSimpleGraph.TBPDomSetHelper256.Extend(constref aSub, aCand: TBits256);
var
  NewSub, NewCand: TBits256;
  I, J: SizeInt;
begin
  if aSub.NonEmpty then
    begin
      if aCand.PopCount >= System.High(FRecentBest) then
        exit;
      if SecondsBetween(Now, FStartTime) >= FTimeOut then
        begin
          FCanceled := True;
          exit;
        end;
      I := aSub.Bsf;
      NewCand := aCand;
      NewCand[FNodes[I]] := True;
      NewSub := aSub.Difference(FMatrix[I]);
      NewSub[I] := False;
      Extend(NewSub, NewCand);
      if aCand.PopCount >= System.High(FRecentBest) then
        exit;
      NewCand[FNodes[I]] := False;
      for J in aSub.Intersection(FMatrix[I]) do
        begin
          NewCand[FNodes[J]] := True;
          NewSub := aSub.Difference(FMatrix[J]);
          NewSub[J] := False;
          Extend(NewSub, NewCand);
          if NewCand.PopCount >= System.High(FRecentBest) then
            exit;
          NewCand[FNodes[I]] := True;
          NewSub.Subtract(FMatrix[I]);
          Extend(NewSub, NewCand);
          if NewCand.PopCount >= System.Length(FRecentBest) then
            exit;
          NewCand[FNodes[I]] := False;
          NewCand[FNodes[J]] := False;
        end;
    end
  else
    if aCand.PopCount < System.Length(FRecentBest) then
      FRecentBest := aCand.ToArray;
end;

function TGSimpleGraph.TBPDomSetHelper256.MinDomSet(aGraph: TGSimpleGraph; aTimeOut: Integer;
  out aExact: Boolean): TIntArray;
var
  Sub, Cand: TBits256;
begin
  FTimeOut := aTimeOut and System.High(Integer);
  FCanceled := False;
  FStartTime := Now;
  FillMatrix(aGraph);
  FRecentBest := aGraph.GreedyMDS;
  {%H-}Cand.InitZero;
  Sub.InitRange(aGraph.VertexCount);
  Extend(Sub, Cand);
  aExact := not FCanceled;
  Result := FRecentBest;
end;

{ TGSimpleGraph.TDomSetHelper }

procedure TGSimpleGraph.TDomSetHelper.Extend(constref aSub, aCand: TIntSet);
var
  NewSub, NewCand: TIntSet;
  I, J: SizeInt;
begin
  if aSub.NonEmpty then
    begin
      if aCand.Count >= System.High(FRecentBest) then
        exit;
      if SecondsBetween(Now, FStartTime) >= FTimeOut then
        begin
          FCancel := True;
          exit;
        end;
      I := aSub[0];
      NewCand.Assign(aCand);
      NewCand.Push(I);
      NewSub := aSub.Difference(FMatrix[I]^);
      NewSub.Delete(I);
      Extend(NewSub, NewCand);
      if aCand.Count >= System.High(FRecentBest) then
        exit;
      NewCand.Pop;
      for J in aSub.Intersection(FMatrix[I]^) do
        begin
          NewCand.Push(J);
          NewSub := aSub.Difference(FMatrix[J]^);
          NewSub.Delete(J);
          Extend(NewSub, NewCand);
          if NewCand.Count >= System.High(FRecentBest) then
            exit;
          NewCand.Push(I);
          NewSub.Subtract(FMatrix[I]^);
          Extend(NewSub, NewCand);
          if NewCand.Count >= System.Length(FRecentBest) then
            exit;
          NewCand.Pop;
          NewCand.Pop;
        end;
    end
  else
    if aCand.Count < System.Length(FRecentBest) then
      FRecentBest := aCand.ToArray;
end;

function TGSimpleGraph.TDomSetHelper.MinDomSet(aGraph: TGSimpleGraph; aTimeOut: Integer;
  out aExact: Boolean): TIntArray;
var
  Sub, Cand: TIntSet;
begin
  FTimeOut := aTimeOut and System.High(Integer);
  FCancel := False;
  FStartTime := Now;
  FRecentBest := aGraph.GreedyMDS;
  FMatrix := aGraph.CreateSkeleton;
  Sub.AssignArray(aGraph.SortNodesByDegree(soDesc));
  Extend(Sub, Cand{%H-});
  aExact := not FCancel;
  Result := FRecentBest;
end;

{ TGSimpleGraph.TExactColor.TNode }

constructor TGSimpleGraph.TExactColor.TNode.Create(aDegree, aUpperBound: SizeInt);
begin
  Degree := aDegree;
  Saturation := 0;
  System.SetLength(AdjColors, aUpperBound);
  System.FillChar(Pointer(AdjColors)^, aUpperBound * SizeOf(Integer), 0);
end;

procedure TGSimpleGraph.TExactColor.TNode.NeibPickColor(aColor: SizeInt);
begin
  Saturation += Ord(AdjColors[Pred(aColor)] = 0);
  Inc(AdjColors[Pred(aColor)]);
  Dec(Degree);
end;

procedure TGSimpleGraph.TExactColor.TNode.NeibDropColor(aColor: SizeInt);
begin
  Dec(AdjColors[Pred(aColor)]);
  Saturation -= Ord(AdjColors[Pred(aColor)] = 0);
  Inc(Degree);
end;

function TGSimpleGraph.TExactColor.TNode.Available(aColor: Integer): Boolean;
begin
  Result := AdjColors[Pred(aColor)] = 0;
end;

{ TGSimpleGraph.TExactColor }

function TGSimpleGraph.TExactColor.InitLowerBound: SizeInt;
var
  Clique: TIntArray;
  I, J: SizeInt;
  Exact: Boolean;
begin
  Clique := FGraph.MaxClique(Exact, FTimeOut - SecondsBetween(Now, FStartTime));
  Result := Clique.Length;
  FCancel := not Exact;
  if FCancel or (Result >= FUpperBound) then
    exit;
  System.SetLength(FNodes, FNodeCount);
  for I := 0 to System.High(FNodes) do
    FNodes[I] := TNode.Create(FGraph.AdjLists[I]^.Count, Pred(FUpperBound));
  FColorMap := FGraph.CreateIntArray(0);
  FAchromatic.InitRange(FNodeCount);
  J := 1;
  for I in Clique do
    begin
      FAchromatic[I] := False;
      PickColor(I, J);
      Inc(J);
    end;
end;

procedure TGSimpleGraph.TExactColor.InitComplete;
var
  I: SizeInt;
begin
  System.SetLength(FNodes, FNodeCount);
  for I := 0 to System.High(FNodes) do
    FNodes[I] := TNode.Create(FGraph.AdjLists[I]^.Count, Pred(FUpperBound));
  FAchromatic.InitRange(FNodeCount);
  for I := 0 to System.High(FColorMap) do
    if FColorMap[I] > 0 then
      begin
        FAchromatic[I] := False;
        PickColor(I, FColorMap[I]);
      end;
end;

procedure TGSimpleGraph.TExactColor.PickColor(aIndex, aColor: SizeInt);
var
  p: PAdjItem;
begin
  FColorMap[aIndex] := aColor;
  for p in FGraph.AdjLists[aIndex]^ do
    FNodes[p^.Key].NeibPickColor(aColor);
end;

procedure TGSimpleGraph.TExactColor.DropColor(aIndex, aColor: SizeInt);
var
  p: PAdjItem;
begin
  FColorMap[aIndex] := 0;
  for p in FGraph.AdjLists[aIndex]^ do
    FNodes[p^.Key].NeibDropColor(aColor);
end;

function TGSimpleGraph.TExactColor.SelectNext(out aNode: SizeInt): Boolean;
var
  I, MaxSat: Integer;
begin
  //todo: priority queue ?
  MaxSat := NULL_INDEX;
  aNode := NULL_INDEX;
  for I in FAchromatic do
    with FNodes[I] do
      if (Saturation > MaxSat) or (Saturation = MaxSat) and (Degree > FNodes[aNode].Degree) then
        begin
          MaxSat := Saturation;
          aNode := I;
        end;
  Result := aNode >= 0;
end;

procedure TGSimpleGraph.TExactColor.CheckSolution(aMaxColor: SizeInt);
begin
  if aMaxColor < FUpperBound then
    begin
      FUpperBound := aMaxColor;
      FRecentBest := System.Copy(FColorMap);
    end;
end;

function TGSimpleGraph.TExactColor.DSatur(aMaxColor: SizeInt): SizeInt;
var
  CurrNode, Color: SizeInt;
begin
  if SecondsBetween(Now, FStartTime) >= FTimeOut then //timeout
    begin
      FCancel := True;
      exit(FUpperBound);
    end;
  if aMaxColor >= FUpperBound then
    exit(aMaxColor);
  if FUpperBound <= FLowerBound then
    exit(FUpperBound);
  if not SelectNext(CurrNode) then
    exit(aMaxColor);
  FAchromatic[CurrNode] := False;
  for Color := 1 to aMaxColor do
    if FNodes[CurrNode].Available(Color) then
      begin
        PickColor(CurrNode, Color);
        CheckSolution(DSatur(aMaxColor));
        DropColor(CurrNode, Color);
        if aMaxColor >= FUpperBound  then
          break;
      end;
  if aMaxColor < Pred(FUpperBound) then
    begin
      PickColor(CurrNode, Succ(aMaxColor));
      CheckSolution(DSatur(Succ(aMaxColor)));
      DropColor(CurrNode, Succ(aMaxColor));
    end;
  FAchromatic[CurrNode] := True;
  Result := FUpperBound;
end;

function TGSimpleGraph.TExactColor.Colorize(aGraph: TGSimpleGraph; aTimeOut: Integer; out aColors: TIntArray;
  out aExact: Boolean): SizeInt;
begin
  FTimeOut := aTimeOut and System.High(Integer);
  FNodeCount := aGraph.VertexCount;
  FGraph := aGraph;
  FCancel := False;
  FStartTime := Now;
  FUpperBound := FGraph.GreedyColorRlf(FRecentBest);
  FLowerBound := InitLowerBound;
  if not FCancel and (FLowerBound < FUpperBound) then
    CheckSolution(DSatur(FLowerBound));
  aExact := not FCancel;
  Result := FUpperBound;
  aColors := FRecentBest;
end;

function TGSimpleGraph.TExactColor.IsColorable(aGraph: TGSimpleGraph; aK: SizeInt; aTimeOut: Integer;
  out aColors: TIntArray): TTriLean;
begin
  FTimeOut := aTimeOut and System.High(Integer);
  FNodeCount := aGraph.VertexCount;
  FGraph := aGraph;
  FLowerBound := aK;
  FUpperBound := Succ(aK);
  FCancel := False;
  FStartTime := Now;
  InitLowerBound;
  if not FCancel then
    CheckSolution(DSatur(FLowerBound));
  if FLowerBound >= FUpperBound then
    begin
      aColors := FRecentBest;
      Result := tlTrue;
    end
  else
    if not FCancel then
      Result := tlFalse
    else
      Result := tlUnknown;
end;

function TGSimpleGraph.TExactColor.Complete(aGraph: TGSimpleGraph; aK: SizeInt; aTimeOut: Integer;
  var aColors: TIntArray): Boolean;
begin
  FTimeOut := aTimeOut and System.High(Integer);
  FNodeCount := aGraph.VertexCount;
  FGraph := aGraph;
  FLowerBound := aK;
  FUpperBound := Succ(aK);
  FCancel := False;
  FStartTime := Now;
  FColorMap := System.Copy(aColors);
  InitComplete;
  CheckSolution(DSatur(FLowerBound));
  Result := not FCancel and (FLowerBound >= FUpperBound);
  if Result then
    aColors := FRecentBest;
end;

{ TGSimpleGraph.TGreedyColorRlf.TNode }

class operator TGSimpleGraph.TGreedyColorRlf.TNode.>(constref L, R: TNode): Boolean;
begin
  if L.WDegree <> R.WDegree then
    Result := L.WDegree > R.WDegree
  else
    if L.Degree <> R.Degree then
      Result := L.Degree > R.Degree
    else
      Result := L.Index > R.Index;
end;

class operator TGSimpleGraph.TGreedyColorRlf.TNode.<(constref L, R: TNode): Boolean;
begin
  if L.WDegree <> R.WDegree then
    Result := L.WDegree < R.WDegree
  else
    if L.Degree <> R.Degree then
      Result := L.Degree < R.Degree
    else
      Result := L.Index < R.Index;
end;

class operator TGSimpleGraph.TGreedyColorRlf.TNode.>=(constref L, R: TNode): Boolean;
begin
  Result := not (L < R);
end;

class operator TGSimpleGraph.TGreedyColorRlf.TNode.<=(constref L, R: TNode): Boolean;
begin
  Result := not (L > R);
end;

constructor TGSimpleGraph.TGreedyColorRlf.TNode.Create(aIndex, aDegree: SizeInt);
begin
  Index := aIndex;
  WDegree := 0;
  Degree := aDegree;
end;

{ TGSimpleGraph.TGreedyColorRlf }

function TGSimpleGraph.TGreedyColorRlf.Colorize(aGraph: TGSimpleGraph; out aColors: TIntArray): SizeInt;
var
  Nodes: array of TNode;
  Queue: TQueue;
  Achromatic, CurrIS, InQueue: TBoolVector;
  Node, Neib: TNode;
  I: SizeInt;
  p, p2: PAdjItem;
begin
  System.SetLength(Nodes, aGraph.VertexCount);
  for I := 0 to System.High(Nodes) do
    Nodes[I] := TNode.Create(I, aGraph.AdjLists[I]^.Count);
  aColors.Length := aGraph.VertexCount;
  Achromatic.InitRange(aGraph.VertexCount);
  InQueue.Size := aGraph.VertexCount;
  Queue := TQueue.Create(aGraph.VertexCount);
  Result := 0;
  while Achromatic.NonEmpty do
    begin
      Inc(Result);
      CurrIS := Achromatic;
      InQueue := Achromatic;
      for I in Achromatic do
        Queue.Enqueue(I, Nodes[I]);
      while Queue.TryDequeue(Node) do
        begin
          InQueue[Node.Index] := False;
          if CurrIS[Node.Index] then
            begin
              CurrIS[Node.Index] := False;
              Achromatic[Node.Index] := False;
              aColors[Node.Index] := Result;
              for p in aGraph.AdjLists[Node.Index]^ do
                if Achromatic[p^.Key] then
                  begin
                    Dec(Nodes[p^.Key].Degree);
                    CurrIS[p^.Key] := False;
                    for p2 in aGraph.AdjLists[p^.Key]^ do
                      if InQueue[p2^.Key] then
                        begin
                          Neib := Queue.Peek(p2^.Key);
                          Inc(Neib.WDegree);
                          Queue.Update(p2^.Key, Neib);
                        end;
                  end;
            end;
        end;
    end;
end;

{ TGSimpleGraph.THKMatch }

procedure TGSimpleGraph.THKMatch.Init(aGraph: TGSimpleGraph; constref w, g: TIntArray);
var
  CurrArcIdx: TIntArray = nil;
  Grays: TIntHashSet;
  I, J: SizeInt;
  p: PAdjItem;
begin
  FNodeCount := Succ(aGraph.VertexCount);
  FDummy := Pred(FNodeCount);
  if System.Length(w) <= System.Length(g) then
    begin
      FWhites := w;
      Grays.AddAll(g);
    end
  else
    begin
      FWhites := g;
      Grays.AddAll(w);
    end;

  CurrArcIdx.Length := Succ(FNodeCount);
  J := 0;
  for I := 0 to FNodeCount - 2 do
    begin
      CurrArcIdx[I] := J;
      if Grays.Contains(I) then
        J += Succ(aGraph.DegreeI(I))
      else
        J += aGraph.DegreeI(I);
    end;
  CurrArcIdx[Pred(FNodeCount)] := J;

  System.SetLength(FNodes, Succ(FNodeCount));
  System.SetLength(FArcs, Succ((aGraph.EdgeCount + Grays.Count) * 2));

  for I := 0 to Pred(FNodeCount) do
    begin
      FNodes[I].FirstArc := CurrArcIdx[I];
      FNodes[I].Mate := FDummy;
    end;

  for I in FWhites do
    for p in aGraph.AdjLists[I]^ do
      begin
        J := p^.Destination;
        FArcs[CurrArcIdx[I]].Target := J;
        FArcs[CurrArcIdx[J]].Target := I;
        Inc(CurrArcIdx[I]);
        Inc(CurrArcIdx[J]);
      end;

  J := FDummy;
  for I in Grays do
    begin
      FArcs[CurrArcIdx[I]].Target := J;
      FArcs[CurrArcIdx[J]].Target := I;
      Inc(CurrArcIdx[I]);
      Inc(CurrArcIdx[J]);
    end;

  CurrArcIdx := nil;

  FArcs[System.High(FArcs)].Target := FNodeCount;
  //sentinel node
  FNodes[FNodeCount].FirstArc := System.High(FArcs);
  FNodes[FNodeCount].Distance := INF_DIST;
  FNodes[FNodeCount].Mate := FDummy;

  FQueue := aGraph.CreateIntArray;
end;

function TGSimpleGraph.THKMatch.Bfs: Boolean;
var
  Curr, CurrArc, Matched, Dist: SizeInt;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  for Curr in FWhites do
    if FNodes[Curr].Mate = FDummy then
      begin
        FNodes[Curr].Distance := 0;
        FQueue[qTail] := Curr;
        Inc(qTail);
      end
    else
      FNodes[Curr].Distance := INF_DIST;

  FNodes[FDummy].Distance := INF_DIST;

  while qHead < qTail do
    begin
      Curr := FQueue[qHead];
      Inc(qHead);
      if FNodes[{%H-}Curr].Distance < FNodes[FDummy].Distance then
        begin
          CurrArc := FNodes[Curr].FirstArc;
          Dist := Succ(FNodes[Curr].Distance);
          while CurrArc < FNodes[Succ(Curr)].FirstArc do
            begin
              Matched := FNodes[FArcs[CurrArc].Target].Mate;
              if FNodes[Matched].Distance = INF_DIST then
                begin
                  FNodes[Matched].Distance := Dist;
                  FQueue[qTail] := Matched;
                  Inc(qTail);
                end;
              Inc(CurrArc);
            end;
        end;
    end;
  Result := FNodes[FDummy].Distance <> INF_DIST;
end;

function TGSimpleGraph.THKMatch.Dfs(aRoot: SizeInt): Boolean;
var
  CurrArc, Dist, Next, Mate: SizeInt;
begin
  //todo: non-recursive dfs ???
  if aRoot = FDummy then
    exit(True);
  CurrArc := FNodes[aRoot].FirstArc;
  Dist := Succ(FNodes[aRoot].Distance);
  while CurrArc < FNodes[Succ(aRoot)].FirstArc do
    begin
      Next := FArcs[CurrArc].Target;
      Mate := FNodes[Next].Mate;
      if (FNodes[Mate].Distance = Dist) and Dfs(Mate) then
        begin
          FNodes[aRoot].Mate := Next;
          FNodes[Next].Mate := aRoot;
          exit(True);
        end;
      Inc(CurrArc);
    end;
  FNodes[aRoot].Distance := INF_DIST;
  Result := False;
end;

function TGSimpleGraph.THKMatch.HopcroftKarp: TIntEdgeArray;
var
  I, J, Size: SizeInt;
begin
  Size := 0;
  while Bfs do
    for I in FWhites do
      if FNodes[I].Mate = FDummy then
        Size += Ord(Dfs(I));
  System.SetLength(Result, Size);
  J := 0;
  for I in FWhites do
    if FNodes[I].Mate <> FDummy then
      begin
        Result[J] := TIntEdge.Create(I, FNodes[I].Mate);
        Inc(J);
      end;
end;

function TGSimpleGraph.THKMatch.GetMaxMatch(aGraph: TGSimpleGraph; constref w, g: TIntArray): TIntEdgeArray;
begin
  Init(aGraph, w, g);
  Result := HopcroftKarp;
end;

{ TGSimpleGraph.TBfsMatch }

procedure TGSimpleGraph.TBfsMatch.Match(aNode, aMate: SizeInt);
begin
  FMates[aNode] := aMate;
  FMates[aMate] := aNode;
end;

procedure TGSimpleGraph.TBfsMatch.Init(aGraph: TGSimpleGraph; constref w, g: TIntArray);
var
  I: SizeInt;
  e: TIntEdge;
begin
  FMatchCount := 0;
  FGraph := aGraph;
  FWhites.Size := aGraph.VertexCount;
  FVisited.Size := aGraph.VertexCount;
  if System.Length(w) <= System.Length(g) then
    for I in w do
      FWhites[I] := True
  else
    for I in g do
      FWhites[I] := True;
  FMates := aGraph.CreateIntArray;
  FParents := aGraph.CreateIntArray;
  FQueue := aGraph.CreateIntArray;
  for e in aGraph.GetApproxMatching do
    begin
      Match(e.Source, e.Destination);
      Inc(FMatchCount);
    end;
end;

function TGSimpleGraph.TBfsMatch.FindAugmentPath(aRoot: SizeInt): SizeInt;
var
  Curr, Next: SizeInt;
  p: PAdjItem;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  FVisited.ClearBits;
  FParents[aRoot] := NULL_INDEX;
  FQueue[qTail] := aRoot;
  Inc(qTail);
  while qHead < qTail do
    begin
      Curr := FQueue[qHead];
      Inc(qHead);
      if FWhites[Curr] then
        begin
          for p in FGraph.AdjLists[Curr]^ do
            begin
              Next := p^.Destination;
              if (FMates[Curr] = Next) or FVisited[Next] then
                continue;
              if FMates[Next] = NULL_INDEX then
                begin
                  FParents[Next] := Curr;
                  exit(Next);
                end
              else
                begin
                  FParents[Next] := Curr;
                  FQueue[qTail] := Next;
                  Inc(qTail);
                end;
            end;
        end
      else
        begin
          Next := FMates[Curr];
          FParents[Next] := Curr;
          FQueue[qTail] := Next;
          Inc(qTail);
        end;
    end;
  Result := NULL_INDEX;
end;

procedure TGSimpleGraph.TBfsMatch.AlternatePath(aRoot: SizeInt);
var
  Mate, tmp: SizeInt;
begin
  repeat
    Mate := FParents[aRoot];
    tmp := FMates[Mate];
    Match(aRoot, Mate);
    aRoot := tmp;
  until aRoot = NULL_INDEX;
end;

procedure TGSimpleGraph.TBfsMatch.BfsMatch;
var
  I, Last: SizeInt;
begin
  for I in FWhites do
    if FMates[I] = NULL_INDEX then
      begin
        Last := FindAugmentPath(I);
        if Last <> NULL_INDEX then
          begin
            AlternatePath(Last);
            Inc(FMatchCount);
          end;
      end;
end;

function TGSimpleGraph.TBfsMatch.CreateEdges: TIntEdgeArray;
var
  I, J: SizeInt;
begin
  System.SetLength(Result, FMatchCount);
  J := 0;
  for I in FWhites do
    if FMates[I] <> NULL_INDEX then
      begin
        Result[J] := TIntEdge.Create(I, FMates[I]);
        Inc(J);
      end;
end;

function TGSimpleGraph.TBfsMatch.GetMaxMatch(aGraph: TGSimpleGraph; constref w, g: TIntArray): TIntEdgeArray;
begin
  Init(aGraph, w, g);
  BfsMatch;
  Result := CreateEdges;
end;

{ TGSimpleGraph.TEdMatch }

procedure TGSimpleGraph.TEdMatch.Match(aIndex, aMate: SizeInt);
begin
  FMates[aIndex] := aMate;
  FMates[aMate] := aIndex;
end;

procedure TGSimpleGraph.TEdMatch.ClearBase;
var
  I: SizeInt;
begin
  for I := 0 to System.High(FBase) do
   FBase[I] := I;
end;

procedure TGSimpleGraph.TEdMatch.ClearParents;
begin
  System.FillChar(Pointer(FParents)^, System.Length(FParents) * SizeOf(SizeInt), $ff);
end;

function TGSimpleGraph.TEdMatch.Lca(L, R: SizeInt): SizeInt;
begin
  FLcaUsed.ClearBits;
  repeat
    L := FBase[L];
    FLcaUsed[L] := True;
    if FMates[L] = NULL_INDEX then
        break;
    L := FParents[FMates[L]];
  until False;
  repeat
    R := FBase[R];
    if FLcaUsed[R] then
      exit(R);
     R := FParents[FMates[R]];
  until False;
  Result := NULL_INDEX;
end;

procedure TGSimpleGraph.TEdMatch.MarkPath(aValue, aBloss, aChild: SizeInt);
begin
  while FBase[aBloss] <> aBloss do
    begin
      FBlossoms[FBase[aValue]] := True;
      FBlossoms[FBase[FMates[aValue]]] := True;
      FParents[aValue] := aChild;
      aChild := FMates[aValue];
      aValue := FParents[FMates[aValue]];
    end;
end;

function TGSimpleGraph.TEdMatch.FindAugmentPath(aRoot: SizeInt; out aLast: SizeInt): Boolean;
var
  I, Curr, Next, CurrBase: SizeInt;
  p: TGSimpleGraph.PAdjItem;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  FVisited.ClearBits;
  ClearParents;
  ClearBase;
  FVisited[aRoot] := True;
  FQueue[qTail] := aRoot;
  Inc(qTail);
  while qHead < qTail do
    begin
      Curr := FQueue[qHead];
      Inc(qHead);
      for p in FGraph.AdjLists[Curr]^ do
        begin
          Next := p^.Destination;
          if (FBase[Curr] = FBase[Next]) or (FMates[Curr] = Next) then
            continue;
          if (Next = aRoot) or (FMates[Next] <> NULL_INDEX) and (FParents[FMates[Next]] <> NULL_INDEX) then
            begin
              CurrBase := Lca(Curr, Next);
              FBlossoms.ClearBits;
      	      MarkPath(Curr, CurrBase, Next);
      	      MarkPath(Next, CurrBase, Curr);
              for I := 0 to System.High(FBase) do
                if FBlossoms[FBase[I]] then
                  begin
                    FBase[I] := CurrBase;
                    if not FVisited[I] then
                      begin
                        FVisited[I] := True;
                        FQueue[qTail] := I;
                        Inc(qTail);
                      end;
                  end;
            end
          else
            if FParents[Next] = NULL_INDEX then
              begin
                FParents[Next] := Curr;
                if FMates[Next] = NULL_INDEX then
                  begin
                    aLast := Next;
                    exit(True);
                  end;
                Next := FMates[Next];
                FVisited[Next] := True;
                FQueue[qTail] := Next;
                Inc(qTail);
              end;
        end;
    end;
  Result := False;
end;

procedure TGSimpleGraph.TEdMatch.AlternatePath(aRoot: SizeInt);
var
  Mate, tmp: SizeInt;
begin
  repeat
    Mate := FParents[aRoot];
    tmp := FMates[Mate];
    Match(aRoot, Mate);
    aRoot := tmp;
  until aRoot = NULL_INDEX;
end;

procedure TGSimpleGraph.TEdMatch.EdMatch;
var
  I, Last: SizeInt;
begin
  for I := 0 to System.High(FMates) do
    if (FMates[I] = NULL_INDEX) and FindAugmentPath(I, Last) then
      begin
        AlternatePath(Last);
        Inc(FMatchCount);
      end;
end;

procedure TGSimpleGraph.TEdMatch.Init(aGraph: TGSimpleGraph);
var
  e: TIntEdge;
begin
  FMatchCount := 0;
  FGraph := aGraph;
  FMates := aGraph.CreateIntArray;
  FBase := aGraph.CreateIntArray;
  FParents := aGraph.CreateIntArray;
  FQueue := aGraph.CreateIntArray;
  FVisited.Size := aGraph.VertexCount;
  FLcaUsed.Size := aGraph.VertexCount;
  FBlossoms.Size := aGraph.VertexCount;
  for e in aGraph.GetApproxMatching2 do
    begin
      Match(e.Source, e.Destination);
      Inc(FMatchCount);
    end;
end;

function TGSimpleGraph.TEdMatch.GetMaxMatch(aGraph: TGSimpleGraph): TIntEdgeArray;
var
  I, J: SizeInt;
begin
  Init(aGraph);
  EdMatch;
  System.SetLength(Result, FMatchCount);
  J := 0;
  for I := 0 to System.High(FMates) do
    if FMates[I] <> NULL_INDEX then
      begin
        Result[J] := TIntEdge.Create(I, FMates[I]);
        FMates[FMates[I]] := NULL_INDEX;
        Inc(J);
      end;
end;

{ TGSimpleGraph.TPcMatch }

procedure TGSimpleGraph.TPcMatch.DoMatch(aValue, aMate: SizeInt);
begin
  FMates[aValue] := aMate;
  FMates[aMate] := aValue;
end;

procedure TGSimpleGraph.TPcMatch.TryAugment(aRoot: SizeInt);
var
  Curr, Next, Mate, Tmp: SizeInt;
  p: PAdjItem;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  FInTree.ClearBits;
  FInTree[aRoot] := True;
  FQueue[qTail] := aRoot;
  Inc(qTail);
  while qHead < qTail do
    begin
      Curr := FQueue[qHead];
      Inc(qHead);
      for p in FGraph.AdjLists[Curr]^ do
        if not FInTree[p^.Destination] then
          begin
            Next := p^.Destination;
            Mate := FMates[Next];
            if Mate = NULL_INDEX then
              begin
                FMates[Next] := Curr;
                repeat
                  Tmp := FMates[Curr];
                  FMates[Curr] := Next;
                  if Tmp <> NULL_INDEX then
                    begin
                      Curr := FGrannies[Curr];
                      FMates[Tmp] := Curr;
                      Next := Tmp;
                    end;
                until Tmp = NULL_INDEX;
                Inc(FMatchCount);
                exit;
              end
            else
              if Mate <> Curr then
                begin
                  if Curr <> aRoot then
                    begin
                      Tmp := FGrannies[Curr];
                      while (Tmp <> aRoot) and (Tmp <> Next) do
                        Tmp := FGrannies[Tmp];
                      if Tmp <> aRoot then
                        continue;
                    end;
                  FInTree[Next] := True;
                  FGrannies[Mate] := Curr;
                  FQueue[qTail] := Mate;
                  Inc(qTail);
                end;
          end;
    end;
end;

procedure TGSimpleGraph.TPcMatch.Init(aGraph: TGSimpleGraph);
var
  e: TIntEdge;
begin
  FMatchCount := 0;
  FGraph := aGraph;
  FMates := aGraph.CreateIntArray;
  FGrannies := aGraph.CreateIntArray;
  FQueue := aGraph.CreateIntArray;
  FInTree.Size := aGraph.VertexCount;
  for e in aGraph.GetApproxMatching do
    begin
      DoMatch(e.Source, e.Destination);
      Inc(FMatchCount);
    end;
end;

function TGSimpleGraph.TPcMatch.GetMaxMatch(aGraph: TGSimpleGraph): TIntEdgeArray;
var
  I, J: SizeInt;
begin
  Init(aGraph);
  for I := 0 to System.High(FMates) do
    if FMates[I] = NULL_INDEX then
      TryAugment(I);
  System.SetLength(Result, FMatchCount);
  J := 0;
  for I := 0 to System.High(FMates) do
    if FMates[I] <> NULL_INDEX then
      begin
        Result[J] := TIntEdge.Create(I, FMates[I]);
        FMates[FMates[I]] := NULL_INDEX;
        Inc(J);
      end;
end;

{ TGSimpleGraph.TNiMinCut.TNiEdge }

constructor TGSimpleGraph.TNiMinCut.TNiEdge.Create(aTarget, aWeight: SizeInt);
begin
  Target := aTarget;
  Weight := aWeight;
end;

{ TGSimpleGraph.TNiMinCut }

procedure TGSimpleGraph.TNiMinCut.ClearMarks;
var
  I: SizeInt;
  p: TNiAdjList.PEntry;
begin
  for I in FExistNodes do
    for p in FGraph[I] do
      p^.Scanned := False;
end;

procedure TGSimpleGraph.TNiMinCut.Init(aGraph: TGSimpleGraph);
var
  I: SizeInt;
  p: PAdjItem;
begin
  System.SetLength(FGraph, aGraph.VertexCount);
  for I := 0 to Pred(aGraph.VertexCount) do
    begin
      FGraph[I].EnsureCapacity(aGraph.DegreeI(I));
      for p in aGraph.AdjLists[I]^ do
        FGraph[I].Add(TNiEdge.Create(p^.Destination, 1));
    end;
  FQueue := TQueue.Create(aGraph.VertexCount);
  FExistNodes.InitRange(aGraph.VertexCount);
  FInQueue.Size := aGraph.VertexCount;
  FBestCut := High(SizeInt);
  FCuts := nil;
end;

procedure TGSimpleGraph.TNiMinCut.Init2(aGraph: TGSimpleGraph);
var
  I: SizeInt;
  p: PAdjItem;
begin
  System.SetLength(FGraph, aGraph.VertexCount);
  for I := 0 to Pred(aGraph.VertexCount) do
    begin
      FGraph[I].EnsureCapacity(aGraph.DegreeI(I));
      for p in aGraph.AdjLists[I]^ do
        FGraph[I].Add(TNiEdge.Create(p^.Destination, 1));
    end;
  System.SetLength(FCuts, aGraph.VertexCount);
  for I := 0 to Pred(aGraph.VertexCount) do
    FCuts[I].Add(I);
  FQueue := TQueue.Create(aGraph.VertexCount);
  FExistNodes.InitRange(aGraph.VertexCount);
  FInQueue.Size := aGraph.VertexCount;
  FBestCut := High(SizeInt);
end;

procedure TGSimpleGraph.TNiMinCut.ShrinkEdge(aSource, aTarget: SizeInt);
var
  I: SizeInt;
  p: TNiAdjList.PEntry;
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

procedure TGSimpleGraph.TNiMinCut.ScanFirstSearch;
var
  I: SizeInt;
  p: TNiAdjList.PEntry;
  Item: TIntNode;
begin
  ClearMarks;
  FInQueue.Join(FExistNodes);
  for I in FExistNodes do
    FQueue.Enqueue(I, TIntNode.Create(I, 0));
  while FQueue.Count > 1 do
    begin
      I := FQueue.Dequeue.Index;
      FInQueue[I] := False;
      for p in FGraph[I] do
        if FInQueue[p^.Target] then
          begin
            Item := FQueue.Peek(p^.Target);
            Item.Data += p^.Weight;
            FQueue.Update(p^.Target, Item);
            p^.Scanned := True;
            p^.ScanRank := Item.Data;
          end;
    end;
  Item := FQueue.Dequeue;
  FInQueue[Item.Index] := False;
  if Item.Data < FBestCut then
    begin
      FBestCut := Item.Data;
      if FCuts <> nil then
        FBestSet.Assign(FCuts[Item.Index]);
    end;
end;

procedure TGSimpleGraph.TNiMinCut.Shrink;
var
  I: SizeInt;
  p: TNiAdjList.PEntry;
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

function TGSimpleGraph.TNiMinCut.GetMinCut(aGraph: TGSimpleGraph): SizeInt;
begin
  Init(aGraph);
  while FExistNodes.PopCount >= 2 do
    Shrink;
  Result := FBestCut;
end;

function TGSimpleGraph.TNiMinCut.GetMinCut(aGraph: TGSimpleGraph; out aCut: TIntSet): SizeInt;
begin
  Init2(aGraph);
  while FExistNodes.PopCount >= 2 do
    Shrink;
  Result := FBestCut;
  aCut.Assign(FBestSet);
end;


