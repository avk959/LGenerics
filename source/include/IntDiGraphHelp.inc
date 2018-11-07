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

{ TGIntWeightDiGraph.THPrHelper.TFlowData }

constructor TGIntWeightDiGraph.THPrHelper.TArc.Create(aTarget: PNode; aReverse: PArc; aCap: TWeight);
begin
  Target := aTarget;
  Reverse := aReverse;
  ResCap := aCap;
  IsForward := True;
end;

constructor TGIntWeightDiGraph.THPrHelper.TArc.CreateReverse(aTarget: PNode; aReverse: PArc);
begin
  Target := aTarget;
  Reverse := aReverse;
  ResCap := 0;
  IsForward := False;
end;

function TGIntWeightDiGraph.THPrHelper.TArc.IsSaturated: Boolean;
begin
  Result := ResCap = 0;
end;

function TGIntWeightDiGraph.THPrHelper.TArc.IsResidual: Boolean;
begin
  Result := ResCap > 0;
end;

procedure TGIntWeightDiGraph.THPrHelper.TArc.Push(aFlow: TWeight);
begin
  ResCap -= aFlow;
  Target^.Excess += aFlow;
  Reverse^.ResCap += aFlow;
  Reverse^.Target^.Excess -= aFlow;
end;

{ TGIntWeightDiGraph.THPrHelper.TNode }

function TGIntWeightDiGraph.THPrHelper.TNode.GetColor: TVertexColor;
begin
  Result := TVertexColor(Distance);
end;

procedure TGIntWeightDiGraph.THPrHelper.TNode.SetColor(aValue: TVertexColor);
begin
  Distance := SizeInt(aValue);
end;

procedure TGIntWeightDiGraph.THPrHelper.TNode.Reset;
begin
  CurrentArc := FirstArc;
end;

{ TGIntWeightDiGraph.THPrHelper.TLevel }

function TGIntWeightDiGraph.THPrHelper.TLevel.IsEmpty: Boolean;
begin
  Result := (TopActive = nil) and (TopIdle = nil);
end;

procedure TGIntWeightDiGraph.THPrHelper.TLevel.AddActive(aNode: PNode);
begin
  aNode^.LevelNext := TopActive;
  TopActive := aNode;
end;

procedure TGIntWeightDiGraph.THPrHelper.TLevel.AddIdle(aNode: PNode);
var
  Next: PNode;
begin
  Next := TopIdle;
  TopIdle := aNode;
  aNode^.LevelNext := Next;
  if Next <> nil then
    Next^.LevelPrev := aNode;
end;

procedure TGIntWeightDiGraph.THPrHelper.TLevel.Activate(aNode: PNode);
begin
  RemoveIdle(aNode);
  AddActive(aNode);
end;

procedure TGIntWeightDiGraph.THPrHelper.TLevel.RemoveIdle(aNode: PNode);
var
  Next, Prev: PNode;
begin
  Next := aNode^.LevelNext;
  if TopIdle = aNode then // is on head of the list
    TopIdle := Next
  else
    begin
      Prev := aNode^.LevelPrev;
      Prev^.LevelNext := aNode^.LevelNext;
      if Next <> nil then
        Next^.LevelPrev := Prev;
    end;
end;

procedure TGIntWeightDiGraph.THPrHelper.TLevel.Clear(aLabel: SizeInt);
var
  Next: PNode;
  I: SizeInt;
begin
  Next := TopActive;
  while Next <> nil do
    begin
      Next^.Distance := aLabel;
      Next := Next^.LevelNext;
    end;
  TopActive := nil;
  Next := TopIdle;
  while Next <> nil do
    begin
      Next^.Distance := aLabel;
      Next := Next^.LevelNext;
    end;
  TopIdle  := nil;
end;

{ TGIntWeightDiGraph.THPrHelper }

procedure TGIntWeightDiGraph.THPrHelper.CreateResudualGraph(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt);
var
  CurrArcIdx: TIntArray;
  I, J: SizeInt;
  p: PAdjItem;
begin
  FNodeCount := aGraph.VertexCount;
  System.SetLength(CurrArcIdx, FNodeCount);
  J := 0;
  for I := 0 to System.High(CurrArcIdx) do
    begin
      CurrArcIdx[I] := J;
      J += aGraph.DegreeI(I);
    end;

  System.SetLength(FNodes, Succ(FNodeCount));
  System.SetLength(FArcs, Succ(aGraph.EdgeCount * 2));
  FSource := @FNodes[aSource];
  FSink := @FNodes[aSink];

  for I := 0 to Pred(FNodeCount) do
    begin
      FNodes[I].FirstArc := @FArcs[CurrArcIdx[I]];
      FNodes[I].Excess := 0;
    end;

  for I := 0 to Pred(FNodeCount) do
    for p in aGraph.AdjLists[I]^ do
      begin
        J := p^.Destination;
        FArcs[CurrArcIdx[I]] := TArc.Create(@FNodes[J], @FArcs[CurrArcIdx[J]], p^.Data.Weight);
        FArcs[CurrArcIdx[J]] := TArc.CreateReverse(@FNodes[I], @FArcs[CurrArcIdx[I]]);
        Inc(CurrArcIdx[I]);
        Inc(CurrArcIdx[J]);
      end;

  CurrArcIdx := nil;

  FArcs[System.High(FArcs)] :=
    TArc.Create(@FNodes[FNodeCount], @FArcs[System.High(FArcs)], 0);
  //sentinel node
  FNodes[FNodeCount].FirstArc := @FArcs[System.High(FArcs)];
  FNodes[FNodeCount].CurrentArc := @FArcs[System.High(FArcs)];
  FNodes[FNodeCount].LevelNext := nil;
  FNodes[FNodeCount].LevelPrev := nil;
  FNodes[FNodeCount].Excess := 0;
  FNodes[FNodeCount].Distance := FNodeCount;

  FSource^.Excess := MAX_WEIGHT;
  System.SetLength(FLevels, FNodeCount);
  FMaxLevel := System.High(FLevels);
  System.SetLength(FQueue, FNodeCount);
end;

procedure TGIntWeightDiGraph.THPrHelper.CreateResudualGraphCap(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight);
var
  CurrArcIdx: TIntArray;
  I, J: SizeInt;
  p: PAdjItem;
begin
  //almost same as above, but also stores capacities of the arcs;
  FNodeCount := aGraph.VertexCount;
  System.SetLength(CurrArcIdx, FNodeCount);
  CurrArcIdx[0] := 0;
  J := aGraph.DegreeI(0);

  for I := 1 to System.High(CurrArcIdx) do
    begin
      CurrArcIdx[I] := J;
      J += aGraph.DegreeI(I);
    end;

  System.SetLength(FNodes, Succ(FNodeCount));
  System.SetLength(FArcs, Succ(aGraph.EdgeCount * 2));
  System.SetLength(FCaps, aGraph.EdgeCount * 2);
  FSource := @FNodes[aSource];
  FSink := @FNodes[aSink];

  for I := 0 to Pred(FNodeCount) do
    begin
      FNodes[I].FirstArc := @FArcs[CurrArcIdx[I]];
      FNodes[I].Excess := 0;
    end;

  for I := 0 to Pred(FNodeCount) do
    for p in aGraph.AdjLists[I]^ do
      begin
        J := p^.Destination;
        FCaps[CurrArcIdx[I]] := p^.Data.Weight;
        FCaps[CurrArcIdx[J]] := 0;
        FArcs[CurrArcIdx[I]] := TArc.Create(@FNodes[J], @FArcs[CurrArcIdx[J]], p^.Data.Weight);
        FArcs[CurrArcIdx[J]] := TArc.CreateReverse(@FNodes[I], @FArcs[CurrArcIdx[I]]);
        Inc(CurrArcIdx[I]);
        Inc(CurrArcIdx[J]);
      end;

  CurrArcIdx := nil;

  FArcs[System.High(FArcs)] :=
    TArc.Create(@FNodes[FNodeCount], @FArcs[System.High(FArcs)], 0);
  //sentinel node
  FNodes[FNodeCount].FirstArc := @FArcs[System.High(FArcs)];
  FNodes[FNodeCount].CurrentArc := @FArcs[System.High(FArcs)];
  FNodes[FNodeCount].LevelNext := nil;
  FNodes[FNodeCount].LevelPrev := nil;
  FNodes[FNodeCount].Excess := 0;
  FNodes[FNodeCount].Distance := FNodeCount;

  FSource^.Excess := aReqFlow;
  System.SetLength(FLevels, FNodeCount);
  FMaxLevel := System.High(FLevels);
  System.SetLength(FQueue, FNodeCount);
end;

procedure TGIntWeightDiGraph.THPrHelper.ClearLabels;
var
  I: SizeInt;
begin
  for I := 0 to  System.High(FNodes) do
    FNodes[I].Distance := FNodeCount;
end;

procedure TGIntWeightDiGraph.THPrHelper.GlobalRelabel;
var
  CurrNode, NextNode: PNode;
  CurrArc: PArc;
  Dist: SizeInt;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  System.FillChar(Pointer(FLevels)^, Succ(FMaxLevel) * SizeOf(TLevel), 0);
  FMaxLevel := 0;
  FMaxActiveLevel := NULL_INDEX;
  FMinActiveLevel := FNodeCount;
  ClearLabels;
  FSink^.Distance := 0;
  CurrNode := FSink;
  FQueue[qTail] := FSink;
  Inc(qTail);
  while qHead < qTail do
    begin
      CurrNode := FQueue[qHead];
      Inc(qHead);
      Dist := Succ(CurrNode^.Distance);
      CurrArc := CurrNode^.FirstArc;
      while CurrArc < (CurrNode + 1)^.FirstArc do
        begin
          NextNode := CurrArc^.Target;
          if (NextNode^.Distance = FNodeCount) and CurrArc^.Reverse^.IsResidual then
            begin
              NextNode^.Distance := Dist;
              NextNode^.Reset;
              if Dist > FMaxLevel then
                FMaxLevel := Dist;
              if NextNode^.Excess > 0 then
                begin
                  FLevels[Dist].AddActive(NextNode);
                  if Dist > FMaxActiveLevel then
                    FMaxActiveLevel := Dist;
                  if Dist < FMinActiveLevel then
                    FMinActiveLevel := Dist;
                end
              else
                FLevels[Dist].AddIdle(NextNode);
              FQueue[qTail] := NextNode;
              Inc(qTail);
            end;
          Inc(CurrArc);
        end;
    end;
end;

procedure TGIntWeightDiGraph.THPrHelper.RemoveGap(aLayer: SizeInt);
var
  I: SizeInt;
begin
  for I := Succ(aLayer) to FMaxLevel do
    FLevels[I].Clear(FNodeCount);
  FMaxActiveLevel := Pred(aLayer);
  FMaxLevel := FMaxActiveLevel;
end;

function TGIntWeightDiGraph.THPrHelper.Push(aNode: PNode): Boolean;
var
  CurrArc: PArc;
  NextNode: PNode;
  Dist: SizeInt;
begin
  Dist := Pred(aNode^.Distance);
  while aNode^.CurrentArc < (aNode + 1)^.FirstArc do
    begin
      CurrArc := aNode^.CurrentArc;
      NextNode := CurrArc^.Target;
      if (NextNode^.Distance = Dist) and CurrArc^.IsResidual then
        //arc is not saturated and target belongs to the next layer -> arc is admissible
        begin
          if (Dist > 0) and (NextNode^.Excess = 0) then //-> NextNode in idle list
            begin
              FLevels[Dist].Activate(NextNode);
              if Dist < FMinActiveLevel then
                FMinActiveLevel := Dist;
            end;
          CurrArc^.Push(wMin(aNode^.Excess, CurrArc^.ResCap));
          if aNode^.Excess = 0 then
            break;
        end;
      Inc(aNode^.CurrentArc);
    end;
  Result := aNode^.CurrentArc < (aNode + 1)^.FirstArc;
end;

procedure TGIntWeightDiGraph.THPrHelper.Relabel(aNode: PNode);
var
  CurrArc: PArc;
  MinArc: PArc = nil;
  Dist: SizeInt;
begin
  Dist := FNodeCount;
  aNode^.Distance := FNodeCount;
  CurrArc := aNode^.FirstArc;
  while CurrArc < (aNode + 1)^.FirstArc do
    begin
      if CurrArc^.IsResidual and (CurrArc^.Target^.Distance < Dist) then
        begin
          Dist := CurrArc^.Target^.Distance;
          MinArc := CurrArc;
        end;
      Inc(CurrArc);
    end;
  Inc(Dist);
  if Dist < FNodeCount then
    begin
      aNode^.Distance := Dist;
      aNode^.CurrentArc := MinArc;
      if Dist > FMaxLevel then
        FMaxLevel := Dist;
      if aNode^.Excess > 0 then
        begin
          FLevels[Dist].AddActive(aNode);
          if Dist > FMaxActiveLevel then
            FMaxActiveLevel := Dist;
          if Dist < FMinActiveLevel then
            FMinActiveLevel := Dist;
        end
      else
        FLevels[Dist].AddIdle(aNode);
    end;
end;

procedure TGIntWeightDiGraph.THPrHelper.Execute;
var
  CurrNode: PNode;
  GlobalRelableTreshold, OldMaxActive: SizeInt;
  RelableTimes: SizeInt = 0;
begin
  GlobalRelabel;
  GlobalRelableTreshold := FNodeCount;
  while FMaxActiveLevel >= FMinActiveLevel do
    begin
      CurrNode := FLevels[FMaxActiveLevel].TopActive;
      if CurrNode <> nil then
        begin
          OldMaxActive := FMaxActiveLevel;
          FLevels[OldMaxActive].TopActive := CurrNode^.LevelNext;
          if not Push(CurrNode) then
            begin
              Relabel(CurrNode);
              Inc(RelableTimes);
              if FLevels[OldMaxActive].IsEmpty then
                RemoveGap(OldMaxActive);
              if RelableTimes = GlobalRelableTreshold then
                begin
                  GlobalRelabel;
                  RelableTimes := 0;
                end;
            end
          else
            FLevels[OldMaxActive].AddIdle(CurrNode);
        end
      else
        Dec(FMaxActiveLevel);
    end;
end;

function TGIntWeightDiGraph.THPrHelper.CreateEdges: TEdgeArray;
var
  I, J: SizeInt;
  CurrArc: PArc;
begin
  System.SetLength(Result, Pred(System.Length(FArcs)) div 2);
  J := 0;
  for I := 0 to Pred(FNodeCount) do
    begin
      CurrArc := FNodes[I].FirstArc;
      while CurrArc < FNodes[Succ(I)].FirstArc do
        begin
          if CurrArc^.IsForward then
            begin
              Result[J] := TWeightEdge.Create(I, CurrArc^.Target - PNode(FNodes), CurrArc^.Reverse^.ResCap);
              Inc(J);
            end;
          Inc(CurrArc);
        end;
    end;
end;

function TGIntWeightDiGraph.THPrHelper.RecoverFlow: TEdgeArray;
var
  CurrNode, NextNode, RootNode, RestartNode: PNode;
  StackTop: PNode = nil;
  StackBottom: PNode = nil;
  CurrArc: PArc;
  Delta: TWeight;
begin
  CurrNode := Pointer(FNodes);
  while CurrNode < PNode(FNodes) + FNodeCount do
    begin
      CurrNode^.Color := vcWhite;
      CurrNode^.Parent := nil;
      CurrNode^.Reset;
      CurrArc := CurrNode^.FirstArc;
      while CurrArc < (CurrNode + 1)^.FirstArc do
        begin
          if CurrArc^.Target = CurrNode then
            CurrArc^.ResCap := FCaps[CurrArc - PArc(FArcs)];
          Inc(CurrArc);
        end;
      Inc(CurrNode);
    end;

  CurrNode := Pointer(FNodes);
  while CurrNode < PNode(FNodes) + FNodeCount do
    begin
      if (CurrNode^.Color = vcWhite) and (CurrNode^.Excess > 0) and
         (CurrNode <> FSource) and (CurrNode <> FSink) then
           begin
             RootNode := CurrNode;
             RootNode^.Color := vcGray;
             repeat
               while CurrNode^.CurrentArc < (CurrNode + 1)^.FirstArc do
                 begin
                   CurrArc := CurrNode^.CurrentArc;
                   if (FCaps[CurrArc - PArc(FArcs)] = 0) and CurrArc^.IsResidual and
                      (CurrArc^.Target <> FSource) and (CurrArc^.Target <> FSink) then
                     begin
                       NextNode := CurrArc^.Target;
                       if NextNode^.Color = vcWhite then
                         begin
                           NextNode^.Color := vcGray;
                           NextNode^.Parent := CurrNode;
                           CurrNode := NextNode;
                           break;
                         end
                       else
                         if NextNode^.Color = vcGray then
                           begin
                             //
                             Delta := CurrArc^.ResCap;
                             while True do
                               begin
                                 Delta := wMin(Delta, NextNode^.CurrentArc^.ResCap);
                                 if NextNode = CurrNode then
                                   break
                                 else
                                   NextNode := NextNode^.CurrentArc^.Target;
                               end;
                             //
                             NextNode := CurrNode;
                             while True do
                               begin
                                 CurrArc := NextNode^.CurrentArc;
                                 CurrArc^.ResCap -= Delta;
                                 CurrArc^.Reverse^.ResCap += Delta;
                                 NextNode := CurrArc^.Target;
                                 if NextNode = CurrNode then
                                   break;
                               end;
                             //
                             RestartNode := CurrNode;
                             NextNode := CurrNode^.CurrentArc^.Target;
                             while NextNode <> CurrNode do
                               begin
                                 CurrArc := NextNode^.CurrentArc;
                                 if (NextNode^.Color = vcWhite) or CurrArc^.IsSaturated then
                                   begin
                                     NextNode^.CurrentArc^.Target^.Color := vcWhite;
                                     if NextNode^.Color <> vcWhite then
                                       RestartNode := NextNode;
                                   end;
                                 NextNode := CurrArc^.Target;
                               end;
                             //
                             if RestartNode <> CurrNode then
                               begin
                                 CurrNode := RestartNode;
                                 Inc(CurrNode^.CurrentArc);
                                 break;
                               end;
                             //
                           end;
                     end;
                   Inc(CurrNode^.CurrentArc);
                 end;
               //
               if CurrNode^.CurrentArc >= (CurrNode + 1)^.FirstArc then
                 begin
                   CurrNode^.Color := vcBlack;
                   if CurrNode <> FSource then
                     if StackBottom = nil then
                       begin
                         StackBottom := CurrNode;
                         StackTop := CurrNode;
                       end
                     else
                       begin
                         CurrNode^.OrderNext := StackTop;
                         StackTop := CurrNode;
                       end;
                   if CurrNode <> RootNode then
                     begin
                       CurrNode := CurrNode^.Parent;
                       Inc(CurrNode^.CurrentArc);
                     end
                   else
                     break;
                 end;
             until False;
           end;
      Inc(CurrNode);
    end;

  if StackBottom <> nil then
    begin
      CurrNode := StackTop;
      repeat
        CurrArc := CurrNode^.FirstArc;
        while CurrNode^.Excess > 0 do
          begin
            if (FCaps[CurrArc - PArc(FArcs)] = 0) and CurrArc^.IsResidual then
              CurrArc^.Push(wMin(CurrNode^.Excess, CurrArc^.ResCap));
            Inc(CurrArc);
          end;
        if CurrNode = StackBottom then
          break
        else
          CurrNode := CurrNode^.OrderNext;
      until False;
    end;
  Result := CreateEdges;
end;

function TGIntWeightDiGraph.THPrHelper.GetMaxFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt): TWeight;
begin
  CreateResudualGraph(aGraph, aSource, aSink);
  Execute;
  Result := FSink^.Excess;
end;

function TGIntWeightDiGraph.THPrHelper.GetMaxFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  out a: TEdgeArray): TWeight;
begin
  CreateResudualGraphCap(aGraph, aSource, aSink, MAX_WEIGHT);
  Execute;
  FLevels := nil;
  Result := FSink^.Excess;
  a := RecoverFlow;
end;

function TGIntWeightDiGraph.THPrHelper.GetMinCut(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  out s: TIntArray): TWeight;
var
  I, J: SizeInt;
begin
  CreateResudualGraph(aGraph, aSource, aSink);
  Execute;
  FLevels := nil;
  Result := FSink^.Excess;
  System.SetLength(s, ARRAY_INITIAL_SIZE);
  J := 0;
  for I := 0 to System.High(FNodes) do
    if FNodes[I].Distance = FNodeCount then
      begin
        if System.Length(s) = J then
          System.SetLength(s, J shl 1);
        s[J] := I;
        Inc(J);
      end;
  System.SetLength(s, J);
end;

function TGIntWeightDiGraph.THPrHelper.GetFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight; out m: TWeightArcMap): TWeight;
var
  e: TWeightEdge;
begin
  CreateResudualGraphCap(aGraph, aSource, aSink, aReqFlow);
  Execute;
  FLevels := nil;
  Result := FSink^.Excess;
  for e in RecoverFlow do
    m.Add(TIntEdge.Create(e.Source, e.Destination), e.Weight);
end;

{ TGIntWeightDiGraph.TDinitzHelper.TArc }

constructor TGIntWeightDiGraph.TDinitzHelper.TArc.Create(aTarget: PNode; aReverse: PArc; aCap: TWeight);
begin
  Target := aTarget;
  Reverse := aReverse;
  ResCap := aCap;
  IsForward := True;
end;

constructor TGIntWeightDiGraph.TDinitzHelper.TArc.CreateReverse(aTarget: PNode; aReverse: PArc);
begin
  Target := aTarget;
  Reverse := aReverse;
  ResCap := 0;
  IsForward := False;
end;

procedure TGIntWeightDiGraph.TDinitzHelper.TArc.Push(aFlow: TWeight);
begin
  ResCap -= aFlow;
  Reverse^.ResCap += aFlow;
end;

{ TGIntWeightDiGraph.TDinitzHelper.TNode }

procedure TGIntWeightDiGraph.TDinitzHelper.TNode.Reset;
begin
  CurrentArc := FirstArc;
end;

{ TGIntWeightDiGraph.TDinitzHelper }

procedure TGIntWeightDiGraph.TDinitzHelper.CreateResudualGraph(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt);
var
  CurrArcIdx: TIntArray;
  I, J: SizeInt;
  p: PAdjItem;
  Cap: TWeight;
begin
  FScaleFactor := 8;
  FNodeCount := aGraph.VertexCount;
  System.SetLength(CurrArcIdx, FNodeCount);
  J := 0;
  for I := 0 to System.High(CurrArcIdx) do
    begin
      CurrArcIdx[I] := J;
      J += aGraph.DegreeI(I);
    end;

  System.SetLength(FNodes, Succ(FNodeCount));
  FSource := @FNodes[aSource];
  FSink := @FNodes[aSink];
  System.SetLength(FArcs, Succ(aGraph.EdgeCount * 2));

  for I := 0 to Pred(FNodeCount) do
    FNodes[I].FirstArc := @FArcs[CurrArcIdx[I]];

  FMaxCap := 0;
  for I := 0 to Pred(FNodeCount) do
    for p in aGraph.AdjLists[I]^ do
      begin
        J := p^.Destination;
        Cap := p^.Data.Weight;
        if Cap > FMaxCap then
          FMaxCap := Cap;
        FArcs[CurrArcIdx[I]] := TArc.Create(@FNodes[J], @FArcs[CurrArcIdx[J]], Cap);
        FArcs[CurrArcIdx[J]] := TArc.CreateReverse(@FNodes[I], @FArcs[CurrArcIdx[I]]);
        Inc(CurrArcIdx[I]);
        Inc(CurrArcIdx[J]);
      end;

  CurrArcIdx := nil;

  FArcs[System.High(FArcs)] :=
    TArc.Create(@FNodes[FNodeCount], @FArcs[System.High(FArcs)], 0);
  //sentinel node
  FNodes[FNodeCount].FirstArc := @FArcs[System.High(FArcs)];
  FNodes[FNodeCount].CurrentArc := @FArcs[System.High(FArcs)];
  FNodes[FNodeCount].Distance := NULL_INDEX;

  System.SetLength(FQueue, aGraph.VertexCount);
end;

procedure TGIntWeightDiGraph.TDinitzHelper.ClearLabels;
var
  I: SizeInt;
begin
  for I := 0 to Pred(FNodeCount) do
    FNodes[I].Distance := NULL_INDEX;
  FSource^.Distance := 0;
  FSource^.Reset;
end;

function TGIntWeightDiGraph.TDinitzHelper.Relabel: Boolean;
var
  Curr, Next: PNode;
  CurrArc: PArc;
  Dist: SizeInt;
  qHead: SizeInt = 0;
  qTail: SizeInt = 0;
begin
  ClearLabels;
  FQueue[qTail] := FSource;
  Inc(qTail);
  while (qHead < qTail) and (FSink^.Distance = NULL_INDEX) do
    begin
      Curr := FQueue[qHead];
      Inc(qHead);
      Dist := Succ(Curr^.Distance);
      CurrArc := Curr^.FirstArc;
      while CurrArc < (Curr + 1)^.FirstArc do
        begin
          Next := CurrArc^.Target;
          if (Next^.Distance = NULL_INDEX) and (CurrArc^.ResCap > FMaxCap) then
            begin
              Next^.Reset;
              Next^.Distance := Dist;
              FQueue[qTail] := Next;
              Inc(qTail);
            end;
          Inc(CurrArc);
        end;
    end;
  Result := FSink^.Distance <> NULL_INDEX;
end;

function TGIntWeightDiGraph.TDinitzHelper.Dfs(aRoot: PNode; aFlow: TWeight): TWeight;
var
  Flow: TWeight;
begin
  //todo: non-recursive dfs
  if aFlow > 0 then
    if aRoot <> FSink then
      while aRoot^.CurrentArc < (aRoot + 1)^.FirstArc do
        begin
          if aRoot^.CurrentArc^.Target^.Distance = Succ(aRoot^.Distance) then
            begin
              Flow := Dfs(aRoot^.CurrentArc^.Target, wMin(aFlow, aRoot^.CurrentArc^.ResCap));
              if Flow > 0 then
                begin
                  aRoot^.CurrentArc^.Push(Flow);
                  exit(Flow);
                end;
            end;
          Inc(aRoot^.CurrentArc);
        end
    else
      exit(aFlow);
  Result := 0;
end;

function TGIntWeightDiGraph.TDinitzHelper.Execute: TWeight;
var
  Flow: TWeight;
begin
  Result := 0;
  while FMaxCap >= 1 do
    begin
      FMaxCap := FMaxCap div FScaleFactor;
      while Relabel do
        repeat
          Flow := Dfs(FSource, InfWeight);
          Result += Flow;
        until Flow = 0;
    end;
end;

function TGIntWeightDiGraph.TDinitzHelper.CreateEdges(aGraph: TGIntWeightDiGraph): TEdgeArray;
var
  I, J: SizeInt;
  CurrArc: PArc;
begin
  System.SetLength(Result, aGraph.EdgeCount);
  J := 0;
  for I := 0 to Pred(FNodeCount) do
    begin
      CurrArc := FNodes[I].FirstArc;
      while CurrArc < FNodes[Succ(I)].FirstArc do
        begin
          if CurrArc^.IsForward then
            begin
              Result[J] :=
                TWeightEdge.Create(I, CurrArc^.Target - PNode(FNodes), CurrArc^.Reverse^.ResCap);
              Inc(J);
            end;
          Inc(CurrArc);
        end;
    end;
end;

function TGIntWeightDiGraph.TDinitzHelper.GetMaxFlow(aGraph: TGIntWeightDiGraph; aSource,
  aSink: SizeInt): TWeight;
begin
  CreateResudualGraph(aGraph, aSource, aSink);
  Result := Execute;
end;

function TGIntWeightDiGraph.TDinitzHelper.GetMaxFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  out a: TEdgeArray): TWeight;
begin
  CreateResudualGraph(aGraph, aSource, aSink);
  Result := Execute;
  a := CreateEdges(aGraph);
end;

function TGIntWeightDiGraph.TDinitzHelper.GetMinCut(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  out s: TIntArray): TWeight;
var
  I, J: SizeInt;
begin
  CreateResudualGraph(aGraph, aSource, aSink);
  Result := Execute;
  System.SetLength(s, ARRAY_INITIAL_SIZE);
  J := 0;
  for I := 0 to System.High(FNodes) do
    if FNodes[I].Distance <> NULL_INDEX then
      begin
        if System.Length(s) = J then
          System.SetLength(s, J shl 1);
        s[J] := I;
        Inc(J);
      end;
  System.SetLength(s, J);
end;

{ TGIntWeightDiGraph.TSspMcfHelper.TArc }

constructor TGIntWeightDiGraph.TSspMcfHelper.TArc.Create(aTarget: PNode; aReverse: PArc; aCap: TWeight; aCost: TCost);
begin
  Target := aTarget;
  Reverse := aReverse;
  ResCap := aCap;
  Cost := aCost;
  IsForward := True;
end;

constructor TGIntWeightDiGraph.TSspMcfHelper.TArc.CreateReverse(aTarget: PNode; aReverse: PArc; aCost: TCost);
begin
  Target := aTarget;
  Reverse := aReverse;
  ResCap := 0;
  Cost := -aCost;
  IsForward := False;
end;

procedure TGIntWeightDiGraph.TSspMcfHelper.TArc.Push(aFlow: TWeight);
begin
  ResCap -= aFlow;
  Reverse^.ResCap += aFlow;
end;

{ TGIntWeightDiGraph.TSspMcfHelper }

procedure TGIntWeightDiGraph.TSspMcfHelper.CreateResudualGraph(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight; var aCosts: TEdgeCostMap);
var
  CurrArcIdx: TIntArray;
  I, J: SizeInt;
  c: TCost;
  p: PAdjItem;
begin
  FGraph := aGraph;
  FNodeCount := aGraph.VertexCount;
  FRequestFlow := aReqFlow;
  System.SetLength(CurrArcIdx, FNodeCount);
  J := 0;
  for I := 0 to System.High(CurrArcIdx) do
    begin
      CurrArcIdx[I] := J;
      J += aGraph.DegreeI(I);
    end;

  System.SetLength(FNodes, Succ(FNodeCount));
  FSource := @FNodes[aSource];
  FSink := @FNodes[aSink];
  System.SetLength(FArcs, Succ(aGraph.EdgeCount * 2));

  for I := 0 to Pred(FNodeCount) do
    FNodes[I].FirstArc := @FArcs[CurrArcIdx[I]];
  for I := 0 to Pred(FNodeCount) do
    for p in aGraph.AdjLists[I]^ do
      begin
        J := p^.Destination;
        c := aCosts[TIntEdge.Create(I, J)];
        FArcs[CurrArcIdx[I]] := TArc.Create(@FNodes[J], @FArcs[CurrArcIdx[J]], p^.Data.Weight, c);
        FArcs[CurrArcIdx[J]] := TArc.CreateReverse(@FNodes[I], @FArcs[CurrArcIdx[I]], c);
        Inc(CurrArcIdx[I]);
        Inc(CurrArcIdx[J]);
      end;

  CurrArcIdx := nil;
  Finalize(aCosts);

  FArcs[System.High(FArcs)] :=
    TArc.Create(@FNodes[FNodeCount], @FArcs[System.High(FArcs)], 0, 0);
  //sentinel node
  FNodes[FNodeCount].FirstArc := @FArcs[System.High(FArcs)];
  FNodes[FNodeCount].PathArc := nil;
  FNodes[FNodeCount].Parent := nil;
  FNodes[FNodeCount].Price := 0;
  FNodes[FNodeCount].PathMinCap := 0;

  FInQueue.Size := FNodeCount;
  FReached.Size := FNodeCount;
  FQueue := THeap.Create(FNodeCount);
end;

procedure TGIntWeightDiGraph.TSspMcfHelper.SearchInit;
var
  I: SizeInt;
begin
  for I := 0 to Pred(FNodeCount) do
    FNodes[I].Price := MAX_COST;
  FSource^.Price := 0;
  FSource^.PathMinCap := MAX_WEIGHT;
  FSink^.Parent := nil;
end;

function TGIntWeightDiGraph.TSspMcfHelper.FindNegCycle(out aMinCap: TWeight): Boolean;
var
  Queue, TreePrev, TreeNext, Level: TIntArray;
  Active: TBitVector;
  Curr, Next, Prev, Post, Test, CurrLevel, qHead, qTail: SizeInt;
  CurrArc: PArc;
begin
  SearchInit;
  TreePrev := FGraph.CreateIntArray;
  TreeNext := FGraph.CreateIntArray;
  Level := FGraph.CreateIntArray;
  Queue.Length := FNodeCount;
  Active.Size := FNodeCount;
  qHead := 0;
  qTail := 0;
  FSource^.Parent := FSource;
  Next := FSource - PNode(FNodes);
  TreePrev[Next] := Next;
  TreeNext[Next] := Next;
  Queue[qTail] := Next;
  Inc(qTail);
  Active[Next] := True;
  while qHead <> qTail do
    begin
      Curr := Queue[qHead];
      Inc(qHead);
      if qHead = FNodeCount then
        qHead := 0;
      FInQueue[Curr] := False;
      if not Active[Curr] then
        continue;
      CurrArc := FNodes[Curr].FirstArc;
      while CurrArc < FNodes[Succ(Curr)].FirstArc do
        begin
          if CurrArc^.ResCap > 0 then
            begin
              Next := CurrArc^.Target - PNode(FNodes);
              if FNodes[Curr].Price + CurrArc^.Cost < FNodes[Next].Price then
                begin
                  FNodes[Next].Price := FNodes[Curr].Price + CurrArc^.Cost;
                  FNodes[Next].PathMinCap := wMin(FNodes[Curr].PathMinCap, CurrArc^.ResCap);
                  if TreePrev[Next] <> NULL_INDEX then
                    begin
                      Prev := TreePrev[Next];
                      Test := Next;
                      CurrLevel := 0;
                      repeat
                        if Test = Curr then
                          exit(True);
                        CurrLevel += Level[Test];
                        TreePrev[Test] := NULL_INDEX;
                        Level[Test] := NULL_INDEX;
                        Active[Test] := False;
                        Test := TreeNext[Test];
                      until CurrLevel < 0;
                      Dec(Level[FNodes[Next].Parent - PNode(FNodes)]);
                      TreeNext[Prev] := Test;
                      TreePrev[Test] := Prev;
                    end;
                  FNodes[Next].Parent := @FNodes[Curr];
                  FNodes[Next].PathArc := CurrArc;
                  Inc(Level[Curr]);
                  Post := TreeNext[Curr];
                  TreeNext[Curr] := Next;
                  TreePrev[Next] := Curr;
                  TreeNext[Next] := Post;
                  TreePrev[Post] := Next;
                  if not FInQueue[Next] then
                    begin
                      Queue[qTail] := Next;
                      Inc(qTail);
                      if qTail = FNodeCount then
                        qTail := 0;
                      FInQueue[Next] := True;
                    end;
                  Active[Next] := True;
                end;
            end;
          Inc(CurrArc);
        end;
    end;
  FSource^.Parent := nil;
  Result := FSink^.Parent = nil;
  if not Result then
    aMinCap := FSink^.PathMinCap
  else
    aMinCap := 0;
end;

function TGIntWeightDiGraph.TSspMcfHelper.FindShortestPath(out aMinCap: TWeight): Boolean;
var
  CurrNode, NextNode: PNode;
  CurrArc: PArc;
  Item: TCostItem;
  Price: TCost;
  I: SizeInt;
begin
  FInQueue.ClearBits;
  FReached.ClearBits;
  FQueue.MakeEmpty;
  FSource^.PathArc := nil;
  FSource^.PathMinCap := MAX_WEIGHT;
  aMinCap := 0;
  Item := TCostItem.Create(FSource - PNode(FNodes), 0);
  repeat
    CurrNode := @FNodes[Item.Index];
    FNodes[Item.Index].Price += Item.Cost;
    FReached[Item.Index] := True;
    if CurrNode = FSink then
      break;
    CurrArc := CurrNode^.FirstArc;
    while CurrArc < (CurrNode + 1)^.FirstArc do
      begin
        if CurrArc^.ResCap > 0 then
          begin
            NextNode := CurrArc^.Target;
            if not FReached[NextNode - PNode(FNodes)] then
              begin
                I := NextNode - PNode(FNodes);
                Price := CurrNode^.Price + CurrArc^.Cost - NextNode^.Price;
                if not FInQueue[I] then
                  begin
                    NextNode^.PathMinCap := wMin(CurrNode^.PathMinCap, CurrArc^.ResCap);
                    NextNode^.Parent := CurrNode;
                    NextNode^.PathArc := CurrArc;
                    FQueue.Enqueue(I, TCostItem.Create(I, Price));
                    FInQueue[I] := True;
                  end
                else
                  if Price < FQueue.HeadPtr(I)^.Cost then
                    begin
                      NextNode^.PathMinCap := wMin(CurrNode^.PathMinCap, CurrArc^.ResCap);
                      NextNode^.Parent := CurrNode;
                      NextNode^.PathArc := CurrArc;
                      FQueue.Update(I, TCostItem.Create(I, Price));
                    end;
              end;
          end;
        Inc(CurrArc);
      end;
  until not FQueue.TryDequeue(Item);

  Result := FReached[FSink - PNode(FNodes)];
  if Result then
    begin
      for I in FReached do
        FNodes[I].Price -= Item.Cost;
      aMinCap := FSink^.PathMinCap;
    end;
end;

procedure TGIntWeightDiGraph.TSspMcfHelper.FlowIn(aFlow: TWeight);
var
  ParentNode: PNode;
  CurrArc: PArc;
begin
  ParentNode := FSink^.Parent;
  CurrArc := FSink^.PathArc;
  while CurrArc <> nil do
    begin
      CurrArc^.Push(aFlow);
      CurrArc := ParentNode^.PathArc;
      ParentNode := ParentNode^.Parent;
    end;
end;

function TGIntWeightDiGraph.TSspMcfHelper.Execute: TWeight;
var
  Flow: TWeight;
begin
  Result := 0;
  if FindNegCycle(Flow) then
    exit(0);
  Flow := wMin(Flow, FRequestFlow);
  repeat
    FlowIn(Flow);
    Result += Flow;
    Flow := 0;
    if FindShortestPath(Flow) then
      Flow := wMin(Flow, FRequestFlow - Result);
  until Flow = 0;
end;

function TGIntWeightDiGraph.TSspMcfHelper.GetTotalCost: TCost;
var
  I: SizeInt;
  CurrArc: PArc;
begin
  Result := 0;
  for I := 0 to Pred(FNodeCount) do
    begin
      CurrArc := FNodes[I].FirstArc;
      while CurrArc < FNodes[Succ(I)].FirstArc do
        begin
          if CurrArc^.IsForward then
            Result += CurrArc^.Reverse^.ResCap * CurrArc^.Cost;
          Inc(CurrArc);
        end;
    end;
end;

function TGIntWeightDiGraph.TSspMcfHelper.CreateEdges(out aTotalCost: TCost): TEdgeArray;
var
  I, J, Dst: SizeInt;
  CurrArc: PArc;
  w: TWeight;
begin
  System.SetLength(Result, Pred(System.Length(FArcs)) shr 1);
  J := 0;
  aTotalCost := 0;
  for I := 0 to Pred(FNodeCount) do
    begin
      CurrArc := FNodes[I].FirstArc;
      while CurrArc < FNodes[Succ(I)].FirstArc do
        begin
          if CurrArc^.IsForward then
            begin
              Dst := CurrArc^.Target - PNode(FNodes);
              w := CurrArc^.Reverse^.ResCap;
              aTotalCost += w * CurrArc^.Cost;
              Result[J] := TWeightEdge.Create(I, Dst, w);
              Inc(J);
            end;
          Inc(CurrArc);
        end;
    end;
end;

function TGIntWeightDiGraph.TSspMcfHelper.GetMinCostFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight; var aCosts: TEdgeCostMap; out aTotalCost: TCost): TWeight;
begin
  if aReqFlow <= 0 then
    exit(0);
  CreateResudualGraph(aGraph, aSource, aSink, aReqFlow, aCosts);
  Result := Execute;
  if Result > 0 then
    aTotalCost := GetTotalCost;
end;

function TGIntWeightDiGraph.TSspMcfHelper.GetMinCostFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight; var aCosts: TEdgeCostMap; out aTotalCost: TCost; out a: TEdgeArray): TWeight;
begin
  if aReqFlow <= 0 then
    exit(0);
  CreateResudualGraph(aGraph, aSource, aSink, aReqFlow, aCosts);
  Result := Execute;
  if Result > 0 then
    a := CreateEdges(aTotalCost);
end;

{ TGIntWeightDiGraph.TCosMcfHelper.TArc }

constructor TGIntWeightDiGraph.TCosMcfHelper.TArc.Create(aTarget: PNode; aReverse: PArc; aCap: TWeight;
  aCost: TCost);
begin
  Target := aTarget;
  Reverse := aReverse;
  ResCap := aCap;
  Cost := aCost;
  IsForward := True;
end;

constructor TGIntWeightDiGraph.TCosMcfHelper.TArc.CreateReverse(aTarget: PNode; aReverse: PArc; aCost: TCost);
begin
  Target := aTarget;
  Reverse := aReverse;
  ResCap := 0;
  Cost := -aCost;
  IsForward := False;
end;

procedure TGIntWeightDiGraph.TCosMcfHelper.TArc.Push(aFlow: TWeight);
begin
  ResCap -= aFlow;
  Target^.Excess += aFlow;
  Reverse^.ResCap += aFlow;
  Reverse^.Target^.Excess -= aFlow;
end;

{ TGIntWeightDiGraph.TCosMcfHelper.TNode }

procedure TGIntWeightDiGraph.TCosMcfHelper.TNode.Reset;
begin
  CurrArc := FirstArc;
end;

{ TGIntWeightDiGraph.TCosMcfHelper }

procedure TGIntWeightDiGraph.TCosMcfHelper.CreateResidualGraph(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight; var aCosts: TEdgeCostMap);
var
  CurrArcIdx: TIntArray;
  I, J: SizeInt;
  c: TCost;
  p: PAdjItem;
begin
  FAlpha := 8;
  FNodeCount := aGraph.VertexCount;
  FGraph := aGraph;
  Factor := FAlpha * FNodeCount;
  FRequestFlow := aReqFlow;
  System.SetLength(CurrArcIdx, FNodeCount);
  J := 0;
  for I := 0 to System.High(CurrArcIdx) do
    begin
      CurrArcIdx[I] := J;
      J += aGraph.DegreeI(I);
    end;

  System.SetLength(FNodes, Succ(FNodeCount));
  FSource := @FNodes[aSource];
  FSink := @FNodes[aSink];
  System.SetLength(FArcs, Succ(aGraph.EdgeCount * 2));

  for I := 0 to Pred(FNodeCount) do
    begin
      FNodes[I].FirstArc := @FArcs[CurrArcIdx[I]];
      FNodes[I].CurrArc := @FArcs[CurrArcIdx[I]];
      FNodes[I].Price := MAX_COST;
      FNodes[I].Excess := 0;
    end;

  for I := 0 to Pred(FNodeCount) do
    for p in aGraph.AdjLists[I]^ do
      begin
        J := p^.Destination;
        c := aCosts[TIntEdge.Create(I, J)];
        FArcs[CurrArcIdx[I]] := TArc.Create(@FNodes[J], @FArcs[CurrArcIdx[J]], p^.Data.Weight, c);
        FArcs[CurrArcIdx[J]] := TArc.CreateReverse(@FNodes[I], @FArcs[CurrArcIdx[I]], c);
        Inc(CurrArcIdx[I]);
        Inc(CurrArcIdx[J]);
      end;

  CurrArcIdx := nil;
  Finalize(aCosts);

  FArcs[System.High(FArcs)] :=
    TArc.Create(@FNodes[FNodeCount], @FArcs[System.High(FArcs)], 0, 0);
  //sentinel node
  FNodes[FNodeCount].FirstArc := @FArcs[System.High(FArcs)];
  FNodes[FNodeCount].CurrArc :=  @FArcs[System.High(FArcs)];
  FNodes[FNodeCount].Price := 0;
  FNodes[FNodeCount].Excess := 0;

  FInQueue.Size := FNodeCount;
  FQueue.EnsureCapacity(FNodeCount);
end;

function TGIntWeightDiGraph.TCosMcfHelper.IndexOf(aNode: PNode): SizeInt;
begin
  Result := aNode - PNode(FNodes);
end;

function TGIntWeightDiGraph.TCosMcfHelper.PriceUpdate: Boolean;
var
  Queue, Parents, TreePrev, TreeNext, Level: TIntArray;
  Active: TBitVector;
  Curr, Next, Prev, Post, Test, CurrLevel, qHead, qTail: SizeInt;
  CurrArc: PArc;
begin
  Parents := FGraph.CreateIntArray;
  TreePrev := FGraph.CreateIntArray;
  TreeNext := FGraph.CreateIntArray;
  Level := FGraph.CreateIntArray;
  Queue.Length := FNodeCount;
  Active.Size := FNodeCount;
  qHead := 0;
  qTail := 0;
  Next := IndexOf(FSource);
  FNodes[Next].Price := 0;
  Parents[Next] := Next;
  TreePrev[Next] := Next;
  TreeNext[Next] := Next;
  Active[Next] := True;
  Queue[qTail] := Next;
  Inc(qTail);
  while qHead <> qTail do
    begin
      Curr := Queue[qHead];
      Inc(qHead);
      if qHead = FNodeCount then
        qHead := 0;
      FInQueue[Curr] := False;
      if not Active[Curr] then
        continue;
      CurrArc := FNodes[Curr].FirstArc;
      while CurrArc < FNodes[Succ(Curr)].FirstArc do
        begin
          if CurrArc^.ResCap > 0 then
            begin
              Next := IndexOf(CurrArc^.Target);
              if FNodes[Curr].Price + CurrArc^.Cost < FNodes[Next].Price then
                begin
                  FNodes[Next].Price := FNodes[Curr].Price + CurrArc^.Cost;
                  if TreePrev[Next] <> NULL_INDEX then
                    begin
                      Prev := TreePrev[Next];
                      Test := Next;
                      CurrLevel := 0;
                      repeat
                        if Test = Curr then
                          exit(False);
                        CurrLevel += Level[Test];
                        TreePrev[Test] := NULL_INDEX;
                        Level[Test] := NULL_INDEX;
                        Active[Test] := False;
                        Test := TreeNext[Test];
                      until CurrLevel < 0;
                      Dec(Level[Parents[Next]]);
                      TreeNext[Prev] := Test;
                      TreePrev[Test] := Prev;
                    end;
                  Parents[Next] := Curr;
                  Inc(Level[Curr]);
                  Post := TreeNext[Curr];
                  TreeNext[Curr] := Next;
                  TreePrev[Next] := Curr;
                  TreeNext[Next] := Post;
                  TreePrev[Post] := Next;
                  if not FInQueue[Next] then
                    begin
                      Queue[qTail] := Next;
                      Inc(qTail);
                      if qTail = FNodeCount then
                        qTail := 0;
                      FInQueue[Next] := True;
                    end;
                  Active[Next] := True;
                end;
            end;
          Inc(CurrArc);
        end;
    end;
  Result := Parents[IndexOf(FSink)] >= 0;
end;

procedure TGIntWeightDiGraph.TCosMcfHelper.InitSolution(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt; out
  m: TWeightArcMap);
var
  Helper: THPrHelper;
begin
  FResultFlow := Helper.GetFlow(aGraph, aSource, aSink, FRequestFlow, m);
end;

function TGIntWeightDiGraph.TCosMcfHelper.InitSuccess: Boolean;
var
  ArcMap: TWeightArcMap;
  CurrArc: PArc;
  I, Src, Dst: SizeInt;
  cAbs: TCost;
begin
  FResultFlow := 0;
  if not PriceUpdate then
    exit(False);
  InitSolution(FGraph, FSource - PNode(FNodes), FSink - PNode(FNodes), ArcMap);
  if FResultFlow = 0 then
    exit(False);

  FSource^.Excess += FResultFlow;
  FSink^.Excess -= FResultFlow;
  FEpsilon := 0;

  for I := 0 to Pred(System.High(FArcs)) do
    begin
      FArcs[I].Cost *= Factor;
      if FArcs[I].IsForward then
        begin
          cAbs := Abs(FArcs[I].Cost);
          if cAbs > FEpsilon then
            FEpsilon := cAbs;
          Src := FArcs[I].Reverse^.Target - PNode(FNodes);
          Dst := FArcs[I].Target - PNode(FNodes);
          FArcs[I].Push(ArcMap[TIntEdge.Create(Src, Dst)]);
        end;
    end;
  if FEpsilon < 1 then
    FEpsilon := 1;
  Result := True;
end;

procedure TGIntWeightDiGraph.TCosMcfHelper.InitPhase;
var
  I: SizeInt;
  CurrArc: PArc;
begin
  for I := 0 to Pred(FNodeCount) do
    begin
      CurrArc := FNodes[I].FirstArc;
      while CurrArc < FNodes[Succ(I)].FirstArc do
        begin
          if (CurrArc^.ResCap > 0) and (FNodes[I].Price + CurrArc^.Cost - CurrArc^.Target^.Price < 0) then
            CurrArc^.Push(CurrArc^.ResCap);
          Inc(CurrArc);
        end;
    end;
  for I := 0 to Pred(FNodeCount) do
    if FNodes[I].Excess > 0 then
      begin
        FNodes[I].Reset;
        FQueue.Enqueue(@FNodes[I]);
        FInQueue[I] := True;
      end;
end;

procedure TGIntWeightDiGraph.TCosMcfHelper.Discharge(aNode: PNode);
var
  NextNode: PNode;
  CurrArc, NextArc: PArc;
  MinPrice: TCost;
begin
  while aNode^.Excess > 0 do
    begin
      while aNode^.CurrArc < (aNode + 1)^.FirstArc do
        begin
          CurrArc := aNode^.CurrArc;
          NextNode := CurrArc^.Target;
          if (CurrArc^.ResCap > 0) and (aNode^.Price + CurrArc^.Cost - NextNode^.Price < 0) then
            begin
              CurrArc^.Push(wMin(aNode^.Excess, CurrArc^.ResCap));
              if (NextNode^.Excess > 0) and not FInQueue[IndexOf(NextNode)] then
                begin
                  FQueue.Enqueue(NextNode);
                  FInQueue[IndexOf(NextNode)] := True;
                end;
            end;
          Inc(aNode^.CurrArc);
        end;
      if aNode^.Excess > 0 then
        begin
          CurrArc := aNode^.FirstArc;
          aNode^.Reset;
          MinPrice := MAX_COST;
          while CurrArc < (aNode + 1)^.FirstArc do
            begin
              NextNode := CurrArc^.Target;
              if (CurrArc^.ResCap > 0) and (aNode^.Price + CurrArc^.Cost - NextNode^.Price < MinPrice) then
                MinPrice := aNode^.Price + CurrArc^.Cost - NextNode^.Price;
              Inc(CurrArc);
            end;
          aNode^.Price -= MinPrice + FEpsilon;
        end;
    end;
end;

procedure TGIntWeightDiGraph.TCosMcfHelper.Done;
var
  I: SizeInt;
begin
  for I := 0 to Pred(System.High(FArcs)) do
    if FArcs[I].IsForward then
      FArcs[I].Cost := FArcs[I].Cost div Factor;
end;

procedure TGIntWeightDiGraph.TCosMcfHelper.Execute;
var
  Node: PNode;
begin
  while True do
    begin
      FEpsilon := Math.Max(FEpsilon div FAlpha, 1);
      InitPhase;
      while FQueue.TryDequeue(Node) do
        begin
          FInQueue[IndexOf(Node)] := False;
          Discharge(Node{%H-});
        end;
      if FEpsilon = 1 then
        break;
    end;
  Done;
end;

function TGIntWeightDiGraph.TCosMcfHelper.GetTotalCost: TCost;
var
  I: SizeInt;
  CurrArc: PArc;
begin
  Result := 0;
  for I := 0 to Pred(FNodeCount) do
    begin
      CurrArc := FNodes[I].FirstArc;
      while CurrArc < FNodes[Succ(I)].FirstArc do
        begin
          if CurrArc^.IsForward then
            Result += CurrArc^.Reverse^.ResCap * CurrArc^.Cost;
          Inc(CurrArc);
        end;
    end;
end;

function TGIntWeightDiGraph.TCosMcfHelper.CreateEdges(out aTotalCost: TCost): TEdgeArray;
var
  I, J, Dst: SizeInt;
  CurrArc: PArc;
  w: TWeight;
begin
  System.SetLength(Result, Pred(System.Length(FArcs)) shr 1);
  J := 0;
  aTotalCost := 0;
  for I := 0 to Pred(FNodeCount) do
    begin
      CurrArc := FNodes[I].FirstArc;
      while CurrArc < FNodes[Succ(I)].FirstArc do
        begin
          if CurrArc^.IsForward then
            begin
              Dst := CurrArc^.Target - PNode(FNodes);
              w := CurrArc^.Reverse^.ResCap;
              aTotalCost += w * CurrArc^.Cost;
              Result[J] := TWeightEdge.Create(I, Dst, w);
              Inc(J);
            end;
          Inc(CurrArc);
        end;
    end;
end;

function TGIntWeightDiGraph.TCosMcfHelper.GetMinCostFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight; var aCosts: TEdgeCostMap; out aTotalCost: TCost): TWeight;
begin
  if aReqFlow <= 0 then
    exit(0);
  CreateResidualGraph(aGraph, aSource, aSink, aReqFlow, aCosts);
  if not InitSuccess then
    exit(0);
  Execute;
  Result := FResultFlow;
  aTotalCost := GetTotalCost;
end;

function TGIntWeightDiGraph.TCosMcfHelper.GetMinCostFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight; var aCosts: TEdgeCostMap; out aTotalCost: TCost; out a: TEdgeArray): TWeight;
begin
  if aReqFlow <= 0 then
    exit(0);
  CreateResidualGraph(aGraph, aSource, aSink, aReqFlow, aCosts);
  if not InitSuccess then
    exit(0);
  Execute;
  Result := FResultFlow;
  a := CreateEdges(aTotalCost);
end;


