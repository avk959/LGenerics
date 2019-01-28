{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*                                                                           *
*   Copyright(c) 2018-2019 A.Koverdyaev(avk)                                *
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

procedure TGIntWeightDiGraph.THPrHelper.CreateResidualNet(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt);
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

procedure TGIntWeightDiGraph.THPrHelper.CreateResidualNetCap(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
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
  TimeToGlobalRelable, OldMaxActive: SizeInt;
  RelableCount: SizeInt = 0;
begin
  GlobalRelabel;
  TimeToGlobalRelable := FNodeCount;
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
              Inc(RelableCount);
              if FLevels[OldMaxActive].IsEmpty then
                RemoveGap(OldMaxActive);
              if RelableCount = TimeToGlobalRelable then
                begin
                  GlobalRelabel;
                  RelableCount := 0;
                end;
            end
          else
            FLevels[OldMaxActive].AddIdle(CurrNode);
        end
      else
        Dec(FMaxActiveLevel);
    end;
end;

function TGIntWeightDiGraph.THPrHelper.CreateArcFlows: TEdgeArray;
var
  I, J: SizeInt;
  CurrArc: PArc;
begin
  System.SetLength(Result{%H-}, Pred(System.Length(FArcs)) div 2);
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
  Result := CreateArcFlows;
end;

function TGIntWeightDiGraph.THPrHelper.GetMaxFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt): TWeight;
begin
  CreateResidualNet(aGraph, aSource, aSink);
  Execute;
  Result := FSink^.Excess;
end;

function TGIntWeightDiGraph.THPrHelper.GetMaxFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  out a: TEdgeArray): TWeight;
begin
  CreateResidualNetCap(aGraph, aSource, aSink, MAX_WEIGHT);
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
  CreateResidualNet(aGraph, aSource, aSink);
  Execute;
  FLevels := nil;
  Result := FSink^.Excess;
  System.SetLength(s{%H-}, ARRAY_INITIAL_SIZE);
  J := 0;
  for I := 0 to Pred(FNodeCount) do
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
  CreateResidualNetCap(aGraph, aSource, aSink, aReqFlow);
  Execute;
  FLevels := nil;
  Result := FSink^.Excess;
  for e in RecoverFlow do
    m.Add(e.Edge, e.Weight);
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

procedure TGIntWeightDiGraph.TDinitzHelper.CreateResidualNet(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt);
var
  CurrArcIdx: TIntArray;
  I, J: SizeInt;
  p: PAdjItem;
  Cap: TWeight;
begin
  FNodeCount := aGraph.VertexCount;
  System.SetLength(CurrArcIdx{%H-}, FNodeCount);
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

  FEpsilon := 0;
  for I := 0 to Pred(FNodeCount) do
    for p in aGraph.AdjLists[I]^ do
      begin
        J := p^.Destination;
        Cap := p^.Data.Weight;
        if Cap > FEpsilon then
          FEpsilon := Cap;
        FArcs[CurrArcIdx[I]] := TArc.Create(@FNodes[J], @FArcs[CurrArcIdx[J]], Cap);
        FArcs[CurrArcIdx[J]] := TArc.CreateReverse(@FNodes[I], @FArcs[CurrArcIdx[I]]);
        Inc(CurrArcIdx[I]);
        Inc(CurrArcIdx[J]);
      end;
  CurrArcIdx := nil;
  FScaleFactor := wMax(MIN_SCALE, BsrQWord(FEpsilon));

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

function TGIntWeightDiGraph.TDinitzHelper.BuildLevelGraph: Boolean;
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
          if (Next^.Distance = NULL_INDEX) and (CurrArc^.ResCap > FEpsilon) then
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

function TGIntWeightDiGraph.TDinitzHelper.FindBlockingFlow(aRoot: PNode; aFlow: TWeight): TWeight;
var
  Arc: PArc;
begin
  if aFlow > 0 then
    if aRoot <> FSink then
      while aRoot^.CurrentArc < (aRoot + 1)^.FirstArc do
        begin
          Arc := aRoot^.CurrentArc;
          if Arc^.Target^.Distance = Succ(aRoot^.Distance) then
            begin
              Result := FindBlockingFlow(Arc^.Target, wMin(aFlow, Arc^.ResCap));
              if Result > 0 then
                begin
                  Arc^.Push(Result);
                  exit;
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
  while FEpsilon > 0 do
    begin
      FEpsilon := FEpsilon div FScaleFactor;
      while BuildLevelGraph do
        repeat
          Flow := FindBlockingFlow(FSource, TWeight.INF_VALUE);
          Result += Flow;
        until Flow = 0;
    end;
end;

function TGIntWeightDiGraph.TDinitzHelper.CreateArcFlows(aGraph: TGIntWeightDiGraph): TEdgeArray;
var
  I, J: SizeInt;
  CurrArc: PArc;
begin
  System.SetLength(Result{%H-}, aGraph.EdgeCount);
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
  CreateResidualNet(aGraph, aSource, aSink);
  Result := Execute;
end;

function TGIntWeightDiGraph.TDinitzHelper.GetMaxFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  out a: TEdgeArray): TWeight;
begin
  CreateResidualNet(aGraph, aSource, aSink);
  Result := Execute;
  a := CreateArcFlows(aGraph);
end;

function TGIntWeightDiGraph.TDinitzHelper.GetMinCut(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  out s: TIntArray): TWeight;
var
  I, J: SizeInt;
begin
  CreateResidualNet(aGraph, aSource, aSink);
  Result := Execute;
  System.SetLength(s{%H-}, ARRAY_INITIAL_SIZE);
  J := 0;
  for I := 0 to Pred(FNodeCount) do
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

procedure TGIntWeightDiGraph.TSspMcfHelper.CreateResidualNet(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
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

function TGIntWeightDiGraph.TSspMcfHelper.SetNodePrices: Boolean;
var
  Queue, TreePrev, TreeNext, Level: TIntArray;
  Active: TBitVector;
  Curr, Next, Prev, Post, Test, CurrLevel, qHead, qTail: SizeInt;
  CurrArc: PArc;
  NodePrice: TCost;
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
      NodePrice := FNodes[Curr].Price;
      while CurrArc < FNodes[Succ(Curr)].FirstArc do
        begin
          if CurrArc^.ResCap > 0 then
            begin
              Next := CurrArc^.Target - PNode(FNodes);
              if NodePrice + CurrArc^.Cost < FNodes[Next].Price then
                begin
                  FNodes[Next].Price := NodePrice + CurrArc^.Cost;
                  FNodes[Next].PathMinCap := wMin(FNodes[Curr].PathMinCap, CurrArc^.ResCap);
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
  Result := FSink^.Parent <> nil;
end;
{$PUSH}{$MACRO ON}
function TGIntWeightDiGraph.TSspMcfHelper.FindAugmentPath: Boolean;
var
  Node, NextNode: PNode;
  Arc: PArc;
  I: SizeInt;
  Item: TCostItem;
  NodePrice, RelaxPrice: TCost;
{$DEFINE UpdateNextNode :=
  NextNode^.PathMinCap := wMin(Node^.PathMinCap, Arc^.ResCap);
  NextNode^.Parent := Node;
  NextNode^.PathArc := Arc
}
begin
  FInQueue.ClearBits;
  FReached.ClearBits;
  FQueue.MakeEmpty;
  FSource^.PathArc := nil;
  FSource^.PathMinCap := MAX_WEIGHT;
  Item := TCostItem.Create(FSource - PNode(FNodes), 0);
  repeat
    Node := @FNodes[Item.Index];
    FNodes[Item.Index].Price += Item.Cost;
    FReached[Item.Index] := True;
    if Node = FSink then
      break;
    Arc := Node^.FirstArc;
    NodePrice := Node^.Price;
    while Arc < (Node + 1)^.FirstArc do
      begin
        if Arc^.ResCap > 0 then
          begin
            NextNode := Arc^.Target;
            I := NextNode - PNode(FNodes);
            if not FReached[I] then
              begin
                RelaxPrice := NodePrice + Arc^.Cost - NextNode^.Price;
                if not FInQueue[I] then
                  begin
                    UpdateNextNode;
                    FQueue.Enqueue(I, TCostItem.Create(I, RelaxPrice));
                    FInQueue[I] := True;
                  end
                else
                  if RelaxPrice < FQueue.HeadPtr(I)^.Cost then
                    begin
                      UpdateNextNode;
                      FQueue.Update(I, TCostItem.Create(I, RelaxPrice));
                    end;
              end;
          end;
        Inc(Arc);
      end;
  until not FQueue.TryDequeue(Item);

  Result := FReached[FSink - PNode(FNodes)];
  if Result then
    for I in FReached do
      FNodes[I].Price -= Item.Cost;
{$UNDEF UpdateNextNode}
end;
{$POP}
function TGIntWeightDiGraph.TSspMcfHelper.PushFlow(aRestFlow: TWeight): TWeight;
var
  Node: PNode;
begin
  Node := FSink;
  Result := wMin(aRestFlow, FSink^.PathMinCap);
  repeat
    if Node^.Parent <> nil then
      Node^.PathArc^.Push(Result);
    Node := Node^.Parent;
  until Node = nil;
end;

function TGIntWeightDiGraph.TSspMcfHelper.Execute: TWeight;
begin
  Result := 0;
  if not SetNodePrices then
    exit;
  repeat
    Result += PushFlow(FRequestFlow - Result);
  until (Result = FRequestFlow) or not FindAugmentPath;
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

function TGIntWeightDiGraph.TSspMcfHelper.CreateArcFlows(out aTotalCost: TCost): TEdgeArray;
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
    begin
      Result := 0;
      raise EGraphError.Create(SEMethodNotApplicable);
    end;
  CreateResidualNet(aGraph, aSource, aSink, aReqFlow, aCosts);
  Result := Execute;
  if Result > 0 then
    aTotalCost := GetTotalCost;
end;

function TGIntWeightDiGraph.TSspMcfHelper.GetMinCostFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight; var aCosts: TEdgeCostMap; out aTotalCost: TCost; out a: TEdgeArray): TWeight;
begin
  if aReqFlow <= 0 then
    begin
      Result := 0;
      raise EGraphError.Create(SEMethodNotApplicable);
    end;
  CreateResidualNet(aGraph, aSource, aSink, aReqFlow, aCosts);
  Result := Execute;
  if Result > 0 then
    a := CreateArcFlows(aTotalCost);
end;

{ TGIntWeightDiGraph.TCsMcfHelper.TArc }

constructor TGIntWeightDiGraph.TCsMcfHelper.TArc.Create(aTarget: PNode; aReverse: PArc; aCap: TWeight;
  aCost: TCost);
begin
  Target := aTarget;
  Reverse := aReverse;
  ResCap := aCap;
  Cost := aCost;
  IsForward := True;
end;

constructor TGIntWeightDiGraph.TCsMcfHelper.TArc.CreateReverse(aTarget: PNode; aReverse: PArc; aCost: TCost);
begin
  Target := aTarget;
  Reverse := aReverse;
  ResCap := 0;
  Cost := -aCost;
  IsForward := False;
end;

procedure TGIntWeightDiGraph.TCsMcfHelper.TArc.Push(aFlow: TWeight);
begin
  ResCap -= aFlow;
  Target^.Excess += aFlow;
  Reverse^.ResCap += aFlow;
  Reverse^.Target^.Excess -= aFlow;
end;

procedure TGIntWeightDiGraph.TCsMcfHelper.TArc.PushAll;
begin
  Target^.Excess += ResCap;
  Reverse^.ResCap += ResCap;
  Reverse^.Target^.Excess -= ResCap;
  ResCap := 0;
end;

{ TGIntWeightDiGraph.TCsMcfHelper.TNode }

procedure TGIntWeightDiGraph.TCsMcfHelper.TNode.Reset;
begin
  CurrArc := FirstArc;
end;

{ TGIntWeightDiGraph.TCsMcfHelper }

procedure TGIntWeightDiGraph.TCsMcfHelper.CreateResidualNet(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight; var aCosts: TEdgeCostMap);
var
  CurrArcIdx: TIntArray;
  I, J: SizeInt;
  c: TCost;
  p: PAdjItem;
begin
  FGraph := aGraph;
  FNodeCount := aGraph.VertexCount;
  FScaleFactor := TCost(FNodeCount) * ALPHA;
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
      FNodes[I].Price := 0;
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

function TGIntWeightDiGraph.TCsMcfHelper.IndexOf(aNode: PNode): SizeInt;
begin
  Result := aNode - PNode(FNodes);
end;

function TGIntWeightDiGraph.TCsMcfHelper.PriceUpdate: Boolean;
var
  Queue, Parents, TreePrev, TreeNext, Level: TIntArray;
  InQueue, Active: TBoolVector;
  Curr, Next, Prev, Post, Test, CurrLevel, qLen, qHead, qTail: SizeInt;
  CurrArc: PArc;
  NodePrice: TCost;
begin
  qLen := Succ(FNodeCount);
  qHead := 0;
  qTail := 0;
  TreeNext := FGraph.CreateIntArray(qLen, NULL_INDEX);
  Level := FGraph.CreateIntArray(qLen, NULL_INDEX);
  Parents.Length := qLen;
  TreePrev.Length := qLen;
  Queue.Length := qLen;
  Active.Size := FNodeCount;
  InQueue.Size := FNodeCount;
  Parents[FNodeCount] := FNodeCount;
  TreePrev[FNodeCount] := FNodeCount;
  TreeNext[FNodeCount] := FNodeCount;
  for Curr := 0 to Pred(FNodeCount) do
    begin
      Parents[Curr] := FNodeCount;
      TreePrev[Curr] := FNodeCount;
      Queue[qTail] := Curr;
      Inc(qTail);
    end;
  InQueue.InitRange(FNodeCount);
  Active.InitRange(FNodeCount);
  while qHead <> qTail do
    begin
      Curr := Queue[qHead];
      Inc(qHead);
      if qHead = qLen then
        qHead := 0;
      InQueue[Curr] := False;
      if not Active[Curr] then
        continue;
      CurrArc := FNodes[Curr].FirstArc;
      NodePrice := FNodes[Curr].Price;
      while CurrArc < FNodes[Succ(Curr)].FirstArc do
        begin
          if CurrArc^.ResCap > 0 then
            begin
              Next := IndexOf(CurrArc^.Target);
              if NodePrice + CurrArc^.Cost < FNodes[Next].Price then
                begin
                  FNodes[Next].Price := NodePrice + CurrArc^.Cost;
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
                  if not InQueue[Next] then
                    begin
                      Queue[qTail] := Next;
                      Inc(qTail);
                      if qTail = qLen then
                        qTail := 0;
                      InQueue[Next] := True;
                    end;
                  Active[Next] := True;
                end;
            end;
          Inc(CurrArc);
        end;
    end;
  Result := True;
end;

procedure TGIntWeightDiGraph.TCsMcfHelper.FindRequiredFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  out m: TWeightArcMap);
var
  Helper: THPrHelper;
begin
  FResultFlow := Helper.GetFlow(aGraph, aSource, aSink, FRequestFlow, m);
end;

function TGIntWeightDiGraph.TCsMcfHelper.FindInitSolution: Boolean;
var
  ArcMap: TWeightArcMap;
  I, Src, Dst: SizeInt;
  CostAbs: TCost;
begin
  FResultFlow := 0;
  if not PriceUpdate then
    exit(False);
  FindRequiredFlow(FGraph, IndexOf(FSource), IndexOf(FSink), ArcMap);
  if FResultFlow = 0 then
    exit(False);
  FSource^.Excess += FResultFlow;
  FSink^.Excess -= FResultFlow;
  FEpsilon := 0;
  for I := 0 to Pred(System.High(FArcs)) do
    begin
      FArcs[I].Cost *= FScaleFactor;
      if FArcs[I].IsForward then
        begin
          CostAbs := Abs(FArcs[I].Cost);
          if CostAbs > FEpsilon then
            FEpsilon := CostAbs;
          Src := IndexOf(FArcs[I].Reverse^.Target);
          Dst := IndexOf(FArcs[I].Target);
          FArcs[I].Push(ArcMap[TIntEdge.Create(Src, Dst)]);
        end;
    end;
  if FEpsilon < 1 then
    FEpsilon := 1;
  Result := True;
end;

procedure TGIntWeightDiGraph.TCsMcfHelper.InitPhase;
var
  Arc: PArc;
  I, J: SizeInt;
  PriceI: TCost;
begin
  for I := 0 to Pred(FNodeCount) do
    begin
      Arc := FNodes[I].FirstArc;
      PriceI := FNodes[I].Price;
      while Arc < FNodes[Succ(I)].FirstArc do
        begin
          J := IndexOf(Arc^.Target);
          if (Arc^.ResCap > 0) and (PriceI + Arc^.Cost - FNodes[J].Price <= -FEpsilon) then
            Arc^.PushAll;
          Inc(Arc);
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

procedure TGIntWeightDiGraph.TCsMcfHelper.Discharge(aNode: PNode);
var
  NextNode: PNode;
  Arc: PArc;
  NodePrice, MinPrice: TCost;
begin
  while aNode^.Excess > 0 do
    begin
      NodePrice := aNode^.Price;
      while aNode^.CurrArc < (aNode + 1)^.FirstArc do
        begin
          Arc := aNode^.CurrArc;
          if Arc^.ResCap > 0 then
            begin
              NextNode := Arc^.Target;
              if NodePrice + Arc^.Cost - NextNode^.Price < 0 then
                begin
                  Arc^.Push(wMin(aNode^.Excess, Arc^.ResCap));
                  if (NextNode^.Excess > 0) and not FInQueue[IndexOf(NextNode)] then
                    begin
                      FQueue.Enqueue(NextNode);
                      FInQueue[IndexOf(NextNode)] := True;
                    end;
                end;
            end;
          Inc(aNode^.CurrArc);
        end;
      if aNode^.Excess > 0 then
        begin
          Arc := aNode^.FirstArc;
          MinPrice := MAX_COST;
          while Arc < (aNode + 1)^.FirstArc do
            begin
              if Arc^.ResCap > 0 then
                begin
                  NextNode := Arc^.Target;
                  if NodePrice + Arc^.Cost - NextNode^.Price < MinPrice then
                    MinPrice := NodePrice + Arc^.Cost - NextNode^.Price;
                end;
              Inc(Arc);
            end;
          aNode^.Price -= MinPrice + FEpsilon;
          aNode^.Reset;
        end;
    end;
end;

procedure TGIntWeightDiGraph.TCsMcfHelper.Execute;
var
  Node: PNode;
  NeedUpdate: Boolean = True;
begin
  repeat
    FEpsilon := cMax(FEpsilon div ALPHA, 1);
    if NeedUpdate then
      PriceUpdate;
    InitPhase;
    NeedUpdate := FQueue.NonEmpty;
    while FQueue.TryDequeue(Node) do
      begin
        FInQueue[IndexOf(Node)] := False;
        Discharge(Node{%H-});
      end;
  until FEpsilon = 1;
end;

function TGIntWeightDiGraph.TCsMcfHelper.GetTotalCost: TCost;
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
            Result += CurrArc^.Reverse^.ResCap * (CurrArc^.Cost div FScaleFactor);
          Inc(CurrArc);
        end;
    end;
end;

function TGIntWeightDiGraph.TCsMcfHelper.CreateArcFlows(out aTotalCost: TCost): TEdgeArray;
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
              aTotalCost += w * (CurrArc^.Cost div FScaleFactor);
              Result[J] := TWeightEdge.Create(I, Dst, w);
              Inc(J);
            end;
          Inc(CurrArc);
        end;
    end;
end;

function TGIntWeightDiGraph.TCsMcfHelper.GetMinCostFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight; var aCosts: TEdgeCostMap; out aTotalCost: TCost): TWeight;
begin
  if aReqFlow <= 0 then
    begin
      Result := 0;
      raise EGraphError.Create(SEMethodNotApplicable);
    end;
  CreateResidualNet(aGraph, aSource, aSink, aReqFlow, aCosts);
  if not FindInitSolution then
    exit(0);
  Execute;
  Result := FResultFlow;
  aTotalCost := GetTotalCost;
end;

function TGIntWeightDiGraph.TCsMcfHelper.GetMinCostFlow(aGraph: TGIntWeightDiGraph; aSource, aSink: SizeInt;
  aReqFlow: TWeight; var aCosts: TEdgeCostMap; out aTotalCost: TCost; out a: TEdgeArray): TWeight;
begin
  if aReqFlow <= 0 then
    begin
      Result := 0;
      raise EGraphError.Create(SEMethodNotApplicable);
    end;
  CreateResidualNet(aGraph, aSource, aSink, aReqFlow, aCosts);
  if not FindInitSolution then
    exit(0);
  Execute;
  Result := FResultFlow;
  a := CreateArcFlows(aTotalCost);
end;


