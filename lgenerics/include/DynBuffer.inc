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

function TGCustomArrayBuffer.GetCount: SizeInt;
begin
  Result := FCount;
end;

function TGCustomArrayBuffer.GetCapacity: SizeInt;
begin
  Result := System.Length(FItems);
end;

procedure TGCustomArrayBuffer.CheckEmpty;
begin
  if ElemCount = 0 then
    AccessEmptyError;
end;

procedure TGCustomArrayBuffer.DoClear;
begin
  FItems := nil;
  FCount := 0;
end;

function TGCustomArrayBuffer.IndexInRange(aIndex: SizeInt): Boolean;
begin
  Result := (aIndex >= 0) and (aIndex < ElemCount);
end;

function TGCustomArrayBuffer.IndexInInsertRange(aIndex: SizeInt): Boolean;
begin
  Result := (aIndex >= 0) and (aIndex <= ElemCount);
end;

procedure TGCustomArrayBuffer.CheckIndexRange(aIndex: SizeInt);
begin
  if not IndexInRange(aIndex) then
    IndexOutOfBoundError(aIndex);
end;

procedure TGCustomArrayBuffer.CheckInsertIndexRange(aIndex: SizeInt);
begin
  if not IndexInInsertRange(aIndex) then
    IndexOutOfBoundError(aIndex);
end;

destructor TGCustomArrayBuffer.Destroy;
begin
  DoClear;
  inherited;
end;

{ TGCustomArrayContainer.TEnumerator }

function TGCustomArrayContainer.TEnumerator.GetCurrent: T;
begin
  Result := FItems[FCurrIndex];
end;

constructor TGCustomArrayContainer.TEnumerator.Create(c: TGCustomArrayContainer);
begin
  inherited Create(c);
  FItems := c.FItems;
  FLast := Pred(c.ElemCount);
  FCurrIndex := -1;
end;

function TGCustomArrayContainer.TEnumerator.MoveNext: Boolean;
begin
  Result := FCurrIndex < FLast;
  FCurrIndex += Ord(Result);
end;

procedure TGCustomArrayContainer.TEnumerator.Reset;
begin
  FCurrIndex := -1;
end;

{ TGCustomArrayContainer.TReverseEnumerator }

function TGCustomArrayContainer.TReverseEnumerator.GetCurrent: T;
begin
  Result := FItems[FCurrIndex];
end;

constructor TGCustomArrayContainer.TReverseEnumerator.Create(c: TGCustomArrayContainer);
begin
  inherited Create(c);
  FItems := c.FItems;
  FCount := c.ElemCount;
  FCurrIndex := FCount;
end;

function TGCustomArrayContainer.TReverseEnumerator.MoveNext: Boolean;
begin
  Result := FCurrIndex > 0;
  FCurrIndex -= Ord(Result);
end;

procedure TGCustomArrayContainer.TReverseEnumerator.Reset;
begin
  FCurrIndex := FCount;
end;

{ TGCustomArrayContainer }

procedure TGCustomArrayContainer.Expand(aValue: SizeInt);
begin
  //there aValue > Capacity
  if aValue <= DEFAULT_CONTAINER_CAPACITY then
    System.SetLength(FItems, DEFAULT_CONTAINER_CAPACITY)
  else
    if aValue <= MAX_CONTAINER_SIZE div SizeOf(T) then
      begin
        aValue := Math.Min(MAX_CONTAINER_SIZE div SizeOf(T), LGUtils.RoundUpTwoPower(aValue));
        System.SetLength(FItems, aValue);
      end
    else
      CapacityExceedError(aValue);
end;

procedure TGCustomArrayContainer.ItemAdding;
begin
  if ElemCount = System.Length(FItems) then
    Expand(Succ(ElemCount));
end;

function TGCustomArrayContainer.Append(constref aValue: T): SizeInt;
begin
  Result := ElemCount;
  ItemAdding;
  FItems[Result] := aValue;
  Inc(FCount);
end;

function TGCustomArrayContainer.AppendArray(constref a: array of T): SizeInt;
begin
  Result := System.Length(a);
  if Result > 0 then
    begin
      DoEnsureCapacity(ElemCount + Result);
      TCopyArrayHelper.CopyItems(@a[0], @FItems[ElemCount], Result);
      FCount += Result;
    end;
end;

function TGCustomArrayContainer.AppendEnum(e: IEnumerable): SizeInt;
var
  v: T;
begin
  Result := 0;
  for v in e do
    begin
      ItemAdding;
      FItems[ElemCount] := v;
      Inc(FCount);
      Inc(Result);
    end;
end;

function TGCustomArrayContainer.AppendContainer(aContainer: TSpecContainer): SizeInt;
begin
  Result := aContainer.Count;
  if Result > 0 then
    begin
      DoEnsureCapacity(ElemCount + Result);
      aContainer.CopyItems(@FItems[ElemCount]);
      FCount += Result;
    end;
end;

function TGCustomArrayContainer.AppendEnumerable(e: IEnumerable): SizeInt;
var
  o: TObject;
begin
  o := e._GetRef;
  if o is TSpecContainer then
    Result := AppendContainer(TSpecContainer(o))
  else
    Result := AppendEnum(e)
end;

function TGCustomArrayContainer.GetReverse: IEnumerable;
begin
  Result := TReverseEnumerator.Create(Self);
end;

function TGCustomArrayContainer.DoGetEnumerator: TSpecEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TGCustomArrayContainer.DoTrimToFit;
begin
  if Capacity > ElemCount then
    System.SetLength(FItems, ElemCount);
end;

procedure TGCustomArrayContainer.DoEnsureCapacity(aValue: SizeInt);
begin
  if aValue > Capacity then
    Expand(aValue);
end;

procedure TGCustomArrayContainer.CopyItems(aBuffer: PItem);
begin
  if ElemCount > 0 then
    TCopyArrayHelper.CopyItems(@FItems[0], aBuffer, ElemCount);
end;

constructor TGCustomArrayContainer.Create;
begin
  System.SetLength(FItems, DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGCustomArrayContainer.Create(aCapacity: SizeInt);
begin
  if aCapacity <= MAX_CONTAINER_SIZE div SizeOf(T) then
    begin
      if aCapacity < 0 then
        aCapacity := 0;
      System.SetLength(FItems, aCapacity);
    end
  else
    CapacityExceedError(aCapacity);
end;

constructor TGCustomArrayContainer.Create(constref a: array of T);
begin
  Create(Math.Max(System.Length(a), DEFAULT_CONTAINER_CAPACITY));
  AppendArray(a);
end;

constructor TGCustomArrayContainer.Create(e: IEnumerable);
var
  o: TObject;
begin
  Create;
  o := e._GetRef;
  if o is TSpecContainer then
    AppendContainer(TSpecContainer(o))
  else
    AppendEnum(e);
end;

function TGCustomArrayContainer.Reverse: IEnumerable;
begin
  BeginIteration;
  Result := GetReverse;
end;

function TGCustomArrayContainer.ToArray: TArray;
begin
  Result := System.Copy(FItems, 0, ElemCount);
end;

{ TGLiteDynBuffer.TEnumerator }

function TGLiteDynBuffer.TEnumerator.GetCurrent: T;
begin
  Result := FCurrent^;
end;

function TGLiteDynBuffer.TEnumerator.MoveNext: Boolean;
begin
  if FCurrent < FLast then
    begin
      Inc(FCurrent);
      exit(True);
    end;
  Result := False;
end;

{ TGLiteDynBuffer.TMutableEnumerator }

function TGLiteDynBuffer.TMutableEnumerator.MoveNext: Boolean;
begin
  if FCurrent < FLast then
    begin
      Inc(FCurrent);
      exit(True);
    end;
  Result := False;
end;

{ TGLiteDynBuffer.TReverseEnumerator }

function TGLiteDynBuffer.TReverseEnumerator.GetCurrent: T;
begin
  Result := FCurrent^;
end;

function TGLiteDynBuffer.TReverseEnumerator.MoveNext: Boolean;
begin
  if FCurrent > FFirst then
    begin
      Dec(FCurrent);
      exit(True);
    end;
  Result := False;
end;

{ TGLiteDynBuffer.TMutables }

function TGLiteDynBuffer.TMutables.GetEnumerator: TMutableEnumerator;
begin
  Result := FBuffer^.GetMutableEnumerator;
end;

{ TGLiteDynBuffer.TReverse }

function TGLiteDynBuffer.TReverse.GetEnumerator: TReverseEnumerator;
begin
  Result := FBuffer^.GetReverseEnumerator;
end;

{ TGLiteDynBuffer }

function TGLiteDynBuffer.GetCapacity: SizeInt;
begin
  Result := System.Length(FItems);
end;

procedure TGLiteDynBuffer.Expand(aValue: SizeInt);
begin
  //there aValue > Capacity
  if aValue <= DEFAULT_CONTAINER_CAPACITY then
    System.SetLength(FItems, DEFAULT_CONTAINER_CAPACITY)
  else
    if aValue <= MAX_CONTAINER_SIZE div SizeOf(T) then
      begin
        aValue := Math.Min(LGUtils.RoundUpTwoPower(aValue), MAX_CONTAINER_SIZE div SizeOf(T));
        System.SetLength(FItems, aValue);
      end
    else
      raise ELGCapacityExceed.CreateFmt(SECapacityExceedFmt, [aValue]);
end;

procedure TGLiteDynBuffer.AccessEmptyError;
begin
  raise ELGAccessEmpty.Create(SECantAccessEmpty);
end;

class operator TGLiteDynBuffer.Initialize(var b: TGLiteDynBuffer);
begin
  b.FCount := 0;
end;

class operator TGLiteDynBuffer.Copy(constref aSrc: TGLiteDynBuffer; var aDst: TGLiteDynBuffer);
begin
  if @aDst = @aSrc then
    exit;
  aDst.FItems := System.Copy(aSrc.FItems, 0, aSrc.Capacity);
  aDst.FCount := aSrc.Count;
end;

procedure TGLiteDynBuffer.CheckEmpty;
begin
  if Count = 0 then
    AccessEmptyError;
end;

procedure TGLiteDynBuffer.ItemAdding;
begin
  if FCount = GetCapacity then
    Expand(Succ(FCount));
end;

procedure TGLiteDynBuffer.Clear;
begin
  FItems := nil;
  FCount := 0;
end;

procedure TGLiteDynBuffer.EnsureCapacity(aValue: SizeInt);
begin
  if aValue > GetCapacity then
    Expand(aValue);
end;

procedure TGLiteDynBuffer.TrimToFit;
begin
  System.SetLength(FItems, FCount);
end;

function TGLiteDynBuffer.GetEnumerator: TEnumerator;
begin
  if FCount > 0 then
    begin
      Result.FCurrent := PItem(FItems) - 1;
      Result.FLast := PItem(FItems) + Pred(FCount);
      Result.FItems := FItems;
      exit;
    end;
  Result.FCurrent := nil;
  Result.FLast := nil;
end;

function TGLiteDynBuffer.GetMutableEnumerator: TMutableEnumerator;
begin
  if FCount > 0 then
    begin
      Result.FCurrent := PItem(FItems) - 1;
      Result.FLast := PItem(FItems) + Pred(FCount);
      Result.FItems := FItems;
      exit;
    end;
  Result.FCurrent := nil;
  Result.FLast := nil;
end;

function TGLiteDynBuffer.GetReverseEnumerator: TReverseEnumerator;
begin
  if FCount > 0 then
    begin
      Result.FCurrent := PItem(FItems) + FCount;
      Result.FFirst := PItem(FItems);
      Result.FItems := FItems;
      exit;
    end;
  Result.FCurrent := nil;
  Result.FFirst := nil;
end;

function TGLiteDynBuffer.Mutables: TMutables;
begin
  Result.FBuffer := @Self;
end;

function TGLiteDynBuffer.Reverse: TReverse;
begin
  Result.FBuffer := @Self;
end;

function TGLiteDynBuffer.ToArray: TArray;
begin
  Result := System.Copy(FItems, 0, FCount);
end;

function TGLiteDynBuffer.PushLast(constref aValue: T): SizeInt;
begin
  Result := FCount;
  ItemAdding;
  Inc(FCount);
  FItems[Result] := aValue;
end;

{ TGCustomRingArrayBuffer.TEnumerator }

function TGCustomRingArrayBuffer.TEnumerator.GetCurrent: T;
begin
  Result := FItems[FCurrIndex];
end;

constructor TGCustomRingArrayBuffer.TEnumerator.Create(c: TGCustomRingArrayBuffer);
begin
  inherited Create(c);
  FItems := c.FItems;
  FCapacity := c.Capacity;
  FCount := c.ElemCount;
  FHead := c.Head;
  Reset;
end;

function TGCustomRingArrayBuffer.TEnumerator.MoveNext: Boolean;
begin
  Result := FRest > 0;
  FCurrIndex += Ord(Result);
  FRest -= Ord(Result);
  if FCurrIndex = FCapacity then
    FCurrIndex := 0;
end;

procedure TGCustomRingArrayBuffer.TEnumerator.Reset;
begin
  if FHead = 0 then
    FCurrIndex := Pred(FCapacity)
  else
    FCurrIndex := Pred(FHead);
  FRest := FCount;
end;

{ TGCustomRingArrayBuffer.TReverseEnum }

function TGCustomRingArrayBuffer.TReverseEnum.GetCurrent: T;
begin
  Result := FItems[FCurrIndex];
end;

constructor TGCustomRingArrayBuffer.TReverseEnum.Create(c: TGCustomRingArrayBuffer);
begin
  inherited Create(c);
  FItems := c.FItems;
  FCount := c.ElemCount;
  FHead := c.Head;
  Reset;
end;

function TGCustomRingArrayBuffer.TReverseEnum.MoveNext: Boolean;
var
  c: SizeInt;
begin
  Result := FRest > 0;
  if Result then
    begin
      c := System.Length(FItems);
      Dec(FCurrIndex);
      Dec(FRest);
      if FCurrIndex < 0 then
        FCurrIndex += c;
    end;
end;

procedure TGCustomRingArrayBuffer.TReverseEnum.Reset;
var
  c: SizeInt;
begin
  c := System.Length(FItems);
  FCurrIndex := FHead + FCount;
  FRest := FCount;
  if FCurrIndex >= c then
    FCurrIndex -= c;
end;

{ TGCustomRingArrayContainer }

procedure TGCustomRingArrayBuffer.Expand(aValue: SizeInt);
begin
  if aValue > System.Length(FItems) then
    begin
      if aValue <= MAX_CONTAINER_SIZE div SizeOf(T) then
        begin
          if aValue <= DEFAULT_CONTAINER_CAPACITY then
            aValue :=  DEFAULT_CONTAINER_CAPACITY
          else
            aValue := Math.Min(MAX_CONTAINER_SIZE div SizeOf(T), LGUtils.RoundUpTwoPower(aValue));
          if (ElemCount > 0) and (FHead > 0) then
            Grow(aValue)
          else
            System.SetLength(FItems, aValue);
        end
      else
        CapacityExceedError(aValue);
    end;
end;

procedure TGCustomRingArrayBuffer.Grow(aValue: SizeInt);
var
  OldCapacity, TailPos, ToMove, ExpandCount: SizeInt;
begin
  OldCapacity := System.Length(FItems); //there Count > 0 and FHead > 0
  System.SetLength(FItems, aValue);
  ExpandCount := aValue - OldCapacity;
  //so as not to depend on expand policy
  TailPos := Head + ElemCount;
  if TailPos >= OldCapacity then
    TailPos -= OldCapacity;
  if TailPos <= Head then   // else nothing to move
    begin
      if (TailPos < Succ(OldCapacity - Head)) and (TailPos <= ExpandCount) then
        begin // move tail to tail :)
          System.Move(FItems[0], FItems[OldCapacity], SizeOf(T) * TailPos);
          System.FillChar(FItems[0], SizeOf(T) * TailPos, 0);
        end
      else
        begin // move head to tail
          ToMove := OldCapacity - Head;
          System.Move(FItems[Head], FItems[Head + ExpandCount], SizeOf(T) * ToMove);
          System.FillChar(FItems[Head], SizeOf(T) * Math.Min(ToMove, ExpandCount), 0);
          FHead += ExpandCount;
        end;
    end;
end;

procedure TGCustomRingArrayBuffer.Shrink;
var
  OldCapacity, ElCount, TailPos, ToMove: SizeInt;
begin
  ElCount := ElemCount;
  OldCapacity := System.Length(FItems);  //there Count > 0 and FHead > 0
  TailPos := Head + ElCount;
  if TailPos >= OldCapacity then
    TailPos -= OldCapacity;
  if (TailPos > Head) then // consecutive
    begin
      System.Move(FItems[Head], FItems[0], SizeOf(T) * ElCount);
      if Head >= ElCount then
        FillChar(FItems[Head], SizeOf(T) * ElCount, 0)
      else
        FillChar(FItems[ElCount], SizeOf(T) * Head, 0);
      FHead := 0;
    end
  else
    if TailPos < FHead then // else nothing to move
      begin
        ToMove := OldCapacity - Head;
        System.Move(FItems[Head], FItems[TailPos], SizeOf(T) * ToMove);
        if Head >= ElCount then
          FillChar(FItems[Head], SizeOf(T) * ToMove, 0)
        else
          FillChar(FItems[ElCount], SizeOf(T) * (Head - TailPos), 0);
        FHead := TailPos;
      end;
  System.SetLength(FItems, ElemCount);
end;

procedure TGCustomRingArrayBuffer.ItemAdding;
begin
  if ElemCount = System.Length(FItems) then
    Expand(Succ(ElemCount));
end;

procedure TGCustomRingArrayBuffer.Append(constref aValue: T);
var
  TailPos, c: SizeInt;
begin
  ItemAdding;
  TailPos := Head + ElemCount;
  c := System.Length(FItems);
  if TailPos >= c then
    TailPos -= c;
  FItems[TailPos] := aValue;
  Inc(FCount);
end;

function TGCustomRingArrayBuffer.AppendArray(constref a: array of T): SizeInt;
var
  TailPos, I, c: SizeInt;
begin
  Result := System.Length(a);
  if Result > 0 then
    begin
      DoEnsureCapacity(ElemCount + Result);
      c := System.Length(FItems);
      TailPos := Head + ElemCount;
      if TailPos >= c then
        TailPos -= c;
      for I := 0 to System.High(a) do
        begin
          FItems[TailPos] := a[I];
          Inc(TailPos);
          if TailPos = c then
            TailPos := 0;
        end;
      FCount += Result;
    end;
end;

function TGCustomRingArrayBuffer.AppendContainer(aContainer: TSpecContainer): SizeInt;
var
  TailPos, c: SizeInt;
  v: T;
begin
  if aContainer <> Self then
    begin
      Result := aContainer.Count;
      if Result > 0 then
        begin
          DoEnsureCapacity(ElemCount + Result);
          c := System.Length(FItems);
          TailPos := Head + ElemCount;
          if TailPos >= c then
            TailPos -= c;
          for v in aContainer do
            begin
              FItems[TailPos] := v;
              Inc(TailPos);
              if TailPos = c then
                TailPos := 0;
            end;
        end;
    end
  else
    Result := AppendArray(aContainer.ToArray);
end;

function TGCustomRingArrayBuffer.AppendEnum(e: IEnumerable): SizeInt;
var
  v: T;
  TailPos, c: SizeInt;
begin
  Result := ElemCount;
  for v in e do
    begin
      ItemAdding;
      c := System.Length(FItems);
      TailPos := Head + ElemCount;
      if TailPos >= c then
        TailPos -= c;
      FItems[TailPos] := v;
      Inc(FCount);
    end;
  Result := ElemCount - Result;
end;

function TGCustomRingArrayBuffer.AppendEnumerable(e: IEnumerable): SizeInt;
var
  o: TObject;
begin
  o := e._GetRef;
  if o is TSpecContainer then
    Result := AppendContainer(TSpecContainer(o))
  else
    Result := AppendEnum(e)
end;

function TGCustomRingArrayBuffer.ExtractHead: T;
begin
  Result := FItems[Head];
  FItems[Head] := Default(T);
  Inc(FHead);
  Dec(FCount);
  if Head = System.Length(FItems) then
    FHead := 0;
end;

function TGCustomRingArrayBuffer.DoGetEnumerator: TSpecEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TGCustomRingArrayBuffer.DoClear;
begin
  inherited;
  FHead := 0;
end;

procedure TGCustomRingArrayBuffer.DoTrimToFit;
begin
  if System.Length(FItems) > ElemCount then
    begin
      if (ElemCount > 0) and (Head > 0) then
        Shrink
      else
        System.SetLength(FItems, ElemCount);
    end;
end;

procedure TGCustomRingArrayBuffer.DoEnsureCapacity(aValue: SizeInt);
begin
  if aValue > System.Length(FItems) then
    Expand(aValue);
end;

procedure TGCustomRingArrayBuffer.CopyItems(aBuffer: PItem);
var
  I, h, c: SizeInt;
begin
  c := System.Length(FItems);
  h := Head;
  for I := 0 to Pred(ElemCount) do
    begin
      aBuffer[I] := FItems[h];
      Inc(h);
      if h = c then
        h := 0;
    end;
end;

constructor TGCustomRingArrayBuffer.Create;
begin
  System.SetLength(FItems, DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGCustomRingArrayBuffer.Create(aCapacity: SizeInt);
begin
  if aCapacity <= MAX_CONTAINER_SIZE div SizeOf(T) then
    begin
      if aCapacity < 0 then
        aCapacity := 0;
      System.SetLength(FItems, aCapacity);
    end
  else
    CapacityExceedError(aCapacity);
end;

constructor TGCustomRingArrayBuffer.Create(constref A: array of T);
begin
  Create(Math.Max(System.Length(A), DEFAULT_CONTAINER_CAPACITY));
  AppendArray(A);
end;

constructor TGCustomRingArrayBuffer.Create(e: IEnumerable);
var
  o: TObject;
begin
  Create;
  o := e._GetRef;
  if o is TSpecContainer then
    AppendContainer(TSpecContainer(o))
  else
    AppendEnum(e);
end;

function TGCustomRingArrayBuffer.Reverse: IEnumerable;
begin
  Result := TReverseEnum.Create(Self);
end;

{ TGLiteRingDynBuffer.TEnumerator }

function TGLiteRingDynBuffer.TEnumerator.GetCurrent: T;
begin
  Result := FItems[FCurrIndex];
end;

function TGLiteRingDynBuffer.TEnumerator.MoveNext: Boolean;
begin
  if FRest > 0 then
    begin
      Inc(FCurrIndex);
      Dec(FRest);
      if FCurrIndex = FCapacity then
        FCurrIndex := 0;
      exit(True);
    end;
  Result := False;
end;

{ TGLiteRingDynBuffer.TMutableEnumerator }

function TGLiteRingDynBuffer.TMutableEnumerator.GetCurrent: PItem;
begin
  Result := @FItems[FCurrIndex];
end;

function TGLiteRingDynBuffer.TMutableEnumerator.MoveNext: Boolean;
begin
  if FRest > 0 then
    begin
      Inc(FCurrIndex);
      Dec(FRest);
      if FCurrIndex = FCapacity then
        FCurrIndex := 0;
      exit(True);
    end;
  Result := False;
end;

{ TGLiteRingDynBuffer.TMutables }

function TGLiteRingDynBuffer.TMutables.GetEnumerator: TMutableEnumerator;
begin
  Result := FBuffer{%H-}^.GetMutableEnumerator;
end;

{ TGLiteRingDynBuffer.TReverseEnumerator }

function TGLiteRingDynBuffer.TReverseEnumerator.GetCurrent: T;
begin
  Result := FItems[FCurrIndex];
end;

function TGLiteRingDynBuffer.TReverseEnumerator.MoveNext: Boolean;
begin
  if FRest > 0 then
    begin
      Dec(FCurrIndex);
      Dec(FRest);
      if FCurrIndex < 0 then
        FCurrIndex += FCapacity;
      exit(True);
    end;
  Result := False;
end;

{ TGLiteRingDynBuffer.TReverse }

function TGLiteRingDynBuffer.TReverse.GetEnumerator: TReverseEnumerator;
begin
  Result := FBuffer^.GetReverseEnumerator;
end;

{ TGLiteRingDynBuffer }

function TGLiteRingDynBuffer.GetCapacity: SizeInt;
begin
  Result := System.Length(FItems);
end;

procedure TGLiteRingDynBuffer.Expand(aValue: SizeInt);
begin
  //there aValue > Capacity
  if aValue <= MAX_CONTAINER_SIZE div SizeOf(T) then
    begin
      if aValue <= DEFAULT_CONTAINER_CAPACITY then
        aValue :=  DEFAULT_CONTAINER_CAPACITY
      else
        aValue := Math.Min(LGUtils.RoundUpTwoPower(aValue), MAX_CONTAINER_SIZE div SizeOf(T));
      if (Count > 0) and (FHead > 0) then
        Grow(aValue)
      else
        System.SetLength(FItems, aValue);
    end
  else
    CapacityExceedError(aValue);
end;

procedure TGLiteRingDynBuffer.Grow(aValue: SizeInt);
var
  OldCapacity, TailPos, ToMove, ExpandCount: SizeInt;
begin
  OldCapacity := Capacity; //there Count > 0 and FHead > 0
  System.SetLength(FItems, aValue);
  ExpandCount := aValue - OldCapacity;
  //so as not to depend on expand policy
  TailPos := FHead + Count;
  if TailPos >= OldCapacity then
    TailPos -= OldCapacity;
  if TailPos <= FHead then   // else nothing to move
    begin
      if (TailPos < Succ(OldCapacity - FHead)) and (TailPos <= ExpandCount) then
        begin // move tail to tail :)
          System.Move(FItems[0], FItems[OldCapacity], SizeOf(T) * TailPos);
          System.FillChar(FItems[0], SizeOf(T) * TailPos, 0);
        end
      else
        begin // move head to tail
          ToMove := OldCapacity - FHead;
          System.Move(FItems[FHead], FItems[FHead + ExpandCount], SizeOf(T) * ToMove);
          System.FillChar(FItems[FHead], SizeOf(T) * Math.Min(ToMove, ExpandCount), 0);
          FHead += ExpandCount;
        end;
    end;
end;

procedure TGLiteRingDynBuffer.Shrink;
var
  OldCapacity, TailPos, ToMove: SizeInt;
begin
  OldCapacity := Capacity;  //there Count > 0 and Head > 0
  TailPos := FHead + Count;
  if TailPos >= OldCapacity then
    TailPos -= OldCapacity;
  if (TailPos > FHead) then // consecutive
    begin
      System.Move(FItems[FHead], FItems[0], SizeOf(T) * Count);
      if FHead >= Count then
        FillChar(FItems[FHead], SizeOf(T) * Count, 0)
      else
        FillChar(FItems[Count], SizeOf(T) * FHead, 0);
      FHead := 0;
    end
  else
    if TailPos < FHead then // else nothing to move
      begin
        ToMove := OldCapacity - FHead;
        System.Move(FItems[FHead], FItems[TailPos], SizeOf(T) * ToMove);
        if FHead >= Count then
          FillChar(FItems[FHead], SizeOf(T) * ToMove, 0)
        else
          FillChar(FItems[Count], SizeOf(T) * (FHead - TailPos), 0);
        FHead := TailPos;
      end;
  System.SetLength(FItems, Count);
end;

function TGLiteRingDynBuffer.TailIndex: SizeInt;
begin
  Result := InternalIndex(Pred(Count));
end;

procedure TGLiteRingDynBuffer.ItemAdding;
begin
  if Count = Capacity then
    Expand(Succ(Count));
end;

function TGLiteRingDynBuffer.PopHead: T;
begin
  Result := FItems[FHead];
  FItems[FHead] := Default(T);
  Inc(FHead);
  Dec(FCount);
  if FHead = Capacity then
    FHead := 0;
end;

function TGLiteRingDynBuffer.PopTail: T;
var
  TailPos: SizeInt;
begin
  TailPos := TailIndex;
  Dec(FCount);
  Result := FItems[TailPos];
  FItems[TailPos] := Default(T);
end;

procedure TGLiteRingDynBuffer.CapacityExceedError(aValue: SizeInt);
begin
  raise ELGCapacityExceed.CreateFmt(SECapacityExceedFmt, [aValue]);
end;

procedure TGLiteRingDynBuffer.AccessEmptyError;
begin
  raise ELGAccessEmpty.Create(SECantAccessEmpty);
end;

procedure TGLiteRingDynBuffer.CheckEmpty;
begin
  if Count = 0 then
    AccessEmptyError;
end;

class operator TGLiteRingDynBuffer.Initialize(var b: TGLiteRingDynBuffer);
begin
  b.FCount := 0;
  b.FHead := 0;
end;

class operator TGLiteRingDynBuffer.Copy(constref aSrc: TGLiteRingDynBuffer; var aDst: TGLiteRingDynBuffer);
begin
  if @aDst = @aSrc then
    exit;
  aDst.FItems := System.Copy(aSrc.FItems, 0, aSrc.Capacity);
  aDst.FCount := aSrc.Count;
  aDst.FHead := aSrc.FHead;
end;

function TGLiteRingDynBuffer.InternalIndex(aIndex: SizeInt): SizeInt;
begin
  Result := FHead + aIndex;
  if Result >= Capacity then
    Result -= Capacity;
end;

function TGLiteRingDynBuffer.FastGetItem(aIndex: SizeInt): T;
begin
  Result := FItems[InternalIndex(aIndex)];
end;

procedure TGLiteRingDynBuffer.FastSetItem(aIndex: SizeInt; constref aValue: T);
begin
  FItems[InternalIndex(aIndex)] := aValue;
end;

procedure TGLiteRingDynBuffer.Clear;
begin
  FItems := nil;
  FCount := 0;
  FHead := 0;
end;

procedure TGLiteRingDynBuffer.MakeEmpty;
var
  I, h, c: SizeInt;
begin
  if Count > 0 then
    begin
      c := Capacity;
      h := FHead;
      for I := 0 to Pred(Count) do
        begin
          FItems[h] := Default(T);
          Inc(h);
          if h = c then
            h := 0;
        end;
    end;
  FCount := 0;
  FHead := 0;
end;

procedure TGLiteRingDynBuffer.EnsureCapacity(aValue: SizeInt);
begin
  if aValue > Capacity then
    Expand(aValue);
end;

procedure TGLiteRingDynBuffer.TrimToFit;
begin
  if Capacity > Count then
    begin
      if (Count > 0) and (FHead > 0) then
        Shrink
      else
        System.SetLength(FItems, Count);
    end;
end;

function TGLiteRingDynBuffer.GetEnumerator: TEnumerator;
begin
  Result.FItems := FItems;
  Result.FCapacity := Capacity;
  Result.FCount := FCount;
  Result.FHead := FHead;
  if FHead = 0 then
    Result.FCurrIndex := Pred(Capacity)
  else
    Result.FCurrIndex := Pred(FHead);
  Result.FRest := FCount;
end;

function TGLiteRingDynBuffer.GetMutableEnumerator: TMutableEnumerator;
begin
  Result.FItems := FItems;
  Result.FCapacity := Capacity;
  Result.FCount := FCount;
  Result.FHead := FHead;
  if FHead = 0 then
    Result.FCurrIndex := Pred(Capacity)
  else
    Result.FCurrIndex := Pred(FHead);
  Result.FRest := FCount;
end;

function TGLiteRingDynBuffer.GetReverseEnumerator: TReverseEnumerator;
begin
  Result.FItems := FItems;
  Result.FCapacity := System.Length(FItems);
  Result.FCount := FCount;
  Result.FHead := FHead;
  Result.FCurrIndex := FHead + FCount;
  Result.FRest := FCount;
  if Result.FCurrIndex >= Capacity then
    Result.FCurrIndex -= Capacity;
end;

function TGLiteRingDynBuffer.Mutables: TMutables;
begin
  Result.FBuffer := @Self;
end;

function TGLiteRingDynBuffer.Reverse: TReverse;
begin
  Result.FBuffer := @Self;
end;

function TGLiteRingDynBuffer.ToArray: TArray;
var
  I, h, c: SizeInt;
begin
  System.SetLength(Result, Count);
  if Count > 0 then
    begin
      c := Capacity;
      h := FHead;
      for I := 0 to Pred(Count) do
        begin
          Result[I] := FItems[h];
          Inc(h);
          if h = c then
            h := 0;
        end;
    end;
end;

procedure TGLiteRingDynBuffer.PushLast(constref aValue: T);
var
  TailPos, c: SizeInt;
begin
  ItemAdding;
  TailPos := FHead + Count;
  c := Capacity;
  if TailPos >= c then
    TailPos -= c;
  FItems[TailPos] := aValue;
  Inc(FCount);
end;

procedure TGLiteRingDynBuffer.PushFirst(constref aValue: T);
begin
  ItemAdding;
  Dec(FHead);
  if FHead < 0 then
    FHead += Capacity;
  Inc(FCount);
  FItems[FHead] := aValue;
end;

function TGLiteRingDynBuffer.PopFirst: T;
begin
  CheckEmpty;
  Result := PopHead;
end;

function TGLiteRingDynBuffer.TryPopFirst(out aValue: T): Boolean;
begin
  if Count > 0 then
    begin
      aValue := PopHead;
      exit(True);
    end;
  Result := False;
end;

function TGLiteRingDynBuffer.PopLast: T;
begin
  CheckEmpty;
  Result := PopTail;
end;

function TGLiteRingDynBuffer.TryPopLast(out aValue: T): Boolean;
begin
  if Count > 0 then
    begin
      aValue := PopTail;
      exit(True);
    end;
  Result := False;
end;

function TGLiteRingDynBuffer.PeekFirst: T;
begin
  CheckEmpty;
  Result := FItems[FHead];
end;

function TGLiteRingDynBuffer.PeekFirstItem: PItem;
begin
  CheckEmpty;
  Result := @FItems[FHead];
end;

function TGLiteRingDynBuffer.TryPeekFirst(out aValue: T): Boolean;
begin
  if Count > 0 then
    begin
      aValue := FItems[FHead];
      exit(True);
    end;
  Result := False;
end;

function TGLiteRingDynBuffer.TryPeekFirstItem(out aValue: PItem): Boolean;
begin
  if Count > 0 then
    begin
      aValue := @FItems[FHead];
      exit(True);
    end;
  Result := False;
end;

function TGLiteRingDynBuffer.PeekLast: T;
begin
  CheckEmpty;
  Result := FItems[TailIndex];
end;

function TGLiteRingDynBuffer.TryPeekLast(out aValue: T): Boolean;
begin
  if Count > 0 then
    begin
      aValue := FItems[TailIndex];
      exit(True);
    end;
  Result := False;
end;

function TGLiteRingDynBuffer.PeekLastItem: PItem;
begin
  CheckEmpty;
  Result := @FItems[TailIndex];
end;

function TGLiteRingDynBuffer.TryPeekLastItem(out aValue: PItem): Boolean;
begin
  if Count > 0 then
    begin
      aValue := @FItems[TailIndex];
      exit(True);
    end;
  Result := False;
end;



