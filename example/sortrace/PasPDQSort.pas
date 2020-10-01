unit PasPDQSort;

{$Mode Delphi}
{$PointerMath On}
{$COperators On}
{$TypedAddress On}
{$ImplicitExceptions Off}

interface

const
  PARTIAL_INSERTION_SORT_LIMIT = 8;
  INSERTION_SORT_THRESHOLD = 24;
  BLOCK_SIZE = 64;
  CACHE_LINE_SIZE = 64;
  NINTHER_THRESHOLD = 128;

type
  TPair<K, V> = record
    Key: K;
    Value: V;
    class function Create(constref AKey: K; constref AValue: V): TPair<K, V>; static; inline;
  end;

  Util<T> = record
    class function PreInc(var P: T): T; static; inline;
    class function PostInc(var P: T): T; static; inline;
    class function PreDec(var P: T): T; static; inline;
    class function PostDec(var P: T): T; static; inline;
    class function Min(const A, B: T): T; static; inline;
  end;

  TPDQSorter<T> = record
  public type
    PT = ^T;
  private type
    PtrUtil = Util<PT>;
    ByteUtil = Util<Byte>;
    IntUtil = Util<PtrInt>;
  strict private class var
    OffsetsLStorage, OffsetsRStorage: array[0..Pred(BLOCK_SIZE + CACHE_LINE_SIZE)] of Byte;
  strict private
    class procedure Swap(const L, R: PT); static; inline;
    class procedure Sort3(const A, B, C: PT); static; inline;
    class procedure InsertionSort(const Start, Finish: PT); static; inline;
    class procedure UnguardedInsertionSort(const Start, Finish: PT); static; inline;
    class function PartialInsertionSort(const Start, Finish: PT): Boolean; static; inline;
    class procedure HeapSort(const Start, Finish: PT); static;
    class procedure SwapOffsets(const First, Last: PT;
                                const OffsetsL, OffsetsR: PByte;
                                const Num: PtrUInt;
                                const UseSwaps: Boolean); static;
    class function PartitionRightBranchless(const Start, Finish: PT): TPair<PT, Boolean>; static;
    class function PartitionLeft(const Start, Finish: PT): PT; static;
    class procedure PDQSortLoop(var Start: PT;
                                const Finish: PT;
                                const BadAllowed: PtrInt;
                                LeftMost: Boolean = True); static;
  public
    class procedure Sort(Start: PT; const Finish: PT); static; inline; overload;
    class procedure Sort(constref Arr: array of T); static; overload;
    class procedure Sort(constref Arr: TArray<T>); static; inline; overload;
  end;

implementation

class function TPair<K, V>.Create(constref AKey: K; constref AValue: V): TPair<K, V>;
begin
  with Result do begin
    Key := AKey;
    Value := AValue;
  end;
end;

class function Util<T>.PreInc(var P: T): T;
begin
  Inc(P);
  Result := P;
end;

class function Util<T>.PostInc(var P: T): T;
begin
  Result := P;
  Inc(P);
end;

class function Util<T>.PreDec(var P: T): T;
begin
  Dec(P);
  Result := P;
end;

class function Util<T>.PostDec(var P: T): T;
begin
  Result := P;
  Dec(P);
end;

class function Util<T>.Min(const A, B: T): T;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

class procedure TPDQSorter<T>.Swap(const L, R: PT);
var Tmp: T;
begin
  Tmp := L^;
  L^ := R^;
  R^ := Tmp;
end;

class procedure TPDQSorter<T>.Sort3(const A, B, C: PT);
begin
  if B^ < A^ then Swap(A, B);
  if C^ < B^ then Swap(B, C);
  if B^ < A^ then Swap(A, B);
end;

class procedure TPDQSorter<T>.InsertionSort(const Start, Finish: PT);
var
  Tmp: T;
  Cur, Sift, SiftL: PT;
begin
  if Start = Finish then Exit();
  Cur := Start + 1;
  while Cur <> Finish do begin
    Sift := Cur;
    SiftL := Cur - 1;
    if Sift^ < SiftL^ then begin
      Tmp := Sift^;
      repeat
        PtrUtil.PostDec(Sift)^ := SiftL^;
      until (Sift = Start) or not (Tmp < PtrUtil.PreDec(SiftL)^);
      Sift^ := Tmp;
    end;
    Cur += 1;
  end;
end;

class procedure TPDQSorter<T>.UnguardedInsertionSort(const Start, Finish: PT);
var
  Tmp: T;
  Cur, Sift, SiftL: PT;
begin
  if Start = Finish then Exit();
  Cur := Start + 1;
  while Cur <> Finish do begin
    Sift := Cur;
    SiftL := Cur - 1;
    if Sift^ < SiftL^ then begin
      Tmp := Sift^;
      repeat
        PtrUtil.PostDec(Sift)^ := SiftL^;
      until not (Tmp < PtrUtil.PreDec(SiftL)^);
      Sift^ := Tmp;
    end;
    Cur += 1;
  end;
end;

class function TPDQSorter<T>.PartialInsertionSort(const Start, Finish: PT): Boolean;
var
  Limit: PtrUInt;
  Tmp: T;
  Cur, Sift, SiftL: PT;
begin
  if Start = Finish then Exit(True);
  Limit := 0;
  Cur := Start + 1;
  while Cur <> Finish do begin
    if Limit > PARTIAL_INSERTION_SORT_LIMIT then Exit(False);
    Sift := Cur;
    SiftL := Cur - 1;
    if Sift^ < SiftL^ then begin
      Tmp := Sift^;
      repeat
        PtrUtil.PostDec(Sift)^ := SiftL^;
      until (Sift = Start) or not (Tmp < PtrUtil.PreDec(SiftL)^);
      Sift^ := Tmp;
      Limit += Cur - Sift;
    end;
    Cur += 1;
  end;
  Result := True;
end;

class procedure TPDQSorter<T>.HeapSort(const Start, Finish: PT);
var
  I, OuterLen: PtrUInt;

  procedure SiftDown(const Start2: PT; const InnerLen: PtrUInt; Node: PtrUInt); inline;
  var Left, Right, Greater: PtrUInt;
  begin
    while True do begin
      Left := 2 * Node + 1;
      Right := 2 * Node + 2;
      if (Right < InnerLen) and (Start2[Left] < Start2[Right]) then
        Greater := Right
      else
        Greater := Left;
      if (Greater >= InnerLen) or not (Start2[Node] < Start2[Greater]) then
        Break;
      Swap(Start2 + Node, Start2 + Greater);
      Node := Greater;
    end;
  end;

begin
  OuterLen := Finish - Start;
  for I := OuterLen div 2 - 1 downto 0 do
    SiftDown(Start, OuterLen, I);
  for I := OuterLen - 1 downto 1 do begin
    Swap(Start, Start + I);
    SiftDown(Start, I, 0);
  end;
end;

class procedure TPDQSorter<T>.SwapOffsets(const First, Last: PT;
                                          const OffsetsL, OffsetsR: PByte;
                                          const Num: PtrUInt;
                                          const UseSwaps: Boolean);
var
  Tmp: T;
  I: PtrUInt;
  L, R: PT;
begin
  if UseSwaps then begin
    I := 0;
    while I < Num do begin
      Tmp := (First + OffsetsL[I])^;
      (First + OffsetsL[I])^ := (Last - OffsetsR[I])^;
      (Last - OffsetsR[I])^ := Tmp;
      Inc(I);
    end;
  end else if Num > 0 then begin
    L := First + OffsetsL[0];
    R := Last - OffsetsR[0];
    Tmp := L^;
    L^ := R^;
    for I := 1 to Pred(Num) do begin
      L := First + OffsetsL[I];
      R^ := L^;
      R := Last - OffsetsR[I];
      L^ := R^;
    end;
    R^ := Tmp;
  end;
end;

class function TPDQSorter<T>.PartitionRightBranchless(const Start, Finish: PT): TPair<PT, Boolean>;
var
  I: Byte;
  AlreadyPartitioned: Boolean;
  Pivot, Tmp: T;
  First, Last, It, PivotPos: PT;
  OffsetsL, OffsetsR: PByte;
  Num, NumL, NumR, StartL, StartR, LSize, RSize, UnknownLeft: PtrInt;
begin
  Pivot := Start^;
  First := Start;
  Last := Finish;
  while PtrUtil.PreInc(First)^ < Pivot do
    ;
  if First - 1 = Start then begin
    while (First < Last) and not (PtrUtil.PreDec(Last)^ < Pivot) do
      ;
  end else
    while not (PtrUtil.PreDec(Last)^ < Pivot) do
      ;
  AlreadyPartitioned := First >= Last;
  if not AlreadyPartitioned then begin
    Tmp := First^;
    First^ := Last^;
    Last^ := Tmp;
    First += 1;
  end;
  OffsetsL := Align(@OffsetsLStorage[0], CACHE_LINE_SIZE);
  OffsetsR := Align(@OffsetsRStorage[0], CACHE_LINE_SIZE);
  NumL := 0;
  NumR := 0;
  StartL := 0;
  StartR := 0;
  while Last - First > 2 * BLOCK_SIZE do begin
    if NumL = 0 then begin
      StartL := 0;
      It := First;
      I := 0;
      while I < BLOCK_SIZE do begin
        (OffsetsL + NumL)^ := I;
        NumL += PtrInt(not (It^ < Pivot));
        (OffsetsL + NumL)^ := I + 1;
        NumL += PtrInt(not ((It + 1)^ < Pivot));
        (OffsetsL + NumL)^ := I + 2;
        NumL += PtrInt(not ((It + 2)^ < Pivot));
        (OffsetsL + NumL)^ := I + 3;
        NumL += PtrInt(not ((It + 3)^ < Pivot));
        (OffsetsL + NumL)^ := I + 4;
        NumL += PtrInt(not ((It + 4)^ < Pivot));
        (OffsetsL + NumL)^ := I + 5;
        NumL += PtrInt(not ((It + 5)^ < Pivot));
        (OffsetsL + NumL)^ := I + 6;
        NumL += PtrInt(not ((It + 6)^ < Pivot));
        (OffsetsL + NumL)^ := I + 7;
        NumL += PtrInt(not ((It + 7)^ < Pivot));
        I += 8;
        It += 8;
      end;
    end;
    if NumR = 0 then begin
      StartR := 0;
      It := Last;
      I := 0;
      while I < BLOCK_SIZE do begin
        (OffsetsR + NumR)^ := I + 1;
        NumR += PtrInt((It - 1)^ < Pivot);
        (OffsetsR + NumR)^ := I + 2;
        NumR += PtrInt((It - 2)^ < Pivot);
        (OffsetsR + NumR)^ := I + 3;
        NumR += PtrInt((It - 3)^ < Pivot);
        (OffsetsR + NumR)^ := I + 4;
        NumR += PtrInt((It - 4)^ < Pivot);
        (OffsetsR + NumR)^ := I + 5;
        NumR += PtrInt((It - 5)^ < Pivot);
        (OffsetsR + NumR)^ := I + 6;
        NumR += PtrInt((It - 6)^ < Pivot);
        (OffsetsR + NumR)^ := I + 7;
        NumR += PtrInt((It - 7)^ < Pivot);
        (OffsetsR + NumR)^ := I + 8;
        NumR += PtrInt((It - 8)^ < Pivot);
        I += 8;
        It -= 8;
      end;
    end;
    Num := IntUtil.Min(NumL, NumR);
    SwapOffsets(First, Last, OffsetsL + StartL, OffsetsR + StartR, Num, NumL = NumR);
    NumL -= Num;
    NumR -= Num;
    StartL += Num;
    StartR += Num;
    if NumL = 0 then
      First += BLOCK_SIZE;
    if NumR = 0 then
      Last -= BLOCK_SIZE;
  end;
  LSize := 0;
  RSize := 0;
  if (NumR or NumL) <> 0 then
    UnknownLeft := (Last - First) - BLOCK_SIZE
  else
    UnknownLeft := Last - First;
  if NumR <> 0 then begin
    LSize := UnknownLeft;
    RSize := BLOCK_SIZE;
  end else if NumL <> 0 then begin
    LSize := BLOCK_SIZE;
    RSize := UnknownLeft;
  end else begin
    LSize := UnknownLeft div 2;
    RSize := UnknownLeft - LSize;
  end;
  if (UnknownLeft <> 0) and (NumL = 0) then begin
    StartL := 0;
    It := First;
    I := 0;
    while I < LSize do begin
      (OffsetsL + NumL)^ := ByteUtil.PostInc(I);
      NumL += PtrInt(not (It^ < Pivot));
      It += 1;
    end;
  end;
  if (UnknownLeft <> 0) and (NumR = 0) then begin
    StartR := 0;
    It := Last;
    I := 0;
    while I < RSize do begin
      (OffsetsR + NumR)^ := ByteUtil.PreInc(I);
      NumR += PtrInt(PtrUtil.PreDec(It)^ < Pivot);
    end;
  end;
  Num := IntUtil.Min(NumL, NumR);
  SwapOffsets(First, Last, OffsetsL + StartL, OffsetsR + StartR, Num, NumL = NumR);
  NumL -= Num;
  NumR -= Num;
  StartL += Num;
  StartR += Num;
  if NumL = 0 then
    First += LSize;
  if NumR = 0 then
    Last -= RSize;
  if NumL <> 0 then begin
    OffsetsL += StartL;
    while NumL <> 0 do begin
      NumL -= 1;
      Swap(First + (OffsetsL + NumL)^, PtrUtil.PreDec(Last));
    end;
    First := Last;
  end;
  if NumR <> 0 then begin
    OffsetsR += StartR;
    while NumR <> 0 do begin
      NumR -= 1;
      Swap(Last - (OffsetsR + NumR)^, First);
      First += 1;
    end;
    Last := First;
  end;
  PivotPos := First - 1;
  Start^ := PivotPos^;
  PivotPos^ := Pivot;
  Result := TPair<PT, Boolean>.Create(PivotPos, AlreadyPartitioned);
end;

class function TPDQSorter<T>.PartitionLeft(const Start, Finish: PT): PT;
var
  Pivot: T;
  First, Last, PivotPos: PT;
begin
  Pivot := Start^;
  First := Start;
  Last := Finish;
  while Pivot < PtrUtil.PreDec(Last)^ do
    ;
  if Last + 1 = Finish then begin
    while (First < Last) and not (Pivot < PtrUtil.PreInc(First)^) do
      ;
  end else
    while not (Pivot < PtrUtil.PreInc(First)^) do
      ;
  while First < Last do begin
    Swap(First, Last);
    while Pivot < PtrUtil.PreDec(Last)^ do
      ;
    while not (Pivot < PtrUtil.PreInc(First)^) do
      ;
  end;
  PivotPos := Last;
  Start^ := PivotPos^;
  PivotPos^ := Pivot;
  Result := PivotPos;
end;

class procedure TPDQSorter<T>.PDQSortLoop(var Start: PT;
                                          const Finish: PT;
                                          const BadAllowed: PtrInt;
                                          LeftMost: Boolean);
var
  AlreadyPartitioned: Boolean;
  PivotPos, Dummy: PT;
  Size, S2, LSize, LSizeDiv, RSize, RSizeDiv: PtrInt;
  PartResult: TPair<PT, Boolean>;
begin
  while True do begin
    Size := Finish - Start;
    if Size < INSERTION_SORT_THRESHOLD then begin
      if LeftMost then
        InsertionSort(Start, Finish)
      else
        UnguardedInsertionSort(Start, Finish);
      Exit();
    end;
    S2 := Size div 2;
    if Size > NINTHER_THRESHOLD then begin
      Sort3(Start, Start + S2, Finish - 1);
      Sort3(Start + 1, Start + (S2 - 1), Finish - 2);
      Sort3(Start + 2, Start + (S2 + 1), Finish - 3);
      Sort3(Start + (S2 - 1), Start + S2, Start + (S2 + 1));
      Swap(Start, Start + S2);
    end else
      Sort3(Start + S2, Start, Finish - 1);
    if (not LeftMost) and (not ((Start - 1)^ < Start^)) then begin
      Start := PartitionLeft(Start, Finish) + 1;
      Continue;
    end;
    PartResult := PartitionRightBranchless(Start, Finish);
    PivotPos := PartResult.Key;
    AlreadyPartitioned := PartResult.Value;
    LSize := PivotPos - Start;
    RSize := Finish - (PivotPos + 1);
    if (LSize < Size div 8) or (RSize < Size div 8) then begin
      if (BadAllowed - 1) = 0 then begin
        HeapSort(Start, Finish);
        Exit();
      end;
      if LSize >= INSERTION_SORT_THRESHOLD then begin
        LSizeDiv := LSize div 4;
        Swap(Start, Start + LSizeDiv);
        Swap(PivotPos - 1, PivotPos - LSizeDiv);
        if LSize > NINTHER_THRESHOLD then begin
          Swap(Start + 1, Start + (LSizeDiv + 1));
          Swap(Start + 2, Start + (LSizeDiv + 2));
          Swap(PivotPos - 2, PivotPos - (LSizeDiv + 1));
          Swap(PivotPos - 3, PivotPos - (LSizeDiv + 2));
        end;
      end;
      if RSize >= INSERTION_SORT_THRESHOLD then begin
        RSizeDiv := RSize div 4;
        Swap(PivotPos + 1, PivotPos + (1 + RSizeDiv));
        Swap(Finish - 1, Finish - RSizeDiv);
        if RSize > NINTHER_THRESHOLD then begin
          Swap(PivotPos + 2, PivotPos + (2 + RSizeDiv));
          Swap(PivotPos + 3, PivotPos + (3 + RSizeDiv));
          Swap(Finish - 2, Finish - (1 + RSizeDiv));
          Swap(Finish - 3, Finish - (2 + RSizeDiv));
        end;
      end;
    end else begin
      Dummy := PivotPos + 1;
      if (AlreadyPartitioned and
          PartialInsertionSort(Start, PivotPos) and
          PartialInsertionSort(Dummy, Finish)) then
        Exit();
    end;
    PDQSortLoop(Start, PivotPos, BadAllowed - 1, LeftMost);
    Start := PivotPos + 1;
    LeftMost := False;
  end;
end;

class procedure TPDQSorter<T>.Sort(Start: PT; const Finish: PT);
begin
  if Start = Finish then Exit();
  PDQSortLoop(Start, Finish, BSRQWord(Finish - Start), True);
end;

class procedure TPDQSorter<T>.Sort(constref Arr: array of T);
begin
  Sort(PT(@Arr[0]), PT(@Arr[High(Arr)]) + 1);
end;

class procedure TPDQSorter<T>.Sort(constref Arr: TArray<T>);
begin
  Sort(PT(@Arr[0]), PT(@Arr[High(Arr)]) + 1);
end;

end.
