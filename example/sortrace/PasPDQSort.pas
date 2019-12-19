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
    class function AlignCacheLine(const P: T): T; static; inline;
    class function Min(const A, B: T): T; static; inline;
  end;

  TPDQSorter<T> = record
  public type
    PT = ^T;
    TIsLess = function(constref A, B: T): Boolean;
  private type
    PtrUtil = Util<PT>;
    ByteUtil = Util<Byte>;
    BytePtrUtil = Util<PByte>;
    IntUtil = Util<PtrInt>;
  strict private class var
    OffsetsLStorage, OffsetsRStorage: array[0..Pred(BLOCK_SIZE + CACHE_LINE_SIZE)] of Byte;
  strict private
    class procedure Swap(const L, R: PT); static; inline;
    class procedure Sort3(const A, B, C: PT; const IsLess: TIsLess); static; inline;
    class procedure InsertionSort(const Start, Finish: PT; const IsLess: TIsLess); static; inline;
    class procedure UnguardedInsertionSort(const Start, Finish: PT; const IsLess: TIsLess); static; inline;
    class function PartialInsertionSort(const Start, Finish: PT; const IsLess: TIsLess): Boolean; static; inline;
    class procedure HeapSort(const Start: PT; const Count: PtrUInt; const IsLess: TIsLess); static;
    class procedure SwapOffsets(const First, Last: PT;
                                const OffsetsL, OffsetsR: PByte;
                                const Num: PtrUInt;
                                const UseSwaps: Boolean); static;
    class function PartitionRightBranchless(const Start, Finish: PT; const IsLess: TIsLess): TPair<PT, Boolean>; static;
    class function PartitionRight(const Start, Finish: PT; const IsLess: TIsLess): TPair<PT, Boolean>; static;
    class function PartitionLeft(const Start, Finish: PT; const IsLess: TIsLess): PT; static;
    class procedure PDQSortLoop(var Start: PT;
                                const Finish: PT;
                                const IsLess: TIsLess;
                                var BadAllowed: PtrInt;
                                LeftMost: Boolean = True;
                                const Branchless: Boolean = True); static;
  public
    class procedure Sort(Start: PT;
                         const Finish: PT;
                         const IsLess: TIsLess;
                         const Branchless: Boolean = True); static; inline; overload;
    class procedure Sort(constref Arr: array of T;
                         const IsLess: TIsLess;
                         const Branchless: Boolean = True); static; overload;
    class procedure Sort(constref Arr: TArray<T>;
                         const IsLess: TIsLess;
                         const Branchless: Boolean = True); static; inline; overload;
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

class function Util<T>.AlignCacheLine(const P: T): T;
var IP: PtrUInt;
begin
  IP := PtrUInt(P);
  IP := (IP + CACHE_LINE_SIZE - 1) and -CACHE_LINE_SIZE;
  Result := T(IP);
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

class procedure TPDQSorter<T>.Sort3(const A, B, C: PT; const IsLess: TIsLess);
begin
  if IsLess(B^, A^) then Swap(A, B);
  if IsLess(C^, B^) then Swap(B, C);
  if IsLess(B^, A^) then Swap(A, B);
end;

class procedure TPDQSorter<T>.InsertionSort(const Start, Finish: PT; const IsLess: TIsLess);
var
  Tmp: T;
  Cur, Sift, SiftL: PT;
begin
  if Start = Finish then Exit();
  Cur := Start + 1;
  while Cur <> Finish do begin
    Sift := Cur;
    SiftL := Cur - 1;
    if IsLess(Sift^, SiftL^) then begin
      Tmp := Sift^;
      repeat
        PtrUtil.PostDec(Sift)^ := SiftL^;
      until (Sift = Start) or not IsLess(Tmp, PtrUtil.PreDec(SiftL)^);
      Sift^ := Tmp;
    end;
    Cur += 1;
  end;
end;

class procedure TPDQSorter<T>.UnguardedInsertionSort(const Start, Finish: PT; const IsLess: TIsLess);
var
  Tmp: T;
  Cur, Sift, SiftL: PT;
begin
  if Start = Finish then Exit();
  Cur := Start + 1;
  while Cur <> Finish do begin
    Sift := Cur;
    SiftL := Cur - 1;
    if IsLess(Sift^, SiftL^) then begin
      Tmp := Sift^;
      repeat
        PtrUtil.PostDec(Sift)^ := SiftL^;
      until not IsLess(Tmp, PtrUtil.PreDec(SiftL)^);
      Sift^ := Tmp;
    end;
    Cur += 1;
  end;
end;

class function TPDQSorter<T>.PartialInsertionSort(const Start, Finish: PT; const IsLess: TIsLess): Boolean;
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
    if IsLess(Sift^, SiftL^) then begin
      Tmp := Sift^;
      repeat
        PtrUtil.PostDec(Sift)^ := SiftL^;
      until (Sift = Start) or not IsLess(Tmp, PtrUtil.PreDec(SiftL)^);
      Sift^ := Tmp;
      Limit += Cur - Sift;
    end;
    Cur += 1;
  end;
  Result := True;
end;

class procedure TPDQSorter<T>.HeapSort(const Start: PT; const Count: PtrUInt; const IsLess: TIsLess);
var
  HeapSize: PtrUInt;
  Tmp: T;

  procedure Heapify(I: PtrUInt);
  label
    Again;
  var
    L, R, Largest: PtrUInt;
  begin
again:
    L := 2 * I + 1;
    R := 2 * I + 2;
    if (L < HeapSize) and (not IsLess((Start + L)^, (Start + I)^)) then
      Largest := L
    else
      Largest := I;
    if (R < HeapSize) and (not IsLess((Start + R)^, (Start + Largest)^)) then
      Largest := R;
    if Largest <> I then begin
      Tmp := (Start + I)^;
      (Start + I)^ := (Start + Largest)^;
      (Start + Largest)^ := Tmp;
      I := Largest;
      goto Again;
    end;
  end;

var
  I: PtrUInt;
begin
  HeapSize := Count;
  for I := ((Count - 1) - 1) div 2 downto 0 do
    Heapify(I);
  for I := Count - 1 downto 1 do
  begin
    Tmp := Start^;
    Start^ := (Start + I)^;
    (Start + I)^ := Tmp;
    Dec(HeapSize);
    Heapify(0);
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

class function TPDQSorter<T>.PartitionRightBranchless(const Start, Finish: PT; const IsLess: TIsLess): TPair<PT, Boolean>;
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
  while IsLess(PtrUtil.PreInc(First)^, Pivot) do
    ;
  if First - 1 = Start then begin
    while (First < Last) and not IsLess(PtrUtil.PreDec(Last)^, Pivot) do
      ;
  end else
    while not IsLess(PtrUtil.PreDec(Last)^, Pivot) do
      ;
  AlreadyPartitioned := First >= Last;
  if not AlreadyPartitioned then begin
    Tmp := First^;
    First^ := Last^;
    Last^ := Tmp;
    First += 1;
  end;
  OffsetsL := BytePtrUtil.AlignCacheLine(@OffsetsLStorage[0]);
  OffsetsR := BytePtrUtil.AlignCacheLine(@OffsetsRStorage[0]);
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
        NumL += PtrInt(not IsLess(It^, Pivot));
        (OffsetsL + NumL)^ := I + 1;
        NumL += PtrInt(not IsLess((It + 1)^, Pivot));
        (OffsetsL + NumL)^ := I + 2;
        NumL += PtrInt(not IsLess((It + 2)^, Pivot));
        (OffsetsL + NumL)^ := I + 3;
        NumL += PtrInt(not IsLess((It + 3)^, Pivot));
        (OffsetsL + NumL)^ := I + 4;
        NumL += PtrInt(not IsLess((It + 4)^, Pivot));
        (OffsetsL + NumL)^ := I + 5;
        NumL += PtrInt(not IsLess((It + 5)^, Pivot));
        (OffsetsL + NumL)^ := I + 6;
        NumL += PtrInt(not IsLess((It + 6)^, Pivot));
        (OffsetsL + NumL)^ := I + 7;
        NumL += PtrInt(not IsLess((It + 7)^, Pivot));
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
        NumR += PtrInt(IsLess((It - 1)^, Pivot));
        (OffsetsR + NumR)^ := I + 2;
        NumR += PtrInt(IsLess((It - 2)^, Pivot));
        (OffsetsR + NumR)^ := I + 3;
        NumR += PtrInt(IsLess((It - 3)^, Pivot));
        (OffsetsR + NumR)^ := I + 4;
        NumR += PtrInt(IsLess((It - 4)^, Pivot));
        (OffsetsR + NumR)^ := I + 5;
        NumR += PtrInt(IsLess((It - 5)^, Pivot));
        (OffsetsR + NumR)^ := I + 6;
        NumR += PtrInt(IsLess((It - 6)^, Pivot));
        (OffsetsR + NumR)^ := I + 7;
        NumR += PtrInt(IsLess((It - 7)^, Pivot));
        (OffsetsR + NumR)^ := I + 8;
        NumR += PtrInt(IsLess((It - 8)^, Pivot));
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
      NumL += PtrInt(not IsLess(It^, Pivot));
      It += 1;
    end;
  end;
  if (UnknownLeft <> 0) and (NumR = 0) then begin
    StartR := 0;
    It := Last;
    I := 0;
    while I < RSize do begin
      (OffsetsR + NumR)^ := ByteUtil.PreInc(I);
      NumR += PtrInt(IsLess(PtrUtil.PreDec(It)^, Pivot));
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

class function TPDQSorter<T>.PartitionRight(const Start, Finish: PT; const IsLess: TIsLess): TPair<PT, Boolean>;
var
  AlreadyPartitioned: Boolean;
  Pivot: T;
  First, Last, PivotPos: PT;
begin
  Pivot := Start^;
  First := Start;
  Last := Finish;
  while IsLess(PtrUtil.PreInc(First)^, Pivot) do
    ;
  if First - 1 = Start then begin
    while (First < Last) and not IsLess(PtrUtil.PreDec(Last)^, Pivot) do
      ;
  end else
    while not IsLess(PtrUtil.PreDec(Last)^, Pivot) do
      ;
  AlreadyPartitioned := First >= Last;
  while First < Last do begin
    Swap(First, Last);
    while IsLess(PtrUtil.PreInc(First)^, Pivot) do
      ;
    while not IsLess(PtrUtil.PreDec(Last)^, Pivot) do
      ;
  end;
  PivotPos := First - 1;
  Start^ := PivotPos^;
  PivotPos^ := Pivot;
  Result := TPair<PT, Boolean>.Create(PivotPos, AlreadyPartitioned);
end;

class function TPDQSorter<T>.PartitionLeft(const Start, Finish: PT; const IsLess: TIsLess): PT;
var
  Pivot: T;
  First, Last, PivotPos: PT;
begin
  Pivot := Start^;
  First := Start;
  Last := Finish;
  while IsLess(Pivot, PtrUtil.PreDec(Last)^) do
    ;
  if Last + 1 = Finish then begin
    while (First < Last) and not IsLess(Pivot, PtrUtil.PreInc(First)^) do
      ;
  end else
    while not IsLess(Pivot, PtrUtil.PreInc(First)^) do
      ;
  while First < Last do begin
    Swap(First, Last);
    while IsLess(Pivot, PtrUtil.PreDec(Last)^) do
      ;
    while not IsLess(Pivot, PtrUtil.PreInc(First)^) do
      ;
  end;
  PivotPos := Last;
  Start^ := PivotPos^;
  PivotPos^ := Pivot;
  Result := PivotPos;
end;

class procedure TPDQSorter<T>.PDQSortLoop(var Start: PT;
                                          const Finish: PT;
                                          const IsLess: TIsLess;
                                          var BadAllowed: PtrInt;
                                          LeftMost: Boolean;
                                          const Branchless: Boolean);
var
  AlreadyPartitioned, HighlyUnbalanced: Boolean;
  PivotPos: PT;
  Size, S2, LSize, LSizeDiv, RSize, RSizeDiv: PtrInt;
  PartResult: TPair<PT, Boolean>;
begin
  while True do begin
    Size := Finish - Start;
    if Size < INSERTION_SORT_THRESHOLD then begin
      if LeftMost then
        InsertionSort(Start, Finish, IsLess)
      else
        UnguardedInsertionSort(Start, Finish, IsLess);
      Exit();
    end;
    S2 := Size div 2;
    if Size > NINTHER_THRESHOLD then begin
      Sort3(Start, Start + S2, Finish - 1, IsLess);
      Sort3(Start + 1, Start + (S2 - 1), Finish - 2, IsLess);
      Sort3(Start + 2, Start + (S2 + 1), Finish - 3, IsLess);
      Sort3(Start + (S2 - 1), Start + S2, Start + (S2 + 1), IsLess);
      Swap(Start, Start + S2);
    end else
      Sort3(Start + S2, Start, Finish - 1, IsLess);
    if (not LeftMost) and (not IsLess((Start - 1)^, Start^)) then begin
      Start := PartitionLeft(Start, Finish, IsLess) + 1;
      Continue;
    end;
    if Branchless then
      PartResult := PartitionRightBranchless(Start, Finish, IsLess)
    else
      PartResult := PartitionRight(Start, Finish, IsLess);
    PivotPos := PartResult.Key;
    AlreadyPartitioned := PartResult.Value;
    LSize := PivotPos - Start;
    RSize := Finish - (PivotPos + 1);
    HighlyUnbalanced := (LSize < Size div 8) or (RSize < Size div 8);
    if HighlyUnbalanced then begin
      if IntUtil.PreDec(BadAllowed) = 0 then begin
        HeapSort(Start, (Finish - Start), IsLess);
        BadAllowed := BSRQWord(Finish - Start) + 1;
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
      if (AlreadyPartitioned and
          PartialInsertionSort(Start, PivotPos, IsLess) and
          PartialInsertionSort(PivotPos + 1, Finish, IsLess)) then
        Exit();
    end;
    PDQSortLoop(Start, PivotPos, IsLess, BadAllowed, LeftMost, Branchless);
    Start := PivotPos + 1;
    LeftMost := False;
  end;
end;

class procedure TPDQSorter<T>.Sort(Start: PT; const Finish: PT; const IsLess: TIsLess; const Branchless: Boolean);
var BadAllowed: PtrInt;
begin
  if Start = Finish then Exit();
  BadAllowed := BSRQWord(Finish - Start) + 1;
  PDQSortLoop(Start, Finish, IsLess, BadAllowed, True, Branchless);
end;

class procedure TPDQSorter<T>.Sort(constref Arr: array of T; const IsLess: TIsLess; const Branchless: Boolean);
begin
  Sort(@Arr[0], @Arr[High(Arr)] + 1, IsLess, Branchless);
end;

class procedure TPDQSorter<T>.Sort(constref Arr: TArray<T>; const IsLess: TIsLess; const Branchless: Boolean);
begin
  Sort(@Arr[0], @Arr[High(Arr)] + 1, IsLess, Branchless);
end;

end.