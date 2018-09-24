
{ TGAutoEnumerable.TEnumerable }

constructor TGAutoEnumerable.TEnumerable.Create(e: TGAutoEnumerable);
begin
  FOwner := e;
end;

function TGAutoEnumerable.TEnumerable.GetEnumerator: TCustomEnumerator;
begin
  FOwner.Reset;
  Result := FOwner;
end;

{ TGAutoEnumerable }

constructor TGAutoEnumerable.Create;
begin
  FEnumerable := TEnumerable.Create(Self);
end;

destructor TGAutoEnumerable.Destroy;
begin
  FEnumerable.Free;
  inherited;
end;

{ TGEnumCursor }

function TGEnumCursor.GetCurrent: T;
begin
  Result := FEnum.GetCurrent;
end;

constructor TGEnumCursor.Create(e: TCustomEnumerator);
begin
  inherited Create;
  FEnum := e;
end;

destructor TGEnumCursor.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGEnumCursor.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGEnumCursor.Reset;
begin
  FEnum.Reset;
end;

{ TGEnumRegularFilter }

function TGEnumRegularFilter.MoveNext: Boolean;
begin
  repeat
    if not FEnum.MoveNext then
      exit(False);
    Result := FTest(FEnum.GetCurrent);
  until Result;
end;

constructor TGEnumRegularFilter.Create(e: TCustomEnumerator; aTest: TTest);
begin
  inherited Create(e);
  FTest := aTest;
end;

{ TGEnumDelegatedFilter }

constructor TGEnumDelegatedFilter.Create(e: TCustomEnumerator; aTest: TOnTest);
begin
  inherited Create(e);
  FTest := aTest;
end;

function TGEnumDelegatedFilter.MoveNext: Boolean;
begin
  repeat
    if not FEnum.MoveNext then
      exit(False);
    Result := FTest(FEnum.GetCurrent);
  until Result;
end;

{ TGEnumNestedFilter }

constructor TGEnumNestedFilter.Create(e: TCustomEnumerator; aTest: TNestTest);
begin
  inherited Create(e);
  FTest := aTest;
end;

function TGEnumNestedFilter.MoveNext: Boolean;
begin
  repeat
    if not FEnum.MoveNext then
      exit(False);
    Result := FTest(FEnum.GetCurrent);
  until Result;
end;

{ TGEnumRegularMap }

constructor TGEnumRegularMap.Create(e: TCustomEnumerator; aMap: TMapFunc);
begin
  inherited Create(e);
  FMap := aMap;
end;

function TGEnumRegularMap.GetCurrent: T;
begin
  Result := FMap(FEnum.GetCurrent);
end;

{ TGEnumDelegatedMap }

constructor TGEnumDelegatedMap.Create(e: TCustomEnumerator; aMap: TOnMap);
begin
  inherited Create(e);
  FMap := aMap;
end;

function TGEnumDelegatedMap.GetCurrent: T;
begin
  Result := FMap(FEnum.GetCurrent);
end;

{ TGEnumNestedMap }

constructor TGEnumNestedMap.Create(e: TCustomEnumerator; aMap: TNestMap);
begin
  inherited Create(e);
  FMap := aMap;
end;

function TGEnumNestedMap.GetCurrent: T;
begin
  Result := FMap(FEnum.GetCurrent);
end;

{ TGSkipEnumerable }

procedure TGSkipEnumerable.Skip;
begin
  while (FSkipped < FSkipCount) and FEnum.MoveNext do
    Inc(FSkipped);
end;

constructor TGSkipEnumerable.Create(e: TCustomEnumerator; aCount: SizeInt);
begin
  inherited Create(e);
  FSkipCount := aCount;
  Skip;
end;

procedure TGSkipEnumerable.Reset;
begin
  inherited;
  FSkipped := 0;
  Skip;
end;

{ TGLimitEnumerable }

constructor TGLimitEnumerable.Create(e: TCustomEnumerator; aCount: SizeInt);
begin
  inherited Create(e);
  FLimit := aCount;
end;

function TGLimitEnumerable.MoveNext: Boolean;
begin
  Result := (FCurrent < FLimit) and FEnum.MoveNext;
  FCurrent += Ord(Result);
end;

procedure TGLimitEnumerable.Reset;
begin
  inherited;
  FCurrent := 0;
end;

{ TGArrayCursor.TArrayEnumerable }

function TGArrayCursor.TArrayEnumerable.ToArray: TArray;
begin
  Result := System.Copy(TGArrayCursor(FOwner).FArray, 0, System.Length(TGArrayCursor(FOwner).FArray));
  FOwner.Free;
end;

{ TGArrayCursor }

function TGArrayCursor.GetCurrent: T;
begin
  Result := FArray[FCurrIndex];
end;

constructor TGArrayCursor.Create(constref a: TArray);
begin
  FEnumerable := TArrayEnumerable.Create(Self);
  FArray := a;
  FCurrIndex := -1;
end;

function TGArrayCursor.MoveNext: Boolean;
begin
  Result := FCurrIndex < System.High(FArray);
  FCurrIndex += Ord(Result);
end;

procedure TGArrayCursor.Reset;
begin
  FCurrIndex := -1;
end;

{ TGArrayReverse.TArrayEnumerable }

function TGArrayReverse.TArrayEnumerable.ToArray: TArray;
begin
  Result := specialize TGArrayHelpUtil<T>.CreateReverseCopy(TGArrayReverse(FOwner).FArray);
  FOwner.Free;
end;

{ TGArrayReverse }

function TGArrayReverse.GetCurrent: T;
begin
  Result := FArray[FCurrIndex];
end;

constructor TGArrayReverse.Create(constref a: TArray);
begin
  FEnumerable := TArrayEnumerable.Create(Self);
  FArray := a;
  FCurrIndex := System.Length(a);
end;

function TGArrayReverse.MoveNext: Boolean;
begin
  Result := FCurrIndex > System.Low(FArray);
  FCurrIndex -= Ord(Result);
end;

procedure TGArrayReverse.Reset;
begin
  FCurrIndex := System.Length(FArray);
end;

{ TGArrayEnumerator }

function TGArrayEnumerator.GetCurrent: T;
begin
  Result := FArray[FPosition];
end;

constructor TGArrayEnumerator.Create(constref a: TArray);
begin
  FArray := a;
  FLast := System.High(a);
  FPosition := -1;
end;

function TGArrayEnumerator.MoveNext: Boolean;
begin
  Result := FPosition < FLast;
  FPosition += Ord(Result);
end;

procedure TGArrayEnumerator.Reset;
begin
  FPosition := -1;
end;
