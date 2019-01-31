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
unit LGFunction;

{$MODE OBJFPC}{$H+}
{$INLINE ON}{$WARN 6058 off : }
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses

  SysUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGAbstractContainer;

type

  generic TGMapping<X, Y> = class
  public
  type
    TMapFunc     = specialize TGMapFunc<X, Y>;
    TOnMap       = specialize TGOnMap<X, Y>;
    TNestMap     = specialize TGNestMap<X, Y>;
    TArrayX      = specialize TGArray<X>;
    IEnumerableX = specialize IGEnumerable<X>;
    IEnumerableY = specialize IGEnumerable<Y>;

  strict private
  type

    TArrayCursor = class abstract(specialize TGAutoEnumerable<Y>)
    protected
      FArray: TArrayX;
      FCurrIndex: SizeInt;
    public
      constructor Create(constref a: TArrayX);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TEnumeratorX = specialize TGEnumerator<X>;

    TEnumCursor = class abstract(specialize TGAutoEnumerable<Y>)
    protected
      FEnum: TEnumeratorX;
    public
      constructor Create(e : TEnumeratorX);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TArrayRegularMap = class(TArrayCursor)
    protected
      FMap: TMapFunc;
      function GetCurrent: Y; override;
    public
      constructor Create(constref a: TArrayX; aMap: TMapFunc);
    end;

    TArrayDelegatedMap = class(TArrayCursor)
    protected
      FMap: TOnMap;
      function GetCurrent: Y; override;
    public
      constructor Create(constref a: TArrayX; aMap: TOnMap);
    end;

    TArrayNestedMap = class(TArrayCursor)
    protected
      FMap: TNestMap;
      function GetCurrent: Y; override;
    public
      constructor Create(constref a: TArrayX; aMap: TNestMap);
    end;

    TEnumRegularMap = class(TEnumCursor)
    protected
      FMap: TMapFunc;
      function GetCurrent: Y; override;
    public
      constructor Create(e: IEnumerableX; aMap: TMapFunc);
    end;

    TEnumDelegatedMap = class(TEnumCursor)
    protected
      FMap: TOnMap;
      function GetCurrent: Y; override;
    public
      constructor Create(e: IEnumerableX; aMap: TOnMap);
    end;

    TEnumNestedMap = class(TEnumCursor)
    protected
      FMap: TNestMap;
      function GetCurrent: Y; override;
    public
      constructor Create(e: IEnumerableX; aMap: TNestMap);
    end;

  public
    class function Apply(constref a: TArrayX; f: TMapFunc): IEnumerableY; static; inline;
    class function Apply(constref a: TArrayX; f: TOnMap): IEnumerableY; static; inline;
    class function Apply(constref a: TArrayX; f: TNestMap): IEnumerableY; static; inline;
    class function Apply(e: IEnumerableX; f: TMapFunc): IEnumerableY; static; inline;
    class function Apply(e: IEnumerableX; f: TOnMap): IEnumerableY; static; inline;
    class function Apply(e: IEnumerableX; f: TNestMap): IEnumerableY; static; inline;
  end;

  generic TGFolding<X, Y> = class
  public
  type
    IXEnumerable = specialize IGEnumerable<X>;
    TFold        = specialize TGFold<X, Y>;
    TOnFold      = specialize TGOnFold<X, Y>;
    TNestFold    = specialize TGNestFold<X, Y>;

    class function Left(constref a: array of X; f: TFold; constref y0: Y): Y; static;
    class function Left(constref a: array of X; f: TOnFold; constref y0: Y): Y; static;
    class function Left(constref a: array of X; f: TNestFold; constref y0: Y): Y; static;
    class function Left(e: IXEnumerable; f: TFold; constref y0: Y): Y; static;
    class function Left(e: IXEnumerable; f: TOnFold; constref y0: Y): Y; static;
    class function Left(e: IXEnumerable; f: TNestFold; constref y0: Y): Y; static;
    class function Right(constref a: array of X; f: TFold; constref y0: Y): Y; static;
    class function Right(constref a: array of X; f: TOnFold; constref y0: Y): Y; static;
    class function Right(constref a: array of X; f: TNestFold; constref y0: Y): Y; static;
    class function Right(e: IXEnumerable; f: TFold; constref y0: Y): Y; static;
    class function Right(e: IXEnumerable; f: TOnFold; constref y0: Y): Y; static;
    class function Right(e: IXEnumerable; f: TNestFold; constref y0: Y): Y; static;
  end;
  { monadic regular function }
  generic TGMonadic<T, TResult> = function(constref v: T): TResult;
  { dyadic regular function }
  generic TGDyadic<T1, T2, TResult> = function(constref v1: T1; constref v2: T2): TResult;
  { triadic regular function }
  generic TGTriadic<T1, T2, T3, TResult> = function(constref v1: T1; constref v2: T2; constref v3: T3): TResult;

  generic TGDeferMonadic<T, TResult> = record
  public
  type
    TFun = specialize TGMonadic<T, TResult>;

  strict private
    FCall: TFun;
    FParam: T;
  public
    class function Construct(aFun: TFun; constref v: T): TGDeferMonadic; static;
    function Call: TResult; inline;
  end;

  generic TGDeferDyadic<T1, T2, TResult> = record
  public
  type
    TFun = specialize TGDyadic<T1, T2, TResult>;

  strict private
    FCall: TFun;
    FParam1: T1;
    FParam2: T2;
  public
    class function Construct(aFun: TFun; constref v1: T1; constref v2: T2): TGDeferDyadic; static;
    function Call: TResult; inline;
  end;

  generic TGDeferTriadic<T1, T2, T3, TResult> = record
  public
  type
    TFun = specialize TGTriadic<T1, T2, T3, TResult>;

  strict private
    FCall: TFun;
    FParam1: T1;
    FParam2: T2;
    FParam3: T3;
  public
    class function Construct(aFun: TFun; constref v1: T1; constref v2: T2; constref v3: T3): TGDeferTriadic; static;
    function Call: TResult; inline;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGMapping.TArrayCursor }

constructor TGMapping.TArrayCursor.Create(constref a: TArrayX);
begin
  inherited Create;
  FArray := a;
  FCurrIndex := -1;
end;

function TGMapping.TArrayCursor.MoveNext: Boolean;
begin
  Result := FCurrIndex < System.High(FArray);
  FCurrIndex += Ord(Result);
end;

procedure TGMapping.TArrayCursor.Reset;
begin
  FCurrIndex := -1;
end;

{ TGMapping.TEnumCursor }

constructor TGMapping.TEnumCursor.Create(e: TEnumeratorX);
begin
  inherited Create;
  FEnum := e;
end;

destructor TGMapping.TEnumCursor.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGMapping.TEnumCursor.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGMapping.TEnumCursor.Reset;
begin
  FEnum.Reset;
end;

{ TArrayRegularMap }

function TGMapping.TArrayRegularMap.GetCurrent: Y;
begin
  Result := FMap(FArray[FCurrIndex]);
end;

constructor TGMapping.TArrayRegularMap.Create(constref a: TArrayX; aMap: TMapFunc);
begin
  inherited Create(a);
  FMap := aMap;
end;

{ TArrayDelegatedMap }

function TGMapping.TArrayDelegatedMap.GetCurrent: Y;
begin
  Result := FMap(FArray[FCurrIndex]);
end;

constructor TGMapping.TArrayDelegatedMap.Create(constref a: TArrayX; aMap: TOnMap);
begin
  inherited Create(a);
  FMap := aMap;
end;

{ TGMapping.TArrayNestedMap }

function TGMapping.TArrayNestedMap.GetCurrent: Y;
begin
  Result := FMap(FArray[FCurrIndex]);
end;

constructor TGMapping.TArrayNestedMap.Create(constref a: TArrayX; aMap: TNestMap);
begin
  inherited Create(a);
  FMap := aMap;
end;

{ TEnumRegularMap }

function TGMapping.TEnumRegularMap.GetCurrent: Y;
begin
  Result := FMap(FEnum.Current);
end;

constructor TGMapping.TEnumRegularMap.Create(e: IEnumerableX; aMap: TMapFunc);
begin
  inherited Create(e.GetEnumerator);
  FMap := aMap;
end;

{ TEnumDelegatedMap }

function TGMapping.TEnumDelegatedMap.GetCurrent: Y;
begin
  Result := FMap(FEnum.Current);
end;

constructor TGMapping.TEnumDelegatedMap.Create(e: IEnumerableX; aMap: TOnMap);
begin
  inherited Create(e.GetEnumerator);
  FMap := aMap;
end;

{ TGMapping.TEnumNestedMap }

function TGMapping.TEnumNestedMap.GetCurrent: Y;
begin
  Result := FMap(FEnum.Current);
end;

constructor TGMapping.TEnumNestedMap.Create(e: IEnumerableX; aMap: TNestMap);
begin
  inherited Create(e.GetEnumerator);
  FMap := aMap;
end;

{ TGMapping }

class function TGMapping.Apply(constref a: TArrayX; f: TMapFunc): IEnumerableY;
begin
  Result := TArrayRegularMap.Create(a, f);
end;

class function TGMapping.Apply(constref a: TArrayX; f: TOnMap): IEnumerableY;
begin
  Result := TArrayDelegatedMap.Create(a, f);
end;

class function TGMapping.Apply(constref a: TArrayX; f: TNestMap): IEnumerableY;
begin
  Result := TArrayNestedMap.Create(a, f);
end;

class function TGMapping.Apply(e: IEnumerableX; f: TMapFunc): IEnumerableY;
begin
  Result := TEnumRegularMap.Create(e, f);
end;

class function TGMapping.Apply(e: IEnumerableX; f: TOnMap): IEnumerableY;
begin
  Result := TEnumDelegatedMap.Create(e, f);
end;

class function TGMapping.Apply(e: IEnumerableX; f: TNestMap): IEnumerableY;
begin
  Result := TEnumNestedMap.Create(e, f);
end;

{ TGFolding }

class function TGFolding.Left(constref a: array of X; f: TFold; constref y0: Y): Y;
var
  v: X;
begin
  Result := y0;
  for v in a do
    Result := f(v, Result);
end;

class function TGFolding.Left(constref a: array of X; f: TOnFold; constref y0: Y): Y;
var
  v: X;
begin
  Result := y0;
  for v in a do
    Result := f(v, Result);
end;

class function TGFolding.Left(constref a: array of X; f: TNestFold; constref y0: Y): Y;
var
  v: X;
begin
  Result := y0;
  for v in a do
    Result := f(v, Result);
end;

class function TGFolding.Left(e: IXEnumerable; f: TFold; constref y0: Y): Y;
var
  v: X;
begin
  Result := y0;
  for v in e do
    Result := f(v, Result);
end;

class function TGFolding.Left(e: IXEnumerable; f: TOnFold; constref y0: Y): Y;
var
  v: X;
begin
  Result := y0;
  for v in e do
    Result := f(v, Result);
end;

class function TGFolding.Left(e: IXEnumerable; f: TNestFold; constref y0: Y): Y;
var
  v: X;
begin
  Result := y0;
  for v in e do
    Result := f(v, Result);
end;

class function TGFolding.Right(constref a: array of X; f: TFold; constref y0: Y): Y;
var
  v: X;
  I: SizeInt;
begin
  Result := y0;
  for I := System.High(a) downto 0 do
    Result := f(a[I], Result);
end;

class function TGFolding.Right(constref a: array of X; f: TOnFold; constref y0: Y): Y;
var
  v: X;
  I: SizeInt;
begin
  Result := y0;
  for I := System.High(a) downto 0 do
    Result := f(a[I], Result);
end;

class function TGFolding.Right(constref a: array of X; f: TNestFold; constref y0: Y): Y;
var
  v: X;
  I: SizeInt;
begin
  Result := y0;
  for I := System.High(a) downto 0 do
    Result := f(a[I], Result);
end;

class function TGFolding.Right(e: IXEnumerable; f: TFold; constref y0: Y): Y;
var
  v: X;
begin
  Result := y0;
  for v in e.Reverse do
    Result := f(v, Result);
end;

class function TGFolding.Right(e: IXEnumerable; f: TOnFold; constref y0: Y): Y;
var
  v: X;
begin
  Result := y0;
  for v in e.Reverse do
    Result := f(v, Result);
end;

class function TGFolding.Right(e: IXEnumerable; f: TNestFold; constref y0: Y): Y;
var
  v: X;
begin
  Result := y0;
  for v in e.Reverse do
    Result := f(v, Result);
end;

{ TGDeferMonadic }

class function TGDeferMonadic.Construct(aFun: TFun; constref v: T): TGDeferMonadic;
begin
  Result.FCall := aFun;
  Result.FParam := v;
end;

function TGDeferMonadic.Call: TResult;
begin
  Result := FCall(FParam);
end;

{ TGDeferDyadic }

class function TGDeferDyadic.Construct(aFun: TFun; constref v1: T1; constref v2: T2): TGDeferDyadic;
begin
  Result.FCall := aFun;
  Result.FParam1 := v1;
  Result.FParam2 := v2;
end;

function TGDeferDyadic.Call: TResult;
begin
  Result := FCall(FParam1, FParam2);
end;

{ TGDeferTriadic }

class function TGDeferTriadic.Construct(aFun: TFun; constref v1: T1; constref v2: T2;
  constref v3: T3): TGDeferTriadic;
begin
  Result.FCall := aFun;
  Result.FParam1 := v1;
  Result.FParam2 := v2;
  Result.FParam3 := v3;
end;

function TGDeferTriadic.Call: TResult;
begin
  Result := FCall(FParam1, FParam2, FParam3);
end;

end.

