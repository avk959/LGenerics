{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Some useful string helpers.                                             *
*                                                                           *
*   Copyright(c) 2018-2020 A.Koverdyaev(avk)                                *
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
unit LGStrHelpers;

{$mode objfpc}{$H+}
{$MODESWITCH TYPEHELPERS}
{$MODESWITCH ADVANCEDRECORDS}
{$INLINE ON}

interface

uses

  Classes,
  SysUtils,
  LGUtils,
  LGHelpers,
  LGAbstractContainer,
  LGMiscUtils,
  regexpr;

type

  TAnsiStrHelper = type helper(TAStrHelper) for ansistring
  private
  type
    TStrEnumerable = class(specialize TGAutoEnumerable<string>)
    private
      FString: string;
      FStartIndex,
      FCurrLen,
      FCurrIndex: SizeInt;
      FDelimiters: TSysCharSet;
    protected
      function  GetCurrent: string; override;
    public
      constructor Create(const aValue: string; const aDelimiters: TSysCharSet);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  public
  type
    IStrEnumerable = specialize IGEnumerable<string>;

  const
    WhiteSpaces = [#0..' '];
    AsciiDelimiters = [#0..#255] - ['a'..'z', 'A'..'Z', '0'..'9', '_'];
    function StripWhiteSpaces: ansistring; inline;
    function StripChar(aChar: AnsiChar): ansistring;
    function StripChars(constref aChars: TSysCharSet): ansistring;
    // only single byte delimiters allowed
    function Words(constref aDelimiters: TSysCharSet = AsciiDelimiters): IStrEnumerable; inline;
  end;

  TRegexMatch = class
  protected
  type
    TStrEnumerable = class(specialize TGAutoEnumerable<string>)
    private
      FRegex: TRegExpr;
      FInputString: string;
      FInCycle: Boolean;
    protected
      function  GetCurrent: string; override;
    public
      constructor Create(aRegex: TRegExpr; const s: string);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  var
    FRegex: TRegExpr;
    function  GetExpression: string;
    function  GetModifierStr: string;
    procedure SetExpression(aValue: string);
    procedure SetModifierStr(aValue: string);
  public
  type
    IStrEnumerable = specialize IGEnumerable<string>;

    constructor Create;
    constructor Create(const aRegExpression: string);
    constructor Create(const aRegExpression, aModifierStr: string);
    destructor Destroy; override;
    function Matches(const aValue: string): IStrEnumerable; inline;
    property Expression: string read GetExpression write SetExpression;
    property ModifierStr: string read GetModifierStr write SetModifierStr;
  end;

  TStringListHelper = class helper for TStringList
  public
    type
      IStrEnumerable = specialize IGEnumerable<string>;

    function GetEnumerable: IStrEnumerable; inline;
  end;

implementation
{$Q-}{$B-}{$COPERATORS ON}

{ TAnsiStrHelper.TStrEnumerable }

function TAnsiStrHelper.TStrEnumerable.GetCurrent: string;
begin
  Result := System.Copy(FString, FStartIndex, FCurrLen);
end;

constructor TAnsiStrHelper.TStrEnumerable.Create(const aValue: string; const aDelimiters: TSysCharSet);
begin
  inherited Create;
  FString := aValue;
  FDelimiters := aDelimiters;
  FStartIndex := 1;
  FCurrLen := 0;
  FCurrIndex := 0;
end;

function TAnsiStrHelper.TStrEnumerable.MoveNext: Boolean;
var
  I, Len: SizeInt;
begin
  Len := 0;
  for I := Succ(FCurrIndex) to System.Length(FString) do
    if not (FString[I] in FDelimiters) then
      begin
        if Len = 0 then
          FStartIndex := I;
        Inc(Len);
      end
    else
      if Len <> 0 then
        begin
          FCurrIndex := I;
          FCurrLen := Len;
          exit(True);
        end;
  FCurrIndex := System.Length(FString);
  Result := False;
end;

procedure TAnsiStrHelper.TStrEnumerable.Reset;
begin
  FStartIndex := 1;
  FCurrLen := 0;
  FCurrIndex := 0;
end;

{ TAnsiStrHelper }

function TAnsiStrHelper.StripWhiteSpaces: ansistring;
begin
  Result := StripChars(WhiteSpaces);
end;

function TAnsiStrHelper.StripChar(aChar: AnsiChar): ansistring;
var
  I, J: SizeInt;
  pRes, pSelf: PAnsiChar;
  c: AnsiChar;
begin
  if Self = '' then
    exit('');
  SetLength(Result, System.Length(Self));
  pSelf := PAnsiChar(Self);
  pRes := PAnsiChar(Result);
  J := 0;
  for I := 0 to Pred(System.Length(Self)) do
    begin
      c := pSelf[I];
      if c <> aChar then
        begin
          pRes[J] := c;
          Inc(J);
        end;
    end;
  SetLength(Result, J);
end;

function TAnsiStrHelper.StripChars(constref aChars: TSysCharSet): ansistring;
var
  I, J: SizeInt;
  pRes, pSelf: PAnsiChar;
  c: AnsiChar;
begin
  if Self = '' then
    exit('');
  SetLength(Result, System.Length(Self));
  pSelf := PAnsiChar(Self);
  pRes := PAnsiChar(Result);
  J := 0;
  for I := 0 to Pred(System.Length(Self)) do
    begin
      c := pSelf[I];
      if not (c in aChars) then
        begin
          pRes[J] := c;
          Inc(J);
        end;
    end;
  SetLength(Result, J);
end;

function TAnsiStrHelper.Words(constref aDelimiters: TSysCharSet): IStrEnumerable;
begin
  Result := TStrEnumerable.Create(Self, aDelimiters);
end;

{ TRegexMatch.TStrEnumerable }

function TRegexMatch.TStrEnumerable.GetCurrent: string;
begin
  Result := FRegex.Match[0];
end;

constructor TRegexMatch.TStrEnumerable.Create(aRegex: TRegExpr; const s: string);
begin
  inherited Create;
  FRegex := aRegex;
  FInputString := s;
end;

function TRegexMatch.TStrEnumerable.MoveNext: Boolean;
begin
  if FInCycle then
    Result := FRegex.ExecNext
  else
    begin
      FInCycle := True;
      Result := FRegex.Exec(FInputString);
    end;
end;

procedure TRegexMatch.TStrEnumerable.Reset;
begin
  FInCycle := False;
end;

{ TRegexMatch }

function TRegexMatch.GetExpression: string;
begin
  Result := FRegex.Expression;
end;

function TRegexMatch.GetModifierStr: string;
begin
  Result := FRegex.ModifierStr;
end;

procedure TRegexMatch.SetExpression(aValue: string);
begin
  FRegex.Expression := aValue;
end;

procedure TRegexMatch.SetModifierStr(aValue: string);
begin
  FRegex.ModifierStr := aValue;
end;

constructor TRegexMatch.Create;
begin
  FRegex := TRegExpr.Create;
end;

constructor TRegexMatch.Create(const aRegExpression: string);
begin
  FRegex := TRegExpr.Create(aRegExpression);
end;

constructor TRegexMatch.Create(const aRegExpression, aModifierStr: string);
begin
  FRegex := TRegExpr.Create(aRegExpression);
  FRegex.ModifierStr := aModifierStr;
end;

destructor TRegexMatch.Destroy;
begin
  FRegex.Free;
  inherited;
end;

function TRegexMatch.Matches(const aValue: string): IStrEnumerable;
begin
  Result := TStrEnumerable.Create(FRegex, aValue);
end;

{ TStringListHelper }

function TStringListHelper.GetEnumerable: IStrEnumerable;
begin
  Result := specialize TGClassEnumerable<string, TStringList, TStringsEnumerator>.Create(Self);
end;

end.

