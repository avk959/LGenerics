{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Some useful string helpers.                                             *
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
unit LGStrHelpers;

{$mode objfpc}{$H+}
{$MODESWITCH TYPEHELPERS}
{$MODESWITCH ADVANCEDRECORDS}
{$INLINE ON}{$WARN 6058 off : }

interface

uses

  SysUtils,
  LGUtils,
  LGHelpers,
  LGCustomContainer,
  regexpr;

type

  TAnsiStrHelper = type helper(TAStrHelper) for ansistring
  private
  type

    TEnumerable = class(specialize TGAutoEnumerable<string>)
    private
      FString: string;
      FStartIndex,
      FCurrIndex: SizeInt;
      FDelimiters: TSysCharSet;
      FFound: Boolean;
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
    function SplitSB(constref aDelimiters: TSysCharSet = AsciiDelimiters): IStrEnumerable; inline;
  end;

  TRegexMatch = class
  protected
  type

    TEnumerable = class(specialize TGAutoEnumerable<string>)
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

implementation
{$Q-}{$B-}{$COPERATORS ON}

{ TAnsiStrHelper.TEnumerable }

function TAnsiStrHelper.TEnumerable.GetCurrent: string;
begin
  if FFound then
    Result := System.Copy(FString, FStartIndex, FCurrIndex - FStartIndex)
  else
    Result := '';
end;

constructor TAnsiStrHelper.TEnumerable.Create(const aValue: string; const aDelimiters: TSysCharSet);
begin
  inherited Create;
  FString := aValue;
  FDelimiters := aDelimiters;
  FStartIndex := 1;
  FCurrIndex := 1;
end;

function TAnsiStrHelper.TEnumerable.MoveNext: Boolean;
var
  I, Len: SizeInt;
  WordFound: Boolean;
begin
  Len := System.Length(FString);
  if FCurrIndex <= Len then
    begin
      WordFound := False;
      I := FCurrIndex;
      repeat
        if FString[I] in FDelimiters then
          begin
            if WordFound then
              break;
          end
        else
          if not WordFound then
            begin
              WordFound := True;
              FStartIndex := I;
            end;
        Inc(I);
      until I > Len;
      FCurrIndex := I;
      Result := WordFound;
      FFound := WordFound;
    end
  else
    Result := False;
end;

procedure TAnsiStrHelper.TEnumerable.Reset;
begin
  FStartIndex := 1;
  FCurrIndex := 1;
  FFound := False;
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

function TAnsiStrHelper.SplitSB(constref aDelimiters: TSysCharSet): IStrEnumerable;
begin
  Result := TEnumerable.Create(Self, aDelimiters);
end;

{ TRegexMatch.TEnumerable }

function TRegexMatch.TEnumerable.GetCurrent: string;
begin
  Result := FRegex.Match[0];
end;

constructor TRegexMatch.TEnumerable.Create(aRegex: TRegExpr; const s: string);
begin
  inherited Create;
  FRegex := aRegex;
  FInputString := s;
end;

function TRegexMatch.TEnumerable.MoveNext: Boolean;
begin
  if FInCycle then
    Result := FRegex.ExecNext
  else
    begin
      FInCycle := True;
      Result := FRegex.Exec(FInputString);
    end;
end;

procedure TRegexMatch.TEnumerable.Reset;
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
  Result := TEnumerable.Create(FRegex, aValue);
end;

end.

