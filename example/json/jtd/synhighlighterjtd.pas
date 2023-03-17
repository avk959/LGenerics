{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.
The Original Code is: SynHighlighterJSON.pas, released 2015-01-14.
The Initial Author of this file is Christian-W. Budde.
All Rights Reserved.
Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.
Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.
You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net


last modified 2023-03-10 by avk
old unit name: SynHighlighterJSON

}

unit SynHighlighterJTD;

{$IFDEF FPC}
  {$MODE OBJFPC}
  {$DEFINE SYN_LAZARUS}
{$ENDIF}

{$DEFINE SYNEDIT_INCLUDE}

{$IFdef MSWindows}
  {$DEFINE SYN_WIN32}
{$ENDIF}
{$H+}

interface

uses
  Classes,
  Graphics,
  SynEditHighlighter,
  lgHashSet, lgJson;


type
  TtkTokenKind = (tkString, tkReserved, tkNull, tkNumber, tkSpace, tkSymbol, tkUnknown);
  TRangeState  = (rsUnknown, rsAttribute, rsObjectValue, rsObjectEnd, rsArrayValue, rsArrayEnd);

type

  { TSynJTDSyn }

  TSynJTDSyn = class(TSynCustomHighlighter)
  private
  type
    TStrSetType = specialize TGLiteChainHashSet<string, string>;
    TStrSet     = TStrSetType.TSet;
  var
    FRange: TRangeState;
    fTokenPos :Integer;
    FTokenID: TtkTokenKind;
    FReservedAttri: TSynHighlighterAttributes;
    FAttributeAttri: TSynHighlighterAttributes;
    FCustKeyAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FIdentAttri: TSynHighlighterAttributes;
    Run :LongInt;
    fLineRef :string;
    fLine :PChar;
    fLineNumber :Integer;
    fLineLen :Integer;
    FIdentifiers: TStrSet;
    procedure CloseArrayProc;
    procedure CloseObjectProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure CRProc;
    function  IsLineEnd(_Run: Integer): Boolean;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OpenArrayProc;
    procedure OpenObjectProc;
    procedure ReservedWordProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
    function TokenComp(const aValue: shortstring): Boolean;
    function IsKeywordToken: Boolean; inline;
    function IsCustomKeyToken: Boolean; inline;
    function IsReservedToken: Boolean; inline;
    function GetStrToken: string; inline;
    function IsIdentifierToken: Boolean; inline;
  public
    procedure SetLine(const NewValue: string; LineNumber: Integer); override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string;
  public
  const
    KEYWORD_DEF_COLOR    = clBlue;
    IDENTIFIER_DEF_COLOR = clNavy;
    RESERVED_DEF_COLOR   = clBlack;
    ATTRIBUTE_DEF_COLOR  = clGreen;
    SYMBOL_DEF_COLOR     = clBlack;
    CUSTOM_KEY_DEF_COLOR = clTeal;
    class function GetKeyAttriKey: string; static;
    class function GetAttributeAttriKey: string; static;
    class function GetReservedAttriKey: string; static;
    class function GetSymbolAttriKey: string; static;
    class function GetCustomKeyAttriKey: string; static;
    class function GetIdentAttriKey: string; static;
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetToken: String; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenPos: Integer; override;
    procedure NewDoc(aNode: TJsonNode);
  published
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property AttributeAttri: TSynHighlighterAttributes read FAttributeAttri write FAttributeAttri;
    property ReservedAttri: TSynHighlighterAttributes read FReservedAttri write FReservedAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property CustomKeyAttri: TSynHighlighterAttributes read FCustKeyAttri write FCustKeyAttri;
    property IdentAttri: TSynHighlighterAttributes read FIdentAttri write FIdentAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  SYNS_FilterJson = 'JSON Files (*.json)|*.json';
  SYNS_LangJTD    = 'JSON Typedef';

{ TSynJTDSyn }
function TSynJTDSyn.IsLineEnd(_Run: Integer): Boolean;
begin
  Result := (_Run >= FLineLen) or (FLine[_Run] = #10) or (FLine[_Run] = #13);
end;

constructor TSynJTDSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  //FCaseSensitive := True;

  //keywords
  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey);
  FKeyAttri.Foreground := KEYWORD_DEF_COLOR;
  AddAttribute(FKeyAttri);

  //identifiers
  FIdentAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  FIdentAttri.Foreground := IDENTIFIER_DEF_COLOR;
  AddAttribute(FIdentAttri);

  // attributes
  FAttributeAttri := TSynHighlighterAttributes.Create(SYNS_AttrAttributeName);
  FAttributeAttri.Foreground := ATTRIBUTE_DEF_COLOR;
  AddAttribute(FAttributeAttri);

  // reserved words ("true", "false", "null", and JDT predefined types)
  FReservedAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  FReservedAttri.Foreground := RESERVED_DEF_COLOR;
  AddAttribute(FReservedAttri);

  // spaces
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(FSpaceAttri);

  // symbols
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  FSymbolAttri.Foreground := SYMBOL_DEF_COLOR;
  AddAttribute(FSymbolAttri);

  // non-standard keywords
  FCustKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrValue);
  FCustKeyAttri.Foreground := CUSTOM_KEY_DEF_COLOR;
  AddAttribute(FCustKeyAttri);

  SetAttributesOnChange(@DefHighlightChange);
  FDefaultFilter := SYNS_FilterJSON;
  FRange := rsUnknown;
end;

procedure TSynJTDSyn.CloseArrayProc;
begin
  SymbolProc;
  FRange := rsArrayEnd;
end;

procedure TSynJTDSyn.CloseObjectProc;
begin
  SymbolProc;
  FRange := rsObjectEnd;
end;

procedure TSynJTDSyn.ColonProc;
begin
  SymbolProc;
  FRange := rsObjectValue;
end;

procedure TSynJTDSyn.CommaProc;
begin
  SymbolProc;
  if (FRange = rsObjectValue) or (FRange = rsObjectEnd) or (FRange = rsArrayEnd) then
    FRange := rsAttribute;
end;

procedure TSynJTDSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then Inc(Run);
end;

procedure TSynJTDSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynJTDSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynJTDSyn.NumberProc;

  function ExpectDigit: Boolean;
  begin
    Result := FLine[Run] in  ['0'..'9'];
    while FLine[Run] in  ['0'..'9'] do
      Inc(Run);
  end;

begin
  FTokenID := tkNumber;

  if FLine[Run] = '-' then
    Inc(Run);

  // ensure that a zero is followed by a dot
  if FLine[Run] = '0' then
    if FLine[Run + 1] <> '.' then
    begin
      FTokenID := tkUnknown;
      while (FLine[Run] <> #32) and not IsLineEnd(Run) do Inc(Run);
      Exit;
    end;

  // at least any digit must appear here
  if not ExpectDigit then
  begin
    FTokenID := tkUnknown;
    while (FLine[Run] <> #32) and not IsLineEnd(Run) do Inc(Run);
    Exit;
  end;

  // check for dot
  if FLine[Run] = '.' then
  begin
    // advance
    Inc(Run);

    // at least any digit must appear after a dot!
    if not ExpectDigit then
    begin
      FTokenID := tkUnknown;
      while (FLine[Run] <> #32) and not IsLineEnd(Run) do Inc(Run);
      Exit;
    end;
  end;

  // check for an exponent
  if FLine[Run] in ['e', 'E'] then
  begin
    Inc(Run);

    // allow +/- here
    if (FLine[Run] in ['+', '-']) then
      Inc(Run);

    // at least any digit must appear here
    if not ExpectDigit then
    begin
      FTokenID := tkUnknown;
      while (FLine[Run] <> #32) and not IsLineEnd(Run) do Inc(Run);
      Exit;
    end;
  end;
end;

procedure TSynJTDSyn.OpenArrayProc;
begin
  SymbolProc;
  FRange := rsArrayValue;
end;

procedure TSynJTDSyn.OpenObjectProc;
begin
  SymbolProc;
  FRange := rsAttribute;
end;

procedure TSynJTDSyn.ReservedWordProc;

  procedure SkipToken;
  begin
    while (FLine[Run] <> #32) and (FLine[Run] <> ',') and not IsLineEnd(Run) do
      Inc(Run);
  end;

begin
  FTokenID := tkUnknown;
  case FLine[Run] of
    'n':
      if (FLine[Run + 1] = 'u') and
         (FLine[Run + 2] = 'l') and
         (FLine[Run + 3] = 'l') and not
         (FLine[Run + 4] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) then
      begin
        FTokenID := tkReserved;
        Inc(Run, 4);
      end
      else
        SkipToken;
    't':
      if (FLine[Run + 1] = 'r') and
         (FLine[Run + 2] = 'u') and
         (FLine[Run + 3] = 'e') and not
         (FLine[Run + 4] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) then
      begin
        FTokenID := tkReserved;
        Inc(Run, 4);
      end
      else
        SkipToken;
    'f':
      if (FLine[Run + 1] = 'a') and
         (FLine[Run + 2] = 'l') and
         (FLine[Run + 3] = 's') and
         (FLine[Run + 4] = 'e') and not
         (FLine[Run + 5] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) then
      begin
        FTokenID := tkReserved;
        Inc(Run, 5);
      end
      else
        SkipToken;
    else
      SkipToken;
  end;
end;

procedure TSynJTDSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynJTDSyn.StringProc;

  function IsHex(Digit: AnsiChar): Boolean; overload;
  begin
    Result := (Digit in ['0'..'9', 'A'..'F', 'a'..'f']);
  end;

  function IsHex(Digit: WideChar): Boolean; overload;
  begin
    Result := (Digit in ['0'..'9', 'A'..'F', 'a'..'f']);
  end;

begin
  FTokenID := tkString;

  repeat
    Inc(Run);
    case FLine[Run] of
      '"':
        begin
          Inc(Run);
          Break;
        end;
      '\':
        case FLine[Run + 1] of
          '"', '/', '\', 'b', 'f', 'n', 'r', 't':
            Inc(Run);
          'u':
            begin
              Inc(Run);
              if not (IsHex(FLine[Run + 1]) and IsHex(FLine[Run + 2]) and
                IsHex(FLine[Run + 3]) and IsHex(FLine[Run + 4])) then
              begin
                // a 4 hex digit is expected
                FTokenID := tkUnknown;
                while not (FLine[Run] in [#32, '"']) and not IsLineEnd(Run) do
                  Inc(Run);
                Exit;
              end;
              Inc(Run, 4);
            end;
        end;
    end;
  until IsLineEnd(Run);
end;

procedure TSynJTDSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynJTDSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

function TSynJTDSyn.TokenComp(const aValue: shortstring): Boolean;
var
  Len, I: Integer;
  p: PChar;
begin
  GetTokenEx(p, Len);
  Inc(p);
  Dec(Len, 2);
  if Length(aValue) = Len then begin
    for I := 1 to Pred(Len) do
      if p[I] <> aValue[Succ(I)] then exit(False);
    Result := True;
  end else
    Result := False;
end;

function TSynJTDSyn.IsKeywordToken: Boolean;
begin
  case FLine[GetTokenPos+1] of
    'a': Result := TokenComp('additionalProperties');
    'd': Result := TokenComp('definitions') or TokenComp('discriminator');
    'e': Result := TokenComp('elements') or TokenComp('enum');
    'm': Result := TokenComp('mapping') or TokenComp('metadata');
    'n': Result := TokenComp('nullable');
    'o': Result := TokenComp('optionalProperties');
    'p': Result := TokenComp('properties');
    'r': Result := TokenComp('ref');
    't': Result := TokenComp('type');
    'v': Result := TokenComp('values');
  else
    Result := False;
  end;
end;

function TSynJTDSyn.IsCustomKeyToken: Boolean;
begin
  case FLine[GetTokenPos+1] of
    'd': Result := TokenComp('description');
    'e': Result := TokenComp('enumDescription');
    'p': Result := TokenComp('preferredName') or TokenComp('propsDescription');
  else
    Result := False;
  end;
end;

function TSynJTDSyn.IsReservedToken: Boolean;
begin
  case FLine[GetTokenPos+1] of
    'b': Result := TokenComp('boolean');
    'f': Result := TokenComp('float32') or TokenComp('float64');
    'i': Result := TokenComp('int8') or TokenComp('int16') or TokenComp('int32');
    's': Result := TokenComp('string');
    't': Result := TokenComp('timestamp');
    'u': Result := TokenComp('uint8') or TokenComp('uint16') or TokenComp('uint32');
  else
    Result := False;
  end;
end;

function TSynJTDSyn.GetStrToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos - 2;
  SetString(Result, (FLine + fTokenPos + 1), Len);
end;

function TSynJTDSyn.IsIdentifierToken: Boolean;
begin
  Result := FIdentifiers.Contains(GetStrToken);
end;

procedure TSynJTDSyn.SetLine(const NewValue :string; LineNumber :Integer);
begin
  fLineRef := NewValue;
  fLine := PChar(fLineRef);
  fLineLen :=  Length(NewValue);
  Run := 0;
  fLineNumber := LineNumber;

  Next;
end;

procedure TSynJTDSyn.Next;
begin
  FTokenPos := Run;
  case FLine[Run] of
    #0: NullProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    #10: LFProc;
    #13: CRProc;
    '0'..'9', '-': NumberProc;
    't',
    'f',
    'n': ReservedWordProc;
    '"': StringProc;
    ':': ColonProc;
    '{': OpenObjectProc;
    '[': OpenArrayProc;
    '}': CloseObjectProc;
    ']': CloseArrayProc;
    ',': CommaProc;
    else UnknownProc;
  end;
end;

function TSynJTDSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_IDENTIFIER: Result := FIdentAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FAttributeAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
    SYN_ATTR_NUMBER: Result := FAttributeAttri;
  else
    Result := nil;
  end;
end;

function TSynJTDSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

{$PUSH}{$WARN 4055 OFF}
function TSynJTDSyn.GetRange: Pointer;
begin
  Result := Pointer(PtrUInt(FRange));
end;
{$POP}

function TSynJTDSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynJTDSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkString:
      case FRange of
        rsAttribute:
          if IsKeywordToken then
            Result := FKeyAttri
          else
            if IsCustomKeyToken then
              Result := FCustKeyAttri
            else
              if IsIdentifierToken then
                Result := FIdentAttri
              else
                Result := FAttributeAttri;
        rsObjectValue:
          if IsReservedToken then
            Result := FReservedAttri
          else
            if IsIdentifierToken then
              Result := FIdentAttri
            else
              Result := FAttributeAttri;
        rsArrayValue: Result := FIdentAttri;
      else
        Result := FAttributeAttri;
      end;
    tkReserved: Result := FReservedAttri;
    tkNumber: Result := FAttributeAttri;
    tkSpace: Result := FSpaceAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FAttributeAttri;
  else
    Result := nil;
  end;
end;

function TSynJTDSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynJTDSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

{$PUSH}{$WARN 4055 OFF}
procedure TSynJTDSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(PtrUInt(Value));
end;
{$POP}

procedure TSynJTDSyn.GetTokenEx(out TokenStart :PChar; out TokenLength :integer);
begin
  TokenLength := Run - fTokenPos;
  TokenStart  := FLine + fTokenPos;
end;

function TSynJTDSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynJTDSyn.NewDoc(aNode: TJsonNode);
var
  n: TJsonNode;
  p: TJsonNode.TPair;
begin
  FIdentifiers.Clear;
  if aNode.Find('definitions', n) then
    for p in n.Entries do
      FIdentifiers.Add(p.Key);
end;

function TSynJTDSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;


function TSynJTDSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterJSON;
end;

class function TSynJTDSyn.GetLanguageName: string;
begin
  Result := SYNS_LangJTD;
end;

function TSynJTDSyn.GetSampleSource: string;
begin
  Result :=
  '{                                         ' + LineEnding +
  '    "definitions": {                      ' + LineEnding +
  '       "Point": {                         ' + LineEnding +
  '          "properties": {                 ' + LineEnding +
  '             "x": {"type": "int32"},      ' + LineEnding +
  '             "y": {"type": "int32"}       ' + LineEnding +
  '          }                               ' + LineEnding +
  '       },                                 ' + LineEnding +
  '       "Shape": {"enum": ["circle", "ellipce", "rectangle", "triangle"]},' + LineEnding +
  '       "Figure": {                        ' + LineEnding +
  '           "properties": {                ' + LineEnding +
  '              "name": {"type": "string"}, ' + LineEnding +
  '              "shape": {"ref": "Shape"},  ' + LineEnding +
  '              "area": {"type": "float64"},' + LineEnding +
  '              "center": {"ref": "Point"}  ' + LineEnding +
  '           }                              ' + LineEnding +
  '       }                                  ' + LineEnding +
  '    },                                    ' + LineEnding +
  '    "elements": {"ref": "Figure"},        ' + LineEnding +
  '    "nullable": true,                     ' + LineEnding +
  '    "metadata": {                         ' + LineEnding +
  '       "description": "it is description" ' + LineEnding +
  '    }                                     ' + LineEnding +
  '}';
end;

class function TSynJTDSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_LangJTD;
end;

class function TSynJTDSyn.GetKeyAttriKey: string;
begin
  Result := '/keywordAttri';
end;

class function TSynJTDSyn.GetAttributeAttriKey: string;
begin
  Result := '/attributeAttri';
end;

class function TSynJTDSyn.GetReservedAttriKey: string;
begin
  Result := '/reservedAttri';
end;

class function TSynJTDSyn.GetSymbolAttriKey: string;
begin
  Result := '/symbolAttri';
end;

class function TSynJTDSyn.GetCustomKeyAttriKey: string;
begin
  Result := '/customKeywordAttri';
end;

class function TSynJTDSyn.GetIdentAttriKey: string;
begin
  Result := '/identifierAttri';
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynJTDSyn);
{$ENDIF}
end.
