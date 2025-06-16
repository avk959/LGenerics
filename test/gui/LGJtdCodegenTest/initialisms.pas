{
  Source schema: initialisms.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit initialisms;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TNestedIdInitialism = class sealed(TJtdObject)
  private
    FNormalword: TJtdString;
    FJson: TJtdString;
    function  GetNormalword: TJtdString;
    function  GetJson: TJtdString;
    procedure SetNormalword(aValue: TJtdString);
    procedure SetJson(aValue: TJtdString);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure CreateProps; override;
    procedure ClearProps; override;
    procedure WriteProps(aWriter: TJsonStrWriter); override;
  public
  { refers to "normalword" JSON property }
    property Normalword: TJtdString read GetNormalword write SetNormalword;
  { refers to "json" JSON property }
    property Json: TJtdString read GetJson write SetJson;
  end;

  TRootObject = class sealed(TJtdObject)
  private
    FId: TJtdString;
    FHttp: TJtdString;
    FUtf8: TJtdString;
    FWordWithTrailingInitialismId: TJtdString;
    FWordWithEmbeddedIdInitialism: TJtdString;
    FNestedIdInitialism: TNestedIdInitialism;
    function  GetId: TJtdString;
    function  GetHttp: TJtdString;
    function  GetUtf8: TJtdString;
    function  GetWordWithTrailingInitialismId: TJtdString;
    function  GetWordWithEmbeddedIdInitialism: TJtdString;
    function  GetNestedIdInitialism: TNestedIdInitialism;
    procedure SetId(aValue: TJtdString);
    procedure SetHttp(aValue: TJtdString);
    procedure SetUtf8(aValue: TJtdString);
    procedure SetWordWithTrailingInitialismId(aValue: TJtdString);
    procedure SetWordWithEmbeddedIdInitialism(aValue: TJtdString);
    procedure SetNestedIdInitialism(aValue: TNestedIdInitialism);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure CreateProps; override;
    procedure ClearProps; override;
    procedure WriteProps(aWriter: TJsonStrWriter); override;
  public
  { refers to "id" JSON property }
    property Id: TJtdString read GetId write SetId;
  { refers to "http" JSON property }
    property Http: TJtdString read GetHttp write SetHttp;
  { refers to "utf8" JSON property }
    property Utf8: TJtdString read GetUtf8 write SetUtf8;
  { refers to "word_with_trailing_initialism_id" JSON property }
    property WordWithTrailingInitialismId: TJtdString read GetWordWithTrailingInitialismId write SetWordWithTrailingInitialismId;
  { refers to "word_with_embedded_id_initialism" JSON property }
    property WordWithEmbeddedIdInitialism: TJtdString read GetWordWithEmbeddedIdInitialism write SetWordWithEmbeddedIdInitialism;
  { refers to "nested_id_initialism" JSON property }
    property NestedIdInitialism: TNestedIdInitialism read GetNestedIdInitialism write SetNestedIdInitialism;
  end;

implementation

{ TNestedIdInitialism }

function TNestedIdInitialism.GetNormalword: TJtdString;
begin
  CheckNull;
  Result := FNormalword;
end;

function TNestedIdInitialism.GetJson: TJtdString;
begin
  CheckNull;
  Result := FJson;
end;

procedure TNestedIdInitialism.SetNormalword(aValue: TJtdString);
begin
  DoAssign;
  if aValue = FNormalword then exit;
  FNormalword.Free;
  FNormalword := aValue;
end;

procedure TNestedIdInitialism.SetJson(aValue: TJtdString);
begin
  DoAssign;
  if aValue = FJson then exit;
  FJson.Free;
  FJson := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TNestedIdInitialism.DoReadJson(aNode: TJsonNode);
var
  Flags: array[0..1] of Boolean;
  e: TJsonNode.TPair;
  I: Integer;
begin
  if not aNode.IsObject then ExpectObject(aNode);
  System.FillChar(Flags, SizeOf(Flags), 0);
  for e in aNode.Entries do
    case e.Key of
      'normalword':
        if not Flags[0] then begin
          FNormalword.ReadJson(e.Value);
          Flags[0] := True;
        end else DuplicateProp(e.Key);
      'json':
        if not Flags[1] then begin
          FJson.ReadJson(e.Value);
          Flags[1] := True;
        end else DuplicateProp(e.Key);
    else
      UnknownProp(e.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('normalword');
        1: PropNotFound('json');
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TNestedIdInitialism.DoReadJson(aReader: TJsonReader);
var
  Flags: array[0..1] of Boolean;
  I: Integer;
begin
  if aReader.TokenKind <> tkObjectBegin then ExpectObject(aReader);
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
    case aReader.Name of
      'normalword':
        if not Flags[0] then begin
          FNormalword.ReadJson(aReader);
          Flags[0] := True;
        end else DuplicateProp(aReader);
      'json':
        if not Flags[1] then begin
          FJson.ReadJson(aReader);
          Flags[1] := True;
        end else DuplicateProp(aReader);
    else
      UnknownProp(aReader.Name, aReader);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('normalword', aReader);
        1: PropNotFound('json', aReader);
      end;
end;
{$POP}

procedure TNestedIdInitialism.CreateProps;
begin
  FNormalword := TJtdString.Create;
  FJson := TJtdString.Create;
end;

procedure TNestedIdInitialism.ClearProps;
begin
  FNormalword.Free;
  FJson.Free;
end;

procedure TNestedIdInitialism.WriteProps(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('normalword');
  FNormalword.WriteJson(aWriter);
  aWriter.AddName('json');
  FJson.WriteJson(aWriter);
end;

{ TRootObject }

function TRootObject.GetId: TJtdString;
begin
  CheckNull;
  Result := FId;
end;

function TRootObject.GetHttp: TJtdString;
begin
  CheckNull;
  Result := FHttp;
end;

function TRootObject.GetUtf8: TJtdString;
begin
  CheckNull;
  Result := FUtf8;
end;

function TRootObject.GetWordWithTrailingInitialismId: TJtdString;
begin
  CheckNull;
  Result := FWordWithTrailingInitialismId;
end;

function TRootObject.GetWordWithEmbeddedIdInitialism: TJtdString;
begin
  CheckNull;
  Result := FWordWithEmbeddedIdInitialism;
end;

function TRootObject.GetNestedIdInitialism: TNestedIdInitialism;
begin
  CheckNull;
  Result := FNestedIdInitialism;
end;

procedure TRootObject.SetId(aValue: TJtdString);
begin
  DoAssign;
  if aValue = FId then exit;
  FId.Free;
  FId := aValue;
end;

procedure TRootObject.SetHttp(aValue: TJtdString);
begin
  DoAssign;
  if aValue = FHttp then exit;
  FHttp.Free;
  FHttp := aValue;
end;

procedure TRootObject.SetUtf8(aValue: TJtdString);
begin
  DoAssign;
  if aValue = FUtf8 then exit;
  FUtf8.Free;
  FUtf8 := aValue;
end;

procedure TRootObject.SetWordWithTrailingInitialismId(aValue: TJtdString);
begin
  DoAssign;
  if aValue = FWordWithTrailingInitialismId then exit;
  FWordWithTrailingInitialismId.Free;
  FWordWithTrailingInitialismId := aValue;
end;

procedure TRootObject.SetWordWithEmbeddedIdInitialism(aValue: TJtdString);
begin
  DoAssign;
  if aValue = FWordWithEmbeddedIdInitialism then exit;
  FWordWithEmbeddedIdInitialism.Free;
  FWordWithEmbeddedIdInitialism := aValue;
end;

procedure TRootObject.SetNestedIdInitialism(aValue: TNestedIdInitialism);
begin
  DoAssign;
  if aValue = FNestedIdInitialism then exit;
  FNestedIdInitialism.Free;
  FNestedIdInitialism := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TRootObject.DoReadJson(aNode: TJsonNode);
var
  Flags: array[0..5] of Boolean;
  e: TJsonNode.TPair;
  I: Integer;
begin
  if not aNode.IsObject then ExpectObject(aNode);
  System.FillChar(Flags, SizeOf(Flags), 0);
  for e in aNode.Entries do
    case e.Key of
      'id':
        if not Flags[0] then begin
          FId.ReadJson(e.Value);
          Flags[0] := True;
        end else DuplicateProp(e.Key);
      'http':
        if not Flags[1] then begin
          FHttp.ReadJson(e.Value);
          Flags[1] := True;
        end else DuplicateProp(e.Key);
      'utf8':
        if not Flags[2] then begin
          FUtf8.ReadJson(e.Value);
          Flags[2] := True;
        end else DuplicateProp(e.Key);
      'word_with_trailing_initialism_id':
        if not Flags[3] then begin
          FWordWithTrailingInitialismId.ReadJson(e.Value);
          Flags[3] := True;
        end else DuplicateProp(e.Key);
      'word_with_embedded_id_initialism':
        if not Flags[4] then begin
          FWordWithEmbeddedIdInitialism.ReadJson(e.Value);
          Flags[4] := True;
        end else DuplicateProp(e.Key);
      'nested_id_initialism':
        if not Flags[5] then begin
          FNestedIdInitialism.ReadJson(e.Value);
          Flags[5] := True;
        end else DuplicateProp(e.Key);
    else
      UnknownProp(e.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('id');
        1: PropNotFound('http');
        2: PropNotFound('utf8');
        3: PropNotFound('word_with_trailing_initialism_id');
        4: PropNotFound('word_with_embedded_id_initialism');
        5: PropNotFound('nested_id_initialism');
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TRootObject.DoReadJson(aReader: TJsonReader);
var
  Flags: array[0..5] of Boolean;
  I: Integer;
begin
  if aReader.TokenKind <> tkObjectBegin then ExpectObject(aReader);
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
    case aReader.Name of
      'id':
        if not Flags[0] then begin
          FId.ReadJson(aReader);
          Flags[0] := True;
        end else DuplicateProp(aReader);
      'http':
        if not Flags[1] then begin
          FHttp.ReadJson(aReader);
          Flags[1] := True;
        end else DuplicateProp(aReader);
      'utf8':
        if not Flags[2] then begin
          FUtf8.ReadJson(aReader);
          Flags[2] := True;
        end else DuplicateProp(aReader);
      'word_with_trailing_initialism_id':
        if not Flags[3] then begin
          FWordWithTrailingInitialismId.ReadJson(aReader);
          Flags[3] := True;
        end else DuplicateProp(aReader);
      'word_with_embedded_id_initialism':
        if not Flags[4] then begin
          FWordWithEmbeddedIdInitialism.ReadJson(aReader);
          Flags[4] := True;
        end else DuplicateProp(aReader);
      'nested_id_initialism':
        if not Flags[5] then begin
          FNestedIdInitialism.ReadJson(aReader);
          Flags[5] := True;
        end else DuplicateProp(aReader);
    else
      UnknownProp(aReader.Name, aReader);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('id', aReader);
        1: PropNotFound('http', aReader);
        2: PropNotFound('utf8', aReader);
        3: PropNotFound('word_with_trailing_initialism_id', aReader);
        4: PropNotFound('word_with_embedded_id_initialism', aReader);
        5: PropNotFound('nested_id_initialism', aReader);
      end;
end;
{$POP}

procedure TRootObject.CreateProps;
begin
  FId := TJtdString.Create;
  FHttp := TJtdString.Create;
  FUtf8 := TJtdString.Create;
  FWordWithTrailingInitialismId := TJtdString.Create;
  FWordWithEmbeddedIdInitialism := TJtdString.Create;
  FNestedIdInitialism := TNestedIdInitialism.Create;
end;

procedure TRootObject.ClearProps;
begin
  FId.Free;
  FHttp.Free;
  FUtf8.Free;
  FWordWithTrailingInitialismId.Free;
  FWordWithEmbeddedIdInitialism.Free;
  FNestedIdInitialism.Free;
end;

procedure TRootObject.WriteProps(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('id');
  FId.WriteJson(aWriter);
  aWriter.AddName('http');
  FHttp.WriteJson(aWriter);
  aWriter.AddName('utf8');
  FUtf8.WriteJson(aWriter);
  aWriter.AddName('word_with_trailing_initialism_id');
  FWordWithTrailingInitialismId.WriteJson(aWriter);
  aWriter.AddName('word_with_embedded_id_initialism');
  FWordWithEmbeddedIdInitialism.WriteJson(aWriter);
  aWriter.AddName('nested_id_initialism');
  FNestedIdInitialism.WriteJson(aWriter);
end;

end.
