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
    procedure SetNormalword(aValue: TJtdString);
    procedure SetJson(aValue: TJtdString);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    procedure Clear; override;
  { refers to "normalword" JSON property }
    property Normalword: TJtdString read FNormalword write SetNormalword;
  { refers to "json" JSON property }
    property Json: TJtdString read FJson write SetJson;
  end;

  TRootObject = class sealed(TJtdObject)
  private
    FId: TJtdString;
    FHttp: TJtdString;
    FUtf8: TJtdString;
    FWordWithTrailingInitialismId: TJtdString;
    FWordWithEmbeddedIdInitialism: TJtdString;
    FNestedIdInitialism: TNestedIdInitialism;
    procedure SetId(aValue: TJtdString);
    procedure SetHttp(aValue: TJtdString);
    procedure SetUtf8(aValue: TJtdString);
    procedure SetWordWithTrailingInitialismId(aValue: TJtdString);
    procedure SetWordWithEmbeddedIdInitialism(aValue: TJtdString);
    procedure SetNestedIdInitialism(aValue: TNestedIdInitialism);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    procedure Clear; override;
  { refers to "id" JSON property }
    property Id: TJtdString read FId write SetId;
  { refers to "http" JSON property }
    property Http: TJtdString read FHttp write SetHttp;
  { refers to "utf8" JSON property }
    property Utf8: TJtdString read FUtf8 write SetUtf8;
  { refers to "word_with_trailing_initialism_id" JSON property }
    property WordWithTrailingInitialismId: TJtdString read FWordWithTrailingInitialismId write SetWordWithTrailingInitialismId;
  { refers to "word_with_embedded_id_initialism" JSON property }
    property WordWithEmbeddedIdInitialism: TJtdString read FWordWithEmbeddedIdInitialism write SetWordWithEmbeddedIdInitialism;
  { refers to "nested_id_initialism" JSON property }
    property NestedIdInitialism: TNestedIdInitialism read FNestedIdInitialism write SetNestedIdInitialism;
  end;

implementation

{ TNestedIdInitialism }

procedure TNestedIdInitialism.SetNormalword(aValue: TJtdString);
begin
  if aValue = FNormalword then exit;
  FNormalword.Free;
  FNormalword := aValue;
end;

procedure TNestedIdInitialism.SetJson(aValue: TJtdString);
begin
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
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  for e in aNode.Entries do
    case e.Key of
      'normalword':
        if not Flags[0] then begin
          FNormalword := TJtdString(TJtdString.LoadInstance(e.Value));
          Flags[0] := True;
        end else DuplicateProp(e.Key);
      'json':
        if not Flags[1] then begin
          FJson := TJtdString(TJtdString.LoadInstance(e.Value));
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
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
    case aReader.Name of
      'normalword':
        if not Flags[0] then begin
          FNormalword := TJtdString(TJtdString.LoadInstance(aReader));
          Flags[0] := True;
        end else DuplicateProp(aReader);
      'json':
        if not Flags[1] then begin
          FJson := TJtdString(TJtdString.LoadInstance(aReader));
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

procedure TNestedIdInitialism.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName('normalword');
  Normalword.WriteJson(aWriter);
  aWriter.AddName('json');
  Json.WriteJson(aWriter);
  aWriter.EndObject;
end;

procedure TNestedIdInitialism.Clear;
begin
  FreeAndNil(FNormalword);
  FreeAndNil(FJson);
end;

{ TRootObject }

procedure TRootObject.SetId(aValue: TJtdString);
begin
  if aValue = FId then exit;
  FId.Free;
  FId := aValue;
end;

procedure TRootObject.SetHttp(aValue: TJtdString);
begin
  if aValue = FHttp then exit;
  FHttp.Free;
  FHttp := aValue;
end;

procedure TRootObject.SetUtf8(aValue: TJtdString);
begin
  if aValue = FUtf8 then exit;
  FUtf8.Free;
  FUtf8 := aValue;
end;

procedure TRootObject.SetWordWithTrailingInitialismId(aValue: TJtdString);
begin
  if aValue = FWordWithTrailingInitialismId then exit;
  FWordWithTrailingInitialismId.Free;
  FWordWithTrailingInitialismId := aValue;
end;

procedure TRootObject.SetWordWithEmbeddedIdInitialism(aValue: TJtdString);
begin
  if aValue = FWordWithEmbeddedIdInitialism then exit;
  FWordWithEmbeddedIdInitialism.Free;
  FWordWithEmbeddedIdInitialism := aValue;
end;

procedure TRootObject.SetNestedIdInitialism(aValue: TNestedIdInitialism);
begin
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
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  for e in aNode.Entries do
    case e.Key of
      'id':
        if not Flags[0] then begin
          FId := TJtdString(TJtdString.LoadInstance(e.Value));
          Flags[0] := True;
        end else DuplicateProp(e.Key);
      'http':
        if not Flags[1] then begin
          FHttp := TJtdString(TJtdString.LoadInstance(e.Value));
          Flags[1] := True;
        end else DuplicateProp(e.Key);
      'utf8':
        if not Flags[2] then begin
          FUtf8 := TJtdString(TJtdString.LoadInstance(e.Value));
          Flags[2] := True;
        end else DuplicateProp(e.Key);
      'word_with_trailing_initialism_id':
        if not Flags[3] then begin
          FWordWithTrailingInitialismId := TJtdString(TJtdString.LoadInstance(e.Value));
          Flags[3] := True;
        end else DuplicateProp(e.Key);
      'word_with_embedded_id_initialism':
        if not Flags[4] then begin
          FWordWithEmbeddedIdInitialism := TJtdString(TJtdString.LoadInstance(e.Value));
          Flags[4] := True;
        end else DuplicateProp(e.Key);
      'nested_id_initialism':
        if not Flags[5] then begin
          FNestedIdInitialism := TNestedIdInitialism(TNestedIdInitialism.LoadInstance(e.Value));
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
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
    case aReader.Name of
      'id':
        if not Flags[0] then begin
          FId := TJtdString(TJtdString.LoadInstance(aReader));
          Flags[0] := True;
        end else DuplicateProp(aReader);
      'http':
        if not Flags[1] then begin
          FHttp := TJtdString(TJtdString.LoadInstance(aReader));
          Flags[1] := True;
        end else DuplicateProp(aReader);
      'utf8':
        if not Flags[2] then begin
          FUtf8 := TJtdString(TJtdString.LoadInstance(aReader));
          Flags[2] := True;
        end else DuplicateProp(aReader);
      'word_with_trailing_initialism_id':
        if not Flags[3] then begin
          FWordWithTrailingInitialismId := TJtdString(TJtdString.LoadInstance(aReader));
          Flags[3] := True;
        end else DuplicateProp(aReader);
      'word_with_embedded_id_initialism':
        if not Flags[4] then begin
          FWordWithEmbeddedIdInitialism := TJtdString(TJtdString.LoadInstance(aReader));
          Flags[4] := True;
        end else DuplicateProp(aReader);
      'nested_id_initialism':
        if not Flags[5] then begin
          FNestedIdInitialism := TNestedIdInitialism(TNestedIdInitialism.LoadInstance(aReader));
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

procedure TRootObject.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName('id');
  Id.WriteJson(aWriter);
  aWriter.AddName('http');
  Http.WriteJson(aWriter);
  aWriter.AddName('utf8');
  Utf8.WriteJson(aWriter);
  aWriter.AddName('word_with_trailing_initialism_id');
  WordWithTrailingInitialismId.WriteJson(aWriter);
  aWriter.AddName('word_with_embedded_id_initialism');
  WordWithEmbeddedIdInitialism.WriteJson(aWriter);
  aWriter.AddName('nested_id_initialism');
  NestedIdInitialism.WriteJson(aWriter);
  aWriter.EndObject;
end;

procedure TRootObject.Clear;
begin
  FreeAndNil(FId);
  FreeAndNil(FHttp);
  FreeAndNil(FUtf8);
  FreeAndNil(FWordWithTrailingInitialismId);
  FreeAndNil(FWordWithEmbeddedIdInitialism);
  FreeAndNil(FNestedIdInitialism);
end;

end.
