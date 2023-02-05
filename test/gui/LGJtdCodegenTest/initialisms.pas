{
  Source schema: initialisms.jtd.json

  This unit was automatically created by JtdPasCodegen, do not edit.
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
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
    property Normalword: TJtdString read FNormalword write SetNormalword;
    property Json: TJtdString read FJson write SetJson;
  end;

  TInitialisms = class sealed(TJtdObject)
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
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
    property Id: TJtdString read FId write SetId;
    property Http: TJtdString read FHttp write SetHttp;
    property Utf8: TJtdString read FUtf8 write SetUtf8;
    property WordWithTrailingInitialismId: TJtdString read FWordWithTrailingInitialismId write SetWordWithTrailingInitialismId;
    property WordWithEmbeddedIdInitialism: TJtdString read FWordWithEmbeddedIdInitialism write SetWordWithEmbeddedIdInitialism;
    property NestedIdInitialism: TNestedIdInitialism read FNestedIdInitialism write SetNestedIdInitialism;
  end;

implementation

{ TNestedIdInitialism }

class function TNestedIdInitialism.GetJtdClass: TJtdEntityClass;
begin
  Result := TNestedIdInitialism;
end;

procedure TNestedIdInitialism.Clear;
begin
  FreeAndNil(FNormalword);
  FreeAndNil(FJson);
end;

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
  p: TJsonNode.TPair;
  Flags: array[0..1] of Boolean;
  I: Integer;
begin
  if not aNode.IsObject then ReadError;
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  for p in aNode.Entries do
    case p.Key of
      'normalword':
        begin
          FNormalword := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[0] := True;
        end;
      'json':
        begin
          FJson := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[1] := True;
        end;
    else
      UnknownProp(p.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('normalword');
        1: PropNotFound('json');
      else
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TNestedIdInitialism.DoReadJson(aReader: TJsonReader);
var
  Flags: array[0..1] of Boolean;
  I: Integer;
begin
  if aReader.TokenKind <> tkObjectBegin then ReadError;
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReadError;
    if aReader.TokenKind = tkObjectEnd then break;
    case aReader.Name of
      'normalword':
        begin
          FNormalword := TJtdString(TJtdString.ReadJson(aReader));
          Flags[0] := True;
        end;
      'json':
        begin
          FJson := TJtdString(TJtdString.ReadJson(aReader));
          Flags[1] := True;
        end;
    else
      UnknownProp(aReader.Name);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('normalword');
        1: PropNotFound('json');
      else
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

{ TInitialisms }

class function TInitialisms.GetJtdClass: TJtdEntityClass;
begin
  Result := TInitialisms;
end;

procedure TInitialisms.Clear;
begin
  FreeAndNil(FId);
  FreeAndNil(FHttp);
  FreeAndNil(FUtf8);
  FreeAndNil(FWordWithTrailingInitialismId);
  FreeAndNil(FWordWithEmbeddedIdInitialism);
  FreeAndNil(FNestedIdInitialism);
end;

procedure TInitialisms.SetId(aValue: TJtdString);
begin
  if aValue = FId then exit;
  FId.Free;
  FId := aValue;
end;

procedure TInitialisms.SetHttp(aValue: TJtdString);
begin
  if aValue = FHttp then exit;
  FHttp.Free;
  FHttp := aValue;
end;

procedure TInitialisms.SetUtf8(aValue: TJtdString);
begin
  if aValue = FUtf8 then exit;
  FUtf8.Free;
  FUtf8 := aValue;
end;

procedure TInitialisms.SetWordWithTrailingInitialismId(aValue: TJtdString);
begin
  if aValue = FWordWithTrailingInitialismId then exit;
  FWordWithTrailingInitialismId.Free;
  FWordWithTrailingInitialismId := aValue;
end;

procedure TInitialisms.SetWordWithEmbeddedIdInitialism(aValue: TJtdString);
begin
  if aValue = FWordWithEmbeddedIdInitialism then exit;
  FWordWithEmbeddedIdInitialism.Free;
  FWordWithEmbeddedIdInitialism := aValue;
end;

procedure TInitialisms.SetNestedIdInitialism(aValue: TNestedIdInitialism);
begin
  if aValue = FNestedIdInitialism then exit;
  FNestedIdInitialism.Free;
  FNestedIdInitialism := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TInitialisms.DoReadJson(aNode: TJsonNode);
var
  p: TJsonNode.TPair;
  Flags: array[0..5] of Boolean;
  I: Integer;
begin
  if not aNode.IsObject then ReadError;
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  for p in aNode.Entries do
    case p.Key of
      'id':
        begin
          FId := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[0] := True;
        end;
      'http':
        begin
          FHttp := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[1] := True;
        end;
      'utf8':
        begin
          FUtf8 := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[2] := True;
        end;
      'word_with_trailing_initialism_id':
        begin
          FWordWithTrailingInitialismId := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[3] := True;
        end;
      'word_with_embedded_id_initialism':
        begin
          FWordWithEmbeddedIdInitialism := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[4] := True;
        end;
      'nested_id_initialism':
        begin
          FNestedIdInitialism := TNestedIdInitialism(TNestedIdInitialism.ReadJson(p.Value));
          Flags[5] := True;
        end;
    else
      UnknownProp(p.Key);
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
      else
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TInitialisms.DoReadJson(aReader: TJsonReader);
var
  Flags: array[0..5] of Boolean;
  I: Integer;
begin
  if aReader.TokenKind <> tkObjectBegin then ReadError;
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReadError;
    if aReader.TokenKind = tkObjectEnd then break;
    case aReader.Name of
      'id':
        begin
          FId := TJtdString(TJtdString.ReadJson(aReader));
          Flags[0] := True;
        end;
      'http':
        begin
          FHttp := TJtdString(TJtdString.ReadJson(aReader));
          Flags[1] := True;
        end;
      'utf8':
        begin
          FUtf8 := TJtdString(TJtdString.ReadJson(aReader));
          Flags[2] := True;
        end;
      'word_with_trailing_initialism_id':
        begin
          FWordWithTrailingInitialismId := TJtdString(TJtdString.ReadJson(aReader));
          Flags[3] := True;
        end;
      'word_with_embedded_id_initialism':
        begin
          FWordWithEmbeddedIdInitialism := TJtdString(TJtdString.ReadJson(aReader));
          Flags[4] := True;
        end;
      'nested_id_initialism':
        begin
          FNestedIdInitialism := TNestedIdInitialism(TNestedIdInitialism.ReadJson(aReader));
          Flags[5] := True;
        end;
    else
      UnknownProp(aReader.Name);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('id');
        1: PropNotFound('http');
        2: PropNotFound('utf8');
        3: PropNotFound('word_with_trailing_initialism_id');
        4: PropNotFound('word_with_embedded_id_initialism');
        5: PropNotFound('nested_id_initialism');
      else
      end;
end;
{$POP}

procedure TInitialisms.DoWriteJson(aWriter: TJsonStrWriter);
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

end.
