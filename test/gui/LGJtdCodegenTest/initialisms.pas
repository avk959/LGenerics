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
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
    procedure ClearFields; override;
  public
  { refers to "normalword" JSON property }
    property Normalword: TJtdString read FNormalword write SetNormalword;
  { refers to "json" JSON property }
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
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
    procedure ClearFields; override;
  public
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

procedure TNestedIdInitialism.WriteFields(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('normalword');
  Normalword.WriteJson(aWriter);
  aWriter.AddName('json');
  Json.WriteJson(aWriter);
end;

procedure TNestedIdInitialism.DoClear;
begin
end;

procedure TNestedIdInitialism.ClearFields;
begin
  FNormalword.Free;
  FJson.Free;
end;

procedure TNestedIdInitialism.CreateFields;
begin
  FNormalword := TJtdString.Create;
  FJson := TJtdString.Create;
end;

{ TInitialisms }

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
procedure TInitialisms.DoReadJson(aReader: TJsonReader);
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

procedure TInitialisms.WriteFields(aWriter: TJsonStrWriter);
begin
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
end;

procedure TInitialisms.DoClear;
begin
end;

procedure TInitialisms.ClearFields;
begin
  FId.Free;
  FHttp.Free;
  FUtf8.Free;
  FWordWithTrailingInitialismId.Free;
  FWordWithEmbeddedIdInitialism.Free;
  FNestedIdInitialism.Free;
end;

procedure TInitialisms.CreateFields;
begin
  FId := TJtdString.Create;
  FHttp := TJtdString.Create;
  FUtf8 := TJtdString.Create;
  FWordWithTrailingInitialismId := TJtdString.Create;
  FWordWithEmbeddedIdInitialism := TJtdString.Create;
  FNestedIdInitialism := TNestedIdInitialism.Create;
end;

end.
