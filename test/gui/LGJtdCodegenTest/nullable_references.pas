{
  Source schema: nullable_references.jtd.json

  This unit was automatically created by JtdPasCodegen, do not edit.
}
unit nullable_references;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TNotnullStringString = TJtdString;

{ TNullStringString is nullable }
  TNullStringString = TJtdString;

  TNullableReferences = class sealed(TJtdObject)
  private
    FNotnullString: TNotnullStringString;
    FNullString: TNullStringString;
    FNotnullRefNotnullString: TNotnullStringString;
    FNullRefNotnullString: TNotnullStringString;
    FNotnullRefNullString: TNullStringString;
    FNullRefNullString: TNullStringString;
    procedure SetNotnullString(aValue: TNotnullStringString);
    procedure SetNullString(aValue: TNullStringString);
    procedure SetNotnullRefNotnullString(aValue: TNotnullStringString);
    procedure SetNullRefNotnullString(aValue: TNotnullStringString);
    procedure SetNotnullRefNullString(aValue: TNullStringString);
    procedure SetNullRefNullString(aValue: TNullStringString);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
  { refers to "notnull_string" JSON property }
    property NotnullString: TNotnullStringString read FNotnullString write SetNotnullString;
  { refers to "null_string" JSON property }
    property NullString: TNullStringString read FNullString write SetNullString;
  { refers to "notnull_ref_notnull_string" JSON property }
    property NotnullRefNotnullString: TNotnullStringString read FNotnullRefNotnullString write SetNotnullRefNotnullString;
  { refers to "null_ref_notnull_string" JSON property }
    property NullRefNotnullString: TNotnullStringString read FNullRefNotnullString write SetNullRefNotnullString;
  { refers to "notnull_ref_null_string" JSON property }
    property NotnullRefNullString: TNullStringString read FNotnullRefNullString write SetNotnullRefNullString;
  { refers to "null_ref_null_string" JSON property }
    property NullRefNullString: TNullStringString read FNullRefNullString write SetNullRefNullString;
  end;

implementation

{ TNullableReferences }

class function TNullableReferences.GetJtdClass: TJtdEntityClass;
begin
  Result := TNullableReferences;
end;

procedure TNullableReferences.Clear;
begin
  FreeAndNil(FNotnullString);
  FreeAndNil(FNullString);
  FreeAndNil(FNotnullRefNotnullString);
  FreeAndNil(FNullRefNotnullString);
  FreeAndNil(FNotnullRefNullString);
  FreeAndNil(FNullRefNullString);
end;

procedure TNullableReferences.SetNotnullString(aValue: TNotnullStringString);
begin
  if aValue = FNotnullString then exit;
  FNotnullString.Free;
  FNotnullString := aValue;
end;

procedure TNullableReferences.SetNullString(aValue: TNullStringString);
begin
  if aValue = FNullString then exit;
  FNullString.Free;
  FNullString := aValue;
end;

procedure TNullableReferences.SetNotnullRefNotnullString(aValue: TNotnullStringString);
begin
  if aValue = FNotnullRefNotnullString then exit;
  FNotnullRefNotnullString.Free;
  FNotnullRefNotnullString := aValue;
end;

procedure TNullableReferences.SetNullRefNotnullString(aValue: TNotnullStringString);
begin
  if aValue = FNullRefNotnullString then exit;
  FNullRefNotnullString.Free;
  FNullRefNotnullString := aValue;
end;

procedure TNullableReferences.SetNotnullRefNullString(aValue: TNullStringString);
begin
  if aValue = FNotnullRefNullString then exit;
  FNotnullRefNullString.Free;
  FNotnullRefNullString := aValue;
end;

procedure TNullableReferences.SetNullRefNullString(aValue: TNullStringString);
begin
  if aValue = FNullRefNullString then exit;
  FNullRefNullString.Free;
  FNullRefNullString := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TNullableReferences.DoReadJson(aNode: TJsonNode);
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
      'notnull_string':
        begin
          FNotnullString := TNotnullStringString(TNotnullStringString.ReadJson(p.Value));
          Flags[0] := True;
        end;
      'null_string':
        begin
          FNullString := TNullStringString(TNullStringString.ReadJson(p.Value));
          Flags[1] := True;
        end;
      'notnull_ref_notnull_string':
        begin
          FNotnullRefNotnullString := TNotnullStringString(TNotnullStringString.ReadJson(p.Value));
          Flags[2] := True;
        end;
      'null_ref_notnull_string':
        begin
          FNullRefNotnullString := TNotnullStringString(TNotnullStringString.ReadJson(p.Value));
          Flags[3] := True;
        end;
      'notnull_ref_null_string':
        begin
          FNotnullRefNullString := TNullStringString(TNullStringString.ReadJson(p.Value));
          Flags[4] := True;
        end;
      'null_ref_null_string':
        begin
          FNullRefNullString := TNullStringString(TNullStringString.ReadJson(p.Value));
          Flags[5] := True;
        end;
    else
      UnknownProp(p.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('notnull_string');
        1: PropNotFound('null_string');
        2: PropNotFound('notnull_ref_notnull_string');
        3: PropNotFound('null_ref_notnull_string');
        4: PropNotFound('notnull_ref_null_string');
        5: PropNotFound('null_ref_null_string');
      else
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TNullableReferences.DoReadJson(aReader: TJsonReader);
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
      'notnull_string':
        begin
          FNotnullString := TNotnullStringString(TNotnullStringString.ReadJson(aReader));
          Flags[0] := True;
        end;
      'null_string':
        begin
          FNullString := TNullStringString(TNullStringString.ReadJson(aReader));
          Flags[1] := True;
        end;
      'notnull_ref_notnull_string':
        begin
          FNotnullRefNotnullString := TNotnullStringString(TNotnullStringString.ReadJson(aReader));
          Flags[2] := True;
        end;
      'null_ref_notnull_string':
        begin
          FNullRefNotnullString := TNotnullStringString(TNotnullStringString.ReadJson(aReader));
          Flags[3] := True;
        end;
      'notnull_ref_null_string':
        begin
          FNotnullRefNullString := TNullStringString(TNullStringString.ReadJson(aReader));
          Flags[4] := True;
        end;
      'null_ref_null_string':
        begin
          FNullRefNullString := TNullStringString(TNullStringString.ReadJson(aReader));
          Flags[5] := True;
        end;
    else
      UnknownProp(aReader.Name);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('notnull_string');
        1: PropNotFound('null_string');
        2: PropNotFound('notnull_ref_notnull_string');
        3: PropNotFound('null_ref_notnull_string');
        4: PropNotFound('notnull_ref_null_string');
        5: PropNotFound('null_ref_null_string');
      else
      end;
end;
{$POP}

procedure TNullableReferences.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName('notnull_string');
  NotnullString.WriteJson(aWriter);
  aWriter.AddName('null_string');
  NullString.WriteJson(aWriter);
  aWriter.AddName('notnull_ref_notnull_string');
  NotnullRefNotnullString.WriteJson(aWriter);
  aWriter.AddName('null_ref_notnull_string');
  NullRefNotnullString.WriteJson(aWriter);
  aWriter.AddName('notnull_ref_null_string');
  NotnullRefNullString.WriteJson(aWriter);
  aWriter.AddName('null_ref_null_string');
  NullRefNullString.WriteJson(aWriter);
  aWriter.EndObject;
end;

end.
