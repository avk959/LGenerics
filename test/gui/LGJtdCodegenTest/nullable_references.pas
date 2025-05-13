{
  Source schema: nullable_references.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit nullable_references;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TNotnullStringString = TJtdString;

{ TNullStringString is nullable }
  TNullStringString = TJtdNullableString;

{ TNotnullStringString1 is nullable }
  TNotnullStringString1 = TJtdNullableString;

  TNullableReferences = class sealed(TJtdObject)
  private
    FNotnullString: TNotnullStringString;
    FNullString: TNullStringString;
    FNotnullRefNotnullString: TNotnullStringString;
    FNullRefNotnullString: TNotnullStringString1;
    FNotnullRefNullString: TNullStringString;
    FNullRefNullString: TNullStringString;
    procedure SetNotnullString(aValue: TNotnullStringString);
    procedure SetNullString(aValue: TNullStringString);
    procedure SetNotnullRefNotnullString(aValue: TNotnullStringString);
    procedure SetNullRefNotnullString(aValue: TNotnullStringString1);
    procedure SetNotnullRefNullString(aValue: TNullStringString);
    procedure SetNullRefNullString(aValue: TNullStringString);
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
    procedure ClearFields; override;
  public
  { refers to "notnull_string" JSON property }
    property NotnullString: TNotnullStringString read FNotnullString write SetNotnullString;
  { refers to "null_string" JSON property }
    property NullString: TNullStringString read FNullString write SetNullString;
  { refers to "notnull_ref_notnull_string" JSON property }
    property NotnullRefNotnullString: TNotnullStringString read FNotnullRefNotnullString write SetNotnullRefNotnullString;
  { refers to "null_ref_notnull_string" JSON property }
    property NullRefNotnullString: TNotnullStringString1 read FNullRefNotnullString write SetNullRefNotnullString;
  { refers to "notnull_ref_null_string" JSON property }
    property NotnullRefNullString: TNullStringString read FNotnullRefNullString write SetNotnullRefNullString;
  { refers to "null_ref_null_string" JSON property }
    property NullRefNullString: TNullStringString read FNullRefNullString write SetNullRefNullString;
  end;

implementation

{ TNullableReferences }

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

procedure TNullableReferences.SetNullRefNotnullString(aValue: TNotnullStringString1);
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
procedure TNullableReferences.DoReadJson(aReader: TJsonReader);
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
      'notnull_string':
        if not Flags[0] then begin
          FNotnullString.ReadJson(aReader);
          Flags[0] := True;
        end else DuplicateProp(aReader);
      'null_string':
        if not Flags[1] then begin
          FNullString.ReadJson(aReader);
          Flags[1] := True;
        end else DuplicateProp(aReader);
      'notnull_ref_notnull_string':
        if not Flags[2] then begin
          FNotnullRefNotnullString.ReadJson(aReader);
          Flags[2] := True;
        end else DuplicateProp(aReader);
      'null_ref_notnull_string':
        if not Flags[3] then begin
          FNullRefNotnullString.ReadJson(aReader);
          Flags[3] := True;
        end else DuplicateProp(aReader);
      'notnull_ref_null_string':
        if not Flags[4] then begin
          FNotnullRefNullString.ReadJson(aReader);
          Flags[4] := True;
        end else DuplicateProp(aReader);
      'null_ref_null_string':
        if not Flags[5] then begin
          FNullRefNullString.ReadJson(aReader);
          Flags[5] := True;
        end else DuplicateProp(aReader);
    else
      UnknownProp(aReader.Name, aReader);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('notnull_string', aReader);
        1: PropNotFound('null_string', aReader);
        2: PropNotFound('notnull_ref_notnull_string', aReader);
        3: PropNotFound('null_ref_notnull_string', aReader);
        4: PropNotFound('notnull_ref_null_string', aReader);
        5: PropNotFound('null_ref_null_string', aReader);
      end;
end;
{$POP}

procedure TNullableReferences.WriteFields(aWriter: TJsonStrWriter);
begin
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
end;

procedure TNullableReferences.DoClear;
begin
end;

procedure TNullableReferences.ClearFields;
begin
  FNotnullString.Free;
  FNullString.Free;
  FNotnullRefNotnullString.Free;
  FNullRefNotnullString.Free;
  FNotnullRefNullString.Free;
  FNullRefNullString.Free;
end;

procedure TNullableReferences.CreateFields;
begin
  FNotnullString := TNotnullStringString.Create;
  FNullString := TNullStringString.Create;
  FNotnullRefNotnullString := TNotnullStringString.Create;
  FNullRefNotnullString := TNotnullStringString1.Create;
  FNotnullRefNullString := TNullStringString.Create;
  FNullRefNullString := TNullStringString.Create;
end;

end.
