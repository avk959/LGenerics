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

  TNotnullString = TJtdString;

{ TNullString is nullable; }
  TNullString = TJtdString;

  TNotnullRefNotnullString = TNotnullString;

{ TNullRefNotnullString is nullable; }
  TNullRefNotnullString = TNotnullString;

  TNotnullRefNullString = TNullString;

{ TNullRefNullString is nullable; }
  TNullRefNullString = TNullString;

  TRootObject = class sealed(TJtdObject)
  private
    FNotnullString: TNotnullString;
    FNullString: TNullString;
    FNotnullRefNotnullString: TNotnullRefNotnullString;
    FNullRefNotnullString: TNullRefNotnullString;
    FNotnullRefNullString: TNotnullRefNullString;
    FNullRefNullString: TNullRefNullString;
    procedure SetNotnullString(aValue: TNotnullString);
    procedure SetNullString(aValue: TNullString);
    procedure SetNotnullRefNotnullString(aValue: TNotnullRefNotnullString);
    procedure SetNullRefNotnullString(aValue: TNullRefNotnullString);
    procedure SetNotnullRefNullString(aValue: TNotnullRefNullString);
    procedure SetNullRefNullString(aValue: TNullRefNullString);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    procedure Clear; override;
  { refers to "notnull_string" JSON property }
    property NotnullString: TNotnullString read FNotnullString write SetNotnullString;
  { refers to "null_string" JSON property }
    property NullString: TNullString read FNullString write SetNullString;
  { refers to "notnull_ref_notnull_string" JSON property }
    property NotnullRefNotnullString: TNotnullRefNotnullString read FNotnullRefNotnullString write SetNotnullRefNotnullString;
  { refers to "null_ref_notnull_string" JSON property }
    property NullRefNotnullString: TNullRefNotnullString read FNullRefNotnullString write SetNullRefNotnullString;
  { refers to "notnull_ref_null_string" JSON property }
    property NotnullRefNullString: TNotnullRefNullString read FNotnullRefNullString write SetNotnullRefNullString;
  { refers to "null_ref_null_string" JSON property }
    property NullRefNullString: TNullRefNullString read FNullRefNullString write SetNullRefNullString;
  end;

implementation

{ TRootObject }

procedure TRootObject.SetNotnullString(aValue: TNotnullString);
begin
  if aValue = FNotnullString then exit;
  FNotnullString.Free;
  FNotnullString := aValue;
end;

procedure TRootObject.SetNullString(aValue: TNullString);
begin
  if aValue = FNullString then exit;
  FNullString.Free;
  FNullString := aValue;
end;

procedure TRootObject.SetNotnullRefNotnullString(aValue: TNotnullRefNotnullString);
begin
  if aValue = FNotnullRefNotnullString then exit;
  FNotnullRefNotnullString.Free;
  FNotnullRefNotnullString := aValue;
end;

procedure TRootObject.SetNullRefNotnullString(aValue: TNullRefNotnullString);
begin
  if aValue = FNullRefNotnullString then exit;
  FNullRefNotnullString.Free;
  FNullRefNotnullString := aValue;
end;

procedure TRootObject.SetNotnullRefNullString(aValue: TNotnullRefNullString);
begin
  if aValue = FNotnullRefNullString then exit;
  FNotnullRefNullString.Free;
  FNotnullRefNullString := aValue;
end;

procedure TRootObject.SetNullRefNullString(aValue: TNullRefNullString);
begin
  if aValue = FNullRefNullString then exit;
  FNullRefNullString.Free;
  FNullRefNullString := aValue;
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
      'notnull_string':
        if not Flags[0] then begin
          FNotnullString := TNotnullString(TNotnullString.LoadInstance(e.Value));
          Flags[0] := True;
        end else DuplicateProp(e.Key);
      'null_string':
        if not Flags[1] then begin
          FNullString := TNullString(TNullString.LoadInstance(e.Value));
          Flags[1] := True;
        end else DuplicateProp(e.Key);
      'notnull_ref_notnull_string':
        if not Flags[2] then begin
          FNotnullRefNotnullString := TNotnullRefNotnullString(TNotnullRefNotnullString.LoadInstance(e.Value));
          Flags[2] := True;
        end else DuplicateProp(e.Key);
      'null_ref_notnull_string':
        if not Flags[3] then begin
          FNullRefNotnullString := TNullRefNotnullString(TNullRefNotnullString.LoadInstance(e.Value));
          Flags[3] := True;
        end else DuplicateProp(e.Key);
      'notnull_ref_null_string':
        if not Flags[4] then begin
          FNotnullRefNullString := TNotnullRefNullString(TNotnullRefNullString.LoadInstance(e.Value));
          Flags[4] := True;
        end else DuplicateProp(e.Key);
      'null_ref_null_string':
        if not Flags[5] then begin
          FNullRefNullString := TNullRefNullString(TNullRefNullString.LoadInstance(e.Value));
          Flags[5] := True;
        end else DuplicateProp(e.Key);
    else
      UnknownProp(e.Key);
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
      'notnull_string':
        if not Flags[0] then begin
          FNotnullString := TNotnullString(TNotnullString.LoadInstance(aReader));
          Flags[0] := True;
        end else DuplicateProp(aReader);
      'null_string':
        if not Flags[1] then begin
          FNullString := TNullString(TNullString.LoadInstance(aReader));
          Flags[1] := True;
        end else DuplicateProp(aReader);
      'notnull_ref_notnull_string':
        if not Flags[2] then begin
          FNotnullRefNotnullString := TNotnullRefNotnullString(TNotnullRefNotnullString.LoadInstance(aReader));
          Flags[2] := True;
        end else DuplicateProp(aReader);
      'null_ref_notnull_string':
        if not Flags[3] then begin
          FNullRefNotnullString := TNullRefNotnullString(TNullRefNotnullString.LoadInstance(aReader));
          Flags[3] := True;
        end else DuplicateProp(aReader);
      'notnull_ref_null_string':
        if not Flags[4] then begin
          FNotnullRefNullString := TNotnullRefNullString(TNotnullRefNullString.LoadInstance(aReader));
          Flags[4] := True;
        end else DuplicateProp(aReader);
      'null_ref_null_string':
        if not Flags[5] then begin
          FNullRefNullString := TNullRefNullString(TNullRefNullString.LoadInstance(aReader));
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

procedure TRootObject.DoWriteJson(aWriter: TJsonStrWriter);
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

procedure TRootObject.Clear;
begin
  FreeAndNil(FNotnullString);
  FreeAndNil(FNullString);
  FreeAndNil(FNotnullRefNotnullString);
  FreeAndNil(FNullRefNotnullString);
  FreeAndNil(FNotnullRefNullString);
  FreeAndNil(FNullRefNullString);
end;

end.
