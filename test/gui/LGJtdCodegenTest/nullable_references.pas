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
    function  GetNotnullString: TNotnullString;
    function  GetNullString: TNullString;
    function  GetNotnullRefNotnullString: TNotnullRefNotnullString;
    function  GetNullRefNotnullString: TNullRefNotnullString;
    function  GetNotnullRefNullString: TNotnullRefNullString;
    function  GetNullRefNullString: TNullRefNullString;
    procedure SetNotnullString(aValue: TNotnullString);
    procedure SetNullString(aValue: TNullString);
    procedure SetNotnullRefNotnullString(aValue: TNotnullRefNotnullString);
    procedure SetNullRefNotnullString(aValue: TNullRefNotnullString);
    procedure SetNotnullRefNullString(aValue: TNotnullRefNullString);
    procedure SetNullRefNullString(aValue: TNullRefNullString);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure CreateProps; override;
    procedure ClearProps; override;
    procedure WriteProps(aWriter: TJsonStrWriter); override;
  public
  { refers to "notnull_string" JSON property }
    property NotnullString: TNotnullString read GetNotnullString write SetNotnullString;
  { refers to "null_string" JSON property }
    property NullString: TNullString read GetNullString write SetNullString;
  { refers to "notnull_ref_notnull_string" JSON property }
    property NotnullRefNotnullString: TNotnullRefNotnullString read GetNotnullRefNotnullString write SetNotnullRefNotnullString;
  { refers to "null_ref_notnull_string" JSON property }
    property NullRefNotnullString: TNullRefNotnullString read GetNullRefNotnullString write SetNullRefNotnullString;
  { refers to "notnull_ref_null_string" JSON property }
    property NotnullRefNullString: TNotnullRefNullString read GetNotnullRefNullString write SetNotnullRefNullString;
  { refers to "null_ref_null_string" JSON property }
    property NullRefNullString: TNullRefNullString read GetNullRefNullString write SetNullRefNullString;
  end;

implementation

{ TRootObject }

function TRootObject.GetNotnullString: TNotnullString;
begin
  CheckNull;
  Result := FNotnullString;
end;

function TRootObject.GetNullString: TNullString;
begin
  CheckNull;
  Result := FNullString;
end;

function TRootObject.GetNotnullRefNotnullString: TNotnullRefNotnullString;
begin
  CheckNull;
  Result := FNotnullRefNotnullString;
end;

function TRootObject.GetNullRefNotnullString: TNullRefNotnullString;
begin
  CheckNull;
  Result := FNullRefNotnullString;
end;

function TRootObject.GetNotnullRefNullString: TNotnullRefNullString;
begin
  CheckNull;
  Result := FNotnullRefNullString;
end;

function TRootObject.GetNullRefNullString: TNullRefNullString;
begin
  CheckNull;
  Result := FNullRefNullString;
end;

procedure TRootObject.SetNotnullString(aValue: TNotnullString);
begin
  DoAssign;
  if aValue = FNotnullString then exit;
  FNotnullString.Free;
  FNotnullString := aValue;
end;

procedure TRootObject.SetNullString(aValue: TNullString);
begin
  DoAssign;
  if aValue = FNullString then exit;
  FNullString.Free;
  FNullString := aValue;
end;

procedure TRootObject.SetNotnullRefNotnullString(aValue: TNotnullRefNotnullString);
begin
  DoAssign;
  if aValue = FNotnullRefNotnullString then exit;
  FNotnullRefNotnullString.Free;
  FNotnullRefNotnullString := aValue;
end;

procedure TRootObject.SetNullRefNotnullString(aValue: TNullRefNotnullString);
begin
  DoAssign;
  if aValue = FNullRefNotnullString then exit;
  FNullRefNotnullString.Free;
  FNullRefNotnullString := aValue;
end;

procedure TRootObject.SetNotnullRefNullString(aValue: TNotnullRefNullString);
begin
  DoAssign;
  if aValue = FNotnullRefNullString then exit;
  FNotnullRefNullString.Free;
  FNotnullRefNullString := aValue;
end;

procedure TRootObject.SetNullRefNullString(aValue: TNullRefNullString);
begin
  DoAssign;
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
  System.FillChar(Flags, SizeOf(Flags), 0);
  for e in aNode.Entries do
    case e.Key of
      'notnull_string':
        if not Flags[0] then begin
          FNotnullString.ReadJson(e.Value);
          Flags[0] := True;
        end else DuplicateProp(e.Key);
      'null_string':
        if not Flags[1] then begin
          FNullString.ReadJson(e.Value);
          Flags[1] := True;
        end else DuplicateProp(e.Key);
      'notnull_ref_notnull_string':
        if not Flags[2] then begin
          FNotnullRefNotnullString.ReadJson(e.Value);
          Flags[2] := True;
        end else DuplicateProp(e.Key);
      'null_ref_notnull_string':
        if not Flags[3] then begin
          FNullRefNotnullString.ReadJson(e.Value);
          Flags[3] := True;
        end else DuplicateProp(e.Key);
      'notnull_ref_null_string':
        if not Flags[4] then begin
          FNotnullRefNullString.ReadJson(e.Value);
          Flags[4] := True;
        end else DuplicateProp(e.Key);
      'null_ref_null_string':
        if not Flags[5] then begin
          FNullRefNullString.ReadJson(e.Value);
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

procedure TRootObject.CreateProps;
begin
  FNotnullString := TNotnullString.Create;
  FNullString := TNullString.Create;
  FNotnullRefNotnullString := TNotnullRefNotnullString.Create;
  FNullRefNotnullString := TNullRefNotnullString.Create;
  FNotnullRefNullString := TNotnullRefNullString.Create;
  FNullRefNullString := TNullRefNullString.Create;
end;

procedure TRootObject.ClearProps;
begin
  FNotnullString.Free;
  FNullString.Free;
  FNotnullRefNotnullString.Free;
  FNullRefNotnullString.Free;
  FNotnullRefNullString.Free;
  FNullRefNullString.Free;
end;

procedure TRootObject.WriteProps(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('notnull_string');
  FNotnullString.WriteJson(aWriter);
  aWriter.AddName('null_string');
  FNullString.WriteJson(aWriter);
  aWriter.AddName('notnull_ref_notnull_string');
  FNotnullRefNotnullString.WriteJson(aWriter);
  aWriter.AddName('null_ref_notnull_string');
  FNullRefNotnullString.WriteJson(aWriter);
  aWriter.AddName('notnull_ref_null_string');
  FNotnullRefNullString.WriteJson(aWriter);
  aWriter.AddName('null_ref_null_string');
  FNullRefNullString.WriteJson(aWriter);
end;

end.
