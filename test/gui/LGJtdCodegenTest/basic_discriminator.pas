{
  Source schema: basic_discriminator.jtd.json

  This unit was automatically created by JtdPasCodegen, do not edit.
}
unit basic_discriminator;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TBarBaz = class sealed(TJtdObject)
  private
    FBaz: TJtdString;
    procedure SetBaz(aValue: TJtdString);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
    property Baz: TJtdString read FBaz write SetBaz;
  end;

  TQUUX = class sealed(TJtdObject)
  private
    FQuuz: TJtdString;
    procedure SetQuuz(aValue: TJtdString);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
    property Quuz: TJtdString read FQuuz write SetQuuz;
  end;

  TBasicDiscriminator = class sealed(TJtdVariant)
  protected
    function GetBarBaz: TBarBaz;
    function GetQUUX: TQUUX;
    procedure SetBarBaz(aValue: TBarBaz);
    procedure SetQUUX(aValue: TQUUX);
    class function GetTagJsonName: string; override;
    class function ValidTagValue(const aValue: string): Boolean; override;
    class function GetInstanceClass(const aTag: string): TJtdEntityClass; override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
  { matches the tag "BAR_BAZ" }
    property BarBaz: TBarBaz read GetBarBaz write SetBarBaz;
  { matches the tag "QUUX" }
    property QUUX: TQUUX read GetQUUX write SetQUUX;
  end;

implementation

{ TBarBaz }

class function TBarBaz.GetJtdClass: TJtdEntityClass;
begin
  Result := TBarBaz;
end;

procedure TBarBaz.Clear;
begin
  FreeAndNil(FBaz);
end;

procedure TBarBaz.SetBaz(aValue: TJtdString);
begin
  if aValue = FBaz then exit;
  FBaz.Free;
  FBaz := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TBarBaz.DoReadJson(aNode: TJsonNode);
var
  p: TJsonNode.TPair;
  Flags: array[0..0] of Boolean;
  I: Integer;
begin
  if not aNode.IsObject then ReadError;
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  for p in aNode.Entries do
    case p.Key of
      'baz':
        begin
          FBaz := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[0] := True;
        end;
    else
      UnknownProp(p.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('baz');
      else
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TBarBaz.DoReadJson(aReader: TJsonReader);
var
  Flags: array[0..0] of Boolean;
  I: Integer;
begin
  if aReader.TokenKind <> tkObjectBegin then ReadError;
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReadError;
    if aReader.TokenKind = tkObjectEnd then break;
    case aReader.Name of
      'baz':
        begin
          FBaz := TJtdString(TJtdString.ReadJson(aReader));
          Flags[0] := True;
        end;
    else
      UnknownProp(aReader.Name);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('baz');
      else
      end;
end;
{$POP}

procedure TBarBaz.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName('baz');
  Baz.WriteJson(aWriter);
  aWriter.EndObject;
end;

{ TQUUX }

class function TQUUX.GetJtdClass: TJtdEntityClass;
begin
  Result := TQUUX;
end;

procedure TQUUX.Clear;
begin
  FreeAndNil(FQuuz);
end;

procedure TQUUX.SetQuuz(aValue: TJtdString);
begin
  if aValue = FQuuz then exit;
  FQuuz.Free;
  FQuuz := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TQUUX.DoReadJson(aNode: TJsonNode);
var
  p: TJsonNode.TPair;
  Flags: array[0..0] of Boolean;
  I: Integer;
begin
  if not aNode.IsObject then ReadError;
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  for p in aNode.Entries do
    case p.Key of
      'quuz':
        begin
          FQuuz := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[0] := True;
        end;
    else
      UnknownProp(p.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('quuz');
      else
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TQUUX.DoReadJson(aReader: TJsonReader);
var
  Flags: array[0..0] of Boolean;
  I: Integer;
begin
  if aReader.TokenKind <> tkObjectBegin then ReadError;
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReadError;
    if aReader.TokenKind = tkObjectEnd then break;
    case aReader.Name of
      'quuz':
        begin
          FQuuz := TJtdString(TJtdString.ReadJson(aReader));
          Flags[0] := True;
        end;
    else
      UnknownProp(aReader.Name);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('quuz');
      else
      end;
end;
{$POP}

procedure TQUUX.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName('quuz');
  Quuz.WriteJson(aWriter);
  aWriter.EndObject;
end;

{ TBasicDiscriminator }

class function TBasicDiscriminator.GetJtdClass: TJtdEntityClass;
begin
  Result := TBasicDiscriminator;
end;

class function TBasicDiscriminator.GetTagJsonName: string;
begin
  Result := 'foo';
end;

class function TBasicDiscriminator.ValidTagValue(const aValue: string): Boolean;
begin
  case aValue of
    'BAR_BAZ',
    'QUUX': Result := True;
  else
    Result := False;
  end;
end;

class function TBasicDiscriminator.GetInstanceClass(const aTag: string): TJtdEntityClass;
begin
  case aTag of
    'BAR_BAZ': Result := TBarBaz;
    'QUUX': Result := TQUUX;
  else
    Result := nil;
  end;
end;

function TBasicDiscriminator.GetBarBaz: TBarBaz;
begin
  Result := FInstance as TBarBaz;
end;

function TBasicDiscriminator.GetQUUX: TQUUX;
begin
  Result := FInstance as TQUUX;
end;

procedure TBasicDiscriminator.SetBarBaz(aValue: TBarBaz);
begin
  if aValue = FInstance then exit;
  FInstance.Free;
  FInstance := aValue;
  FTag := 'BAR_BAZ';
end;

procedure TBasicDiscriminator.SetQUUX(aValue: TQUUX);
begin
  if aValue = FInstance then exit;
  FInstance.Free;
  FInstance := aValue;
  FTag := 'QUUX';
end;

end.
