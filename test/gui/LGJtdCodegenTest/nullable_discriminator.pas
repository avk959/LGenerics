{
  Source schema: nullable_discriminator.jtd.json

  This unit was automatically created by JtdPasCodegen, do not edit.
}
unit nullable_discriminator;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TBar = class sealed(TJtdObject)
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
  { refers to "baz" JSON property }
    property Baz: TJtdString read FBaz write SetBaz;
  end;

  TQuux = class sealed(TJtdObject)
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
  { refers to "quuz" JSON property }
    property Quuz: TJtdString read FQuuz write SetQuuz;
  end;

{ TNullableDiscriminator is nullable }
  TNullableDiscriminator = class sealed(TJtdVariant)
  protected
    function GetBar: TBar;
    function GetQuux: TQuux;
    procedure SetBar(aValue: TBar);
    procedure SetQuux(aValue: TQuux);
    class function GetTagJsonName: string; override;
    class function ValidTagValue(const aValue: string): Boolean; override;
    class function GetInstanceClass(const aTag: string): TJtdEntityClass; override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
  { matches the "bar" tag }
    property Bar: TBar read GetBar write SetBar;
  { matches the "quux" tag }
    property Quux: TQuux read GetQuux write SetQuux;
  end;

implementation

{ TBar }

class function TBar.GetJtdClass: TJtdEntityClass;
begin
  Result := TBar;
end;

procedure TBar.Clear;
begin
  FreeAndNil(FBaz);
end;

procedure TBar.SetBaz(aValue: TJtdString);
begin
  if aValue = FBaz then exit;
  FBaz.Free;
  FBaz := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TBar.DoReadJson(aNode: TJsonNode);
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
procedure TBar.DoReadJson(aReader: TJsonReader);
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

procedure TBar.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName('baz');
  Baz.WriteJson(aWriter);
  aWriter.EndObject;
end;

{ TQuux }

class function TQuux.GetJtdClass: TJtdEntityClass;
begin
  Result := TQuux;
end;

procedure TQuux.Clear;
begin
  FreeAndNil(FQuuz);
end;

procedure TQuux.SetQuuz(aValue: TJtdString);
begin
  if aValue = FQuuz then exit;
  FQuuz.Free;
  FQuuz := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TQuux.DoReadJson(aNode: TJsonNode);
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
procedure TQuux.DoReadJson(aReader: TJsonReader);
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

procedure TQuux.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName('quuz');
  Quuz.WriteJson(aWriter);
  aWriter.EndObject;
end;

{ TNullableDiscriminator }

class function TNullableDiscriminator.GetJtdClass: TJtdEntityClass;
begin
  Result := TNullableDiscriminator;
end;

class function TNullableDiscriminator.GetTagJsonName: string;
begin
  Result := 'foo';
end;

class function TNullableDiscriminator.ValidTagValue(const aValue: string): Boolean;
begin
  case aValue of
    'bar',
    'quux': Result := True;
  else
    Result := False;
  end;
end;

class function TNullableDiscriminator.GetInstanceClass(const aTag: string): TJtdEntityClass;
begin
  case aTag of
    'bar': Result := TBar;
    'quux': Result := TQuux;
  else
    Result := nil;
  end;
end;

function TNullableDiscriminator.GetBar: TBar;
begin
  Result := FInstance as TBar;
end;

function TNullableDiscriminator.GetQuux: TQuux;
begin
  Result := FInstance as TQuux;
end;

procedure TNullableDiscriminator.SetBar(aValue: TBar);
begin
  if aValue = FInstance then exit;
  FInstance.Free;
  FInstance := aValue;
  FTag := 'bar';
end;

procedure TNullableDiscriminator.SetQuux(aValue: TQuux);
begin
  if aValue = FInstance then exit;
  FInstance.Free;
  FInstance := aValue;
  FTag := 'quux';
end;

end.
