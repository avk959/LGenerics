{
  Source schema: nullable_discriminator.jtd.json

  This unit was automatically created by JtdPasCodegen.
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
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
    procedure ClearFields; override;
  public
  { refers to "baz" JSON property }
    property Baz: TJtdString read FBaz write SetBaz;
  end;

  TQuux = class sealed(TJtdObject)
  private
    FQuuz: TJtdString;
    procedure SetQuuz(aValue: TJtdString);
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
    procedure ClearFields; override;
  public
  { refers to "quuz" JSON property }
    property Quuz: TJtdString read FQuuz write SetQuuz;
  end;

{ TNullableDiscriminator is nullable }
  TNullableDiscriminator = class sealed(TJtdNullableUnion)
  protected
    function GetBar: TBar;
    function GetQuux: TQuux;
    procedure SetBar(aValue: TBar);
    procedure SetQuux(aValue: TQuux);
    class function GetTagJsonName: string; override;
    class function GetInstanceClass(const aTag: string): TJtdEntityClass; override;
  public
  { matches the "bar" tag }
    property Bar: TBar read GetBar write SetBar;
  { matches the "quux" tag }
    property Quux: TQuux read GetQuux write SetQuux;
  end;

implementation

{ TBar }

procedure TBar.SetBaz(aValue: TJtdString);
begin
  if aValue = FBaz then exit;
  FBaz.Free;
  FBaz := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TBar.DoReadJson(aReader: TJsonReader);
var
  Flags: array[0..0] of Boolean;
  I: Integer;
begin
  if aReader.TokenKind <> tkObjectBegin then ExpectObject(aReader);
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
    case aReader.Name of
      'baz':
        if not Flags[0] then begin
          FBaz.ReadJson(aReader);
          Flags[0] := True;
        end else DuplicateProp(aReader);
    else
      UnknownProp(aReader.Name, aReader);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('baz', aReader);
      end;
end;
{$POP}

procedure TBar.WriteFields(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('baz');
  Baz.WriteJson(aWriter);
end;

procedure TBar.DoClear;
begin
end;

procedure TBar.ClearFields;
begin
  FBaz.Free;
end;

procedure TBar.CreateFields;
begin
  FBaz := TJtdString.Create;
end;

{ TQuux }

procedure TQuux.SetQuuz(aValue: TJtdString);
begin
  if aValue = FQuuz then exit;
  FQuuz.Free;
  FQuuz := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TQuux.DoReadJson(aReader: TJsonReader);
var
  Flags: array[0..0] of Boolean;
  I: Integer;
begin
  if aReader.TokenKind <> tkObjectBegin then ExpectObject(aReader);
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
    case aReader.Name of
      'quuz':
        if not Flags[0] then begin
          FQuuz.ReadJson(aReader);
          Flags[0] := True;
        end else DuplicateProp(aReader);
    else
      UnknownProp(aReader.Name, aReader);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('quuz', aReader);
      end;
end;
{$POP}

procedure TQuux.WriteFields(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('quuz');
  Quuz.WriteJson(aWriter);
end;

procedure TQuux.DoClear;
begin
end;

procedure TQuux.ClearFields;
begin
  FQuuz.Free;
end;

procedure TQuux.CreateFields;
begin
  FQuuz := TJtdString.Create;
end;

class function TNullableDiscriminator.GetTagJsonName: string;
begin
  Result := 'foo';
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
  CheckNull;
  Result := FInstance as TBar;
end;

function TNullableDiscriminator.GetQuux: TQuux;
begin
  CheckNull;
  Result := FInstance as TQuux;
end;

procedure TNullableDiscriminator.SetBar(aValue: TBar);
begin
  if aValue = FInstance then exit;
  FInstance.Free;
  FInstance := aValue;
  FTag := 'bar';
  DoAssign;
end;

procedure TNullableDiscriminator.SetQuux(aValue: TQuux);
begin
  if aValue = FInstance then exit;
  FInstance.Free;
  FInstance := aValue;
  FTag := 'quux';
  DoAssign;
end;

end.
