{
  Source schema: basic_discriminator.jtd.json

  This unit was automatically created by JtdPasCodegen.
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
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
    procedure ClearFields; override;
  public
  { refers to "baz" JSON property }
    property Baz: TJtdString read FBaz write SetBaz;
  end;

  TQUUX = class sealed(TJtdObject)
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

  TFooUnion = class sealed(TJtdUnion)
  protected
    function GetBarBaz: TBarBaz;
    function GetQUUX: TQUUX;
    procedure SetBarBaz(aValue: TBarBaz);
    procedure SetQUUX(aValue: TQUUX);
    class function GetTagJsonName: string; override;
    class function GetInstanceClass(const aTag: string): TJtdEntityClass; override;
  public
  { matches the "BAR_BAZ" tag }
    property BarBaz: TBarBaz read GetBarBaz write SetBarBaz;
  { matches the "QUUX" tag }
    property QUUX: TQUUX read GetQUUX write SetQUUX;
  end;

implementation

{ TBarBaz }

procedure TBarBaz.SetBaz(aValue: TJtdString);
begin
  if aValue = FBaz then exit;
  FBaz.Free;
  FBaz := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TBarBaz.DoReadJson(aReader: TJsonReader);
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

procedure TBarBaz.WriteFields(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('baz');
  Baz.WriteJson(aWriter);
end;

procedure TBarBaz.DoClear;
begin
end;

procedure TBarBaz.ClearFields;
begin
  FBaz.Free;
end;

procedure TBarBaz.CreateFields;
begin
  FBaz := TJtdString.Create;
end;

{ TQUUX }

procedure TQUUX.SetQuuz(aValue: TJtdString);
begin
  if aValue = FQuuz then exit;
  FQuuz.Free;
  FQuuz := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TQUUX.DoReadJson(aReader: TJsonReader);
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

procedure TQUUX.WriteFields(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('quuz');
  Quuz.WriteJson(aWriter);
end;

procedure TQUUX.DoClear;
begin
end;

procedure TQUUX.ClearFields;
begin
  FQuuz.Free;
end;

procedure TQUUX.CreateFields;
begin
  FQuuz := TJtdString.Create;
end;

class function TFooUnion.GetTagJsonName: string;
begin
  Result := 'foo';
end;

class function TFooUnion.GetInstanceClass(const aTag: string): TJtdEntityClass;
begin
  case aTag of
    'BAR_BAZ': Result := TBarBaz;
    'QUUX': Result := TQUUX;
  else
    Result := nil;
  end;
end;

function TFooUnion.GetBarBaz: TBarBaz;
begin
  Result := FInstance as TBarBaz;
end;

function TFooUnion.GetQUUX: TQUUX;
begin
  Result := FInstance as TQUUX;
end;

procedure TFooUnion.SetBarBaz(aValue: TBarBaz);
begin
  if aValue = FInstance then exit;
  FInstance.Free;
  FInstance := aValue;
  FTag := 'BAR_BAZ';
end;

procedure TFooUnion.SetQUUX(aValue: TQUUX);
begin
  if aValue = FInstance then exit;
  FInstance.Free;
  FInstance := aValue;
  FTag := 'QUUX';
end;

end.
