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

  TBarbaz = class sealed(TJtdObject)
  private
    FBaz: TJtdString;
    function  GetBaz: TJtdString;
    procedure SetBaz(aValue: TJtdString);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure CreateProps; override;
    procedure ClearProps; override;
    procedure WriteProps(aWriter: TJsonStrWriter); override;
  public
  { refers to "baz" JSON property }
    property Baz: TJtdString read GetBaz write SetBaz;
  end;

  TQuux = class sealed(TJtdObject)
  private
    FQuuz: TJtdString;
    function  GetQuuz: TJtdString;
    procedure SetQuuz(aValue: TJtdString);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure CreateProps; override;
    procedure ClearProps; override;
    procedure WriteProps(aWriter: TJsonStrWriter); override;
  public
  { refers to "quuz" JSON property }
    property Quuz: TJtdString read GetQuuz write SetQuuz;
  end;

  TRootObject = class sealed(TJtdUnion)
  protected
    function GetBarBaz: TBarbaz;
    function GetQuux: TQuux;
    procedure SetBarBaz(aValue: TBarbaz);
    procedure SetQuux(aValue: TQuux);
    class function GetTagJsonName: string; override;
    class function GetInstanceClass(const aTag: string): TJtdEntityClass; override;
  public
  { matches the "BAR_BAZ" tag; }
    property BarBaz: TBarbaz read GetBarBaz write SetBarBaz;
  { matches the "QUUX" tag; }
    property Quux: TQuux read GetQuux write SetQuux;
  end;

implementation

{ TBarbaz }

function TBarbaz.GetBaz: TJtdString;
begin
  CheckNull;
  Result := FBaz;
end;

procedure TBarbaz.SetBaz(aValue: TJtdString);
begin
  DoAssign;
  if aValue = FBaz then exit;
  FBaz.Free;
  FBaz := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TBarbaz.DoReadJson(aNode: TJsonNode);
var
  Flags: array[0..0] of Boolean;
  e: TJsonNode.TPair;
  I: Integer;
begin
  if not aNode.IsObject then ExpectObject(aNode);
  System.FillChar(Flags, SizeOf(Flags), 0);
  for e in aNode.Entries do
    case e.Key of
      'baz':
        if not Flags[0] then begin
          FBaz.ReadJson(e.Value);
          Flags[0] := True;
        end else DuplicateProp(e.Key);
    else
      UnknownProp(e.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('baz');
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TBarbaz.DoReadJson(aReader: TJsonReader);
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

procedure TBarbaz.CreateProps;
begin
  FBaz := TJtdString.Create;
end;

procedure TBarbaz.ClearProps;
begin
  FBaz.Free;
end;

procedure TBarbaz.WriteProps(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('baz');
  FBaz.WriteJson(aWriter);
end;

{ TQuux }

function TQuux.GetQuuz: TJtdString;
begin
  CheckNull;
  Result := FQuuz;
end;

procedure TQuux.SetQuuz(aValue: TJtdString);
begin
  DoAssign;
  if aValue = FQuuz then exit;
  FQuuz.Free;
  FQuuz := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TQuux.DoReadJson(aNode: TJsonNode);
var
  Flags: array[0..0] of Boolean;
  e: TJsonNode.TPair;
  I: Integer;
begin
  if not aNode.IsObject then ExpectObject(aNode);
  System.FillChar(Flags, SizeOf(Flags), 0);
  for e in aNode.Entries do
    case e.Key of
      'quuz':
        if not Flags[0] then begin
          FQuuz.ReadJson(e.Value);
          Flags[0] := True;
        end else DuplicateProp(e.Key);
    else
      UnknownProp(e.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('quuz');
      end;
end;
{$POP}

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

procedure TQuux.CreateProps;
begin
  FQuuz := TJtdString.Create;
end;

procedure TQuux.ClearProps;
begin
  FQuuz.Free;
end;

procedure TQuux.WriteProps(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('quuz');
  FQuuz.WriteJson(aWriter);
end;

class function TRootObject.GetTagJsonName: string;
begin
  Result := 'foo';
end;

class function TRootObject.GetInstanceClass(const aTag: string): TJtdEntityClass;
begin
  case aTag of
    'BAR_BAZ': Result := TBarbaz;
    'QUUX': Result := TQuux;
  else
    Result := nil;
  end;
end;

function TRootObject.GetBarBaz: TBarbaz;
begin
  CheckNull;
  Result := FInstance as TBarbaz;
end;

function TRootObject.GetQuux: TQuux;
begin
  CheckNull;
  Result := FInstance as TQuux;
end;

procedure TRootObject.SetBarBaz(aValue: TBarbaz);
begin
  DoAssign;
  if aValue = FInstance then exit;
  FInstance.Free;
  FInstance := aValue;
  FTag := 'BAR_BAZ';
end;

procedure TRootObject.SetQuux(aValue: TQuux);
begin
  DoAssign;
  if aValue = FInstance then exit;
  FInstance.Free;
  FInstance := aValue;
  FTag := 'QUUX';
end;

end.
