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

  { TBarBaz }
  TBarBaz = class sealed(TJtdObject)
  private
    FBaz: TJtdString;
    procedure SetBaz(aValue: TJtdString);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteProps(aWriter: TJsonStrWriter); override;
  public
    procedure Clear; override;
  { refers to "baz" JSON property }
    property Baz: TJtdString read FBaz write SetBaz;
  end;

  { TQuux }
  TQuux = class sealed(TJtdObject)
  private
    FQuuz: TJtdString;
    procedure SetQuuz(aValue: TJtdString);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteProps(aWriter: TJsonStrWriter); override;
  public
    procedure Clear; override;
  { refers to "quuz" JSON property }
    property Quuz: TJtdString read FQuuz write SetQuuz;
  end;

  { TRootObject }
  TRootObject = class sealed(TJtdUnion)
  protected
    function  GetBarBaz: TBarBaz;
    function  GetQuux: TQuux;
    procedure SetBarBaz(aValue: TBarBaz);
    procedure SetQuux(aValue: TQuux);
    class function GetTagJsonName: string; override;
    class function GetInstanceClass(const aTag: string): TJtdEntityClass; override;
  public
  { matches the "BAR_BAZ" tag }
    property BarBaz: TBarBaz read GetBarBaz write SetBarBaz;
  { matches the "QUUX" tag }
    property Quux: TQuux read GetQuux write SetQuux;
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
procedure TBarBaz.DoReadJson(aNode: TJsonNode);
var
  Flags: array[0..0] of Boolean;
  e: TJsonNode.TPair;
  I: Integer;
begin
  if not aNode.IsObject then ExpectObject(aNode);
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  for e in aNode.Entries do begin
    if (FTagField <> '') and (e.Key = FTagField) then continue;
    case e.Key of
      'baz':
        if not Flags[0] then begin
          FBaz := TJtdString(TJtdString.LoadInstance(e.Value));
          Flags[0] := True;
        end else DuplicateProp(e.Key);
    else
      UnknownProp(e.Key);
    end;
  end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('baz');
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TBarBaz.DoReadJson(aReader: TJsonReader);
var
  Flags: array[0..0] of Boolean;
  I: Integer;
begin
  if aReader.TokenKind <> tkObjectBegin then ExpectObject(aReader);
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
    if (FTagField <> '') and (aReader.Name = FTagField) then continue;
    case aReader.Name of
      'baz':
        if not Flags[0] then begin
          FBaz := TJtdString(TJtdString.LoadInstance(aReader));
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

procedure TBarBaz.DoWriteProps(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('baz');
  Baz.WriteJson(aWriter);
end;

procedure TBarBaz.Clear;
begin
  FreeAndNil(FBaz);
end;

{ TQuux }

procedure TQuux.SetQuuz(aValue: TJtdString);
begin
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
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  for e in aNode.Entries do begin
    if (FTagField <> '') and (e.Key = FTagField) then continue;
    case e.Key of
      'quuz':
        if not Flags[0] then begin
          FQuuz := TJtdString(TJtdString.LoadInstance(e.Value));
          Flags[0] := True;
        end else DuplicateProp(e.Key);
    else
      UnknownProp(e.Key);
    end;
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
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
    if (FTagField <> '') and (aReader.Name = FTagField) then continue;
    case aReader.Name of
      'quuz':
        if not Flags[0] then begin
          FQuuz := TJtdString(TJtdString.LoadInstance(aReader));
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

procedure TQuux.DoWriteProps(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('quuz');
  Quuz.WriteJson(aWriter);
end;

procedure TQuux.Clear;
begin
  FreeAndNil(FQuuz);
end;

{ TRootObject }

class function TRootObject.GetTagJsonName: string;
begin
  Result := 'foo';
end;

class function TRootObject.GetInstanceClass(const aTag: string): TJtdEntityClass;
begin
  case aTag of
    'BAR_BAZ': Result := TBarBaz;
    'QUUX': Result := TQuux;
  else
    Result := nil;
  end;
end;

function TRootObject.GetBarBaz: TBarBaz;
begin
  Result := FInstance as TBarBaz;
end;

function TRootObject.GetQuux: TQuux;
begin
  Result := FInstance as TQuux;
end;

procedure TRootObject.SetBarBaz(aValue: TBarBaz);
begin
  if aValue = FInstance then exit;
  FInstance.Free;
  FInstance := aValue;
  FTag := 'BAR_BAZ';
end;

procedure TRootObject.SetQuux(aValue: TQuux);
begin
  if aValue = FInstance then exit;
  FInstance.Free;
  FInstance := aValue;
  FTag := 'QUUX';
end;

end.
