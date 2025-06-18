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
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
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
    procedure Clear; override;
  { refers to "quuz" JSON property }
    property Quuz: TJtdString read FQuuz write SetQuuz;
  end;

{ TRootObject is nullable; }
  TRootObject = class sealed(TJtdUnion)
  protected
    function  GetBar: TBar;
    function  GetQuux: TQuux;
    procedure SetBar(aValue: TBar);
    procedure SetQuux(aValue: TQuux);
    class function GetTagJsonName: string; override;
    class function GetInstanceClass(const aTag: string): TJtdEntityClass; override;
  public
  { matches the "bar" tag; }
    property Bar: TBar read GetBar write SetBar;
  { matches the "quux" tag; }
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
procedure TBar.DoReadJson(aNode: TJsonNode);
var
  Flags: array[0..0] of Boolean;
  e: TJsonNode.TPair;
  I: Integer;
begin
  if not aNode.IsObject then ExpectObject(aNode);
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  for e in aNode.Entries do
    case e.Key of
      'baz':
        if not Flags[0] then begin
          FBaz := TJtdString(TJtdString.LoadInstance(e.Value));
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
procedure TBar.DoReadJson(aReader: TJsonReader);
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

procedure TBar.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName('baz');
  Baz.WriteJson(aWriter);
  aWriter.EndObject;
end;

procedure TBar.Clear;
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
  for e in aNode.Entries do
    case e.Key of
      'quuz':
        if not Flags[0] then begin
          FQuuz := TJtdString(TJtdString.LoadInstance(e.Value));
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
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
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

procedure TQuux.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName('quuz');
  Quuz.WriteJson(aWriter);
  aWriter.EndObject;
end;

procedure TQuux.Clear;
begin
  FreeAndNil(FQuuz);
end;

class function TRootObject.GetTagJsonName: string;
begin
  Result := 'foo';
end;

class function TRootObject.GetInstanceClass(const aTag: string): TJtdEntityClass;
begin
  case aTag of
    'bar': Result := TBar;
    'quux': Result := TQuux;
  else
    Result := nil;
  end;
end;

function TRootObject.GetBar: TBar;
begin
  Result := FInstance as TBar;
end;

function TRootObject.GetQuux: TQuux;
begin
  Result := FInstance as TQuux;
end;

procedure TRootObject.SetBar(aValue: TBar);
begin
  if aValue = FInstance then exit;
  FInstance.Free;
  FInstance := aValue;
  FTag := 'bar';
end;

procedure TRootObject.SetQuux(aValue: TQuux);
begin
  if aValue = FInstance then exit;
  FInstance.Free;
  FInstance := aValue;
  FTag := 'quux';
end;

end.
