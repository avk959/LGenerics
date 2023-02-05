{
  Source schema: enum_collisions.jtd.json

  This unit was automatically created by JtdPasCodegen, do not edit.
}
unit enum_collisions;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TBar = (x, y);

{ Container for some TBar enumeration element }
  TBarElem = class sealed(specialize TJtdEnum<TBar>)
    class function GetJtdClass: TJtdEntityClass; override;
  end;

  TFoo = class sealed(TJtdObject)
  private
    FBar: TBarElem;
    procedure SetBar(aValue: TBarElem);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
    property Bar: TBarElem read FBar write SetBar;
  end;

  TFooBar = TBarElem;

  TEnumCollisions = class sealed(TJtdObject)
  private
    FFoo: TFoo;
    FFooBar: TFooBar;
    procedure SetFoo(aValue: TFoo);
    procedure SetFooBar(aValue: TFooBar);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
    property Foo: TFoo read FFoo write SetFoo;
    property FooBar: TFooBar read FFooBar write SetFooBar;
  end;

implementation

{ TBarElem }

class function TBarElem.GetJtdClass: TJtdEntityClass;
begin
  Result := TBarElem;
end;

{ TFoo }

class function TFoo.GetJtdClass: TJtdEntityClass;
begin
  Result := TFoo;
end;

procedure TFoo.Clear;
begin
  FreeAndNil(FBar);
end;

procedure TFoo.SetBar(aValue: TBarElem);
begin
  if aValue = FBar then exit;
  FBar.Free;
  FBar := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TFoo.DoReadJson(aNode: TJsonNode);
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
      'bar':
        begin
          FBar := TBarElem(TBarElem.ReadJson(p.Value));
          Flags[0] := True;
        end;
    else
      UnknownProp(p.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('bar');
      else
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TFoo.DoReadJson(aReader: TJsonReader);
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
      'bar':
        begin
          FBar := TBarElem(TBarElem.ReadJson(aReader));
          Flags[0] := True;
        end;
    else
      UnknownProp(aReader.Name);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('bar');
      else
      end;
end;
{$POP}

procedure TFoo.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName('bar');
  Bar.WriteJson(aWriter);
  aWriter.EndObject;
end;

{ TEnumCollisions }

class function TEnumCollisions.GetJtdClass: TJtdEntityClass;
begin
  Result := TEnumCollisions;
end;

procedure TEnumCollisions.Clear;
begin
  FreeAndNil(FFoo);
  FreeAndNil(FFooBar);
end;

procedure TEnumCollisions.SetFoo(aValue: TFoo);
begin
  if aValue = FFoo then exit;
  FFoo.Free;
  FFoo := aValue;
end;

procedure TEnumCollisions.SetFooBar(aValue: TFooBar);
begin
  if aValue = FFooBar then exit;
  FFooBar.Free;
  FFooBar := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TEnumCollisions.DoReadJson(aNode: TJsonNode);
var
  p: TJsonNode.TPair;
  Flags: array[0..1] of Boolean;
  I: Integer;
begin
  if not aNode.IsObject then ReadError;
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  for p in aNode.Entries do
    case p.Key of
      'foo':
        begin
          FFoo := TFoo(TFoo.ReadJson(p.Value));
          Flags[0] := True;
        end;
      'foo_bar':
        begin
          FFooBar := TFooBar(TFooBar.ReadJson(p.Value));
          Flags[1] := True;
        end;
    else
      UnknownProp(p.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('foo');
        1: PropNotFound('foo_bar');
      else
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TEnumCollisions.DoReadJson(aReader: TJsonReader);
var
  Flags: array[0..1] of Boolean;
  I: Integer;
begin
  if aReader.TokenKind <> tkObjectBegin then ReadError;
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReadError;
    if aReader.TokenKind = tkObjectEnd then break;
    case aReader.Name of
      'foo':
        begin
          FFoo := TFoo(TFoo.ReadJson(aReader));
          Flags[0] := True;
        end;
      'foo_bar':
        begin
          FFooBar := TFooBar(TFooBar.ReadJson(aReader));
          Flags[1] := True;
        end;
    else
      UnknownProp(aReader.Name);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('foo');
        1: PropNotFound('foo_bar');
      else
      end;
end;
{$POP}

procedure TEnumCollisions.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName('foo');
  Foo.WriteJson(aWriter);
  aWriter.AddName('foo_bar');
  FooBar.WriteJson(aWriter);
  aWriter.EndObject;
end;

end.
