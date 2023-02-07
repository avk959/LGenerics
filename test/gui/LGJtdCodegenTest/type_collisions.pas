{
  Source schema: type_collisions.jtd.json

  This unit was automatically created by JtdPasCodegen, do not edit.
}
unit type_collisions;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TBar = class sealed(TJtdObject)
  private
    FX: TJtdBool;
    procedure SetX(aValue: TJtdBool);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
  { refers to "x" JSON property }
    property X: TJtdBool read FX write SetX;
  end;

  TFoo = class sealed(TJtdObject)
  private
    FBar: TBar;
    procedure SetBar(aValue: TBar);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
  { refers to "bar" JSON property }
    property Bar: TBar read FBar write SetBar;
  end;

  TFooBar = class sealed(TJtdObject)
  private
    FX: TJtdString;
    procedure SetX(aValue: TJtdString);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
  { refers to "x" JSON property }
    property X: TJtdString read FX write SetX;
  end;

  TTypeCollisions = class sealed(TJtdObject)
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
  { refers to "foo" JSON property }
    property Foo: TFoo read FFoo write SetFoo;
  { refers to "foo_bar" JSON property }
    property FooBar: TFooBar read FFooBar write SetFooBar;
  end;

implementation

{ TBar }

class function TBar.GetJtdClass: TJtdEntityClass;
begin
  Result := TBar;
end;

procedure TBar.Clear;
begin
  FreeAndNil(FX);
end;

procedure TBar.SetX(aValue: TJtdBool);
begin
  if aValue = FX then exit;
  FX.Free;
  FX := aValue;
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
      'x':
        begin
          FX := TJtdBool(TJtdBool.ReadJson(p.Value));
          Flags[0] := True;
        end;
    else
      UnknownProp(p.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('x');
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
      'x':
        begin
          FX := TJtdBool(TJtdBool.ReadJson(aReader));
          Flags[0] := True;
        end;
    else
      UnknownProp(aReader.Name);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('x');
      else
      end;
end;
{$POP}

procedure TBar.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName('x');
  X.WriteJson(aWriter);
  aWriter.EndObject;
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

procedure TFoo.SetBar(aValue: TBar);
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
          FBar := TBar(TBar.ReadJson(p.Value));
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
          FBar := TBar(TBar.ReadJson(aReader));
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

{ TFooBar }

class function TFooBar.GetJtdClass: TJtdEntityClass;
begin
  Result := TFooBar;
end;

procedure TFooBar.Clear;
begin
  FreeAndNil(FX);
end;

procedure TFooBar.SetX(aValue: TJtdString);
begin
  if aValue = FX then exit;
  FX.Free;
  FX := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TFooBar.DoReadJson(aNode: TJsonNode);
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
      'x':
        begin
          FX := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[0] := True;
        end;
    else
      UnknownProp(p.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('x');
      else
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TFooBar.DoReadJson(aReader: TJsonReader);
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
      'x':
        begin
          FX := TJtdString(TJtdString.ReadJson(aReader));
          Flags[0] := True;
        end;
    else
      UnknownProp(aReader.Name);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('x');
      else
      end;
end;
{$POP}

procedure TFooBar.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName('x');
  X.WriteJson(aWriter);
  aWriter.EndObject;
end;

{ TTypeCollisions }

class function TTypeCollisions.GetJtdClass: TJtdEntityClass;
begin
  Result := TTypeCollisions;
end;

procedure TTypeCollisions.Clear;
begin
  FreeAndNil(FFoo);
  FreeAndNil(FFooBar);
end;

procedure TTypeCollisions.SetFoo(aValue: TFoo);
begin
  if aValue = FFoo then exit;
  FFoo.Free;
  FFoo := aValue;
end;

procedure TTypeCollisions.SetFooBar(aValue: TFooBar);
begin
  if aValue = FFooBar then exit;
  FFooBar.Free;
  FFooBar := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TTypeCollisions.DoReadJson(aNode: TJsonNode);
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
procedure TTypeCollisions.DoReadJson(aReader: TJsonReader);
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

procedure TTypeCollisions.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName('foo');
  Foo.WriteJson(aWriter);
  aWriter.AddName('foo_bar');
  FooBar.WriteJson(aWriter);
  aWriter.EndObject;
end;

end.
