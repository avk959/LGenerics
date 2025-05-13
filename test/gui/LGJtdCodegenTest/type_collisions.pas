{
  Source schema: type_collisions.jtd.json

  This unit was automatically created by JtdPasCodegen.
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
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
    procedure ClearFields; override;
  public
  { refers to "x" JSON property }
    property X: TJtdBool read FX write SetX;
  end;

  TFoo = class sealed(TJtdObject)
  private
    FBar: TBar;
    procedure SetBar(aValue: TBar);
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
    procedure ClearFields; override;
  public
  { refers to "bar" JSON property }
    property Bar: TBar read FBar write SetBar;
  end;

  TFooBar = class sealed(TJtdObject)
  private
    FX: TJtdString;
    procedure SetX(aValue: TJtdString);
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
    procedure ClearFields; override;
  public
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
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
    procedure ClearFields; override;
  public
  { refers to "foo" JSON property }
    property Foo: TFoo read FFoo write SetFoo;
  { refers to "foo_bar" JSON property }
    property FooBar: TFooBar read FFooBar write SetFooBar;
  end;

implementation

{ TBar }

procedure TBar.SetX(aValue: TJtdBool);
begin
  if aValue = FX then exit;
  FX.Free;
  FX := aValue;
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
      'x':
        if not Flags[0] then begin
          FX.ReadJson(aReader);
          Flags[0] := True;
        end else DuplicateProp(aReader);
    else
      UnknownProp(aReader.Name, aReader);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('x', aReader);
      end;
end;
{$POP}

procedure TBar.WriteFields(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('x');
  X.WriteJson(aWriter);
end;

procedure TBar.DoClear;
begin
end;

procedure TBar.ClearFields;
begin
  FX.Free;
end;

procedure TBar.CreateFields;
begin
  FX := TJtdBool.Create;
end;

{ TFoo }

procedure TFoo.SetBar(aValue: TBar);
begin
  if aValue = FBar then exit;
  FBar.Free;
  FBar := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TFoo.DoReadJson(aReader: TJsonReader);
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
      'bar':
        if not Flags[0] then begin
          FBar.ReadJson(aReader);
          Flags[0] := True;
        end else DuplicateProp(aReader);
    else
      UnknownProp(aReader.Name, aReader);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('bar', aReader);
      end;
end;
{$POP}

procedure TFoo.WriteFields(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('bar');
  Bar.WriteJson(aWriter);
end;

procedure TFoo.DoClear;
begin
end;

procedure TFoo.ClearFields;
begin
  FBar.Free;
end;

procedure TFoo.CreateFields;
begin
  FBar := TBar.Create;
end;

{ TFooBar }

procedure TFooBar.SetX(aValue: TJtdString);
begin
  if aValue = FX then exit;
  FX.Free;
  FX := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TFooBar.DoReadJson(aReader: TJsonReader);
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
      'x':
        if not Flags[0] then begin
          FX.ReadJson(aReader);
          Flags[0] := True;
        end else DuplicateProp(aReader);
    else
      UnknownProp(aReader.Name, aReader);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('x', aReader);
      end;
end;
{$POP}

procedure TFooBar.WriteFields(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('x');
  X.WriteJson(aWriter);
end;

procedure TFooBar.DoClear;
begin
end;

procedure TFooBar.ClearFields;
begin
  FX.Free;
end;

procedure TFooBar.CreateFields;
begin
  FX := TJtdString.Create;
end;

{ TTypeCollisions }

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
procedure TTypeCollisions.DoReadJson(aReader: TJsonReader);
var
  Flags: array[0..1] of Boolean;
  I: Integer;
begin
  if aReader.TokenKind <> tkObjectBegin then ExpectObject(aReader);
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
    case aReader.Name of
      'foo':
        if not Flags[0] then begin
          FFoo.ReadJson(aReader);
          Flags[0] := True;
        end else DuplicateProp(aReader);
      'foo_bar':
        if not Flags[1] then begin
          FFooBar.ReadJson(aReader);
          Flags[1] := True;
        end else DuplicateProp(aReader);
    else
      UnknownProp(aReader.Name, aReader);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('foo', aReader);
        1: PropNotFound('foo_bar', aReader);
      end;
end;
{$POP}

procedure TTypeCollisions.WriteFields(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('foo');
  Foo.WriteJson(aWriter);
  aWriter.AddName('foo_bar');
  FooBar.WriteJson(aWriter);
end;

procedure TTypeCollisions.DoClear;
begin
end;

procedure TTypeCollisions.ClearFields;
begin
  FFoo.Free;
  FFooBar.Free;
end;

procedure TTypeCollisions.CreateFields;
begin
  FFoo := TFoo.Create;
  FFooBar := TFooBar.Create;
end;

end.
