{
  Source schema: enum_collisions.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit enum_collisions;

{$MODE OBJFPC}{$H+}{$B-}{$SCOPEDENUMS ON}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TBarEnum = (x, y);

{ Container for some TBarEnum enumeration element }
  TBar = class sealed(specialize TJtdEnum<TBarEnum>);

  TFoo = class sealed(TJtdObject)
  private
    FBar: TBar;
    function  GetBar: TBar;
    procedure SetBar(aValue: TBar);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure CreateProps; override;
    procedure ClearProps; override;
    procedure WriteProps(aWriter: TJsonStrWriter); override;
  public
  { refers to "bar" JSON property }
    property Bar: TBar read GetBar write SetBar;
  end;

  TFooBarEnum = (x, y);

{ Container for some TFooBarEnum enumeration element }
  TFooBar = class sealed(specialize TJtdEnum<TFooBarEnum>);

  TRootObject = class sealed(TJtdObject)
  private
    FFoo: TFoo;
    FFooBar: TFooBar;
    function  GetFoo: TFoo;
    function  GetFooBar: TFooBar;
    procedure SetFoo(aValue: TFoo);
    procedure SetFooBar(aValue: TFooBar);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure CreateProps; override;
    procedure ClearProps; override;
    procedure WriteProps(aWriter: TJsonStrWriter); override;
  public
  { refers to "foo" JSON property }
    property Foo: TFoo read GetFoo write SetFoo;
  { refers to "foo_bar" JSON property }
    property FooBar: TFooBar read GetFooBar write SetFooBar;
  end;

implementation

{ TFoo }

function TFoo.GetBar: TBar;
begin
  CheckNull;
  Result := FBar;
end;

procedure TFoo.SetBar(aValue: TBar);
begin
  DoAssign;
  if aValue = FBar then exit;
  FBar.Free;
  FBar := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TFoo.DoReadJson(aNode: TJsonNode);
var
  Flags: array[0..0] of Boolean;
  e: TJsonNode.TPair;
  I: Integer;
begin
  if not aNode.IsObject then ExpectObject(aNode);
  System.FillChar(Flags, SizeOf(Flags), 0);
  for e in aNode.Entries do
    case e.Key of
      'bar':
        if not Flags[0] then begin
          FBar.ReadJson(e.Value);
          Flags[0] := True;
        end else DuplicateProp(e.Key);
    else
      UnknownProp(e.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('bar');
      end;
end;
{$POP}

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

procedure TFoo.CreateProps;
begin
  FBar := TBar.Create;
end;

procedure TFoo.ClearProps;
begin
  FBar.Free;
end;

procedure TFoo.WriteProps(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('bar');
  FBar.WriteJson(aWriter);
end;

{ TRootObject }

function TRootObject.GetFoo: TFoo;
begin
  CheckNull;
  Result := FFoo;
end;

function TRootObject.GetFooBar: TFooBar;
begin
  CheckNull;
  Result := FFooBar;
end;

procedure TRootObject.SetFoo(aValue: TFoo);
begin
  DoAssign;
  if aValue = FFoo then exit;
  FFoo.Free;
  FFoo := aValue;
end;

procedure TRootObject.SetFooBar(aValue: TFooBar);
begin
  DoAssign;
  if aValue = FFooBar then exit;
  FFooBar.Free;
  FFooBar := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TRootObject.DoReadJson(aNode: TJsonNode);
var
  Flags: array[0..1] of Boolean;
  e: TJsonNode.TPair;
  I: Integer;
begin
  if not aNode.IsObject then ExpectObject(aNode);
  System.FillChar(Flags, SizeOf(Flags), 0);
  for e in aNode.Entries do
    case e.Key of
      'foo':
        if not Flags[0] then begin
          FFoo.ReadJson(e.Value);
          Flags[0] := True;
        end else DuplicateProp(e.Key);
      'foo_bar':
        if not Flags[1] then begin
          FFooBar.ReadJson(e.Value);
          Flags[1] := True;
        end else DuplicateProp(e.Key);
    else
      UnknownProp(e.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('foo');
        1: PropNotFound('foo_bar');
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TRootObject.DoReadJson(aReader: TJsonReader);
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

procedure TRootObject.CreateProps;
begin
  FFoo := TFoo.Create;
  FFooBar := TFooBar.Create;
end;

procedure TRootObject.ClearProps;
begin
  FFoo.Free;
  FFooBar.Free;
end;

procedure TRootObject.WriteProps(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('foo');
  FFoo.WriteJson(aWriter);
  aWriter.AddName('foo_bar');
  FFooBar.WriteJson(aWriter);
end;

end.
