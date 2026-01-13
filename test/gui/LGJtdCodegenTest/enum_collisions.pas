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

{ TBar: container for some TBarEnum enumeration element }
  TBar = class sealed(specialize TJtdEnum<TBarEnum>);

  TFoo = class sealed(TJtdObject)
  private
    FBar: TBar;
    procedure SetBar(aValue: TBar);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteProps(aWriter: TJsonStrWriter); override;
  public
    procedure Clear; override;
  { refers to "bar" JSON property }
    property Bar: TBar read FBar write SetBar;
  end;

  TFooBarEnum = (x, y);

{ TFooBar: container for some TFooBarEnum enumeration element }
  TFooBar = class sealed(specialize TJtdEnum<TFooBarEnum>);

  TRootObject = class sealed(TJtdObject)
  private
    FFoo: TFoo;
    FFooBar: TFooBar;
    procedure SetFoo(aValue: TFoo);
    procedure SetFooBar(aValue: TFooBar);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteProps(aWriter: TJsonStrWriter); override;
  public
    procedure Clear; override;
  { refers to "foo" JSON property }
    property Foo: TFoo read FFoo write SetFoo;
  { refers to "foo_bar" JSON property }
    property FooBar: TFooBar read FFooBar write SetFooBar;
  end;

implementation

{ TFoo }

procedure TFoo.SetBar(aValue: TBar);
begin
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
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  for e in aNode.Entries do begin
    if (FTagField <> '') and (e.Key = FTagField) then continue;
    case e.Key of
      'bar':
        if not Flags[0] then begin
          FBar := TBar(TBar.LoadInstance(e.Value));
          Flags[0] := True;
        end else DuplicateProp(e.Key);
    else
      UnknownProp(e.Key);
    end;
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
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
    if (FTagField <> '') and (aReader.Name = FTagField) then continue;
    case aReader.Name of
      'bar':
        if not Flags[0] then begin
          FBar := TBar(TBar.LoadInstance(aReader));
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

procedure TFoo.DoWriteProps(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('bar');
  Bar.WriteJson(aWriter);
end;

procedure TFoo.Clear;
begin
  FreeAndNil(FBar);
end;

{ TRootObject }

procedure TRootObject.SetFoo(aValue: TFoo);
begin
  if aValue = FFoo then exit;
  FFoo.Free;
  FFoo := aValue;
end;

procedure TRootObject.SetFooBar(aValue: TFooBar);
begin
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
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  for e in aNode.Entries do begin
    if (FTagField <> '') and (e.Key = FTagField) then continue;
    case e.Key of
      'foo':
        if not Flags[0] then begin
          FFoo := TFoo(TFoo.LoadInstance(e.Value));
          Flags[0] := True;
        end else DuplicateProp(e.Key);
      'foo_bar':
        if not Flags[1] then begin
          FFooBar := TFooBar(TFooBar.LoadInstance(e.Value));
          Flags[1] := True;
        end else DuplicateProp(e.Key);
    else
      UnknownProp(e.Key);
    end;
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
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
    if (FTagField <> '') and (aReader.Name = FTagField) then continue;
    case aReader.Name of
      'foo':
        if not Flags[0] then begin
          FFoo := TFoo(TFoo.LoadInstance(aReader));
          Flags[0] := True;
        end else DuplicateProp(aReader);
      'foo_bar':
        if not Flags[1] then begin
          FFooBar := TFooBar(TFooBar.LoadInstance(aReader));
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

procedure TRootObject.DoWriteProps(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('foo');
  Foo.WriteJson(aWriter);
  aWriter.AddName('foo_bar');
  FooBar.WriteJson(aWriter);
end;

procedure TRootObject.Clear;
begin
  FreeAndNil(FFoo);
  FreeAndNil(FFooBar);
end;

end.
