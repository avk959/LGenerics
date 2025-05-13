{
  Source schema: nullable_properties.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit nullable_properties;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TBaz = class sealed(specialize TJtdList<TJtdBool>);

  TQuux = class sealed(specialize TJtdList<TJtdBool>);

{ TNullableProperties is nullable }
  TNullableProperties = class sealed(TJtdNullableObject)
  private
    FFoo: TJtdBool;
    FBar: TJtdString;
    FBaz: TBaz;
    FQuux: TQuux;
    function  GetFoo: TJtdBool;
    function  GetBar: TJtdString;
    function  GetBaz: TBaz;
    function  GetQuux: TQuux;
    procedure SetFoo(aValue: TJtdBool);
    procedure SetBar(aValue: TJtdString);
    procedure SetBaz(aValue: TBaz);
    procedure SetQuux(aValue: TQuux);
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
  public
  { refers to "foo" JSON property }
    property Foo: TJtdBool read GetFoo write SetFoo;
  { refers to "bar" JSON property }
    property Bar: TJtdString read GetBar write SetBar;
  { refers to "baz" JSON property }
    property Baz: TBaz read GetBaz write SetBaz;
  { refers to "quux" JSON property }
    property Quux: TQuux read GetQuux write SetQuux;
  end;

implementation

{ TNullableProperties }

function TNullableProperties.GetFoo: TJtdBool;
begin
  CheckNull;
  Result := FFoo;
end;

function TNullableProperties.GetBar: TJtdString;
begin
  CheckNull;
  Result := FBar;
end;

function TNullableProperties.GetBaz: TBaz;
begin
  CheckNull;
  Result := FBaz;
end;

function TNullableProperties.GetQuux: TQuux;
begin
  CheckNull;
  Result := FQuux;
end;

procedure TNullableProperties.SetFoo(aValue: TJtdBool);
begin
  CheckFields;
  if aValue = FFoo then exit;
  FFoo.Free;
  FFoo := aValue;
end;

procedure TNullableProperties.SetBar(aValue: TJtdString);
begin
  CheckFields;
  if aValue = FBar then exit;
  FBar.Free;
  FBar := aValue;
end;

procedure TNullableProperties.SetBaz(aValue: TBaz);
begin
  CheckFields;
  if aValue = FBaz then exit;
  FBaz.Free;
  FBaz := aValue;
end;

procedure TNullableProperties.SetQuux(aValue: TQuux);
begin
  CheckFields;
  if aValue = FQuux then exit;
  FQuux.Free;
  FQuux := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TNullableProperties.DoReadJson(aReader: TJsonReader);
var
  Flags: array[0..3] of Boolean;
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
      'bar':
        if not Flags[1] then begin
          FBar.ReadJson(aReader);
          Flags[1] := True;
        end else DuplicateProp(aReader);
      'baz':
        if not Flags[2] then begin
          FBaz.ReadJson(aReader);
          Flags[2] := True;
        end else DuplicateProp(aReader);
      'quux':
        if not Flags[3] then begin
          FQuux.ReadJson(aReader);
          Flags[3] := True;
        end else DuplicateProp(aReader);
    else
      UnknownProp(aReader.Name, aReader);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('foo', aReader);
        1: PropNotFound('bar', aReader);
        2: PropNotFound('baz', aReader);
        3: PropNotFound('quux', aReader);
      end;
end;
{$POP}

procedure TNullableProperties.WriteFields(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('foo');
  Foo.WriteJson(aWriter);
  aWriter.AddName('bar');
  Bar.WriteJson(aWriter);
  aWriter.AddName('baz');
  Baz.WriteJson(aWriter);
  aWriter.AddName('quux');
  Quux.WriteJson(aWriter);
end;

procedure TNullableProperties.DoClear;
begin
  FreeAndNil(FFoo);
  FreeAndNil(FBar);
  FreeAndNil(FBaz);
  FreeAndNil(FQuux);
end;

procedure TNullableProperties.CreateFields;
begin
  FFoo := TJtdBool.Create;
  FBar := TJtdString.Create;
  FBaz := TBaz.Create;
  FQuux := TQuux.Create;
end;

end.
