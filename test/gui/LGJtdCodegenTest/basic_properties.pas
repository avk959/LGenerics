{
  Source schema: basic_properties.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit basic_properties;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TBaz = class sealed(specialize TJtdList<TJtdBool>);

  TQuux = class sealed(specialize TJtdList<TJtdBool>);

  TBasicProperties = class sealed(TJtdObject)
  private
    FFoo: TJtdBool;
    FBar: TJtdString;
    FBaz: TBaz;
    FQuux: TQuux;
    procedure SetFoo(aValue: TJtdBool);
    procedure SetBar(aValue: TJtdString);
    procedure SetBaz(aValue: TBaz);
    procedure SetQuux(aValue: TQuux);
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
    procedure ClearFields; override;
  public
  { refers to "foo" JSON property }
    property Foo: TJtdBool read FFoo write SetFoo;
  { refers to "bar" JSON property }
    property Bar: TJtdString read FBar write SetBar;
  { refers to "baz" JSON property }
    property Baz: TBaz read FBaz write SetBaz;
  { refers to "quux" JSON property }
    property Quux: TQuux read FQuux write SetQuux;
  end;

implementation

{ TBasicProperties }

procedure TBasicProperties.SetFoo(aValue: TJtdBool);
begin
  if aValue = FFoo then exit;
  FFoo.Free;
  FFoo := aValue;
end;

procedure TBasicProperties.SetBar(aValue: TJtdString);
begin
  if aValue = FBar then exit;
  FBar.Free;
  FBar := aValue;
end;

procedure TBasicProperties.SetBaz(aValue: TBaz);
begin
  if aValue = FBaz then exit;
  FBaz.Free;
  FBaz := aValue;
end;

procedure TBasicProperties.SetQuux(aValue: TQuux);
begin
  if aValue = FQuux then exit;
  FQuux.Free;
  FQuux := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TBasicProperties.DoReadJson(aReader: TJsonReader);
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

procedure TBasicProperties.WriteFields(aWriter: TJsonStrWriter);
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

procedure TBasicProperties.DoClear;
begin
end;

procedure TBasicProperties.ClearFields;
begin
  FFoo.Free;
  FBar.Free;
  FBaz.Free;
  FQuux.Free;
end;

procedure TBasicProperties.CreateFields;
begin
  FFoo := TJtdBool.Create;
  FBar := TJtdString.Create;
  FBaz := TBaz.Create;
  FQuux := TQuux.Create;
end;

end.
