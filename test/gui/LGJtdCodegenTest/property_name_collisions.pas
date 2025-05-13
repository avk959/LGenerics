{
  Source schema: property_name_collisions.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit property_name_collisions;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TPropertyNameCollisions = class sealed(TJtdObject)
  private
    FFoo: TJtdString;
    FFoo1: TJtdString;
    procedure SetFoo(aValue: TJtdString);
    procedure SetFoo1(aValue: TJtdString);
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
    procedure ClearFields; override;
  public
  { refers to "foo" JSON property }
    property Foo: TJtdString read FFoo write SetFoo;
  { refers to "Foo" JSON property }
    property Foo1: TJtdString read FFoo1 write SetFoo1;
  end;

implementation

{ TPropertyNameCollisions }

procedure TPropertyNameCollisions.SetFoo(aValue: TJtdString);
begin
  if aValue = FFoo then exit;
  FFoo.Free;
  FFoo := aValue;
end;

procedure TPropertyNameCollisions.SetFoo1(aValue: TJtdString);
begin
  if aValue = FFoo1 then exit;
  FFoo1.Free;
  FFoo1 := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TPropertyNameCollisions.DoReadJson(aReader: TJsonReader);
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
      'Foo':
        if not Flags[1] then begin
          FFoo1.ReadJson(aReader);
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
        1: PropNotFound('Foo', aReader);
      end;
end;
{$POP}

procedure TPropertyNameCollisions.WriteFields(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('foo');
  Foo.WriteJson(aWriter);
  aWriter.AddName('Foo');
  Foo1.WriteJson(aWriter);
end;

procedure TPropertyNameCollisions.DoClear;
begin
end;

procedure TPropertyNameCollisions.ClearFields;
begin
  FFoo.Free;
  FFoo1.Free;
end;

procedure TPropertyNameCollisions.CreateFields;
begin
  FFoo := TJtdString.Create;
  FFoo1 := TJtdString.Create;
end;

end.
