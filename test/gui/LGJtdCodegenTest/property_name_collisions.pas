{
  Source schema: property_name_collisions.jtd.json

  This unit was automatically created by JtdPasCodegen, do not edit.
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
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
    property Foo: TJtdString read FFoo write SetFoo;
    property Foo1: TJtdString read FFoo1 write SetFoo1;
  end;

implementation

{ TPropertyNameCollisions }

class function TPropertyNameCollisions.GetJtdClass: TJtdEntityClass;
begin
  Result := TPropertyNameCollisions;
end;

procedure TPropertyNameCollisions.Clear;
begin
  FreeAndNil(FFoo);
  FreeAndNil(FFoo1);
end;

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
procedure TPropertyNameCollisions.DoReadJson(aNode: TJsonNode);
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
          FFoo := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[0] := True;
        end;
      'Foo':
        begin
          FFoo1 := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[1] := True;
        end;
    else
      UnknownProp(p.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('foo');
        1: PropNotFound('Foo');
      else
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TPropertyNameCollisions.DoReadJson(aReader: TJsonReader);
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
          FFoo := TJtdString(TJtdString.ReadJson(aReader));
          Flags[0] := True;
        end;
      'Foo':
        begin
          FFoo1 := TJtdString(TJtdString.ReadJson(aReader));
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
        1: PropNotFound('Foo');
      else
      end;
end;
{$POP}

procedure TPropertyNameCollisions.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName('foo');
  Foo.WriteJson(aWriter);
  aWriter.AddName('Foo');
  Foo1.WriteJson(aWriter);
  aWriter.EndObject;
end;

end.
