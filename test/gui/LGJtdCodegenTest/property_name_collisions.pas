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

  TRootObject = class sealed(TJtdObject)
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
    procedure Clear; override;
  { refers to "foo" JSON property }
    property Foo: TJtdString read FFoo write SetFoo;
  { refers to "Foo" JSON property }
    property Foo1: TJtdString read FFoo1 write SetFoo1;
  end;

implementation

{ TRootObject }

procedure TRootObject.SetFoo(aValue: TJtdString);
begin
  if aValue = FFoo then exit;
  FFoo.Free;
  FFoo := aValue;
end;

procedure TRootObject.SetFoo1(aValue: TJtdString);
begin
  if aValue = FFoo1 then exit;
  FFoo1.Free;
  FFoo1 := aValue;
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
  for e in aNode.Entries do
    case e.Key of
      'foo':
        if not Flags[0] then begin
          FFoo := TJtdString(TJtdString.LoadInstance(e.Value));
          Flags[0] := True;
        end else DuplicateProp(e.Key);
      'Foo':
        if not Flags[1] then begin
          FFoo1 := TJtdString(TJtdString.LoadInstance(e.Value));
          Flags[1] := True;
        end else DuplicateProp(e.Key);
    else
      UnknownProp(e.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('foo');
        1: PropNotFound('Foo');
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
    case aReader.Name of
      'foo':
        if not Flags[0] then begin
          FFoo := TJtdString(TJtdString.LoadInstance(aReader));
          Flags[0] := True;
        end else DuplicateProp(aReader);
      'Foo':
        if not Flags[1] then begin
          FFoo1 := TJtdString(TJtdString.LoadInstance(aReader));
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

procedure TRootObject.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName('foo');
  Foo.WriteJson(aWriter);
  aWriter.AddName('Foo');
  Foo1.WriteJson(aWriter);
  aWriter.EndObject;
end;

procedure TRootObject.Clear;
begin
  FreeAndNil(FFoo);
  FreeAndNil(FFoo1);
end;

end.
