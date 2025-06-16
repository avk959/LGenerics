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
    function  GetFoo: TJtdString;
    function  GetFoo1: TJtdString;
    procedure SetFoo(aValue: TJtdString);
    procedure SetFoo1(aValue: TJtdString);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure CreateProps; override;
    procedure ClearProps; override;
    procedure WriteProps(aWriter: TJsonStrWriter); override;
  public
  { refers to "foo" JSON property }
    property Foo: TJtdString read GetFoo write SetFoo;
  { refers to "Foo" JSON property }
    property Foo1: TJtdString read GetFoo1 write SetFoo1;
  end;

implementation

{ TRootObject }

function TRootObject.GetFoo: TJtdString;
begin
  CheckNull;
  Result := FFoo;
end;

function TRootObject.GetFoo1: TJtdString;
begin
  CheckNull;
  Result := FFoo1;
end;

procedure TRootObject.SetFoo(aValue: TJtdString);
begin
  DoAssign;
  if aValue = FFoo then exit;
  FFoo.Free;
  FFoo := aValue;
end;

procedure TRootObject.SetFoo1(aValue: TJtdString);
begin
  DoAssign;
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
  System.FillChar(Flags, SizeOf(Flags), 0);
  for e in aNode.Entries do
    case e.Key of
      'foo':
        if not Flags[0] then begin
          FFoo.ReadJson(e.Value);
          Flags[0] := True;
        end else DuplicateProp(e.Key);
      'Foo':
        if not Flags[1] then begin
          FFoo1.ReadJson(e.Value);
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

procedure TRootObject.CreateProps;
begin
  FFoo := TJtdString.Create;
  FFoo1 := TJtdString.Create;
end;

procedure TRootObject.ClearProps;
begin
  FFoo.Free;
  FFoo1.Free;
end;

procedure TRootObject.WriteProps(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('foo');
  FFoo.WriteJson(aWriter);
  aWriter.AddName('Foo');
  FFoo1.WriteJson(aWriter);
end;

end.
