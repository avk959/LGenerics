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

  TRootObject = class sealed(TJtdObject)
  private
    FFoo: TJtdBool;
    FBar: TJtdString;
    FBaz: specialize TJtdList<TJtdBool>;
    FQuux: specialize TJtdList<TJtdBool>;
    function  GetFoo: TJtdBool;
    function  GetBar: TJtdString;
    function  GetBaz: specialize TJtdList<TJtdBool>;
    function  GetQuux: specialize TJtdList<TJtdBool>;
    procedure SetFoo(aValue: TJtdBool);
    procedure SetBar(aValue: TJtdString);
    procedure SetBaz(aValue: specialize TJtdList<TJtdBool>);
    procedure SetQuux(aValue: specialize TJtdList<TJtdBool>);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure CreateProps; override;
    procedure ClearProps; override;
    procedure WriteProps(aWriter: TJsonStrWriter); override;
  public
  { refers to "foo" JSON property }
    property Foo: TJtdBool read GetFoo write SetFoo;
  { refers to "bar" JSON property }
    property Bar: TJtdString read GetBar write SetBar;
  { refers to "baz" JSON property }
    property Baz: specialize TJtdList<TJtdBool> read GetBaz write SetBaz;
  { refers to "quux" JSON property }
    property Quux: specialize TJtdList<TJtdBool> read GetQuux write SetQuux;
  end;

implementation

{ TRootObject }

function TRootObject.GetFoo: TJtdBool;
begin
  CheckNull;
  Result := FFoo;
end;

function TRootObject.GetBar: TJtdString;
begin
  CheckNull;
  Result := FBar;
end;

function TRootObject.GetBaz: specialize TJtdList<TJtdBool>;
begin
  CheckNull;
  Result := FBaz;
end;

function TRootObject.GetQuux: specialize TJtdList<TJtdBool>;
begin
  CheckNull;
  Result := FQuux;
end;

procedure TRootObject.SetFoo(aValue: TJtdBool);
begin
  DoAssign;
  if aValue = FFoo then exit;
  FFoo.Free;
  FFoo := aValue;
end;

procedure TRootObject.SetBar(aValue: TJtdString);
begin
  DoAssign;
  if aValue = FBar then exit;
  FBar.Free;
  FBar := aValue;
end;

procedure TRootObject.SetBaz(aValue: specialize TJtdList<TJtdBool>);
begin
  DoAssign;
  if aValue = FBaz then exit;
  FBaz.Free;
  FBaz := aValue;
end;

procedure TRootObject.SetQuux(aValue: specialize TJtdList<TJtdBool>);
begin
  DoAssign;
  if aValue = FQuux then exit;
  FQuux.Free;
  FQuux := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TRootObject.DoReadJson(aNode: TJsonNode);
var
  Flags: array[0..3] of Boolean;
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
      'bar':
        if not Flags[1] then begin
          FBar.ReadJson(e.Value);
          Flags[1] := True;
        end else DuplicateProp(e.Key);
      'baz':
        if not Flags[2] then begin
          FBaz.ReadJson(e.Value);
          Flags[2] := True;
        end else DuplicateProp(e.Key);
      'quux':
        if not Flags[3] then begin
          FQuux.ReadJson(e.Value);
          Flags[3] := True;
        end else DuplicateProp(e.Key);
    else
      UnknownProp(e.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('foo');
        1: PropNotFound('bar');
        2: PropNotFound('baz');
        3: PropNotFound('quux');
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TRootObject.DoReadJson(aReader: TJsonReader);
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

procedure TRootObject.CreateProps;
begin
  FFoo := TJtdBool.Create;
  FBar := TJtdString.Create;
  FBaz := specialize TJtdList<TJtdBool>.Create;
  FQuux := specialize TJtdList<TJtdBool>.Create;
end;

procedure TRootObject.ClearProps;
begin
  FFoo.Free;
  FBar.Free;
  FBaz.Free;
  FQuux.Free;
end;

procedure TRootObject.WriteProps(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('foo');
  FFoo.WriteJson(aWriter);
  aWriter.AddName('bar');
  FBar.WriteJson(aWriter);
  aWriter.AddName('baz');
  FBaz.WriteJson(aWriter);
  aWriter.AddName('quux');
  FQuux.WriteJson(aWriter);
end;

end.
