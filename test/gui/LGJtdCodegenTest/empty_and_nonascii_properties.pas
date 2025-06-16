{
  Source schema: empty_and_nonascii_properties.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit empty_and_nonascii_properties;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TRootObject = class sealed(TJtdObject)
  private
    FEmptyname: TJtdString;
    FP_foo: TJtdString;
    F_foo: TJtdString;
    FP0foo: TJtdString;
    FFoo0bar: TJtdString;
    FFoo_bar: TJtdString;
    FFoo_bar1: TJtdString;
    FFoo___bar: TJtdString;
    function  GetEmptyname: TJtdString;
    function  GetP_foo: TJtdString;
    function  Get_foo: TJtdString;
    function  GetP0foo: TJtdString;
    function  GetFoo0bar: TJtdString;
    function  GetFoo_bar: TJtdString;
    function  GetFoo_bar1: TJtdString;
    function  GetFoo___bar: TJtdString;
    procedure SetEmptyname(aValue: TJtdString);
    procedure SetP_foo(aValue: TJtdString);
    procedure Set_foo(aValue: TJtdString);
    procedure SetP0foo(aValue: TJtdString);
    procedure SetFoo0bar(aValue: TJtdString);
    procedure SetFoo_bar(aValue: TJtdString);
    procedure SetFoo_bar1(aValue: TJtdString);
    procedure SetFoo___bar(aValue: TJtdString);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure CreateProps; override;
    procedure ClearProps; override;
    procedure WriteProps(aWriter: TJsonStrWriter); override;
  public
  { refers to "" JSON property }
    property Emptyname: TJtdString read GetEmptyname write SetEmptyname;
  { refers to "$foo" JSON property }
    property P_foo: TJtdString read GetP_foo write SetP_foo;
  { refers to "_foo" JSON property }
    property _foo: TJtdString read Get_foo write Set_foo;
  { refers to "0foo" JSON property }
    property P0foo: TJtdString read GetP0foo write SetP0foo;
  { refers to "foo0bar" JSON property }
    property Foo0bar: TJtdString read GetFoo0bar write SetFoo0bar;
  { refers to "foo bar" JSON property }
    property Foo_bar: TJtdString read GetFoo_bar write SetFoo_bar;
  { refers to "foo\nbar" JSON property }
    property Foo_bar1: TJtdString read GetFoo_bar1 write SetFoo_bar1;
  { refers to "foo\uFDFDbar" JSON property }
    property Foo___bar: TJtdString read GetFoo___bar write SetFoo___bar;
  end;

implementation

{ TRootObject }

function TRootObject.GetEmptyname: TJtdString;
begin
  CheckNull;
  Result := FEmptyname;
end;

function TRootObject.GetP_foo: TJtdString;
begin
  CheckNull;
  Result := FP_foo;
end;

function TRootObject.Get_foo: TJtdString;
begin
  CheckNull;
  Result := F_foo;
end;

function TRootObject.GetP0foo: TJtdString;
begin
  CheckNull;
  Result := FP0foo;
end;

function TRootObject.GetFoo0bar: TJtdString;
begin
  CheckNull;
  Result := FFoo0bar;
end;

function TRootObject.GetFoo_bar: TJtdString;
begin
  CheckNull;
  Result := FFoo_bar;
end;

function TRootObject.GetFoo_bar1: TJtdString;
begin
  CheckNull;
  Result := FFoo_bar1;
end;

function TRootObject.GetFoo___bar: TJtdString;
begin
  CheckNull;
  Result := FFoo___bar;
end;

procedure TRootObject.SetEmptyname(aValue: TJtdString);
begin
  DoAssign;
  if aValue = FEmptyname then exit;
  FEmptyname.Free;
  FEmptyname := aValue;
end;

procedure TRootObject.SetP_foo(aValue: TJtdString);
begin
  DoAssign;
  if aValue = FP_foo then exit;
  FP_foo.Free;
  FP_foo := aValue;
end;

procedure TRootObject.Set_foo(aValue: TJtdString);
begin
  DoAssign;
  if aValue = F_foo then exit;
  F_foo.Free;
  F_foo := aValue;
end;

procedure TRootObject.SetP0foo(aValue: TJtdString);
begin
  DoAssign;
  if aValue = FP0foo then exit;
  FP0foo.Free;
  FP0foo := aValue;
end;

procedure TRootObject.SetFoo0bar(aValue: TJtdString);
begin
  DoAssign;
  if aValue = FFoo0bar then exit;
  FFoo0bar.Free;
  FFoo0bar := aValue;
end;

procedure TRootObject.SetFoo_bar(aValue: TJtdString);
begin
  DoAssign;
  if aValue = FFoo_bar then exit;
  FFoo_bar.Free;
  FFoo_bar := aValue;
end;

procedure TRootObject.SetFoo_bar1(aValue: TJtdString);
begin
  DoAssign;
  if aValue = FFoo_bar1 then exit;
  FFoo_bar1.Free;
  FFoo_bar1 := aValue;
end;

procedure TRootObject.SetFoo___bar(aValue: TJtdString);
begin
  DoAssign;
  if aValue = FFoo___bar then exit;
  FFoo___bar.Free;
  FFoo___bar := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TRootObject.DoReadJson(aNode: TJsonNode);
var
  Flags: array[0..7] of Boolean;
  e: TJsonNode.TPair;
  I: Integer;
begin
  if not aNode.IsObject then ExpectObject(aNode);
  System.FillChar(Flags, SizeOf(Flags), 0);
  for e in aNode.Entries do
    case TJsonNode.PasStrToAsciiJson(e.Key) of
      '""':
        if not Flags[0] then begin
          FEmptyname.ReadJson(e.Value);
          Flags[0] := True;
        end else DuplicateProp(e.Key);
      '"$foo"':
        if not Flags[1] then begin
          FP_foo.ReadJson(e.Value);
          Flags[1] := True;
        end else DuplicateProp(e.Key);
      '"_foo"':
        if not Flags[2] then begin
          F_foo.ReadJson(e.Value);
          Flags[2] := True;
        end else DuplicateProp(e.Key);
      '"0foo"':
        if not Flags[3] then begin
          FP0foo.ReadJson(e.Value);
          Flags[3] := True;
        end else DuplicateProp(e.Key);
      '"foo0bar"':
        if not Flags[4] then begin
          FFoo0bar.ReadJson(e.Value);
          Flags[4] := True;
        end else DuplicateProp(e.Key);
      '"foo bar"':
        if not Flags[5] then begin
          FFoo_bar.ReadJson(e.Value);
          Flags[5] := True;
        end else DuplicateProp(e.Key);
      '"foo\nbar"':
        if not Flags[6] then begin
          FFoo_bar1.ReadJson(e.Value);
          Flags[6] := True;
        end else DuplicateProp(e.Key);
      '"foo\uFDFDbar"':
        if not Flags[7] then begin
          FFoo___bar.ReadJson(e.Value);
          Flags[7] := True;
        end else DuplicateProp(e.Key);
    else
      UnknownProp(e.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('""');
        1: PropNotFound('"$foo"');
        2: PropNotFound('"_foo"');
        3: PropNotFound('"0foo"');
        4: PropNotFound('"foo0bar"');
        5: PropNotFound('"foo bar"');
        6: PropNotFound('"foo\nbar"');
        7: PropNotFound('"foo\uFDFDbar"');
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TRootObject.DoReadJson(aReader: TJsonReader);
var
  Flags: array[0..7] of Boolean;
  I: Integer;
begin
  if aReader.TokenKind <> tkObjectBegin then ExpectObject(aReader);
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
    case TJsonNode.PasStrToAsciiJson(aReader.Name) of
      '""':
        if not Flags[0] then begin
          FEmptyname.ReadJson(aReader);
          Flags[0] := True;
        end else DuplicateProp(aReader);
      '"$foo"':
        if not Flags[1] then begin
          FP_foo.ReadJson(aReader);
          Flags[1] := True;
        end else DuplicateProp(aReader);
      '"_foo"':
        if not Flags[2] then begin
          F_foo.ReadJson(aReader);
          Flags[2] := True;
        end else DuplicateProp(aReader);
      '"0foo"':
        if not Flags[3] then begin
          FP0foo.ReadJson(aReader);
          Flags[3] := True;
        end else DuplicateProp(aReader);
      '"foo0bar"':
        if not Flags[4] then begin
          FFoo0bar.ReadJson(aReader);
          Flags[4] := True;
        end else DuplicateProp(aReader);
      '"foo bar"':
        if not Flags[5] then begin
          FFoo_bar.ReadJson(aReader);
          Flags[5] := True;
        end else DuplicateProp(aReader);
      '"foo\nbar"':
        if not Flags[6] then begin
          FFoo_bar1.ReadJson(aReader);
          Flags[6] := True;
        end else DuplicateProp(aReader);
      '"foo\uFDFDbar"':
        if not Flags[7] then begin
          FFoo___bar.ReadJson(aReader);
          Flags[7] := True;
        end else DuplicateProp(aReader);
    else
      UnknownProp(aReader.Name, aReader);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('""', aReader);
        1: PropNotFound('"$foo"', aReader);
        2: PropNotFound('"_foo"', aReader);
        3: PropNotFound('"0foo"', aReader);
        4: PropNotFound('"foo0bar"', aReader);
        5: PropNotFound('"foo bar"', aReader);
        6: PropNotFound('"foo\nbar"', aReader);
        7: PropNotFound('"foo\uFDFDbar"', aReader);
      end;
end;
{$POP}

procedure TRootObject.CreateProps;
begin
  FEmptyname := TJtdString.Create;
  FP_foo := TJtdString.Create;
  F_foo := TJtdString.Create;
  FP0foo := TJtdString.Create;
  FFoo0bar := TJtdString.Create;
  FFoo_bar := TJtdString.Create;
  FFoo_bar1 := TJtdString.Create;
  FFoo___bar := TJtdString.Create;
end;

procedure TRootObject.ClearProps;
begin
  FEmptyname.Free;
  FP_foo.Free;
  F_foo.Free;
  FP0foo.Free;
  FFoo0bar.Free;
  FFoo_bar.Free;
  FFoo_bar1.Free;
  FFoo___bar.Free;
end;

procedure TRootObject.WriteProps(aWriter: TJsonStrWriter);
begin
  aWriter.AddName(TJsonNode.JsonStrToPas('""'));
  FEmptyname.WriteJson(aWriter);
  aWriter.AddName(TJsonNode.JsonStrToPas('"$foo"'));
  FP_foo.WriteJson(aWriter);
  aWriter.AddName(TJsonNode.JsonStrToPas('"_foo"'));
  F_foo.WriteJson(aWriter);
  aWriter.AddName(TJsonNode.JsonStrToPas('"0foo"'));
  FP0foo.WriteJson(aWriter);
  aWriter.AddName(TJsonNode.JsonStrToPas('"foo0bar"'));
  FFoo0bar.WriteJson(aWriter);
  aWriter.AddName(TJsonNode.JsonStrToPas('"foo bar"'));
  FFoo_bar.WriteJson(aWriter);
  aWriter.AddName(TJsonNode.JsonStrToPas('"foo\nbar"'));
  FFoo_bar1.WriteJson(aWriter);
  aWriter.AddName(TJsonNode.JsonStrToPas('"foo\uFDFDbar"'));
  FFoo___bar.WriteJson(aWriter);
end;

end.
