{
  Source schema: empty_and_nonascii_properties.jtd.json

  This unit was automatically created by JtdPasCodegen, do not edit.
}
unit empty_and_nonascii_properties;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TEmptyAndNonasciiProperties = class sealed(TJtdObject)
  private
    FEmptyName: TJtdString;
    FP_foo: TJtdString;
    F_foo: TJtdString;
    FP0foo: TJtdString;
    FFoo0bar: TJtdString;
    FFoo_bar: TJtdString;
    FFoo_bar1: TJtdString;
    FFoo___bar: TJtdString;
    procedure SetEmptyName(aValue: TJtdString);
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
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
  { refers to "" JSON property }
    property EmptyName: TJtdString read FEmptyName write SetEmptyName;
  { refers to "$foo" JSON property }
    property P_foo: TJtdString read FP_foo write SetP_foo;
  { refers to "_foo" JSON property }
    property _foo: TJtdString read F_foo write Set_foo;
  { refers to "0foo" JSON property }
    property P0foo: TJtdString read FP0foo write SetP0foo;
  { refers to "foo0bar" JSON property }
    property Foo0bar: TJtdString read FFoo0bar write SetFoo0bar;
  { refers to "foo bar" JSON property }
    property Foo_bar: TJtdString read FFoo_bar write SetFoo_bar;
  { refers to "foo\nbar" JSON property }
    property Foo_bar1: TJtdString read FFoo_bar1 write SetFoo_bar1;
  { refers to "foo\uFDFDbar" JSON property }
    property Foo___bar: TJtdString read FFoo___bar write SetFoo___bar;
  end;

implementation

{ TEmptyAndNonasciiProperties }

class function TEmptyAndNonasciiProperties.GetJtdClass: TJtdEntityClass;
begin
  Result := TEmptyAndNonasciiProperties;
end;

procedure TEmptyAndNonasciiProperties.Clear;
begin
  FreeAndNil(FEmptyName);
  FreeAndNil(FP_foo);
  FreeAndNil(F_foo);
  FreeAndNil(FP0foo);
  FreeAndNil(FFoo0bar);
  FreeAndNil(FFoo_bar);
  FreeAndNil(FFoo_bar1);
  FreeAndNil(FFoo___bar);
end;

procedure TEmptyAndNonasciiProperties.SetEmptyName(aValue: TJtdString);
begin
  if aValue = FEmptyName then exit;
  FEmptyName.Free;
  FEmptyName := aValue;
end;

procedure TEmptyAndNonasciiProperties.SetP_foo(aValue: TJtdString);
begin
  if aValue = FP_foo then exit;
  FP_foo.Free;
  FP_foo := aValue;
end;

procedure TEmptyAndNonasciiProperties.Set_foo(aValue: TJtdString);
begin
  if aValue = F_foo then exit;
  F_foo.Free;
  F_foo := aValue;
end;

procedure TEmptyAndNonasciiProperties.SetP0foo(aValue: TJtdString);
begin
  if aValue = FP0foo then exit;
  FP0foo.Free;
  FP0foo := aValue;
end;

procedure TEmptyAndNonasciiProperties.SetFoo0bar(aValue: TJtdString);
begin
  if aValue = FFoo0bar then exit;
  FFoo0bar.Free;
  FFoo0bar := aValue;
end;

procedure TEmptyAndNonasciiProperties.SetFoo_bar(aValue: TJtdString);
begin
  if aValue = FFoo_bar then exit;
  FFoo_bar.Free;
  FFoo_bar := aValue;
end;

procedure TEmptyAndNonasciiProperties.SetFoo_bar1(aValue: TJtdString);
begin
  if aValue = FFoo_bar1 then exit;
  FFoo_bar1.Free;
  FFoo_bar1 := aValue;
end;

procedure TEmptyAndNonasciiProperties.SetFoo___bar(aValue: TJtdString);
begin
  if aValue = FFoo___bar then exit;
  FFoo___bar.Free;
  FFoo___bar := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TEmptyAndNonasciiProperties.DoReadJson(aNode: TJsonNode);
var
  p: TJsonNode.TPair;
  Flags: array[0..7] of Boolean;
  I: Integer;
begin
  if not aNode.IsObject then ReadError;
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  for p in aNode.Entries do
    case TJsonNode.PasStrToAsciiJson(p.Key) of
      '""':
        begin
          FEmptyName := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[0] := True;
        end;
      '"$foo"':
        begin
          FP_foo := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[1] := True;
        end;
      '"_foo"':
        begin
          F_foo := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[2] := True;
        end;
      '"0foo"':
        begin
          FP0foo := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[3] := True;
        end;
      '"foo0bar"':
        begin
          FFoo0bar := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[4] := True;
        end;
      '"foo bar"':
        begin
          FFoo_bar := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[5] := True;
        end;
      '"foo\nbar"':
        begin
          FFoo_bar1 := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[6] := True;
        end;
      '"foo\uFDFDbar"':
        begin
          FFoo___bar := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[7] := True;
        end;
    else
      UnknownProp(p.Key);
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
      else
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TEmptyAndNonasciiProperties.DoReadJson(aReader: TJsonReader);
var
  Flags: array[0..7] of Boolean;
  I: Integer;
begin
  if aReader.TokenKind <> tkObjectBegin then ReadError;
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReadError;
    if aReader.TokenKind = tkObjectEnd then break;
    case TJsonNode.PasStrToAsciiJson(aReader.Name) of
      '""':
        begin
          FEmptyName := TJtdString(TJtdString.ReadJson(aReader));
          Flags[0] := True;
        end;
      '"$foo"':
        begin
          FP_foo := TJtdString(TJtdString.ReadJson(aReader));
          Flags[1] := True;
        end;
      '"_foo"':
        begin
          F_foo := TJtdString(TJtdString.ReadJson(aReader));
          Flags[2] := True;
        end;
      '"0foo"':
        begin
          FP0foo := TJtdString(TJtdString.ReadJson(aReader));
          Flags[3] := True;
        end;
      '"foo0bar"':
        begin
          FFoo0bar := TJtdString(TJtdString.ReadJson(aReader));
          Flags[4] := True;
        end;
      '"foo bar"':
        begin
          FFoo_bar := TJtdString(TJtdString.ReadJson(aReader));
          Flags[5] := True;
        end;
      '"foo\nbar"':
        begin
          FFoo_bar1 := TJtdString(TJtdString.ReadJson(aReader));
          Flags[6] := True;
        end;
      '"foo\uFDFDbar"':
        begin
          FFoo___bar := TJtdString(TJtdString.ReadJson(aReader));
          Flags[7] := True;
        end;
    else
      UnknownProp(aReader.Name);
    end;
  until False;
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
      else
      end;
end;
{$POP}

procedure TEmptyAndNonasciiProperties.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName(TJsonNode.JsonStrToPas('""'));
  EmptyName.WriteJson(aWriter);
  aWriter.AddName(TJsonNode.JsonStrToPas('"$foo"'));
  P_foo.WriteJson(aWriter);
  aWriter.AddName(TJsonNode.JsonStrToPas('"_foo"'));
  _foo.WriteJson(aWriter);
  aWriter.AddName(TJsonNode.JsonStrToPas('"0foo"'));
  P0foo.WriteJson(aWriter);
  aWriter.AddName(TJsonNode.JsonStrToPas('"foo0bar"'));
  Foo0bar.WriteJson(aWriter);
  aWriter.AddName(TJsonNode.JsonStrToPas('"foo bar"'));
  Foo_bar.WriteJson(aWriter);
  aWriter.AddName(TJsonNode.JsonStrToPas('"foo\nbar"'));
  Foo_bar1.WriteJson(aWriter);
  aWriter.AddName(TJsonNode.JsonStrToPas('"foo\uFDFDbar"'));
  Foo___bar.WriteJson(aWriter);
  aWriter.EndObject;
end;

end.
