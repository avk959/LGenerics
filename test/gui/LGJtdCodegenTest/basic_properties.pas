{
  Source schema: basic_properties.jtd.json

  This unit was automatically created by JtdPasCodegen, do not edit.
}
unit basic_properties;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TBaz = class sealed(specialize TJtdList<TJtdBool>)
    class function GetJtdClass: TJtdEntityClass; override;
  end;

  TQuux = class sealed(specialize TJtdList<TJtdBool>)
    class function GetJtdClass: TJtdEntityClass; override;
  end;

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
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
    property Foo: TJtdBool read FFoo write SetFoo;
    property Bar: TJtdString read FBar write SetBar;
    property Baz: TBaz read FBaz write SetBaz;
    property Quux: TQuux read FQuux write SetQuux;
  end;

implementation

{ TBaz }

class function TBaz.GetJtdClass: TJtdEntityClass;
begin
  Result := TBaz;
end;

{ TQuux }

class function TQuux.GetJtdClass: TJtdEntityClass;
begin
  Result := TQuux;
end;

{ TBasicProperties }

class function TBasicProperties.GetJtdClass: TJtdEntityClass;
begin
  Result := TBasicProperties;
end;

procedure TBasicProperties.Clear;
begin
  FreeAndNil(FFoo);
  FreeAndNil(FBar);
  FreeAndNil(FBaz);
  FreeAndNil(FQuux);
end;

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
procedure TBasicProperties.DoReadJson(aNode: TJsonNode);
var
  p: TJsonNode.TPair;
  Flags: array[0..3] of Boolean;
  I: Integer;
begin
  if not aNode.IsObject then ReadError;
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  for p in aNode.Entries do
    case p.Key of
      'foo':
        begin
          FFoo := TJtdBool(TJtdBool.ReadJson(p.Value));
          Flags[0] := True;
        end;
      'bar':
        begin
          FBar := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[1] := True;
        end;
      'baz':
        begin
          FBaz := TBaz(TBaz.ReadJson(p.Value));
          Flags[2] := True;
        end;
      'quux':
        begin
          FQuux := TQuux(TQuux.ReadJson(p.Value));
          Flags[3] := True;
        end;
    else
      UnknownProp(p.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('foo');
        1: PropNotFound('bar');
        2: PropNotFound('baz');
        3: PropNotFound('quux');
      else
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TBasicProperties.DoReadJson(aReader: TJsonReader);
var
  Flags: array[0..3] of Boolean;
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
          FFoo := TJtdBool(TJtdBool.ReadJson(aReader));
          Flags[0] := True;
        end;
      'bar':
        begin
          FBar := TJtdString(TJtdString.ReadJson(aReader));
          Flags[1] := True;
        end;
      'baz':
        begin
          FBaz := TBaz(TBaz.ReadJson(aReader));
          Flags[2] := True;
        end;
      'quux':
        begin
          FQuux := TQuux(TQuux.ReadJson(aReader));
          Flags[3] := True;
        end;
    else
      UnknownProp(aReader.Name);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('foo');
        1: PropNotFound('bar');
        2: PropNotFound('baz');
        3: PropNotFound('quux');
      else
      end;
end;
{$POP}

procedure TBasicProperties.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName('foo');
  Foo.WriteJson(aWriter);
  aWriter.AddName('bar');
  Bar.WriteJson(aWriter);
  aWriter.AddName('baz');
  Baz.WriteJson(aWriter);
  aWriter.AddName('quux');
  Quux.WriteJson(aWriter);
  aWriter.EndObject;
end;

end.
