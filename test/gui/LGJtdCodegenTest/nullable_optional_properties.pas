{
  Source schema: nullable_optional_properties.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit nullable_optional_properties;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TRootObject = class sealed(TJtdObject)
  private
    FFoo: TJtdString;
    FBar: specialize TJtdList<TJtdString>;
    FBaz: TJtdBool;
    procedure SetFoo(aValue: TJtdString);
    procedure SetBar(aValue: specialize TJtdList<TJtdString>);
    procedure SetBaz(aValue: TJtdBool);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    procedure Clear; override;

  { All of the following properties are optional, so may contain NILs }

  { refers to "foo" JSON property; is nullable; }
    property Foo: TJtdString read FFoo write SetFoo;
  { refers to "bar" JSON property }
    property Bar: specialize TJtdList<TJtdString> read FBar write SetBar;
  { refers to "baz" JSON property; is nullable; }
    property Baz: TJtdBool read FBaz write SetBaz;
  end;

implementation

{ TRootObject }

procedure TRootObject.SetFoo(aValue: TJtdString);
begin
  if aValue = FFoo then exit;
  FFoo.Free;
  FFoo := aValue;
end;

procedure TRootObject.SetBar(aValue: specialize TJtdList<TJtdString>);
begin
  if aValue = FBar then exit;
  FBar.Free;
  FBar := aValue;
end;

procedure TRootObject.SetBaz(aValue: TJtdBool);
begin
  if aValue = FBaz then exit;
  FBaz.Free;
  FBaz := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TRootObject.DoReadJson(aNode: TJsonNode);
var
  e: TJsonNode.TPair;
begin
  if not aNode.IsObject then ExpectObject(aNode);
  Clear;
  for e in aNode.Entries do
    case e.Key of
      'foo':
         if FFoo = nil then
           FFoo := TJtdString(TJtdString.LoadInstance(e.Value))
         else DuplicateProp(e.Key);
      'bar':
         if FBar = nil then
           FBar := specialize TJtdList<TJtdString>(specialize TJtdList<TJtdString>.LoadInstance(e.Value))
         else DuplicateProp(e.Key);
      'baz':
         if FBaz = nil then
           FBaz := TJtdBool(TJtdBool.LoadInstance(e.Value))
         else DuplicateProp(e.Key);
    else
      UnknownProp(e.Key);
    end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TRootObject.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> tkObjectBegin then ExpectObject(aReader);
  Clear;
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
    case aReader.Name of
      'foo':
         if FFoo = nil then
           FFoo := TJtdString(TJtdString.LoadInstance(aReader))
         else DuplicateProp(aReader);
      'bar':
         if FBar = nil then
           FBar := specialize TJtdList<TJtdString>(specialize TJtdList<TJtdString>.LoadInstance(aReader))
         else DuplicateProp(aReader);
      'baz':
         if FBaz = nil then
           FBaz := TJtdBool(TJtdBool.LoadInstance(aReader))
         else DuplicateProp(aReader);
    else
      UnknownProp(aReader.Name, aReader);
    end;
  until False;
end;
{$POP}

procedure TRootObject.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  if Foo <> nil then begin
    aWriter.AddName('foo');
    Foo.WriteJson(aWriter);
  end;
  if Bar <> nil then begin
    aWriter.AddName('bar');
    Bar.WriteJson(aWriter);
  end;
  if Baz <> nil then begin
    aWriter.AddName('baz');
    Baz.WriteJson(aWriter);
  end;
  aWriter.EndObject;
end;

procedure TRootObject.Clear;
begin
  FreeAndNil(FFoo);
  FreeAndNil(FBar);
  FreeAndNil(FBaz);
end;

end.
