{
  Source schema: optional_properties.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit optional_properties;

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
    function  GetFoo: TJtdString;
    function  GetBar: specialize TJtdList<TJtdString>;
    function  GetBaz: TJtdBool;
    procedure SetFoo(aValue: TJtdString);
    procedure SetBar(aValue: specialize TJtdList<TJtdString>);
    procedure SetBaz(aValue: TJtdBool);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoClear; override;
    procedure WriteProps(aWriter: TJsonStrWriter); override;
  public

  { All of the following properties are optional, so may contain NILs }

  { refers to "foo" JSON property }
    property Foo: TJtdString read GetFoo write SetFoo;
  { refers to "bar" JSON property }
    property Bar: specialize TJtdList<TJtdString> read GetBar write SetBar;
  { refers to "baz" JSON property }
    property Baz: TJtdBool read GetBaz write SetBaz;
  end;

implementation

{ TRootObject }

function TRootObject.GetFoo: TJtdString;
begin
  CheckNull;
  Result := FFoo;
end;

function TRootObject.GetBar: specialize TJtdList<TJtdString>;
begin
  CheckNull;
  Result := FBar;
end;

function TRootObject.GetBaz: TJtdBool;
begin
  CheckNull;
  Result := FBaz;
end;

procedure TRootObject.SetFoo(aValue: TJtdString);
begin
  DoAssign;
  if aValue = FFoo then exit;
  FFoo.Free;
  FFoo := aValue;
end;

procedure TRootObject.SetBar(aValue: specialize TJtdList<TJtdString>);
begin
  DoAssign;
  if aValue = FBar then exit;
  FBar.Free;
  FBar := aValue;
end;

procedure TRootObject.SetBaz(aValue: TJtdBool);
begin
  DoAssign;
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

procedure TRootObject.DoClear;
begin
  FreeAndNil(FFoo);
  FreeAndNil(FBar);
  FreeAndNil(FBaz);
end;

procedure TRootObject.WriteProps(aWriter: TJsonStrWriter);
begin
  if FFoo <> nil then begin
    aWriter.AddName('foo');
    FFoo.WriteJson(aWriter);
  end;
  if FBar <> nil then begin
    aWriter.AddName('bar');
    FBar.WriteJson(aWriter);
  end;
  if FBaz <> nil then begin
    aWriter.AddName('baz');
    FBaz.WriteJson(aWriter);
  end;
end;

end.
