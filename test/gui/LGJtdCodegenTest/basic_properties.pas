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
    procedure SetFoo(aValue: TJtdBool);
    procedure SetBar(aValue: TJtdString);
    procedure SetBaz(aValue: specialize TJtdList<TJtdBool>);
    procedure SetQuux(aValue: specialize TJtdList<TJtdBool>);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteProps(aWriter: TJsonStrWriter); override;
  public
    procedure Clear; override;
  { refers to "foo" JSON property }
    property Foo: TJtdBool read FFoo write SetFoo;
  { refers to "bar" JSON property }
    property Bar: TJtdString read FBar write SetBar;
  { refers to "baz" JSON property }
    property Baz: specialize TJtdList<TJtdBool> read FBaz write SetBaz;
  { refers to "quux" JSON property }
    property Quux: specialize TJtdList<TJtdBool> read FQuux write SetQuux;
  end;

implementation

{ TRootObject }

procedure TRootObject.SetFoo(aValue: TJtdBool);
begin
  if aValue = FFoo then exit;
  FFoo.Free;
  FFoo := aValue;
end;

procedure TRootObject.SetBar(aValue: TJtdString);
begin
  if aValue = FBar then exit;
  FBar.Free;
  FBar := aValue;
end;

procedure TRootObject.SetBaz(aValue: specialize TJtdList<TJtdBool>);
begin
  if aValue = FBaz then exit;
  FBaz.Free;
  FBaz := aValue;
end;

procedure TRootObject.SetQuux(aValue: specialize TJtdList<TJtdBool>);
begin
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
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  for e in aNode.Entries do begin
    if (FTagField <> '') and (e.Key = FTagField) then continue;
    case e.Key of
      'foo':
        if not Flags[0] then begin
          FFoo := TJtdBool(TJtdBool.LoadInstance(e.Value));
          Flags[0] := True;
        end else DuplicateProp(e.Key);
      'bar':
        if not Flags[1] then begin
          FBar := TJtdString(TJtdString.LoadInstance(e.Value));
          Flags[1] := True;
        end else DuplicateProp(e.Key);
      'baz':
        if not Flags[2] then begin
          FBaz := specialize TJtdList<TJtdBool>(specialize TJtdList<TJtdBool>.LoadInstance(e.Value));
          Flags[2] := True;
        end else DuplicateProp(e.Key);
      'quux':
        if not Flags[3] then begin
          FQuux := specialize TJtdList<TJtdBool>(specialize TJtdList<TJtdBool>.LoadInstance(e.Value));
          Flags[3] := True;
        end else DuplicateProp(e.Key);
    else
      UnknownProp(e.Key);
    end;
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
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
    if (FTagField <> '') and (aReader.Name = FTagField) then continue;
    case aReader.Name of
      'foo':
        if not Flags[0] then begin
          FFoo := TJtdBool(TJtdBool.LoadInstance(aReader));
          Flags[0] := True;
        end else DuplicateProp(aReader);
      'bar':
        if not Flags[1] then begin
          FBar := TJtdString(TJtdString.LoadInstance(aReader));
          Flags[1] := True;
        end else DuplicateProp(aReader);
      'baz':
        if not Flags[2] then begin
          FBaz := specialize TJtdList<TJtdBool>(specialize TJtdList<TJtdBool>.LoadInstance(aReader));
          Flags[2] := True;
        end else DuplicateProp(aReader);
      'quux':
        if not Flags[3] then begin
          FQuux := specialize TJtdList<TJtdBool>(specialize TJtdList<TJtdBool>.LoadInstance(aReader));
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

procedure TRootObject.DoWriteProps(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('foo');
  Foo.WriteJson(aWriter);
  aWriter.AddName('bar');
  Bar.WriteJson(aWriter);
  aWriter.AddName('baz');
  Baz.WriteJson(aWriter);
  aWriter.AddName('quux');
  Quux.WriteJson(aWriter);
end;

procedure TRootObject.Clear;
begin
  FreeAndNil(FFoo);
  FreeAndNil(FBar);
  FreeAndNil(FBaz);
  FreeAndNil(FQuux);
end;

end.
