{
  Source schema: keywords.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit keywords;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TFor_ = TJtdString;

  TObject_ = TJtdString;

  TRootObject = class sealed(TJtdObject)
  private
    FFor_: TFor_;
    FObject_: TObject_;
    function  GetFor_: TFor_;
    function  GetObject_: TObject_;
    procedure SetFor_(aValue: TFor_);
    procedure SetObject_(aValue: TObject_);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure CreateProps; override;
    procedure ClearProps; override;
    procedure WriteProps(aWriter: TJsonStrWriter); override;
  public
  { refers to "for" JSON property }
    property For_: TFor_ read GetFor_ write SetFor_;
  { refers to "object" JSON property }
    property Object_: TObject_ read GetObject_ write SetObject_;
  end;

implementation

{ TRootObject }

function TRootObject.GetFor_: TFor_;
begin
  CheckNull;
  Result := FFor_;
end;

function TRootObject.GetObject_: TObject_;
begin
  CheckNull;
  Result := FObject_;
end;

procedure TRootObject.SetFor_(aValue: TFor_);
begin
  DoAssign;
  if aValue = FFor_ then exit;
  FFor_.Free;
  FFor_ := aValue;
end;

procedure TRootObject.SetObject_(aValue: TObject_);
begin
  DoAssign;
  if aValue = FObject_ then exit;
  FObject_.Free;
  FObject_ := aValue;
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
      'for':
        if not Flags[0] then begin
          FFor_.ReadJson(e.Value);
          Flags[0] := True;
        end else DuplicateProp(e.Key);
      'object':
        if not Flags[1] then begin
          FObject_.ReadJson(e.Value);
          Flags[1] := True;
        end else DuplicateProp(e.Key);
    else
      UnknownProp(e.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('for');
        1: PropNotFound('object');
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
      'for':
        if not Flags[0] then begin
          FFor_.ReadJson(aReader);
          Flags[0] := True;
        end else DuplicateProp(aReader);
      'object':
        if not Flags[1] then begin
          FObject_.ReadJson(aReader);
          Flags[1] := True;
        end else DuplicateProp(aReader);
    else
      UnknownProp(aReader.Name, aReader);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('for', aReader);
        1: PropNotFound('object', aReader);
      end;
end;
{$POP}

procedure TRootObject.CreateProps;
begin
  FFor_ := TFor_.Create;
  FObject_ := TObject_.Create;
end;

procedure TRootObject.ClearProps;
begin
  FFor_.Free;
  FObject_.Free;
end;

procedure TRootObject.WriteProps(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('for');
  FFor_.WriteJson(aWriter);
  aWriter.AddName('object');
  FObject_.WriteJson(aWriter);
end;

end.
