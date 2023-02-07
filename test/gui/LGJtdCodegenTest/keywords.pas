{
  Source schema: keywords.jtd.json

  This unit was automatically created by JtdPasCodegen, do not edit.
}
unit keywords;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TFor_String = TJtdString;

  TObject_String = TJtdString;

  TKeywords = class sealed(TJtdObject)
  private
    FFor_: TFor_String;
    FObject_: TObject_String;
    procedure SetFor_(aValue: TFor_String);
    procedure SetObject_(aValue: TObject_String);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
  { refers to "for" JSON property }
    property For_: TFor_String read FFor_ write SetFor_;
  { refers to "object" JSON property }
    property Object_: TObject_String read FObject_ write SetObject_;
  end;

implementation

{ TKeywords }

class function TKeywords.GetJtdClass: TJtdEntityClass;
begin
  Result := TKeywords;
end;

procedure TKeywords.Clear;
begin
  FreeAndNil(FFor_);
  FreeAndNil(FObject_);
end;

procedure TKeywords.SetFor_(aValue: TFor_String);
begin
  if aValue = FFor_ then exit;
  FFor_.Free;
  FFor_ := aValue;
end;

procedure TKeywords.SetObject_(aValue: TObject_String);
begin
  if aValue = FObject_ then exit;
  FObject_.Free;
  FObject_ := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TKeywords.DoReadJson(aNode: TJsonNode);
var
  p: TJsonNode.TPair;
  Flags: array[0..1] of Boolean;
  I: Integer;
begin
  if not aNode.IsObject then ReadError;
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  for p in aNode.Entries do
    case p.Key of
      'for':
        begin
          FFor_ := TFor_String(TFor_String.ReadJson(p.Value));
          Flags[0] := True;
        end;
      'object':
        begin
          FObject_ := TObject_String(TObject_String.ReadJson(p.Value));
          Flags[1] := True;
        end;
    else
      UnknownProp(p.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('for');
        1: PropNotFound('object');
      else
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TKeywords.DoReadJson(aReader: TJsonReader);
var
  Flags: array[0..1] of Boolean;
  I: Integer;
begin
  if aReader.TokenKind <> tkObjectBegin then ReadError;
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReadError;
    if aReader.TokenKind = tkObjectEnd then break;
    case aReader.Name of
      'for':
        begin
          FFor_ := TFor_String(TFor_String.ReadJson(aReader));
          Flags[0] := True;
        end;
      'object':
        begin
          FObject_ := TObject_String(TObject_String.ReadJson(aReader));
          Flags[1] := True;
        end;
    else
      UnknownProp(aReader.Name);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('for');
        1: PropNotFound('object');
      else
      end;
end;
{$POP}

procedure TKeywords.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName('for');
  For_.WriteJson(aWriter);
  aWriter.AddName('object');
  Object_.WriteJson(aWriter);
  aWriter.EndObject;
end;

end.
