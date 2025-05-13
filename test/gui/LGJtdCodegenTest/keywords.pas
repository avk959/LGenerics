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

  TFor_String = TJtdString;

  TObject_String = TJtdString;

  TKeywords = class sealed(TJtdObject)
  private
    FFor_: TFor_String;
    FObject_: TObject_String;
    procedure SetFor_(aValue: TFor_String);
    procedure SetObject_(aValue: TObject_String);
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
    procedure ClearFields; override;
  public
  { refers to "for" JSON property }
    property For_: TFor_String read FFor_ write SetFor_;
  { refers to "object" JSON property }
    property Object_: TObject_String read FObject_ write SetObject_;
  end;

implementation

{ TKeywords }

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
procedure TKeywords.DoReadJson(aReader: TJsonReader);
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

procedure TKeywords.WriteFields(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('for');
  For_.WriteJson(aWriter);
  aWriter.AddName('object');
  Object_.WriteJson(aWriter);
end;

procedure TKeywords.DoClear;
begin
end;

procedure TKeywords.ClearFields;
begin
  FFor_.Free;
  FObject_.Free;
end;

procedure TKeywords.CreateFields;
begin
  FFor_ := TFor_String.Create;
  FObject_ := TObject_String.Create;
end;

end.
