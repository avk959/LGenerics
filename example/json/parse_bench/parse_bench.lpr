{
  The benchmark expects the path to the samples folder as a parameter
}
program parse_bench;
{$mode delphi}
uses

  LazUtf8, Classes, SysUtils, DateUtils, FileUtil,
  lgUtils, lgJson, JsonTools, FpJson, JsonParser;

type
  TParseFun = function(const s: string): Boolean;
  TParser   = TGTuple2<string, TParseFun>;

function lgParse(const s: string): Boolean;
var
  o: lgJson.TJsonNode;
begin
  if lgJson.TJsonNode.TryParse(s, o) then
    begin
      o.Free;
      exit(True);
    end;
  Result := False;
end;

function jtParse(const s: string): Boolean;
var
  o: TJsonNode;
begin
  o := TJsonNode.Create;
  Result := o.TryParse(s);
  o.Free;
end;

function fpParse(const s: string): Boolean;
var
  o: TJSONData = nil;
begin
  try
    o := GetJSON(s);
    Result := o <> nil;
    if Result then
      o.Free;
  except
    Result := False;
  end;
end;

const
  Parsers: array of TParser = [
    (F1: 'lgJson';    F2: @lgParse),
    (F1: 'JsonTools'; F2: @jtParse),
    (F1: 'FpJson';    F2: @fpParse)];
  Interval = 5000;
  RepCount = 5;

var
  SampleDir: string = '';
  FileList: TGSharedRef<TStringList>;

procedure Run;
var
  Stream: TGAutoRef<TStringStream>;
  Parser: TParser;
  CurrFile, FileName, JsonText: string;
  I, Score, BestScore, Times: Integer;
  Start: TTime;
begin
  for CurrFile in FileList.Instance do
    begin
      Stream.Instance.LoadFromFile(CurrFile);
      FileName := ExtractFileName(CurrFile);
      JsonText := Stream.Instance.DataString;
      for Parser in Parsers do
        begin
          BestScore := 0;
          for I := 1 to RepCount do
            begin
              Times := 0;
              Start := Time;
              while MillisecondsBetween(Time, Start) < Interval do
                begin
                  if not Parser.F2(JsonText) then
                    break;
                  Inc(Times);
                end;
              Score := Round(Length(JsonText) * Times/MillisecondsBetween(Time, Start));
              if Score > BestScore then
                BestScore := Score;
            end;
           WriteLn(StdErr, FileName, ' ', Parser.F1, ' ', BestScore);
           WriteLn(FileName, ' ', Parser.F1, ' ', BestScore);
        end;
    end;
end;

begin
  if ParamCount < 1 then exit;
  if not DirectoryExists(ParamStr(1)) then exit;
  SampleDir := ParamStr(1);
  FileList.Instance := FindAllFiles(SampleDir);
  Run;
end.

