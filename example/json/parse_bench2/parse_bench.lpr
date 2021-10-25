{
  The benchmark expects the path to the samples folder as a parameter
}
program parse_bench;

{$mode delphi}

uses
  LazUtf8, Classes, SysUtils, DateUtils, FileUtil,
  lgUtils, lgJson,
  JsonTools,
  xquery, fastjsonreader,
  FpJson, JsonParser, JsonScanner;

type
  TParseFun = function(const s: string): Boolean;
  TParser   = TGTuple2<string, TParseFun>;

function lgParse(const s: string): Boolean;
var
  Json: lgJson.TJsonNode;
begin
  if lgJson.TJsonNode.TryParse(s, Json) then
    begin
      Json.Free;
      exit(True);
    end;
  Result := False;
end;

function jtParse(const s: string): Boolean;
begin
  Result := False;
  with TJsonNode.Create do
    try
      Result := TryParse(s);
    finally
      Free;
    end;
end;

function xqParse(const s: string): Boolean;
var
  Json: IXQValue;
begin
  try
    Json := TXQJsonParser.parse(s, []);
  except
  end;
  Result := Json <> nil;
end;

function fpParse(const s: string): Boolean;
var
  Json: TJSONData = nil;
  Parser: TJsonParser;
begin
  Result := False;
  Parser := TJsonParser.Create(s, [joUTF8, joStrict]);
  try
    try
      Json := Parser.Parse;
      Result := Json <> nil;
      if Result then
        Json.Free;
    except
    end;
  finally
    Parser.Free;
  end;
end;

const
  Parsers: array of TParser = [
    (F1: 'lgJson      '; F2: @lgParse),
    (F1: 'JsonTools   '; F2: @jtParse),
    (F1: 'FpJson      '; F2: @fpParse),
    (F1: 'XQJsonParser'; F2: @xqParse)];

  Interval = 2000;
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
  for Parser in Parsers do
    for CurrFile in FileList.Instance do
      begin
        Stream.Instance.LoadFromFile(CurrFile);
        FileName := ExtractFileName(CurrFile);
        JsonText := Stream.Instance.DataString;
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
            if I < RepCount then
              Sleep(2000);
          end;
         WriteLn(StdErr, '1 ', FileName, ' ', Parser.F1, ' ', BestScore);
         WriteLn('1 ', FileName, ' ', Parser.F1, ' ', BestScore);
         Sleep(5000);
      end;
end;

begin
  if (ParamCount < 1) or not DirectoryExists(ParamStr(1)) then exit;
  SampleDir := ParamStr(1);
  FileList.Instance := FindAllFiles(SampleDir);
  Run;
end.

