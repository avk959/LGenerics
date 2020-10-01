{
  pascal reserved words usage statistics
}
program rwstat_lite;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}
{$MODESWITCH ADVANCEDRECORDS}

uses
  SysUtils,
  LGUtils,
  LGHashSet,
  LGHashMultiSet,
  LGHelpers,
  LGMiscUtils,
  LGStrHelpers,
  LGArrayHelpers;

type
  TCountSpec  = specialize TGLiteHashMultiSetLP<string, string>;
  TCounter    = TCountSpec.TMultiSet;
  TDictSpec   = specialize TGLiteHashSetLP<string, string>;
  TDictionary = TDictSpec.TSet;
  TCountItem  = TCounter.TEntry;
  THelper     = specialize TGNestedArrayHelper<TCountItem>;

const

  UsageString =
  'Usage: rwstat [OPTIONS] [FILES]'           + LineEnding + //pascal source files
  'Options:'                                  + LineEnding +
  '  -o --output:NAME   set output file name' + LineEnding + //by default stdout
  '  -m --modifiers     including modifiers'  + LineEnding +
  '  -h --help          print this help menu';

  sFailedOpenFmt   = 'Failed to open file %s';
  sFailedCreateFmt = 'Failed to create file %s';
  sFinished        = 'rwstat finished';

var
  OutFile: string = 'stdout';
  InFiles: TStringArray = nil;
  AllowModifiers: Boolean = False;
  Reader: specialize TGAutoRef<TTextFileReader>;
  Counter: TCounter;
  Dictionary: TDictionary;

function ParamsFound: Boolean;
var
  Opt: TCmdLineOption;
begin
  if ParamCount < 1 then
    exit(False);
  for Opt in CmdLineOptions do
    case Opt.Kind of
      pkOption, pkLongOption:
        case Opt.Key of
          'h', 'help':
            exit(False);
          'm', 'modifiers':
            AllowModifiers := True;
          'o', 'output':
            if Opt.Value <> '' then
              OutFile := Opt.Value;
        end;
      pkArgument:
        InFiles.Add(Opt.Value);
    end;
  Result := Length(InFiles) > 0;
end;

procedure ProcessFiles;
var
  TotalCount: SizeInt = 0;
  function ToLower(const aValue: string): string;
  begin
    Result := LowerCase(aValue);
    Inc(TotalCount);
  end;
  function CompareItem(const L, R: TCountItem): Boolean;
  begin
    Result := R.Count < L.Count;
  end;
  function IsReservedWord(const aValue: string): Boolean;
  begin
    Result := Dictionary.Contains(aValue);
  end;
var
  FileName, CurrLine: string;
  Item: TCountItem;
begin
  Dictionary.AddAll([{$I reswords.inc}]);
  if AllowModifiers then
    Dictionary.AddAll([{$I modifiers.inc}]);

  for FileName in InFiles do
    if Reader.Instance.Open(FileName) then
      for CurrLine in Reader.Instance do
        Counter.AddAll(CurrLine.Words.Map(@ToLower).Select(@IsReservedWord))
    else
      WriteLn(Format(sFailedOpenFmt, [FileName]));

  with TTextFileWriter.Create(OutFile) do
    try
      if IsOpen then
        for Item in THelper.Sorted(Counter.ToEntryArray, @CompareItem) do
          WriteLn(OutFile^, Item.Count,  #9, Item.Key)
      else
        WriteLn(Format(sFailedCreateFmt, [OutFile]));
    finally
      Free;
    end;
  Writeln('total words:             ', TotalCount);
  Writeln('total reserved words:    ', Counter.Count);
  Writeln('distinct reserved words: ', Counter.EntryCount);
  Writeln(sFinished);
end;

begin
  if ParamsFound then
    ProcessFiles
  else
    Writeln(UsageString);
end.

