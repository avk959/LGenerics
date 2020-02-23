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
  TCounter    = specialize TGLiteHashMultiSetLP<string, string>;
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
  ReadRef: specialize TGAutoRef<TTextFileReader>;
  CountRef: TCounter;
  DictRef: TDictionary;

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
  function ToLower(constref aValue: string): string;
  begin
    Result := LowerCase(aValue);
    Inc(TotalCount);
  end;
  function CompareItem(constref L, R: TCountItem): Boolean;
  begin
    Result := R.Count < L.Count;
  end;
  function IsReservedWord(constref aValue: string): Boolean;
  begin
    Result := DictRef.Contains(aValue);
  end;
var
  FileName, CurrLine: string;
  Item: TCountItem;
begin
  DictRef.AddAll([{$I reswords.inc}]);
  if AllowModifiers then
    DictRef.AddAll([{$I modifiers.inc}]);

  for FileName in InFiles do
    if ReadRef.Instance.Open(FileName) then
      for CurrLine in ReadRef.Instance do
        CountRef.AddAll(CurrLine.Words.Map(@ToLower).Select(@IsReservedWord))
    else
      WriteLn(Format(sFailedOpenFmt, [FileName]));

  with TTextFileWriter.Create(OutFile) do
    try
      if IsOpen then
        for Item in THelper.Sorted(CountRef.ToEntryArray, @CompareItem) do
          WriteLn(OutFile^, Item.Count,  #9, Item.Key)
      else
        WriteLn(Format(sFailedCreateFmt, [OutFile]));
    finally
      Free;
    end;
  Writeln('total words:             ', TotalCount);
  Writeln('total reserved words:    ', CountRef.Count);
  Writeln('distinct reserved words: ', CountRef.EntryCount);
  Writeln(sFinished);
end;

begin
  if ParamsFound then
    ProcessFiles
  else
    Writeln(UsageString);
end.

