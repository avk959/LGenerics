{
  pascal reserved words usage statistics
}
program rwstat;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}

uses
  SysUtils,
  LGUtils,
  LGHashSet,
  LGHashMultiSet,
  LGHelpers,
  LGMiscUtils,
  LGStrHelpers;

type
  TCounter    = specialize TGHashMultiSetLP<string>;
  TDictionary = specialize TGHashSetLP<string>;
  TCountItem  = TCounter.TEntry;

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
  CountRef: specialize TGAutoRef<TCounter>;
  DictRef: specialize TGAutoRef<TDictionary>;

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
  function CompareItem(constref L, R: TCountItem): SizeInt;
  begin
    Result := -SizeInt.Compare(L.Count, R.Count);
  end;
var
  FileName, CurrLine: string;
  Item: TCountItem;
begin
  DictRef.Instance.AddAll([{$I reswords.inc}]);
  if AllowModifiers then
    DictRef.Instance.AddAll([{$I modifiers.inc}]);

  for FileName in InFiles do
    if ReadRef.Instance.Open(FileName) then
      for CurrLine in ReadRef.Instance do
        CountRef.Instance.AddAll(CurrLine.Words.Map(@ToLower).Select(@DictRef.Instance.Contains))
    else
      WriteLn(Format(sFailedOpenFmt, [FileName]));

  with TTextFileWriter.Create(OutFile) do
    try
      if IsOpen then
        for Item in CountRef.Instance.Entries.Sorted(@CompareItem) do
          Writeln(OutFile^, Item.Count, #9, Item.Key)
      else
        WriteLn(Format(sFailedCreateFmt, [OutFile]));
    finally
      Free;
    end;
  Writeln('total words:             ', TotalCount);
  Writeln('total reserved words:    ', CountRef.Instance.Count);
  Writeln('distinct reserved words: ', CountRef.Instance.EntryCount);
  Writeln(sFinished);
end;

begin
  if ParamsFound then
    ProcessFiles
  else
    Writeln(UsageString);
end.

