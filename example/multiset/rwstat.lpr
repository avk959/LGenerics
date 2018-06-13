{ pascal reserved words usage statistics }
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
  OutFileName: string = 'stdout';
  InFileNames: TStringArray = nil;
  AllowModifiers: Boolean = False;
  InputText: specialize TGAutoRef<TTextFileReader>;
  Counter: specialize TGAutoRef<TCounter>;
  Dict: specialize TGAutoRef<TDictionary>;

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
          'h', 'help': exit(False);
          'm', 'modifiers': AllowModifiers := True;
          'o', 'output':  if Opt.Value <> '' then OutFileName := Opt.Value;
        end;
      pkArgument: InFileNames.Add(Opt.Value);
    end;
  Result := Length(InFileNames) > 0;
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
  Dict.Instance.AddAll([{$I reswords.inc}]);
  if AllowModifiers then
    Dict.Instance.AddAll([{$I modifiers.inc}]);

  for FileName in InFileNames do
    if InputText.Instance.Open(FileName) then //LoadText ???
      for CurrLine in InputText.Instance do
        Counter.Instance.AddAll(CurrLine.SplitSB.Map(@ToLower).Select(@Dict.Instance.Contains))
    else
      WriteLn(Format(sFailedOpenFmt, [FileName]));

  with TTextFileWriter.Create(OutFileName) do
    try
      if IsOpen then
        for Item in Counter.Instance.Entries.Sorted(@CompareItem) do
          Writeln(OutFile^, Item.Count, #9, Item.Key)
      else
        WriteLn(Format(sFailedCreateFmt, [OutFileName]));
    finally
      Free;
    end;
  Writeln('total words:             ', TotalCount);
  Writeln('total reserved words:    ', Counter.Instance.Count);
  Writeln('distinct reserved words: ', Counter.Instance.EntryCount);
  Writeln(sFinished);
end;

begin
  if ParamsFound then
    ProcessFiles
  else
    Writeln(UsageString);
end.

