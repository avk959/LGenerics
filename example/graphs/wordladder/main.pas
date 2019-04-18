{*******************************************************************************************
*                                                                                          *
*  Word ladder (also known as Doublets, word-links, change-the-word puzzles, paragrams,    *
*  laddergrams, or Word golf) is a word game invented by Lewis Carroll. A word ladder      *
*  puzzle begins with two words, and to solve the puzzle one must find a chain of other    *
*  words to link the two, in which two adjacent words (that is, words in successive steps) *
*  differ by one letter.                                                                   *
*                                                                                          *
*  Each step consists of a single letter substitution. For example, the following are      *
*  solutions to the word ladder puzzle between words "cold" and "warm".                    *
*     COLD → CORD → CARD → WARD → WARM                                                     *
*     COLD → CORD → CORM → WORM → WARM                                                     *
*     COLD → WOLD → WORD → WARD → WARM                                                     *
*                                                                                          *
*  from https://en.wikipedia.org/wiki/Word_ladder                                          *
*                                                                                          *
********************************************************************************************}
unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  DateUtils,
  LGUtils,
  LGSparseGraph,
  LGSimpleGraph;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btSolve: TButton;
    btClear: TButton;
    edTarget: TEdit;
    edSource: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    mmResult: TMemo;
    procedure btClearClick(Sender: TObject);
    procedure btSolveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    slWords: TStringList;
    Graph: TStrChart;
    CurrWordLen: SizeInt;
    CurrSource,
    CurrTarget: string;
    function  LoadDictionary: Boolean;
    procedure BuildGraph;
    procedure NewGraph;
    function  InputValid: Boolean;
    procedure SearchFor;
    procedure DisableControls;
    procedure EnableControls;
    procedure PrintPath(aPath: TIntArray);
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  if not LoadDictionary then
    begin
      ShowMessage(
      'To work properly you need a dictionary'+ LineEnding +'in the application folder named "words.txt"');
      Application.Terminate;
      exit;
    end;
  Graph := TStrChart.Create;
  Caption := 'Word ladder - ' + slWords.Count.ToString + ' words';
end;

procedure TfrmMain.btClearClick(Sender: TObject);
begin
  mmResult.Clear;
end;

procedure TfrmMain.btSolveClick(Sender: TObject);
begin
  SearchFor;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  slWords.Free;
  Graph.Free;
end;

function TfrmMain.LoadDictionary: Boolean;
begin
  if not FileExists('words.txt') then
    exit(False);
  slWords := TStringList.Create;
  slWords.LoadFromFile('words.txt');
  Result := True;
end;

procedure TfrmMain.BuildGraph;
var
  I, J, Pos, Diff: SizeInt;
  s, d: string;
begin
  Graph.Clear;
  for s in slWords do
    if Length(s) = CurrWordLen then
      Graph.AddVertex(s);
  for I := 0 to Pred(Graph.VertexCount) do
    begin
      s := Graph[I];
      for J := Succ(I) to Pred(Graph.VertexCount) do
        begin
          d := Graph[J];
          Diff := 0;
          for Pos := 1 to CurrWordLen do
            if s[Pos] <> d[Pos] then
              begin
                Inc(Diff);
                if Diff > 1 then
                  break;
              end;
          if Diff = 1 then
            Graph.AddEdgeI(I, J);
        end;
      Application.ProcessMessages;
    end;
end;

procedure TfrmMain.NewGraph;
var
  StartTime: TTime;
  Elapsed: SizeInt;
const
  IsBuiltFmt = 'graph is built with %d vertices and %d edges in %d milliseconds';
begin
  mmResult.Append('');
  mmResult.Append('started building a new graph, please wait...');
  StartTime := Time;
  BuildGraph;
  Elapsed := MilliSecondsBetween(Time, StartTime);
  mmResult.Append(Format(IsBuiltFmt,[Graph.VertexCount, Graph.EdgeCount, Elapsed]));
  mmResult.Append('separate component count = ' + Graph.SeparateCount.ToString);
end;

function TfrmMain.InputValid: Boolean;
var
  OldCaption: string;
begin
  CurrSource := edSource.Text;
  if CurrSource = '' then
    begin
      mmResult.Append('need to specify a source word');
      exit(False);
    end;
  CurrTarget := edTarget.Text;
  if CurrTarget = '' then
    begin
      mmResult.Append('need to specify a target word');
      exit(False);
    end;
  if CurrSource.Length <> CurrTarget.Length then
    begin
      mmResult.Append('the target word need to be the same length as the source word');
      exit(False);
    end;
  if CurrSource = CurrTarget then
    begin
      mmResult.Append('words need to be different');
      exit(False);
    end;
  if CurrWordLen <> CurrSource.Length then
    begin
      CurrWordLen := CurrSource.Length;
      DisableControls;
      OldCaption := Caption;
      Caption := 'Word ladder - building new graph...';
      NewGraph;
      EnableControls;
      Caption := OldCaption;
    end;
  Result := True;
end;

procedure TfrmMain.SearchFor;
var
  Path: TIntArray;
  StartTime: TTime;
  Elapsed: SizeInt;
const
  SearchFmt = 'started searching for ladder from "%s" to "%s"';
  FinishFmt = 'searching for ladder from "%s" to "%s" finished, elapsed time: %d milliseconds';
begin
  if not InputValid then
    exit;
  if not Graph.ContainsVertex(CurrSource) then
    begin
      mmResult.Append('sourse word not found');
      exit;
    end;
  if not Graph.ContainsVertex(CurrTarget) then
    begin
      mmResult.Append('target word not found');
      exit;
    end;
  if not Graph.PathExists(CurrSource, CurrTarget) then
    begin
      mmResult.Append('ladder not found');
      exit;
    end;
  mmResult.Append(Format(SearchFmt, [CurrSource, CurrTarget]));
  StartTime := Time;
  Path := Graph.ShortestPath(CurrSource, CurrTarget);
  Elapsed := MilliSecondsBetween(Time, StartTime);
  mmResult.Append(Format(FinishFmt, [CurrSource, CurrTarget, Elapsed]));
  if Length(Path) > 0 then
    PrintPath(Path)
  else
    mmResult.Append('ladder not found');
end;

procedure TfrmMain.DisableControls;
begin
  edSource.Enabled := False;
  edTarget.Enabled := False;
  btSolve.Enabled := False;
  btClear.Enabled := False;
  Screen.Cursor := crHourGlass;
end;

procedure TfrmMain.EnableControls;
begin
  edSource.Enabled := True;
  edTarget.Enabled := True;
  btSolve.Enabled := True;
  btClear.Enabled := True;
  Screen.Cursor := crDefault;
end;

procedure TfrmMain.PrintPath(aPath: TIntArray);
type
  TStringBuilderRef = specialize TGAutoRef<TStringBuilder>;
var
  I, Len: SizeInt;
  SbRef: TStringBuilderRef;
  sb: TStringBuilder;
begin
  Len := Length(aPath);
  mmResult.Append('ladder: ');
  if Len = 0 then
    begin
      mmResult.Append('empty');
      exit;
    end;
  if Len < 2 then
    begin
      mmResult.Append(Graph[aPath[0]]);
      exit;
    end;
  sb := {%H-}SbRef;
  for I := 0 to Len - 2 do
    begin
      sb.Append(Graph[aPath[I]]);
      sb.Append(' - ');
    end;
  sb.Append(Graph[aPath[High(aPath)]]);
  mmResult.Append(sb.ToString);
end;

end.

