unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  DateUtils,
  LGraphUtils,
  LGSimpleGraph;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btSearch: TButton;
    btClear: TButton;
    edTarget: TEdit;
    edSource: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    mmResult: TMemo;
    procedure btClearClick(Sender: TObject);
    procedure btSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  type
    TOutline = specialize TGOutline<string, string>;
  var
    slWords: TStringList;
    gOutline: TOutline;
    CurrWordLen: SizeInt;
    CurrSource,
    CurrTarget: string;
    procedure LoadWords;
    procedure LoadOutline;
    function  SingleLetterDiff(constref L, R: string): Boolean;
    function  InputValid: Boolean;
    procedure SearchFor;
    procedure DisableInput;
    procedure EnableInput;
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
  gOutline := TOutline.Create;
  LoadWords;
  Caption := 'Word ladder - ' + slWords.Count.ToString + ' words';
end;

procedure TfrmMain.btClearClick(Sender: TObject);
begin
  mmResult.Clear;
end;

procedure TfrmMain.btSearchClick(Sender: TObject);
begin
  SearchFor;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  slWords.Free;
  gOutline.Free;
end;

procedure TfrmMain.LoadWords;
begin
  slWords := TStringList.Create;
  slWords.LoadFromFile('words.txt');
end;

procedure TfrmMain.LoadOutline;
var
  s: string;
  StartTime: TTime;
  Elapsed, I, J: SizeInt;
const
  IsBuiltFmt = 'graph is built with %d vertices and %d edges in %d milliseconds';
begin
  gOutline.Clear;
  mmResult.Append('');
  mmResult.Append('start building new graph');
  StartTime := Time;
  for s in slWords do
    if Length(s) = CurrWordLen then
      gOutline.AddVertex(s);
  for I := 0 to Pred(gOutline.VertexCount) do
    begin
      for J := Succ(I) to Pred(gOutline.VertexCount) do
        if SingleLetterDiff(gOutline[I], gOutline[J]) then
          gOutline.AddEdgeI(I, J);
      Application.ProcessMessages;
    end;
  Elapsed := MilliSecondsBetween(Time, StartTime);
  mmResult.Append(Format(IsBuiltFmt,[gOutline.VertexCount, gOutline.EdgeCount, Elapsed]));
end;

function TfrmMain.SingleLetterDiff(constref L, R: string): Boolean;
var
  I, Diff: SizeInt;
begin
  Diff := 0;
  for I := 1 to Length(L) do
    if L[I] <> R[I] then
      begin
        Inc(Diff);
        if Diff > 1 then
          exit(False);
      end;
  Result := True;
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
      DisableInput;
      OldCaption := Caption;
      Caption := 'Word ladder - building new graph...';
      LoadOutline;
      EnableInput;
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
  SearchFmt = 'start search for ladder from "%s" to "%s"';
  FinishFmt = 'search for ladder from "%s" to "%s" finished, elapsed time: %d milliseconds';
begin
  if not InputValid then
    exit;
  if not gOutline.ContainsVertex(CurrSource) then
    begin
      mmResult.Append('sourse word does not found');
      exit;
    end;
  if not gOutline.ContainsVertex(CurrTarget) then
    begin
      mmResult.Append('target word does not found');
      exit;
    end;
  mmResult.Append(Format(SearchFmt, [CurrSource, CurrTarget]));
  StartTime := Time;
  Path := gOutline.ShortestPath(CurrSource, CurrTarget);
  Elapsed := MilliSecondsBetween(Time, StartTime);
  mmResult.Append(Format(FinishFmt, [CurrSource, CurrTarget, Elapsed]));
  if Length(Path) > 0 then
    PrintPath(Path)
  else
    mmResult.Append('ladder not found');
end;

procedure TfrmMain.DisableInput;
begin
  edSource.Enabled := False;
  edTarget.Enabled := False;
  btSearch.Enabled := False;
  btClear.Enabled := False;
  Screen.Cursor := crHourGlass;
end;

procedure TfrmMain.EnableInput;
begin
  edSource.Enabled := True;
  edTarget.Enabled := True;
  btSearch.Enabled := True;
  btClear.Enabled := True;
  Screen.Cursor := crDefault;
end;

procedure TfrmMain.PrintPath(aPath: TIntArray);
var
  I, Len: SizeInt;
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
      mmResult.Append(gOutline[aPath[0]]);
      exit;
    end;
  sb := TStringBuilder.Create;
  try
    for I := 0 to Len - 2 do
      begin
        sb.Append(gOutline[aPath[I]]);
        sb.Append(' - ');
      end;
    sb.Append(gOutline[aPath[High(aPath)]]);
    mmResult.Append(sb.ToString);
  finally
    sb.Free;
  end;
end;

end.

