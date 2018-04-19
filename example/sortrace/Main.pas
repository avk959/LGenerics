{
  Inspired by HANTAO ZHANG, BAOLUO MENG, YIWEN LIANG, "Sort Race"

  It would be useful to compare performance of different sorting algorithms
  on different samples of input data.
}
unit Main;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, TypInfo, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls,
  Spin, Grids, LGArrayHelpers, Samples;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btRunSelected: TButton;
    btCancel: TButton;
    btRunAll: TButton;
    cbSampleSize: TComboBox;
    cbSampleClass: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MainStatus: TStatusBar;
    pnGrid: TPanel;
    pnLeft: TPanel;
    pnRight: TPanel;
    pnMain: TPanel;
    seKValue: TSpinEdit;
    seIterationCount: TSpinEdit;
    sgResultView: TStringGrid;
    Splitter1: TSplitter;
    procedure btCancelClick(Sender: TObject);
    procedure btRunAllClick(Sender: TObject);
    procedure btRunSelectedClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  type
    TDoubleHelper = specialize TGComparableArrayHelper<Double>;
    TSingleResult = array[TSortAlgo] of Double;
    TMultiResult  = array[TSampleClass, TSortAlgo] of Double;
  var
    FWaitStatus: string;
    FSingleResult: TSingleResult;
    FMultiResult: TMultiResult;
    FTestRunning,
    FCancelTest: Boolean;
    procedure FillControls;
    procedure FillcbSampleSize;
    procedure FillcbSampleClass;
    procedure FillGridFixedCells;
    procedure DisableControls;
    procedure EnableControls;
    function  GetSampleRow(aClass: TSampleClass): Longint; inline;
    function  GetAlgoCol(Algo: TSortAlgo): Longint; inline;
    procedure UpdateStatus(IsRunning: Boolean);
    procedure AppShowHint(Sender: TObject);
    procedure AppQueryEndSession(var {%H-}Cancel: Boolean);
    procedure SetStatusText(aPanelIdx: SizeInt; const aText: string);
    procedure ShowSingleTestResult;
    procedure ShowMultiTestResult;
    procedure DoRunSingleTest(aSize: TSampleSize; aClass: TSampleClass; K, Count: SizeInt);
    procedure DoRunAllTests(aSize: TSampleSize; K, Count: SizeInt);

    procedure FormCreated;
    procedure RunSingleTest;
    procedure RunAllTests;
    procedure RunCancelled;
  public
    property TestRunning: Boolean read FTestRunning;
    property CancelTest: Boolean read FCancelTest;
    property WaitStatus: string read FWaitStatus;
  end;

var
  frmMain: TfrmMain;

const
  SPause             = 'Pause';
  SAppRunning        = ' - running';
  SSingleCompleteFmt = 'Single test for "%s" with array size %s and K = %u complete';
  SMultiCompleteFmt  = 'All tests with array size %s and K = %u complete';
  SSampleClassFmt    = 'Sample class: %s';
  SSampleSizeFmt     = 'Sample size: %s';
  SAlgoFmt           = 'Algorithm: %s';
  SIterationFmt      = 'Iteration: %u';
  STestCancelled     = 'Test cancelled by user';
  SSumOfMean         = 'Sum of average times';
  SBestTime          = 'Best average time, s';
  SampleClassCol     = 0;
  AlgoNameRow        = 0;
  BestTimeCol        = 1;

implementation
{$COPERATORS ON}

{$R *.lfm}

const

  pnDataSample = 0;
  pnDataSize   = 1;
  pnSortAlgo   = 2;
  pnIteration  = 3;
  RoundFactor  = 1000;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FormCreated;
end;

procedure TfrmMain.btRunSelectedClick(Sender: TObject);
begin
  RunSingleTest;
end;

procedure TfrmMain.btCancelClick(Sender: TObject);
begin
  RunCancelled;
end;

procedure TfrmMain.btRunAllClick(Sender: TObject);
begin
  RunAllTests;
end;

procedure TfrmMain.FillControls;
begin
  FillcbSampleSize;
  FillcbSampleClass;
  FillGridFixedCells;
end;

procedure TfrmMain.FillcbSampleSize;
var
  s: TSampleSize;
begin
  cbSampleSize.Clear;
  for s in TSampleSize do
    cbSampleSize.Items.Add(s.ToString);
  cbSampleSize.ItemIndex := 0;
end;

procedure TfrmMain.FillcbSampleClass;
var
  s: TSampleClass;
begin
  cbSampleClass.Clear;
  for s in TSampleClass do
    cbSampleClass.Items.Add(s.GetName);
  cbSampleClass.ItemIndex := 0;
end;

procedure TfrmMain.FillGridFixedCells;
var
  I: LongInt;
begin
  sgResultView.RowCount := Ord(High(TSampleClass)) + 3;
  sgResultView.ColCount := Ord(High(TSortAlgo)) + 3;
  for I := 1 to sgResultView.RowCount - 2 do
    sgResultView.Cells[SampleClassCol, I] := TSampleClass(I - 1).GetName;
  sgResultView.Cells[SampleClassCol, Pred(sgResultView.RowCount)] := SSumOfMean;
  sgResultView.Cells[1, 0] := SBestTime;
  for I := 2 to Pred(sgResultView.ColCount) do
    sgResultView.Cells[I, AlgoNameRow] := TSortAlgo(I - 2).GetName + '*';
  sgResultView.AutoSizeColumns;
end;

procedure TfrmMain.DisableControls;
begin
  btRunSelected.Enabled := False;
  btRunAll.Enabled := False;
  cbSampleSize.Enabled := False;
  cbSampleClass.Enabled := False;
  seKValue.Enabled := False;
  seIterationCount.Enabled := False;
  btCancel.Enabled := True;
  Application.ProcessMessages;
end;

procedure TfrmMain.EnableControls;
begin
  btRunSelected.Enabled := True;
  btRunAll.Enabled := True;
  cbSampleSize.Enabled := True;
  cbSampleClass.Enabled := True;
  seKValue.Enabled := True;
  seIterationCount.Enabled := True;
  btCancel.Enabled := False;
  Application.ProcessMessages;
end;

function TfrmMain.GetSampleRow(aClass: TSampleClass): Longint;
begin
  Result := Longint(aClass) + 1;
end;

function TfrmMain.GetAlgoCol(Algo: TSortAlgo): Longint;
begin
  Result := Longint(Algo) + 2;
end;

procedure TfrmMain.UpdateStatus(IsRunning: Boolean);
begin
  if IsRunning then
    Caption := Application.Title + SAppRunning
  else
    Caption := Application.Title;
  FTestRunning := IsRunning;
  MainStatus.SimplePanel := not IsRunning;
  if MainStatus.SimplePanel then
    MainStatus.SimpleText := WaitStatus;
  Application.ProcessMessages;
end;

procedure TfrmMain.AppShowHint(Sender: TObject);
var
  h: string;
begin
  h := Application.Hint;
  with MainStatus do
    if h <> '' then
      begin
        SimpleText := h;
        SimplePanel := True;
      end
    else
      begin
        SimplePanel := not TestRunning;
        SimpleText := WaitStatus;
      end;
end;

procedure TfrmMain.AppQueryEndSession(var Cancel: Boolean);
begin
  Close;
end;

procedure TfrmMain.SetStatusText(aPanelIdx: SizeInt; const aText: string);
begin
  MainStatus.Panels[aPanelIdx].Text := aText;
end;

procedure TfrmMain.ShowSingleTestResult;
var
  MinTime: Double;
  Algo, WinAlgo: TSortAlgo;
  Sample: TSampleClass;
begin
  if CancelTest then
    exit;
  sgResultView.Clean([gzNormal]);
  Sample := TSampleClass(cbSampleClass.ItemIndex);
  WinAlgo := TSortAlgo(TDoubleHelper.IndexOfMin(FSingleResult));
  MinTime := FSingleResult[WinAlgo];
  for Algo in TSortAlgo do
    sgResultView.Cells[GetAlgoCol(Algo), GetSampleRow(Sample)] :=
      FloatToStr(Round(FSingleResult[Algo]/MinTime*RoundFactor)/RoundFactor);
  sgResultView.Cells[BestTimeCol, GetSampleRow(Sample)] := FloatToStrF(MinTime, ffFixed, 0, 8);
  sgResultView.AutoSizeColumns;
  FWaitStatus := Format(
    SSingleCompleteFmt, [Sample.GetName, cbSampleSize.Items[cbSampleSize.ItemIndex], seKValue.Value]);
end;

procedure TfrmMain.ShowMultiTestResult;
var
  MinTime: Double;
  Algo: TSortAlgo;
  Sample: TSampleClass;
  AverageSum: TSingleResult;
begin
  if CancelTest then
    exit;
  sgResultView.Clean([gzNormal]);
  AverageSum := Default(TSingleResult);
  for Sample in TSampleClass do
    begin
      TDoubleHelper.FindMin(FMultiResult[Sample], MinTime);
      sgResultView.Cells[BestTimeCol, GetSampleRow(Sample)] := FloatToStrF(MinTime, ffFixed, 0, 8);
      for Algo in TSortAlgo do
        begin
          AverageSum[Algo] += FMultiResult[Sample, Algo];
          FMultiResult[Sample, Algo] := FMultiResult[Sample, Algo] / MinTime;
          sgResultView.Cells[GetAlgoCol(Algo), GetSampleRow(Sample)] :=
            FloatToStr(Round(FMultiResult[Sample, Algo]*RoundFactor)/RoundFactor);
        end;
    end;
  TDoubleHelper.FindMin(AverageSum, MinTime);
  sgResultView.Cells[BestTimeCol, Pred(sgResultView.RowCount)] := FloatToStrF(MinTime, ffFixed, 0, 5);
  for Algo in TSortAlgo do
    begin
      AverageSum[Algo] /= MinTime;
      sgResultView.Cells[GetAlgoCol(Algo), Pred(sgResultView.RowCount)] :=
        FloatToStr(Round(AverageSum[Algo]*RoundFactor)/RoundFactor);
    end;
  sgResultView.AutoSizeColumns;
  FWaitStatus := Format(SMultiCompleteFmt, [TSampleSize(cbSampleSize.ItemIndex).ToString, seKValue.Value]);
end;

procedure TfrmMain.DoRunSingleTest(aSize: TSampleSize; aClass: TSampleClass; K, Count: SizeInt);
var
  I: SizeInt;
  TotalTime: Double;
  Algo: TSortAlgo;
begin
  for Algo in TSortAlgo do
    begin
      TotalTime := 0.0;
      SetStatusText(pnSortAlgo, Format(SAlgoFmt, [Algo.GetName]));
      for I := 1 to Count do
        begin
          SetStatusText(pnIteration, Format(SIterationFmt, [I]));
          Application.ProcessMessages;
          if CancelTest then
            exit;
          TotalTime += aClass.RunTest(Algo, aSize, K);
        end;
      FSingleResult[Algo] := TotalTime/Count;
    end;
end;

procedure TfrmMain.DoRunAllTests(aSize: TSampleSize; K, Count: SizeInt);
var
  I: SizeInt;
  TotalTime: Double;
  Algo: TSortAlgo;
  Sample: TSampleClass;
begin
  for Sample in TSampleClass do
    begin
      SetStatusText(pnDataSample, Format(SSampleClassFmt, [Sample.GetName]));
      for Algo in TSortAlgo do
        begin
          TotalTime := 0.0;
          SetStatusText(pnSortAlgo, Format(SAlgoFmt, [Algo.GetName]));
          for I := 1 to Count do
            begin
              SetStatusText(pnIteration, Format(SIterationFmt, [I]));
              Application.ProcessMessages;
              if CancelTest then
                exit;
              TotalTime += Sample.RunTest(Algo, aSize, K);
            end;
          FMultiResult[Sample, Algo] := TotalTime / Count;
        end;
    end;
end;

procedure TfrmMain.FormCreated;
begin
  Caption := Application.Title;
  Application.OnHint := @AppShowHint;
  Application.OnQueryEndSession := @AppQueryEndSession;
  FWaitStatus := SPause;
  MainStatus.SimplePanel := True;
  MainStatus.SimpleText := WaitStatus;
  FillControls;
end;

procedure TfrmMain.RunSingleTest;
begin
  UpdateStatus(True);
  DisableControls;
  SetStatusText(pnDataSample, Format(SSampleClassFmt, [cbSampleClass.Items[cbSampleClass.ItemIndex]]));
  SetStatusText(pnDataSize, Format(SSampleSizeFmt, [cbSampleSize.Items[cbSampleSize.ItemIndex]]));
  FCancelTest := False;
  DoRunSingleTest(TSampleSize(cbSampleSize.ItemIndex), TSampleClass(cbSampleClass.ItemIndex), seKValue.Value,
                  seIterationCount.Value);
  ShowSingleTestResult;
  EnableControls;
  UpdateStatus(False);
end;

procedure TfrmMain.RunAllTests;
begin
  UpdateStatus(True);
  DisableControls;
  SetStatusText(pnDataSize, Format(SSampleSizeFmt, [cbSampleSize.Items[cbSampleSize.ItemIndex]]));
  FCancelTest := False;
  DoRunAllTests(TSampleSize(cbSampleSize.ItemIndex), seKValue.Value, seIterationCount.Value);
  ShowMultiTestResult;
  EnableControls;
  UpdateStatus(False);
end;

procedure TfrmMain.RunCancelled;
begin
  FCancelTest := True;
  FWaitStatus := STestCancelled;
end;

end.

