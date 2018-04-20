unit Main;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, EditBtn,
  LGAsync,
  LGArrayHelpers,
  EpikTimer;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btFutures: TButton;
    btFutures1: TButton;
    btSequential: TButton;
    btSequential1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    lbResultTime: TLabel;
    lbParallelResult: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure btFuturesClick(Sender: TObject);
    procedure btSequentialClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btFutures1Click(Sender: TObject);
    procedure btSequential1Click(Sender: TObject);
  private
    FTimer: TEpikTimer;
  public

  end;

const
  PieceSize  = 10000;
  PieceCount = 64;
  BigSize    = 1000000;

type
  TDWordArray = array of DWord;
  TTestData   = array[1..PieceCount] of TDWordArray;
  TSortHelper = specialize TGComparableArrayHelper<DWord>;


var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

function CreateData: TTestData;
var
  I, J: Integer;
begin
  for I := 1 to PieceCount do
    begin
      SetLength(Result[I], PieceSize);
      for J := 0 to High(Result[I]) do
        Result[I][J] := Random(High(DWord));
    end;
end;

function CreateBigArray: TDWordArray;
var
  I: Integer;
begin
  SetLength(Result, BigSize);
  for I := 0 to High(Result) do
    Result[I] := I;
  TSortHelper.RandomShuffle(Result);
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := 'Futures test';
  FTimer := TEpikTimer.Create(Self);
  FTimer.CorrelateTimebases;
end;

function DataSorted(constref aData: TTestData): Boolean;
var
  I: Integer;
begin
  for I := 1 to PieceCount do
    if not TSortHelper.IsNonDescending(aData[I]) then
      exit(False);
  Result :=  True;
end;

procedure TfrmMain.btSequentialClick(Sender: TObject);
var
  Data: TTestData;
  Elapsed: Extended;
  I: Integer;
begin
  Data := CreateData;
  FTimer.Clear;
  FTimer.Start;
  ////////////////
  for I := 1 to PieceCount do
    TSortHelper.Sort(Data[I]);
  ////////////////
  FTimer.Stop;
  Elapsed := FTimer.Elapsed;
  lbResultTime.Caption := FloatToStr(Round(Elapsed*100000)/100000) + ' s';
  if not DataSorted(Data) then
    ShowMessage('Sequential sort failed');
end;


function Sort(constref a: TDWordArray): Boolean;
begin
  TSortHelper.Sort(a);
  Result := True;
end;

procedure TfrmMain.btFuturesClick(Sender: TObject);
type
  TFunc   = specialize TGAsyncFunc<TDWordArray, Boolean>;
  TFuture = specialize TGFuture<Boolean>;
var
  Futures: array[1..PieceCount]of TFuture;
  Data: TTestData;
  Elapsed: Extended;
  I: Integer;
begin
  Data := CreateData;
  FTimer.Clear;
  FTimer.Start;
  ////////////////////
  for I := 1 to PieceCount do
    Futures[I] := TFunc.Call(@Sort, Data[I]);
  for I := 1 to PieceCount do
    Futures[I].GetValue.OrElse(True);
  //////////////////
  FTimer.Stop;
  Elapsed := FTimer.Elapsed;
  lbResultTime.Caption := FloatToStr(Round(Elapsed*100000)/100000) + ' s';
  if not DataSorted(Data) then
    ShowMessage('Sort with futures failed');
end;

procedure TfrmMain.btSequential1Click(Sender: TObject);
var
  Data: TDWordArray;
  Elapsed: Extended;
begin
  Data := CreateBigArray;
  FTimer.Clear;
  FTimer.Start;
  /////////////////////
  TSortHelper.Sort(Data);
  ////////////////////
  FTimer.Stop;
  Elapsed := FTimer.Elapsed;
  lbParallelResult.Caption := FloatToStr(Round(Elapsed*100000)/100000) + ' s';
  if not TSortHelper.IsStrictAscending(Data) then
    ShowMessage('Sequential sort failed');
end;


function SortLeft(constref a: TDWordArray; constref aFrom, aTo: Integer): Integer;
begin
  TSortHelper.Sort(a[aFrom..aTo]);
  Result := aFrom;
end;

function SortRight(constref a: TDWordArray; constref aFrom, aTo: Integer): Integer;
begin
  TSortHelper.Sort(a[aFrom..aTo]);
  Result := aTo;
end;

function MergeSortLeft(constref a: TDWordArray; constref aFrom, aTo: Integer): Integer;
begin
  TSortHelper.MergeSort(a[aFrom..aTo]);
  Result := aFrom;
end;

function MergeSortRight(constref a: TDWordArray; constref aFrom, aTo: Integer): Integer;
begin
  TSortHelper.MergeSort(a[aFrom..aTo]);
  Result := aTo;
end;

procedure TfrmMain.btFutures1Click(Sender: TObject);
type
  TFunc   = specialize TGAsync3AFunc<TDWordArray, Integer, Integer, Integer>;
  TFuture = specialize TGFuture<Integer>;
var
  p1, p2, p3, p4, p5, p6:  TFuture;
  Data: TDWordArray;
  Elapsed: Extended;
  ChankSize: Integer;
begin
  TTaskExecutor.EnsureThreadCount(4);
  Data := CreateBigArray;
  ChankSize := Length(Data) div 4;
  FTimer.Clear;
  FTimer.Start;
  ////////////////////////////////////////////////////////////////
  p1 := TFunc.Call(@SortLeft, Data, 0, Pred(ChankSize));
  p2 := TFunc.Call(@SortRight, Data, ChankSize, Pred(ChankSize*2));
  p3 := TFunc.Call(@SortLeft, Data, ChankSize*2, Pred(ChankSize*3));
  p4 := TFunc.Call(@SortRight, Data, ChankSize*3, High(Data));

  p5 := TFunc.Call(@MergeSortLeft, Data, p1.Value, p2.Value);
  p6 := TFunc.Call(@MergeSortRight, Data, p3.Value, p4.Value);

  TFunc.Call(@MergeSortLeft, Data, p5.Value, p6.Value).Value;
  ////////////////////////////////////////////////////////////////
  FTimer.Stop;
  Elapsed := FTimer.Elapsed;
  lbParallelResult.Caption := FloatToStr(Round(Elapsed*100000)/100000) + ' s';
  if not TSortHelper.IsStrictAscending(Data) then
    ShowMessage('Sort with futures failed');
end;

end.

