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
    lbResultTime1: TLabel;
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
    procedure StartTimer;
  public

  end;

const
  PieceSize  = 10000;
  PieceCount = 128;
  BigSize    = 1000000;

type
  TDWordArray  = array of DWord;
  PDWordArray  = ^TDWordArray;
  TSmallArray  = array[0..PieceSize - 1] of DWord;
  PSmallArray  = ^TSmallArray;
  TTestData    = array of TSmallArray;
  TSmallArrays = array of PSmallArray;
  THelper      = specialize TGComparableArrayHelper<DWord>;


var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

function CreateData: TTestData;
var
  I, J: Integer;
begin
  SetLength(Result, PieceCount);
  for I := 0 to Pred(PieceCount) do
    for J := 0 to Pred(PieceSize) do
      Result[I, J] := Random(MaxInt);
end;

function CreateBigArray: TDWordArray;
var
  I: Integer;
begin
  SetLength(Result, BigSize);
  for I := 0 to High(Result) do
    Result[I] := I;
  THelper.RandomShuffle(Result);
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := 'Futures test';
  FTimer := TEpikTimer.Create(Self);
  FTimer.CorrelateTimebases;
  TDefaultExecutor.EnsureThreadCount(8);
end;

function DataSorted(constref aData: TTestData): Boolean;
var
  I: Integer;
begin
  for I := 0 to Pred(PieceCount) do
    if not THelper.IsNonDescending(aData[I]) then
      exit(False);
  Result :=  True;
end;

procedure TfrmMain.btSequentialClick(Sender: TObject);
var
  Data: TTestData;
  I: Integer;
begin
  Data := CreateData;
  StartTimer;
  ////////////////
  for I := 0 to Pred(PieceCount) do
    THelper.Sort(Data[I]);
  ////////////////
  FTimer.Stop;
  lbResultTime.Caption := FloatToStr(Round(FTimer.Elapsed*100000)/100000) + ' s';
  if not DataSorted(Data) then
    ShowMessage('Sequential sort failed');
end;

function Sort(const a: PSmallArray): Boolean;
begin
  THelper.Sort(a^);
  Result := True;
end;

function CreateDataList(const aData: TTestData): TSmallArrays;
var
  I: SizeInt;
  sa: TSmallArrays = nil;
begin
  SetLength(sa, Length(aData));
  for I := 0 to Pred(Length(aData)) do
    sa[I] := @aData[I];
  Result := sa;
end;

procedure TfrmMain.btFuturesClick(Sender: TObject);
type
  TMonadic = specialize TGAsyncMonadic<PSmallArray, Boolean>;
var
  Data: TTestData;
begin
  Data := CreateData;
  StartTimer;
  ////////////////////
  TMonadic.Spawn(@Sort, CreateDataList(Data), DefaultExecutor).WaitFor;
  //////////////////
  FTimer.Stop;
  lbResultTime.Caption := FloatToStr(Round(FTimer.Elapsed*100000)/100000) + ' s';
  if not DataSorted(Data) then
    ShowMessage('Sort with futures failed');
end;

procedure TfrmMain.btSequential1Click(Sender: TObject);
var
  Data: TDWordArray;
begin
  Data := CreateBigArray;
  StartTimer;
  /////////////////////
  THelper.Sort(Data);
  ////////////////////
  FTimer.Stop;
  lbResultTime1.Caption := FloatToStr(Round(FTimer.Elapsed*100000)/100000) + ' s';
  if not THelper.IsStrictAscending(Data) then
    ShowMessage('Sequential sort failed');
end;

procedure TfrmMain.StartTimer;
begin
  FTimer.Clear;
  FTimer.Start;
end;


function SortLeft(const a: PDWordArray; const aFrom, aTo: Integer): Integer;
begin
  THelper.Sort(a^[aFrom..aTo]);
  Result := aFrom;
end;

function SortRight(const a: PDWordArray; const aFrom, aTo: Integer): Integer;
begin
  THelper.Sort(a^[aFrom..aTo]);
  Result := aTo;
end;

function MergeSortLeft(const a: PDWordArray; const aFrom, aTo: Integer): Integer;
begin
  THelper.MergeSort(a^[aFrom..aTo]);
  Result := aFrom;
end;

function MergeSortRight(const a: PDWordArray; const aFrom, aTo: Integer): Integer;
begin
  THelper.MergeSort(a^[aFrom..aTo]);
  Result := aTo;
end;

procedure TfrmMain.btFutures1Click(Sender: TObject);
type
  TTriadic = specialize TGAsyncTriadic<PDWordArray, Integer, Integer, Integer>;
var
  p1, p2, p3, p4, p5, p6: specialize TGFuture<Integer>;
  Data: TDWordArray;
  ChankSize: Integer;
begin
  Data := CreateBigArray;
  ChankSize := Length(Data) div 4;
  StartTimer;
  ////////////////////////////////////////////////////////////////
  p1 := TTriadic.Call(@SortLeft, @Data, 0, Pred(ChankSize), DefaultExecutor);
  p2 := TTriadic.Call(@SortRight, @Data, ChankSize, Pred(ChankSize*2), DefaultExecutor);
  p3 := TTriadic.Call(@SortLeft, @Data, ChankSize*2, Pred(ChankSize*3), DefaultExecutor);
  p4 := TTriadic.Call(@SortRight, @Data, ChankSize*3, High(Data), DefaultExecutor);

  p5 := TTriadic.Call(@MergeSortLeft, @Data, p1.Value, p2.Value, DefaultExecutor);
  p6 := TTriadic.Call(@MergeSortRight, @Data, p3.Value, p4.Value, DefaultExecutor);

  TTriadic.Call(@MergeSortLeft, @Data, p5.Value, p6.Value, DefaultExecutor).WaitFor;
  ////////////////////////////////////////////////////////////////
  FTimer.Stop;
  lbResultTime1.Caption := FloatToStr(Round(FTimer.Elapsed*100000)/100000) + ' s';
  if not THelper.IsStrictAscending(Data) then
    ShowMessage('Sort with futures failed');
end;

end.

