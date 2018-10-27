{
  generator sudoku 16 x 16
}
unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, Spin, StdCtrls,
  LGArrayHelpers,
  LGCustomGraph,
  LGSimpleGraph;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btNew: TButton;
    btShow: TButton;
    Label1: TLabel;
    sgCells: TStringGrid;
    seHintCount: TSpinEdit;
    procedure btNewClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sgCellsDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
  private
  type
    THelper   = specialize TGOrdinalArrayHelper<SizeInt>;
    TSolution = array[0..255] of SizeInt;
    TSeed     = array[1..16] of SizeInt;
  var
    Graph: TIntChart;
    CurrSolution,
    Hints: TSolution;
    function  GenGraph: TIntChart;
    function  CreateColorArray: TIntArray;
    function  CreateRangeArray: TIntArray;
    procedure CreateHints;
    procedure NewSeed(var aSeed: TSeed);
    function  GenSolution: Boolean;
    procedure NewSolution;
    procedure ShowSolutionHints;
    procedure ShowSolution;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Caption := Application.Title;
  for I := 1 to Pred(sgCells.ColCount) do
    sgCells.Cells[I, 0] := I.ToString;
  Graph := GenGraph;
  Randomize;
end;

procedure TfrmMain.sgCellsDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  if (aCol <> 0) and (aCol and 3 = 0) and (aRow <> 0) then
    with sgCells.Canvas do
      begin
        Pen.Color := sgCells.GridLineColor;
        MoveTo(aRect.Right - 2, aRect.Top);
        LineTo(aRect.Right - 2, aRect.Bottom);
      end;
  if (aRow <> 0) and (aRow and 3 = 0) and (aCol <> 0) then
    with sgCells.Canvas do
      begin
        Pen.Color := sgCells.GridLineColor;
        MoveTo(aRect.Left, aRect.Bottom - 2);
        LineTo(aRect.Right, aRect.Bottom - 2);
      end;
end;

function TfrmMain.GenGraph: TIntChart;
var
  I, J, K, I1, J1, I2: SizeInt;
  a: array[0..15] of SizeInt;
begin
  Result := TIntChart.Create;
  for I := 1 to 256 do
    Result.AddVertex(I);
  //lines
  K := 1;
  while K < 256 do
    begin
      for I := K to K + 14 do
        for J := Succ(I) to K + 15 do
          Result.AddEdge(I, J);
      K += 16;
    end;
  //columns
  for K := 1 to 16 do
    begin
      I := K;
      while I < K + 240 do
        begin
          J := I + 16;
          while J <= K + 240 do
            begin
              Result.AddEdge(I, J);
              J += 16;
            end;
          I += 16;
        end;
    end;
  //regions
  K := 1;
  while K < 256 do
    begin
      I := K;
      while I < K + 16 do
        begin
          J := 0;
          J1 := I;
          for I1 := 0 to 3 do
            begin
              for I2 := 0 to 3 do
                begin
                  a[J] := J1 + I2;
                  Inc(J);
                end;
              J1 += 16;
            end;
          for I1 := 0 to 15 do
            for I2 := Succ(I1) to 15 do
              Result.AddEdge(a[I1], a[I2]);
          I += 4;
        end;
      K += 64;
    end;
end;

function TfrmMain.CreateColorArray: TIntArray;
begin
  Result{%H-}.Length := 256;
  FillChar(Pointer(Result)^, SizeOf(SizeInt) * 256, 0);
end;

function TfrmMain.CreateRangeArray: TIntArray;
var
  I: SizeInt;
begin
  Result{%H-}.Length := 256;
  for I := 0 to 255 do
    Result[I] := I;
end;

procedure TfrmMain.CreateHints;
var
  I: SizeInt;
  a: TIntArray;
begin
  a := CreateRangeArray;
  THelper.RandomShuffle(a);
  a.Length := 256 - seHintCount.Value;
  Hints := CurrSolution;
  for I in a do
    Hints[I] := 0;
end;

procedure TfrmMain.NewSeed(var aSeed: TSeed);
var
  I: SizeInt;
begin
  for I := 1 to 16 do
    aSeed[I] := I;
  THelper.RandomShuffle(aSeed);
end;

function TfrmMain.GenSolution: Boolean;
var
  Seed: TSeed;
  Colors: TIntArray;
  OldCursor: TCursor;
  I: SizeInt;
begin
  Colors := CreateColorArray;
  NewSeed(Seed{%H-});
  for I := 1 to 16 do
    Colors[Graph.IndexOf(I)] := Seed[I];

  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    Result := Graph.FindCompleteColoring(16, Colors, 5);
  finally
    Screen.Cursor := OldCursor;
  end;

  if not Result then
    begin
      ShowMessage('Failed to create solution,' + sLineBreak + 'try again.');
      exit;
    end;

  for I := 0 to 255 do
    CurrSolution[I] := Colors[I];
  CreateHints;
end;

procedure TfrmMain.NewSolution;
var
  Enable, Done: Boolean;
begin
  Enable := btShow.Enabled;
  btNew.Enabled := False;
  btShow.Enabled := False;
  try
    Done := GenSolution;
    if Done then
      ShowSolutionHints;
  finally
    btNew.Enabled := True;
    btShow.Enabled := Enable or Done;
  end;
end;

procedure TfrmMain.ShowSolutionHints;
var
  Col, Row, I: Integer;
begin
  I := 0;
  for Col := 1 to Pred(sgCells.ColCount) do
    for Row := 1 to Pred(sgCells.RowCount) do
      begin
        if Hints[I] > 0 then
          sgCells.Cells[Col, Row] := Hints[I].ToString
        else
          sgCells.Cells[Col, Row] := '';
        Inc(I);
      end;
end;

procedure TfrmMain.ShowSolution;
var
  Col, Row, I: Integer;
begin
  I := 0;
  for Col := 1 to Pred(sgCells.ColCount) do
    for Row := 1 to Pred(sgCells.RowCount) do
      begin
        sgCells.Cells[Col, Row] := CurrSolution[I].ToString;
        Inc(I);
      end;
end;

procedure TfrmMain.btNewClick(Sender: TObject);
begin
  NewSolution;
end;

procedure TfrmMain.btShowClick(Sender: TObject);
begin
  ShowSolution;
end;

end.

