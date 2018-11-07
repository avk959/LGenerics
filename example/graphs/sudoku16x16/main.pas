{
  16x16 sudoku generator (using graph coloring)
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
    seCluesCount: TSpinEdit;
    procedure btNewClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sgCellsDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; {%H-}aState: TGridDrawState);
    procedure sgCellsPrepareCanvas(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
  private
  type
    THelper   = specialize TGOrdinalArrayHelper<SizeInt>;
    TSolution = array[0..255] of SizeInt;
    TSeed     = array[1..16] of SizeInt;
  var
    Graph: TIntChart;
    Solution,
    Clues: TSolution;
    SaveColor: TColor;
    function  GenSudokuGraph: TIntChart;
    function  CreateColorArray: TIntArray;
    function  CreateRangeArray: TIntArray;
    procedure CreateClues;
    procedure NewSeed(var aSeed: TSeed);
    function  GenSolution: Boolean;
    procedure NewSolution;
    procedure ShowClues;
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
  SaveColor := sgCells.Font.Color;
  Graph := GenSudokuGraph;
  Randomize;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Graph.Free;
end;

procedure TfrmMain.sgCellsDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  if (aCol <> 0) and (aRow <> 0) then
    with sgCells.Canvas do
      begin
        if aCol and 3 = 0 then
          begin
            Pen.Color := sgCells.GridLineColor;
            MoveTo(aRect.Right - 2, aRect.Top);
            LineTo(aRect.Right - 2, aRect.Bottom);
          end;
        if aRow and 3 = 0 then
          begin
            Pen.Color := sgCells.GridLineColor;
            MoveTo(aRect.Left, aRect.Bottom - 2);
            LineTo(aRect.Right, aRect.Bottom - 2);
         end;
      end;
end;

procedure TfrmMain.sgCellsPrepareCanvas(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
begin
  if sgCells.Objects[aCol, aRow] = nil then
    sgCells.Canvas.Font.Color := SaveColor
  else
    sgCells.Canvas.Font.Color := clBlue;
end;

function TfrmMain.GenSudokuGraph: TIntChart;
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
  FillChar(Pointer(Result)^, SizeOf(SizeInt) * Result.Length, 0);
end;

function TfrmMain.CreateRangeArray: TIntArray;
var
  I: SizeInt;
begin
  Result{%H-}.Length := 256;
  for I := 0 to High(Result) do
    Result[I] := I;
end;

procedure TfrmMain.CreateClues;
var
  I: SizeInt;
  ToHide: TIntArray;
begin
  ToHide := CreateRangeArray;
  THelper.RandomShuffle(ToHide);
  ToHide.Length := ToHide.Length - seCluesCount.Value;
  Clues := Solution;
  for I in ToHide do
    Clues[I] := 0;
end;

procedure TfrmMain.NewSeed(var aSeed: TSeed);
var
  I: SizeInt;
begin
  for I := Low(aSeed) to High(aSeed) do
    aSeed[I] := I;
  THelper.RandomShuffle(aSeed); //16!
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
  for I := Low(Seed) to High(Seed) do
    Colors[Graph.IndexOf(I)] := Seed[I];

  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;
  try
    Result := Graph.FindCompleteColoring(16, Colors, 5);//wait for at most 5s
  finally
    Screen.Cursor := OldCursor;
  end;

  if not Result then
    begin
      ShowMessage('Failed to create solution,' + sLineBreak + 'try again.');
      exit;
    end;

  for I := Low(Colors) to High(Colors) do
    Solution[I] := Colors[I];
  CreateClues;
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
      ShowClues;
  finally
    btNew.Enabled := True;
    btShow.Enabled := Enable or Done;
  end;
end;

procedure TfrmMain.ShowClues;
var
  Col, Row, I: Integer;
begin
  sgCells.BeginUpdate;
  try
    I := 0;
    for Col := 1 to Pred(sgCells.ColCount) do
      for Row := 1 to Pred(sgCells.RowCount) do
        begin
          if Clues[I] > 0 then
            begin
              sgCells.Cells[Col, Row] := Clues[I].ToString;
              sgCells.Objects[Col, Row] := TObject(Clues[I]);
            end
          else
            begin
              sgCells.Cells[Col, Row] := '';
              sgCells.Objects[Col, Row] := nil;
            end;
          Inc(I);
        end;
  finally
    sgCells.EndUpdate;
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
        sgCells.Cells[Col, Row] := Solution[I].ToString;
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
  btShow.Enabled := False;
end;

end.

