{
  A knight's tour is a sequence of moves of a knight on a chessboard
  such that the knight visits every square exactly once.

  An example of the simplest solution for an 8x8 chessboard
  (from a Russian forum member, slightly modified).
}
program knight_tour;

{$mode objfpc}{$H+}

uses
  SysUtils,
  lgUtils,
  lgSparseGraph,
  lgSimpleGraph;

type
  TSolver = class
  strict private
  const
    FindSpan = 5;
  var
    FBoard: TStrChart;
    procedure CreateKnightMoveGraph;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Solve;
  end;

  TKnightTourSolver = specialize TGAutoRef<TSolver>;

procedure TSolver.CreateKnightMoveGraph;

  procedure TryAddEdge(x0, y0, x1, y1: Integer);
  begin
    if (DWord(x1) <= 7) and (DWord(y1) <= 7) then
      FBoard.AddEdgeI(x0 * 8 + y0, x1 * 8 + y1);
  end;

var
  I, J: Integer;
const
  Chars: array[1..8] of AnsiChar = 'abcdefgh';
begin
  FBoard := TStrChart.Create;
  for I in [1..8] do
    for J in [1..8] do
      FBoard.AddVertex(Chars[I] + J.ToString);
  for I in [0..7] do
    for J in [0..7] do
      begin
        TryAddEdge(I, J, I - 1, J - 2);
        TryAddEdge(I, J, I + 1, J - 2);
        TryAddEdge(I, J, I + 1, J + 2);
        TryAddEdge(I, J, I - 1, J + 2);
        TryAddEdge(I, J, I - 2, J - 1);
        TryAddEdge(I, J, I + 2, J - 1);
        TryAddEdge(I, J, I + 2, J + 1);
        TryAddEdge(I, J, I - 2, J + 1);
      end;
end;

constructor TSolver.Create;
begin
  CreateKnightMoveGraph;
end;

destructor TSolver.Destroy;
begin
  FBoard.Free;
  inherited;
end;

procedure TSolver.Solve;
var
  Src, I: SizeInt;
  PathVec: TIntArrayVector;
  Path: TIntArray;
const
  EdgeSquares: array of SizeInt = (
    0, 1, 2, 3, 4, 5, 6, 7, 8, 15, 16, 23, 24, 31, 32, 39, 40, 47, 48,
    55, 56, 57, 58, 59, 60, 61, 62, 63);
begin
  Src := EdgeSquares[Random(Length(EdgeSquares))];
  if not(FBoard.FindHamiltonPathsI(Src, 1, PathVec, FindSpan) and
         FBoard.IsHamiltonPath(PathVec[0], Src)) then
    begin
      WriteLn('Couldn''t find any solution from square ', FBoard[Src], ' within ', FindSpan, ' s');
      exit;
    end;
  WriteLn('Found the first solution from square ', FBoard[Src], ':');
  Path := PathVec[0];
  for I := 0 to Length(Path) - 2 do
    Write(FBoard[Path[I]], '-');
  WriteLn(FBoard[Path[High(Path)]]);
end;

var
  Solver: TKnightTourSolver;

begin
  Randomize;
  Solver.Instance.Solve;
{$ifdef mswindows}
  ReadLn;
{$endif}
end.


