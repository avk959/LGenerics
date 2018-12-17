{
  The eight queens puzzle is the problem of placing eight chess queens on an
  8×8 chessboard so that no two queens threaten each other. Thus, a solution
  requires that no two queens share the same row, column, or diagonal.

  This example finds all solutions of this puzzle;
  The chessboard will be presented in the form of a graph. We will connect with
  the edge each field with all other fields reachable by a queen from the given
  field. The required solutions of the puzzle are exactly the maximum independent
  sets of the constructed graph.
}
program queens;

{$mode objfpc}{$H+}

uses
  SysUtils,
  LGUtils,
  LGVector,
  LGSparseGraph,
  LGSimpleGraph;

type
  TStrArrayVector = specialize TGLiteVector<TStringArray>;

  TQueensSolver = class
  private
    FBoard: TStrChart;
    FSolutions: TStrArrayVector;
    procedure BuildBoard;
    function  CopySolution(const s: TIntArray): TStringArray;
    procedure OnFindSolution(const s: TIntArray; var {%H-}aCancel: Boolean);
    procedure PrintSolutions;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Solve;
  end;

  TSolverRef = specialize TGAutoRef<TQueensSolver>;

var
  Ref: TSolverRef;
  Solver: TQueensSolver;

procedure TQueensSolver.BuildBoard;
const
  Chars: array[1..8] of AnsiChar = 'abcdefgh';
var
  I, J, K: Integer;
  Brd: array[1..8, 1..8] of string;
  Field: string;
begin
  //fill board
  for I in [1..8] do
    for J in [1..8] do
      begin
        Field := Chars[I] + J.ToString;
        Brd[I, J] := Field;
        FBoard.AddVertex(Field);
      end;
  //connect all the fields that are reachable by the queen
  //verticals
  for K in [1..8] do
    for I in [1..8] do
      for J := Succ(I) to 8 do
        FBoard.AddEdge({%H-}Brd[K, I], {%H-}Brd[K, J]);

  //horizontals
  for K in [1..8] do
    for I in [1..8] do
      for J := Succ(I) to 8 do
        FBoard.AddEdge(Brd[I, K], Brd[J, K]);

  //right diagonals
  for K in [1..8] do
    for I := 1 to Succ(8 - K) do
      for J := Succ(I) to Succ(8 - K) do
        FBoard.AddEdge(Brd[Pred(K + I), I], Brd[Pred(K + J), J]);
  for K in [2..8] do
    for I := 1 to Succ(8 - K) do
      for J := Succ(I) to Succ(8 - K) do
        FBoard.AddEdge(Brd[I, Pred(K + I)], Brd[J, Pred(K + J)]);

  //left diagonals
  for K  := 8 downto 1 do
    for I := 1 to K do
      for J := Succ(I) to K do
        FBoard.AddEdge(Brd[I, Succ(K - I)], Brd[J, Succ(K - J)]);
  for K  := 2 to 8 do
    for I := 1 to Succ(8 - K) do
      for J := Succ(I) to Succ(8 - K) do
        FBoard.AddEdge(Brd[Pred(K + I), Succ(8 - I)], Brd[Pred(K + J), Succ(8 - J)]);
end;

function TQueensSolver.CopySolution(const s: TIntArray): TStringArray;
var
  I: SizeInt;
begin
  SetLength(Result, Length(s));
  for I := 0 to High(s) do
    Result[I] := FBoard[s[I]];
end;

procedure TQueensSolver.OnFindSolution(const s: TIntArray; var aCancel: Boolean);
begin
  if Length(s) = 8 then
    FSolutions.Add(CopySolution(s));
end;

procedure TQueensSolver.PrintSolutions;
const
  Fmt = '%s, %s, %s, %s, %s, %s, %s, %s';
var
  a: TStringArray;
begin
  for a in FSolutions do
    Writeln(Format(Fmt, [a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]]));
  Writeln;
  Writeln('Total solutions: ', FSolutions.Count);
end;

constructor TQueensSolver.Create;
begin
  FBoard := TStrChart.Create;
end;

destructor TQueensSolver.Destroy;
begin
  FBoard.Free;
  inherited;
end;

procedure TQueensSolver.Solve;
begin
  BuildBoard;
  FBoard.ListAllMIS(@OnFindSolution);
  PrintSolutions;
end;

begin
  Solver := {%H-}Ref;
  Solver.Solve;
  Writeln('Press any key to exit');
  Readln;
end.

