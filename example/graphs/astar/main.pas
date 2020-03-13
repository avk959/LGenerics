{
  This is a simple comparison of the performance of Dijkstra and A* algorithms
  on a random graph of points in a plane.
}
unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  LGUtils, LGSparseGraph, LGSimpleGraph, LGHashSet;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btFind: TButton;
    btNew: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    mmInfo: TMemo;
    sedPointCount: TSpinEdit;
    sedSouce: TSpinEdit;
    sedTarget: TSpinEdit;
    procedure btFindClick(Sender: TObject);
    procedure btNewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCurrGraph: specialize TGAutoRef<TPointsChart>;
    function CreateNewGraph: TPointsChart;
  public

  end;

var
  frmMain: TfrmMain;

implementation
{$B-}{$COPERATORS ON}

{$R *.lfm}

{$PUSH}{$J+}
const
  CpuClockPeriod: Double = 0.0;
{$POP}

{$ASMMODE INTEL}
function RDTSC: QWord; assembler; nostackframe;
asm
  rdtscp
{$IFDEF CPUAMD64}
  shl rdx, 32
  or  rax, rdx
{$ENDIF}
end;

procedure CheckCpuSpeed;
var
  Start, Stop, m, n: QWord;
begin
  Start := GetTickCount64;
  while Start = GetTickCount64 do;
  m := RDTSC;
  Stop := Start + 1000;
  while GetTickCount64 < Stop do;
  n := RDTSC;
  CpuClockPeriod := 1000.0/(n - m);
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  CheckCpuSpeed;
end;

function TfrmMain.CreateNewGraph: TPointsChart;
type
  TSetSpec = specialize TGLiteHashSetLP<Integer, Integer>;
  TIntSet  = TSetSpec.TSet;
var
  XSet: TIntSet;
  GSize, s, d: SizeInt;
begin
  GSize := sedPointCount.Value;
  mmInfo.Append('Creating new graph...');
  Result := TPointsChart.Create;
  Result.EnsureCapacity(GSize);
  while Result.VertexCount < GSize do
    begin
      repeat s := Random(High(Integer)) until XSet.Add(s);
      repeat d := Random(High(Integer)) until s <> d;
      Result.AddVertex(TPoint.Create(s, d));
    end;
  XSet.Clear;
  repeat
    s := Random(GSize);
    repeat d := Random(GSize) until s <> d;
    Result.AddEdgeI(s, d);
  until (Result.EdgeCount >= GSize * 3);
  Result.EnsureConnected;
  mmInfo.Append(
    Format('A random graph with %d vertices and %d edges is ready.', [Result.VertexCount, Result.EdgeCount]));
end;

procedure TfrmMain.btFindClick(Sender: TObject);
var
  g: TPointsChart;
  pathDijk, pathAStar: TIntArray;
  src, trg, I: SizeInt;
  wDijk, wAStar: ValReal;
  strPath: string;
  Elapsed: QWord;
  T: Double;
begin
  src := sedSouce.Value;
  if (src < 0) or (src >= sedPointCount.Value) then
    begin
      mmInfo.Append(Format('Invalid source point index (%d)', [src]));
      exit;
    end;
  trg := sedTarget.Value;
  if (trg < 0) or (trg >= sedPointCount.Value) then
    begin
      mmInfo.Append(Format('Invalid target point index (%d)', [trg]));
      exit;
    end;

  if not FCurrGraph.HasInstance then
    FCurrGraph.Instance := CreateNewGraph;
  g := FCurrGraph;

  mmInfo.Append(
    Format('Gonna to find the shortest path from a point with index %d to a point with index %d.',
    [src, trg]));

  Elapsed := RDTSC;

  pathDijk := g.MinPathI(src, trg, wDijk);

  Elapsed := RDTSC - Elapsed;
  T := Elapsed * CpuClockPeriod;

  mmInfo.Append('Dijkstra'' pathfinder:');
  mmInfo.Append('completed in        ' + FloatToStr(Round(T*100000)/100000) + ' ms.');

  Elapsed := RDTSC;

  pathAStar := g.MinPathAStarI(src, trg, wAStar);

  Elapsed := RDTSC - Elapsed;
  T := Elapsed * CpuClockPeriod;

  mmInfo.Append('A* pathfinder:');
  mmInfo.Append('completed in        ' + FloatToStr(Round(T*100000)/100000) + ' ms.');
  mmInfo.Append('Dijkstra'' weight = ' + wDijk.ToString);

  strPath := '';
  for I in pathDijk do
    strPath :=  strPath + IntToStr(I) + '-';
  SetLength(strPath, Pred(Length(strPath)));
  mmInfo.Append('Dijkstra'' path =    ' + strPath);

  mmInfo.Append('AStar'' weight =    ' + wAStar.ToString);
  strPath := '';
  for I in pathAStar do
    strPath :=  strPath + IntToStr(I) + '-';
  SetLength(strPath, Pred(Length(strPath)));
  mmInfo.Append('AStar'' path =       ' + strPath);
end;

procedure TfrmMain.btNewClick(Sender: TObject);
begin
  FCurrGraph.Instance := CreateNewGraph;
end;

end.

