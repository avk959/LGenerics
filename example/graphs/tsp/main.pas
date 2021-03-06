unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Spin,
  lgSparseGraph,
  lgHashSet;

type

  TPoint2D    = specialize TGPoint2D<Integer>;
  TPointArray = array of TPoint2D;

  { TfrmMain }

  TfrmMain = class(TForm)
    btNew: TButton;
    btStartGreedy: TButton;
    btStartBnB: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbBnBCost: TLabel;
    lbGreedyCost: TLabel;
    pbGreedy: TPaintBox;
    pbBnB: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    seSize: TSpinEdit;
    seTtl: TSpinEdit;
    procedure btNewClick(Sender: TObject);
    procedure btStartBnBClick(Sender: TObject);
    procedure btStartGreedyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
  private
  type
    TSolver      = specialize TGMetricTspHelper<Integer>;
    THashSetSpec = specialize TGLiteHashSetLP<TPoint2D, TPoint2D>;
    THashSet     = THashSetSpec.TSet;

  const
    GAP = 5;

  var
    FPoints: TPointArray;
    FMatrix: TSolver.TTspMatrix;
    FGreedyTour,
    FBnBTour: TIntArray;
    FRange: Integer;
    FGreedyTourCost,
    FBnBTourCost: Integer;
    FScale: Double;
    procedure NewPoints;
    procedure RunGreedy;
    procedure RunBnB;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btNewClick(Sender: TObject);
begin
  NewPoints;
end;

procedure TfrmMain.btStartBnBClick(Sender: TObject);
begin
  RunBnB;
end;

procedure TfrmMain.btStartGreedyClick(Sender: TObject);
begin
  RunGreedy;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FRange := 10000;
  FScale := (pbGreedy.Width - GAP * 2) / FRange;
  Randomize;
end;

procedure TfrmMain.PaintBoxPaint(Sender: TObject);
var
  PaintBox: TPaintBox;
  Tour: TIntArray;
  I, X, Y, OldX, OldY: Integer;
  p: TPoint2D;
begin
  if not (Sender is TPaintBox) then
    exit;
  PaintBox := TPaintBox(Sender);
  if PaintBox = pbGreedy then
    Tour := FGreedyTour
  else
    Tour := FBnBTour;
  PaintBox.Canvas.Brush.Color := $00e9ffff;
  PaintBox.Canvas.FillRect(0, 0, PaintBox.Width, PaintBox.Height);
  if FPoints = nil then
    exit;
  PaintBox.Canvas.Pen.Width := 1;
  if Tour = nil then
    begin
      PaintBox.Canvas.Brush.Color := clBlue;
      PaintBox.Canvas.Pen.Color := clBlue;
      for I := 0 to High(FPoints) do
        begin
          X := Round(FPoints[I].X * FScale) + GAP;
          Y := Round(FPoints[I].Y * FScale) + GAP;
          PaintBox.Canvas.MoveTo(X, Y);
          PaintBox.Canvas.Rectangle(X-2, Y-2, X+1, Y+1);
        end
    end
  else
    begin
      p := FPoints[Tour[0]];
      X := Round(p.X * FScale) + GAP;
      Y := Round(p.Y * FScale) + GAP;
      PaintBox.Canvas.MoveTo(X, Y);
      PaintBox.Canvas.Brush.Color := clBlue;
      for I := 1 to High(Tour) do
        begin
          OldX := X;
          OldY := Y;
          p := FPoints[Tour[I]];
          X := Round(p.X * FScale) + GAP;
          Y := Round(p.Y * FScale) + GAP;
          PaintBox.Canvas.Pen.Color := clLime;
          PaintBox.Canvas.LineTo(X, Y);
          PaintBox.Canvas.Pen.Color := clBlue;
          PaintBox.Canvas.Rectangle(OldX-2, OldY-2, OldX+1, OldY+1);
        end;
      PaintBox.Canvas.Brush.Color := clRed;
      PaintBox.Canvas.Pen.Color := clRed;
      PaintBox.Canvas.Rectangle(X-2, Y-2, X+2, Y+2);
    end;
end;

procedure TfrmMain.NewPoints;
var
  HashSet: THashSet;
  I, J, Size: SizeInt;
  Dist: Integer;
begin
  FGreedyTour := nil;
  FBnBTour := nil;
  FPoints := nil;
  lbGreedyCost.Caption := '-';
  lbBnBCost.Caption := '-';
  Size := seSize.Value;
  SetLength(FPoints, Size);
  for I := 0 to High(FPoints) do
    repeat
      FPoints[I] := TPoint2D.Create(Random(FRange), Random(FRange));
    until HashSet.Add(FPoints[I]);
  SetLength(FMatrix, Size, Size);
  for I := 0 to Pred(Size) do
    for J := 0 to Pred(Size) do
      if I > J then
        begin
          Dist := Round(FPoints[I].Distance(FPoints[J]));
          FMatrix[I, J] := Dist;
          FMatrix[J, I] := Dist;
        end;
  for I := 0 to Pred(Size) do
    FMatrix[I, I] := 0;
  pbGreedy.Invalidate;
  pbBnB.Invalidate;
end;

procedure TfrmMain.RunGreedy;
begin
  if FPoints = nil then
    exit;
  FGreedyTour := nil;
  lbGreedyCost.Caption := 'running';
  Application.ProcessMessages;
  FGreedyTour := TSolver.FindGreedyFast(FMatrix, FGreedyTourCost);
  lbGreedyCost.Caption := IntToStr(FGreedyTourCost);
  pbGreedy.Invalidate;
end;

procedure TfrmMain.RunBnB;
begin
  if FPoints = nil then
    exit;
  FBnBTour := nil;
  lbBnBCost.Caption := 'running';
  Application.ProcessMessages;
  if TSolver.FindExact(FMatrix, FBnBTour, FBnBTourCost, seTtl.Value) then
    lbBnBCost.Caption := IntToStr(FBnBTourCost) + '(exact)'
  else
    lbBnBCost.Caption := IntToStr(FBnBTourCost) + '(approx)';
  pbBnB.Invalidate;
end;

end.

