unit LGTable2DTest;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGAbstractContainer,
  LGTable2D,
  LGArrayHelpers;

type

  THashTable2DTest = class(TTestCase)
  private
  type
    TTable     = specialize TGHashTable2D2<string, string, Integer>;
    TAutoTable = specialize TGAutoRef<TTable>;

  published
    procedure AddRow;
    procedure AddRows;
    procedure RemoveRow;
    procedure Clear;
    procedure EnsureRowCapacity;
    procedure TrimToFit;
    procedure AddCell;
    procedure RemoveCell;
    procedure RowMaps;
    procedure RemoveColumn;
    procedure Items;
  end;

  TTreeTable2DTest = class(TTestCase)
  private
  type
    TTable     = specialize TGTreeTable2D2<string, string, Integer>;
    TAutoTable = specialize TGAutoRef<TTable>;

  published
    procedure AddRow;
    procedure AddRows;
    procedure RemoveRow;
    procedure Clear;
    procedure EnsureRowCapacity;
    procedure TrimToFit;
    procedure AddCell;
    procedure RemoveCell;
    procedure RowMaps;
    procedure RemoveColumn;
    procedure Items;
  end;

  TListTable2DTest = class(TTestCase)
  private
  type
    TTable     = specialize TGListTable2D2<string, string, Integer>;
    TAutoTable = specialize TGAutoRef<TTable>;

  published
    procedure AddRow;
    procedure AddRows;
    procedure RemoveRow;
    procedure Clear;
    procedure EnsureRowCapacity;
    procedure TrimToFit;
    procedure AddCell;
    procedure RemoveCell;
    procedure RowMaps;
    procedure RemoveColumn;
    procedure Items;
  end;

implementation
{$B-}{$COPERATORS ON}{$WARNINGS OFF}

const
  RowArray:    array[1..5] of string = ('fish','bird','reptile','insect', 'plant');
  FishArray:   array[1..4] of string = ('pike', 'crucian', 'ruff', 'burbot');
  BirdArray:   array[1..4] of string = ('sparrow', 'swift', 'raven', 'stork');
  ReptArray:   array[1..4] of string = ('turtle', 'alligator', 'snake', 'lizard');
  InsectArray: array[1..4] of string = ('ant', 'fly', 'hornet', 'dragonfly');
  PlantArray:  array[1..4] of string = ('nettle', 'cedar', 'wheat', 'maple');

{ THashTable2DTest }

procedure THashTable2DTest.AddRow;
var
  tbl: TAutoTable;
  r: string;
begin
  AssertTrue(tbl.Instance.RowCount = 0);
  for r in RowArray do
    tbl.Instance.AddRow(r);
  AssertTrue(tbl.Instance.RowCount = 5);
  for r in RowArray do
    AssertTrue(tbl.Instance.ContainsRow(r));
end;

procedure THashTable2DTest.AddRows;
var
  tbl: TAutoTable;
  r: string;
begin
  AssertTrue(tbl.Instance.RowCount = 0);
  tbl.Instance.AddRows(RowArray);
  AssertTrue(tbl.Instance.RowCount = 5);
  for r in RowArray do
    AssertTrue(tbl.Instance.ContainsRow(r));
end;

procedure THashTable2DTest.RemoveRow;
var
  tbl: TAutoTable;
  r: string;
begin
  tbl.Instance.AddRows(RowArray);
  AssertTrue(tbl.Instance.RowCount = 5);
  for r in RowArray do
    AssertTrue(tbl.Instance.RemoveRow(r) = 0);
  AssertTrue(tbl.Instance.IsEmpty);
end;

procedure THashTable2DTest.Clear;
var
  tbl: TAutoTable;
  s: string;
begin
  AssertTrue(tbl.Instance.CellCount = 0);
  for s in RowArray do
    AssertTrue(tbl.Instance.RowMaps[s].Add('unknown', 1));
  AssertTrue(tbl.Instance.RowCount = 5);
  AssertTrue(tbl.Instance.CellCount = 5);
  tbl.Instance.Clear;
  AssertTrue(tbl.Instance.RowCount = 0);
  AssertTrue(tbl.Instance.CellCount = 0);
end;

procedure THashTable2DTest.EnsureRowCapacity;
var
  tbl: TAutoTable;
  c: SizeInt;
begin
  c := tbl.Instance.ExpandTreshold;
  tbl.Instance.EnsureRowCapacity(c + 1);
  AssertTrue(tbl.Instance.RowCapacity > c);
end;

procedure THashTable2DTest.TrimToFit;
var
  tbl: TAutoTable;
  c: SizeInt;
begin
  AssertTrue(tbl.Instance.AddRows(RowArray) = 5);
  c := tbl.Instance.RowCapacity;
  tbl.Instance.TrimToFit;
  AssertTrue(tbl.Instance.RowCapacity < c);
  AssertTrue(tbl.Instance.RowCapacity = 0);
end;

procedure THashTable2DTest.AddCell;
var
  tbl: TAutoTable;
  r, c: string;
begin
  AssertTrue(tbl.Instance.RowCount = 0);
  AssertTrue(tbl.Instance.CellCount = 0);
  AssertTrue(tbl.Instance.AddCell(RowArray[1], FishArray[1], 2));
  AssertTrue(tbl.Instance.AddCell(RowArray[1], FishArray[2], 1));
  AssertTrue(tbl.Instance.RowCount = 1);
  AssertTrue(tbl.Instance.CellCount = 2);
  tbl.Instance.Clear;
  tbl.Instance.AddRows(RowArray);
  AssertTrue(tbl.Instance.CellCount = 0);
  r := RowArray[1];
  for c in FishArray do
    AssertTrue(tbl.Instance.AddCell(r, c, 1));
  AssertTrue(tbl.Instance.ColCount[r] = 4);
  for c in FishArray do
    AssertTrue(tbl.Instance.ContainsCell(r, c));
  AssertTrue(tbl.Instance.CellCount = 4);
  r := RowArray[2];
  for c in BirdArray do
    AssertTrue(tbl.Instance.AddCell(r, c, 1));
  AssertTrue(tbl.Instance.ColCount[r] = 4);
  for c in BirdArray do
    AssertTrue(tbl.Instance.ContainsCell(r, c));
  AssertTrue(tbl.Instance.CellCount = 8);
  r := RowArray[3];
  for c in ReptArray do
    AssertTrue(tbl.Instance.AddCell(r, c, 1));
  AssertTrue(tbl.Instance.ColCount[r] = 4);
  for c in ReptArray do
    AssertTrue(tbl.Instance.ContainsCell(r, c));
  AssertTrue(tbl.Instance.CellCount = 12);
  r := RowArray[4];
  for c in InsectArray do
    AssertTrue(tbl.Instance.AddCell(r, c, 1));
  AssertTrue(tbl.Instance.ColCount[r] = 4);
  for c in InsectArray do
    AssertTrue(tbl.Instance.ContainsCell(r, c));
  AssertTrue(tbl.Instance.CellCount = 16);
  r := RowArray[5];
  for c in PlantArray do
    AssertTrue(tbl.Instance.AddCell(r, c, 1));
  AssertTrue(tbl.Instance.ColCount[r] = 4);
  for c in PlantArray do
    AssertTrue(tbl.Instance.ContainsCell(r, c));
  AssertTrue(tbl.Instance.CellCount = 20);
end;

procedure THashTable2DTest.RemoveCell;
var
  tbl: TAutoTable;
  r, c: string;
begin
  tbl.Instance.AddRows(RowArray);
  c := 'total';
  for r in tbl.Instance.Rows do
    AssertTrue(tbl.Instance.AddCell(r, c, 0));
  AssertTrue(tbl.Instance.CellCount = 5);
  for r in tbl.Instance.Rows do
    AssertTrue(tbl.Instance.RemoveCell(r, c));
  AssertTrue(tbl.Instance.CellCount = 0);
  tbl.Instance.TrimToFit;
  AssertTrue(tbl.Instance.IsEmpty);
end;

procedure THashTable2DTest.RowMaps;
var
  tbl: TAutoTable;
  r, c: string;
begin
  c := 'total';
  tbl.Instance.AddRows(RowArray);
  for r in tbl.Instance.Rows do
    AssertTrue(tbl.Instance.RowMaps[r].Add(c, 0));
  AssertTrue(tbl.Instance.CellCount = 5);
end;

procedure THashTable2DTest.RemoveColumn;
var
  tbl: TAutoTable;
  r, c: string;
begin
  tbl.Instance.AddRows(RowArray);
  AssertTrue(tbl.Instance.RemoveColumn('total') = 0);
  r := RowArray[1];
  for c in FishArray do
    tbl.Instance.AddCell(r, c, 1);
  tbl.Instance.AddCell(r, 'total', 4);

  r := RowArray[2];
  for c in BirdArray do
    tbl.Instance.AddCell(r, c, 1);
  tbl.Instance.AddCell(r, 'total', 4);

  r := RowArray[3];
  for c in ReptArray do
    tbl.Instance.AddCell(r, c, 1);
  tbl.Instance.AddCell(r, 'total', 4);

  r := RowArray[4];
  for c in InsectArray do
    tbl.Instance.AddCell(r, c, 1);
  tbl.Instance.AddCell(r, 'total', 4);

  r := RowArray[5];
  for c in PlantArray do
    tbl.Instance.AddCell(r, c, 1);
  tbl.Instance.AddCell(r, 'total', 4);

  AssertTrue(tbl.Instance.CellCount = 25);
  for r in tbl.Instance.Rows do
    begin
      AssertTrue(tbl.Instance.RowMaps[r].Count = 5);
      AssertTrue(tbl.Instance.RowMaps[r].Contains('total'));
    end;

  AssertTrue(tbl.Instance.RemoveColumn('total') = 5);

  AssertTrue(tbl.Instance.CellCount = 20);
  for r in tbl.Instance.Rows do
    begin
      AssertTrue(tbl.Instance.RowMaps[r].Count = 4);
      AssertFalse(tbl.Instance.RowMaps[r].Contains('total'));
    end;
end;

procedure THashTable2DTest.Items;
var
  tbl: TAutoTable;
  r, c: string;
begin
  tbl.Instance.AddRows(RowArray);
  r := RowArray[1];
  for c in FishArray do
    tbl.Instance[r, c] := 1;
  tbl.Instance[r, 'total'] := 4;
  AssertTrue(tbl.Instance.RowCount = 5);
  AssertTrue(tbl.Instance.CellCount = 5);
  AssertTrue(tbl.Instance.ColCount[r] = 5);
end;

{ TTreeTable2DTest }

procedure TTreeTable2DTest.AddRow;
var
  tbl: TAutoTable;
  r: string;
begin
  AssertTrue(tbl.Instance.RowCount = 0);
  for r in RowArray do
    tbl.Instance.AddRow(r);
  AssertTrue(tbl.Instance.RowCount = 5);
  for r in RowArray do
    AssertTrue(tbl.Instance.ContainsRow(r));
end;

procedure TTreeTable2DTest.AddRows;
var
  tbl: TAutoTable;
  r: string;
begin
  AssertTrue(tbl.Instance.RowCount = 0);
  tbl.Instance.AddRows(RowArray);
  AssertTrue(tbl.Instance.RowCount = 5);
  for r in RowArray do
    AssertTrue(tbl.Instance.ContainsRow(r));
end;

procedure TTreeTable2DTest.RemoveRow;
var
  tbl: TAutoTable;
  r: string;
begin
  tbl.Instance.AddRows(RowArray);
  AssertTrue(tbl.Instance.RowCount = 5);
  for r in RowArray do
    AssertTrue(tbl.Instance.RemoveRow(r) = 0);
  AssertTrue(tbl.Instance.IsEmpty);
end;

procedure TTreeTable2DTest.Clear;
var
  tbl: TAutoTable;
  s: string;
begin
  AssertTrue(tbl.Instance.CellCount = 0);
  for s in RowArray do
    AssertTrue(tbl.Instance.RowMaps[s].Add('unknown', 1));
  AssertTrue(tbl.Instance.RowCount = 5);
  AssertTrue(tbl.Instance.CellCount = 5);
  tbl.Instance.Clear;
  AssertTrue(tbl.Instance.RowCount = 0);
  AssertTrue(tbl.Instance.CellCount = 0);
end;

procedure TTreeTable2DTest.EnsureRowCapacity;
var
  tbl: TAutoTable;
  c: SizeInt;
begin
  c := tbl.Instance.ExpandTreshold;
  tbl.Instance.EnsureRowCapacity(c + 1);
  AssertTrue(tbl.Instance.RowCapacity > c);
end;

procedure TTreeTable2DTest.TrimToFit;
var
  tbl: TAutoTable;
  c: SizeInt;
begin
  AssertTrue(tbl.Instance.AddRows(RowArray) = 5);
  c := tbl.Instance.RowCapacity;
  tbl.Instance.TrimToFit;
  AssertTrue(tbl.Instance.RowCapacity < c);
  AssertTrue(tbl.Instance.RowCapacity = 0);
end;

procedure TTreeTable2DTest.AddCell;
var
  tbl: TAutoTable;
  r, c: string;
begin
  AssertTrue(tbl.Instance.RowCount = 0);
  AssertTrue(tbl.Instance.CellCount = 0);
  AssertTrue(tbl.Instance.AddCell(RowArray[1], FishArray[1], 2));
  AssertTrue(tbl.Instance.AddCell(RowArray[1], FishArray[2], 1));
  AssertTrue(tbl.Instance.RowCount = 1);
  AssertTrue(tbl.Instance.CellCount = 2);
  tbl.Instance.Clear;
  tbl.Instance.AddRows(RowArray);
  AssertTrue(tbl.Instance.CellCount = 0);
  r := RowArray[1];
  for c in FishArray do
    AssertTrue(tbl.Instance.AddCell(r, c, 1));
  AssertTrue(tbl.Instance.ColCount[r] = 4);
  for c in FishArray do
    AssertTrue(tbl.Instance.ContainsCell(r, c));
  AssertTrue(tbl.Instance.CellCount = 4);
  r := RowArray[2];
  for c in BirdArray do
    AssertTrue(tbl.Instance.AddCell(r, c, 1));
  AssertTrue(tbl.Instance.ColCount[r] = 4);
  for c in BirdArray do
    AssertTrue(tbl.Instance.ContainsCell(r, c));
  AssertTrue(tbl.Instance.CellCount = 8);
  r := RowArray[3];
  for c in ReptArray do
    AssertTrue(tbl.Instance.AddCell(r, c, 1));
  AssertTrue(tbl.Instance.ColCount[r] = 4);
  for c in ReptArray do
    AssertTrue(tbl.Instance.ContainsCell(r, c));
  AssertTrue(tbl.Instance.CellCount = 12);
  r := RowArray[4];
  for c in InsectArray do
    AssertTrue(tbl.Instance.AddCell(r, c, 1));
  AssertTrue(tbl.Instance.ColCount[r] = 4);
  for c in InsectArray do
    AssertTrue(tbl.Instance.ContainsCell(r, c));
  AssertTrue(tbl.Instance.CellCount = 16);
  r := RowArray[5];
  for c in PlantArray do
    AssertTrue(tbl.Instance.AddCell(r, c, 1));
  AssertTrue(tbl.Instance.ColCount[r] = 4);
  for c in PlantArray do
    AssertTrue(tbl.Instance.ContainsCell(r, c));
  AssertTrue(tbl.Instance.CellCount = 20);
end;

procedure TTreeTable2DTest.RemoveCell;
var
  tbl: TAutoTable;
  r, c: string;
begin
  tbl.Instance.AddRows(RowArray);
  c := 'total';
  for r in tbl.Instance.Rows do
    AssertTrue(tbl.Instance.AddCell(r, c, 0));
  AssertTrue(tbl.Instance.CellCount = 5);
  for r in tbl.Instance.Rows do
    AssertTrue(tbl.Instance.RemoveCell(r, c));
  AssertTrue(tbl.Instance.CellCount = 0);
  tbl.Instance.TrimToFit;
  AssertTrue(tbl.Instance.IsEmpty);
end;

procedure TTreeTable2DTest.RowMaps;
var
  tbl: TAutoTable;
  r, c: string;
begin
  c := 'total';
  tbl.Instance.AddRows(RowArray);
  for r in tbl.Instance.Rows do
    AssertTrue(tbl.Instance.RowMaps[r].Add(c, 0));
  AssertTrue(tbl.Instance.CellCount = 5);
end;

procedure TTreeTable2DTest.RemoveColumn;
var
  tbl: TAutoTable;
  r, c: string;
begin
  tbl.Instance.AddRows(RowArray);
  AssertTrue(tbl.Instance.RemoveColumn('total') = 0);
  r := RowArray[1];
  for c in FishArray do
    tbl.Instance.AddCell(r, c, 1);
  tbl.Instance.AddCell(r, 'total', 4);

  r := RowArray[2];
  for c in BirdArray do
    tbl.Instance.AddCell(r, c, 1);
  tbl.Instance.AddCell(r, 'total', 4);

  r := RowArray[3];
  for c in ReptArray do
    tbl.Instance.AddCell(r, c, 1);
  tbl.Instance.AddCell(r, 'total', 4);

  r := RowArray[4];
  for c in InsectArray do
    tbl.Instance.AddCell(r, c, 1);
  tbl.Instance.AddCell(r, 'total', 4);

  r := RowArray[5];
  for c in PlantArray do
    tbl.Instance.AddCell(r, c, 1);
  tbl.Instance.AddCell(r, 'total', 4);

  AssertTrue(tbl.Instance.CellCount = 25);
  for r in tbl.Instance.Rows do
    begin
      AssertTrue(tbl.Instance.RowMaps[r].Count = 5);
      AssertTrue(tbl.Instance.RowMaps[r].Contains('total'));
    end;

  AssertTrue(tbl.Instance.RemoveColumn('total') = 5);

  AssertTrue(tbl.Instance.CellCount = 20);
  for r in tbl.Instance.Rows do
    begin
      AssertTrue(tbl.Instance.RowMaps[r].Count = 4);
      AssertFalse(tbl.Instance.RowMaps[r].Contains('total'));
    end;
end;

procedure TTreeTable2DTest.Items;
var
  tbl: TAutoTable;
  r, c: string;
begin
  tbl.Instance.AddRows(RowArray);
  r := RowArray[1];
  for c in FishArray do
    tbl.Instance[r, c] := 1;
  tbl.Instance[r, 'total'] := 4;
  AssertTrue(tbl.Instance.RowCount = 5);
  AssertTrue(tbl.Instance.CellCount = 5);
  AssertTrue(tbl.Instance.ColCount[r] = 5);
end;

{ TListTable2DTest }

procedure TListTable2DTest.AddRow;
var
  tbl: TAutoTable;
  r: string;
begin
  AssertTrue(tbl.Instance.RowCount = 0);
  for r in RowArray do
    tbl.Instance.AddRow(r);
  AssertTrue(tbl.Instance.RowCount = 5);
  for r in RowArray do
    AssertTrue(tbl.Instance.ContainsRow(r));
end;

procedure TListTable2DTest.AddRows;
var
  tbl: TAutoTable;
  r: string;
begin
  AssertTrue(tbl.Instance.RowCount = 0);
  tbl.Instance.AddRows(RowArray);
  AssertTrue(tbl.Instance.RowCount = 5);
  for r in RowArray do
    AssertTrue(tbl.Instance.ContainsRow(r));
end;

procedure TListTable2DTest.RemoveRow;
var
  tbl: TAutoTable;
  r: string;
begin
  tbl.Instance.AddRows(RowArray);
  AssertTrue(tbl.Instance.RowCount = 5);
  for r in RowArray do
    AssertTrue(tbl.Instance.RemoveRow(r) = 0);
  AssertTrue(tbl.Instance.IsEmpty);
end;

procedure TListTable2DTest.Clear;
var
  tbl: TAutoTable;
  s: string;
begin
  AssertTrue(tbl.Instance.CellCount = 0);
  for s in RowArray do
    AssertTrue(tbl.Instance.RowMaps[s].Add('unknown', 1));
  AssertTrue(tbl.Instance.RowCount = 5);
  AssertTrue(tbl.Instance.CellCount = 5);
  tbl.Instance.Clear;
  AssertTrue(tbl.Instance.RowCount = 0);
  AssertTrue(tbl.Instance.CellCount = 0);
end;

procedure TListTable2DTest.EnsureRowCapacity;
var
  tbl: TAutoTable;
  c: SizeInt;
begin
  c := tbl.Instance.ExpandTreshold;
  tbl.Instance.EnsureRowCapacity(c + 1);
  AssertTrue(tbl.Instance.RowCapacity > c);
end;

procedure TListTable2DTest.TrimToFit;
var
  tbl: TAutoTable;
  c: SizeInt;
begin
  AssertTrue(tbl.Instance.AddRows(RowArray) = 5);
  c := tbl.Instance.RowCapacity;
  tbl.Instance.TrimToFit;
  AssertTrue(tbl.Instance.RowCapacity < c);
  AssertTrue(tbl.Instance.RowCapacity = 0);
end;

procedure TListTable2DTest.AddCell;
var
  tbl: TAutoTable;
  r, c: string;
begin
  AssertTrue(tbl.Instance.RowCount = 0);
  AssertTrue(tbl.Instance.CellCount = 0);
  AssertTrue(tbl.Instance.AddCell(RowArray[1], FishArray[1], 2));
  AssertTrue(tbl.Instance.AddCell(RowArray[1], FishArray[2], 1));
  AssertTrue(tbl.Instance.RowCount = 1);
  AssertTrue(tbl.Instance.CellCount = 2);
  tbl.Instance.Clear;
  tbl.Instance.AddRows(RowArray);
  AssertTrue(tbl.Instance.CellCount = 0);
  r := RowArray[1];
  for c in FishArray do
    AssertTrue(tbl.Instance.AddCell(r, c, 1));
  AssertTrue(tbl.Instance.ColCount[r] = 4);
  for c in FishArray do
    AssertTrue(tbl.Instance.ContainsCell(r, c));
  AssertTrue(tbl.Instance.CellCount = 4);
  r := RowArray[2];
  for c in BirdArray do
    AssertTrue(tbl.Instance.AddCell(r, c, 1));
  AssertTrue(tbl.Instance.ColCount[r] = 4);
  for c in BirdArray do
    AssertTrue(tbl.Instance.ContainsCell(r, c));
  AssertTrue(tbl.Instance.CellCount = 8);
  r := RowArray[3];
  for c in ReptArray do
    AssertTrue(tbl.Instance.AddCell(r, c, 1));
  AssertTrue(tbl.Instance.ColCount[r] = 4);
  for c in ReptArray do
    AssertTrue(tbl.Instance.ContainsCell(r, c));
  AssertTrue(tbl.Instance.CellCount = 12);
  r := RowArray[4];
  for c in InsectArray do
    AssertTrue(tbl.Instance.AddCell(r, c, 1));
  AssertTrue(tbl.Instance.ColCount[r] = 4);
  for c in InsectArray do
    AssertTrue(tbl.Instance.ContainsCell(r, c));
  AssertTrue(tbl.Instance.CellCount = 16);
  r := RowArray[5];
  for c in PlantArray do
    AssertTrue(tbl.Instance.AddCell(r, c, 1));
  AssertTrue(tbl.Instance.ColCount[r] = 4);
  for c in PlantArray do
    AssertTrue(tbl.Instance.ContainsCell(r, c));
  AssertTrue(tbl.Instance.CellCount = 20);
end;

procedure TListTable2DTest.RemoveCell;
var
  tbl: TAutoTable;
  r, c: string;
begin
  tbl.Instance.AddRows(RowArray);
  c := 'total';
  for r in tbl.Instance.Rows do
    AssertTrue(tbl.Instance.AddCell(r, c, 0));
  AssertTrue(tbl.Instance.CellCount = 5);
  for r in tbl.Instance.Rows do
    AssertTrue(tbl.Instance.RemoveCell(r, c));
  AssertTrue(tbl.Instance.CellCount = 0);
  tbl.Instance.TrimToFit;
  AssertTrue(tbl.Instance.IsEmpty);
end;

procedure TListTable2DTest.RowMaps;
var
  tbl: TAutoTable;
  r, c: string;
begin
  c := 'total';
  tbl.Instance.AddRows(RowArray);
  for r in tbl.Instance.Rows do
    AssertTrue(tbl.Instance.RowMaps[r].Add(c, 0));
  AssertTrue(tbl.Instance.CellCount = 5);
end;

procedure TListTable2DTest.RemoveColumn;
var
  tbl: TAutoTable;
  r, c: string;
begin
  tbl.Instance.AddRows(RowArray);
  AssertTrue(tbl.Instance.RemoveColumn('total') = 0);
  r := RowArray[1];
  for c in FishArray do
    tbl.Instance.AddCell(r, c, 1);
  tbl.Instance.AddCell(r, 'total', 4);

  r := RowArray[2];
  for c in BirdArray do
    tbl.Instance.AddCell(r, c, 1);
  tbl.Instance.AddCell(r, 'total', 4);

  r := RowArray[3];
  for c in ReptArray do
    tbl.Instance.AddCell(r, c, 1);
  tbl.Instance.AddCell(r, 'total', 4);

  r := RowArray[4];
  for c in InsectArray do
    tbl.Instance.AddCell(r, c, 1);
  tbl.Instance.AddCell(r, 'total', 4);

  r := RowArray[5];
  for c in PlantArray do
    tbl.Instance.AddCell(r, c, 1);
  tbl.Instance.AddCell(r, 'total', 4);

  AssertTrue(tbl.Instance.CellCount = 25);
  for r in tbl.Instance.Rows do
    begin
      AssertTrue(tbl.Instance.RowMaps[r].Count = 5);
      AssertTrue(tbl.Instance.RowMaps[r].Contains('total'));
    end;

  AssertTrue(tbl.Instance.RemoveColumn('total') = 5);

  AssertTrue(tbl.Instance.CellCount = 20);
  for r in tbl.Instance.Rows do
    begin
      AssertTrue(tbl.Instance.RowMaps[r].Count = 4);
      AssertFalse(tbl.Instance.RowMaps[r].Contains('total'));
    end;
end;

procedure TListTable2DTest.Items;
var
  tbl: TAutoTable;
  r, c: string;
begin
  tbl.Instance.AddRows(RowArray);
  r := RowArray[1];
  for c in FishArray do
    tbl.Instance[r, c] := 1;
  tbl.Instance[r, 'total'] := 4;
  AssertTrue(tbl.Instance.RowCount = 5);
  AssertTrue(tbl.Instance.CellCount = 5);
  AssertTrue(tbl.Instance.ColCount[r] = 5);
end;

initialization
  RegisterTest(THashTable2DTest);
  RegisterTest(TTreeTable2DTest);
  RegisterTest(TListTable2DTest);
end.

