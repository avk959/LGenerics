{****************************************************************************
*         _     ____ _____ _   _ _____ ____  ___ ____ ____                  *
*        | |   / ___| ____| \ | | ____|  _ \|_ _/ ___/ ___|                 *
*        | |  | |  _|  _| |  \| |  _| | |_) || | |   \___ \                 *
*        | |__| |_| | |___| |\  | |___|  _ < | | |___ ___) |                *
*        |_____\____|_____|_| \_|_____|_| \_\___\____|____/                 *
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic sparse table implementations.                                   *
*                                                                           *
*   Copyright(c) 2018 A.Koverdyaev(avk)                                     *
*                                                                           *
*   This code is free software; you can redistribute it and/or modify it    *
*   under the terms of the Apache License, Version 2.0;                     *
*   You may obtain a copy of the License at                                 *
*     http://www.apache.org/licenses/LICENSE-2.0.                           *
*                                                                           *
*  Unless required by applicable law or agreed to in writing, software      *
*  distributed under the License is distributed on an "AS IS" BASIS,        *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *
*  See the License for the specific language governing permissions and      *
*  limitations under the License.                                           *
*                   _         __     ___           __                       *
*            /\  /\/_\/\   /\/__\   / __\/\ /\  /\ \ \                      *
*           / /_/ //_\\ \ / /_\    / _\ / / \ \/  \/ /                      *
*          / __  /  _  \ V //__   / /   \ \_/ / /\  /                       *
*          \/ /_/\_/ \_/\_/\__/   \/     \___/\_\ \/                        *
*****************************************************************************}
unit LGSparseTable;

{$mode objfpc}{$H+}
{$INLINE ON}

interface

uses
  SysUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGCustomContainer,
  LGHashTable,
  LGAvlTree,
  LGSortedList;

type

  { TGCustomHashSparseTable: implements rows as as linear probing hashmap }
  generic TGCustomHashSparseTable<TRow, TCol, TValue, TRowEqRel> = class abstract(
      specialize TGCustomSparseTable<TRow, TCol, TValue>)
  protected
  type
    THashTable = specialize TGHashTableLP<TRow, TTableEntry, TRowEqRel>;

    TColEnumerable = class(TCustomColEntryEnumerable)
    protected
      FEnum: THashTable.TEnumerator;
      FCurrValue: TValue;
      FCol: TCol;
      function GetCurrent: TColEntry; override;
    public
      constructor Create(aTable: TGCustomHashSparseTable; constref ACol: TCol);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TCellEnumerable = class(TCustomCellEntryEnumerable)
    protected
      FEnum: THashTable.TEnumerator;
      FRowEnum: TRowEntryEnumerator;
      FCurrRowEntry: TRowEntry;
      function GetCurrent: TCellEntry; override;
    public
      constructor Create(aTable: TGCustomHashSparseTable);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TRowEnumerable = class(specialize TGAutoEnumerable<TRow>)
    protected
      FEnum: THashTable.TEnumerator;
      function GetCurrent: TRow; override;
    public
      constructor Create(aTable: TGCustomHashSparseTable);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TRowMapEnumerable = class(specialize TGAutoEnumerable<IRowMap>)
    protected
      FEnum: THashTable.TEnumerator;
      function GetCurrent: IRowMap; override;
    public
      constructor Create(aTable: TGCustomHashSparseTable);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  var
    FTable: THashTable;
    function  CreateRowMap: TCustomRowMap; virtual; abstract;
    function  GetFillRatio: Single; inline;
    function  GetLoadFactor: Single; inline;
    procedure SetLoadFactor(aValue: Single); inline;
    procedure ClearItems;
    function  GetRowCount: SizeInt; override;
    function  DoFindRow(constref aRow: TRow): PTableEntry; override;
  { returns True if row found, False otherwise }
    function  DoFindOrAddRow(constref aRow: TRow; out p: PTableEntry): Boolean; override;
    function  DoRemoveRow(constref aRow: TRow): SizeInt; override;
    function  GetColumn(const aCol: TCol): IColEntryEnumerable; override;
    function  GetCellEntries: ICellEntryEnumerable; override;
  public
    constructor Create;
    constructor Create(aRowCapacity: SizeInt);
    constructor Create(aLoadFactor: Single);
    constructor Create(aRowCapacity: SizeInt; aLoadFactor: Single);
    procedure Clear; override;
    procedure EnsureRowCapacity(aValue: SizeInt); override;
    function  RowEnum: IRowEnumerable; override;
    function  RowMapEnum: IRowMapEnumerable; override;
    property  LoadFactor: Single read GetLoadFactor write SetLoadFactor;
    property  FillRatio: Single read GetFillRatio;
  end;

  { TGHashSparseTable implements spares table with row map as linear probing hashmap;

      functor TRowEqRel(row equality relation) must provide:
        class function HashCode([const[ref]] r: TRow): SizeInt;
        class function Equal([const[ref]] L, R: TRow): Boolean;

      functor TColEqRel(column equality relation) must provide:
        class function HashCode([const[ref]] c: TCol): SizeInt;
        class function Equal([const[ref]] L, R: TCol): Boolean; }
  generic TGHashSparseTable<TRow, TCol, TValue, TRowEqRel, TColEqRel> = class(
    specialize TGCustomHashSparseTable<TRow, TCol, TValue, TRowEqRel>)
  protected
  type
    TRowMap = class(TCustomRowMap)
    private
    type
      TEntry = record
        Key: TCol;
        Value:  TValue;
      end;
      PEntry = ^TEntry;

      TRowMapTable = specialize TGHashTableLP<TCol, TEntry, TColEqRel>;

      TEnumerator = class(TRowEntryEnumerator)
      protected
        FEnum: TRowMapTable.TEnumerator;
        function GetCurrent: TRowEntry; override;
      public
        constructor Create(aMap: TRowMapTable);
        function  MoveNext: Boolean; override;
        procedure Reset; override;
      end;

    const
      INITIAL_CAPACITY = 8;
      LOAD_FACTOR    = 0.65; //todo: why ???

    var
      FTable: TCustomSparseTable;
      FMap: TRowMapTable;
    protected
      function  GetCount: SizeInt; override;
    public
      constructor Create(aTable: TCustomSparseTable);
      destructor Destroy; override;
      function  GetEnumerator: TRowEntryEnumerator; override;
      procedure TrimToFit; inline;
      function  Contains(constref aCol: TCol): Boolean; override;
      function  TryGetValue(constref aCol: TCol; out aValue: TValue): Boolean; override;
    { returns True if not contains aCol was added, False otherwise }
      function  Add(constref aCol: TCol; constref aValue: TValue): Boolean; override;
      procedure AddOrSetValue(const aCol: TCol; const aValue: TValue); override;
      function  Remove(constref aCol: TCol): Boolean; override;
    end;

    function  CreateRowMap: TCustomRowMap; override;
  public
    destructor Destroy; override;
    procedure TrimToFit; override;
  end;

  { TGHashSparseTableR assumes that TRow implements TRowEqRel }
  generic TGHashSparseTableR<TRow, TCol, TValue, TColEqRel> = class(
    specialize TGHashSparseTable<TRow, TCol, TValue, TRow, TColEqRel>);

  { TGHashSparseTableC assumes that TCol implements TColEqRel }
  generic TGHashSparseTableC<TRow, TCol, TValue, TRowEqRel> = class(
    specialize TGHashSparseTable<TRow, TCol, TValue, TRowEqRel, TCol>);

  { TGHashSparseTable2 assumes that TRow implements TRowEqRel and TCol implements TColEqRel }
  generic TGHashSparseTable2<TRow, TCol, TValue> = class(
    specialize TGHashSparseTable<TRow, TCol, TValue, TRow, TCol>);

  { TGTreeSparseTable implements spares table with row map as avl_tree map;

      functor TRowEqRel(row equality relation) must provide:
        class function HashCode([const[ref]] r: TRow): SizeInt;
        class function Equal([const[ref]] L, R: TRow): Boolean;

      functor TColCmpRel(column equality relation) must provide:
        class function Compare([const[ref]] L, R: TCol): SizeInt; }
  generic TGTreeSparseTable<TRow, TCol, TValue, TRowEqRel, TColCmpRel> = class(
    specialize TGCustomHashSparseTable<TRow, TCol, TValue, TRowEqRel>)
  protected
  type
    TEntry = record
      Key: TCol;
      Value:  TValue;
    end;
    PEntry = ^TEntry;

    TNode        = specialize TGAvlTreeNode<TEntry>;
    PNode        = ^TNode;
    TNodeManager = specialize TGPageNodeManager<TNode>;
    PNodeManager = ^TNodeManager;

    { TRowMap }

    TRowMap = class(TCustomRowMap)
    private
    type
      TRowMapTable = specialize TGAvlTree2<TCol, TEntry, TNodeManager, TColCmpRel>;

      TEnumerator = class(TRowEntryEnumerator)
      protected
        FEnum: TRowMapTable.TEnumerator;
        function GetCurrent: TRowEntry; override;
      public
        constructor Create(aMap: TRowMapTable);
        function  MoveNext: Boolean; override;
        procedure Reset; override;
      end;

    var
      FTable: TCustomSparseTable;
      FMap: TRowMapTable;
    protected
      function  GetCount: SizeInt; override;
    public
      constructor Create(aTable: TGTreeSparseTable);
      destructor Destroy; override;
      function  GetEnumerator: TRowEntryEnumerator; override;
      function  Contains(constref aCol: TCol): Boolean; override;
      function  TryGetValue(constref aCol: TCol; out aValue: TValue): Boolean; override;
    { returns True if not contains aCol was added, False otherwise }
      function  Add(constref aCol: TCol; constref aValue: TValue): Boolean; override;
      procedure AddOrSetValue(const aCol: TCol; const aValue: TValue); override;
      function  Remove(constref aCol: TCol): Boolean; override;
    end;

  var
    FNodeManager: TNodeManager;
    function  CreateRowMap: TCustomRowMap; override;
  public
    constructor Create;
    constructor Create(aRowCapacity: SizeInt);
    constructor Create(aLoadFactor: Single);
    constructor Create(aRowCapacity: SizeInt; aLoadFactor: Single);
    destructor Destroy; override;
    procedure Clear; override;
    procedure TrimToFit; override;
  end;

  { TGTreeSparseTableR assumes that TRow implements TRowEqRel }
  generic TGTreeSparseTableR<TRow, TCol, TValue, TColCmpRel> = class(
    specialize TGTreeSparseTable<TRow, TCol, TValue, TRow, TColCmpRel>);

  { TGTreeSparseTableC assumes that TCol implements TColCmpRel }
  generic TGTreeSparseTableC<TRow, TCol, TValue, TRowEqRel> = class(
    specialize TGTreeSparseTable<TRow, TCol, TValue, TRowEqRel, TCol>);

  { TGTreeSparseTable2 assumes that TRow implements TRowEqRel and TCol implements TColCmpRel }
  generic TGTreeSparseTable2<TRow, TCol, TValue> = class(
    specialize TGTreeSparseTable<TRow, TCol, TValue, TRow, TCol>);


  { TGListSparseTable implements spares table with row map as listmap ;

      functor TRowEqRel(row equality relation) must provide:
        class function HashCode([const[ref]] r: TRow): SizeInt;
        class function Equal([const[ref]] L, R: TRow): Boolean;

      functor TColCmpRel(column equality relation) must provide:
        class function Compare([const[ref]] L, R: TCol): SizeInt; }
  generic TGListSparseTable<TRow, TCol, TValue, TRowEqRel, TColCmpRel> = class(
    specialize TGCustomHashSparseTable<TRow, TCol, TValue, TRowEqRel>)
  protected
  type
    TEntry = record
      Key: TCol;
      Value:  TValue;
    end;
    PEntry = ^TEntry;

    TRowMap = class(TCustomRowMap)
    private
    type
      TRowMapTable = specialize TGSortedListTable<TCol, TEntry, TColCmpRel>;

      TEnumerator = class(TRowEntryEnumerator)
      protected
        FEnum: TRowMapTable.TEnumerator;
        function GetCurrent: TRowEntry; override;
      public
        constructor Create(aMap: TRowMapTable);
        function  MoveNext: Boolean; override;
        procedure Reset; override;
      end;

    const
      INITIAL_CAPACITY = 8;

    var
      FTable: TCustomSparseTable;
      FMap: TRowMapTable;
    protected
      function  GetCount: SizeInt; override;
    public
      constructor Create(aTable: TCustomSparseTable);
      destructor Destroy; override;
      function  GetEnumerator: TRowEntryEnumerator; override;
      procedure TrimToFit; inline;
      function  Contains(constref aCol: TCol): Boolean; override;
      function  TryGetValue(constref aCol: TCol; out aValue: TValue): Boolean; override;
    { returns True if not contains aCol was added, False otherwise }
      function  Add(constref aCol: TCol; constref aValue: TValue): Boolean; override;
      procedure AddOrSetValue(const aCol: TCol; const aValue: TValue); override;
      function  Remove(constref aCol: TCol): Boolean; override;
    end;

   function  CreateRowMap: TCustomRowMap; override;
  public
    destructor Destroy; override;
    procedure TrimToFit; override;
  end;

  { TGListSparseTableR assumes that TRow implements TRowEqRel }
  generic TGListSparseTableR<TRow, TCol, TValue, TColCmpRel> = class(
    specialize TGListSparseTable<TRow, TCol, TValue, TRow, TColCmpRel>);

  { TGListSparseTableC assumes that TCol implements TColCmpRel }
  generic TGListSparseTableC<TRow, TCol, TValue, TRowEqRel> = class(
    specialize TGListSparseTable<TRow, TCol, TValue, TRowEqRel, TCol>);

  { TGListSparseTable2 assumes that TRow implements TRowEqRel and TCol implements TColCmpRel }
  generic TGListSparseTable2<TRow, TCol, TValue> = class(
    specialize TGListSparseTable<TRow, TCol, TValue, TRow, TCol>);

implementation
{$B-}{$COPERATORS ON}

{ TGCustomHashSparseTable.TColEnumerable }

function TGCustomHashSparseTable.TColEnumerable.GetCurrent: TColEntry;
begin
  Result.Row := FEnum.Current^.Key;
  Result.Value := FCurrValue;
end;

constructor TGCustomHashSparseTable.TColEnumerable.Create(aTable: TGCustomHashSparseTable; constref ACol: TCol);
begin
  inherited Create;
  FEnum := aTable.FTable.GetEnumerator;
  FCol := aCol;
  FCurrValue := Default(TValue);
end;

function TGCustomHashSparseTable.TColEnumerable.MoveNext: Boolean;
begin
  repeat
    if not FEnum.MoveNext then
      exit(False);
    Result := FEnum.Current^.Columns.TryGetValue(FCol, FCurrValue);
  until Result;
end;

procedure TGCustomHashSparseTable.TColEnumerable.Reset;
begin
  FEnum.Reset;
  FCurrValue := Default(TValue);
end;

{ TGCustomHashSparseTable.TCellEnumerable }

function TGCustomHashSparseTable.TCellEnumerable.GetCurrent: TCellEntry;
begin
  Result.Row := FEnum.Current^.Key;
  Result.Column := FCurrRowEntry.Column;
  Result.Value := FCurrRowEntry.Value;
end;

constructor TGCustomHashSparseTable.TCellEnumerable.Create(aTable: TGCustomHashSparseTable);
begin
  inherited Create;
  FEnum :=  aTable.FTable.GetEnumerator;
  FCurrRowEntry := Default(TRowEntry);
end;

destructor TGCustomHashSparseTable.TCellEnumerable.Destroy;
begin
  FRowEnum.Free;
  inherited;
end;

function TGCustomHashSparseTable.TCellEnumerable.MoveNext: Boolean;
begin
  repeat
    if not Assigned(FRowEnum) then
      begin
        if not FEnum.MoveNext then
          exit(False);
        FRowEnum := FEnum.Current^.Columns.GetEnumerator;
      end;
    Result := FRowEnum.MoveNext;
    if not Result then
      FreeAndNil(FRowEnum)
    else
      FCurrRowEntry := FRowEnum.Current;
  until Result;
end;

procedure TGCustomHashSparseTable.TCellEnumerable.Reset;
begin
  FEnum.Reset;
  FreeAndNil(FRowEnum);
  FCurrRowEntry := Default(TRowEntry);
end;

{ TGCustomHashSparseTable.TRowEnumerable }

function TGCustomHashSparseTable.TRowEnumerable.GetCurrent: TRow;
begin
  Result := FEnum.Current^.Key;
end;

constructor TGCustomHashSparseTable.TRowEnumerable.Create(aTable: TGCustomHashSparseTable);
begin
  inherited Create;
  FEnum := aTable.FTable.GetEnumerator;
end;

function TGCustomHashSparseTable.TRowEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomHashSparseTable.TRowEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomHashSparseTable.TRowMapEnumerable }

function TGCustomHashSparseTable.TRowMapEnumerable.GetCurrent: IRowMap;
begin
  Result := FEnum.Current^.Columns;
end;

constructor TGCustomHashSparseTable.TRowMapEnumerable.Create(aTable: TGCustomHashSparseTable);
begin
  inherited Create;
  FEnum := aTable.FTable.GetEnumerator;
end;

function TGCustomHashSparseTable.TRowMapEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomHashSparseTable.TRowMapEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomHashSparseTable }

function TGCustomHashSparseTable.GetFillRatio: Single;
begin
  Result := FTable.FillRatio;
end;

function TGCustomHashSparseTable.GetLoadFactor: Single;
begin
  Result := FTable.LoadFactor;
end;

procedure TGCustomHashSparseTable.SetLoadFactor(aValue: Single);
begin
  FTable.LoadFactor := aValue;
end;

procedure TGCustomHashSparseTable.ClearItems;
var
  p: THashTable.PEntry;
begin
  for p in FTable do
    p^.Columns.Free;
end;

function TGCustomHashSparseTable.GetRowCount: SizeInt;
begin
  Result := FTable.Count;
end;

function TGCustomHashSparseTable.DoFindRow(constref aRow: TRow): PTableEntry;
var
  Pos: SizeInt;
begin
  Result := FTable.Find(aRow, Pos);
end;

function TGCustomHashSparseTable.DoFindOrAddRow(constref aRow: TRow; out p: PTableEntry): Boolean;
var
  Pos: SizeInt;
begin
  Result := FTable.FindOrAdd(aRow, p, Pos);
  if not Result then
    begin
      p^.Key := aRow;
      p^.Columns := CreateRowMap;
    end;
end;

function TGCustomHashSparseTable.DoRemoveRow(constref aRow: TRow): SizeInt;
var
  Pos: SizeInt;
  p: PTableEntry;
begin
  p := FTable.Find(aRow, Pos);
  if p <> nil then
    begin
      Result := p^.Columns.Count;
      p^.Columns.Free;
      FTable.RemoveAt(Pos);
    end
  else
    Result := 0;
end;

function TGCustomHashSparseTable.GetColumn(const aCol: TCol): IColEntryEnumerable;
begin
  Result := TColEnumerable.Create(Self, aCol);
end;

function TGCustomHashSparseTable.GetCellEntries: ICellEntryEnumerable;
begin
  Result := TCellEnumerable.Create(Self);
end;

constructor TGCustomHashSparseTable.Create;
begin
  FTable := THashTable.Create;
end;

constructor TGCustomHashSparseTable.Create(aRowCapacity: SizeInt);
begin
  FTable := THashTable.Create(aRowCapacity);
end;

constructor TGCustomHashSparseTable.Create(aLoadFactor: Single);
begin
  FTable := THashTable.Create(aLoadFactor);
end;

constructor TGCustomHashSparseTable.Create(aRowCapacity: SizeInt; aLoadFactor: Single);
begin
  FTable := THashTable.Create(aRowCapacity, aLoadFactor);
end;

procedure TGCustomHashSparseTable.Clear;
begin
  ClearItems;
  FTable.Clear;
end;

procedure TGCustomHashSparseTable.EnsureRowCapacity(aValue: SizeInt);
begin
  FTable.EnsureCapacity(aValue);
end;

function TGCustomHashSparseTable.RowEnum: IRowEnumerable;
begin
  Result := TRowEnumerable.Create(Self);
end;

function TGCustomHashSparseTable.RowMapEnum: IRowMapEnumerable;
begin
  Result := TRowMapEnumerable.Create(Self);
end;

{ TGHashSparseTable.TRowMap.TEnumerator }

function TGHashSparseTable.TRowMap.TEnumerator.GetCurrent: TRowEntry;
begin
  Result.Column := FEnum.Current^.Key;
  Result.Value := FEnum.Current^.Value;
end;

constructor TGHashSparseTable.TRowMap.TEnumerator.Create(aMap: TRowMapTable);
begin
  FEnum := aMap.GetEnumerator;
end;

function TGHashSparseTable.TRowMap.TEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGHashSparseTable.TRowMap.TEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGHashSparseTable.TRowMap }

function TGHashSparseTable.TRowMap.GetCount: SizeInt;
begin
  Result := FMap.Count;
end;

constructor TGHashSparseTable.TRowMap.Create(aTable: TCustomSparseTable);
begin
  FMap := TRowMapTable.Create(INITIAL_CAPACITY, LOAD_FACTOR);
  FTable := aTable;
end;

destructor TGHashSparseTable.TRowMap.Destroy;
begin
  FTable.FCellCount -= FMap.Count;
  FMap.Free;
  inherited;
end;

function TGHashSparseTable.TRowMap.GetEnumerator: TRowEntryEnumerator;
begin
  Result := TEnumerator.Create(Self.FMap);
end;

procedure TGHashSparseTable.TRowMap.TrimToFit;
begin
  FMap.TrimToFit;
end;

function TGHashSparseTable.TRowMap.Contains(constref aCol: TCol): Boolean;
var
  p: SizeInt;
begin
  Result := FMap.Find(aCol, p) <> nil;
end;

function TGHashSparseTable.TRowMap.TryGetValue(constref aCol: TCol; out aValue: TValue): Boolean;
var
  Pos: SizeInt;
  p: PEntry;
begin
  p := FMap.Find(aCol, Pos);
  Result := p <> nil;
  if Result then
    aValue := p^.Value;
end;

function TGHashSparseTable.TRowMap.Add(constref aCol: TCol; constref aValue: TValue): Boolean;
var
  Pos: SizeInt;
  p: PEntry;
begin
  Result := not FMap.FindOrAdd(aCol, p, Pos);
  if Result then
    begin
      p^.Key := aCol;
      p^.Value := aValue;
      Inc(FTable.FCellCount);
    end;
end;

procedure TGHashSparseTable.TRowMap.AddOrSetValue(const aCol: TCol; const aValue: TValue);
var
  Pos: SizeInt;
  p: PEntry;
begin
  if FMap.FindOrAdd(aCol, p, Pos) then
    p^.Value := aValue
  else
    begin
      p^.Key := aCol;
      p^.Value := aValue;
      Inc(FTable.FCellCount);
    end;
end;

function TGHashSparseTable.TRowMap.Remove(constref aCol: TCol): Boolean;
begin
  Result := FMap.Remove(aCol);
  FTable.FCellCount -= Ord(Result);
end;

{ TGHashSparseTable }

function TGHashSparseTable.CreateRowMap: TCustomRowMap;
begin
  Result := TRowMap.Create(Self);
end;

destructor TGHashSparseTable.Destroy;
begin
  Clear;
  FTable.Free;
  inherited;
end;

procedure TGHashSparseTable.TrimToFit;
var
  p: PTableEntry;
begin
  FTable.TrimToFit;
  for p in FTable do
    TRowMap(p^.Columns).TrimToFit;
end;

{ TGTreeSparseTable.TRowMap.TEnumerator }

function TGTreeSparseTable.TRowMap.TEnumerator.GetCurrent: TRowEntry;
begin
  Result.Column := FEnum.Current^.Key;
  Result.Value := FEnum.Current^.Value;
end;

constructor TGTreeSparseTable.TRowMap.TEnumerator.Create(aMap: TRowMapTable);
begin
  FEnum := aMap.GetEnumerator;
end;

function TGTreeSparseTable.TRowMap.TEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGTreeSparseTable.TRowMap.TEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGTreeSparseTable.TRowMap }

function TGTreeSparseTable.TRowMap.GetCount: SizeInt;
begin
  Result := FMap.Count;
end;

constructor TGTreeSparseTable.TRowMap.Create(aTable: TGTreeSparseTable);
begin
  FMap := TRowMapTable.Create(aTable.FNodeManager);
  FTable := aTable;
end;

destructor TGTreeSparseTable.TRowMap.Destroy;
begin
  FTable.FCellCount -= FMap.Count;
  FMap.Free;
  inherited;
end;

function TGTreeSparseTable.TRowMap.GetEnumerator: TRowEntryEnumerator;
begin
  Result := TEnumerator.Create(Self.FMap);
end;

function TGTreeSparseTable.TRowMap.Contains(constref aCol: TCol): Boolean;
begin
  Result := FMap.Find(aCol) <> nil;
end;

function TGTreeSparseTable.TRowMap.TryGetValue(constref aCol: TCol; out aValue: TValue): Boolean;
var
  Pos: SizeInt;
  p: PNode;
begin
  p := FMap.Find(aCol);
  Result := p <> nil;
  if Result then
    aValue := p^.Data.Value;
end;

function TGTreeSparseTable.TRowMap.Add(constref aCol: TCol; constref aValue: TValue): Boolean;
var
  Pos: SizeInt;
  p: PNode;
begin
  Result := not FMap.FindOrAdd(aCol, p);
  if Result then
    begin
      p^.Data.Value := aValue;
      Inc(FTable.FCellCount);
    end;
end;

procedure TGTreeSparseTable.TRowMap.AddOrSetValue(const aCol: TCol; const aValue: TValue);
var
  Pos: SizeInt;
  p: PNode;
begin
  if not FMap.FindOrAdd(aCol, p) then
    Inc(FTable.FCellCount);
  p^.Data.Value := aValue;
end;

function TGTreeSparseTable.TRowMap.Remove(constref aCol: TCol): Boolean;
begin
  Result := FMap.Remove(aCol);
  FTable.FCellCount -= Ord(Result);
end;

{ TGTreeSparseTable }

function TGTreeSparseTable.CreateRowMap: TCustomRowMap;
begin
  Result := TRowMap.Create(Self);
end;

constructor TGTreeSparseTable.Create;
begin
  inherited Create;
  FNodeManager := TNodeManager.Create;
end;

constructor TGTreeSparseTable.Create(aRowCapacity: SizeInt);
begin
  inherited Create(aRowCapacity);
  FNodeManager := TNodeManager.Create;
end;

constructor TGTreeSparseTable.Create(aLoadFactor: Single);
begin
  inherited Create(aLoadFactor);
  FNodeManager := TNodeManager.Create;
end;

constructor TGTreeSparseTable.Create(aRowCapacity: SizeInt; aLoadFactor: Single);
begin
  inherited Create(aRowCapacity, aLoadFactor);
  FNodeManager := TNodeManager.Create;
end;

destructor TGTreeSparseTable.Destroy;
begin
  Clear;
  FTable.Free;
  FNodeManager.Free;
  inherited;
end;

procedure TGTreeSparseTable.Clear;
begin
  inherited;
  FNodeManager.Clear;
end;

procedure TGTreeSparseTable.TrimToFit;
begin
  FTable.TrimToFit;
  if CellCount = 0 then
    FNodeManager.Clear;
end;

{ TGListSparseTable.TRowMap.TEnumerator }

function TGListSparseTable.TRowMap.TEnumerator.GetCurrent: TRowEntry;
begin
  Result.Column := FEnum.Current^.Key;
  Result.Value := FEnum.Current^.Value;
end;

constructor TGListSparseTable.TRowMap.TEnumerator.Create(aMap: TRowMapTable);
begin
  FEnum := aMap.GetEnumerator;
end;

function TGListSparseTable.TRowMap.TEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGListSparseTable.TRowMap.TEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGListSparseTable.TRowMap }

function TGListSparseTable.TRowMap.GetCount: SizeInt;
begin
  Result := FMap.Count;
end;

constructor TGListSparseTable.TRowMap.Create(aTable: TCustomSparseTable);
begin
  FMap := TRowMapTable.Create(INITIAL_CAPACITY);
  FTable := aTable;
end;

destructor TGListSparseTable.TRowMap.Destroy;
begin
  FTable.FCellCount -= FMap.Count;
  FMap.Free;
  inherited;
end;

function TGListSparseTable.TRowMap.GetEnumerator: TRowEntryEnumerator;
begin
  Result := TEnumerator.Create(Self.FMap);
end;

procedure TGListSparseTable.TRowMap.TrimToFit;
begin
  FMap.TrimToFit;
end;

function TGListSparseTable.TRowMap.Contains(constref aCol: TCol): Boolean;
var
  I: SizeInt;
begin
  Result := FMap.Find(aCol, I) <> nil;
end;

function TGListSparseTable.TRowMap.TryGetValue(constref aCol: TCol; out aValue: TValue): Boolean;
var
  I: SizeInt;
  p: PEntry;
begin
  p := FMap.Find(aCol, I);
  Result := p <> nil;
  if Result then
    aValue := p^.Value;
end;

function TGListSparseTable.TRowMap.Add(constref aCol: TCol; constref aValue: TValue): Boolean;
var
  I: SizeInt;
  p: PEntry;
begin
  Result := not FMap.FindOrAdd(aCol, p, I);
  if Result then
    begin
      p^.Value := aValue;
      Inc(FTable.FCellCount);
    end;
end;

procedure TGListSparseTable.TRowMap.AddOrSetValue(const aCol: TCol; const aValue: TValue);
var
  I: SizeInt;
  p: PEntry;
begin
  if not FMap.FindOrAdd(aCol, p, I) then
    Inc(FTable.FCellCount);
  p^.Value := aValue;
end;

function TGListSparseTable.TRowMap.Remove(constref aCol: TCol): Boolean;
begin
  Result := FMap.Remove(aCol);
  FTable.FCellCount -= Ord(Result);
end;

{ TGListSparseTable }

function TGListSparseTable.CreateRowMap: TCustomRowMap;
begin
  Result := TRowMap.Create(Self);
end;

destructor TGListSparseTable.Destroy;
begin
  Clear;
  FTable.Free;
  inherited;
end;

procedure TGListSparseTable.TrimToFit;
var
  p: PTableEntry;
begin
  FTable.TrimToFit;
  for p in FTable do
    TRowMap(p^.Columns).TrimToFit;
end;

end.

