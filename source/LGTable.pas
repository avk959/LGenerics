{****************************************************************************
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
*                                                                           *
*****************************************************************************}
unit LGTable;

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

  { TGCustomHashTable: implements rows as as linear probing hashmap }
  generic TGCustomHashTable<TRow, TCol, TValue, TRowEqRel> = class abstract(
    specialize TGCustomTable<TRow, TCol, TValue>)
  protected
  type
    TRowHashTable = specialize TGHashTableLP<TRow, TRowEntry, TRowEqRel>;

    TColEnumerable = class(TCustomColDataEnumerable)
    protected
      FEnum: TRowHashTable.TEnumerator;
      FCurrValue: TValue;
      FCol: TCol;
      function GetCurrent: TColData; override;
    public
      constructor Create(aTable: TGCustomHashTable; constref ACol: TCol);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TCellEnumerable = class(TCustomCellDataEnumerable)
    protected
      FEnum: TRowHashTable.TEnumerator;
      FRowEnum: TRowDataEnumerator;
      FCurrRowEntry: TRowData;
      function GetCurrent: TCellData; override;
    public
      constructor Create(aTable: TGCustomHashTable);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TRowEnumerable = class(specialize TGAutoEnumerable<TRow>)
    protected
      FEnum: TRowHashTable.TEnumerator;
      function GetCurrent: TRow; override;
    public
      constructor Create(aTable: TGCustomHashTable);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TRowMapEnumerable = class(specialize TGAutoEnumerable<IRowMap>)
    protected
      FEnum: TRowHashTable.TEnumerator;
      function GetCurrent: IRowMap; override;
    public
      constructor Create(aTable: TGCustomHashTable);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  var
    FRowTable: TRowHashTable;
    function  CreateRowMap: TCustomRowMap; virtual; abstract;
    function  GetFillRatio: Single; inline;
    function  GetLoadFactor: Single; inline;
    procedure SetLoadFactor(aValue: Single); inline;
    procedure ClearItems;
    function  GetRowCount: SizeInt; override;
    function  DoFindRow(constref aRow: TRow): PRowEntry; override;
  { returns True if row found, False otherwise }
    function  DoFindOrAddRow(constref aRow: TRow; out p: PRowEntry): Boolean; override;
    function  DoRemoveRow(constref aRow: TRow): SizeInt; override;
    function  GetColumn(const aCol: TCol): IColDataEnumerable; override;
    function  GetCellData: ICellDataEnumerable; override;
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

  { TGHashTable implements table with row map as linear probing hashmap;

      functor TRowEqRel(row equality relation) must provide:
        class function HashCode([const[ref]] r: TRow): SizeInt;
        class function Equal([const[ref]] L, R: TRow): Boolean;

      functor TColEqRel(column equality relation) must provide:
        class function HashCode([const[ref]] c: TCol): SizeInt;
        class function Equal([const[ref]] L, R: TCol): Boolean; }
  generic TGHashTable<TRow, TCol, TValue, TRowEqRel, TColEqRel> = class(
    specialize TGCustomHashTable<TRow, TCol, TValue, TRowEqRel>)
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

      TEnumerator = class(TRowDataEnumerator)
      protected
        FEnum: TRowMapTable.TEnumerator;
        function GetCurrent: TRowData; override;
      public
        constructor Create(aMap: TRowMapTable);
        function  MoveNext: Boolean; override;
        procedure Reset; override;
      end;

    const
      INITIAL_CAPACITY = 8;
      LOAD_FACTOR    = 0.65; //todo: why ???

    var
      FTable: TCustomTable;
      FMap: TRowMapTable;
    protected
      function  GetCount: SizeInt; override;
    public
      constructor Create(aTable: TCustomTable);
      destructor Destroy; override;
      function  GetEnumerator: TRowDataEnumerator; override;
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

  { TGHashTableR assumes that TRow implements TRowEqRel }
  generic TGHashTableR<TRow, TCol, TValue, TColEqRel> = class(
    specialize TGHashTable<TRow, TCol, TValue, TRow, TColEqRel>);

  { TGHashTableC assumes that TCol implements TColEqRel }
  generic TGHashTableC<TRow, TCol, TValue, TRowEqRel> = class(
    specialize TGHashTable<TRow, TCol, TValue, TRowEqRel, TCol>);

  { TGHashTable2 assumes that TRow implements TRowEqRel and TCol implements TColEqRel }
  generic TGHashTable2<TRow, TCol, TValue> = class(
    specialize TGHashTable<TRow, TCol, TValue, TRow, TCol>);

  { TGTreeTable implements table with row map as avl_tree map;

      functor TRowEqRel(row equality relation) must provide:
        class function HashCode([const[ref]] r: TRow): SizeInt;
        class function Equal([const[ref]] L, R: TRow): Boolean;

      functor TColCmpRel(column equality relation) must provide:
        class function Compare([const[ref]] L, R: TCol): SizeInt; }
  generic TGTreeTable<TRow, TCol, TValue, TRowEqRel, TColCmpRel> = class(
    specialize TGCustomHashTable<TRow, TCol, TValue, TRowEqRel>)
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

    TRowMap = class(TCustomRowMap)
    private
    type
      TRowMapTable = specialize TGAvlTree2<TCol, TEntry, TNodeManager, TColCmpRel>;

      TEnumerator = class(TRowDataEnumerator)
      protected
        FEnum: TRowMapTable.TEnumerator;
        function GetCurrent: TRowData; override;
      public
        constructor Create(aMap: TRowMapTable);
        function  MoveNext: Boolean; override;
        procedure Reset; override;
      end;

    var
      FTable: TCustomTable;
      FMap: TRowMapTable;
    protected
      function  GetCount: SizeInt; override;
    public
      constructor Create(aTable: TGTreeTable);
      destructor Destroy; override;
      function  GetEnumerator: TRowDataEnumerator; override;
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

  { TGTreeTableR assumes that TRow implements TRowEqRel }
  generic TGTreeTableR<TRow, TCol, TValue, TColCmpRel> = class(
    specialize TGTreeTable<TRow, TCol, TValue, TRow, TColCmpRel>);

  { TGTreeTableC assumes that TCol implements TColCmpRel }
  generic TGTreeTableC<TRow, TCol, TValue, TRowEqRel> = class(
    specialize TGTreeTable<TRow, TCol, TValue, TRowEqRel, TCol>);

  { TGTreeTable2 assumes that TRow implements TRowEqRel and TCol implements TColCmpRel }
  generic TGTreeTable2<TRow, TCol, TValue> = class(
    specialize TGTreeTable<TRow, TCol, TValue, TRow, TCol>);


  { TGListTable implements table with row map as listmap ;

      functor TRowEqRel(row equality relation) must provide:
        class function HashCode([const[ref]] r: TRow): SizeInt;
        class function Equal([const[ref]] L, R: TRow): Boolean;

      functor TColCmpRel(column equality relation) must provide:
        class function Compare([const[ref]] L, R: TCol): SizeInt; }
  generic TGListTable<TRow, TCol, TValue, TRowEqRel, TColCmpRel> = class(
    specialize TGCustomHashTable<TRow, TCol, TValue, TRowEqRel>)
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

      TEnumerator = class(TRowDataEnumerator)
      protected
        FEnum: TRowMapTable.TEnumerator;
        function GetCurrent: TRowData; override;
      public
        constructor Create(aMap: TRowMapTable);
        function  MoveNext: Boolean; override;
        procedure Reset; override;
      end;

    const
      INITIAL_CAPACITY = 8;

    var
      FTable: TCustomTable;
      FMap: TRowMapTable;
    protected
      function  GetCount: SizeInt; override;
    public
      constructor Create(aTable: TCustomTable);
      destructor Destroy; override;
      function  GetEnumerator: TRowDataEnumerator; override;
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

  { TGListTableR assumes that TRow implements TRowEqRel }
  generic TGListTableR<TRow, TCol, TValue, TColCmpRel> = class(
    specialize TGListTable<TRow, TCol, TValue, TRow, TColCmpRel>);

  { TGListTableC assumes that TCol implements TColCmpRel }
  generic TGListTableC<TRow, TCol, TValue, TRowEqRel> = class(
    specialize TGListTable<TRow, TCol, TValue, TRowEqRel, TCol>);

  { TGListTable2 assumes that TRow implements TRowEqRel and TCol implements TColCmpRel }
  generic TGListTable2<TRow, TCol, TValue> = class(
    specialize TGListTable<TRow, TCol, TValue, TRow, TCol>);

implementation
{$B-}{$COPERATORS ON}

{ TGCustomHashTable.TColEnumerable }

function TGCustomHashTable.TColEnumerable.GetCurrent: TColData;
begin
  Result.Row := FEnum.Current^.Key;
  Result.Value := FCurrValue;
end;

constructor TGCustomHashTable.TColEnumerable.Create(aTable: TGCustomHashTable; constref ACol: TCol);
begin
  inherited Create;
  FEnum := aTable.FRowTable.GetEnumerator;
  FCol := aCol;
  FCurrValue := Default(TValue);
end;

function TGCustomHashTable.TColEnumerable.MoveNext: Boolean;
begin
  repeat
    if not FEnum.MoveNext then
      exit(False);
    Result := FEnum.Current^.Columns.TryGetValue(FCol, FCurrValue);
  until Result;
end;

procedure TGCustomHashTable.TColEnumerable.Reset;
begin
  FEnum.Reset;
  FCurrValue := Default(TValue);
end;

{ TGCustomHashTable.TCellEnumerable }

function TGCustomHashTable.TCellEnumerable.GetCurrent: TCellData;
begin
  Result.Row := FEnum.Current^.Key;
  Result.Column := FCurrRowEntry.Column;
  Result.Value := FCurrRowEntry.Value;
end;

constructor TGCustomHashTable.TCellEnumerable.Create(aTable: TGCustomHashTable);
begin
  inherited Create;
  FEnum :=  aTable.FRowTable.GetEnumerator;
  FCurrRowEntry := Default(TRowData);
end;

destructor TGCustomHashTable.TCellEnumerable.Destroy;
begin
  FRowEnum.Free;
  inherited;
end;

function TGCustomHashTable.TCellEnumerable.MoveNext: Boolean;
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

procedure TGCustomHashTable.TCellEnumerable.Reset;
begin
  FEnum.Reset;
  FreeAndNil(FRowEnum);
  FCurrRowEntry := Default(TRowData);
end;

{ TGCustomHashTable.TRowEnumerable }

function TGCustomHashTable.TRowEnumerable.GetCurrent: TRow;
begin
  Result := FEnum.Current^.Key;
end;

constructor TGCustomHashTable.TRowEnumerable.Create(aTable: TGCustomHashTable);
begin
  inherited Create;
  FEnum := aTable.FRowTable.GetEnumerator;
end;

function TGCustomHashTable.TRowEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomHashTable.TRowEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomHashTable.TRowMapEnumerable }

function TGCustomHashTable.TRowMapEnumerable.GetCurrent: IRowMap;
begin
  Result := FEnum.Current^.Columns;
end;

constructor TGCustomHashTable.TRowMapEnumerable.Create(aTable: TGCustomHashTable);
begin
  inherited Create;
  FEnum := aTable.FRowTable.GetEnumerator;
end;

function TGCustomHashTable.TRowMapEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomHashTable.TRowMapEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomHashTable }

function TGCustomHashTable.GetFillRatio: Single;
begin
  Result := FRowTable.FillRatio;
end;

function TGCustomHashTable.GetLoadFactor: Single;
begin
  Result := FRowTable.LoadFactor;
end;

procedure TGCustomHashTable.SetLoadFactor(aValue: Single);
begin
  FRowTable.LoadFactor := aValue;
end;

procedure TGCustomHashTable.ClearItems;
var
  p: TRowHashTable.PEntry;
begin
  for p in FRowTable do
    p^.Columns.Free;
end;

function TGCustomHashTable.GetRowCount: SizeInt;
begin
  Result := FRowTable.Count;
end;

function TGCustomHashTable.DoFindRow(constref aRow: TRow): PRowEntry;
var
  Pos: SizeInt;
begin
  Result := FRowTable.Find(aRow, Pos);
end;

function TGCustomHashTable.DoFindOrAddRow(constref aRow: TRow; out p: PRowEntry): Boolean;
var
  Pos: SizeInt;
begin
  Result := FRowTable.FindOrAdd(aRow, p, Pos);
  if not Result then
    begin
      p^.Key := aRow;
      p^.Columns := CreateRowMap;
    end;
end;

function TGCustomHashTable.DoRemoveRow(constref aRow: TRow): SizeInt;
var
  Pos: SizeInt;
  p: PRowEntry;
begin
  p := FRowTable.Find(aRow, Pos);
  if p <> nil then
    begin
      Result := p^.Columns.Count;
      p^.Columns.Free;
      FRowTable.RemoveAt(Pos);
    end
  else
    Result := 0;
end;

function TGCustomHashTable.GetColumn(const aCol: TCol): IColDataEnumerable;
begin
  Result := TColEnumerable.Create(Self, aCol);
end;

function TGCustomHashTable.GetCellData: ICellDataEnumerable;
begin
  Result := TCellEnumerable.Create(Self);
end;

constructor TGCustomHashTable.Create;
begin
  FRowTable := TRowHashTable.Create;
end;

constructor TGCustomHashTable.Create(aRowCapacity: SizeInt);
begin
  FRowTable := TRowHashTable.Create(aRowCapacity);
end;

constructor TGCustomHashTable.Create(aLoadFactor: Single);
begin
  FRowTable := TRowHashTable.Create(aLoadFactor);
end;

constructor TGCustomHashTable.Create(aRowCapacity: SizeInt; aLoadFactor: Single);
begin
  FRowTable := TRowHashTable.Create(aRowCapacity, aLoadFactor);
end;

procedure TGCustomHashTable.Clear;
begin
  ClearItems;
  FRowTable.Clear;
end;

procedure TGCustomHashTable.EnsureRowCapacity(aValue: SizeInt);
begin
  FRowTable.EnsureCapacity(aValue);
end;

function TGCustomHashTable.RowEnum: IRowEnumerable;
begin
  Result := TRowEnumerable.Create(Self);
end;

function TGCustomHashTable.RowMapEnum: IRowMapEnumerable;
begin
  Result := TRowMapEnumerable.Create(Self);
end;

{ TGHashTable.TRowMap.TEnumerator }

function TGHashTable.TRowMap.TEnumerator.GetCurrent: TRowData;
begin
  Result.Column := FEnum.Current^.Key;
  Result.Value := FEnum.Current^.Value;
end;

constructor TGHashTable.TRowMap.TEnumerator.Create(aMap: TRowMapTable);
begin
  FEnum := aMap.GetEnumerator;
end;

function TGHashTable.TRowMap.TEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGHashTable.TRowMap.TEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGHashTable.TRowMap }

function TGHashTable.TRowMap.GetCount: SizeInt;
begin
  Result := FMap.Count;
end;

constructor TGHashTable.TRowMap.Create(aTable: TCustomTable);
begin
  FMap := TRowMapTable.Create(INITIAL_CAPACITY, LOAD_FACTOR);
  FTable := aTable;
end;

destructor TGHashTable.TRowMap.Destroy;
begin
  FTable.FCellCount -= FMap.Count;
  FMap.Free;
  inherited;
end;

function TGHashTable.TRowMap.GetEnumerator: TRowDataEnumerator;
begin
  Result := TEnumerator.Create(Self.FMap);
end;

procedure TGHashTable.TRowMap.TrimToFit;
begin
  FMap.TrimToFit;
end;

function TGHashTable.TRowMap.Contains(constref aCol: TCol): Boolean;
var
  p: SizeInt;
begin
  Result := FMap.Find(aCol, p) <> nil;
end;

function TGHashTable.TRowMap.TryGetValue(constref aCol: TCol; out aValue: TValue): Boolean;
var
  Pos: SizeInt;
  p: PEntry;
begin
  p := FMap.Find(aCol, Pos);
  Result := p <> nil;
  if Result then
    aValue := p^.Value;
end;

function TGHashTable.TRowMap.Add(constref aCol: TCol; constref aValue: TValue): Boolean;
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

procedure TGHashTable.TRowMap.AddOrSetValue(const aCol: TCol; const aValue: TValue);
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

function TGHashTable.TRowMap.Remove(constref aCol: TCol): Boolean;
begin
  Result := FMap.Remove(aCol);
  FTable.FCellCount -= Ord(Result);
end;

{ TGHashTable }

function TGHashTable.CreateRowMap: TCustomRowMap;
begin
  Result := TRowMap.Create(Self);
end;

destructor TGHashTable.Destroy;
begin
  Clear;
  FRowTable.Free;
  inherited;
end;

procedure TGHashTable.TrimToFit;
var
  p: PRowEntry;
begin
  for p in FRowTable do
    if p^.Columns.IsEmpty then
      FRowTable.Remove(p^.Key)
    else
      TRowMap(p^.Columns).TrimToFit;
  FRowTable.TrimToFit;
end;

{ TGTreeTable.TRowMap.TEnumerator }

function TGTreeTable.TRowMap.TEnumerator.GetCurrent: TRowData;
begin
  Result.Column := FEnum.Current^.Key;
  Result.Value := FEnum.Current^.Value;
end;

constructor TGTreeTable.TRowMap.TEnumerator.Create(aMap: TRowMapTable);
begin
  FEnum := aMap.GetEnumerator;
end;

function TGTreeTable.TRowMap.TEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGTreeTable.TRowMap.TEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGTreeTable.TRowMap }

function TGTreeTable.TRowMap.GetCount: SizeInt;
begin
  Result := FMap.Count;
end;

constructor TGTreeTable.TRowMap.Create(aTable: TGTreeTable);
begin
  FMap := TRowMapTable.Create(aTable.FNodeManager);
  FTable := aTable;
end;

destructor TGTreeTable.TRowMap.Destroy;
begin
  FTable.FCellCount -= FMap.Count;
  FMap.Free;
  inherited;
end;

function TGTreeTable.TRowMap.GetEnumerator: TRowDataEnumerator;
begin
  Result := TEnumerator.Create(Self.FMap);
end;

function TGTreeTable.TRowMap.Contains(constref aCol: TCol): Boolean;
begin
  Result := FMap.Find(aCol) <> nil;
end;

function TGTreeTable.TRowMap.TryGetValue(constref aCol: TCol; out aValue: TValue): Boolean;
var
  Pos: SizeInt;
  p: PNode;
begin
  p := FMap.Find(aCol);
  Result := p <> nil;
  if Result then
    aValue := p^.Data.Value;
end;

function TGTreeTable.TRowMap.Add(constref aCol: TCol; constref aValue: TValue): Boolean;
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

procedure TGTreeTable.TRowMap.AddOrSetValue(const aCol: TCol; const aValue: TValue);
var
  Pos: SizeInt;
  p: PNode;
begin
  if not FMap.FindOrAdd(aCol, p) then
    Inc(FTable.FCellCount);
  p^.Data.Value := aValue;
end;

function TGTreeTable.TRowMap.Remove(constref aCol: TCol): Boolean;
begin
  Result := FMap.Remove(aCol);
  FTable.FCellCount -= Ord(Result);
end;

{ TGTreeTable }

function TGTreeTable.CreateRowMap: TCustomRowMap;
begin
  Result := TRowMap.Create(Self);
end;

constructor TGTreeTable.Create;
begin
  inherited Create;
  FNodeManager := TNodeManager.Create;
end;

constructor TGTreeTable.Create(aRowCapacity: SizeInt);
begin
  inherited Create(aRowCapacity);
  FNodeManager := TNodeManager.Create;
end;

constructor TGTreeTable.Create(aLoadFactor: Single);
begin
  inherited Create(aLoadFactor);
  FNodeManager := TNodeManager.Create;
end;

constructor TGTreeTable.Create(aRowCapacity: SizeInt; aLoadFactor: Single);
begin
  inherited Create(aRowCapacity, aLoadFactor);
  FNodeManager := TNodeManager.Create;
end;

destructor TGTreeTable.Destroy;
begin
  Clear;
  FRowTable.Free;
  FNodeManager.Free;
  inherited;
end;

procedure TGTreeTable.Clear;
begin
  inherited;
  FNodeManager.Clear;
end;

procedure TGTreeTable.TrimToFit;
var
  p: PRowEntry;
begin
  for p in FRowTable do
    if p^.Columns.IsEmpty then
      FRowTable.Remove(p^.Key);
  FRowTable.TrimToFit;
  if CellCount = 0 then
    FNodeManager.Clear;
end;

{ TGListTable.TRowMap.TEnumerator }

function TGListTable.TRowMap.TEnumerator.GetCurrent: TRowData;
begin
  Result.Column := FEnum.Current^.Key;
  Result.Value := FEnum.Current^.Value;
end;

constructor TGListTable.TRowMap.TEnumerator.Create(aMap: TRowMapTable);
begin
  FEnum := aMap.GetEnumerator;
end;

function TGListTable.TRowMap.TEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGListTable.TRowMap.TEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGListTable.TRowMap }

function TGListTable.TRowMap.GetCount: SizeInt;
begin
  Result := FMap.Count;
end;

constructor TGListTable.TRowMap.Create(aTable: TCustomTable);
begin
  FMap := TRowMapTable.Create(INITIAL_CAPACITY);
  FTable := aTable;
end;

destructor TGListTable.TRowMap.Destroy;
begin
  FTable.FCellCount -= FMap.Count;
  FMap.Free;
  inherited;
end;

function TGListTable.TRowMap.GetEnumerator: TRowDataEnumerator;
begin
  Result := TEnumerator.Create(Self.FMap);
end;

procedure TGListTable.TRowMap.TrimToFit;
begin
  FMap.TrimToFit;
end;

function TGListTable.TRowMap.Contains(constref aCol: TCol): Boolean;
var
  I: SizeInt;
begin
  Result := FMap.Find(aCol, I) <> nil;
end;

function TGListTable.TRowMap.TryGetValue(constref aCol: TCol; out aValue: TValue): Boolean;
var
  I: SizeInt;
  p: PEntry;
begin
  p := FMap.Find(aCol, I);
  Result := p <> nil;
  if Result then
    aValue := p^.Value;
end;

function TGListTable.TRowMap.Add(constref aCol: TCol; constref aValue: TValue): Boolean;
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

procedure TGListTable.TRowMap.AddOrSetValue(const aCol: TCol; const aValue: TValue);
var
  I: SizeInt;
  p: PEntry;
begin
  if not FMap.FindOrAdd(aCol, p, I) then
    Inc(FTable.FCellCount);
  p^.Value := aValue;
end;

function TGListTable.TRowMap.Remove(constref aCol: TCol): Boolean;
begin
  Result := FMap.Remove(aCol);
  FTable.FCellCount -= Ord(Result);
end;

{ TGListTable }

function TGListTable.CreateRowMap: TCustomRowMap;
begin
  Result := TRowMap.Create(Self);
end;

destructor TGListTable.Destroy;
begin
  Clear;
  FRowTable.Free;
  inherited;
end;

procedure TGListTable.TrimToFit;
var
  p: PRowEntry;
begin
  for p in FRowTable do
    if p^.Columns.IsEmpty then
      FRowTable.Remove(p^.Key)
    else
      TRowMap(p^.Columns).TrimToFit;
  FRowTable.TrimToFit;
end;

end.

