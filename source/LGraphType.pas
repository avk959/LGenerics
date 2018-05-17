{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic simple sparse graph abstract ancestor.                          *
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
unit LGraphType;

{$mode objfpc}{$H+}
{$INLINE ON}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes,
  SysUtils,
  math,
  LGUtils,
  {%H-}LGHelpers,
  LGraphUtils,
  LGVector,
  LGStrConst;

type

  generic TGAdjItem<TData> = record
    Destination: SizeInt;
    Data: TData;
    property Key: SizeInt read Destination;
  end;

  { TGVertexItem }

  generic TGVertexItem<TVertex, TData> = record
  public
  type
    TAdjItem      = specialize TGAdjItem<TData>;
    TAdjItemArray = array of TAdjItem;
    PAdjItem      = ^TAdjItem;

  private
  type
    TNode = record
      Hash: SizeInt;
      Item: TAdjItem;
    end;

    PNode     = ^TNode;
    TNodeList = array of TNode;

  const
    NODE_SIZE               = SizeOf(TNode);
    SLOT_NOT_FOUND: SizeInt = Low(SizeInt);
    USED_FLAG: SizeInt      = SizeInt(1);
    MAX_CAPACITY: SizeInt   = MAX_CONTAINER_SIZE div NODE_SIZE;
    INITIAL_SIZE            = 8;

  public
  type
    TEnumerator = record
    private
      FList: PNode;
      FCurrIndex,
      FLastIndex: SizeInt;
      function  GetCurrent: PAdjItem; inline;
    public
      function  MoveNext: Boolean;
      procedure Reset; inline;
      property  Current: PAdjItem read GetCurrent;
    end;

  var
    Vertex: TVertex;
  private
    FNodeList: TNodeList;
    FShift,
    FCount,
    FTag: SizeInt;
    function  GetCapacity: SizeInt; inline;
    procedure Rehash(var aNewList: TNodeList);
    procedure Resize(aNewCapacity: SizeInt);
    procedure Expand;
    function  DoFind(aValue, aHash: SizeInt): SizeInt;
    procedure DoRemove(aIndex: SizeInt);
    class function HashCode(aValue: SizeInt): SizeInt; static; inline;
    class function NewList(aCapacity: SizeInt): TNodeList; static; //inline;
    property Shift: SizeInt read FShift;
    class constructor Init;
    class operator Initialize(var Item: TGVertexItem);
    class operator Copy(constref aSrc: TGVertexItem; var aDst: TGVertexItem);
  public
    function  GetEnumerator: TEnumerator;
    function  ToArray: TAdjItemArray;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear;
    procedure ClearItems;
    procedure TrimToFit; inline;
    function  Contains(aDst: SizeInt): Boolean; inline;
    function  FindOrAdd(aDst: SizeInt; out e: PAdjItem): Boolean; inline;
    function  Find(aDst: SizeInt): PAdjItem;
    function  FindFirst(out aDst: SizeInt): Boolean; inline;
    function  Add(constref aItem: TAdjItem): Boolean; inline;
    function  Remove(aDst: SizeInt): Boolean; inline;
    property  Key: TVertex read Vertex;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
    property  Tag: SizeInt read FTag write FTag;
  end;

  { TGVertexHashList }

  generic TGVertexHashList<TVertex, TEdgeData, TEqRel> = record
  public
  type
    TVertexItem = specialize TGVertexItem<TVertex, TEdgeData>;
    PVertexItem = ^TVertexItem;

  private
  type
    TNode = record
      Hash,
      Next: SizeInt;
      Item: TVertexItem;
    end;

    TNodeList   = array of TNode;
    TChainList  = array of SizeInt;
    //PVertexList = ^TGVertexHashList;

  const
    NULL_INDEX  = SizeInt(-1);
    NODE_SIZE   = SizeOf(TNode);
    MAX_CAPACITY: SizeInt  = MAX_CONTAINER_SIZE div NODE_SIZE;

  var
    FNodeList: TNodeList;
    FChainList: TChainList;
    FCount: SizeInt;
    function  GetCapacity: SizeInt; inline;
    function  GetItem(aIndex: SizeInt): TVertex; inline;
    procedure InitialAlloc;
    procedure Rehash;
    procedure Resize(aNewCapacity: SizeInt);
    procedure Expand;
    function  Find(constref v: TVertex): SizeInt;
    function  Find(constref v: TVertex; aHash: SizeInt): SizeInt;
    function  DoAdd(constref v: TVertex; aHash: SizeInt): SizeInt;
    procedure RemoveFromChain(aIndex: SizeInt);
    procedure DoDelete(aIndex: SizeInt);
    function  DoRemove(constref v: TVertex): Boolean;
    procedure CheckIndexRange(aIndex: SizeInt); //inline;
    class constructor Init;
    class operator Initialize(var vl: TGVertexHashList);
    class operator Copy(constref aSrc: TGVertexHashList; var aDst: TGVertexHashList);
  public
    procedure Clear;
    procedure EnsureCapacity(aValue: SizeInt);
    procedure TrimToFit;
    function  IndexOf(constref v: TVertex): SizeInt; inline;
    function  FindOrAdd(constref v: TVertex; out aIndex: SizeInt): Boolean;
    function  Remove(constref v: TVertex): Boolean; inline;
    procedure Delete(aIndex: SizeInt); inline;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
    property  Items[aIndex: SizeInt]: TVertex read GetItem; default;
  end;

  { TGCustomSimpleSparseGraph simple sparse graph abstract ancestor class based on adjacency lists;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGCustomSimpleSparseGraph<TVertex, TEdgeData, TVertexEqRel> = class abstract
  protected
  type
    TVertexList = specialize TGVertexHashList<TVertex, TEdgeData, TVertexEqRel>;
    TVertexItem = TVertexList.TVertexItem;

  var
    FVertexList: TVertexList;
    FEdgeCount: SizeInt;
    FLabel: string;
    function  GetVertexCount: SizeInt; inline;
    function  GetVertex(aIndex: SizeInt): TVertex; inline;
  public
  type
    TAdjItem = TVertexItem.TAdjItem;

    TEdge = record
      Source,
      Destination: SizeInt;
      Data:  TEdgeData;
    end;

    TIncidentEdge = record
      Destination: SizeInt;
      Data:  TEdgeData;
    end;

    TAdjEnumerator = record
    private
      FEnum: TVertexItem.TEnumerator;
      function  GetCurrent: SizeInt; inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: SizeInt read GetCurrent;
    end;

    TAdjVertices = record
    private
      FGraph: TGCustomSimpleSparseGraph;
      FSource: SizeInt;
    public
      function GetEnumerator: TAdjEnumerator; inline;
    end;

    TIncidentEnumerator = record
    private
      FEnum: TVertexItem.TEnumerator;
      function  GetCurrent: TIncidentEdge;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: TIncidentEdge read GetCurrent;
    end;

    TIncidentEdges = record
    private
      FGraph: TGCustomSimpleSparseGraph;
      FSource: SizeInt;
    public
      function GetEnumerator: TIncidentEnumerator; inline;
    end;

    TEdgeEnumerator = record
    private
      FList: TVertexList.TNodeList;
      FEnum: TVertexItem.TEnumerator;
      FCurrIndex,
      FLastIndex: SizeInt;
      FEnumDone: Boolean;
      function  GetCurrent: TEdge;
    public
      function  MoveNext: Boolean;
      procedure Reset;
      property  Current: TEdge read GetCurrent;
    end;

    TEdges = record
    private
      FGraph: TGCustomSimpleSparseGraph;
    public
      function GetEnumerator: TEdgeEnumerator; inline;
    end;

    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear; inline;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    procedure TrimToFit; inline;
    function  ContainsVertex(constref v: TVertex): Boolean; inline;
    function  ContainsEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  ContainsEdgeI(aSrc, aDst: SizeInt): Boolean;
    function  IndexOf(constref v: TVertex): SizeInt; inline;
  { returns True and vertex index, if it was added, False otherwise }
    function  AddVertex(constref v: TVertex; out aIndex: SizeInt): Boolean; inline;
    function  Adjacent(constref aSrc, aDst: TVertex): Boolean; inline;
    function  AdjacentI(aSrc, aDst: SizeInt): Boolean; //inline;
  { enumerates indices of adjacent vertices }
    function  AdjVertices(constref aSrc: TVertex): TAdjVertices; inline;
    function  AdjVerticesI(aSrc: SizeInt): TAdjVertices;
  { enumerates incident edges }
    function  IncidentEdges(constref aSrc: TVertex): TIncidentEdges; inline;
    function  IncidentEdgesI(aSrc: SizeInt): TIncidentEdges;
  { enumerates all edges }
    function  Edges: TEdges;
  { returns count of visited vertices; aOnGray calls after vertex visite, aOnWhite calls after vertex found }
    function  DSFTraversal(constref aRoot: TVertex; aOnGray: TOnVertexVisit = nil;
                           aOnWhite: TOnVertexVisit = nil): SizeInt; inline;
    function  DSFTraversalI(aRoot: SizeInt; aOnGray: TOnVertexVisit = nil;
                            aOnWhite: TOnVertexVisit = nil): SizeInt;
    function  DSFTraversal(constref aRoot: TVertex; aOnGray, aOnWhite: TNestVertexVisit): SizeInt; inline;
    function  DSFTraversalI(aRoot: SizeInt; aOnGray, aOnWhite: TNestVertexVisit): SizeInt;
  { returns count of visited vertices; aOnGray calls after vertex visite, aOnWhite calls after vertex found }
    function  BSFTraversal(constref aRoot: TVertex; aOnGray: TOnVertexVisit = nil;
                           aOnWhite: TOnVertexVisit = nil): SizeInt; inline;
    function  BSFTraversalI(aRoot: SizeInt; aOnGray: TOnVertexVisit = nil;
                            aOnWhite: TOnVertexVisit = nil): SizeInt;
    function  BSFTraversal(constref aRoot: TVertex; aOnGray, aOnWhite: TNestVertexVisit): SizeInt; inline;
    function  BSFTraversalI(aRoot: SizeInt; aOnGray, aOnWhite: TNestVertexVisit): SizeInt;

    function  SimplePathExists(constref aSrc, aDst: TVertex): Boolean; inline;
    function  SimplePathExistsI(aSrc, aDst: SizeInt): Boolean;

    property  GraphLabel: string read FLabel write FLabel;
    property  VertexCount: SizeInt read GetVertexCount;
    property  EdgeCount: SizeInt read FEdgeCount;
    property  Vertices[aIndex: SizeInt]: TVertex read GetVertex; default;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGVertexItem.TEnumerator }

function TGVertexItem.TEnumerator.GetCurrent: PAdjItem;
begin
  Result := @FList[FCurrIndex].Item;
end;

function TGVertexItem.TEnumerator.MoveNext: Boolean;
begin
  repeat
    if FCurrIndex >= FLastIndex then
      exit(False);
    Inc(FCurrIndex);
    Result := FList[FCurrIndex].Hash <> 0;
  until Result;
end;

procedure TGVertexItem.TEnumerator.Reset;
begin
  FCurrIndex := -1;
end;

{ TGVertexItem }

function TGVertexItem.GetCapacity: SizeInt;
begin
  Result := System.Length(FNodeList);
end;

procedure TGVertexItem.Rehash(var aNewList: TNodeList);
var
  h, I, J, Mask: SizeInt;
begin
  if Count > 0 then
    begin
      Mask := System.High(aNewList);
      for I := 0 to System.High(FNodeList) do
        begin
          if FNodeList[I].Hash <> 0 then
            begin
              h := FNodeList[I].Hash shr Shift;
              for J := 0 to Mask do
                begin
                  if aNewList[h].Hash = 0 then // -> target node is empty
                    begin
                      aNewList[h] := FNodeList[I];
                      break;
                    end;
                  h := Succ(h) and Mask;       // probe sequence
                end;
            end;
        end;
    end;
end;

procedure TGVertexItem.Resize(aNewCapacity: SizeInt);
var
  List: TNodeList;
begin
  List := NewList(aNewCapacity);
{$IF DEFINED(CPU64)}
  FShift := 64 - BsrQWord(QWord(aNewCapacity));
{$ELSEIF DEFINED(CPU32)}
  FShift := 32 - BsrDWord(DWord(aNewCapacity));
{$ELSE}
  FShift := 16 - BsrWord(Word(aNewCapacity));
{$ENDIF}
  Rehash(List);
  FNodeList := List;
end;

procedure TGVertexItem.Expand;
var
  NewCapacity, OldCapacity: SizeInt;
begin
  OldCapacity := GetCapacity;
  if OldCapacity > 0 then
    begin
      NewCapacity := Math.Min(MAX_CAPACITY, OldCapacity shl 1);
      if NewCapacity > OldCapacity then
        Resize(NewCapacity);
    end
  else
    Resize(INITIAL_SIZE);
end;

function TGVertexItem.DoFind(aValue, aHash: SizeInt): SizeInt;
var
  I, Pos, Mask: SizeInt;
begin
  Mask := System.High(FNodeList);
  Result := SLOT_NOT_FOUND;
  Pos := aHash shr Shift;
  for I := 0 to Mask do
    begin
      if FNodeList[Pos].Hash = 0 then // node empty => key not found
        exit(not Pos)
      else
        if FNodeList[Pos].Item.Key = aValue then
          exit(Pos);                // key found
      Pos := Succ(Pos) and Mask;    // probe sequence
    end;
end;

procedure TGVertexItem.DoRemove(aIndex: SizeInt);
var
  h, Gap, Mask: SizeInt;
begin
  Mask := System.High(FNodeList);
  FNodeList[aIndex].Hash := 0;
  Gap := aIndex;
  aIndex := Succ(aIndex) and Mask;
  Dec(FCount);
  repeat
    if FNodeList[aIndex].Hash = 0 then
      break;
    h := FNodeList[aIndex].Hash shr Shift;
    if (h <> aIndex) and (Succ(aIndex - h + Mask) and Mask >= Succ(aIndex - Gap + Mask) and Mask) then
      begin
        FNodeList[Gap] := FNodeList[aIndex];
        FNodeList[aIndex].Hash := 0;
        Gap := aIndex;
      end;
    aIndex := Succ(aIndex) and Mask;
  until False;
end;

class function TGVertexItem.HashCode(aValue: SizeInt): SizeInt;
begin
{$IF DEFINED(CPU64)}
  Result := aValue * SizeInt($9e3779b97f4a7c15);
{$ELSEIF DEFINED(CPU32)}
  Result := aValue * SizeInt($9e3779b9);
{$ELSE}
  Result := aValue * SizeInt($9e37);
{$ENDIF}
end;

class function TGVertexItem.NewList(aCapacity: SizeInt): TNodeList;
begin
  System.SetLength(Result, aCapacity);
  System.FillChar(Result[0], aCapacity * NODE_SIZE, 0);
end;

class constructor TGVertexItem.Init;
begin
{$PUSH}{$J+}
  MAX_CAPACITY := LGUtils.RoundUpTwoPower(MAX_CAPACITY);
{$POP}
end;

class operator TGVertexItem.Initialize(var Item: TGVertexItem);
begin
  Item.FCount := 0;
end;

class operator TGVertexItem.Copy(constref aSrc: TGVertexItem; var aDst: TGVertexItem);
begin
  aDst.FNodeList := System.Copy(aSrc.FNodeList);
  aDst.FCount := aSrc.Count;
  aDst.FShift := aSrc.FShift;
end;

function TGVertexItem.GetEnumerator: TEnumerator;
begin
  Result.FLastIndex := System.High(FNodeList);
  if Result.FLastIndex >= 0 then
    Result.FList := @FNodeList[0]
  else
    Result.FList := nil;
  Result.FCurrIndex := -1;
end;

function TGVertexItem.ToArray: TAdjItemArray;
var
  I, J: SizeInt;
begin
  System.SetLength(Result, Count);
  if Count > 0 then
    begin
      I := 0;
      J := 0;
      repeat
        if FNodeList[J].Hash <> 0 then
          begin
            Result[I] := FNodeList[J].Item;
            Inc(I);
          end;
        Inc(J);
      until J >= Capacity;
    end;
end;

function TGVertexItem.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TGVertexItem.NonEmpty: Boolean;
begin
  Result := Count <> 0;
end;

procedure TGVertexItem.Clear;
begin
  FNodeList := nil;
  FCount := 0;
end;

procedure TGVertexItem.ClearItems;
begin
  System.FillChar(FNodeList[0], Capacity * NODE_SIZE, 0);
  FCount := 0;
end;

procedure TGVertexItem.TrimToFit;
var
  NewCapacity: SizeInt;
begin
  if Count = 0 then
    begin
      NewCapacity := LGUtils.RoundUpTwoPower(Count shl 1);
      if NewCapacity < GetCapacity then
        Resize(NewCapacity);
    end
  else
    Clear;
end;

function TGVertexItem.Contains(aDst: SizeInt): Boolean;
begin
  Result := Find(aDst) <> nil;
end;

function TGVertexItem.FindOrAdd(aDst: SizeInt; out e: PAdjItem): Boolean;
var
  Hash, Pos: SizeInt;
begin
  if FNodeList = nil then
    Expand;
  Hash := HashCode(aDst) or USED_FLAG;
  Pos := DoFind(aDst, Hash);
  Result := Pos >= 0; // key found?
  if not Result then   // key not found, will add new slot
    begin
      if Count >= Capacity shr 1 then
        begin
          Expand;
          Pos := DoFind(aDst, Hash);
        end;
      if Pos <> SLOT_NOT_FOUND then
        begin
          Pos := not Pos;
          FNodeList[Pos].Hash := Hash;
          Inc(FCount);
        end
      else
        raise ELGCapacityExceed.CreateFmt(SECapacityExceedFmt, [Succ(Count)]);
    end;
  e := @FNodeList[Pos].Item;
end;

function TGVertexItem.Find(aDst: SizeInt): PAdjItem;
var
  Pos: SizeInt;
begin
  Result := nil;
  if Count > 0 then
    begin
      Pos := DoFind(aDst, HashCode(aDst));
      if Pos >= 0 then
        Result := @FNodeList[Pos].Item;
    end;
end;

function TGVertexItem.FindFirst(out aDst: SizeInt): Boolean;
var
  I: SizeInt;
begin
  if Count > 0 then
    for I := 0 to Pred(Capacity) do
      if FNodeList[I].Hash <> 0 then
        begin
          aDst := FNodeList[I].Item.Key;
          exit(True);
        end;
  Result := False;
end;

function TGVertexItem.Add(constref aItem: TAdjItem): Boolean;
var
  p: PAdjItem;
begin
  Result := not FindOrAdd(aItem.Key, p);
  if Result then
    p^ := aItem;
end;

function TGVertexItem.Remove(aDst: SizeInt): Boolean;
var
  Pos: SizeInt;
begin
  if Count > 0 then
    begin
      Pos := DoFind(aDst, HashCode(aDst));
      Result := Pos >= 0;
      if Result then
        DoRemove(Pos);
    end
  else
    Result := False;
end;

{ TGVertexHashList }

function TGVertexHashList.GetCapacity: SizeInt;
begin
  Result := System.Length(FNodeList);
end;

function TGVertexHashList.GetItem(aIndex: SizeInt): TVertex;
begin
  CheckIndexRange(aIndex);
  Result := FNodeList[aIndex].Item.Vertex;
end;

procedure TGVertexHashList.InitialAlloc;
begin
  System.SetLength(FNodeList, DEFAULT_CONTAINER_CAPACITY);
  System.SetLength(FChainList, DEFAULT_CONTAINER_CAPACITY);
  System.FillChar(FChainList[0], DEFAULT_CONTAINER_CAPACITY * SizeOf(SizeInt), $ff);
end;

procedure TGVertexHashList.Rehash;
var
  I, J, Mask: SizeInt;
begin
  Mask := Pred(Capacity);
  System.FillChar(FChainList[0], Succ(Mask) * SizeOf(SizeInt), $ff);
  for I := 0 to Pred(Count) do
    begin
      J := FNodeList[I].Hash and Mask;
      FNodeList[I].Next := FChainList[J];
      FChainList[J] := I;
    end;
end;

procedure TGVertexHashList.Resize(aNewCapacity: SizeInt);
begin
  System.SetLength(FNodeList, aNewCapacity);
  System.SetLength(FChainList, aNewCapacity);
  Rehash;
end;

procedure TGVertexHashList.Expand;
var
  OldCapacity: SizeInt;
begin
  OldCapacity := Capacity;
  if OldCapacity > 0 then
    begin
      if OldCapacity < MAX_CAPACITY then
        Resize(OldCapacity shl 1)
      else
        raise ELGCapacityExceed.CreateFmt(SECapacityExceedFmt, [OldCapacity shl 1]);
    end
  else
    InitialAlloc;
end;

function TGVertexHashList.Find(constref v: TVertex): SizeInt;
var
  h: SizeInt;
begin
  h := TEqRel.HashCode(v);
  Result := FChainList[h and Pred(Capacity)];
  while Result <> NULL_INDEX do
    begin
      if (FNodeList[Result].Hash = h) and TEqRel.Equal(FNodeList[Result].Item.Vertex, v) then
        exit;
      Result := FNodeList[Result].Next;
    end;
end;

function TGVertexHashList.Find(constref v: TVertex; aHash: SizeInt): SizeInt;
begin
  Result := FChainList[aHash and Pred(Capacity)];
  while Result <> NULL_INDEX do
    begin
      if (FNodeList[Result].Hash = aHash) and TEqRel.Equal(FNodeList[Result].Item.Vertex, v) then
        exit;
      Result := FNodeList[Result].Next;
    end;
end;

function TGVertexHashList.DoAdd(constref v: TVertex; aHash: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  Result := Count;
  FNodeList[Result].Hash := aHash;
  I := aHash and Pred(Capacity);
  FNodeList[Result].Next := FChainList[I];
  FNodeList[Result].Item.Vertex := v;
  FChainList[I] := Result;
  Inc(FCount);
end;

procedure TGVertexHashList.RemoveFromChain(aIndex: SizeInt);
var
  I, Curr, Prev: SizeInt;
begin
  I := FNodeList[aIndex].Hash and Pred(Capacity);
  Curr := FChainList[I];
  Prev := NULL_INDEX;
  while Curr <> NULL_INDEX do
    begin
      if Curr = aIndex then
        begin
          if Prev <> NULL_INDEX then
            FNodeList[Prev].Next := FNodeList[Curr].Next
          else
            FChainList[I] := FNodeList[Curr].Next;
          exit;
        end;
      Prev := Curr;
      Curr := FNodeList[Curr].Next;
    end;
end;

procedure TGVertexHashList.DoDelete(aIndex: SizeInt);
begin
  Dec(FCount);
  if aIndex < Count then
    begin
      FNodeList[aIndex].Item := Default(TVertexItem);
      System.Move(FNodeList[Succ(aIndex)], FNodeList[aIndex], (Count - aIndex) * NODE_SIZE);
      System.FillChar(FNodeList[Count].Item, SizeOf(TVertexItem), 0);
      Rehash;
    end
  else   // last element
    begin
      RemoveFromChain(aIndex);
      System.FillChar(FNodeList[Count].Item, SizeOf(TVertexItem), 0);
    end;
end;

function TGVertexHashList.DoRemove(constref v: TVertex): Boolean;
var
  RemoveIdx: SizeInt;
begin
  RemoveIdx := Find(v);
  Result := RemoveIdx >= 0;
  if Result then
    DoDelete(RemoveIdx);
end;

procedure TGVertexHashList.CheckIndexRange(aIndex: SizeInt);
begin
  if (aIndex < 0) or (aIndex >= Count) then
    raise ELGListError.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
end;

class constructor TGVertexHashList.Init;
begin
{$PUSH}{$J+}
  MAX_CAPACITY := LGUtils.RoundUpTwoPower(MAX_CAPACITY);
{$POP}
end;

class operator TGVertexHashList.Initialize(var vl: TGVertexHashList);
begin
  vl.FCount := 0;
end;

class operator TGVertexHashList.Copy(constref aSrc: TGVertexHashList; var aDst: TGVertexHashList);
begin
  aDst.FNodeList := System.Copy(aSrc.FNodeList);
  aDst.FChainList := System.Copy(aSrc.FChainList);
  aDst.FCount := aSrc.FCount;
end;

procedure TGVertexHashList.Clear;
begin
  FNodeList := nil;
  FChainList := nil;
  FCount := 0;
end;

procedure TGVertexHashList.EnsureCapacity(aValue: SizeInt);
begin
  if aValue <= Capacity then
    exit;
  if aValue <= DEFAULT_CONTAINER_CAPACITY then
    aValue := DEFAULT_CONTAINER_CAPACITY
  else
    if aValue <= MAX_CAPACITY then
      aValue := LGUtils.RoundUpTwoPower(aValue)
    else
      raise ELGCapacityExceed.CreateFmt(SECapacityExceedFmt, [aValue]);
  Resize(aValue);
end;

procedure TGVertexHashList.TrimToFit;
var
  I, NewCapacity: SizeInt;
begin
  if Count > 0 then
    begin
      NewCapacity := LGUtils.RoundUpTwoPower(Count shl 1);
      if NewCapacity < Capacity then
        begin
          for I := 0 to Pred(Count) do
            FNodeList[I].Item.TrimToFit;
          Resize(NewCapacity);
        end;
    end
  else
    Clear;
end;

function TGVertexHashList.IndexOf(constref v: TVertex): SizeInt;
begin
  if Count > 0 then
    Result := Find(v)
  else
    Result := NULL_INDEX;
end;

function TGVertexHashList.FindOrAdd(constref v: TVertex; out aIndex: SizeInt): Boolean;
var
  h: SizeInt;
begin
  h := TEqRel.HashCode(v);
  if Count > 0 then
    aIndex := Find(v, h)
  else
    aIndex := NULL_INDEX;
  Result := aIndex >= 0;
  if not Result then
    begin
      if Count = Capacity then
        Expand;
      aIndex := DoAdd(v, h);
    end;
end;

function TGVertexHashList.Remove(constref v: TVertex): Boolean;
begin
  if Count > 0 then
    Result := DoRemove(v)
  else
    Result := False;
end;

procedure TGVertexHashList.Delete(aIndex: SizeInt);
begin
  CheckIndexRange(aIndex);
  DoDelete(aIndex);
end;

{ TGCustomSimpleSparseGraph.TAdjEnumerator }

function TGCustomSimpleSparseGraph.TAdjEnumerator.GetCurrent: SizeInt;
begin
  Result := FEnum.Current^.Destination;
end;

function TGCustomSimpleSparseGraph.TAdjEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomSimpleSparseGraph.TAdjEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomSimpleSparseGraph.TAdjVertices }

function TGCustomSimpleSparseGraph.TAdjVertices.GetEnumerator: TAdjEnumerator;
begin
  Result.FEnum := FGraph.FVertexList.FNodeList[FSource].Item.GetEnumerator;
end;

{ TGCustomSimpleSparseGraph.TIncidentEnumerator }

function TGCustomSimpleSparseGraph.TIncidentEnumerator.GetCurrent: TIncidentEdge;
var
  p: ^TAdjItem;
begin
  p := FEnum.Current;
  Result.Destination := p^.Destination;
  Result.Data := p^.Data;
end;

function TGCustomSimpleSparseGraph.TIncidentEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomSimpleSparseGraph.TIncidentEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomSimpleSparseGraph.TIncidentEdges }

function TGCustomSimpleSparseGraph.TIncidentEdges.GetEnumerator: TIncidentEnumerator;
begin
  Result.FEnum := FGraph.FVertexList.FNodeList[FSource].Item.GetEnumerator;
end;

{ TGCustomSimpleSparseGraph.TEdgeEnumerator }

function TGCustomSimpleSparseGraph.TEdgeEnumerator.GetCurrent: TEdge;
var
  p: ^TAdjItem;
begin
  p := FEnum.Current;
  Result.Source := FCurrIndex;
  Result.Destination := p^.Destination;
  Result.Data := p^.Data;
end;

function TGCustomSimpleSparseGraph.TEdgeEnumerator.MoveNext: Boolean;
begin
  repeat
    if FEnumDone then
      begin
        if FCurrIndex >= FLastIndex then
          exit(False);
        Inc(FCurrIndex);
        FEnum := FList[FCurrIndex].Item.GetEnumerator;
      end;
    Result := FEnum.MoveNext;
    FEnumDone := not Result;
  until Result;
end;

procedure TGCustomSimpleSparseGraph.TEdgeEnumerator.Reset;
begin
  FCurrIndex := -1;
  FEnumDone := True;
end;

{ TGCustomSimpleSparseGraph.TEdges }

function TGCustomSimpleSparseGraph.TEdges.GetEnumerator: TEdgeEnumerator;
begin
  Result.FList := FGraph.FVertexList.FNodeList;
  Result.FLastIndex := Pred(FGraph.FVertexList.Count);
  Result.FCurrIndex := -1;
  Result.FEnumDone := True;
end;

{ TGCustomSimpleSparseGraph }

function TGCustomSimpleSparseGraph.GetVertexCount: SizeInt;
begin
  Result := FVertexList.Count;
end;

function TGCustomSimpleSparseGraph.GetVertex(aIndex: SizeInt): TVertex;
begin
  Result := FVertexList[aIndex];
end;

function TGCustomSimpleSparseGraph.IsEmpty: Boolean;
begin
  Result := FVertexList.Count = 0;
end;

function TGCustomSimpleSparseGraph.NonEmpty: Boolean;
begin
  Result := FVertexList.Count <> 0;
end;

procedure TGCustomSimpleSparseGraph.Clear;
begin
  FVertexList.Clear;
  FEdgeCount := 0;
  FLabel := '';
end;

procedure TGCustomSimpleSparseGraph.EnsureCapacity(aValue: SizeInt);
begin
  FVertexList.EnsureCapacity(aValue);
end;

procedure TGCustomSimpleSparseGraph.TrimToFit;
begin
  FVertexList.TrimToFit;
end;

function TGCustomSimpleSparseGraph.ContainsVertex(constref v: TVertex): Boolean;
begin
  Result := FVertexList.IndexOf(v) >= 0;
end;

function TGCustomSimpleSparseGraph.ContainsEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := ContainsEdgeI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGCustomSimpleSparseGraph.ContainsEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  if (aSrc < 0) or (aSrc >= FVertexList.Count) then
    exit(False);
  if (aDst < 0) or (aDst >= FVertexList.Count) then
    exit(False);
  Result := FVertexList.FNodeList[aSrc].Item.Contains(aDst);
end;

function TGCustomSimpleSparseGraph.IndexOf(constref v: TVertex): SizeInt;
begin
  Result := FVertexList.IndexOf(v);
end;

function TGCustomSimpleSparseGraph.AddVertex(constref v: TVertex; out aIndex: SizeInt): Boolean;
begin
  Result := not FVertexList.FindOrAdd(v, aIndex);
  if Result then
    FVertexList.FNodeList[aIndex].Item.Tag := 0;
end;

function TGCustomSimpleSparseGraph.Adjacent(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := AdjacentI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGCustomSimpleSparseGraph.AdjacentI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := FVertexList.FNodeList[aSrc].Item.Contains(aDst) or
            FVertexList.FNodeList[aDst].Item.Contains(aSrc);
end;

function TGCustomSimpleSparseGraph.AdjVertices(constref aSrc: TVertex): TAdjVertices;
begin
  Result := AdjVerticesI(FVertexList.IndexOf(aSrc));
end;

function TGCustomSimpleSparseGraph.AdjVerticesI(aSrc: SizeInt): TAdjVertices;
begin
  FVertexList.CheckIndexRange(aSrc);
  Result.FGraph := Self;
  Result.FSource := aSrc;
end;

function TGCustomSimpleSparseGraph.IncidentEdges(constref aSrc: TVertex): TIncidentEdges;
begin
  Result := IncidentEdgesI(FVertexList.IndexOf(aSrc));
end;

function TGCustomSimpleSparseGraph.IncidentEdgesI(aSrc: SizeInt): TIncidentEdges;
begin
  FVertexList.CheckIndexRange(aSrc);
  Result.FGraph := Self;
  Result.FSource := aSrc;
end;

function TGCustomSimpleSparseGraph.Edges: TEdges;
begin
  Result.FGraph := Self;
end;

function TGCustomSimpleSparseGraph.DSFTraversal(constref aRoot: TVertex; aOnGray: TOnVertexVisit;
  aOnWhite: TOnVertexVisit): SizeInt;
begin
  Result := DSFTraversalI(FVertexList.IndexOf(aRoot), aOnGray, aOnWhite);
end;

function TGCustomSimpleSparseGraph.DSFTraversalI(aRoot: SizeInt; aOnGray: TOnVertexVisit;
  aOnWhite: TOnVertexVisit): SizeInt;
var
  Visited: TBitVector;
  Stack: TIntStack;
begin
  Result := 0;
  FVertexList.CheckIndexRange(aRoot);
  Visited.Size := VertexCount;
  {%H-}Stack.Push(aRoot);
  repeat
    if not Visited[aRoot] then
      begin
        Inc(Result);
        Visited[aRoot] := True;
        if aOnGray <> nil then
          aOnGray(aRoot);
        for aRoot in AdjVerticesI(aRoot) do
          if not Visited[aRoot] then
            begin
              Stack.Push(aRoot);
              if aOnWhite <> nil then
                aOnWhite(aRoot);
            end;
      end;
  until not Stack.TryPop(aRoot);
end;

function TGCustomSimpleSparseGraph.DSFTraversal(constref aRoot: TVertex; aOnGray,
  aOnWhite: TNestVertexVisit): SizeInt;
begin
  Result := DSFTraversalI(FVertexList.IndexOf(aRoot), aOnGray, aOnWhite);
end;

function TGCustomSimpleSparseGraph.DSFTraversalI(aRoot: SizeInt; aOnGray,
  aOnWhite: TNestVertexVisit): SizeInt;
var
  Visited: TBitVector;
  Stack: TIntStack;
begin
  Result := 0;
  FVertexList.CheckIndexRange(aRoot);
  Visited.Size := VertexCount;
  {%H-}Stack.Push(aRoot);
  repeat
    if not Visited[aRoot] then
      begin
        Inc(Result);
        Visited[aRoot] := True;
        if aOnGray <> nil then
          aOnGray(aRoot);
        for aRoot in AdjVerticesI(aRoot) do
          if not Visited[aRoot] then
            begin
              Stack.Push(aRoot);
              if aOnWhite <> nil then
                aOnWhite(aRoot);
            end;
      end;
  until not Stack.TryPop(aRoot);
end;

function TGCustomSimpleSparseGraph.BSFTraversal(constref aRoot: TVertex; aOnGray: TOnVertexVisit;
  aOnWhite: TOnVertexVisit): SizeInt;
begin
  Result := BSFTraversalI(FVertexList.IndexOf(aRoot), aOnGray, aOnWhite);
end;

function TGCustomSimpleSparseGraph.BSFTraversalI(aRoot: SizeInt; aOnGray: TOnVertexVisit;
  aOnWhite: TOnVertexVisit): SizeInt;
var
  Visited: TBitVector;
  Queue: TIntQueue;
begin
  Result := 0;
  FVertexList.CheckIndexRange(aRoot);
  Visited.Size := VertexCount;
  Queue.Enqueue(aRoot);
  while Queue.TryDequeue(aRoot) do
    if not Visited[aRoot] then
      begin
        Inc(Result);
        Visited[aRoot] := True;
        if aOnGray <> nil then
          aOnGray(aRoot);
        for aRoot in AdjVerticesI(aRoot) do
          if not Visited[aRoot] then
            begin
              Queue.Enqueue(aRoot);
              if aOnWhite <> nil then
                aOnWhite(aRoot);
            end;
      end;
end;

function TGCustomSimpleSparseGraph.BSFTraversal(constref aRoot: TVertex; aOnGray,
  aOnWhite: TNestVertexVisit): SizeInt;
begin
  Result := BSFTraversalI(FVertexList.IndexOf(aRoot), aOnGray, aOnWhite);
end;

function TGCustomSimpleSparseGraph.BSFTraversalI(aRoot: SizeInt; aOnGray, aOnWhite: TNestVertexVisit): SizeInt;
var
  Visited: TBitVector;
  Queue: TIntQueue;
begin
  Result := 0;
  FVertexList.CheckIndexRange(aRoot);
  Visited.Size := VertexCount;
  Queue.Enqueue(aRoot);
  while Queue.TryDequeue(aRoot) do
    if not Visited[aRoot] then
      begin
        Inc(Result);
        Visited[aRoot] := True;
        if aOnGray <> nil then
          aOnGray(aRoot);
        for aRoot in AdjVerticesI(aRoot) do
          if not Visited[aRoot] then
            begin
              Queue.Enqueue(aRoot);
              if aOnWhite <> nil then
                aOnWhite(aRoot);
            end;
      end;
end;

function TGCustomSimpleSparseGraph.SimplePathExists(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := SimplePathExistsI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGCustomSimpleSparseGraph.SimplePathExistsI(aSrc, aDst: SizeInt): Boolean;
var
  Visited: TBitVector;
  Stack: TIntStack;
begin
  if (aSrc = aDst) or (VertexCount < 2) then
    exit(False);
  Visited.Size := VertexCount;
  {%H-}Stack.Push(aSrc);
  repeat
    if not Visited[aSrc] then
      begin
        if AdjacentI(aSrc, aDst) then
           exit(True);
        Visited[aSrc] := True;
        for aSrc in AdjVerticesI(aSrc) do
          if not Visited[aSrc] then
            Stack.Push(aSrc);
      end;
  until not Stack.TryPop(aSrc);
  Result := False;
end;

end.

