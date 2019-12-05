{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Some variants of the generic treap.                                     *
*                                                                           *
*   Copyright(c) 2019 A.Koverdyaev(avk)                                     *
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
unit LGTreap;

{$MODE OBJFPC}{$H+}
{$INLINE ON}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  SysUtils, Math,
  LGUtils,
  {%H-}LGHelpers,
  LGStrConst;

type
  { TGBstUtil }
  generic TGBstUtil<TKey, TNode, TCmpRel> = class
  public
  type
    PNode      = ^TNode;
    TOnVisit   = procedure(aNode: PNode; var aGoOn: Boolean) of object;
    TNestVisit = procedure(aNode: PNode; var aGoOn: Boolean) is nested;

    class function  GetTreeSize(aNode: PNode): SizeInt; static;
    class function  GetHeight(aNode: PNode): SizeInt; static;
    class function  GetLowest(aRoot: PNode): PNode; static;
    class function  GetHighest(aRoot: PNode): PNode; static;
    class function  FindKey(aRoot: PNode; constref aKey: TKey): PNode; static;
    class function  GetLess(aRoot: PNode; constref aKey: TKey): PNode; static;
    class function  GetLessOrEqual(aRoot: PNode; constref aKey: TKey): PNode;static;
    class function  GetGreater(aRoot: PNode; constref aKey: TKey): PNode; static;
    class function  GetGreaterOrEqual(aRoot: PNode; constref aKey: TKey): PNode; static;
    class procedure ClearTree(aNode: PNode); static;
    class procedure FreeNode(aNode: PNode); static; inline;
    class function  PreOrderTraversal(aRoot: PNode; aOnVisit: TOnVisit): SizeInt; static;
    class function  PreOrderTraversal(aRoot: PNode; aOnVisit: TNestVisit): SizeInt; static;
    class function  InOrderTraversal(aRoot: PNode; aOnVisit: TOnVisit): SizeInt; static;
    class function  InOrderTraversal(aRoot: PNode; aOnVisit: TNestVisit): SizeInt; static;
    class function  PostOrderTraversal(aRoot: PNode; aOnVisit: TOnVisit): SizeInt; static;
    class function  PostOrderTraversal(aRoot: PNode; aOnVisit: TNestVisit): SizeInt; static;
  end;

  { TGIndexedBstUtil }
  generic TGIndexedBstUtil<TKey, TNode, TCmpRel> = class(specialize TGBstUtil<TKey, TNode, TCmpRel>)
  public
    class function GetNodeSize(aNode: PNode): SizeInt; static; inline;
    class function GetByIndex(aRoot: PNode; aIndex: SizeInt): PNode; static;
    class function GetKeyIndex(aRoot: PNode; constref aKey: TKey): SizeInt;
  end;

  { TGLiteTreap: BST only;
    on assignment and when passed by value, the whole treap is copied;
      functor TCmpRel (comparision relation) must provide:
        class function Compare([const[ref]] L, R: TKey): SizeInt; }
  generic TGLiteTreap<TKey, TValue, TCmpRel> = record
  public
  type
    PNode = ^TNode;
    TNode = record
    private
      FLeft,
      FRight: PNode;
      FKey: TKey;
      FPrio: SizeUInt;
    public
      Value: TValue;
      property Left: PNode read FLeft;
      property Right: PNode read FRight;
      property Key: TKey read FKey;
    end;

    TUtil       = specialize TGBstUtil<TKey, TNode, TCmpRel>;
    TOnVisit    = TUtil.TOnVisit;
    TNestVisit  = TUtil.TNestVisit;
    TEntry      = specialize TGMapEntry<TKey, TValue>;
    TEntryArray = array of TEntry;

  private
    FRoot: PNode;
    function  GetCount: SizeInt;
    function  GetHeight: SizeInt;
    class function  NewNode(constref aKey: TKey): PNode; static;
    class function  CopyTree(aRoot: PNode): PNode; static;
  { splits tree of aRoot into two subtrees, where min(R.Key) >= aKey }
    class procedure SplitNode(constref aKey: TKey; aRoot: PNode; out L, R: PNode); static;
    class function  MergeNode(L, R: PNode): PNode; static;
    class procedure AddNode(var aRoot: PNode; aNode: PNode); static;
    class function  RemoveNode(constref aKey: TKey; var aRoot: PNode): Boolean; static;
    class operator  Initialize(var aTreap: TGLiteTreap);
    class operator  Finalize(var aTreap: TGLiteTreap);
    class operator  Copy(constref aSrc: TGLiteTreap; var aDst: TGLiteTreap);
    class operator  AddRef(var aTreap: TGLiteTreap);
  public
    { splits aTreap so that L will contain all keys < aKey and  R will contain all keys >= aKey;
      aTreap becomes empty }
    class procedure Split(constref aKey: TKey; var aTreap: TGLiteTreap; out L, R: TGLiteTreap); static;
    function  IsEmpty: Boolean; inline;
    procedure Clear;
    function  ToArray: TEntryArray;         //O(N)
    function  Find(constref aKey: TKey): PNode;
    function  CountOf(constref aKey: TKey): SizeInt;//O(N)
    function  Add(constref aKey: TKey): PNode;
    function  Remove(constref aKey: TKey): Boolean;
  { splits treap so that result will contain all elements with keys >= aKey }
    procedure Split(constref aKey: TKey; out aTreap: TGLiteTreap);
    property  Root: PNode read FRoot;
    property  Count: SizeInt read GetCount;  //O(N)
    property  Height: SizeInt read GetHeight;//O(N)
  end;

  { TGLiteIdxTreap: BST which allows index access(IOW rank and N-th order statistics)
    on assignment and when passed by value, the whole treap is copied;
      functor TCmpRel (comparision relation) must provide:
        class function Compare([const[ref]] L, R: TKey): SizeInt; }
  generic TGLiteIdxTreap<TKey, TValue, TCmpRel> = record
  public
  type
    PNode = ^TNode;
    TNode = record
    private
      FLeft,
      FRight: PNode;
      FKey: TKey;
      FPrio: SizeUInt;
      FSize: SizeInt;
    public
      Value: TValue;
      property Left: PNode read FLeft;
      property Right: PNode read FRight;
      property Key: TKey read FKey;
      property Size: SizeInt read FSize;
    end;

    TUtil       = specialize TGIndexedBstUtil<TKey, TNode, TCmpRel>;
    TOnVisit    = TUtil.TOnVisit;
    TNestVisit  = TUtil.TNestVisit;
    TEntry      = specialize TGMapEntry<TKey, TValue>;
    TEntryArray = array of TEntry;

  private
    FRoot: PNode;
    function  GetCount: SizeInt; inline;
    function  GetHeight: SizeInt;
    function  GetItem(aIndex: SizeInt): PNode; inline;
    procedure CheckIndexRange(aIndex: SizeInt); inline;
    class function  NewNode(constref aKey: TKey): PNode; static;
    class function  CopyTree(aRoot: PNode): PNode; static;
    class procedure UpdateSize(aNode: PNode); static; inline;
    class procedure SplitNode(constref aKey: TKey; aRoot: PNode; out L, R: PNode); static;
    class function  MergeNode(L, R: PNode): PNode; static;
    class procedure AddNode(var aRoot: PNode; aNode: PNode); static;
    class function  RemoveNode(constref aKey: TKey; var aRoot: PNode): Boolean; static;
    class operator  Initialize(var aTreap: TGLiteIdxTreap);
    class operator  Finalize(var aTreap: TGLiteIdxTreap);
    class operator  Copy(constref aSrc: TGLiteIdxTreap; var aDst: TGLiteIdxTreap);
    class operator  AddRef(var aTreap: TGLiteIdxTreap); inline;
  public
  { splits aTreap so that L will contain all keys < aKey and  R will contain all keys >= aKey;
    aTreap becomes empty }
    class procedure Split(constref aKey: TKey; var aTreap: TGLiteIdxTreap; out L, R: TGLiteIdxTreap); static;
    function  IsEmpty: Boolean; inline;
    procedure Clear;
    function  ToArray: TEntryArray;         //O(N)
    function  Find(constref aKey: TKey): PNode;
    function  IndexOf(constref aKey: TKey): SizeInt; inline;
    function  CountOf(constref aKey: TKey): SizeInt;
    function  Add(constref aKey: TKey): PNode;
    function  Remove(constref aKey: TKey): Boolean;
  { splits treap so that result will contain all elements with keys >= aKey }
    procedure Split(constref aKey: TKey; out aTreap: TGLiteIdxTreap);
    property  Root: PNode read FRoot;
    property  Count: SizeInt read GetCount;
    property  Height: SizeInt read GetHeight; //O(N)
    property  Items[aIndex: SizeInt]: PNode read GetItem; default;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGBstUtil }

class function TGBstUtil.GetTreeSize(aNode: PNode): SizeInt;
var
  Size: SizeInt = 0;
  procedure Traverse(aNode: PNode);
  begin
    if aNode <> nil then
      begin
        Inc(Size);
        Traverse(aNode^.Left);
        Traverse(aNode^.Right);
      end;
  end;
begin
  Traverse(aNode);
  Result := Size;
end;

class function TGBstUtil.GetHeight(aNode: PNode): SizeInt;
begin
  if aNode <> nil then
    Result := Math.Max(Succ(GetHeight(aNode^.Left)), Succ(GetHeight(aNode^.Right)))
  else
    Result := 0;
end;

class function TGBstUtil.GetLowest(aRoot: PNode): PNode;
begin
  Result := aRoot;
  if Result <> nil then
    while Result^.Left <> nil do
      Result := Result^.Left;
end;

class function TGBstUtil.GetHighest(aRoot: PNode): PNode;
begin
  Result := aRoot;
  if Result <> nil then
    while Result^.Right <> nil do
      Result := Result^.Right;
end;

class function TGBstUtil.FindKey(aRoot: PNode; constref aKey: TKey): PNode;
begin
  Result := aRoot;
  while Result <> nil do
    case SizeInt(TCmpRel.Compare(aKey, Result^.Key)) of
      System.Low(SizeInt)..-1: Result := Result^.Left;
      1..System.High(SizeInt): Result := Result^.Right;
    else
      exit;
    end;
end;

class function TGBstUtil.GetLess(aRoot: PNode; constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := aRoot;
  Result := nil;
  while Node <> nil do
    if TCmpRel.Compare(aKey, Node^.Key) > 0 then
      begin
        Result := Node;
        Node := Node^.Right;
      end
    else
      Node := Node^.Left;
end;

class function TGBstUtil.GetLessOrEqual(aRoot: PNode; constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := aRoot;
  Result := nil;
  while Node <> nil do
    if TCmpRel.Compare(aKey, Node^.Key) >= 0 then
      begin
        Result := Node;
        Node := Node^.Right;
      end
    else
      Node := Node^.Left;
end;

class function TGBstUtil.GetGreater(aRoot: PNode; constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := aRoot;
  Result := nil;
  while Node <> nil do
    if TCmpRel.Compare(aKey, Node^.Key) < 0 then
      begin
        Result := Node;
        Node := Node^.Left;
      end
    else
      Node := Node^.Right;
end;

class function TGBstUtil.GetGreaterOrEqual(aRoot: PNode; constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := aRoot;
  Result := nil;
  while Node <> nil do
    if TCmpRel.Compare(aKey, Node^.Key) <= 0 then
      begin
        Result := Node;
        Node := Node^.Left;
      end
    else
      Node := Node^.Right;
end;

class procedure TGBstUtil.ClearTree(aNode: PNode);
begin
  if aNode <> nil then
    begin
      ClearTree(aNode^.Left);
      ClearTree(aNode^.Right);
      //if IsManagedType(TNode) then
        aNode^ := Default(TNode);
      System.FreeMem(aNode);
    end;
end;

class procedure TGBstUtil.FreeNode(aNode: PNode);
begin
  //if IsManagedType(TNode) then
    aNode^ := Default(TNode);
  System.FreeMem(aNode);
end;

class function TGBstUtil.PreOrderTraversal(aRoot: PNode; aOnVisit: TOnVisit): SizeInt;
var
  Visited: SizeInt = 0;
  Goon: Boolean = True;

  procedure Visit(aNode: PNode);
  begin
    if (aNode <> nil) and Goon then
      begin
        if aOnVisit <> nil then
          begin
            aOnVisit(aNode, Goon);
            Inc(Visited);
          end;
        Visit(aNode^.Left);
        Visit(aNode^.Right);
      end;
  end;

begin
  Visit(aRoot);
  Result := Visited;
end;

class function TGBstUtil.PreOrderTraversal(aRoot: PNode; aOnVisit: TNestVisit): SizeInt;
var
  Visited: SizeInt = 0;
  Goon: Boolean = True;

  procedure Visit(aNode: PNode);
  begin
    if (aNode <> nil) and Goon then
      begin
        if aOnVisit <> nil then
          begin
            aOnVisit(aNode, Goon);
            Inc(Visited);
          end;
        Visit(aNode^.Left);
        Visit(aNode^.Right);
      end;
  end;

begin
  Visit(aRoot);
  Result := Visited;
end;

class function TGBstUtil.InOrderTraversal(aRoot: PNode; aOnVisit: TOnVisit): SizeInt;
var
  Visited: SizeInt = 0;
  Goon: Boolean = True;

  procedure Visit(aNode: PNode);
  begin
    if (aNode <> nil) and Goon then
      begin
        Visit(aNode^.Left);
        if (aOnVisit <> nil) and Goon then
          begin
            aOnVisit(aNode, Goon);
            Inc(Visited);
          end;
        Visit(aNode^.Right);
      end;
  end;

begin
  Visit(aRoot);
  Result := Visited;
end;

class function TGBstUtil.InOrderTraversal(aRoot: PNode; aOnVisit: TNestVisit): SizeInt;
var
  Visited: SizeInt = 0;
  Goon: Boolean = True;

  procedure Visit(aNode: PNode);
  begin
    if (aNode <> nil) and Goon then
      begin
        Visit(aNode^.Left);
        if (aOnVisit <> nil) and Goon then
          begin
            aOnVisit(aNode, Goon);
            Inc(Visited);
          end;
        Visit(aNode^.Right);
      end;
  end;

begin
  Visit(aRoot);
  Result := Visited;
end;

class function TGBstUtil.PostOrderTraversal(aRoot: PNode; aOnVisit: TOnVisit): SizeInt;
var
  Visited: SizeInt = 0;
  Goon: Boolean = True;

  procedure Visit(aNode: PNode);
  begin
    if (aNode <> nil) and Goon then
      begin
        Visit(aNode^.Left);
        Visit(aNode^.Right);
        if (aOnVisit <> nil) and Goon then
          begin
            aOnVisit(aNode, Goon);
            Inc(Visited);
          end;
      end;
  end;

begin
  Visit(aRoot);
  Result := Visited;
end;

class function TGBstUtil.PostOrderTraversal(aRoot: PNode; aOnVisit: TNestVisit): SizeInt;
var
  Visited: SizeInt = 0;
  Goon: Boolean = True;

  procedure Visit(aNode: PNode);
  begin
    if (aNode <> nil) and Goon then
      begin
        Visit(aNode^.Left);
        Visit(aNode^.Right);
        if (aOnVisit <> nil) and Goon then
          begin
            aOnVisit(aNode, Goon);
            Inc(Visited);
          end;
      end;
  end;

begin
  Visit(aRoot);
  Result := Visited;
end;

{ TGIndexedBstUtil }

class function TGIndexedBstUtil.GetNodeSize(aNode: PNode): SizeInt;
begin
  if aNode <> nil then
    Result := aNode^.Size
  else
    Result := 0;
end;

class function TGIndexedBstUtil.GetByIndex(aRoot: PNode; aIndex: SizeInt): PNode;
var
  LSize: SizeInt;
begin
  Result := aRoot;
  while Result <> nil do
    begin
      LSize := GetNodeSize(Result^.Left);
      if LSize = aIndex then
        exit
      else
        if LSize > aIndex then
          Result := Result^.Left
        else
          begin
            Result := Result^.Right;
            aIndex -= Succ(LSize);
          end;
    end;
end;

class function TGIndexedBstUtil.GetKeyIndex(aRoot: PNode; constref aKey: TKey): SizeInt;
var
  Pos: SizeInt = 0;
begin
  while aRoot <> nil do
    case SizeInt(TCmpRel.Compare(aKey, aRoot^.Key)) of
      System.Low(SizeInt)..-1: aRoot := aRoot^.Left;
      1..System.High(SizeInt):
        begin
          Pos += Succ(GetNodeSize(aRoot^.Left));
          aRoot := aRoot^.Right;
        end;
    else
      exit(Pos + GetNodeSize(aRoot^.Left));
    end;
  Result := NULL_INDEX;
end;

{ TGLiteTreap }

function TGLiteTreap.GetCount: SizeInt;
begin
  if FRoot <> nil then
    exit(TUtil.GetTreeSize(FRoot));
  Result := 0;
end;

function TGLiteTreap.GetHeight: SizeInt;
begin
  Result := TUtil.GetHeight(FRoot);
end;

class function TGLiteTreap.NewNode(constref aKey: TKey): PNode;
begin
  Result := System.GetMem(SizeOf(TNode));
  System.FillChar(Result^, SizeOf(TNode), 0);
  Result^.FKey := aKey;
  Result^.FPrio := {$IFDEF CPU64}BJNextRandom64{$ELSE}BJNextRandom{$ENDIF};
end;

class function TGLiteTreap.CopyTree(aRoot: PNode): PNode;
var
  Tmp: TGLiteTreap;
  procedure Visit(aNode: PNode);
  begin
    if aNode <> nil then
      begin
        Visit(aNode^.FLeft);
        Tmp.Add(aNode^.Key)^.Value := aNode^.Value;
        Visit(aNode^.FRight);
      end;
  end;
begin
  if aRoot <> nil then
    begin
      Visit(aRoot);
      Result := {%H-}Tmp.FRoot;
      Tmp.FRoot := nil;
    end
  else
    Result := nil;
end;

class procedure TGLiteTreap.SplitNode(constref aKey: TKey; aRoot: PNode; out L, R: PNode);
begin
  if aRoot <> nil then
    begin
      if TCmpRel.Compare(aRoot^.Key, aKey) < 0 then
        begin
          L := aRoot;
          SplitNode(aKey, L^.FRight, L^.FRight, R);
        end
      else
        begin
          R := aRoot;
          SplitNode(aKey, R^.FLeft, L, R^.FLeft);
        end;
      exit;
    end;
  L := nil;
  R := nil;
end;

class function TGLiteTreap.MergeNode(L, R: PNode): PNode;
begin
  if L = nil then
    Result := R
  else
    if R = nil then
      Result := L
    else
      if L^.FPrio > R^.FPrio then
        begin
          L^.FRight := MergeNode(L^.FRight, R);
          Result := L;
        end
      else
        begin
          R^.FLeft := MergeNode(L, R^.FLeft);
          Result := R;
        end;
end;

class procedure TGLiteTreap.AddNode(var aRoot: PNode; aNode: PNode);
begin
  if aRoot <> nil then
    begin
      if aRoot^.FPrio < aNode^.FPrio then
        begin
          SplitNode(aNode^.Key, aRoot, aNode^.FLeft, aNode^.FRight);
          aRoot := aNode;
        end
      else
        if TCmpRel.Compare(aNode^.Key, aRoot^.Key) < 0 then
          AddNode(aRoot^.FLeft, aNode)
        else
          AddNode(aRoot^.FRight, aNode);
    end
  else
    aRoot := aNode;
end;


class function TGLiteTreap.RemoveNode(constref aKey: TKey; var aRoot: PNode): Boolean;
var
  Found: PNode;
begin
  if aRoot <> nil then
    case SizeInt(TCmpRel.Compare(aKey, aRoot^.Key)) of
      System.Low(SizeInt)..-1: exit(RemoveNode(aKey, aRoot^.FLeft));
      1..System.High(SizeInt): exit(RemoveNode(aKey, aRoot^.FRight));
    else
      Found := aRoot;
      aRoot := MergeNode(aRoot^.FLeft, aRoot^.FRight);
      TUtil.FreeNode(Found);
      exit(True);
    end;
  Result := False;
end;

class operator TGLiteTreap.Initialize(var aTreap: TGLiteTreap);
begin
  aTreap.FRoot := nil;
end;

class operator TGLiteTreap.Finalize(var aTreap: TGLiteTreap);
begin
  aTreap.Clear;
end;

class operator TGLiteTreap.Copy(constref aSrc: TGLiteTreap; var aDst: TGLiteTreap);
begin
  aDst.Clear;
  if aSrc.FRoot <> nil then
    aDst.FRoot := CopyTree(aSrc.FRoot);
end;

class operator TGLiteTreap.AddRef(var aTreap: TGLiteTreap);
begin
  if aTreap.FRoot <> nil then
    aTreap.FRoot := CopyTree(aTreap.FRoot);
end;

class procedure TGLiteTreap.Split(constref aKey: TKey; var aTreap: TGLiteTreap; out L, R: TGLiteTreap);
begin
  if aTreap.FRoot = nil then
    exit;
  SplitNode(aKey, aTreap.FRoot, L.FRoot, R.FRoot);
  aTreap.FRoot := nil;
end;

function TGLiteTreap.IsEmpty: Boolean;
begin
  Result := FRoot = nil;
end;

procedure TGLiteTreap.Clear;
begin
  if FRoot <> nil then
    TUtil.ClearTree(FRoot);
  FRoot := nil;
end;

function TGLiteTreap.ToArray: TEntryArray;
var
  a: TEntryArray = nil;
  I: Integer = 0;
  procedure Visit(aNode: PNode);
  begin
    if aNode <> nil then
      begin
        Visit(aNode^.FLeft);
        if System.Length(a) = I then
          System.SetLength(a, I * 2);
        a[I] := TEntry.Create(aNode^.Key, aNode^.Value);
        Inc(I);
        Visit(aNode^.FRight);
      end;
  end;
begin
  if FRoot <> nil then
    begin
      System.SetLength(a, ARRAY_INITIAL_SIZE);
      Visit(FRoot);
      System.SetLength(a, I);
    end;
  Result := a;
end;

function TGLiteTreap.Find(constref aKey: TKey): PNode;
begin
  if FRoot <> nil then
    exit(TUtil.FindKey(FRoot, aKey));
  Result := nil;
end;

function TGLiteTreap.CountOf(constref aKey: TKey): SizeInt;
var
  L, M, R, Gt: PNode;
begin
  if FRoot <> nil then
    begin
      Gt := TUtil.GetGreater(FRoot, aKey);
      SplitNode(aKey, FRoot, L, R);
      if Gt <> nil then
        begin
          SplitNode(Gt^.Key, R, M, R);
          Result := TUtil.GetTreeSize(M);
          FRoot := MergeNode(MergeNode(L, M), R);
        end
      else
        begin
          Result := TUtil.GetTreeSize(R);
          FRoot := MergeNode(L, R);
        end;
    end
  else
    Result := 0;
end;

function TGLiteTreap.Add(constref aKey: TKey): PNode;
begin
  Result := NewNode(aKey);
  if FRoot <> nil then
    AddNode(FRoot, Result)
  else
    FRoot := Result;
end;

function TGLiteTreap.Remove(constref aKey: TKey): Boolean;
begin
  if FRoot <> nil then
    Result := RemoveNode(aKey, FRoot)
  else
    Result := False;
end;

procedure TGLiteTreap.Split(constref aKey: TKey; out aTreap: TGLiteTreap);
begin
  if FRoot <> nil then
    SplitNode(aKey, FRoot, FRoot, aTreap.FRoot);
end;

{ TGLiteIdxTreap }

function TGLiteIdxTreap.GetCount: SizeInt;
begin
  Result := TUtil.GetNodeSize(FRoot);
end;

function TGLiteIdxTreap.GetHeight: SizeInt;
begin
  Result := TUtil.GetHeight(FRoot);
end;

function TGLiteIdxTreap.GetItem(aIndex: SizeInt): PNode;
begin
  Result := TUtil.GetByIndex(FRoot, aIndex);
end;

procedure TGLiteIdxTreap.CheckIndexRange(aIndex: SizeInt);
begin
  if SizeUInt(aIndex) >= SizeUInt(TUtil.GetNodeSize(FRoot)) then
    raise EArgumentOutOfRangeException.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
end;

class function TGLiteIdxTreap.NewNode(constref aKey: TKey): PNode;
begin
  Result := System.GetMem(SizeOf(TNode));
  System.FillChar(Result^, SizeOf(TNode), 0);
  Result^.FKey := aKey;
  Result^.FPrio := {$IFDEF CPU64}BJNextRandom64{$ELSE}BJNextRandom{$ENDIF};
  Result^.FSize := 1;
end;

class function TGLiteIdxTreap.CopyTree(aRoot: PNode): PNode;
var
  Tmp: TGLiteIdxTreap;
  procedure Visit(aNode: PNode);
  begin
    if aNode <> nil then
      begin
        Visit(aNode^.FLeft);
        Tmp.Add(aNode^.Key)^.Value := aNode^.Value;
        Visit(aNode^.FRight);
      end;
  end;
begin
  if aRoot <> nil then
    begin
      Visit(aRoot);
      Result := {%H-}Tmp.FRoot;
      Tmp.FRoot := nil;
    end
  else
    Result := nil;
end;

class procedure TGLiteIdxTreap.UpdateSize(aNode: PNode);
begin
  with aNode^ do
    begin
      FSize := 1;
      if Left <> nil then
        FSize += Left^.FSize;
      if Right <> nil then
        FSize += Right^.FSize;
    end;
end;

class procedure TGLiteIdxTreap.SplitNode(constref aKey: TKey; aRoot: PNode; out L, R: PNode);
begin
  if aRoot <> nil then
    begin
      if TCmpRel.Compare(aRoot^.Key, aKey) < 0 then
        begin
          L := aRoot;
          SplitNode(aKey, L^.FRight, L^.FRight, R);
        end
      else
        begin
          R := aRoot;
          SplitNode(aKey, R^.FLeft, L, R^.FLeft);
        end;
      UpdateSize(aRoot);
      exit;
    end;
  L := nil;
  R := nil;
end;

class function TGLiteIdxTreap.MergeNode(L, R: PNode): PNode;
begin
  if L = nil then
    Result := R
  else
    if R = nil then
      Result := L
    else
      begin
        if L^.FPrio > R^.FPrio then
          begin
            L^.FRight := MergeNode(L^.FRight, R);
            Result := L;
          end
        else
          begin
            R^.FLeft := MergeNode(L, R^.FLeft);
            Result := R;
          end;
        UpdateSize(Result);
      end;
end;

class procedure TGLiteIdxTreap.AddNode(var aRoot: PNode; aNode: PNode);
begin
  if aRoot <> nil then
    begin
      if aRoot^.FPrio < aNode^.FPrio then
        begin
          SplitNode(aNode^.Key, aRoot, aNode^.FLeft, aNode^.FRight);
          aRoot := aNode;
        end
      else
        if TCmpRel.Compare(aNode^.Key, aRoot^.Key) < 0 then
          AddNode(aRoot^.FLeft, aNode)
        else
          AddNode(aRoot^.FRight, aNode);
      UpdateSize(aRoot);
    end
  else
    aRoot := aNode;
end;

class function TGLiteIdxTreap.RemoveNode(constref aKey: TKey; var aRoot: PNode): Boolean;
var
  Found: PNode;
  c: SizeInt;
begin
  if aRoot <> nil then
    begin
      c := TCmpRel.Compare(aKey, aRoot^.Key);
      if c = 0 then
        begin
          Found := aRoot;
          aRoot := MergeNode(aRoot^.FLeft, aRoot^.FRight);
          TUtil.FreeNode(Found);
          Result := True;
        end
      else
        begin
          if c < 0 then
            Result := RemoveNode(aKey, aRoot^.FLeft)
          else
            Result := RemoveNode(aKey, aRoot^.FRight);
          if Result then
            UpdateSize(aRoot);
        end;
    end
  else
    Result := False;
end;

class operator TGLiteIdxTreap.Initialize(var aTreap: TGLiteIdxTreap);
begin
  aTreap.FRoot := nil;
end;

class operator TGLiteIdxTreap.Finalize(var aTreap: TGLiteIdxTreap);
begin
  aTreap.Clear;
end;

class operator TGLiteIdxTreap.Copy(constref aSrc: TGLiteIdxTreap; var aDst: TGLiteIdxTreap);
begin
  aDst.Clear;
  if aSrc.FRoot <> nil then
    aDst.FRoot := CopyTree(aSrc.FRoot);
end;

class operator TGLiteIdxTreap.AddRef(var aTreap: TGLiteIdxTreap);
begin
  if aTreap.FRoot <> nil then
    aTreap.FRoot := CopyTree(aTreap.FRoot);
end;

class procedure TGLiteIdxTreap.Split(constref aKey: TKey; var aTreap: TGLiteIdxTreap;
  out L, R: TGLiteIdxTreap);
begin
  if aTreap.FRoot = nil then
    exit;
  SplitNode(aKey, aTreap.FRoot, L.FRoot, R.FRoot);
  aTreap.FRoot := nil;
end;

function TGLiteIdxTreap.IsEmpty: Boolean;
begin
  Result := FRoot = nil;
end;

procedure TGLiteIdxTreap.Clear;
begin
  if FRoot <> nil then
    TUtil.ClearTree(FRoot);
  FRoot := nil;
end;

function TGLiteIdxTreap.ToArray: TEntryArray;
var
  a: TEntryArray = nil;
  I: Integer = 0;
  procedure Visit(aNode: PNode);
  begin
    if aNode <> nil then
      begin
        Visit(aNode^.FLeft);
        if System.Length(a) = I then
          System.SetLength(a, I * 2);
        a[I] := TEntry.Create(aNode^.Key, aNode^.Value);
        Inc(I);
        Visit(aNode^.FRight);
      end;
  end;
begin
  if FRoot <> nil then
    begin
      System.SetLength(a, ARRAY_INITIAL_SIZE);
      Visit(FRoot);
      System.SetLength(a, I);
    end;
  Result := a;
end;

function TGLiteIdxTreap.Find(constref aKey: TKey): PNode;
begin
  if FRoot <> nil then
    exit(TUtil.FindKey(FRoot, aKey));
  Result := nil;
end;

function TGLiteIdxTreap.IndexOf(constref aKey: TKey): SizeInt;
begin
  Result := TUtil.GetKeyIndex(FRoot, aKey);
end;

function TGLiteIdxTreap.CountOf(constref aKey: TKey): SizeInt;
var
  L, M, R, Gt: PNode;
begin
  if FRoot <> nil then
    begin
      Gt := TUtil.GetGreater(FRoot, aKey);
      SplitNode(aKey, FRoot, L, R);
      if Gt <> nil then
        begin
          SplitNode(Gt^.Key, R, M, R);
          Result := TUtil.GetNodeSize(M);
          FRoot := MergeNode(MergeNode(L, M), R);
        end
      else
        begin
          Result := TUtil.GetNodeSize(R);
          FRoot := MergeNode(L, R);
        end;
    end
  else
    Result := 0;
end;

function TGLiteIdxTreap.Add(constref aKey: TKey): PNode;
begin
  Result := NewNode(aKey);
  if FRoot <> nil then
    AddNode(FRoot, Result)
  else
    FRoot := Result;
end;

function TGLiteIdxTreap.Remove(constref aKey: TKey): Boolean;
begin
  if FRoot <> nil then
    Result := RemoveNode(aKey, FRoot)
  else
    Result := False;
end;

procedure TGLiteIdxTreap.Split(constref aKey: TKey; out aTreap: TGLiteIdxTreap);
begin
  if FRoot <> nil then
    SplitNode(aKey, FRoot, FRoot, aTreap.FRoot);
end;

end.
