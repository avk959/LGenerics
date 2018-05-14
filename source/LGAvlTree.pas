{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic AVL tree implementations for internal use.                      *
*   (see https://en.wikipedia.org/wiki/AVL_tree)                            *
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
unit LGAvlTree;

{$mode objfpc}{$H+}
{$INLINE ON}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}
{$PACKRECORDS DEFAULT}

interface

uses

  SysUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGStrConst;

{.$DEFINE AVLTREE_ENABLE_PAGEDNODEMANAGER}//if uncomment define, will use TGPageNodeManager
type

  { TGCustomAvlTree }

  generic TGCustomAvlTree<TKey, TEntry> = class abstract
  public
  type
    PNode = ^TNode;

    TNode = record
    private
    {$IFNDEF CPU16}
      Left,
      Right: PNode;
      FParent: SizeInt;
      function  GetBalance: SizeInt; inline;
      function  GetParent: PNode; inline;
      procedure SetBalance(aValue: SizeInt); inline;
      procedure SetParent(aValue: PNode); inline;
      procedure SwapBalance(aNode: PNode); inline;
      property  Parent: PNode read GetParent write SetParent;
      property  Balance: SizeInt read GetBalance write SetBalance;
    {$ELSE !CPU16}
      Left,
      Right,
      Parent: PNode;
      Balance: SizeInt;
      procedure SwapBalance(aNode: PNode); inline;
    {$ENDIF !CPU16}
      //for node manager
      property  NextLink: PNode read Left write Left;
    public
      Data: TEntry;
      function Successor: PNode;
      function Predecessor: PNode;
    end;
{$IFDEF AVLTREE_ENABLE_PAGEDNODEMANAGER}
    TNodeManager = specialize TGPageNodeManager<TNode>;
{$ELSE AVLTREE_ENABLE_PAGEDNODEMANAGER}
    TNodeManager = specialize TGNodeManager<TNode>;
{$ENDIF AVLTREE_ENABLE_PAGEDNODEMANAGER}
    PEntry      = ^TEntry;
    TTest       = specialize TGTest<TKey>;
    TOnTest     = specialize TGOnTest<TKey>;
    TNestTest   = specialize TGNestTest<TKey>;
    TEntryTest  = function(p: PEntry): Boolean of object;
    TEntryEvent = procedure(p: PEntry) of object;

    TEnumerator = class
    protected
      FCurrNode: PNode;
      FInCycle: Boolean;
      FTree: TGCustomAvlTree;
    public
      constructor Create(aTree: TGCustomAvlTree);
      function  MoveNext: Boolean; virtual;
      procedure Reset;
      property  Current: PNode read FCurrNode;
    end;

  protected
  type
    TReverseEnumerator = class(TEnumerator)
      function  MoveNext: Boolean; override;
    end;

    TEnumeratorAt = class(TEnumerator)
    public
      FRootNode: PNode;
      constructor Create(aRootNode: PNode);
      function  MoveNext: Boolean; override;
    end;

  var
    FNodeManager: TNodeManager;
    FRoot: PNode;
    FCount: SizeInt;
    function  NewNode: PNode;
    procedure DisposeNode(aNode: PNode); inline;
    function  GetCapacity: SizeInt; inline;
    procedure ClearTree;
    function  GetHighest: PNode;
    function  GetLowest: PNode;
    function  FindNode(constref aKey: TKey; out aInsertPos: PNode): PNode; virtual; abstract;
    function  FindInsertPos(constref aKey: TKey): PNode; virtual; abstract;
    procedure InsertNode(aNode: PNode); virtual; abstract;
    procedure InsertNodeAt(aNode, aParent: PNode); virtual; abstract;
    procedure ReplaceWithSuccessor(aNode: PNode);
    procedure DoRemoveNode(aNode: PNode);
    procedure BalanceAfterInsert(aNode: PNode);
    procedure BalanceAfterRemove(aNode: PNode; aNodeBalance: SizeInt);
    procedure RotateLeft(aNode: PNode);
    procedure RotateRight(aNode: PNode);
    procedure RemoveNodeWithChilds(aNode: PNode);
  public
    constructor Create;
    constructor Create(aCapacity: SizeInt);
    destructor Destroy; override;
    procedure Clear; inline;
    procedure EnsureCapacity(aValue: SizeInt);
    procedure TrimToFit; inline;
    function  FindOrAdd(constref aKey: TKey; out aNode: PNode): Boolean;
    function  Add(constref aData: TEntry): PNode; inline;
    function  Remove(constref aKey: TKey): Boolean;
    procedure RemoveNode(aNode: PNode); inline;
    function  GetEnumerator: TEnumerator; inline;
    function  GetReverseEnumerator: TEnumerator; inline;
    function  GetEnumeratorAt(constref aKey: TKey; aInclusive: Boolean): TEnumeratorAt; inline;
    function  RemoveIf(aTest: TTest; aOnRemove: TEntryEvent = nil): SizeInt;
    function  RemoveIf(aTest: TOnTest; aOnRemove: TEntryEvent = nil): SizeInt;
    function  RemoveIf(aTest: TNestTest; aOnRemove: TEntryEvent = nil): SizeInt;
    function  RemoveIf(aTest: TEntryTest; aOnRemove: TEntryEvent = nil): SizeInt;
    function  Find(constref aKey: TKey): PNode; virtual; abstract;
    function  FindLess(constref aKey: TKey): PNode; virtual; abstract;
    function  FindLessOrEqual(constref aKey: TKey): PNode; virtual; abstract;
    function  FindGreater(constref aKey: TKey): PNode; virtual; abstract;
    function  FindGreaterOrEqual(constref aKey: TKey): PNode; virtual; abstract;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
    property  Lowest: PNode read GetLowest;
    property  Highest: PNode read GetHighest;
  end;

  { TGAvlTree
      functor TKeyCmpRel (key comparision relation) must provide:
        class function Compare([const[ref]] L, R: TKey): SizeInt;  }
  generic TGAvlTree<TKey, TEntry, TKeyCmpRel> = class(specialize TGCustomAvlTree<TKey, TEntry>)
  protected
    function  FindNode(constref aKey: TKey; out aInsertPos: PNode): PNode; override;
    function  FindInsertPos(constref aKey: TKey): PNode; override;
    procedure InsertNode(aNode: PNode); override;
    procedure InsertNodeAt(aNode, aParent: PNode); override;
  public
    function  Clone: TGAvlTree;
    function  Find(constref aKey: TKey): PNode; override;
    function  FindLess(constref aKey: TKey): PNode; override;
    function  FindLessOrEqual(constref aKey: TKey): PNode; override;
    function  FindGreater(constref aKey: TKey): PNode; override;
    function  FindGreaterOrEqual(constref aKey: TKey): PNode; override;
  end;

  { TGComparableAvlTree assumes that type TKey has defined comparision operators }
  generic TGComparableAvlTree<TKey, TEntry> = class(specialize TGCustomAvlTree<TKey, TEntry>)
  protected
    function  FindNode(constref aKey: TKey; out aInsertPos: PNode): PNode; override;
    function  FindInsertPos(constref aKey: TKey): PNode; override;
    procedure InsertNode(aNode: PNode); override;
    procedure InsertNodeAt(aNode, aParent: PNode); override;
  public
    function  Clone: TGComparableAvlTree;
    function  Find(constref aKey: TKey): PNode; override;
    function  FindLess(constref aKey: TKey): PNode; override;
    function  FindLessOrEqual(constref aKey: TKey): PNode; override;
    function  FindGreater(constref aKey: TKey): PNode; override;
    function  FindGreaterOrEqual(constref aKey: TKey): PNode; override;
  end;

  { TGRegularAvlTree is avl tree with regular comparator }
  generic TGRegularAvlTree<TKey, TEntry> = class(specialize TGCustomAvlTree<TKey, TEntry>)
  public
  type
    TCompare = specialize TGCompare<TKey>;
  private
    FCompare: TCompare;
  protected
    function  FindNode(constref aKey: TKey; out aInsertPos: PNode): PNode; override;
    function  FindInsertPos(constref aKey: TKey): PNode; override;
    procedure InsertNode(aNode: PNode); override;
    procedure InsertNodeAt(aNode, aParent: PNode); override;
  public
    constructor Create(aCompare: TCompare);
    constructor Create(aCapacity: SizeInt; aCompare: TCompare);
    function  Clone: TGRegularAvlTree;
    function  Find(constref aKey: TKey): PNode; override;
    function  FindLess(constref aKey: TKey): PNode; override;
    function  FindLessOrEqual(constref aKey: TKey): PNode; override;
    function  FindGreater(constref aKey: TKey): PNode; override;
    function  FindGreaterOrEqual(constref aKey: TKey): PNode; override;
    property  Comparator: TCompare read FCompare;
  end;

  { TGDelegatedAvlTree is avl tree with delegated comparator }
  generic TGDelegatedAvlTree<TKey, TEntry> = class(specialize TGCustomAvlTree<TKey, TEntry>)
  public
  type
    TOnCompare = specialize TGOnCompare<TKey>;
  private
    FCompare: TOnCompare;
  protected
    function  FindNode(constref aKey: TKey; out aInsertPos: PNode): PNode; override;
    function  FindInsertPos(constref aKey: TKey): PNode; override;
    procedure InsertNode(aNode: PNode); override;
    procedure InsertNodeAt(aNode, aParent: PNode); override;
  public
    constructor Create(aCompare: TOnCompare);
    constructor Create(aCapacity: SizeInt; aCompare: TOnCompare);
    function  Clone: TGDelegatedAvlTree;
    function  Find(constref aKey: TKey): PNode; override;
    function  FindLess(constref aKey: TKey): PNode; override;
    function  FindLessOrEqual(constref aKey: TKey): PNode; override;
    function  FindGreater(constref aKey: TKey): PNode; override;
    function  FindGreaterOrEqual(constref aKey: TKey): PNode; override;
    property  Comparator: TOnCompare read FCompare;
  end;

  generic TGAvlTreeNode<T> = record
  private
  type
    TAvlTreeNode = specialize TGAvlTreeNode<T>;
  public
  type
    PNode = ^TAvlTreeNode;
  private
  {$IFNDEF CPU16}
    Left,
    Right: PNode;
    FParent: SizeInt;
    function  GetBalance: SizeInt; inline;
    function  GetParent: PNode; inline;
    procedure SetBalance(aValue: SizeInt); inline;
    procedure SetParent(aValue: PNode); inline;
    procedure SwapBalance(aNode: PNode); inline;
  {$ELSE !CPU16}
    Left,
    Right,
    Parent: PNode;
    Balance: SizeInt;
    procedure SwapBalance(aNode: PNode); inline;
  {$ENDIF !CPU16}
  public
    Data: T;
    function Successor: PNode;
    function Predecessor: PNode;
    property Parent: PNode read GetParent write SetParent;
    property Balance: SizeInt read GetBalance write SetBalance;
    //for node manager
    property NextLink: PNode read Left write Left;
  end;

  { TGAvlTree2: simplified version TGAvlTree }
  generic TGAvlTree2<TKey, TEntry, TNodeManager, TKeyCmpRel> = class
  public
  type
    TNode  = specialize TGAvlTreeNode<TEntry>;
    PNode  = ^TNode;
    PEntry = ^TEntry;

    TEnumerator = record
    private
      FCurrNode: PNode;
      FInCycle: Boolean;
      FTree: TGAvlTree2;
      function  GetCurrent: PEntry; inline;
      procedure Init(aTree: TGAvlTree2);
    public
      function  MoveNext: Boolean;
      procedure Reset; inline;
      property  Current: PEntry read GetCurrent;
    end;

  strict protected
  type
    PNodeManager = ^TNodeManager;

  var
    FRoot: PNode;
    FCount: SizeInt;
    FNodeManager: TNodeManager;
    function  NewNode: PNode;
    procedure DisposeNode(aNode: PNode); inline;
    procedure ClearTree; inline;
    function  GetHighest: PNode;
    function  GetLowest: PNode;
    function  FindNode(constref aKey: TKey; out aInsertPos: PNode): PNode;
    function  FindInsertPos(constref aKey: TKey): PNode;
    procedure InsertNode(aNode: PNode);
    procedure InsertNodeAt(aNode, aParent: PNode);
    procedure ReplaceWithSuccessor(aNode: PNode);
    procedure DoRemoveNode(aNode: PNode);
    procedure BalanceAfterInsert(aNode: PNode);
    procedure BalanceAfterRemove(aNode: PNode; aNewBalance: SizeInt);
    procedure RotateLeft(aNode: PNode);
    procedure RotateRight(aNode: PNode);
    procedure RemoveNodeWithChilds(aNode: PNode);
  public
    constructor Create(aNodeManager: TNodeManager);
    destructor Destroy; override;
    function  GetEnumerator: TEnumerator; inline;
    procedure Clear; inline;
    function  FindOrAdd(constref aKey: TKey; out aNode: PNode): Boolean;
    function  Find(constref aKey: TKey): PNode;
    function  Remove(constref aKey: TKey): Boolean;
    procedure RemoveNode(aNode: PNode); inline;
    property  Count: SizeInt read FCount;
    property  Lowest: PNode read GetLowest;
    property  Highest: PNode read GetHighest;
  end;

  { TGLiteAvlTree }

  generic TGLiteAvlTree<TKey, TEntry, TKeyCmpRel> = record
  public
  type
    PEntry = ^TEntry;

    TNode = record
    private
      procedure ClearLinks;
    public
      Left,
      Right,
      Parent: SizeInt;
      Data: TEntry;
      Balance: ShortInt;
    end;

    TNodeList = array of TNode;

  private
  type
    PAvlTree  = ^TGLiteAvlTree;

  const
    NULL_INDEX  = SizeInt(-1);
    NODE_SIZE   = SizeOf(TNode);
    MAX_CAPACITY: SizeInt  = MAX_CONTAINER_SIZE div NODE_SIZE;

  public
  type
    TEnumerator = record
    private
      FTree: PAvlTree;
      FCurrNode,
      FFirstNode: SizeInt;
      FInCycle: Boolean;
      function  GetCurrent: PEntry; inline;
      procedure Init(aTree: PAvlTree);
    public
      function  MoveNext: Boolean;
      procedure Reset; inline;
      property  Current: PEntry read GetCurrent;
    end;

  private
    FNodeList: TNodeList;
    FRoot,
    FCount: SizeInt;
    function  GetCapacity: SizeInt; inline;
    procedure Expand(aValue: SizeInt); inline;
    function  NewNode: SizeInt;
    procedure FreeNode(aNode: SizeInt);
    function  Successor(aNode: SizeInt): SizeInt;
    function  Predecessor(aNode: SizeInt): SizeInt;
    procedure SwapBalance(L, R: SizeInt); inline;
    function  GetHighest: SizeInt;
    function  GetLowest: SizeInt;
    function  FindInsertPos(constref aKey: TKey): SizeInt;
    function  FindNode(constref aKey: TKey; out aInsertPos: SizeInt): SizeInt;
    procedure ReplaceWithSuccessor(aNode: SizeInt);
    procedure RotateLeft(aNode: SizeInt);
    procedure RotateRight(aNode: SizeInt);
    procedure InsertNode(aNode: SizeInt);
    procedure InsertNodeAt(aNode, aParent: SizeInt);
    procedure BalanceAfterInsert(aNode: SizeInt);
    procedure RemoveNode(aNode: SizeInt);
    procedure BalanceAfterRemove(aNode: SizeInt; aNewBalance: ShortInt);

    property  Root: SizeInt read FRoot;
    class procedure CapacityExceedError(aValue: SizeInt); static; inline;
    class constructor Init;
    class operator Initialize(var t: TGLiteAvlTree);
    class operator Copy(constref aSrc: TGLiteAvlTree; var aDst: TGLiteAvlTree);
  public
    function  GetEnumerator: TEnumerator;
    procedure Clear;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    procedure TrimToFit; inline;
    function  FindOrAdd(constref aKey: TKey; out e: PEntry): Boolean;
    function  Find(constref aKey: TKey): PEntry;
    function  Remove(constref aKey: TKey): Boolean;
    procedure RemoveAt(aIndex: SizeInt); inline;
    function  FindLess(constref aKey: TKey): SizeInt;
    function  FindLessOrEqual(constref aKey: TKey): SizeInt;
    function  FindGreater(constref aKey: TKey): SizeInt;
    function  FindGreaterOrEqual(constref aKey: TKey): SizeInt;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
    property  Lowest: SizeInt read GetLowest;
    property  Highest: SizeInt read GetHighest;
    property  NodeList: TNodeList read FNodeList;
  end;

implementation
{$B-}{$COPERATORS ON}

{$PUSH}{$Q-}
{$IFNDEF CPU16}
function TGCustomAvlTree.TNode.GetBalance: SizeInt;
begin
  Assert((FParent and 3) <> 0, Format('Inconsistent internal Balance value(%d)', [FParent and 3]));
  Result := (FParent and 3) - 2;
end;

function TGCustomAvlTree.TNode.GetParent: PNode;
begin
  Result := {%H-}Pointer(FParent and not SizeInt(3));
end;

procedure TGCustomAvlTree.TNode.SetBalance(aValue: SizeInt);
begin
  Assert((aValue + 2) in [1..3],
    Format('Inconsistent input Balance value(%d) in '+{$I %CURRENTROUTINE%}, [aValue]));
  FParent := (FParent and not SizeInt(3)) or ((aValue + 2) and 3);
end;

procedure TGCustomAvlTree.TNode.SetParent(aValue: PNode);
begin
  Assert({%H-}SizeUint(aValue) and 3 = 0,
    Format('Unaligned input Parent value($%x) in '+{$I %CURRENTROUTINE%}, [aValue]));
  FParent := {%H-}SizeInt(aValue) or (FParent and 3);
end;

procedure TGCustomAvlTree.TNode.SwapBalance(aNode: PNode);
var
  b: SizeInt;
begin
  b := FParent and 3;
  FParent := (FParent and not SizeInt(3)) or (aNode^.FParent and 3);
  aNode^.FParent := (aNode^.FParent and not SizeInt(3)) or b;
end;
{$ELSE !CPU16}
procedure TGCustomAvlTree.TNode.SwapBalance(aNode: PNode);
var
  b: SizeInt;
begin
  b := Balance;
  Balance := aNode^.Balance;
  aNode^.Balance := b;
end;
{$ENDIF !CPU16}
{$POP}

function TGCustomAvlTree.TNode.Successor: PNode;
begin
  Result := Right;
  if Result <> nil then
    while Result^.Left <> nil do
      Result := Result^.Left
  else
    begin
      Result := @Self;
      while (Result^.Parent <> nil) and (Result^.Parent^.Right = Result) do
        Result := Result^.Parent;
      Result := Result^.Parent;
    end;
end;

function TGCustomAvlTree.TNode.Predecessor: PNode;
begin
  Result := Left;
  if Result <> nil then
    while Result^.Right <> nil do
      Result := Result^.Right
  else
    begin
      Result := @Self;
      while (Result^.Parent <> nil) and (Result^.Parent^.Left = Result) do
        Result := Result^.Parent;
      Result := Result^.Parent;
    end;
end;

{ TGCustomAvlTree.TEnumerator }

constructor TGCustomAvlTree.TEnumerator.Create(aTree: TGCustomAvlTree);
begin
  FTree := aTree;
end;

function TGCustomAvlTree.TEnumerator.MoveNext: Boolean;
var
  NextNode: PNode = nil;
begin
  if FCurrNode <> nil then
    NextNode := FCurrNode^.Successor
  else
    if not FInCycle then
      begin
        NextNode := FTree.Lowest;
        FInCycle := True;
      end;
  Result := NextNode <> nil;
  if Result then
    FCurrNode := NextNode;
end;

procedure TGCustomAvlTree.TEnumerator.Reset;
begin
  FCurrNode := nil;
  FInCycle := False;
end;

{ TGCustomAvlTree.TReverseEnumerator }

function TGCustomAvlTree.TReverseEnumerator.MoveNext: Boolean;
var
  NextNode: PNode = nil;
begin
  if FCurrNode <> nil then
    NextNode := FCurrNode^.Predecessor
  else
    if not FInCycle then
      begin
        NextNode := FTree.Highest;
        FInCycle := True;
      end;
  Result := NextNode <> nil;
  if Result then
    FCurrNode := NextNode;
end;

{ TGCustomAvlTree.TEnumeratorAt }

constructor TGCustomAvlTree.TEnumeratorAt.Create(aRootNode: PNode);
begin
  FRootNode := aRootNode;
end;

function TGCustomAvlTree.TEnumeratorAt.MoveNext: Boolean;
var
  NextNode: PNode = nil;
begin
  if FCurrNode <> nil then
    NextNode := FCurrNode^.Successor
  else
    if not FInCycle then
      begin
        NextNode := FRootNode;
        FInCycle := True;
      end;
  Result := NextNode <> nil;
  if Result then
    FCurrNode := NextNode;
end;

{ TGCustomAvlTree }

function TGCustomAvlTree.NewNode: PNode;
begin
  Result := FNodeManager.NewNode;
  Result^.Balance := 0;
  Inc(FCount);
end;

procedure TGCustomAvlTree.DisposeNode(aNode: PNode);
begin
  if aNode <> nil then
    begin
      aNode^ := Default(TNode);
      FNodeManager.FreeNode(aNode);
      Dec(FCount);
    end;
end;

function TGCustomAvlTree.GetCapacity: SizeInt;
begin
  Result := Count + FNodeManager.FreeCount;
end;

procedure TGCustomAvlTree.ClearTree;
begin
  RemoveNodeWithChilds(FRoot);
  FRoot := nil;
  Assert(Count = 0, Format('Inconsistent Count value(%d) after '+{$I %CURRENTROUTINE%}, [Count]));
end;

function TGCustomAvlTree.GetHighest: PNode;
begin
  Result := FRoot;
  if Result <> nil then
    while Result^.Right <> nil do
      Result := Result^.Right;
end;

function TGCustomAvlTree.GetLowest: PNode;
begin
  Result := FRoot;
  if Result <> nil then
    while Result^.Left <> nil do
      Result := Result^.Left;
end;

procedure TGCustomAvlTree.ReplaceWithSuccessor(aNode: PNode);
var
  SuccNode, OldParent, OldLeft, OldRight,
  OldSuccParent, OldSuccLeft, OldSuccRight: PNode;
begin
  SuccNode := aNode^.Successor;
  aNode^.SwapBalance(SuccNode);

  OldParent := aNode^.Parent;
  OldLeft := aNode^.Left;
  OldRight := aNode^.Right;
  OldSuccParent := SuccNode^.Parent;
  OldSuccLeft := SuccNode^.Left;
  OldSuccRight := SuccNode^.Right;

  if OldParent <> nil then
    begin
      if OldParent^.Left = aNode then
        OldParent^.Left := SuccNode
      else
        OldParent^.Right := SuccNode;
    end
  else
    FRoot := SuccNode;

  SuccNode^.Parent := OldParent;

  if OldSuccParent <> aNode then
    begin
      if OldSuccParent^.Left = SuccNode then
        OldSuccParent^.Left := aNode
      else
        OldSuccParent^.Right := aNode;
      SuccNode^.Right := OldRight;
      aNode^.Parent := OldSuccParent;
      if OldRight <> nil then
        OldRight^.Parent := SuccNode;
    end
  else
    begin
      SuccNode^.Right := aNode;
      aNode^.Parent := SuccNode;
    end;

  aNode^.Left := OldSuccLeft;
  if OldSuccLeft <> nil then
    OldSuccLeft^.Parent := aNode;
  aNode^.Right := OldSuccRight;
  if OldSuccRight <> nil then
    OldSuccRight^.Parent := aNode;
  SuccNode^.Left := OldLeft;
  if OldLeft <> nil then
    OldLeft^.Parent := SuccNode;
end;

procedure TGCustomAvlTree.DoRemoveNode(aNode: PNode);
var
  OldParent, Child: PNode;
begin
  if (aNode^.Left <> nil) and (aNode^.Right <> nil) then
    ReplaceWithSuccessor(aNode);
  OldParent := aNode^.Parent;
  aNode^.Parent := nil;
  if aNode^.Left <> nil then
    Child := aNode^.Left
  else
    Child := aNode^.Right;
  if Child <> nil then
    Child^.Parent := OldParent;
  if OldParent <> nil then
    begin
      if OldParent^.Left = aNode then
        begin
          OldParent^.Left := Child;
          BalanceAfterRemove(OldParent, Succ(OldParent^.Balance));
        end
      else
        begin
          OldParent^.Right := Child;
          BalanceAfterRemove(OldParent, Pred(OldParent^.Balance));
        end;
    end
  else
    FRoot := Child;
  DisposeNode(aNode);
end;

procedure TGCustomAvlTree.BalanceAfterInsert(aNode: PNode);
var
  OldParent, OldChild: PNode;
begin
  OldParent := aNode^.Parent;
  while OldParent <> nil do
    if OldParent^.Left = aNode then //aNode is left child => we need decrease OldParent^.Balance
      case OldParent^.Balance of
        -1:
          begin
            if aNode^.Balance = -1 then
              begin
                RotateRight(OldParent);
                aNode^.Balance := 0;
                OldParent^.Balance := 0;
              end
            else
              begin
                OldChild := aNode^.Right;
                RotateLeft(aNode);
                RotateRight(OldParent);
                case OldChild^.Balance of
                  -1:
                    begin
                      aNode^.Balance := 0;
                      OldParent^.Balance := 1;
                    end;
                  0:
                    begin
                      aNode^.Balance := 0;
                      OldParent^.Balance := 0
                    end;
                else //1
                  aNode^.Balance := -1;
                  OldParent^.Balance := 0;
                end;
                OldChild^.Balance := 0;
              end;
            break;
          end;
         0:
          begin
            OldParent^.Balance := -1;
            aNode := OldParent;
            OldParent := aNode^.Parent;
          end;
      else //1
        OldParent^.Balance := 0;
        break;
      end
    else  // aNode is right child => we need increase OldParent^.Balance
      case OldParent^.Balance of
       -1:
         begin
           OldParent^.Balance := 0;
           break;
         end;
        0:
          begin
            OldParent^.Balance := 1;
            aNode := OldParent;
            OldParent := aNode^.Parent;
          end;
      else // 1
        if aNode^.Balance = 1 then
          begin
            RotateLeft(OldParent);
            aNode^.Balance := 0;
            OldParent^.Balance := 0;
          end
        else
          begin
            OldChild := aNode^.Left;
            RotateRight(aNode);
            RotateLeft(OldParent);
            case OldChild^.Balance of
              -1:
                begin
                  aNode^.Balance := 1;
                  OldParent^.Balance := 0;
                end;
              0:
                begin
                  aNode^.Balance := 0;
                  OldParent^.Balance := 0;
                end;
            else  //1
              aNode^.Balance := 0;
              OldParent^.Balance := -1
            end;
            OldChild^.Balance := 0;
          end;
        break;
      end;
end;

procedure TGCustomAvlTree.BalanceAfterRemove(aNode: PNode; aNodeBalance: SizeInt);
var
  OldParent, Child, ChildOfChild: PNode;
begin
  while aNode <> nil do
    begin
      OldParent := aNode^.Parent;
      case aNodeBalance of
        -2:
          begin
            Child := aNode^.Left;
            if Child^.Balance <= 0 then
              begin
                RotateRight(aNode);
                aNode^.Balance := -Succ(Child^.Balance);
                aNodeBalance := Succ(Child^.Balance);
                aNode := Child;
                Child^.Balance := aNodeBalance;
              end
            else
              begin
                ChildOfChild := Child^.Right;
                RotateLeft(Child);
                RotateRight(aNode);
                case ChildOfChild^.Balance of
                  -1:
                    begin
                      aNode^.Balance := 1;
                      Child^.Balance := 0;
                    end;
                   0:
                     begin
                       aNode^.Balance := 0;
                       Child^.Balance := 0;
                     end;
                else // 1
                  aNode^.Balance := 0;
                  Child^.Balance := -1;
                end;
                aNode := ChildOfChild;
                aNodeBalance := 0;
                ChildOfChild^.Balance := 0;
              end;
          end;
        -1:
          begin
            aNode^.Balance := aNodeBalance;
            break;
          end;
         0:
           begin
             aNode^.Balance := aNodeBalance;
             if OldParent <> nil then
               begin
                 if OldParent^.Left = aNode then
                   aNodeBalance := Succ(OldParent^.Balance)
                 else
                   aNodeBalance := Pred(OldParent^.Balance);
                 aNode := OldParent;
               end
             else
               break;
           end;
         1:
           begin
             aNode^.Balance := aNodeBalance;
             break;
           end;
         2:
           begin
             Child := aNode^.Right;
             if Child^.Balance >= 0 then
               begin
                 RotateLeft(aNode);
                 aNode^.Balance := SizeInt(1) - Child^.Balance;
                 aNodeBalance := Pred(Child^.Balance);
                 aNode := Child;
                 Child^.Balance := aNodeBalance;
               end
             else
               begin
                 ChildOfChild := Child^.Left;
                 RotateRight(Child);
                 RotateLeft(aNode);
                 case ChildOfChild^.Balance of
                   -1:
                     begin
                       aNode^.Balance := 0;
                       Child^.Balance := 1;
                     end;
                    0:
                      begin
                        aNode^.Balance := 0;
                        Child^.Balance := 0;
                      end;
                 else // 1
                   aNode^.Balance := -1;
                   Child^.Balance := 0;
                 end;
                 ChildOfChild^.Balance := 0;
                 aNode := ChildOfChild;
                 aNodeBalance := 0;
               end;
           end;
      end;
    end;
end;

procedure TGCustomAvlTree.RotateLeft(aNode: PNode);
var
  OldParent, OldRight, OldRightLeft: PNode;
begin
  OldRight := aNode^.Right;
  OldRightLeft := OldRight^.Left;
  OldParent := aNode^.Parent;
  if OldParent <> nil then
    begin
      if OldParent^.Left = aNode then
        OldParent^.Left := OldRight
      else
        OldParent^.Right := OldRight;
    end
  else
    FRoot := OldRight;
  OldRight^.Parent := OldParent;
  aNode^.Parent := OldRight;
  aNode^.Right := OldRightLeft;
  if OldRightLeft <> nil then
    OldRightLeft^.Parent := aNode;
  OldRight^.Left := aNode;
end;

procedure TGCustomAvlTree.RotateRight(aNode: PNode);
var
  OldParent, OldLeft, OldLeftRight: PNode;
begin
  OldLeft := aNode^.Left;
  OldLeftRight := OldLeft^.Right;
  OldParent := aNode^.Parent;
  if OldParent <> nil then
    begin
      if OldParent^.Left = aNode then
        OldParent^.Left := OldLeft
      else
        OldParent^.Right := OldLeft;
    end
  else
    FRoot := OldLeft;
  OldLeft^.Parent := OldParent;
  aNode^.Parent := OldLeft;
  aNode^.Left := OldLeftRight;
  if OldLeftRight <> nil then
    OldLeftRight^.Parent := aNode;
  OldLeft^.Right := aNode;
end;

procedure TGCustomAvlTree.RemoveNodeWithChilds(aNode: PNode);
begin
  if aNode <> nil then
    begin
      RemoveNodeWithChilds(aNode^.Left);
      RemoveNodeWithChilds(aNode^.Right);
      aNode^.Data := Default(TEntry);
      FNodeManager.DisposeNode(aNode);
      Dec(FCount);
    end;
end;

constructor TGCustomAvlTree.Create;
begin
  FNodeManager := TNodeManager.Create;
  FNodeManager.EnsureFreeCount(DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGCustomAvlTree.Create(aCapacity: SizeInt);
begin
  FNodeManager := TNodeManager.Create;
  if aCapacity > 0 then
    FNodeManager.EnsureFreeCount(aCapacity);
end;

destructor TGCustomAvlTree.Destroy;
begin
  ClearTree;
  FNodeManager.Free;
  inherited;
end;

procedure TGCustomAvlTree.Clear;
begin
  ClearTree;
  FNodeManager.Clear;
end;

procedure TGCustomAvlTree.EnsureCapacity(aValue: SizeInt);
begin
  if aValue > Capacity then
    FNodeManager.EnsureFreeCount(aValue - Count);
end;

procedure TGCustomAvlTree.TrimToFit;
begin
  if Count > 0 then
    FNodeManager.ClearFreeList
  else
    FNodeManager.Clear;
end;

function TGCustomAvlTree.FindOrAdd(constref aKey: TKey; out aNode: PNode): Boolean;
var
  ParentNode: PNode;
begin
  aNode := FindNode(aKey, ParentNode);
  Result := aNode <> nil;
  if not Result then
    begin
      aNode := NewNode;
      aNode^.Data.Key := aKey;
      InsertNodeAt(aNode, ParentNode);
    end;
end;

function TGCustomAvlTree.Add(constref aData: TEntry): PNode;
begin
  Result := NewNode;
  Result^.Data := aData;
  InsertNode(Result);
end;

function TGCustomAvlTree.Remove(constref aKey: TKey): Boolean;
var
  Node: PNode;
begin
  Node := Find(aKey);
  Result := Node <> nil;
  if Result then
    DoRemoveNode(Node);
end;

procedure TGCustomAvlTree.RemoveNode(aNode: PNode);
begin
  if aNode <> nil then
    DoRemoveNode(aNode);
end;

function TGCustomAvlTree.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TGCustomAvlTree.GetReverseEnumerator: TEnumerator;
begin
  Result := TReverseEnumerator.Create(Self);
end;

function TGCustomAvlTree.GetEnumeratorAt(constref aKey: TKey; aInclusive: Boolean): TEnumeratorAt;
begin
  if aInclusive then
    Result := TEnumeratorAt.Create(FindGreaterOrEqual(aKey))
  else
    Result := TEnumeratorAt.Create(FindGreater(aKey))
end;

function TGCustomAvlTree.RemoveIf(aTest: TTest; aOnRemove: TEntryEvent): SizeInt;
var
  CurrNode, NextNode: PNode;
begin
  CurrNode := Lowest;
  Result := 0;
  while CurrNode <> nil do
    begin
      NextNode := CurrNode^.Successor;
      if aTest(CurrNode^.Data.Key) then
        begin
          if aOnRemove <> nil then
            aOnRemove(@CurrNode^.Data);
          DoRemoveNode(CurrNode);
          Inc(Result);
        end;
      CurrNode := NextNode;
    end;
end;

function TGCustomAvlTree.RemoveIf(aTest: TOnTest; aOnRemove: TEntryEvent): SizeInt;
var
  CurrNode, NextNode: PNode;
begin
  CurrNode := Lowest;
  Result := 0;
  while CurrNode <> nil do
    begin
      NextNode := CurrNode^.Successor;
      if aTest(CurrNode^.Data.Key) then
        begin
          if aOnRemove <> nil then
            aOnRemove(@CurrNode^.Data);
          DoRemoveNode(CurrNode);
          Inc(Result);
        end;
      CurrNode := NextNode;
    end;
end;

function TGCustomAvlTree.RemoveIf(aTest: TNestTest; aOnRemove: TEntryEvent): SizeInt;
var
  CurrNode, NextNode: PNode;
begin
  CurrNode := Lowest;
  Result := 0;
  while CurrNode <> nil do
    begin
      NextNode := CurrNode^.Successor;
      if aTest(CurrNode^.Data.Key) then
        begin
          if aOnRemove <> nil then
            aOnRemove(@CurrNode^.Data);
          DoRemoveNode(CurrNode);
          Inc(Result);
        end;
      CurrNode := NextNode;
    end;
end;

function TGCustomAvlTree.RemoveIf(aTest: TEntryTest; aOnRemove: TEntryEvent): SizeInt;
var
  CurrNode, NextNode: PNode;
begin
  CurrNode := Lowest;
  Result := 0;
  while CurrNode <> nil do
    begin
      NextNode := CurrNode^.Successor;
      if aTest(@CurrNode^.Data) then
        begin
          if aOnRemove <> nil then
            aOnRemove(@CurrNode^.Data);
          DoRemoveNode(CurrNode);
          Inc(Result);
        end;
      CurrNode := NextNode;
    end;
end;

{ TGAvlTree }

function TGAvlTree.FindNode(constref aKey: TKey; out aInsertPos: PNode): PNode;
var
  c: SizeInt;
begin
  Result := FRoot;
  aInsertPos := nil;
  while Result <> nil do
    begin
      c := TKeyCmpRel.Compare(aKey, Result^.Data.Key);
      aInsertPos := Result;
      if c < 0 then
        Result := Result^.Left
      else
        if c > 0 then
          Result := Result^.Right
        else
          break;
    end;
end;

function TGAvlTree.FindInsertPos(constref aKey: TKey): PNode;
begin
  Result := FRoot;
  while Result <> nil do
    if TKeyCmpRel.Compare(aKey, Result^.Data.Key) < 0 then
      begin
        if Result^.Left <> nil then
          Result := Result^.Left
        else
          break;
      end
    else
      begin
        if Result^.Right <> nil then
          Result := Result^.Right
        else
          break;
      end;
end;

procedure TGAvlTree.InsertNode(aNode: PNode);
var
  ParentNode: PNode;
begin
  if FRoot <> nil then
    begin
      ParentNode := FindInsertPos(aNode^.Data.Key);
      aNode^.Parent := ParentNode;
      if TKeyCmpRel.Compare(aNode^.Data.Key, ParentNode^.Data.Key) < 0 then
        ParentNode^.Left := aNode
      else
        ParentNode^.Right := aNode;
      BalanceAfterInsert(aNode);
    end
  else
    begin
      FRoot := aNode;
      aNode^.Parent := nil;
    end;
end;

procedure TGAvlTree.InsertNodeAt(aNode, aParent: PNode);
begin
  if aParent <> nil then
    begin
      aNode^.Parent := aParent;
      if TKeyCmpRel.Compare(aNode^.Data.Key, aParent^.Data.Key) < 0 then
        aParent^.Left := aNode
      else
        aParent^.Right := aNode;
      BalanceAfterInsert(aNode);
    end
  else
    begin
      FRoot := aNode;
      aNode^.Parent := nil;
    end;
end;

function TGAvlTree.Clone: TGAvlTree;
var
  p: PNode;
begin
  Result := TGAvlTree.Create(Count);
  for p in Self do
    Result.Add(p^.Data);
end;

function TGAvlTree.Find(constref aKey: TKey): PNode;
var
  c: SizeInt;
begin
  Result := FRoot;
  while Result <> nil do
    begin
      c := TKeyCmpRel.Compare(aKey, Result^.Data.Key);
      if c < 0 then
        Result := Result^.Left
      else
        if c > 0 then
          Result := Result^.Right
        else
          break;
    end;
end;

function TGAvlTree.FindLess(constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := FRoot;
  Result := nil;
  while Node <> nil do
    if TKeyCmpRel.Compare(aKey, Node^.Data.Key) > 0 then
      begin
        Result := Node;
        Node := Node^.Right;
      end
    else
      Node := Node^.Left;
end;

function TGAvlTree.FindLessOrEqual(constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := FRoot;
  Result := nil;
  while Node <> nil do
    if TKeyCmpRel.Compare(aKey, Node^.Data.Key) >= 0 then
      begin
        Result := Node;
        Node := Node^.Right;
      end
    else
      Node := Node^.Left;
end;

function TGAvlTree.FindGreater(constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := FRoot;
  Result := nil;
  while Node <> nil do
    if TKeyCmpRel.Compare(aKey, Node^.Data.Key) < 0 then
      begin
        Result := Node;
        Node := Node^.Left;
      end
    else
      Node := Node^.Right;
end;

function TGAvlTree.FindGreaterOrEqual(constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := FRoot;
  Result := nil;
  while Node <> nil do
    if TKeyCmpRel.Compare(aKey, Node^.Data.Key) <= 0 then
      begin
        Result := Node;
        Node := Node^.Left;
      end
    else
      Node := Node^.Right;
end;

{ TGComparableAvlTree }

function TGComparableAvlTree.FindNode(constref aKey: TKey; out aInsertPos: PNode): PNode;
begin
  Result := FRoot;
  aInsertPos := nil;
  while Result <> nil do
    begin
      aInsertPos := Result;
      if aKey < Result^.Data.Key then
        Result := Result^.Left
      else
        if aKey > Result^.Data.Key then
          Result := Result^.Right
        else
          break;
    end;
end;

function TGComparableAvlTree.FindInsertPos(constref aKey: TKey): PNode;
begin
  Result := FRoot;
  while Result <> nil do
    if aKey < Result^.Data.Key then
      begin
        if Result^.Left <> nil then
          Result := Result^.Left
        else
          break;
      end
    else
      begin
        if Result^.Right <> nil then
          Result := Result^.Right
        else
          break;
      end;
end;

procedure TGComparableAvlTree.InsertNode(aNode: PNode);
// If there are already exist nodes with the same value it will be inserted rightmost
var
  ParentNode: PNode;
begin
  if FRoot <> nil then
    begin
      ParentNode := FindInsertPos(aNode^.Data.Key);
      aNode^.Parent := ParentNode;
      if aNode^.Data.Key < ParentNode^.Data.Key then
        ParentNode^.Left := aNode
      else
        ParentNode^.Right := aNode;
      BalanceAfterInsert(aNode);
    end
  else
    begin
      FRoot := aNode;
      aNode^.Parent := nil;
    end;
end;

procedure TGComparableAvlTree.InsertNodeAt(aNode, aParent: PNode);
begin
  if aParent <> nil then
    begin
      aNode^.Parent := aParent;
      if aNode^.Data.Key < aParent^.Data.Key then
        aParent^.Left := aNode
      else
        aParent^.Right := aNode;
      BalanceAfterInsert(aNode);
    end
  else
    begin
      FRoot := aNode;
      aNode^.Parent := nil;
    end;
end;

function TGComparableAvlTree.Clone: TGComparableAvlTree;
var
  p: PNode;
begin
  Result := TGComparableAvlTree.Create(Count);
  for p in Self do
    Result.Add(p^.Data);
end;

function TGComparableAvlTree.Find(constref aKey: TKey): PNode;
begin
  Result := FRoot;
  while Result <> nil do
    if aKey < Result^.Data.Key then
      Result := Result^.Left
    else
      if aKey > Result^.Data.Key then
        Result := Result^.Right
      else
        break;
end;

function TGComparableAvlTree.FindLess(constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := FRoot;
  Result := nil;
  while Node <> nil do
    if aKey > Node^.Data.Key then
      begin
        Result := Node;
        Node := Node^.Right;
      end
    else
      Node := Node^.Left;
end;

function TGComparableAvlTree.FindLessOrEqual(constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := FRoot;
  Result := nil;
  while Node <> nil do
    if aKey >= Node^.Data.Key then
      begin
        Result := Node;
        Node := Node^.Right;
      end
    else
      Node := Node^.Left;
end;

function TGComparableAvlTree.FindGreater(constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := FRoot;
  Result := nil;
  while Node <> nil do
    if aKey < Node^.Data.Key then
      begin
        Result := Node;
        Node := Node^.Left;
      end
    else
      Node := Node^.Right;
end;

function TGComparableAvlTree.FindGreaterOrEqual(constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := FRoot;
  Result := nil;
  while Node <> nil do
    if aKey <= Node^.Data.Key then
      begin
        Result := Node;
        Node := Node^.Left;
      end
    else
      Node := Node^.Right;
end;

{ TGRegularAvlTree }

function TGRegularAvlTree.FindNode(constref aKey: TKey; out aInsertPos: PNode): PNode;
var
  c: SizeInt;
begin
  Result := FRoot;
  aInsertPos := nil;
  while Result <> nil do
    begin
      c := FCompare(aKey, Result^.Data.Key);
      aInsertPos := Result;
      if c < 0 then
        Result := Result^.Left
      else
        if c > 0 then
          Result := Result^.Right
        else
          break;
    end;
end;

function TGRegularAvlTree.FindInsertPos(constref aKey: TKey): PNode;
begin
  Result := FRoot;
  while Result <> nil do
    if FCompare(aKey, Result^.Data.Key) < 0 then
      begin
        if Result^.Left <> nil then
          Result := Result^.Left
        else
          break;
      end
    else
      begin
        if Result^.Right <> nil then
          Result := Result^.Right
        else
          break;
      end;
end;

procedure TGRegularAvlTree.InsertNode(aNode: PNode);
// If there are already exist nodes with the same value it will be inserted rightmost
var
  ParentNode: PNode;
begin
  if FRoot <> nil then
    begin
      ParentNode := FindInsertPos(aNode^.Data.Key);
      aNode^.Parent := ParentNode;
      if FCompare(aNode^.Data.Key, ParentNode^.Data.Key) < 0 then
        ParentNode^.Left := aNode
      else
        ParentNode^.Right := aNode;
      BalanceAfterInsert(aNode);
    end
  else
    begin
      FRoot := aNode;
      aNode^.Parent := nil;
    end;
end;

procedure TGRegularAvlTree.InsertNodeAt(aNode, aParent: PNode);
begin
  if aParent <> nil then
    begin
      aNode^.Parent := aParent;
      if FCompare(aNode^.Data.Key, aParent^.Data.Key) < 0 then
        aParent^.Left := aNode
      else
        aParent^.Right := aNode;
      BalanceAfterInsert(aNode);
    end
  else
    begin
      FRoot := aNode;
      aNode^.Parent := nil;
    end;
end;

constructor TGRegularAvlTree.Create(aCompare: TCompare);
begin
  inherited Create;
  FCompare := aCompare;
end;

constructor TGRegularAvlTree.Create(aCapacity: SizeInt; aCompare: TCompare);
begin
  inherited Create(aCapacity);
  FCompare := aCompare;
end;

function TGRegularAvlTree.Clone: TGRegularAvlTree;
var
  p: PNode;
begin
  Result := TGRegularAvlTree.Create(Count, FCompare);
  for p in Self do
    Result.Add(p^.Data);
end;

function TGRegularAvlTree.Find(constref aKey: TKey): PNode;
var
  c: SizeInt;
begin
  Result := FRoot;
  while Result <> nil do
    begin
      c := FCompare(aKey, Result^.Data.Key);
      if c < 0 then
        Result := Result^.Left
      else
        if c > 0 then
          Result := Result^.Right
        else
          exit;
    end;
end;

function TGRegularAvlTree.FindLess(constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := FRoot;
  Result := nil;
  while Node <> nil do
    if FCompare(aKey, Node^.Data.Key) > 0 then
      begin
        Result := Node;
        Node := Node^.Right;
      end
    else
      Node := Node^.Left;
end;

function TGRegularAvlTree.FindLessOrEqual(constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := FRoot;
  Result := nil;
  while Node <> nil do
    if FCompare(aKey, Node^.Data.Key) >= 0 then
      begin
        Result := Node;
        Node := Node^.Right;
      end
    else
      Node := Node^.Left;
end;

function TGRegularAvlTree.FindGreater(constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := FRoot;
  Result := nil;
  while Node <> nil do
    if FCompare(aKey, Node^.Data.Key) < 0 then
      begin
        Result := Node;
        Node := Node^.Left;
      end
    else
      Node := Node^.Right;
end;

function TGRegularAvlTree.FindGreaterOrEqual(constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := FRoot;
  Result := nil;
  while Node <> nil do
    if FCompare(aKey, Node^.Data.Key) <= 0 then
      begin
        Result := Node;
        Node := Node^.Left;
      end
    else
      Node := Node^.Right;
end;

{ TGDelegatedAvlTree }

function TGDelegatedAvlTree.FindNode(constref aKey: TKey; out aInsertPos: PNode): PNode;
var
  c: SizeInt;
begin
  Result := FRoot;
  aInsertPos := nil;
  while Result <> nil do
    begin
      c := FCompare(aKey, Result^.Data.Key);
      aInsertPos := Result;
      if c < 0 then
        Result := Result^.Left
      else
        if c > 0 then
          Result := Result^.Right
        else
          break;
    end;
end;

function TGDelegatedAvlTree.FindInsertPos(constref aKey: TKey): PNode;
begin
  Result := FRoot;
  while Result <> nil do
    if FCompare(aKey, Result^.Data.Key) < 0 then
      begin
        if Result^.Left <> nil then
          Result := Result^.Left
        else
          break;
      end
    else
      begin
        if Result^.Right <> nil then
          Result := Result^.Right
        else
          break;
      end;
end;

procedure TGDelegatedAvlTree.InsertNode(aNode: PNode);
// If there are already exist nodes with the same value it will be inserted rightmost
var
  ParentNode: PNode;
begin
  if FRoot <> nil then
    begin
      ParentNode := FindInsertPos(aNode^.Data.Key);
      aNode^.Parent := ParentNode;
      if FCompare(aNode^.Data.Key, ParentNode^.Data.Key) < 0 then
        ParentNode^.Left := aNode
      else
        ParentNode^.Right := aNode;
      BalanceAfterInsert(aNode);
    end
  else
    begin
      FRoot := aNode;
      aNode^.Parent := nil;
    end;
end;

procedure TGDelegatedAvlTree.InsertNodeAt(aNode, aParent: PNode);
begin
  if aParent <> nil then
    begin
      aNode^.Parent := aParent;
      if FCompare(aNode^.Data.Key, aParent^.Data.Key) < 0 then
        aParent^.Left := aNode
      else
        aParent^.Right := aNode;
      BalanceAfterInsert(aNode);
    end
  else
    begin
      FRoot := aNode;
      aNode^.Parent := nil;
    end;
end;

constructor TGDelegatedAvlTree.Create(aCompare: TOnCompare);
begin
  inherited Create;
  FCompare := aCompare;
end;

constructor TGDelegatedAvlTree.Create(aCapacity: SizeInt; aCompare: TOnCompare);
begin
  inherited Create(aCapacity);
  FCompare := aCompare;
end;

function TGDelegatedAvlTree.Clone: TGDelegatedAvlTree;
var
  p: PNode;
begin
  Result := TGDelegatedAvlTree.Create(Count, FCompare);
  for p in Self do
    Result.Add(p^.Data);
end;

function TGDelegatedAvlTree.Find(constref aKey: TKey): PNode;
var
  c: SizeInt;
begin
  Result := FRoot;
  while Result <> nil do
    begin
      c := FCompare(aKey, Result^.Data.Key);
      if c < 0 then
        Result := Result^.Left
      else
        if c > 0 then
          Result := Result^.Right
        else
          exit;
    end;
end;

function TGDelegatedAvlTree.FindLess(constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := FRoot;
  Result := nil;
  while Node <> nil do
    if FCompare(aKey, Node^.Data.Key) > 0 then
      begin
        Result := Node;
        Node := Node^.Right;
      end
    else
      Node := Node^.Left;
end;

function TGDelegatedAvlTree.FindLessOrEqual(constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := FRoot;
  Result := nil;
  while Node <> nil do
    if FCompare(aKey, Node^.Data.Key) >= 0 then
      begin
        Result := Node;
        Node := Node^.Right;
      end
    else
      Node := Node^.Left;
end;

function TGDelegatedAvlTree.FindGreater(constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := FRoot;
  Result := nil;
  while Node <> nil do
    if FCompare(aKey, Node^.Data.Key) < 0 then
      begin
        Result := Node;
        Node := Node^.Left;
      end
    else
      Node := Node^.Right;
end;

function TGDelegatedAvlTree.FindGreaterOrEqual(constref aKey: TKey): PNode;
var
  Node: PNode;
begin
  Node := FRoot;
  Result := nil;
  while Node <> nil do
    if FCompare(aKey, Node^.Data.Key) <= 0 then
      begin
        Result := Node;
        Node := Node^.Left;
      end
    else
      Node := Node^.Right;
end;

{$IFNDEF CPU16}
function TGAvlTreeNode.GetBalance: SizeInt;
begin
  Assert((FParent and 3) <> 0, Format('Inconsistent internal Balance value(%d)', [FParent and 3]));
  Result := (FParent and 3) - 2;
end;

function TGAvlTreeNode.GetParent: PNode;
begin
  Result := {%H-}Pointer(FParent and not SizeInt(3));
end;

procedure TGAvlTreeNode.SetBalance(aValue: SizeInt);
begin
  Assert((aValue + 2) in [1..3],
    Format('Inconsistent input Balance value(%d) in '+{$I %CURRENTROUTINE%}, [aValue]));
  FParent := (FParent and not SizeInt(3)) or ((aValue + 2) and 3);
end;

procedure TGAvlTreeNode.SetParent(aValue: PNode);
begin
  Assert({%H-}SizeUint(aValue) and 3 = 0,
    Format('Unaligned input Parent value($%x) in '+{$I %CURRENTROUTINE%}, [aValue]));
  FParent := {%H-}SizeInt(aValue) or (FParent and 3);
end;

procedure TGAvlTreeNode.SwapBalance(aNode: PNode);
var
  b: SizeInt;
begin
  b := FParent and 3;
  FParent := (FParent and not SizeInt(3)) or (aNode^.FParent and 3);
  aNode^.FParent := (aNode^.FParent and not SizeInt(3)) or b;
end;
{$ELSE !CPU16}
procedure TGAvlTreeNode.SwapBalance(aNode: PNode);
var
  b: SizeInt;
begin
  b := Balance;
  Balance := aNode^.Balance;
  aNode^.Balance := b;
end;
{$ENDIF !CPU16}

function TGAvlTreeNode.Successor: PNode;
begin
  Result := Right;
  if Result <> nil then
    while Result^.Left <> nil do
      Result := Result^.Left
  else
    begin
      Result := @Self;
      while (Result^.Parent <> nil) and (Result^.Parent^.Right = Result) do
        Result := Result^.Parent;
      Result := Result^.Parent;
    end;
end;

function TGAvlTreeNode.Predecessor: PNode;
begin
  Result := Left;
  if Result <> nil then
    while Result^.Right <> nil do
      Result := Result^.Right
  else
    begin
      Result := @Self;
      while (Result^.Parent <> nil) and (Result^.Parent^.Left = Result) do
        Result := Result^.Parent;
      Result := Result^.Parent;
    end;
end;

{ TGAvlTree2.TEnumerator }

function TGAvlTree2.TEnumerator.GetCurrent: PEntry;
begin
  Result := @FCurrNode^.Data;
end;

procedure TGAvlTree2.TEnumerator.Init(aTree: TGAvlTree2);
begin
  FTree := aTree;
  Reset;
end;

function TGAvlTree2.TEnumerator.MoveNext: Boolean;
var
  NextNode: PNode = nil;
begin
  if FCurrNode <> nil then
    NextNode := FCurrNode^.Successor
  else
    if not FInCycle then
      begin
        NextNode := FTree.Lowest;
        FInCycle := True;
      end;
  Result := NextNode <> nil;
  if Result then
    FCurrNode := NextNode;
end;

procedure TGAvlTree2.TEnumerator.Reset;
begin
  FCurrNode := nil;
  FInCycle := False;
end;

{ TGAvlTree2 }

function TGAvlTree2.NewNode: PNode;
begin
  Result := FNodeManager.NewNode;
  Result^.Balance := 0;
  Inc(FCount);
end;

procedure TGAvlTree2.DisposeNode(aNode: PNode);
begin
  if aNode <> nil then
    begin
      aNode^ := Default(TNode);
      FNodeManager.FreeNode(aNode);
      Dec(FCount);
    end;
end;

procedure TGAvlTree2.ClearTree;
begin
  RemoveNodeWithChilds(FRoot);
  FRoot := nil;
end;

function TGAvlTree2.GetHighest: PNode;
begin
  Result := FRoot;
  if Result <> nil then
    while Result^.Right <> nil do
      Result := Result^.Right;
end;

function TGAvlTree2.GetLowest: PNode;
begin
  Result := FRoot;
  if Result <> nil then
    while Result^.Left <> nil do
      Result := Result^.Left;
end;

function TGAvlTree2.FindNode(constref aKey: TKey; out aInsertPos: PNode): PNode;
var
  c: SizeInt;
begin
  Result := FRoot;
  aInsertPos := nil;
  while Result <> nil do
    begin
      c := TKeyCmpRel.Compare(aKey, Result^.Data.Key);
      aInsertPos := Result;
      if c < 0 then
        Result := Result^.Left
      else
        if c > 0 then
          Result := Result^.Right
        else
          break;
    end;
end;

function TGAvlTree2.FindInsertPos(constref aKey: TKey): PNode;
begin
  Result := FRoot;
  while Result <> nil do
    if TKeyCmpRel.Compare(aKey, Result^.Data.Key) < 0 then
      begin
        if Result^.Left <> nil then
          Result := Result^.Left
        else
          break;
      end
    else
      begin
        if Result^.Right <> nil then
          Result := Result^.Right
        else
          break;
      end;
end;

procedure TGAvlTree2.InsertNode(aNode: PNode);
var
  ParentNode: PNode;
begin
  if FRoot <> nil then
    begin
      ParentNode := FindInsertPos(aNode^.Data.Key);
      aNode^.Parent := ParentNode;
      if TKeyCmpRel.Compare(aNode^.Data.Key, ParentNode^.Data.Key) < 0 then
        ParentNode^.Left := aNode
      else
        ParentNode^.Right := aNode;
      BalanceAfterInsert(aNode);
    end
  else
    begin
      FRoot := aNode;
      aNode^.Parent := nil;
    end;
end;

procedure TGAvlTree2.InsertNodeAt(aNode, aParent: PNode);
begin
  if aParent <> nil then
    begin
      aNode^.Parent := aParent;
      if TKeyCmpRel.Compare(aNode^.Data.Key, aParent^.Data.Key) < 0 then
        aParent^.Left := aNode
      else
        aParent^.Right := aNode;
      BalanceAfterInsert(aNode);
    end
  else
    begin
      FRoot := aNode;
      aNode^.Parent := nil;
    end;
end;

procedure TGAvlTree2.ReplaceWithSuccessor(aNode: PNode);
var
  SuccNode, OldParent, OldLeft, OldRight,
  OldSuccParent, OldSuccLeft, OldSuccRight: PNode;
begin
  SuccNode := aNode^.Successor;
  aNode^.SwapBalance(SuccNode);

  OldParent := aNode^.Parent;
  OldLeft := aNode^.Left;
  OldRight := aNode^.Right;
  OldSuccParent := SuccNode^.Parent;
  OldSuccLeft := SuccNode^.Left;
  OldSuccRight := SuccNode^.Right;

  if OldParent <> nil then
    begin
      if OldParent^.Left = aNode then
        OldParent^.Left := SuccNode
      else
        OldParent^.Right := SuccNode;
    end
  else
    FRoot := SuccNode;

  SuccNode^.Parent := OldParent;

  if OldSuccParent <> aNode then
    begin
      if OldSuccParent^.Left = SuccNode then
        OldSuccParent^.Left := aNode
      else
        OldSuccParent^.Right := aNode;
      SuccNode^.Right := OldRight;
      aNode^.Parent := OldSuccParent;
      if OldRight <> nil then
        OldRight^.Parent := SuccNode;
    end
  else
    begin
      SuccNode^.Right := aNode;
      aNode^.Parent := SuccNode;
    end;

  aNode^.Left := OldSuccLeft;
  if OldSuccLeft <> nil then
    OldSuccLeft^.Parent := aNode;
  aNode^.Right := OldSuccRight;
  if OldSuccRight <> nil then
    OldSuccRight^.Parent := aNode;
  SuccNode^.Left := OldLeft;
  if OldLeft <> nil then
    OldLeft^.Parent := SuccNode;
end;

procedure TGAvlTree2.DoRemoveNode(aNode: PNode);
var
  OldParent, Child: PNode;
begin
  if (aNode^.Left <> nil) and (aNode^.Right <> nil) then
    ReplaceWithSuccessor(aNode);
  OldParent := aNode^.Parent;
  aNode^.Parent := nil;
  if aNode^.Left <> nil then
    Child := aNode^.Left
  else
    Child := aNode^.Right;
  if Child <> nil then
    Child^.Parent := OldParent;
  if OldParent <> nil then
    begin
      if OldParent^.Left = aNode then
        begin
          OldParent^.Left := Child;
          BalanceAfterRemove(OldParent, Succ(OldParent^.Balance));
        end
      else
        begin
          OldParent^.Right := Child;
          BalanceAfterRemove(OldParent, Pred(OldParent^.Balance));
        end;
    end
  else
    FRoot := Child;
  DisposeNode(aNode);
end;

procedure TGAvlTree2.BalanceAfterInsert(aNode: PNode);
var
  OldParent, OldChild: PNode;
begin
  OldParent := aNode^.Parent;
  while OldParent <> nil do
    if OldParent^.Left = aNode then //aNode is left child => we need decrease OldParent^.Balance
      case OldParent^.Balance of
        -1:
          begin
            if aNode^.Balance = -1 then
              begin
                RotateRight(OldParent);
                aNode^.Balance := 0;
                OldParent^.Balance := 0;
              end
            else
              begin
                OldChild := aNode^.Right;
                RotateLeft(aNode);
                RotateRight(OldParent);
                case OldChild^.Balance of
                  -1:
                    begin
                      aNode^.Balance := 0;
                      OldParent^.Balance := 1;
                    end;
                  0:
                    begin
                      aNode^.Balance := 0;
                      OldParent^.Balance := 0
                    end;
                else //1
                  aNode^.Balance := -1;
                  OldParent^.Balance := 0;
                end;
                OldChild^.Balance := 0;
              end;
            break;
          end;
         0:
          begin
            OldParent^.Balance := -1;
            aNode := OldParent;
            OldParent := aNode^.Parent;
          end;
      else //1
        OldParent^.Balance := 0;
        break;
      end
    else  // aNode is right child => we need increase OldParent^.Balance
      case OldParent^.Balance of
       -1:
         begin
           OldParent^.Balance := 0;
           break;
         end;
        0:
          begin
            OldParent^.Balance := 1;
            aNode := OldParent;
            OldParent := aNode^.Parent;
          end;
      else // 1
        if aNode^.Balance = 1 then
          begin
            RotateLeft(OldParent);
            aNode^.Balance := 0;
            OldParent^.Balance := 0;
          end
        else
          begin
            OldChild := aNode^.Left;
            RotateRight(aNode);
            RotateLeft(OldParent);
            case OldChild^.Balance of
              -1:
                begin
                  aNode^.Balance := 1;
                  OldParent^.Balance := 0;
                end;
              0:
                begin
                  aNode^.Balance := 0;
                  OldParent^.Balance := 0;
                end;
            else  //1
              aNode^.Balance := 0;
              OldParent^.Balance := -1
            end;
            OldChild^.Balance := 0;
          end;
        break;
      end;
end;

procedure TGAvlTree2.BalanceAfterRemove(aNode: PNode; aNewBalance: SizeInt);
var
  OldParent, Child, ChildOfChild: PNode;
begin
  while aNode <> nil do
    begin
      OldParent := aNode^.Parent;
      case aNewBalance of
        -2:
          begin
            Child := aNode^.Left;
            if Child^.Balance <= 0 then
              begin
                RotateRight(aNode);
                aNode^.Balance := -Succ(Child^.Balance);
                aNewBalance := Succ(Child^.Balance);
                aNode := Child;
                Child^.Balance := aNewBalance;
              end
            else
              begin
                ChildOfChild := Child^.Right;
                RotateLeft(Child);
                RotateRight(aNode);
                case ChildOfChild^.Balance of
                  -1:
                    begin
                      aNode^.Balance := 1;
                      Child^.Balance := 0;
                    end;
                   0:
                     begin
                       aNode^.Balance := 0;
                       Child^.Balance := 0;
                     end;
                else // 1
                  aNode^.Balance := 0;
                  Child^.Balance := -1;
                end;
                aNode := ChildOfChild;
                aNewBalance := 0;
                ChildOfChild^.Balance := 0;
              end;
          end;
        -1:
          begin
            aNode^.Balance := aNewBalance;
            break;
          end;
         0:
           begin
             aNode^.Balance := aNewBalance;
             if OldParent <> nil then
               begin
                 if OldParent^.Left = aNode then
                   aNewBalance := Succ(OldParent^.Balance)
                 else
                   aNewBalance := Pred(OldParent^.Balance);
                 aNode := OldParent;
               end
             else
               break;
           end;
         1:
           begin
             aNode^.Balance := aNewBalance;
             break;
           end;
         2:
           begin
             Child := aNode^.Right;
             if Child^.Balance >= 0 then
               begin
                 RotateLeft(aNode);
                 aNode^.Balance := SizeInt(1) - Child^.Balance;
                 aNewBalance := Pred(Child^.Balance);
                 aNode := Child;
                 Child^.Balance := aNewBalance;
               end
             else
               begin
                 ChildOfChild := Child^.Left;
                 RotateRight(Child);
                 RotateLeft(aNode);
                 case ChildOfChild^.Balance of
                   -1:
                     begin
                       aNode^.Balance := 0;
                       Child^.Balance := 1;
                     end;
                    0:
                      begin
                        aNode^.Balance := 0;
                        Child^.Balance := 0;
                      end;
                 else // 1
                   aNode^.Balance := -1;
                   Child^.Balance := 0;
                 end;
                 ChildOfChild^.Balance := 0;
                 aNode := ChildOfChild;
                 aNewBalance := 0;
               end;
           end;
      end;
    end;
end;

procedure TGAvlTree2.RotateLeft(aNode: PNode);
var
  OldParent, OldRight, OldRightLeft: PNode;
begin
  OldRight := aNode^.Right;
  OldRightLeft := OldRight^.Left;
  OldParent := aNode^.Parent;
  if OldParent <> nil then
    begin
      if OldParent^.Left = aNode then
        OldParent^.Left := OldRight
      else
        OldParent^.Right := OldRight;
    end
  else
    FRoot := OldRight;
  OldRight^.Parent := OldParent;
  aNode^.Parent := OldRight;
  aNode^.Right := OldRightLeft;
  if OldRightLeft <> nil then
    OldRightLeft^.Parent := aNode;
  OldRight^.Left := aNode;
end;

procedure TGAvlTree2.RotateRight(aNode: PNode);
var
  OldParent, OldLeft, OldLeftRight: PNode;
begin
  OldLeft := aNode^.Left;
  OldLeftRight := OldLeft^.Right;
  OldParent := aNode^.Parent;
  if OldParent <> nil then
    begin
      if OldParent^.Left = aNode then
        OldParent^.Left := OldLeft
      else
        OldParent^.Right := OldLeft;
    end
  else
    FRoot := OldLeft;
  OldLeft^.Parent := OldParent;
  aNode^.Parent := OldLeft;
  aNode^.Left := OldLeftRight;
  if OldLeftRight <> nil then
    OldLeftRight^.Parent := aNode;
  OldLeft^.Right := aNode;
end;

procedure TGAvlTree2.RemoveNodeWithChilds(aNode: PNode);
begin
  if aNode <> nil then
    begin
      RemoveNodeWithChilds(aNode^.Left);
      RemoveNodeWithChilds(aNode^.Right);
      aNode^.Data := Default(TEntry);
      DisposeNode(aNode);
    end;
end;

constructor TGAvlTree2.Create(aNodeManager: TNodeManager);
begin
  FNodeManager := aNodeManager;
end;

destructor TGAvlTree2.Destroy;
begin
  ClearTree;
  inherited;
end;

function TGAvlTree2.GetEnumerator: TEnumerator;
begin
  Result.Init(Self);
end;

procedure TGAvlTree2.Clear;
begin
  ClearTree;
end;

function TGAvlTree2.FindOrAdd(constref aKey: TKey; out aNode: PNode): Boolean;
var
  ParentNode: PNode;
begin
  aNode := FindNode(aKey, ParentNode);
  Result := aNode <> nil;
  if not Result then
    begin
      aNode := NewNode;
      aNode^.Data.Key := aKey;
      InsertNodeAt(aNode, ParentNode);
    end;
end;

function TGAvlTree2.Find(constref aKey: TKey): PNode;
var
  c: SizeInt;
begin
  Result := FRoot;
  while Result <> nil do
    begin
      c := TKeyCmpRel.Compare(aKey, Result^.Data.Key);
      if c < 0 then
        Result := Result^.Left
      else
        if c > 0 then
          Result := Result^.Right
        else
          break;
    end;
end;

function TGAvlTree2.Remove(constref aKey: TKey): Boolean;
var
  Node: PNode;
begin
  Node := Find(aKey);
  Result := Node <> nil;
  if Result then
    DoRemoveNode(Node);
end;

procedure TGAvlTree2.RemoveNode(aNode: PNode);
begin
  if aNode <> nil then
    DoRemoveNode(aNode);
end;

{ TGLiteAvlTree.TNode }

procedure TGLiteAvlTree.TNode.ClearLinks;
begin
  Left := NULL_INDEX;
  Right := NULL_INDEX;
  Parent := NULL_INDEX;
  Balance := 0;
end;

{ TGLiteAvlTree.TEnumerator }

function TGLiteAvlTree.TEnumerator.GetCurrent: PEntry;
begin
  Result := @FTree^.FNodeList[FCurrNode].Data;
end;

procedure TGLiteAvlTree.TEnumerator.Init(aTree: PAvlTree);
begin
  FTree := aTree;
  FFirstNode := aTree^.Lowest;
  Reset;
end;

function TGLiteAvlTree.TEnumerator.MoveNext: Boolean;
var
  NextNode: SizeInt = NULL_INDEX;
begin
  if FCurrNode <> NULL_INDEX then
    NextNode := FTree^.Successor(FCurrNode)
  else
    if not FInCycle then
      begin
        NextNode := FFirstNode;
        FInCycle := True;
      end;
  Result := NextNode <> NULL_INDEX;
  if Result then
    FCurrNode := NextNode;
end;

procedure TGLiteAvlTree.TEnumerator.Reset;
begin
  FCurrNode := NULL_INDEX;
  FInCycle := False;
end;

{ TGLiteAvlTree }

function TGLiteAvlTree.GetCapacity: SizeInt;
begin
  Result := System.Length(FNodeList);
end;

procedure TGLiteAvlTree.Expand(aValue: SizeInt);
begin
  //there aValue > Capacity
  if aValue <= DEFAULT_CONTAINER_CAPACITY then
    System.SetLength(FNodeList, DEFAULT_CONTAINER_CAPACITY)
  else
    if aValue <= MAX_CAPACITY then
      begin
        aValue := LGUtils.RoundUpTwoPower(aValue);
        System.SetLength(FNodeList, aValue);
      end
    else
      CapacityExceedError(aValue);
end;

function TGLiteAvlTree.NewNode: SizeInt;
begin
  if Count = Capacity then
    Expand(Succ(Count));
  Result := Count;
  FNodeList[Result].ClearLinks;
  Inc(FCount);
end;

procedure TGLiteAvlTree.FreeNode(aNode: SizeInt);
var
  Last, Parent, Child: SizeInt;
begin
  Last := Pred(Count);
  FNodeList[aNode].Data := Default(TEntry);
  if aNode < Last then
    begin
      System.Move(FNodeList[Last], FNodeList[aNode], NODE_SIZE);
      System.FillChar(FNodeList[Last].Data , SizeOf(TEntry), 0); //////////////
      if Root = Last then
        FRoot := aNode;
      Parent := FNodeList[aNode].Parent;
      if Parent <> NULL_INDEX then
        begin
          if FNodeList[Parent].Left = Last then
            FNodeList[Parent].Left := aNode
          else
            FNodeList[Parent].Right := aNode;
        end;
      Child := FNodeList[aNode].Left;
      if Child <> NULL_INDEX then
        FNodeList[Child].Parent := aNode;
      Child := FNodeList[aNode].Right;
      if Child <> NULL_INDEX then
        FNodeList[Child].Parent := aNode;
    end;
  Dec(FCount);
end;

function TGLiteAvlTree.Successor(aNode: SizeInt): SizeInt;
var
  Parent: SizeInt;
begin
  if aNode = NULL_INDEX then
    exit(aNode);
  Result := FNodeList[aNode].Right;
  if Result <> NULL_INDEX then
    while FNodeList[Result].Left <> NULL_INDEX do
      Result := FNodeList[Result].Left
  else
    begin
      Result := aNode;
      Parent := FNodeList[aNode].Parent;
      while (Parent <> NULL_INDEX) and (FNodeList[Parent].Right = Result) do
        begin
          Result := FNodeList[Result].Parent;
          Parent := FNodeList[Result].Parent;
        end;
      Result := Parent;
    end;
end;

function TGLiteAvlTree.Predecessor(aNode: SizeInt): SizeInt;
var
  Parent: SizeInt;
begin
  if aNode = NULL_INDEX then
    exit(aNode);
  Result := FNodeList[aNode].Left;
  if Result <> NULL_INDEX then
    while FNodeList[Result].Right <> NULL_INDEX do
      Result := FNodeList[Result].Right
  else
    begin
      Result := aNode;
      Parent := FNodeList[aNode].Parent;
      while (Parent <> NULL_INDEX) and (FNodeList[Parent].Left = Result) do
        begin
          Result := FNodeList[Result].Parent;
          Parent := FNodeList[Result].Parent;
        end;
      Result := Parent;
    end;
end;

procedure TGLiteAvlTree.SwapBalance(L, R: SizeInt);
var
  b: ShortInt;
begin
  b := FNodeList[L].Balance;
  FNodeList[L].Balance := FNodeList[R].Balance;
  FNodeList[R].Balance := b;
end;

function TGLiteAvlTree.GetHighest: SizeInt;
var
  Curr: SizeInt;
begin
  Result := Root;
  if Result <> NULL_INDEX then
    begin
      Curr := FNodeList[Result].Right;
      while Curr <> NULL_INDEX do
        begin
          Result := Curr;
          Curr := FNodeList[Curr].Right;
        end;
    end;
end;

function TGLiteAvlTree.GetLowest: SizeInt;
var
  Curr: SizeInt;
begin
  Result := Root;
  if Result <> NULL_INDEX then
    begin
      Curr := FNodeList[Result].Left;
      while Curr <> NULL_INDEX do
        begin
          Result := Curr;
          Curr := FNodeList[Curr].Left;
        end;
    end;
    while FNodeList[Result].Left <> NULL_INDEX do
      Result := FNodeList[Result].Left;
end;

function TGLiteAvlTree.FindInsertPos(constref aKey: TKey): SizeInt;
var
  Curr: SizeInt;
begin
  Result := Root;
  while Result <> NULL_INDEX do
    if TKeyCmpRel.Compare(aKey, FNodeList[Result].Data.Key) < 0 then
      begin
        Curr := FNodeList[Result].Left;
        if Curr <> NULL_INDEX then
          Result := Curr
        else
          break;
      end
    else
      begin
        Curr := FNodeList[Result].Right;
        if Curr <> NULL_INDEX then
          Result := Curr
        else
          break;
      end;
end;

function TGLiteAvlTree.FindNode(constref aKey: TKey; out aInsertPos: SizeInt): SizeInt;
var
  Cmp: SizeInt;
begin
  Result := Root;
  aInsertPos := NULL_INDEX;
  while Result <> NULL_INDEX do
    begin
      Cmp := TKeyCmpRel.Compare(aKey, FNodeList[Result].Data.Key);
      aInsertPos := Result;
      if Cmp < 0 then
        Result := FNodeList[Result].Left
      else
        if Cmp > 0 then
          Result := FNodeList[Result].Right
        else
          break;
    end;
end;

procedure TGLiteAvlTree.ReplaceWithSuccessor(aNode: SizeInt);
var
  SuccNode, OldParent, OldLeft, OldRight,
  OldSuccParent, OldSuccLeft, OldSuccRight: SizeInt;
begin
  SuccNode := Successor(aNode);
  SwapBalance(aNode, SuccNode);

  OldParent := FNodeList[aNode].Parent;
  OldLeft := FNodeList[aNode].Left;
  OldRight := FNodeList[aNode].Right;
  OldSuccParent := FNodeList[SuccNode].Parent;
  OldSuccLeft := FNodeList[SuccNode].Left;
  OldSuccRight := FNodeList[SuccNode].Right;

  if OldParent <> NULL_INDEX then
    begin
      if FNodeList[OldParent].Left = aNode then
        FNodeList[OldParent].Left := SuccNode
      else
        FNodeList[OldParent].Right := SuccNode;
    end
  else
    FRoot := SuccNode;

  FNodeList[SuccNode].Parent := OldParent;

  if OldSuccParent <> aNode then
    begin
      if FNodeList[OldSuccParent].Left = SuccNode then
        FNodeList[OldSuccParent].Left := aNode
      else
        FNodeList[OldSuccParent].Right := aNode;
      FNodeList[SuccNode].Right := OldRight;
      FNodeList[aNode].Parent := OldSuccParent;
      if OldRight <> NULL_INDEX then
        FNodeList[OldRight].Parent := SuccNode;
    end
  else
    begin
      FNodeList[SuccNode].Right := aNode;
      FNodeList[aNode].Parent := SuccNode;
    end;

  FNodeList[aNode].Left := OldSuccLeft;
  if OldSuccLeft <> NULL_INDEX then
    FNodeList[OldSuccLeft].Parent := aNode;
  FNodeList[aNode].Right := OldSuccRight;
  if OldSuccRight <> NULL_INDEX then
    FNodeList[OldSuccRight].Parent := aNode;
  FNodeList[SuccNode].Left := OldLeft;
  if OldLeft <> NULL_INDEX then
    FNodeList[OldLeft].Parent := SuccNode;
end;

procedure TGLiteAvlTree.RotateLeft(aNode: SizeInt);
var
  OldParent, OldRight, OldRightLeft: SizeInt;
begin
  OldRight := FNodeList[aNode].Right;
  OldRightLeft := FNodeList[OldRight].Left;
  OldParent := FNodeList[aNode].Parent;
  if OldParent <> NULL_INDEX then
    begin
      if FNodeList[OldParent].Left = aNode then
        FNodeList[OldParent].Left := OldRight
      else
        FNodeList[OldParent].Right := OldRight;
    end
  else
    FRoot := OldRight;
  FNodeList[OldRight].Parent := OldParent;
  FNodeList[aNode].Parent := OldRight;
  FNodeList[aNode].Right := OldRightLeft;
  if OldRightLeft <> NULL_INDEX then
    FNodeList[OldRightLeft].Parent := aNode;
  FNodeList[OldRight].Left := aNode;
end;

procedure TGLiteAvlTree.RotateRight(aNode: SizeInt);
var
  OldParent, OldLeft, OldLeftRight: SizeInt;
begin
  OldLeft := FNodeList[aNode].Left;
  OldLeftRight := FNodeList[OldLeft].Right;
  OldParent := FNodeList[aNode].Parent;
  if OldParent <> NULL_INDEX then
    begin
      if FNodeList[OldParent].Left = aNode then
        FNodeList[OldParent].Left := OldLeft
      else
        FNodeList[OldParent].Right := OldLeft;
    end
  else
    FRoot := OldLeft;
  FNodeList[OldLeft].Parent := OldParent;
  FNodeList[aNode].Parent := OldLeft;
  FNodeList[aNode].Left := OldLeftRight;
  if OldLeftRight <> NULL_INDEX then
    FNodeList[OldLeftRight].Parent := aNode;
  FNodeList[OldLeft].Right := aNode;
end;

procedure TGLiteAvlTree.InsertNode(aNode: SizeInt);
var
  Parent: SizeInt;
begin
  if FRoot <> NULL_INDEX then
    begin
      Parent := FindInsertPos(FNodeList[aNode].Data.Key);
      FNodeList[aNode].Parent := Parent;
      if TKeyCmpRel.Compare(FNodeList[aNode].Data.Key, FNodeList[Parent].Data.Key) < 0 then
        FNodeList[Parent].Left := aNode
      else
        FNodeList[Parent].Right := aNode;
      BalanceAfterInsert(aNode);
    end
  else
    begin
      FRoot := aNode;
      FNodeList[aNode].Parent := NULL_INDEX;
    end;
end;

procedure TGLiteAvlTree.InsertNodeAt(aNode, aParent: SizeInt);
begin
  if aParent <> NULL_INDEX then
    begin
      FNodeList[aNode].Parent := aParent;
      if TKeyCmpRel.Compare(FNodeList[aNode].Data.Key, FNodeList[aParent].Data.Key) < 0 then
        FNodeList[aParent].Left := aNode
      else
        FNodeList[aParent].Right := aNode;
      BalanceAfterInsert(aNode);
    end
  else
    begin
      FRoot := aNode;
      FNodeList[aNode].Parent := NULL_INDEX;
    end;
end;

procedure TGLiteAvlTree.BalanceAfterInsert(aNode: SizeInt);
var
  OldParent, OldChild: SizeInt;
begin
  OldParent := FNodeList[aNode].Parent;
  while OldParent <> NULL_INDEX do
    if FNodeList[OldParent].Left = aNode then //aNode is left child => we must decrease OldParent.Balance
      case FNodeList[OldParent].Balance of
        -1:
          begin
            if FNodeList[aNode].Balance = -1 then
              begin
                RotateRight(OldParent);
                FNodeList[aNode].Balance := 0;
                FNodeList[OldParent].Balance := 0;
              end
            else
              begin
                OldChild := FNodeList[aNode].Right;
                RotateLeft(aNode);
                RotateRight(OldParent);
                case FNodeList[OldChild].Balance of
                  -1:
                    begin
                      FNodeList[aNode].Balance := 0;
                      FNodeList[OldParent].Balance := 1;
                    end;
                  0:
                    begin
                      FNodeList[aNode].Balance := 0;
                      FNodeList[OldParent].Balance := 0
                    end;
                else //1
                  FNodeList[aNode].Balance := -1;
                  FNodeList[OldParent].Balance := 0;
                end;
                FNodeList[OldChild].Balance := 0;
              end;
            break;
          end;
        0:
          begin
            FNodeList[OldParent].Balance := -1;
            aNode := OldParent;
            OldParent := FNodeList[aNode].Parent;
          end;
      else //1
        FNodeList[OldParent].Balance := 0;
        break;
      end
    else  // aNode is right child => we must increase OldParent.Balance
      case FNodeList[OldParent].Balance of
       -1:
         begin
           FNodeList[OldParent].Balance := 0;
           break;
         end;
        0:
          begin
            FNodeList[OldParent].Balance := 1;
            aNode := OldParent;
            OldParent := FNodeList[aNode].Parent;
          end;
      else // 1
        if FNodeList[aNode].Balance = 1 then
          begin
            RotateLeft(OldParent);
            FNodeList[aNode].Balance := 0;
            FNodeList[OldParent].Balance := 0;
          end
        else
          begin
            OldChild := FNodeList[aNode].Left;
            RotateRight(aNode);
            RotateLeft(OldParent);
            case FNodeList[OldChild].Balance of
              -1:
                begin
                  FNodeList[aNode].Balance := 1;
                  FNodeList[OldParent].Balance := 0;
                end;
              0:
                begin
                  FNodeList[aNode].Balance := 0;
                  FNodeList[OldParent].Balance := 0;
                end;
            else  //1
              FNodeList[aNode].Balance := 0;
              FNodeList[OldParent].Balance := -1
            end;
            FNodeList[OldChild].Balance := 0;
          end;
        break;
      end;
end;

procedure TGLiteAvlTree.RemoveNode(aNode: SizeInt);
var
  OldParent, Child: SizeInt;
begin
  if (FNodeList[aNode].Left <> NULL_INDEX) and (FNodeList[aNode].Right <> NULL_INDEX) then
    ReplaceWithSuccessor(aNode);
  OldParent := FNodeList[aNode].Parent;
  FNodeList[aNode].Parent := NULL_INDEX;
  if FNodeList[aNode].Left <> NULL_INDEX then
    Child := FNodeList[aNode].Left
  else
    Child := FNodeList[aNode].Right;
  if Child <> NULL_INDEX then
    FNodeList[Child].Parent := OldParent;
  if OldParent <> NULL_INDEX then
    begin
      if FNodeList[OldParent].Left = aNode then
        begin
          FNodeList[OldParent].Left := Child;
          BalanceAfterRemove(OldParent, Succ(FNodeList[OldParent].Balance));
        end
      else
        begin
          FNodeList[OldParent].Right := Child;
          BalanceAfterRemove(OldParent, Pred(FNodeList[OldParent].Balance));
        end;
    end
  else
    FRoot := Child;
  FreeNode(aNode);
end;

procedure TGLiteAvlTree.BalanceAfterRemove(aNode: SizeInt; aNewBalance: ShortInt);
var
  OldParent, Child, ChildOfChild: SizeInt;
begin
  while aNode <> NULL_INDEX do
    begin
      OldParent := FNodeList[aNode].Parent;
      case aNewBalance of
        -2:
          begin
            Child := FNodeList[aNode].Left;
            if FNodeList[Child].Balance <= 0 then
              begin
                RotateRight(aNode);
                FNodeList[aNode].Balance := -Succ(FNodeList[Child].Balance);
                aNewBalance := Succ(FNodeList[Child].Balance);
                aNode := Child;
                FNodeList[Child].Balance := aNewBalance;
              end
            else
              begin
                ChildOfChild := FNodeList[Child].Right;
                RotateLeft(Child);
                RotateRight(aNode);
                case FNodeList[ChildOfChild].Balance of
                  -1:
                    begin
                      FNodeList[aNode].Balance := 1;
                      FNodeList[Child].Balance := 0;
                    end;
                   0:
                     begin
                       FNodeList[aNode].Balance := 0;
                       FNodeList[Child].Balance := 0;
                     end;
                else // 1
                  FNodeList[aNode].Balance := 0;
                  FNodeList[Child].Balance := -1;
                end;
                aNode := ChildOfChild;
                aNewBalance := 0;
                FNodeList[ChildOfChild].Balance := 0;
              end;
          end;
        -1:
          begin
            FNodeList[aNode].Balance := aNewBalance;
            break;
          end;
         0:
           begin
             FNodeList[aNode].Balance := aNewBalance;
             if OldParent <> NULL_INDEX then
               begin
                 if FNodeList[OldParent].Left = aNode then
                   aNewBalance := Succ(FNodeList[OldParent].Balance)
                 else
                   aNewBalance := Pred(FNodeList[OldParent].Balance);
                 aNode := OldParent;
               end
             else
               break;
           end;
         1:
           begin
             FNodeList[aNode].Balance := aNewBalance;
             break;
           end;
         2:
           begin
             Child := FNodeList[aNode].Right;
             if FNodeList[Child].Balance >= 0 then
               begin
                 RotateLeft(aNode);
                 FNodeList[aNode].Balance := ShortInt(1) - FNodeList[Child].Balance;
                 aNewBalance := Pred(FNodeList[Child].Balance);
                 aNode := Child;
                 FNodeList[Child].Balance := aNewBalance;
               end
             else
               begin
                 ChildOfChild := FNodeList[Child].Left;
                 RotateRight(Child);
                 RotateLeft(aNode);
                 case FNodeList[ChildOfChild].Balance of
                   -1:
                     begin
                       FNodeList[aNode].Balance := 0;
                       FNodeList[Child].Balance := 1;
                     end;
                    0:
                      begin
                        FNodeList[aNode].Balance := 0;
                        FNodeList[Child].Balance := 0;
                      end;
                 else // 1
                   FNodeList[aNode].Balance := -1;
                   FNodeList[Child].Balance := 0;
                 end;
                 FNodeList[ChildOfChild].Balance := 0;
                 aNode := ChildOfChild;
                 aNewBalance := 0;
               end;
           end;
      end;
    end;
end;

class procedure TGLiteAvlTree.CapacityExceedError(aValue: SizeInt);
begin
  raise ELGCapacityExceed.CreateFmt(SECapacityExceedFmt, [aValue]);
end;

class constructor TGLiteAvlTree.Init;
begin
{$PUSH}{$J+}
  MAX_CAPACITY := LGUtils.RoundUpTwoPower(MAX_CAPACITY);
{$POP}
end;

class operator TGLiteAvlTree.Initialize(var t: TGLiteAvlTree);
begin
  t.FRoot := NULL_INDEX;
  t.FCount := 0;
end;

class operator TGLiteAvlTree.Copy(constref aSrc: TGLiteAvlTree; var aDst: TGLiteAvlTree);
begin
  aDst.FNodeList := System.Copy(aSrc.FNodeList);
  aDst.FRoot := aSrc.Root;
  aDst.FCount := aSrc.Count;
end;

function TGLiteAvlTree.GetEnumerator: TEnumerator;
begin
  Result.Init(@Self);
end;

procedure TGLiteAvlTree.Clear;
begin
  FNodeList := nil;;
  FRoot := NULL_INDEX;
  FCount := 0;
end;

procedure TGLiteAvlTree.EnsureCapacity(aValue: SizeInt);
begin
  if aValue > Capacity then
    Expand(aValue);
end;

procedure TGLiteAvlTree.TrimToFit;
begin
  System.SetLength(FNodeList, Count);
end;

function TGLiteAvlTree.FindOrAdd(constref aKey: TKey; out e: PEntry): Boolean;
var
  Node, ParentNode: SizeInt;
begin
  Node := FindNode(aKey, ParentNode);
  Result := Node <> NULL_INDEX;
  if not Result then
    begin
      Node := NewNode;
      FNodeList[Node].Data.Key := aKey;
      InsertNodeAt(Node, ParentNode);
    end;
  e := @FNodeList[Node].Data;
end;

function TGLiteAvlTree.Find(constref aKey: TKey): PEntry;
var
  Node, ParentNode: SizeInt;
begin
  Node := FindNode(aKey, ParentNode);
  if Node <> NULL_INDEX then
    Result :=  @FNodeList[Node].Data
  else
    Result := nil;
end;

function TGLiteAvlTree.Remove(constref aKey: TKey): Boolean;
var
  Node, ParentNode: SizeInt;
begin
  Node := FindNode(aKey, ParentNode);
  Result := Node <> NULL_INDEX;
  if Result then
    RemoveNode(Node);
end;

procedure TGLiteAvlTree.RemoveAt(aIndex: SizeInt);
begin
  if aIndex <> NULL_INDEX then
    RemoveNode(aIndex);
end;

function TGLiteAvlTree.FindLess(constref aKey: TKey): SizeInt;
var
  CurrNode: SizeInt;
begin
  CurrNode := Root;
  Result := NULL_INDEX;
  while CurrNode <> NULL_INDEX do
    if TKeyCmpRel.Compare(aKey, FNodeList[CurrNode].Data.Key) > 0 then
      begin
        Result := CurrNode;
        CurrNode := FNodeList[CurrNode].Right;
      end
    else
      CurrNode := FNodeList[CurrNode].Left;
end;

function TGLiteAvlTree.FindLessOrEqual(constref aKey: TKey): SizeInt;
var
  CurrNode: SizeInt;
begin
  CurrNode := Root;
  Result := NULL_INDEX;
  while CurrNode <> NULL_INDEX do
    if TKeyCmpRel.Compare(aKey, FNodeList[CurrNode].Data.Key) >= 0 then
      begin
        Result := CurrNode;
        CurrNode := FNodeList[CurrNode].Right;
      end
    else
      CurrNode := FNodeList[CurrNode].Left;
end;

function TGLiteAvlTree.FindGreater(constref aKey: TKey): SizeInt;
var
  CurrNode: SizeInt;
begin
  CurrNode := Root;
  Result := NULL_INDEX;
  while CurrNode <> NULL_INDEX do
    if TKeyCmpRel.Compare(aKey, FNodeList[CurrNode].Data.Key) < 0 then
      begin
        Result := CurrNode;
        CurrNode := FNodeList[CurrNode].Left;
      end
    else
      CurrNode := FNodeList[CurrNode].Right;
end;

function TGLiteAvlTree.FindGreaterOrEqual(constref aKey: TKey): SizeInt;
var
  CurrNode: SizeInt;
begin
  CurrNode := Root;
  Result := NULL_INDEX;
  while CurrNode <> NULL_INDEX do
    if TKeyCmpRel.Compare(aKey, FNodeList[CurrNode].Data.Key) <= 0 then
      begin
        Result := CurrNode;
        CurrNode := FNodeList[CurrNode].Left;
      end
    else
      CurrNode := FNodeList[CurrNode].Right;
end;

end.

