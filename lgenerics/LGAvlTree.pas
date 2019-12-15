{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Some generic AVL tree implementations.                                  *
*   (see https://en.wikipedia.org/wiki/AVL_tree)                            *
*                                                                           *
*   Copyright(c) 2018-2019 A.Koverdyaev(avk)                                *
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

  SysUtils, Math,
  LGUtils,
  {%H-}LGHelpers,
  LGStrConst;

{.$DEFINE AVLTREE_ENABLE_PAGEDNODEMANAGER}//if uncomment define, will use TGPageNodeManager
type

  generic TGCustomAvlTree<TKey, TEntry> = class abstract
  public
  type
    PNode = ^TNode;
    TNode = record
    private
    {$IFNDEF CPU16}
    const
      PTR_MASK = High(SizeUInt) xor 3;
    var
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
      property  NextLink: PNode read Left write Left; //for node manager
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
      FCurrNode,
      FFirstNode: PNode;
      FInCycle: Boolean;
    public
      constructor Create(aFirstNode: PNode);
      function  MoveNext: Boolean; virtual;
      procedure Reset;
      property  Current: PNode read FCurrNode;
    end;

  protected
  type
    TReverseEnumerator = class(TEnumerator)
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
    function  GetEnumeratorAt(constref aKey: TKey; aInclusive: Boolean): TEnumerator; inline;
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
        class function Compare([const[ref]] L, R: TKey): SizeInt; }
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
  const
    PTR_MASK = High(SizeUInt) xor 3;
  var
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
    property NextLink: PNode read Left write Left; //for node manager
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
      procedure Reset;
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
    procedure DisposeNode(aNode: PNode);
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

  { TGLiteAvlTree
      functor TKeyCmpRel (key comparision relation) must provide:
        class function Compare([const[ref]] L, R: TKey): SizeInt; }
  generic TGLiteAvlTree<TKey, TEntry, TKeyCmpRel> = record
  public
  type
    PEntry = ^TEntry;

    TNode = record
    private
      FParent: SizeInt;
      procedure ClearLinks; inline;
      function  GetBalance: SizeInt; inline;
      function  GetParent: SizeInt; inline;
      procedure SetBalance(aValue: SizeInt); inline;
      procedure SetParent(aValue: SizeInt); inline;
    public
      Left,
      Right: SizeInt;
      Data: TEntry;
      property Parent: SizeInt read GetParent write SetParent;
      property Balance: SizeInt read GetBalance write SetBalance;
    end;

    TNodeList = array of TNode;
    TAvlState = (asConsistent,    //Ok
                 asInvalidLink,   //Invalid BST link
                 asInvalidKey,    //Invalid BST key
                 asInvalidHDelta, //right-left height difference exceeds 1
                 asInvalidBalance //height delta/balance mismatch
                 );

  private
  const
{$IF DEFINED(CPU64)}
    SHIFT: SizeInt        = 62;
    MASK: SizeInt         = SizeInt($c000000000000000);
    NOT_MASK: SizeInt     = not SizeInt($c000000000000000);
    ZERO_BALANCE: SizeInt = SizeInt($8000000000000000);
{$ELSEIF DEFINED(CPU32)}
    SHIFT                 = 30;
    MASK: SizeInt         = SizeInt($c0000000);
    NOT_MASK: SizeInt     = not SizeInt($c0000000);
    ZERO_BALANCE: SizeInt = SizeInt($80000000);
{$ELSE}
    SHIFT: SizeInt        = 14;
    MASK: SizeInt         = SizeInt($c000);
    NOT_MASK: SizeInt     = not SizeInt($c000);
    ZERO_BALANCE: SizeInt = SizeInt($8000);
{$ENDIF}

  type
    PAvlTree  = ^TGLiteAvlTree;

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
      procedure Reset;
      property  Current: PEntry read GetCurrent;
    end;

  private
    FNodes: TNodeList;
    function  GetCapacity: SizeInt; inline;
    function  GetCount: SizeInt; inline;
    function  GetRoot: SizeInt; inline;
    procedure SetRoot(aValue: SizeInt); inline;
    procedure Expand(aValue: SizeInt);
    function  NewNode: SizeInt;
    procedure FreeNode(aNode: SizeInt);
    function  Successor(aNode: SizeInt): SizeInt;
    function  Predecessor(aNode: SizeInt): SizeInt;
    procedure SwapBalance(L, R: SizeInt); inline;
    function  GetHighest: SizeInt;
    function  GetLowest: SizeInt;
    function  GetNodeHeight(aNode: SizeInt): SizeInt;
    function  GetHeight: SizeInt;
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
    function  TestNodeState(aNode: SizeInt; var aState: TAvlState): SizeInt;
    property  Root: SizeInt read GetRoot write SetRoot;
    class operator Copy(constref aSrc: TGLiteAvlTree; var aDst: TGLiteAvlTree);
    class operator AddRef(var aTree: TGLiteAvlTree);
  public
    function  GetEnumerator: TEnumerator;
    procedure Clear; inline;
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
    function  CheckState: TAvlState;
    property  Count: SizeInt read GetCount;
    property  Capacity: SizeInt read GetCapacity;
    property  Lowest: SizeInt read GetLowest;
    property  Highest: SizeInt read GetHighest;
    property  NodeList: TNodeList read FNodes;
    property  Height: SizeInt read GetHeight;
  end;

implementation
{$B-}{$COPERATORS ON}

{$PUSH}{$Q-}
{$IFNDEF CPU16}
function TGCustomAvlTree.TNode.GetBalance: SizeInt;
begin
  Result := (FParent and 3) - 2;
end;

function TGCustomAvlTree.TNode.GetParent: PNode;
var
  p: SizeInt absolute Result;
begin
  p := FParent and SizeInt(PTR_MASK);
end;

procedure TGCustomAvlTree.TNode.SetBalance(aValue: SizeInt);
begin
  FParent := (FParent and SizeInt(PTR_MASK)) or ((aValue + 2) and 3);
end;

procedure TGCustomAvlTree.TNode.SetParent(aValue: PNode);
var
  p: SizeInt absolute aValue;
begin
  FParent := p or (FParent and 3);
end;

procedure TGCustomAvlTree.TNode.SwapBalance(aNode: PNode);
var
  b: SizeInt;
begin
  b := FParent and 3;
  FParent := (FParent and SizeInt(PTR_MASK)) or (aNode^.FParent and 3);
  aNode^.FParent := (aNode^.FParent and SizeInt(PTR_MASK)) or b;
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

constructor TGCustomAvlTree.TEnumerator.Create(aFirstNode: PNode);
begin
  FFirstNode := aFirstNode;
end;

function TGCustomAvlTree.TEnumerator.MoveNext: Boolean;
var
  Node: PNode = nil;
begin
  if FCurrNode <> nil then
    Node := FCurrNode^.Successor
  else
    if not FInCycle then
      begin
        FInCycle := True;
        Node := FFirstNode;
      end;
  if Node <> nil then
    begin
      FCurrNode := Node;
      exit(True);
    end;
  Result := False;
end;

procedure TGCustomAvlTree.TEnumerator.Reset;
begin
  FCurrNode := nil;
  FInCycle := False;
end;

{ TGCustomAvlTree.TReverseEnumerator }

function TGCustomAvlTree.TReverseEnumerator.MoveNext: Boolean;
var
  Node: PNode = nil;
begin
  if FCurrNode <> nil then
    Node := FCurrNode^.Predecessor
  else
    if not FInCycle then
      begin
        FInCycle := True;
        Node := FFirstNode;
      end;
  if Node <> nil then
    begin
      FCurrNode := Node;
      exit(True);
    end;
  Result := False;
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
      //if IsManagedType(TNode) then
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
  Result := TEnumerator.Create(Lowest);
end;

function TGCustomAvlTree.GetReverseEnumerator: TEnumerator;
begin
  Result := TReverseEnumerator.Create(Highest);
end;

function TGCustomAvlTree.GetEnumeratorAt(constref aKey: TKey; aInclusive: Boolean): TEnumerator;
begin
  if aInclusive then
    Result := TEnumerator.Create(FindGreaterOrEqual(aKey))
  else
    Result := TEnumerator.Create(FindGreater(aKey))
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
var
  r: SizeInt absolute Result;
begin
  r := FParent and SizeInt(PTR_MASK);
end;

procedure TGAvlTreeNode.SetBalance(aValue: SizeInt);
begin
  Assert(((aValue + 2) >= 1) and ((aValue + 2) <= 3),
    Format('Inconsistent input Balance value(%d) in '+{$I %CURRENTROUTINE%}, [aValue]));
  FParent := (FParent and SizeInt(PTR_MASK)) or ((aValue + 2) and 3);
end;

procedure TGAvlTreeNode.SetParent(aValue: PNode);
var
  p: SizeInt absolute aValue;
begin
  Assert(p and 3 = 0, Format('Unaligned input Parent value($%x) in '+{$I %CURRENTROUTINE%}, [aValue]));
  FParent := p or (FParent and 3);
end;

procedure TGAvlTreeNode.SwapBalance(aNode: PNode);
var
  b: SizeInt;
begin
  b := FParent and 3;
  FParent := (FParent and SizeInt(PTR_MASK)) or (aNode^.FParent and 3);
  aNode^.FParent := (aNode^.FParent and SizeInt(PTR_MASK)) or b;
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
  Left := 0;
  Right := 0;
  FParent := ZERO_BALANCE;
end;

function TGLiteAvlTree.TNode.GetBalance: SizeInt;
begin
  Result := (FParent shr SHIFT) - 2;
end;

function TGLiteAvlTree.TNode.GetParent: SizeInt;
begin
  Result := FParent and NOT_MASK;
end;

procedure TGLiteAvlTree.TNode.SetBalance(aValue: SizeInt);
begin
  FParent := (FParent and NOT_MASK) or ((aValue + 2) shl SHIFT);
end;

procedure TGLiteAvlTree.TNode.SetParent(aValue: SizeInt);
begin
  FParent := (FParent and MASK) or aValue;
end;

{ TGLiteAvlTree.TEnumerator }

function TGLiteAvlTree.TEnumerator.GetCurrent: PEntry;
begin
  Result := @FTree^.FNodes[FCurrNode].Data;
end;

procedure TGLiteAvlTree.TEnumerator.Init(aTree: PAvlTree);
begin
  FTree := aTree;
  FFirstNode := aTree^.Lowest;
  FCurrNode := 0;
  FInCycle := False;
end;

function TGLiteAvlTree.TEnumerator.MoveNext: Boolean;
var
  NextNode: SizeInt = 0;
begin
  if FCurrNode <> 0 then
    NextNode := FTree^.Successor(FCurrNode)
  else
    if not FInCycle then
      begin
        NextNode := FFirstNode;
        FInCycle := True;
      end;
  Result := NextNode <> 0;
  if Result then
    FCurrNode := NextNode;
end;

procedure TGLiteAvlTree.TEnumerator.Reset;
begin
  FCurrNode := 0;
  FInCycle := False;
end;

{ TGLiteAvlTree }

function TGLiteAvlTree.GetCapacity: SizeInt;
begin
  if FNodes = nil then exit(0);
  Result := System.High(FNodes);
end;

function TGLiteAvlTree.GetCount: SizeInt;
begin
  if FNodes = nil then exit(0);
  Result := FNodes[0].Left;
end;

function TGLiteAvlTree.GetRoot: SizeInt;
begin
  if FNodes = nil then exit(0);
  Result := FNodes[0].Right;
end;

procedure TGLiteAvlTree.SetRoot(aValue: SizeInt);
begin
  FNodes[0].Right := aValue;
end;

procedure TGLiteAvlTree.Expand(aValue: SizeInt);
var
  OldLen: SizeInt;
begin
  //there aValue > Capacity
  OldLen := System.Length(FNodes);
  if aValue < DEFAULT_CONTAINER_CAPACITY then
    System.SetLength(FNodes, DEFAULT_CONTAINER_CAPACITY)
  else
    if aValue < MAX_CONTAINER_SIZE div SizeOf(TNode) then
      System.SetLength(FNodes, LGUtils.RoundUpTwoPower(aValue))
    else
      raise ELGCapacityExceed.CreateFmt(SECapacityExceedFmt, [aValue]);
  if OldLen = 0 then
    begin
      FNodes[0].Left := 0;
      FNodes[0].Right := 0;
    end;
end;

function TGLiteAvlTree.NewNode: SizeInt;
begin
  if Count = Capacity then
    Expand(Count + 2);
  Result := Succ(Count);
  FNodes[Result].ClearLinks;
  Inc(FNodes[0].Left);
end;

procedure TGLiteAvlTree.FreeNode(aNode: SizeInt);
var
  Last, Parent, Child: SizeInt;
begin
  Last := Count;
  FNodes[aNode].Data := Default(TEntry);
  if aNode < Last then
    begin
      System.Move(FNodes[Last], FNodes[aNode], SizeOf(TNode));
      System.FillChar(FNodes[Last].Data , SizeOf(TEntry), 0); //////////////
      if Root = Last then
        Root := aNode;
      Parent := FNodes[aNode].Parent;
      if Parent <> 0 then
        begin
          if FNodes[Parent].Left = Last then
            FNodes[Parent].Left := aNode
          else
            FNodes[Parent].Right := aNode;
        end;
      Child := FNodes[aNode].Left;
      if Child <> 0 then
        FNodes[Child].Parent := aNode;
      Child := FNodes[aNode].Right;
      if Child <> 0 then
        FNodes[Child].Parent := aNode;
    end;
  Dec(FNodes[0].Left);
end;

function TGLiteAvlTree.Successor(aNode: SizeInt): SizeInt;
var
  Parent: SizeInt;
begin
  if aNode = 0 then
    exit(aNode);
  Result := FNodes[aNode].Right;
  if Result <> 0 then
    while FNodes[Result].Left <> 0 do
      Result := FNodes[Result].Left
  else
    begin
      Result := aNode;
      Parent := FNodes[aNode].Parent;
      while (Parent <> 0) and (FNodes[Parent].Right = Result) do
        begin
          Result := FNodes[Result].Parent;
          Parent := FNodes[Result].Parent;
        end;
      Result := Parent;
    end;
end;

function TGLiteAvlTree.Predecessor(aNode: SizeInt): SizeInt;
var
  Parent: SizeInt;
begin
  if aNode = 0 then
    exit(aNode);
  Result := FNodes[aNode].Left;
  if Result <> 0 then
    while FNodes[Result].Right <> 0 do
      Result := FNodes[Result].Right
  else
    begin
      Result := aNode;
      Parent := FNodes[aNode].Parent;
      while (Parent <> 0) and (FNodes[Parent].Left = Result) do
        begin
          Result := FNodes[Result].Parent;
          Parent := FNodes[Result].Parent;
        end;
      Result := Parent;
    end;
end;

procedure TGLiteAvlTree.SwapBalance(L, R: SizeInt);
var
  b: SizeInt;
begin
  b := FNodes[L].FParent and MASK;
  FNodes[L].FParent := (FNodes[L].FParent and NOT_MASK) or (FNodes[R].FParent and MASK);
  FNodes[R].FParent := (FNodes[R].FParent and NOT_MASK) or b;
end;

function TGLiteAvlTree.GetHighest: SizeInt;
var
  Curr: SizeInt;
begin
  Result := Root;
  if Result <> 0 then
    begin
      Curr := FNodes[Result].Right;
      while Curr <> 0 do
        begin
          Result := Curr;
          Curr := FNodes[Curr].Right;
        end;
    end;
end;

function TGLiteAvlTree.GetLowest: SizeInt;
var
  Curr: SizeInt;
begin
  Result := Root;
  if Result <> 0 then
    begin
      Curr := FNodes[Result].Left;
      while Curr <> 0 do
        begin
          Result := Curr;
          Curr := FNodes[Curr].Left;
        end;
    end;
end;

function TGLiteAvlTree.GetNodeHeight(aNode: SizeInt): SizeInt;
begin
  if aNode = 0 then exit(0);
  Result := Succ(Math.Max(GetNodeHeight(FNodes[aNode].Left), GetNodeHeight(FNodes[aNode].Right)));
end;

function TGLiteAvlTree.GetHeight: SizeInt;
begin
  if FNodes = nil then exit(0);
  Result := GetNodeHeight(Root);
end;

function TGLiteAvlTree.FindInsertPos(constref aKey: TKey): SizeInt;
var
  Curr: SizeInt;
begin
  Result := Root;
  while Result <> 0 do
    if TKeyCmpRel.Compare(aKey, FNodes[Result].Data.Key) < 0 then
      begin
        Curr := FNodes[Result].Left;
        if Curr <> 0 then
          Result := Curr
        else
          break;
      end
    else
      begin
        Curr := FNodes[Result].Right;
        if Curr <> 0 then
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
  aInsertPos := 0;
  while Result <> 0 do
    begin
      Cmp := TKeyCmpRel.Compare(aKey, FNodes[Result].Data.Key);
      aInsertPos := Result;
      if Cmp < 0 then
        Result := FNodes[Result].Left
      else
        if Cmp > 0 then
          Result := FNodes[Result].Right
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

  OldParent := FNodes[aNode].Parent;
  OldLeft := FNodes[aNode].Left;
  OldRight := FNodes[aNode].Right;
  OldSuccParent := FNodes[SuccNode].Parent;
  OldSuccLeft := FNodes[SuccNode].Left;
  OldSuccRight := FNodes[SuccNode].Right;

  if OldParent <> 0 then
    begin
      if FNodes[OldParent].Left = aNode then
        FNodes[OldParent].Left := SuccNode
      else
        FNodes[OldParent].Right := SuccNode;
    end
  else
    Root := SuccNode;

  FNodes[SuccNode].Parent := OldParent;

  if OldSuccParent <> aNode then
    begin
      if FNodes[OldSuccParent].Left = SuccNode then
        FNodes[OldSuccParent].Left := aNode
      else
        FNodes[OldSuccParent].Right := aNode;
      FNodes[SuccNode].Right := OldRight;
      FNodes[aNode].Parent := OldSuccParent;
      if OldRight <> 0 then
        FNodes[OldRight].Parent := SuccNode;
    end
  else
    begin
      FNodes[SuccNode].Right := aNode;
      FNodes[aNode].Parent := SuccNode;
    end;

  FNodes[aNode].Left := OldSuccLeft;
  if OldSuccLeft <> 0 then
    FNodes[OldSuccLeft].Parent := aNode;
  FNodes[aNode].Right := OldSuccRight;
  if OldSuccRight <> 0 then
    FNodes[OldSuccRight].Parent := aNode;
  FNodes[SuccNode].Left := OldLeft;
  if OldLeft <> 0 then
    FNodes[OldLeft].Parent := SuccNode;
end;

procedure TGLiteAvlTree.RotateLeft(aNode: SizeInt);
var
  OldParent, OldRight, OldRightLeft: SizeInt;
begin
  OldRight := FNodes[aNode].Right;
  OldRightLeft := FNodes[OldRight].Left;
  OldParent := FNodes[aNode].Parent;
  if OldParent <> 0 then
    begin
      if FNodes[OldParent].Left = aNode then
        FNodes[OldParent].Left := OldRight
      else
        FNodes[OldParent].Right := OldRight;
    end
  else
    Root := OldRight;
  FNodes[OldRight].Parent := OldParent;
  FNodes[aNode].Parent := OldRight;
  FNodes[aNode].Right := OldRightLeft;
  if OldRightLeft <> 0 then
    FNodes[OldRightLeft].Parent := aNode;
  FNodes[OldRight].Left := aNode;
end;

procedure TGLiteAvlTree.RotateRight(aNode: SizeInt);
var
  OldParent, OldLeft, OldLeftRight: SizeInt;
begin
  OldLeft := FNodes[aNode].Left;
  OldLeftRight := FNodes[OldLeft].Right;
  OldParent := FNodes[aNode].Parent;
  if OldParent <> 0 then
    begin
      if FNodes[OldParent].Left = aNode then
        FNodes[OldParent].Left := OldLeft
      else
        FNodes[OldParent].Right := OldLeft;
    end
  else
    Root := OldLeft;
  FNodes[OldLeft].Parent := OldParent;
  FNodes[aNode].Parent := OldLeft;
  FNodes[aNode].Left := OldLeftRight;
  if OldLeftRight <> 0 then
    FNodes[OldLeftRight].Parent := aNode;
  FNodes[OldLeft].Right := aNode;
end;

procedure TGLiteAvlTree.InsertNode(aNode: SizeInt);
var
  ParentNode: SizeInt;
begin
  if Root <> 0 then
    begin
      ParentNode := FindInsertPos(FNodes[aNode].Data.Key);
      FNodes[aNode].Parent := ParentNode;
      if TKeyCmpRel.Compare(FNodes[aNode].Data.Key, FNodes[ParentNode].Data.Key) < 0 then
        FNodes[ParentNode].Left := aNode
      else
        FNodes[ParentNode].Right := aNode;
      BalanceAfterInsert(aNode);
    end
  else
    Root := aNode;
end;

procedure TGLiteAvlTree.InsertNodeAt(aNode, aParent: SizeInt);
begin
  if aParent <> 0 then
    begin
      FNodes[aNode].Parent := aParent;
      if TKeyCmpRel.Compare(FNodes[aNode].Data.Key, FNodes[aParent].Data.Key) < 0 then
        FNodes[aParent].Left := aNode
      else
        FNodes[aParent].Right := aNode;
      BalanceAfterInsert(aNode);
    end
  else
    Root := aNode;
end;

procedure TGLiteAvlTree.BalanceAfterInsert(aNode: SizeInt);
var
  OldParent, OldChild: SizeInt;
begin
  OldParent := FNodes[aNode].Parent;
  while OldParent <> 0 do
    if FNodes[OldParent].Left = aNode then //aNode is left child => we must decrease OldParent.Balance
      case FNodes[OldParent].Balance of
        -1:
          begin
            if FNodes[aNode].Balance = -1 then
              begin
                RotateRight(OldParent);
                FNodes[aNode].Balance := 0;
                FNodes[OldParent].Balance := 0;
              end
            else
              begin
                OldChild := FNodes[aNode].Right;
                RotateLeft(aNode);
                RotateRight(OldParent);
                case FNodes[OldChild].Balance of
                  -1:
                    begin
                      FNodes[aNode].Balance := 0;
                      FNodes[OldParent].Balance := 1;
                    end;
                  0:
                    begin
                      FNodes[aNode].Balance := 0;
                      FNodes[OldParent].Balance := 0
                    end;
                else //1
                  FNodes[aNode].Balance := -1;
                  FNodes[OldParent].Balance := 0;
                end;
                FNodes[OldChild].Balance := 0;
              end;
            break;
          end;
        0:
          begin
            FNodes[OldParent].Balance := -1;
            aNode := OldParent;
            OldParent := FNodes[aNode].Parent;
          end;
      else //1
        FNodes[OldParent].Balance := 0;
        break;
      end
    else  // aNode is right child => we must increase OldParent.Balance
      case FNodes[OldParent].Balance of
       -1:
         begin
           FNodes[OldParent].Balance := 0;
           break;
         end;
        0:
          begin
            FNodes[OldParent].Balance := 1;
            aNode := OldParent;
            OldParent := FNodes[aNode].Parent;
          end;
      else // 1
        if FNodes[aNode].Balance = 1 then
          begin
            RotateLeft(OldParent);
            FNodes[aNode].Balance := 0;
            FNodes[OldParent].Balance := 0;
          end
        else
          begin
            OldChild := FNodes[aNode].Left;
            RotateRight(aNode);
            RotateLeft(OldParent);
            case FNodes[OldChild].Balance of
              -1:
                begin
                  FNodes[aNode].Balance := 1;
                  FNodes[OldParent].Balance := 0;
                end;
              0:
                begin
                  FNodes[aNode].Balance := 0;
                  FNodes[OldParent].Balance := 0;
                end;
            else  //1
              FNodes[aNode].Balance := 0;
              FNodes[OldParent].Balance := -1
            end;
            FNodes[OldChild].Balance := 0;
          end;
        break;
      end;
end;

procedure TGLiteAvlTree.RemoveNode(aNode: SizeInt);
var
  OldParent, Child: SizeInt;
begin
  if (FNodes[aNode].Left <> 0) and (FNodes[aNode].Right <> 0) then
    ReplaceWithSuccessor(aNode);
  OldParent := FNodes[aNode].Parent;
  FNodes[aNode].Parent := 0;
  if FNodes[aNode].Left <> 0 then
    Child := FNodes[aNode].Left
  else
    Child := FNodes[aNode].Right;
  if Child <> 0 then
    FNodes[Child].Parent := OldParent;
  if OldParent <> 0 then
    begin
      if FNodes[OldParent].Left = aNode then
        begin
          FNodes[OldParent].Left := Child;
          BalanceAfterRemove(OldParent, Succ(FNodes[OldParent].Balance));
        end
      else
        begin
          FNodes[OldParent].Right := Child;
          BalanceAfterRemove(OldParent, Pred(FNodes[OldParent].Balance));
        end;
    end
  else
    Root := Child;
  FreeNode(aNode);
end;

procedure TGLiteAvlTree.BalanceAfterRemove(aNode: SizeInt; aNewBalance: ShortInt);
var
  OldParent, Child, ChildOfChild: SizeInt;
begin
  while aNode <> 0 do
    begin
      OldParent := FNodes[aNode].Parent;
      case aNewBalance of
        -2:
          begin
            Child := FNodes[aNode].Left;
            if FNodes[Child].Balance <= 0 then
              begin
                RotateRight(aNode);
                FNodes[aNode].Balance := -Succ(FNodes[Child].Balance);
                aNewBalance := Succ(FNodes[Child].Balance);
                aNode := Child;
                FNodes[Child].Balance := aNewBalance;
              end
            else
              begin
                ChildOfChild := FNodes[Child].Right;
                RotateLeft(Child);
                RotateRight(aNode);
                case FNodes[ChildOfChild].Balance of
                  -1:
                    begin
                      FNodes[aNode].Balance := 1;
                      FNodes[Child].Balance := 0;
                    end;
                   0:
                     begin
                       FNodes[aNode].Balance := 0;
                       FNodes[Child].Balance := 0;
                     end;
                else // 1
                  FNodes[aNode].Balance := 0;
                  FNodes[Child].Balance := -1;
                end;
                aNode := ChildOfChild;
                aNewBalance := 0;
                FNodes[ChildOfChild].Balance := 0;
              end;
          end;
        -1:
          begin
            FNodes[aNode].Balance := aNewBalance;
            break;
          end;
         0:
           begin
             FNodes[aNode].Balance := aNewBalance;
             if OldParent <> 0 then
               begin
                 if FNodes[OldParent].Left = aNode then
                   aNewBalance := Succ(FNodes[OldParent].Balance)
                 else
                   aNewBalance := Pred(FNodes[OldParent].Balance);
                 aNode := OldParent;
               end
             else
               break;
           end;
         1:
           begin
             FNodes[aNode].Balance := aNewBalance;
             break;
           end;
         2:
           begin
             Child := FNodes[aNode].Right;
             if FNodes[Child].Balance >= 0 then
               begin
                 RotateLeft(aNode);
                 FNodes[aNode].Balance := ShortInt(1) - FNodes[Child].Balance;
                 aNewBalance := Pred(FNodes[Child].Balance);
                 aNode := Child;
                 FNodes[Child].Balance := aNewBalance;
               end
             else
               begin
                 ChildOfChild := FNodes[Child].Left;
                 RotateRight(Child);
                 RotateLeft(aNode);
                 case FNodes[ChildOfChild].Balance of
                   -1:
                     begin
                       FNodes[aNode].Balance := 0;
                       FNodes[Child].Balance := 1;
                     end;
                    0:
                      begin
                        FNodes[aNode].Balance := 0;
                        FNodes[Child].Balance := 0;
                      end;
                 else // 1
                   FNodes[aNode].Balance := -1;
                   FNodes[Child].Balance := 0;
                 end;
                 FNodes[ChildOfChild].Balance := 0;
                 aNode := ChildOfChild;
                 aNewBalance := 0;
               end;
           end;
      end;
    end;
end;

function TGLiteAvlTree.TestNodeState(aNode: SizeInt; var aState: TAvlState): SizeInt;
var
  LHeight, RHeight: SizeInt;
begin
  if (aNode = 0) or (aState <> asConsistent) then exit(0);
  if FNodes[aNode].Left <> 0 then
    begin
      if FNodes[FNodes[aNode].Left].Parent <> aNode then
        begin
          aState := asInvalidLink;
          exit(0);
        end;
      if TKeyCmpRel.Compare(FNodes[FNodes[aNode].Left].Data.Key, FNodes[aNode].Data.Key) >= 0 then
        begin
          aState := asInvalidKey;
          exit(0);
        end;
    end;
  if FNodes[aNode].Right <> 0 then
    begin
      if FNodes[FNodes[aNode].Right].Parent <> aNode then
        begin
          aState := asInvalidLink;
          exit(0);
        end;
      if TKeyCmpRel.Compare(FNodes[FNodes[aNode].Right].Data.Key, FNodes[aNode].Data.Key) < 0 then
        begin
          aState := asInvalidKey;
          exit(0);
        end;
    end;
  LHeight := TestNodeState(FNodes[aNode].Left, aState);
  RHeight := TestNodeState(FNodes[aNode].Right, aState);
  if aState <> asConsistent then exit(0);
  if Abs(RHeight - LHeight) > 1 then
    begin
      aState := asInvalidHDelta;
      exit(0);
    end;
  if FNodes[aNode].Balance <> (RHeight - LHeight) then
    begin
      aState := asInvalidBalance;
      exit(0);
    end;
  Result := Succ(Math.Max(LHeight, RHeight));
end;

class operator TGLiteAvlTree.Copy(constref aSrc: TGLiteAvlTree; var aDst: TGLiteAvlTree);
begin
  if @aSrc <> @aDst then
    aDst.FNodes := System.Copy(aSrc.FNodes);
end;

class operator TGLiteAvlTree.AddRef(var aTree: TGLiteAvlTree);
begin
  aTree.FNodes := System.Copy(aTree.FNodes);
end;

function TGLiteAvlTree.GetEnumerator: TEnumerator;
begin
  Result.Init(@Self);
end;

procedure TGLiteAvlTree.Clear;
begin
  FNodes := nil;
end;

procedure TGLiteAvlTree.EnsureCapacity(aValue: SizeInt);
begin
  if aValue > Capacity then
    Expand(aValue);
end;

procedure TGLiteAvlTree.TrimToFit;
begin
  if Count > 0 then
    System.SetLength(FNodes, Succ(Count))
  else
    Clear;
end;

function TGLiteAvlTree.FindOrAdd(constref aKey: TKey; out e: PEntry): Boolean;
var
  Node, ParentNode: SizeInt;
begin
  Node := FindNode(aKey, ParentNode);
  Result := Node <> 0;
  if not Result then
    begin
      Node := NewNode;
      FNodes[Node].Data.Key := aKey;
      InsertNodeAt(Node, ParentNode);
    end;
  e := @FNodes[Node].Data;
end;

function TGLiteAvlTree.Find(constref aKey: TKey): PEntry;
var
  Node, ParentNode: SizeInt;
begin
  Node := FindNode(aKey, ParentNode);
  if Node <> 0 then
    Result :=  @FNodes[Node].Data
  else
    Result := nil;
end;

function TGLiteAvlTree.Remove(constref aKey: TKey): Boolean;
var
  Node, ParentNode: SizeInt;
begin
  Node := FindNode(aKey, ParentNode);
  Result := Node <> 0;
  if Result then
    RemoveNode(Node);
end;

procedure TGLiteAvlTree.RemoveAt(aIndex: SizeInt);
begin
  if aIndex > 0 then
    RemoveNode(aIndex);
end;

function TGLiteAvlTree.FindLess(constref aKey: TKey): SizeInt;
var
  CurrNode: SizeInt;
begin
  CurrNode := Root;
  Result := 0;
  while CurrNode <> 0 do
    if TKeyCmpRel.Compare(aKey, FNodes[CurrNode].Data.Key) > 0 then
      begin
        Result := CurrNode;
        CurrNode := FNodes[CurrNode].Right;
      end
    else
      CurrNode := FNodes[CurrNode].Left;
end;

function TGLiteAvlTree.FindLessOrEqual(constref aKey: TKey): SizeInt;
var
  CurrNode: SizeInt;
begin
  CurrNode := Root;
  Result := 0;
  while CurrNode <> 0 do
    if TKeyCmpRel.Compare(aKey, FNodes[CurrNode].Data.Key) >= 0 then
      begin
        Result := CurrNode;
        CurrNode := FNodes[CurrNode].Right;
      end
    else
      CurrNode := FNodes[CurrNode].Left;
end;

function TGLiteAvlTree.FindGreater(constref aKey: TKey): SizeInt;
var
  CurrNode: SizeInt;
begin
  CurrNode := Root;
  Result := 0;
  while CurrNode <> 0 do
    if TKeyCmpRel.Compare(aKey, FNodes[CurrNode].Data.Key) < 0 then
      begin
        Result := CurrNode;
        CurrNode := FNodes[CurrNode].Left;
      end
    else
      CurrNode := FNodes[CurrNode].Right;
end;

function TGLiteAvlTree.FindGreaterOrEqual(constref aKey: TKey): SizeInt;
var
  CurrNode: SizeInt;
begin
  CurrNode := Root;
  Result := 0;
  while CurrNode <> 0 do
    if TKeyCmpRel.Compare(aKey, FNodes[CurrNode].Data.Key) <= 0 then
      begin
        Result := CurrNode;
        CurrNode := FNodes[CurrNode].Left;
      end
    else
      CurrNode := FNodes[CurrNode].Right;
end;

function TGLiteAvlTree.CheckState: TAvlState;
begin
  Result := asConsistent;
  if Root <> 0 then
    TestNodeState(Root, Result);
end;

end.

