unit LGRootTreeTest;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGArrayHelpers,
  LGStack,
  LGRootTree;

type

  { TTestLiteRootTree }

  TTestLiteRootTree = class(TTestCase)
  private
  type
    TTestTree = specialize TGLiteRootedTree<Integer>;
    TTreeNode = TTestTree.TTreeNode;

    TTestObj  = class
    private
      FCounter: PInteger;
    public
      constructor Create(aCounter: PInteger);
      destructor Destroy; override;
    end;

    TObjTestTree = specialize TGLiteObjRootedTree<TTestObj>;
    TObjTreeNode = TObjTestTree.TTreeNode;

    procedure CreateTestTree11(out aTree: TTestTree);
    procedure CreateTestTree10(out aTree: TTestTree);
    procedure CreateRandomTree(out aTree: TTestTree);
    function  FindNode(aRoot: TTreeNode; aValue: Integer): TTreeNode;
  published
    procedure TreeNodeEnumerator;
    procedure TreeNodeClear;
    procedure TreeNodeRoot;
    procedure TreeNodeIsLeaf;
    procedure TreeNodeDegree;
    procedure TreeNodeLevel;
    procedure TreeNodeHeight;
    procedure TreeNodeWidth;
    procedure TreeNodeSize;
    procedure TreeNodeGetParent;
    procedure TreeNodeGetFirstChild;
    procedure TreeNodeGetLastChild;
    procedure TreeNodeGetSibling;
    procedure TreeNodeInSameTree;
    procedure TreeNodeDistanceFrom;
    procedure TreeNodeIsDescendant;
    procedure TreeNodeIsAncestor;
    procedure TreeNodeGetLca;
    procedure TreeNodeAddChild;
    procedure AddLastChild;
    procedure TreeAsCallParam;
    procedure TreeNodeAddChildTree;
    procedure TreeNodeAddLastChildTree;
    procedure TreeNodeExtract;
    procedure TreeNodeAncestors;
    procedure TreeNodeChildren;
    procedure TreeNodePreorderTraversal;
    procedure TreeNodePostorderTraversal;
    procedure TreeNodeBfsTraversal;
    procedure TreeNodeDfsTraversalR2L;
    procedure TreeNodeDfsTraversalL2R;
    procedure TreeEnumerator;
    procedure TreeAssignment;
    procedure TreeClear;
    procedure TreeOwnsNode;
    procedure TreeRemoveNode;
    procedure ObjTreeClear;
    procedure ObjTreeRemoveNode;
  end;

implementation

{ TTestLiteRootTree.TTestObj }

constructor TTestLiteRootTree.TTestObj.Create(aCounter: PInteger);
begin
  FCounter := aCounter;
end;

destructor TTestLiteRootTree.TTestObj.Destroy;
begin
  if FCounter <> nil then
    Inc(FCounter^);
  inherited;
end;

procedure TTestLiteRootTree.CreateTestTree11(out aTree: TTestTree);
var
  L, R: TTreeNode;
begin
{
       0
      / \
 3 - 1   2 - 4
    / \  |
   5   6 7
   |   |
   8   9
       |
      10
}
  aTree.Root.Value := 0;
  R := aTree.Root.AddChild(2);
  L := aTree.Root.AddChild(1);
  R.AddChild(4);
  R.AddChild(7);
  L.AddChild(6).AddChild(9).AddChild(10);
  L.AddChild(5).AddChild(8);
  L.AddChild(3);
end;

procedure TTestLiteRootTree.CreateTestTree10(out aTree: TTestTree);
var
  L, R: TTreeNode;
begin
{
        0
      / | \
    1   2   3
  / | \   / | \
 4  5  6 7  8  9
}

  aTree.Root.Value := 0;
  R := aTree.Root.AddChild(3);
  aTree.Root.AddChild(2);
  L := aTree.Root.AddChild(1);
  R.AddChild(9);
  R.AddChild(8);
  R.AddChild(7);
  L.AddChild(6);
  L.AddChild(5);
  L.AddChild(4);
end;

procedure TTestLiteRootTree.CreateRandomTree(out aTree: TTestTree);
type
  TStack = specialize TGLiteStack<TTreeNode>;
var
  s1, s2: TStack;
  Node: TTreeNode;
  pSrc, pDst: ^TStack;
  I, J: Integer;
  tmp: Pointer;
begin
  pSrc := @s1;
  pDst := @s2;
  pSrc^.Push(aTree.Root);
  for I := 1 to 7 do
    begin
      while pSrc^.TryPop(Node) do
        for J := 1 to Random(7) do
          pDst^.Push(Node.AddChild);
      tmp := pSrc;
      pSrc := pDst;
      pDst := tmp;
    end;
end;

function TTestLiteRootTree.FindNode(aRoot: TTreeNode; aValue: Integer): TTreeNode;
begin
  for Result in aRoot.PreorderTraversal do
    if Result.Value = aValue then
      exit;
  Result.Clear;
end;

procedure TTestLiteRootTree.TreeNodeEnumerator;
var
  Tree: TTestTree;
  Node: TTreeNode;
  I: Integer;
begin
  I := 0;
  for Node in Tree.Root do
    Inc(I);
  AssertTrue(I = 1);
  CreateTestTree11(Tree);
  I := 0;
  for Node in Tree.Root do
    Inc(I);
  AssertTrue(I = 11);
end;

procedure TTestLiteRootTree.TreeNodeClear;
var
  Tree: TTestTree;
  Node: TTreeNode;
begin
  Node := Tree.Root;
  AssertTrue(Node.Assigned);
  Node.Clear;
  AssertFalse(Node.Assigned);
end;

procedure TTestLiteRootTree.TreeNodeRoot;
var
  Tree: TTestTree;
  Node, Root: TTreeNode;
  I: Integer;
begin
  Root := {%H-}Tree.Root;
  AssertTrue(Root.Assigned);
  AssertTrue(Root.IsRoot);
  Node := Root;
  for I := 1 to 10 do
    begin
      Node := Node.AddChild;
      AssertFalse(Node.IsRoot);
      AssertTrue(Node.Root = Root);
    end;
end;

procedure TTestLiteRootTree.TreeNodeIsLeaf;
var
  Tree: TTestTree;
  Node, Next: TTreeNode;
  I: Integer;
begin
  Node := {%H-}Tree.Root;
  AssertTrue(Node.IsLeaf);
  for I := 1 to 10 do
    begin
      Next := Node.AddChild;
      AssertFalse(Node.IsLeaf);
      AssertTrue(Next.IsLeaf);
      Node := Next;
    end;
end;

procedure TTestLiteRootTree.TreeNodeDegree;
var
  Tree: TTestTree;
  Node: TTreeNode;
  I: Integer;
begin
  AssertTrue(Tree.Root.Degree = 0);
  for I := 1 to 10 do
    begin
      Node := Tree.Root.AddChild;
      AssertTrue(Tree.Root.Degree = I);
      AssertTrue(Node.Degree = 0);
      Node.AddChild;
      AssertTrue(Node.Degree = 1);
    end;
end;

procedure TTestLiteRootTree.TreeNodeLevel;
var
  Tree: TTestTree;
  Node: TTreeNode;
  I: Integer;
begin
  Node := {%H-}Tree.Root;
  AssertTrue(Node.Level = 0);
  for I := 1 to 10 do
    begin
      Node := Node.AddChild;
      AssertTrue(Node.Level = I);
    end;
end;

procedure TTestLiteRootTree.TreeNodeHeight;
var
  Tree: TTestTree;
  Node: TTreeNode;
begin
  AssertTrue(Tree.Root.Height = 0);
  CreateTestTree11(Tree);

  AssertTrue(Tree.Root.Height = 4);
  Node := FindNode(Tree.Root, 1);
  AssertTrue(Node.Height = 3);
  Node := FindNode(Tree.Root, 2);
  AssertTrue(Node.Height = 1);
  Node := FindNode(Tree.Root, 3);
  AssertTrue(Node.Height = 0);
  Node := FindNode(Tree.Root, 4);
  AssertTrue(Node.Height = 0);
  Node := FindNode(Tree.Root, 5);
  AssertTrue(Node.Height = 1);
  Node := FindNode(Tree.Root, 6);
  AssertTrue(Node.Height = 2);
  Node := FindNode(Tree.Root, 7);
  AssertTrue(Node.Height = 0);
  Node := FindNode(Tree.Root, 8);
  AssertTrue(Node.Height = 0);
  Node := FindNode(Tree.Root, 9);
  AssertTrue(Node.Height = 1);
  Node := FindNode(Tree.Root, 10);
  AssertTrue(Node.Height = 0);
end;

procedure TTestLiteRootTree.TreeNodeWidth;
var
  Tree: TTestTree;
  Node: TTreeNode;
  Lvl: SizeInt = -1;
begin
  CreateTestTree11(Tree);

  AssertTrue(Tree.Root.Width(Lvl) = 1);
  AssertTrue(Lvl = 0);
  Node := FindNode(Tree.Root, 1);
  AssertTrue(Node.Width(Lvl) = 2);
  AssertTrue(Lvl = 1);
  Node := FindNode(Tree.Root, 2);
  AssertTrue(Node.Width(Lvl) = 2);
  AssertTrue(Lvl = 1);
  Node := FindNode(Tree.Root, 3);
  AssertTrue(Node.Width(Lvl) = 5);
  AssertTrue(Lvl = 2);
  Node := FindNode(Tree.Root, 4);
  AssertTrue(Node.Width(Lvl) = 5);
  AssertTrue(Lvl = 2);
  Node := FindNode(Tree.Root, 5);
  AssertTrue(Node.Width(Lvl) = 5);
  AssertTrue(Lvl = 2);
  Node := FindNode(Tree.Root, 6);
  AssertTrue(Node.Width(Lvl) = 5);
  AssertTrue(Lvl = 2);
  Node := FindNode(Tree.Root, 7);
  AssertTrue(Node.Width(Lvl) = 5);
  AssertTrue(Lvl = 2);
  Node := FindNode(Tree.Root, 8);
  AssertTrue(Node.Width(Lvl) = 2);
  AssertTrue(Lvl = 3);
  Node := FindNode(Tree.Root, 9);
  AssertTrue(Node.Width(Lvl) = 2);
  AssertTrue(Lvl = 3);
  Node := FindNode(Tree.Root, 10);
  AssertTrue(Node.Width(Lvl) = 1);
  AssertTrue(Lvl = 4);
end;

procedure TTestLiteRootTree.TreeNodeSize;
var
  Tree: TTestTree;
  Node: TTreeNode;
begin
  AssertTrue(Tree.Root.Size = 1);
  CreateTestTree11(Tree);
  AssertTrue(Tree.Root.Size = 11);
  Node := FindNode(Tree.Root, 1);
  AssertTrue(Node.Size = 7);
  Node := FindNode(Tree.Root, 2);
  AssertTrue(Node.Size = 3);
  Node := FindNode(Tree.Root, 3);
  AssertTrue(Node.Size = 1);
  Node := FindNode(Tree.Root, 4);
  AssertTrue(Node.Size = 1);
  Node := FindNode(Tree.Root, 5);
  AssertTrue(Node.Size = 2);
  Node := FindNode(Tree.Root, 6);
  AssertTrue(Node.Size = 3);
  Node := FindNode(Tree.Root, 7);
  AssertTrue(Node.Size = 1);
  Node := FindNode(Tree.Root, 8);
  AssertTrue(Node.Size = 1);
  Node := FindNode(Tree.Root, 9);
  AssertTrue(Node.Size = 2);
  Node := FindNode(Tree.Root, 10);
  AssertTrue(Node.Size = 1);
end;

procedure TTestLiteRootTree.TreeNodeGetParent;
var
  Tree: TTestTree;
  Nodes: array[0..10] of TTreeNode;
  Parent: TTreeNode;
  I: Integer;
begin
  Nodes[0] := {%H-}Tree.Root;
  AssertFalse(Nodes[0].GetParent(Parent));
  AssertFalse(Parent.Assigned);
  for I := 1 to 10 do
    Nodes[I] := Nodes[I - 1].AddChild;
  for I := 1 to 10 do
    begin
      AssertTrue(Nodes[I].GetParent(Parent));
      AssertTrue(Parent.Assigned);
      AssertTrue(Parent = Nodes[I - 1]);
    end;
end;

procedure TTestLiteRootTree.TreeNodeGetFirstChild;
var
  Tree: TTestTree;
  Nodes: array[0..10] of TTreeNode;
  Child: TTreeNode;
  I: Integer;
begin
  Nodes[0] := {%H-}Tree.Root;
  for I := 1 to 10 do
    Nodes[I] := Nodes[I - 1].AddChild;
  for I := 0 to 9 do
    begin
      AssertTrue(Nodes[I].GetFirstChild(Child));
      AssertTrue(Child.Assigned);
      AssertTrue(Child = Nodes[I + 1]);
    end;
  AssertFalse(Nodes[10].GetFirstChild(Child));
  AssertFalse(Child.Assigned);
end;

procedure TTestLiteRootTree.TreeNodeGetLastChild;
var
  Tree: TTestTree;
  Child: TTreeNode;
begin
  {%H-}Child.Clear;
  AssertFalse(Tree.Root.GetLastChild(Child));
  AssertFalse(Child.Assigned);
  CreateTestTree10(Tree);
  AssertTrue(Tree.Root.GetLastChild(Child));
  AssertTrue(Child.Value = 3);
  AssertTrue(Child.GetLastChild(Child));
  AssertTrue(Child.Value = 9);
  AssertFalse(FindNode(Tree.Root, 2).GetLastChild(Child));
  AssertFalse(Child.Assigned);
end;

procedure TTestLiteRootTree.TreeNodeGetSibling;
var
  Tree: TTestTree;
  Nodes: array[0..10] of TTreeNode;
  Sibling: TTreeNode;
  I: Integer;
begin
  Nodes[0] := {%H-}Tree.Root;
  for I := 1 to 10 do
    Nodes[I] := Nodes[0].AddChild;
  for I := 10 downto 2 do
    begin
      AssertTrue(Nodes[I].GetSibling(Sibling));
      AssertTrue(Sibling.Assigned);
      AssertTrue(Sibling = Nodes[I - 1]);
    end;
  AssertFalse(Nodes[1].GetSibling(Sibling));
  AssertFalse(Sibling.Assigned);
end;

procedure TTestLiteRootTree.TreeNodeInSameTree;
var
  Tree, Tree2: TTestTree;
  L, R: TTreeNode;
begin
  AssertTrue(Tree.Root.InSameTree(Tree.Root));
  L := Tree.Root.AddChild;
  AssertTrue(L.InSameTree(Tree.Root));
  R := L;
  AssertTrue(L.InSameTree(R));
  R := L.AddChild;
  AssertTrue(L.InSameTree(R));
  L := Tree2.Root.AddChild;
  AssertFalse(L.InSameTree(R));
end;

procedure TTestLiteRootTree.TreeNodeDistanceFrom;
var
  Tree, Tree2: TTestTree;
  Node: TTreeNode;
begin
  CreateTestTree11(Tree);
  Node := FindNode(Tree.Root, 3);
  AssertTrue(Node.DistanceFrom(FindNode(Tree.Root, 9)) = 3);
  AssertTrue(Node.DistanceFrom(FindNode(Tree.Root, 7)) = 4);
  AssertTrue(Node.DistanceFrom(Tree2.Root.AddChild) = -1);
end;

procedure TTestLiteRootTree.TreeNodeIsDescendant;
var
  Tree: TTestTree;
  Node: TTreeNode;
begin
  Node := Tree.Root;
  AssertFalse(Node.IsDescendantOf(Tree.Root));
  Node := Node.AddChild;
  AssertTrue(Node.IsDescendantOf(Tree.Root));
  AssertFalse(Tree.Root.IsDescendantOf(Node));
end;

procedure TTestLiteRootTree.TreeNodeIsAncestor;
var
  Tree: TTestTree;
  Node: TTreeNode;
begin
  Node := Tree.Root;
  AssertFalse(Node.IsAncestorOf(Tree.Root));
  Node := Node.AddChild;
  AssertFalse(Node.IsAncestorOf(Tree.Root));
  AssertTrue(Tree.Root.IsAncestorOf(Node));
end;

procedure TTestLiteRootTree.TreeNodeGetLca;
var
  Tree, Tree2: TTestTree;
  Node, Lca: TTreeNode;
begin
  Node := Tree.Root;
  AssertFalse(Node.GetLca(Tree.Root, Lca));
  AssertFalse(Lca.Assigned);
  Node := Node.AddChild;
  AssertFalse(Node.GetLca(Tree2.Root.AddChild, Lca));
  AssertFalse(Lca.Assigned);
  Tree.Clear;
  Tree2.Clear;

  CreateTestTree11(Tree);

  Node := FindNode(Tree.Root, 3);
  AssertTrue(Node.GetLca(FindNode(Tree.Root, 4), Lca));
  AssertTrue(Lca = Tree.Root);

  Node := FindNode(Tree.Root, 3);
  AssertTrue(Node.GetLca(FindNode(Tree.Root, 1), Lca));
  AssertTrue(Lca.Value = 0);

  Node := FindNode(Tree.Root, 3);
  AssertTrue(Node.GetLca(FindNode(Tree.Root, 10), Lca));
  AssertTrue(Lca.Value = 1);

  Node := FindNode(Tree.Root, 4);
  AssertTrue(Node.GetLca(FindNode(Tree.Root, 7), Lca));
  AssertTrue(Lca.Value = 2);

  Node := FindNode(Tree.Root, 2);
  AssertTrue(Node.GetLca(FindNode(Tree.Root, 7), Lca));
  AssertTrue(Lca.Value = 0);

  Node := FindNode(Tree.Root, 7);
  AssertTrue(Node.GetLca(FindNode(Tree.Root, 10), Lca));
  AssertTrue(Lca.Value = 0);
end;

procedure TTestLiteRootTree.TreeNodeAddChild;
var
  Tree: TTestTree;
  Node, Child, Tmp: TTreeNode;
begin
  Node := Tree.Root;
  AssertFalse(Node.HasChildren);
  Child := Node.AddChild;
  AssertTrue(Child.Assigned);
  AssertTrue(Child.Value = 0);
  AssertTrue(Node.GetFirstChild(Tmp));
  AssertTrue(Tmp = Child);
  Child := Node.AddChild(15);
  AssertTrue(Child.Value = 15);
end;

procedure TTestLiteRootTree.AddLastChild;
var
  Tree: TTestTree;
  Node, Child, Tmp: TTreeNode;
begin
  Node := Tree.Root;
  Child := Node.AddLastChild;
  AssertTrue(Child.Assigned);
  AssertTrue(Node.GetLastChild(Tmp));
  AssertTrue(Tmp = Child);
  Child := Node.AddLastChild(15);
  AssertTrue(Child.Assigned);
  AssertTrue(Node.GetLastChild(Tmp));
  AssertTrue(Tmp = Child);
  AssertTrue(Child.Value = 15);
end;

procedure TTestLiteRootTree.TreeAsCallParam;
  procedure ByValue(aTree: TTestTree);
  begin
    Assert(aTree.Count = 10);
  end;
  procedure AsVar(var aTree: TTestTree);
  begin
    Assert(aTree.Count = 10);
    aTree.Root.AddChild(10);
  end;
  procedure AsConst(const aTree: TTestTree);
  begin
    Assert(aTree.Count = 10);
  end;
var
  Tree: TTestTree;
begin
  CreateTestTree10(Tree);
  Assert(Tree.Count = 10);
  ByValue(Tree);
  Assert(Tree.Count = 10);
  AsConst(Tree);
  Assert(Tree.Count = 10);
  AsVar(Tree);
  Assert(Tree.Count = 11);
end;

procedure TTestLiteRootTree.TreeNodeAddChildTree;
var
  Tree, ChildTree: TTestTree;
  Child: TTreeNode;
  Raised: Boolean = False;
begin
  try
    Tree.Root.AddChildTree(ChildTree{%H-});
  except
    Raised := True;
  end;
  AssertTrue(Raised);
  CreateTestTree11(ChildTree);
  Tree.Root.AddChildTree(ChildTree);
  AssertTrue(ChildTree.IsEmpty);
  AssertTrue(Tree.Root.GetFirstChild(Child));
  AssertTrue(Child.Value = 0);
  AssertTrue(Tree.Root.Size = 12);
end;

procedure TTestLiteRootTree.TreeNodeAddLastChildTree;
var
  Tree, ChildTree: TTestTree;
  Child: TTreeNode;
begin
  CreateTestTree10(Tree);
  CreateTestTree11(ChildTree);
  Tree.Root.AddLastChildTree(ChildTree);
  AssertTrue(ChildTree.IsEmpty);
  AssertTrue(Tree.Count = 21);
  AssertTrue(Tree.Root.GetLastChild(Child));
  AssertTrue(Child.Value = 0);
end;

procedure TTestLiteRootTree.TreeNodeExtract;
var
  Tree, ExTree: TTestTree;
begin
  AssertFalse(Tree.Root.Extract(ExTree));
  CreateTestTree11(Tree);
  AssertTrue(FindNode(Tree.Root, 1).Extract(ExTree));
  AssertTrue(Tree.Root.Size = 4);
  AssertTrue(ExTree.Root.Value = 1);
  AssertTrue(ExTree.Root.Size = 7);
end;

procedure TTestLiteRootTree.TreeNodeAncestors;
var
  Tree: TTestTree;
  Node, Ancest: TTreeNode;
  a: array of Integer;
  I: Integer;
begin
  CreateTestTree11(Tree);
  for Node in Tree.Root.PreorderTraversal do
    for Ancest in Node.Ancestors do
      AssertTrue(Ancest.IsAncestorOf(Node));
  a := FindNode(Tree.Root, 10).Ancestors.ToArray;
  AssertTrue(Length(a) = 4);
  for I in a do
    AssertTrue(I in [0, 1, 6, 9]);

  for Ancest in FindNode(Tree.Root, 10).Ancestors do
    begin
      I := Ancest.Size;
      AssertTrue(Tree.RemoveNode(Ancest) = I);
    end;
  AssertTrue(Tree.IsEmpty);
end;

procedure TTestLiteRootTree.TreeNodeChildren;
var
  Tree: TTestTree;
  Child: TTreeNode;
  a: array of Integer;
  I: Integer;
begin
  I := 0;
  for Child in Tree.Root.Children do
    Inc(I);
  AssertTrue(I = 0);
  for I := 1 to 10 do
    Tree.Root.AddChild(I);
  AssertTrue(Tree.Count = 11);

  I := 0;
  for Child in Tree.Root.Children do
    begin
      Inc(I);
      AssertTrue(Child.Value in [1..10]);
    end;
  AssertTrue(I = 10);

  a := Tree.Root.Children.ToArray;
  AssertTrue(Length(a) = 10);
  for I in a do
    AssertTrue(I in [1..10]);

  for Child in Tree.Root.Children do
    Child.AddChild;
  AssertTrue(Tree.Count = 21);

  for Child in Tree.Root.Children do
    AssertTrue(Tree.RemoveNode(Child) = 2);
  AssertTrue(Tree.Count = 1);
end;

procedure TTestLiteRootTree.TreeNodePreorderTraversal;
var
  Tree: TTestTree;
  Node: TTreeNode;
  a: array of Integer;
  I: Integer;
begin
  I := 0;
  for Node in Tree.Root.PreorderTraversal do
    Inc(I);
  AssertTrue(I = 1);
  CreateTestTree11(Tree);
  a := Tree.Root.PreorderTraversal.ToArray;
  AssertTrue(Length(a) = 11);
  for I in a do
    AssertTrue(I in [0..10]);
end;

procedure TTestLiteRootTree.TreeNodePostorderTraversal;
var
  Tree: TTestTree;
  Node: TTreeNode;
  a: array of Integer;
  I: Integer;
begin
  I := 0;
  for Node in Tree.Root.PostorderTraversal do
    Inc(I);
  AssertTrue(I = 1);
  CreateTestTree11(Tree);
  I := 0;
  for Node in Tree.Root.PostorderTraversal do
    Inc(I, Tree.RemoveNode(Node));
  AssertTrue(I = 11);
  AssertTrue(Tree.IsEmpty);

  CreateTestTree11(Tree);
  a := Tree.Root.PostorderTraversal.ToArray;
  AssertTrue(Length(a) = 11);
  for I in a do
    AssertTrue(I in [0..10]);
end;

procedure TTestLiteRootTree.TreeNodeBfsTraversal;
var
  FindValue: Integer;
  Node: TTreeNode;
  procedure OnWhite(aNode: TTreeNode);
  begin
    if aNode.Value = FindValue then
      begin
        Node := aNode;
        Abort;
      end;
  end;
var
  Tree: TTestTree;
  I: Integer;
begin
  CreateTestTree11(Tree);
  for I := 0 to 10 do
    begin
      FindValue := I;
      Node.Clear;
      try Tree.Root.BfsTraversal(@OnWhite, nil, nil) except end;
      AssertTrue(Node.Assigned);
      AssertTrue(Node.Value = FindValue);
    end;
end;

procedure TTestLiteRootTree.TreeNodeDfsTraversalR2L;
var
  Counter: Integer = 0;
  procedure OnGray(aNode: TTreeNode);
  begin
    aNode.Value := Counter;
    Inc(Counter);
  end;
  procedure OnBlack(aNode: TTreeNode);
  begin
    aNode.Value := Counter - aNode.Value;
  end;
var
  Tree: TTestTree;
  Node: TTreeNode;
  sz: SizeInt;
begin
  CreateRandomTree(Tree);
  sz := Tree.Root.DfsTraversalR2L(nil, @OnGray, @OnBlack);
  AssertTrue(sz = Tree.Root.Size);
  for Node in Tree.Root.PreorderTraversal do
    AssertTrue(Node.Size = Node.Value);
end;

procedure TTestLiteRootTree.TreeNodeDfsTraversalL2R;
type
  THelper = specialize TGOrdinalArrayHelper<Integer>;
var
  Counter: Integer = 0;
  a: array of Integer = nil;
  procedure OnGray(aNode: TTreeNode);
  begin
    a[Counter] := aNode.Value;
    Inc(Counter);
  end;
var
  Tree: TTestTree;
const
  Expect10: array[0..9] of Integer = (0, 1, 4, 5, 6, 2, 3, 7, 8, 9);
  Expect11: array[0..10] of Integer = (0, 1, 3, 5, 8, 6, 9, 10, 2, 7, 4);
begin
  CreateTestTree10(Tree);
  SetLength(a, 10);
  AssertTrue(Tree.Root.DfsTraversalL2R(nil, @OnGray, nil) = Tree.Count);
  AssertTrue(THelper.Same(a, Expect10));
  Tree.Clear;
  Counter := 0;
  SetLength(a, 11);
  CreateTestTree11(Tree);
  AssertTrue(Tree.Root.DfsTraversalL2R(nil, @OnGray, nil) = Tree.Count);
  AssertTrue(THelper.Same(a, Expect11));
end;

procedure TTestLiteRootTree.TreeEnumerator;
var
  Tree: TTestTree;
  Node: TTreeNode;
  I: Integer;
begin
  I := 0;
  for Node in Tree do
    Inc(I);
  AssertTrue(I = 0);
  CreateTestTree11(Tree);
  for Node in Tree do
    Inc(I);
  AssertTrue(I = 11);
end;

procedure TTestLiteRootTree.TreeAssignment;
var
  Tree1, Tree2: TTestTree;
begin
  CreateTestTree11(Tree1);
  Tree1.Root.Value := 12;
  Tree2 := Tree1;
  AssertTrue(Tree1.Count = 11);
  AssertTrue(Tree2.Count = 11);
  AssertTrue(Tree2.Root.Value = 12);
  Tree1.Clear;
  AssertTrue(Tree1.Count = 0);
  AssertTrue(Tree2.Count = 11);
  AssertTrue(Tree2.Root.Value = 12);
end;

procedure TTestLiteRootTree.TreeClear;
var
  Tree: TTestTree;
begin
  AssertTrue(Tree.Root.Assigned);
  AssertFalse(Tree.IsEmpty);
  Tree.Clear;
  AssertTrue(Tree.IsEmpty);
  CreateTestTree11(Tree);
  AssertTrue(Tree.Count = 11);
  Tree.Clear;
  AssertTrue(Tree.IsEmpty);
end;

procedure TTestLiteRootTree.TreeOwnsNode;
var
  Tree1, Tree2: TTestTree;
  Node: TTreeNode;
begin
  CreateTestTree11(Tree1);
  CreateTestTree11(Tree2);
  for Node in Tree1.Root.PreorderTraversal do
    AssertFalse(Tree2.OwnsNode(Node));
  for Node in Tree2.Root.PreorderTraversal do
    AssertTrue(Tree2.OwnsNode(Node));
end;

procedure TTestLiteRootTree.TreeRemoveNode;
var
  Tree1, Tree2: TTestTree;
  Node: TTreeNode;
  sz: SizeInt;
begin
  CreateTestTree11(Tree1);
  AssertTrue(Tree1.RemoveNode(Tree2.Root) = 0);
  AssertTrue(Tree2.RemoveNode(Tree1.Root) = 0);
  Node := Tree2.Root.AddChild;
  AssertTrue(Tree1.RemoveNode(Node) = 1);
  Tree1.Root.GetFirstChild(Node);
  sz := Node.Size;
  AssertTrue(Tree2.RemoveNode(Node) = sz);
  AssertTrue(Tree1.Count = 11 - sz);
end;

procedure TTestLiteRootTree.ObjTreeClear;
var
  Tree: TObjTestTree;
  Node: TObjTreeNode;
  a: array of TTestObj;
  I, Counter: Integer;
begin
  AssertTrue({%H-}Tree.OwnsObjects);
  Node := Tree.Root;
  Counter := 0;
  for I := 1 to 15 do
    Node := Node.AddChild(TTestObj.Create(@Counter));
  AssertTrue(Tree.Count = 16);
  Tree.Clear;
  AssertTrue(Tree.IsEmpty);
  AssertTrue(Counter = 15);
  SetLength(a{%H-}, 16);
  for I := 0 to 15 do
    a[I] := TTestObj.Create(@Counter);
  Counter := 0;
  Tree.OwnsObjects := False;
  Tree.Root.Value := a[0];
  for I := 1 to 15 do
    Node := Node.AddChild(a[I]);
  AssertTrue(Tree.Count = 16);
  Tree.Clear;
  AssertTrue(Tree.Count = 0);
  AssertTrue(Counter = 0);
  for I := 0 to 15 do
    a[I].Free;
  AssertTrue(Counter = 16);
end;

procedure TTestLiteRootTree.ObjTreeRemoveNode;
var
  Tree1, Tree2: TObjTestTree;
  Node, Parent: TObjTreeNode;
  a: array of TTestObj;
  I, Counter: Integer;
begin
  AssertTrue(Tree1.RemoveNode(Tree2.Root) = 0);

  Node := Tree1.Root;
  Node.Value := TTestObj.Create(@Counter);
  for I := 1 to 15 do
    Node := Node.AddChild(TTestObj.Create(@Counter));
  I := 0;
  Counter := 0;
  Parent := Default(TObjTreeNode);
  while Node.GetParent(Parent) do
    begin
      Inc(I);
      AssertTrue(Tree1.RemoveNode(Node) = 1);
      AssertTrue(I = Counter);
      Node := Parent;
    end;
  AssertTrue(Tree1.RemoveNode(Node) = 1);
  AssertTrue(Tree1.IsEmpty);
  AssertTrue(Counter = 16);

  SetLength(a{%H-}, 16);
  for I := 0 to 15 do
    a[I] := TTestObj.Create(@Counter);

  Tree1.OwnsObjects := False;
  Tree1.Root.Value := a[0];
  for I := 1 to 15 do
    Node := Node.AddChild(a[I]);
  AssertTrue(Tree1.Count = 16);
  Counter := 0;
  while Node.GetParent(Parent) do
    begin
      AssertTrue(Tree1.RemoveNode(Node) = 1);
      Node := Parent;
    end;
  AssertTrue(Tree1.RemoveNode(Node) = 1);
  AssertTrue(Tree1.IsEmpty);
  AssertTrue(Counter = 0);
  for I := 0 to 15 do
    a[I].Free;
  AssertTrue(Counter = 16);
end;



initialization

  RegisterTest(TTestLiteRootTree);

end.

