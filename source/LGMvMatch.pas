{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   The Micali-Vazirani Maximum Cardinality Matching Algorithm.             *
*   Adopted and modified implementation of Steven Crocker.                  *
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
unit LGMvMatch;

{$mode objfpc}{$H+}
{$INLINE ON}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  SysUtils, LGUtils;

type
  TDirection = -1..1;
  TParity    = (paEven, paOdd);
  TSide      = (siNone, siLeft, siRight);

  PNode      = ^TNode;
  PEdge      = ^TEdge;
  PBlossom   = ^TBlossom;
  PLevel     = ^TLevel;

  TEdge = record
    Node1,
    Node2,
    PredNode: PNode;
    Next1,              // next edge incident to Node1
    Next2,              // next edge incident to Node2
    PredNext,
    PredPrev,
    NextBridge,         // next bridge on the same level
    NextAnomaly: PEdge;
    VisitBlossom: PBlossom;
    VisitSide: TSide;
    IsBridge,
    Used: Boolean;
    function OtherNode(aNode: PNode): PNode; inline;
    function NextEdge(aNode: PNode): PEdge; inline;
  end;

  TNode = record
    Mate,
    DfsParent,              // parent node from the ddfs
    SidePrev,
    SideNext,               // nodes with same side marking (doubly linked list)
    BStar,                  // base star structure
    NextEven,               // next node with this even level
    NextOdd,                // next node with this odd level
    NextNodeOnPath,
    SNext: PNode;           // pointer for any use
    FirstEdge,              // first incident edge (singly linked list)
    MatchedEdge,
    FirstProp,              // first prop (singly linked list)
    FirstAnomaly,           // first anomaly (singly linked list)
    LastUsed,
    LastVisited,
    DfsParentEdge,          // parent edge pointer from the ddfs *)
    NextEdgeOnPath: PEdge;
    Blossom,
    VisitBlossom: PBlossom; // blossom searching when visited
    Index,
    PredecCount,            // predecessor count
    EvenLevel,
    OddLevel,
    Level,
    CallID: SizeInt;        // unique ID of BlossAug this node visited on
    DfsSide,                // side of blossom node is on in ddfs
    VisitSide: TSide;       // side of blossom searching when visited
    Used: Boolean;
    function  Matched: Boolean; inline;
    procedure AddPredecessor(aEdg: PEdge);
    procedure AddAnomaly(aEdg: PEdge); inline;
    function  OtherNode(aEdg: PEdge): PNode; inline;
    function  NextEdge(aEdg: PEdge): PEdge; inline;
  end;

  TBlossom = record
    Base:  PNode;        // base vertex of blossom
    Peake: PEdge;        // peak edge of blossom
  end;

  TLevel = record
    FirstNode: PNode;    // first node on this level (singly linked list)
    FirstBridge,         // first of the bridges on this level
    LastBridge:  PEdge;  // last of the bridges on this level
    procedure AddBridge(aEdg: PEdge);
  end;

  TNodes = array of TNode;
  TEdges = array of TEdge;

const
  NEG_DIR: TDirection = -1;
  POS_DIR: TDirection = 1;

  procedure Match(var aNodes: TNodes; var aEdges: TEdges; var aMatched: SizeInt);

implementation
{$B-}{$COPERATORS ON}

{ TEdge }

function TEdge.OtherNode(aNode: PNode): PNode;
begin
  if aNode = Node1 then
    Result := Node2
  else
    Result := Node1;
end;

function TEdge.NextEdge(aNode: PNode): PEdge;
begin
  if aNode = Node1 then
    Result := Next1
  else
    Result := Next2;
end;

{ TNode }

function TNode.Matched: Boolean;
begin
  Result := Mate <> nil;
end;

procedure TNode.AddPredecessor(aEdg: PEdge);
var
  First: PEdge;
begin
  First := FirstProp;
  aEdg^.PredNext := First;
  if First <> nil then
    First^.PredPrev := aEdg;
  aEdg^.PredNode := @Self;
  FirstProp := aEdg;
  aEdg^.PredPrev := nil;
  Inc(PredecCount);
end;

procedure TNode.AddAnomaly(aEdg: PEdge);
begin
  aEdg^.NextAnomaly := FirstAnomaly;
  FirstAnomaly := aEdg;
end;

function TNode.OtherNode(aEdg: PEdge): PNode;
begin
  Result := aEdg^.OtherNode(@Self);
end;

function TNode.NextEdge(aEdg: PEdge): PEdge;
begin
  Result := aEdg^.NextEdge(@Self);
end;

{ TLevel }

procedure TLevel.AddBridge(aEdg: PEdge);
begin
  if FirstBridge = nil then
    FirstBridge := aEdg
  else
    LastBridge^.NextBridge := aEdg;
  LastBridge := aEdg;
  aEdg^.NextBridge := nil;
  aEdg^.IsBridge := True;
end;

procedure Match(var aNodes: TNodes; var aEdges: TEdges; var aMatched: SizeInt);
var
  Levels: array of TLevel;
  Blossoms: array of TBlossom;
  NodeCount, InfLevel, CurrBlossom: SizeInt;
  Dummy: PBlossom;

  procedure Search;
  var
    StSides: array[TSide] of PNode;
    CurrV, CurrU: PNode;
    NextEdge, CurrEdge, CurrProp: PEdge;
    CurrLevel: SizeInt;
    LastBaCall: SizeInt = 0;
    Augmented: Boolean = True;
    AugOccur: Boolean = False;

    procedure SetLevel(aNode: PNode; aLevel: SizeInt; aParity: TParity);
    begin
      if aParity = paEven then
        begin
          aNode^.EvenLevel := aLevel;
          aNode^.NextEven := Levels[aLevel].FirstNode;
          Levels[aLevel].FirstNode := aNode;
        end
      else
        begin
          aNode^.OddLevel := aLevel;
          aNode^.NextOdd := Levels[aLevel].FirstNode;
          Levels[aLevel].FirstNode := aNode;
        end;
      if aLevel < aNode^.Level then
        aNode^.Level := aLevel;
    end;

    procedure BlossAug(w1, w2: PNode; var Augmented: Boolean; aPeak: PEdge);
    var
      u, uL, uR, vL, vR, w, Barrier, Dcv, DcvBStar: PNode;
      Anomaly, RAncEdge, LAncEdge: PEdge;
      Side: TSide;
      BlossFound: Boolean;
      Bloss: PBlossom;

      procedure ConnectPath(Node1, Node2: PNode; aEdg: PEdge; aDir: TDirection);
      begin
        if aDir = NEG_DIR then
          begin
            Node1^.NextNodeOnPath := Node2;
            Node1^.NextEdgeOnPath := aEdg;
          end
        else
          begin
            Node2^.NextNodeOnPath := Node1;
            Node2^.NextEdgeOnPath := aEdg;
          end;
      end;

      procedure FindPath(aHigh, aLow: PNode; aBloss: PBlossom; aDir: TDirection; aSide: TSide); forward;

      procedure OpenBlossom(aNode, aBase: PNode; aDir: TDirection);
      var
        Blos: PBlossom;
        Peak: PEdge;
        Node1, Node2: PNode;
      begin
        Blos := aNode^.Blossom;
        if not Odd(aNode^.Level) then
          FindPath(aNode, aBase, Blos, aDir, aNode^.DfsSide)
        else
          begin
            Peak := Blos^.Peake;
            Node1 := Peak^.Node1;
            Node2 := Peak^.Node2;
            if aNode^.DfsSide = siLeft then
              begin
                FindPath(Node1, aNode, Blos, -aDir, siLeft);
                FindPath(Node2, aBase, Blos, aDir, siRight);
                ConnectPath(Node1, Node2, Peak, -aDir);
              end
            else
              begin
                FindPath(Node2, aNode, Blos, -aDir, siRight);
                FindPath(Node1, aBase, Blos, aDir, siLeft);
                ConnectPath(Node2, Node1, Peak, -aDir);
              end;
          end;
      end;

      function BaseStar(aNode: PNode): PNode;
      var
        Curr, Next: PNode;
      begin
        Curr := aNode;
        while Curr^.BStar <> nil do
          Curr := Curr^.BStar;
        while aNode <> Curr do
          begin
            Next := aNode^.BStar;
            aNode^.BStar := Curr;
            aNode := Next;
          end;
        Result := Curr;
      end;

      procedure FindPath(aHigh, aLow: PNode; aBloss: PBlossom; aDir: TDirection; aSide: TSide);
      var
        CurrU, CurrV: PNode;
        ParentEdg: PEdge;
        WrongBlossom: Boolean;
      begin
        if aHigh <> aLow then
          begin
            WrongBlossom := False;
            CurrV := aHigh;
            CurrU := CurrV;
            CurrProp := aHigh^.FirstProp;
            while (CurrU <> aLow) do
              begin
                if CurrProp = nil then
                  CurrProp := CurrV^.FirstProp;
                while (CurrProp^.VisitBlossom = aBloss) and (CurrProp^.PredNext <> nil) do
                  CurrProp := CurrProp^.PredNext;
                CurrV^.LastVisited := CurrProp;
                if CurrProp^.VisitBlossom = aBloss then
                  begin
                    CurrV := CurrV^.DfsParent;
                    CurrProp := CurrV^.LastVisited;
                  end
                else
                  if ((CurrV^.Blossom <> aBloss) and WrongBlossom) then
                    begin
                      CurrV := CurrV^.DfsParent;
                      CurrProp := CurrV^.LastVisited;
                    end
                  else
                    begin
                      WrongBlossom := False;
                      if CurrV^.Blossom = aBloss then
                        begin
                          CurrProp^.VisitBlossom := aBloss;
                          CurrProp^.VisitSide := aSide;
                          CurrU := CurrV^.OtherNode(CurrProp);
                        end
                      else
                        CurrU := CurrV^.Blossom^.Base;
                      if CurrU <> aLow then
                        if ((CurrU^.VisitSide = aSide) and (CurrU^.VisitBlossom = aBloss)) or
                           (CurrU^.Level <= aLow^.Level) or ((CurrU^.Blossom = aBloss) and (CurrU^.DfsSide <> aSide)) then
                            WrongBlossom := True
                        else
                          begin
                            CurrU^.VisitBlossom := aBloss;
                            CurrU^.VisitSide := aSide;
                            CurrU^.DfsParent := CurrV;
                            CurrU^.DfsParentEdge := CurrProp;
                            CurrV := CurrU;
                            CurrProp := CurrV^.LastVisited;
                          end;
                    end;
              end;
              CurrU^.DfsParent := CurrV;
              CurrU^.DfsParentEdge := CurrProp;
              ParentEdg := CurrU^.DfsParentEdge;
              while CurrV <> aHigh do
                begin
                  ConnectPath(CurrU, CurrV, ParentEdg, aDir);
                  CurrU := CurrV;
                  CurrV := CurrV^.DfsParent;
                  ParentEdg := CurrU^.DfsParentEdge;
                end;
              ConnectPath(CurrU, CurrV, ParentEdg, aDir);
              if aDir = POS_DIR then
                begin
                  CurrV := aHigh;
                  CurrU := aHigh^.NextNodeOnPath;
                end
              else
                begin
                  CurrU := aLow;
                  CurrV := aLow^.NextNodeOnPath;
                end;
              while not (((aDir = POS_DIR) and (CurrV = aLow)) or ((aDir = NEG_DIR) and (CurrU = aHigh))) do
                begin
                  if CurrV^.Blossom <> aBloss then
                    OpenBlossom(CurrV, CurrU, aDir);
                  if aDir = POS_DIR then
                    begin
                      CurrV := CurrU;
                      CurrU := CurrU^.NextNodeOnPath;
                    end
                  else
                    begin
                      CurrU := CurrV;
                      CurrV := CurrV^.NextNodeOnPath;
                    end;
                end;
          end;
      end;

      procedure Augment(aFrom, aTo: PNode);
      var
        Mate: PNode;
        MEdge: PEdge;
      begin
        repeat
          Mate := aFrom^.NextNodeOnPath;
          MEdge := aFrom^.NextEdgeOnPath;
          aFrom^.Mate := Mate;
          aFrom^.MatchedEdge := MEdge;
          Mate^.Mate := aFrom;
          Mate^.MatchedEdge := MEdge;
          aFrom := Mate^.NextNodeOnPath;
        until Mate = aTo;
      end;

      procedure TopologicErase(aFrom, aTo: PNode);
      var
        ESrackTop, CurrNode: PNode;

        procedure StackPush(aNode: PNode);
        begin
          aNode^.SNext := ESrackTop;
          ESrackTop := aNode;
        end;

        function TryStackPop(out aNode: PNode): Boolean;
        begin
          Result := ESrackTop <> nil;
          if Result then
            begin
              aNode := ESrackTop;
              ESrackTop := ESrackTop^.SNext;
              aNode^.SNext := nil;
            end;
        end;

        procedure Erase(aNode: PNode);
        var
          CurrNode: PNode;
          CurrEdge: PEdge;
        begin
          aNode^.Used := False;
          CurrEdge := aNode^.FirstEdge;
          while CurrEdge <> nil do
            with CurrEdge^ do
              begin
                if (PredNode <> nil) and (PredNode <> aNode) then
                  begin
                    if PredNode^.Used then
                      begin
                        CurrNode := PredNode;
                        Dec(CurrNode^.PredecCount);
                        if CurrNode^.PredecCount = 0 then
                          StackPush(CurrNode);
                        if PredNext <> nil then
                          PredNext^.PredPrev := PredPrev;
                        if PredPrev <> nil then
                          PredPrev^.PredNext := PredNext
                        else
                          CurrNode^.FirstProp := PredNext;
                      end;
                  end;
                CurrEdge := CurrEdge^.NextEdge(aNode);
              end;
        end;

      begin
        ESrackTop := nil;
        while aFrom <> aTo do
          begin
            Erase(aFrom);
            aFrom := aFrom^.NextNodeOnPath;
          end;
        Erase(aFrom);
        while TryStackPop(CurrNode) do
          Erase(CurrNode);
      end;

      procedure MarkNode(aNode: PNode; aSide: TSide);
      var
        Next: PNode;
      begin
        Next := StSides[aSide];
        aNode^.SideNext := Next;
        aNode^.DfsSide := aSide;
        StSides[aSide] := aNode;
        aNode^.SidePrev := nil;
        aNode^.CallID := LastBaCall;
        if Next <> nil then
          Next^.SidePrev := aNode;
      end;

      procedure UnMarkNode(aNode: PNode);
      var
        Prev, Next: PNode;
      begin
        if aNode <> nil then
          begin
            Prev := aNode^.SidePrev;
            Next := aNode^.SideNext;
            if Prev = nil then
              StSides[aNode^.DfsSide] := Next
            else
              Prev^.SideNext := Next;
            if Next <> nil then
              Next^.SidePrev := Prev;
            aNode^.CallID := -1;
          end;
      end;

      procedure PushLeft(var aVL, aVR: PNode; var aEL: PEdge);
      begin
        if aEL = nil then
          aEL := aVL^.FirstProp;
        while (aEL^.Used) and (aEL^.PredNext <> nil) do
          aEL := aEL^.PredNext;
        aVL^.LastUsed := aEL;
        if aEL^.Used then
          begin
            if aVL^.DfsParent = nil then
              BlossFound := True
            else
              begin
                aVL := aVL^.DfsParent;
                aEL := aVL^.LastUsed;
              end;
          end
        else
          begin
            aEL^.Used := True;
            uL := aVL^.OtherNode(aEL);
            uL := BaseStar(uL);
            if uL^.CallID <> LastBaCall then
              begin
                MarkNode(uL, siLeft);
                uL^.DfsParent := aVL;
                uL^.DfsParentEdge := aEL;
                aVL := uL;
                aEL := aVL^.LastUsed;
              end
            else
              if (uL = Barrier) or (uL <> aVR) then
                begin
                  if (uL = Barrier) and (uL = aVR) and (Dcv = nil) then
                    Dcv := uL;
                end
              else
                begin
                  UnMarkNode(uL);
                  MarkNode(uL, siLeft);
                  aVR := aVR^.DfsParent;
                  RAncEdge := aVR^.LastUsed;
                  uL^.DfsParent := aVL;
                  uL^.DfsParentEdge := aEL;
                  aVL := uL;
                  aEL := aVL^.LastUsed;
                  Dcv := uL;
                end;
          end;
      end;

    procedure PushRight(var aVL, aVR: PNode; var aER: PEdge);
    begin
      if aER = nil then
        aER := aVR^.FirstProp;
      while aER^.Used and (aER^.PredNext <> nil) do
        aER := aER^.PredNext;
      aVR^.LastUsed := aER;
      if aER^.Used then
        begin
          if aVR = Barrier then
            begin
              if aVL^.DfsParent = nil then
                BlossFound := True
              else
                begin
                  aVR := Dcv;
                  aER := aVR^.LastUsed;
                  Barrier := Dcv;
                  UnMarkNode(aVR);
                  MarkNode(aVR, siRight);
                  aVL := aVL^.DfsParent;
                  LAncEdge := aVL^.LastUsed;
                end;
            end
          else
            begin
              aVR := aVR^.DfsParent;
              aER := aVR^.LastUsed;
            end
        end
      else
        begin
          aER^.Used := True;
          uR := aVR^.OtherNode(aER);
          uR := BaseStar(uR);
          if uR^.CallID <> LastBaCall then
            begin
              MarkNode(uR, siRight);
              uR^.DfsParent := aVR;
              uR^.DfsParentEdge := aER;
              aVR := uR;
              aER := aVR^.LastUsed;
            end
          else
            if uR = aVL then
              Dcv := uR;
        end;
    end;

    function NewBlossom: PBlossom;
    begin
      Result := @Blossoms[CurrBlossom];
      Inc(CurrBlossom);
    end;

    begin
      BlossFound := False;
      Augmented := False;
      if w1^.Used and w2^.Used and (BaseStar(w1) <> BaseStar(w2)) then
        begin
          vL := BaseStar(w1);
          vR := BaseStar(w2);
          Inc(LastBaCall);
          LAncEdge := vL^.LastUsed;
          RAncEdge := vR^.LastUsed;
          StSides[siLeft] := nil;
          StSides[siRight] := nil;
          MarkNode(vL, siLeft);
          MarkNode(vR, siRight);
          vL^.DfsParent := nil;
          Dcv := nil;
          Barrier := vR;

          while not (BlossFound or Augmented) do              // ddfs search
            if (vL^.Mate = nil) and (vR^.Mate = nil) then
              begin
                FindPath(w1, vL, nil, NEG_DIR, siLeft);
                FindPath(w2, vR, nil, POS_DIR, siRight);
                ConnectPath(w1, w2, aPeak, NEG_DIR);
                Augment(vL, vR);
                TopologicErase(vL, vR);
                Augmented := True;
              end
            else
              if vL^.Level >= vR^.Level then
                PushLeft(vL, vR, LAncEdge)
              else
                PushRight(vL, vR, RAncEdge);

          if BlossFound then
            begin
              UnMarkNode(Dcv);
              Bloss := NewBlossom;
              Bloss^.Base := Dcv;
              Bloss^.Peake := aPeak;
              DcvBStar := BaseStar(Dcv);
              for Side := siLeft to siRight do
                begin
                  u := StSides[Side];
                  while u <> nil do
                    begin
                      u^.Blossom := Bloss;
                      u^.BStar := DcvBStar;
                      if Odd(u^.Level) then
                        begin
                          SetLevel(u, Succ(CurrLevel shl 1) - u^.OddLevel, paEven);
                          Anomaly := u^.FirstAnomaly;
                          while Anomaly <> nil do
                            begin
                              w := u^.OtherNode(Anomaly);
                              Levels[(u^.EvenLevel + w^.EvenLevel) shr 1].AddBridge(Anomaly);
                              Anomaly^.Used := True;
                              Anomaly := Anomaly^.NextAnomaly;
                            end;
                        end
                      else
                        SetLevel(u, Succ(CurrLevel shl 1) - u^.EvenLevel, paOdd);
                      u := u^.SideNext;
                    end;
                end;
            end;
        end;
    end;

    procedure ReinitNodes;
    var
      I: SizeInt;
    begin
      for I := 0 to System.High(aNodes) do
        with aNodes[I] do
          begin
            BStar := nil;
            LastUsed := nil;
            LastVisited := nil;
            NextOdd := nil;
            NextEven := nil;
            Blossom := nil;
            FirstProp := nil;
            FirstAnomaly := nil;
            VisitBlossom := Dummy;
            OddLevel := InfLevel;
            Level := InfLevel;
            if Mate = nil then
              begin
                SetLevel(@aNodes[I], 0, paEven);
                PredecCount := 1;
              end
            else
              begin
                EvenLevel := InfLevel;
                PredecCount := 0;
              end;
            VisitSide := siNone;
            Used := True;
          end;
    end;

    procedure ReinitEdges;
    var
      I: SizeInt;
    begin
      for I := 0 to System.High(aEdges) do
        with aEdges[I] do
          begin
            PredNode := nil;
            NextBridge := nil;
            VisitBlossom := Dummy;
            VisitSide := siNone;
            Used := False;
            IsBridge := False;
          end;
    end;

  begin
    while Augmented do
      begin
        System.FillChar(Pointer(Levels)^, System.Length(Levels) * SizeOf(TLevel), 0);
        ReinitNodes;
        ReinitEdges;
        CurrLevel := -1;
        CurrBlossom := 0;
        Augmented := False;
        while not Augmented and (CurrLevel < Pred(NodeCount)) do
          begin
            Inc(CurrLevel);
            CurrV := Levels[CurrLevel].FirstNode;
            if CurrV = nil then
              break;
            if Odd(CurrLevel) then
              while CurrV <> nil do
                begin
                  CurrEdge := CurrV^.MatchedEdge;
                  CurrU := CurrV^.Mate;
                  if (CurrU^.OddLevel = CurrLevel) and (not CurrEdge^.IsBridge) then
                    Levels[(CurrU^.OddLevel + CurrV^.OddLevel) shr 1].AddBridge(CurrEdge)
                  else
                    if CurrU^.OddLevel = InfLevel then
                      begin
                        SetLevel(CurrU, Succ(CurrLevel), paEven);
                        CurrU^.AddPredecessor(CurrEdge);
                      end;
                  CurrV := CurrV^.NextOdd;
                end
            else
              while CurrV <> nil do
                begin
                  CurrEdge := CurrV^.FirstEdge;
                  while CurrEdge <> nil do
                    begin
                      CurrU := CurrV^.OtherNode(CurrEdge);
                      NextEdge := CurrEdge^.NextEdge(CurrV);
                      if (CurrV^.Mate <> CurrU) and not CurrEdge^.Used then
                        if (CurrU^.EvenLevel < InfLevel) and (not CurrEdge^.IsBridge) then
                          Levels[(CurrU^.EvenLevel + CurrV^.EvenLevel) shr 1].AddBridge(CurrEdge)
                        else
                          if not CurrEdge^.IsBridge then
                            begin
                              if CurrU^.OddLevel = InfLevel then
                                SetLevel(CurrU, Succ(CurrLevel), paOdd);
                              if CurrU^.OddLevel = Succ(CurrLevel) then
                                CurrU^.AddPredecessor(CurrEdge)
                              else
                                if CurrU^.OddLevel < CurrLevel then
                                  CurrU^.AddAnomaly(CurrEdge);
                            end;
                      CurrEdge := NextEdge;
                    end;
                  CurrV := CurrV^.NextEven;
                end;
            CurrEdge := Levels[CurrLevel].FirstBridge;
            while CurrEdge <> nil do
              begin
                BlossAug(CurrEdge^.Node1, CurrEdge^.Node2, AugOccur, CurrEdge);
                Augmented := Augmented or AugOccur;
                aMatched += Ord(AugOccur);
                CurrEdge := CurrEdge^.NextBridge;
              end;
          end;
      end;
  end;

var
  DummyBlossom: TBlossom;
begin
  NodeCount := System.Length(aNodes);
  InfLevel := Succ(NodeCount);
  DummyBlossom.Base := nil;
  DummyBlossom.Peake := nil;
  Dummy := @DummyBlossom;
  System.SetLength(Levels, Succ(NodeCount));
  System.SetLength(Blossoms, NodeCount);
  Search;
end;

end.

