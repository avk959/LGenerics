{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic simple directed graph implementation.                           *
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
unit LGSimpleDigraph;

{$mode objfpc}{$H+}
{$INLINE ON}{$WARN 6058 off : }
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGraphUtils,
  LGStrConst;

type

  TSortOrder      = LGUtils.TSortOrder;

  { TGSimpleDiGraph is simple sparse directed graph based on adjacency lists;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGSimpleDiGraph<TVertex, TEdgeData, TVertexEqRel> = class(
    specialize TGCustomGraph<TVertex, TEdgeData, TVertexEqRel>)
  public
  type
    TReachabilityMatrix = record
    private
      FMatrix: TSquareBitMatrix;
      FIds: TIntArray;
      function  GetSize: SizeInt; inline;
    public
      constructor Create(constref aMatrix: TSquareBitMatrix; constref aIds: TIntArray);
      function  Reachable(aSrc, aDst: SizeInt): Boolean; inline;
      procedure Discard; inline;
      property  Size: SizeInt read GetSize;
    end;

  protected
    FReachabilityMatrix: TReachabilityMatrix;
    function  GetClosureValid: Boolean; inline;
    procedure DoRemoveVertex(aIndex: SizeInt);
    function  DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
    function  FindCycle(aRoot: SizeInt; out aCycle: TIntArray): SizeInt;
    function  TopoSort(aIndex: SizeInt): TIntArray;
    function  SearchForStrongComponents(out aCompIds: TIntArray): SizeInt;
    function  GetReachabilityMatrix: TReachabilityMatrix;
  public
  { returns True and vertex index, if it was added, False otherwise }
    function  AddVertex(constref aVertex: TVertex; out aIndex: SizeInt): Boolean;
    function  AddVertex(constref aVertex: TVertex): Boolean; inline;
    procedure RemoveVertex(constref aVertex: TVertex); inline;
    procedure RemoveVertexI(aIndex: SizeInt);
    function  AddEdge(constref aSrc, aDst: TVertex; aData: TEdgeData): Boolean;
    function  AddEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  AddEdgeI(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  AddEdgeI(aSrc, aDst: SizeInt): Boolean; inline;
    function  RemoveEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  RemoveEdgeI(aSrc, aDst: SizeInt): Boolean;
    procedure SaveToStream(aStream: TStream; aWriteVertex: TOnWriteVertex; aWriteData: TOnWriteData);
    procedure LoadFromStream(aStream: TStream; aReadVertex: TOnReadVertex; aReadData: TOnReadData);
    procedure SaveToFile(const aFileName: string; aWriteVertex: TOnWriteVertex; aWriteData: TOnWriteData);
    procedure LoadFromFile(const aFileName: string; aReadVertex: TOnReadVertex; aReadData: TOnReadData);
    function  InDegree(constref aVertex: TVertex): SizeInt; inline;
    function  InDegreeI(aIndex: SizeInt): SizeInt;
    function  OutDegree(constref aVertex: TVertex): SizeInt; inline;
    function  OutDegreeI(aIndex: SizeInt): SizeInt;
    function  Degree(constref aVertex: TVertex): SizeInt; inline;
    function  DegreeI(aIndex: SizeInt): SizeInt;
    function  Isolated(constref aVertex: TVertex): Boolean; inline;
    function  IsolatedI(aIndex: SizeInt): Boolean; inline;
    function  IsSource(constref aVertex: TVertex): Boolean; inline;
    function  IsSourceI(aIndex: SizeInt): Boolean;
    function  IsSink(constref aVertex: TVertex): Boolean; inline;
    function  IsSinkI(aIndex: SizeInt): Boolean;
    function  SourceCount: SizeInt;
    function  SinkCount: SizeInt;
  { checks whether the aDst is reachable from the aSrc(each vertex is reachable from itself) }
    function  PathExists(constref aSrc, aDst: TVertex): Boolean; inline;
    function  PathExistsI(aSrc, aDst: SizeInt): Boolean;
  { checks whether exists any cycle in subgraph that reachable from a Root;
    if True then aCycle will contain indices of the vertices of the cycle }
    function  ContainsCycle(constref aRoot: TVertex; out aCycle: TIntArray): Boolean; inline;
    function  ContainsCycleI(aRoot: SizeInt; out aCycle: TIntArray): Boolean;
    function  ContainsEulerianCircuit: Boolean;
    function  FindEulerianCircuit(out aCircuit: TIntVector): Boolean;
    function  IsDag(constref aRoot: TVertex): Boolean; inline;
    function  IsDagI(aRoot: SizeInt): Boolean;
    function  MaxBitMatrixSize: SizeInt; inline;
  { creates internal transitive closure }
    procedure FillReachabilityMatrix;
  { returns internal transitive closure }
    function  CreateReachabilityMatrix: TReachabilityMatrix;
  { returns array of vertex indices in topological order staring from aRoot, without any acyclic checks }
    function  TopologicalSort(constref aRoot: TVertex; aOrder: TSortOrder = soAsc): TIntArray; inline;
    function  TopologicalSortI(aRoot: SizeInt; aOrder: TSortOrder = soAsc): TIntArray;
  { returns count of the strong connected components; the corresponding element of the
    aCompIds will contain it component index(used Gabow's algotitm) }
    function  FindStrongComponents(out aCompIds: TIntArray): SizeInt;

    function  Clone: TGSimpleDiGraph;
    function  Reverse: TGSimpleDiGraph;
    property  ReachabilityMatrixValid: Boolean read GetClosureValid;
  end;

implementation
{$B-}{$COPERATORS ON}
uses
  bufstream;

{ TGSimpleDiGraph.TReachabilityMatrix }

function TGSimpleDiGraph.TReachabilityMatrix.GetSize: SizeInt;
begin
  Result := FMatrix.Size;
end;

constructor TGSimpleDiGraph.TReachabilityMatrix.Create(constref aMatrix: TSquareBitMatrix; constref aIds: TIntArray);
begin
  FMatrix := aMatrix;
  FIds := aIds;
end;

function TGSimpleDiGraph.TReachabilityMatrix.Reachable(aSrc, aDst: SizeInt): Boolean;
begin
  Result := FMatrix[FIds[aSrc], FIds[aDst]];
end;

procedure TGSimpleDiGraph.TReachabilityMatrix.Discard;
begin
  FMatrix.Clear;
  FIds := nil;
end;

{ TGSimpleDiGraph }

function TGSimpleDiGraph.GetClosureValid: Boolean;
begin
  Result := FReachabilityMatrix.Size <> 0;
end;

procedure TGSimpleDiGraph.DoRemoveVertex(aIndex: SizeInt);
var
  I, J: SizeInt;
  p: ^TAdjItem;
  CurrEdges: TAdjList.TAdjItemArray;
begin
  FEdgeCount -= FNodeList[aIndex].AdjList.Count;
  for p in FNodeList[aIndex].AdjList do
    Dec(FNodeList[p^.Destination].Tag);
  Delete(aIndex);
  for I := 0 to Pred(VertexCount) do
    begin
      CurrEdges := FNodeList[I].AdjList.ToArray;
      FNodeList[I].AdjList.MakeEmpty;
      for J := 0 to System.High(CurrEdges) do
        begin
          if CurrEdges[J].Destination <> aIndex then
            begin
              if CurrEdges[J].Destination > aIndex then
                Dec(CurrEdges[J].Destination);
              FNodeList[I].AdjList.Add(CurrEdges[J]);
            end;
        end;
    end;
  FReachabilityMatrix.Discard;
end;

function TGSimpleDiGraph.DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
begin
  if aSrc = aDst then
    exit(False);
  Result := FNodeList[aSrc].AdjList.Add(TAdjItem.Create(aDst, aData));
  if Result then
    begin
      Inc(FNodeList[aDst].Tag);
      Inc(FEdgeCount);
      FReachabilityMatrix.Discard;
    end;
end;

function TGSimpleDiGraph.DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
begin
  if aSrc = aDst then
    exit(False);
  Result := FNodeList[aSrc].AdjList.Remove(aDst);
  if Result then
    begin
      Dec(FNodeList[aDst].Tag);
      Dec(FEdgeCount);
      FReachabilityMatrix.Discard;
    end;
end;

function TGSimpleDiGraph.FindCycle(aRoot: SizeInt; out aCycle: TIntArray): SizeInt;
var
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  InStack: TBitVector;
  InOrder,
  Parents: TIntArray;
  Counter, Next: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  InOrder := CreateIntArray;
  Parents := CreateIntArray;
  InStack.Size := VertexCount;
  InOrder[aRoot] := 0;
  Counter := 1;
  {%H-}Stack.Push(aRoot);
  while Stack.TryPeek(aRoot) do
    if AdjEnums[aRoot].MoveNext then
      begin
        Next := AdjEnums[aRoot].Current;
        if InOrder[Next] = -1 then
          begin
            Parents[Next] := aRoot;
            InOrder[Next] := Counter;
            InStack[Next] := True;
            Inc(Counter);
            Stack.Push(Next);
          end
        else
          if (InOrder[aRoot] >= InOrder[Next]) and InStack[Next] then
            begin
              aCycle := TreeToCycle(Parents, Next, aRoot);
              exit(Counter);
            end;
      end
    else
      InStack[Stack.Pop] := False;
  Result := Counter;
end;

function TGSimpleDiGraph.TopoSort(aIndex: SizeInt): TIntArray;
var
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  Visited: TBitVector;
  Counter, Next: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  Result := CreateIntArray;
  Visited.Size := VertexCount;
  Counter := VertexCount;
  Visited[aIndex] := True;
  {%H-}Stack.Push(aIndex);
  while Stack.TryPeek(aIndex) do
    if AdjEnums[aIndex].MoveNext then
      begin
        Next := AdjEnums[aIndex].Current;
        if not Visited[Next] then
          begin
            Visited[Next] := True;
            Stack.Push(Next);
          end;
      end
    else
      begin
        Dec(Counter);
        Result[Counter] := Stack.Pop;
      end;
end;

function TGSimpleDiGraph.SearchForStrongComponents(out aCompIds: TIntArray): SizeInt;
var
  Stack, VtxStack, PathStack: TIntStack;
  AdjEnums: TAdjEnumArray;
  InOrder: TIntArray;
  I, Counter, Curr, Next: SizeInt;
begin
  InOrder := CreateIntArray;
  aCompIds := CreateIntArray;
  AdjEnums := CreateAdjEnumArray;
  Counter := 0;
  Result := 0;
  for I := 0 to Pred(VertexCount) do
    if InOrder[I] = -1 then
      begin
        InOrder[I] := Counter;
        Inc(Counter);
        {%H-}Stack.Push(I);
        VtxStack{%H-}.Push(I);
        PathStack{%H-}.Push(I);
        while Stack.TryPeek(Curr) do
          if AdjEnums[{%H-}Curr].MoveNext then
            begin
              Next := AdjEnums[Curr].Current;
              if InOrder[Next] = -1 then
                begin
                  InOrder[Next] := Counter;
                  Inc(Counter);
                  Stack.Push(Next);
                  VtxStack.Push(Next);
                  PathStack.Push(Next);
                end
              else
                if aCompIds[Next] = -1 then
                  while InOrder[PathStack.Peek] > InOrder[Next] do
                    PathStack.Pop;
            end
          else
            begin
              Curr := Stack.Pop;
              if PathStack.Peek = Curr then
                begin
                  PathStack.Pop;
                  repeat
                    Next := VtxStack.Pop;
                    aCompIds[Next] := Result;
                  until Next = Curr;
                  Inc(Result);
                end;
            end;
      end;
end;

function TGSimpleDiGraph.GetReachabilityMatrix: TReachabilityMatrix;
var
  Stack: TIntStack;
  InOrder, Parents, Ids: TIntArray;
  m: TSquareBitMatrix;
  AdjEnums: TAdjEnumArray;
  I, J, ScCount, Counter, Curr, Next: SizeInt;
begin
  ScCount := FindStrongComponents(Ids);
  m := TSquareBitMatrix.Create(ScCount);
  if ScCount = 1 then
    begin
      m[0, 0] := True;
      exit(TReachabilityMatrix.Create(m, Ids));
    end;
  InOrder := CreateIntArray;
  Parents := CreateIntArray;
  AdjEnums := CreateAdjEnumArray;
  Counter := 0;
  for I := 0 to Pred(ScCount) do
    if InOrder[I] = -1 then
      begin
        InOrder[I] := Counter;
        {%H-}Stack.Push(I);
        Inc(Counter);
        while Stack.TryPeek(Curr) do
          if AdjEnums[{%H-}Curr].MoveNext then
            begin
              Next := AdjEnums[Curr].Current;
              m[Ids[Curr], Ids[Next]] := True;
              if InOrder[Next] = -1 then
                begin
                  Parents[Next] := Curr;
                  InOrder[Next] := Counter;
                  Inc(Counter);
                  Stack.Push(Next);
                end;
            end
          else
            begin
              Next := Stack.Pop;
              Curr := Parents[Next];
              if (Curr <> -1) and (InOrder[Next] < InOrder[Curr]) then
                begin
                  Curr := Ids[Curr];
                  Next := Ids[Next];
                  for J := 0 to Pred(ScCount) do
                    if m[Next, Ids[J]] then
                      m[Curr, Ids[J]] := True;
                end;
            end;
      end;
  Result := TReachabilityMatrix.Create(m, Ids);
end;

function TGSimpleDiGraph.AddVertex(constref aVertex: TVertex; out aIndex: SizeInt): Boolean;
begin
  Result := not FindOrAdd(aVertex, aIndex);
  if Result then
    begin
      FNodeList[aIndex].Tag := 0;
      FReachabilityMatrix.Discard;
    end;
end;

function TGSimpleDiGraph.AddVertex(constref aVertex: TVertex): Boolean;
var
  Dummy: SizeInt;
begin
  Result := AddVertex(aVertex, Dummy);
end;

procedure TGSimpleDiGraph.RemoveVertex(constref aVertex: TVertex);
begin
  RemoveVertexI(IndexOf(aVertex));
end;

procedure TGSimpleDiGraph.RemoveVertexI(aIndex: SizeInt);
begin
  CheckIndexRange(aIndex);
  DoRemoveVertex(aIndex);
end;

function TGSimpleDiGraph.AddEdge(constref aSrc, aDst: TVertex; aData: TEdgeData): Boolean;
var
  SrcIdx, DstIdx: SizeInt;
begin
  AddVertex(aSrc, SrcIdx);
  AddVertex(aDst, DstIdx);
  Result := DoAddEdge(SrcIdx, DstIdx, aData);
end;

function TGSimpleDiGraph.AddEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := AddEdge(aSrc, aDst, DefaultEdgeData);
end;

function TGSimpleDiGraph.AddEdgeI(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  Result := DoAddEdge(aSrc, aDst, aData);
end;

function TGSimpleDiGraph.AddEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := AddEdgeI(aSrc, aDst, DefaultEdgeData);
end;

function TGSimpleDiGraph.RemoveEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := RemoveEdgeI(IndexOf(aSrc), IndexOf(aDst));
end;

function TGSimpleDiGraph.RemoveEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  Result := DoRemoveEdge(aSrc, aDst);
end;

procedure TGSimpleDiGraph.SaveToStream(aStream: TStream; aWriteVertex: TOnWriteVertex; aWriteData: TOnWriteData);
var
  Header: TStreamHeader;
  I: SizeInt;
  Edge: TEdge;
  wbs: TWriteBufStream;
begin
  if not (Assigned(aWriteVertex) and Assigned(aWriteData)) then
    raise ELGraphError.Create(SEWriteCallbackMissed);
  wbs := TWriteBufStream.Create(aStream);
  try
    //write header
    Header.Magic := GRAPH_MAGIC;
    Header.Version := CURRENT_VERSION;
    Header.TitleSize := System.Length(Title);
    Header.VertexCount := VertexCount;
    Header.EdgeCount := EdgeCount;
    wbs.WriteBuffer(Header, SizeOf(Header));
    //write title
    if Header.TitleSize > 0 then
      wbs.WriteBuffer(FTitle[1], Header.TitleSize);
    //write Items, but does not save any info about connected
    //this should allow transfer data between directed/undirected graphs ???
    //or need save edges from dfs ???
    for I := 0 to Pred(Header.VertexCount) do
      aWriteVertex(wbs, FNodeList[I].Vertex);
    //write edges
    for Edge in Edges do
      begin
        wbs.WriteBuffer(Edge.Source, SizeOf(Edge.Source));
        wbs.WriteBuffer(Edge.Destination, SizeOf(Edge.Destination));
        aWriteData(wbs, Edge.Data);
      end;
  finally
    wbs.Free;
  end;
end;

procedure TGSimpleDiGraph.LoadFromStream(aStream: TStream; aReadVertex: TOnReadVertex; aReadData: TOnReadData);
var
  h: TStreamHeader;
  I, vInd: SizeInt;
  e: TEdge;
  Vertex: TVertex;
  rbs: TReadBufStream;
begin
  if not (Assigned(aReadVertex) and Assigned(aReadData)) then
    raise ELGraphError.Create(SEReadCallbackMissed);
  rbs := TReadBufStream.Create(aStream);
  try
    //read header
    rbs.ReadBuffer(h, SizeOf(h));
    if h.Magic <> GRAPH_MAGIC then
      raise ELGraphError.Create(SEUnknownGraphStreamFmt);
    if h.Version > CURRENT_VERSION then
      raise ELGraphError.Create(SEUnsuppGraphFmtVersion);
    Clear;
    EnsureCapacity(h.VertexCount);
    //read title
    System.SetLength(FTitle, h.TitleSize);
    if h.TitleSize > 0 then
      rbs.ReadBuffer(FTitle[1], h.TitleSize);
    //read Items
    for I := 0 to Pred(h.VertexCount) do
      begin
        aReadVertex(rbs, Vertex);
        if not AddVertex(Vertex, vInd) then
          raise ELGraphError.Create(SEGraphStreamCorrupt);
        if vInd <> I then
          raise ELGraphError.Create(SEGraphStreamReadIntern);
      end;
    //read edges
    for I := 0 to Pred(h.EdgeCount) do
      begin
        rbs.ReadBuffer(e.Source, SizeOf(e.Source));
        rbs.ReadBuffer(e.Destination, SizeOf(e.Destination));
        aReadData(rbs, e.Data);
        AddEdgeI(e.Source, e.Destination, e.Data);
      end;
  finally
    rbs.Free;
  end;
end;

procedure TGSimpleDiGraph.SaveToFile(const aFileName: string; aWriteVertex: TOnWriteVertex;
  aWriteData: TOnWriteData);
var
  fs: TStream;
begin
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(fs, aWriteVertex, aWriteData);
  finally
    fs.Free;
  end;
end;

procedure TGSimpleDiGraph.LoadFromFile(const aFileName: string; aReadVertex: TOnReadVertex; aReadData: TOnReadData);
var
  fs: TStream;
begin
  fs := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs, aReadVertex, aReadData);
  finally
    fs.Free;
  end;
end;

function TGSimpleDiGraph.InDegree(constref aVertex: TVertex): SizeInt;
begin
  Result := InDegreeI(IndexOf(aVertex));
end;

function TGSimpleDiGraph.InDegreeI(aIndex: SizeInt): SizeInt;
begin
  CheckIndexRange(aIndex);
  Result := FNodeList[aIndex].Tag;
end;

function TGSimpleDiGraph.OutDegree(constref aVertex: TVertex): SizeInt;
begin
  Result := OutDegreeI(IndexOf(aVertex));
end;

function TGSimpleDiGraph.OutDegreeI(aIndex: SizeInt): SizeInt;
begin
  CheckIndexRange(aIndex);
  Result := FNodeList[aIndex].AdjList.Count;
end;

function TGSimpleDiGraph.Degree(constref aVertex: TVertex): SizeInt;
begin
  Result := DegreeI(IndexOf(aVertex));
end;

function TGSimpleDiGraph.DegreeI(aIndex: SizeInt): SizeInt;
begin
  CheckIndexRange(aIndex);
  Result := FNodeList[aIndex].AdjList.Count + FNodeList[aIndex].Tag;
end;

function TGSimpleDiGraph.Isolated(constref aVertex: TVertex): Boolean;
begin
  Result := Degree(aVertex) = 0;
end;

function TGSimpleDiGraph.IsolatedI(aIndex: SizeInt): Boolean;
begin
  Result := DegreeI(aIndex) = 0;
end;

function TGSimpleDiGraph.IsSource(constref aVertex: TVertex): Boolean;
begin
  Result := IsSourceI(IndexOf(aVertex));
end;

function TGSimpleDiGraph.IsSourceI(aIndex: SizeInt): Boolean;
begin
  CheckIndexRange(aIndex);
  Result := (FNodeList[aIndex].AdjList.Count <> 0) and (FNodeList[aIndex].Tag = 0);
end;

function TGSimpleDiGraph.IsSink(constref aVertex: TVertex): Boolean;
begin
  Result := IsSinkI(IndexOf(aVertex));
end;

function TGSimpleDiGraph.IsSinkI(aIndex: SizeInt): Boolean;
begin
  CheckIndexRange(aIndex);
  Result := (FNodeList[aIndex].AdjList.Count = 0) and (FNodeList[aIndex].Tag <> 0);
end;

function TGSimpleDiGraph.SourceCount: SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I := 0 to Pred(VertexCount) do
    if (FNodeList[I].AdjList.Count <> 0) and (FNodeList[I].Tag = 0) then
      Inc(Result);
end;

function TGSimpleDiGraph.SinkCount: SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I := 0 to Pred(VertexCount) do
    if (FNodeList[I].AdjList.Count = 0) and (FNodeList[I].Tag <> 0) then
      Inc(Result);
end;

function TGSimpleDiGraph.PathExists(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := PathExistsI(IndexOf(aSrc), IndexOf(aDst));
end;

function TGSimpleDiGraph.PathExistsI(aSrc, aDst: SizeInt): Boolean;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  if aSrc = aDst then
    exit(True);
  if ReachabilityMatrixValid then
    exit(FReachabilityMatrix.Reachable(aSrc, aDst));
  Result := CheckPathExists(aSrc, aDst);
end;

function TGSimpleDiGraph.ContainsCycle(constref aRoot: TVertex; out aCycle: TIntArray): Boolean;
begin
  Result := ContainsCycleI(IndexOf(aRoot), aCycle);
end;

function TGSimpleDiGraph.ContainsCycleI(aRoot: SizeInt; out aCycle: TIntArray): Boolean;
begin
  CheckIndexRange(aRoot);
  if VertexCount < 2 then
    exit(False);
  aCycle := nil;
  FindCycle(aRoot, aCycle);
  Result := System.Length(aCycle) <> 0;
end;

function TGSimpleDiGraph.ContainsEulerianCircuit: Boolean;
var
  I, d: SizeInt;
begin
  if VertexCount < 2 then
    exit(False);
  d := 0;
  for I := 0 to Pred(VertexCount) do
    begin
      if InDegreeI(I) <> OutDegreeI(I) then
        exit(False);
      d += DegreeI(I);
    end;
  Result := d > 0;
end;

function TGSimpleDiGraph.FindEulerianCircuit(out aCircuit: TIntVector): Boolean;
var
  g: TGSimpleDiGraph = nil;
  Stack: TIntStack;
  s, d: SizeInt;
begin
  if not ContainsEulerianCircuit then
    exit(False);
  g := Clone;
  try
    s := 0;
    while g.DegreeI(s) = 0 do
      Inc(s);
    aCircuit.Add(s);
    repeat
      repeat
        if not g.FNodeList[s].AdjList.FindFirst(d) then
          break;
        Stack.Push(s);
        g.RemoveEdgeI(s, d);
        s := d;
      until False;
      if not Stack.TryPop(s) then
        break;
      aCircuit.Add(s);
    until False;
  finally
    g.Free;
  end;
  Result := aCircuit.Count > 0;
  if Result then
    TIntVectorHelper.Reverse(aCircuit);
end;

function TGSimpleDiGraph.IsDag(constref aRoot: TVertex): Boolean;
begin
  Result := IsDagI(IndexOf(aRoot));
end;

function TGSimpleDiGraph.IsDagI(aRoot: SizeInt): Boolean;
var
  Dummy: TIntArray = nil;
  I: SizeInt;
begin
  CheckIndexRange(aRoot);
  if VertexCount = 1 then
    exit(True);
  if (FNodeList[aRoot].AdjList.Count = 0) or (FNodeList[aRoot].Tag <> 0) then
    exit(False);
  I := FindCycle(aRoot, Dummy);
  Result := (System.Length(Dummy) = 0) and (I = VertexCount);
end;

function TGSimpleDiGraph.MaxBitMatrixSize: SizeInt;
begin
  Result := TSquareBitMatrix.MaxSize;
end;

procedure TGSimpleDiGraph.FillReachabilityMatrix;
begin
  if IsEmpty or ReachabilityMatrixValid then
    exit;
  FReachabilityMatrix := GetReachabilityMatrix;
end;

function TGSimpleDiGraph.CreateReachabilityMatrix: TReachabilityMatrix;
begin
  if IsEmpty then
    exit;
  Result := GetReachabilityMatrix;
end;

function TGSimpleDiGraph.TopologicalSort(constref aRoot: TVertex; aOrder: TSortOrder): TIntArray;
begin
  Result := TopologicalSortI(IndexOf(aRoot), aOrder);
end;

function TGSimpleDiGraph.TopologicalSortI(aRoot: SizeInt; aOrder: TSortOrder): TIntArray;
begin
  CheckIndexRange(aRoot);
  Result := TopoSort(aRoot);
  if aOrder = soDesc then
    TIntHelper.Reverse(Result);
end;

function TGSimpleDiGraph.FindStrongComponents(out aCompIds: TIntArray): SizeInt;
begin
  if IsEmpty then
    exit(0);
  if VertexCount = 1 then
    exit(1);
  Result := SearchForStrongComponents(aCompIds);
end;

function TGSimpleDiGraph.Clone: TGSimpleDiGraph;
var
  I: SizeInt;
begin
  Result := TGSimpleDiGraph.Create;
  Result.FCount := VertexCount;
  Result.FEdgeCount := EdgeCount;
  Result.FTitle := Title;
  if NonEmpty then
    begin
      Result.FChainList := System.Copy(FChainList);
      System.SetLength(Result.FNodeList, System.Length(FNodeList));
      for I := 0 to Pred(VertexCount) do
        Result.FNodeList[I].Assign(FNodeList[I]);
    end;
end;

function TGSimpleDiGraph.Reverse: TGSimpleDiGraph;
var
  e: TEdge;
  v: TVertex;
begin
  Result := TGSimpleDiGraph.Create;
  for v in Vertices do
    Result.AddVertex(v);
  for e in Edges do
    Result.AddEdgeI(e.Destination, e.Source, e.Data);
end;

end.

