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
{$INLINE ON}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGArrayHelpers,
  LGVector,
  LGraphUtils,
  LGStrConst;

type

  { TGSimpleDiGraph is simple sparse directed graph based on adjacency lists;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGSimpleDiGraph<TVertex, TEdgeData, TVertexEqRel> = class(
    specialize TGCustomGraph<TVertex, TEdgeData, TVertexEqRel>)
  protected
    procedure DoRemoveVertex(aIndex: SizeInt);
    function  DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
    function  FindCycle(out aCycle: TIntVector): Boolean;
  public
  { returns True and vertex index, if it was added, False otherwise }
    function  AddVertex(constref aVertex: TVertex; out aIndex: SizeInt): Boolean;
    function  AddVertex(constref aVertex: TVertex): Boolean; inline;
    procedure RemoveVertex(constref aVertex: TVertex); inline;
    procedure RemoveVertexI(aVtxIndex: SizeInt);
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
    function  InDegree(constref v: TVertex): SizeInt; inline;
    function  InDegreeI(aVtxIndex: SizeInt): SizeInt;
    function  OutDegree(constref aVertex: TVertex): SizeInt; inline;
    function  OutDegreeI(aVtxIndex: SizeInt): SizeInt;
    function  Degree(constref aVertex: TVertex): SizeInt; inline;
    function  DegreeI(aVtxIndex: SizeInt): SizeInt;
    function  Isolated(constref aVertex: TVertex): Boolean; inline;
    function  IsolatedI(aVtxIndex: SizeInt): Boolean; inline;
  { checks whether exists any cycle in graph;
    if True then aCycle will contain indices of the vertices of the cycle }
    function  ContainsCycle(out aCycle: TIntVector): Boolean;
    function  ContainsEulerCycle: Boolean;
    function  FindEulerCycle: TIntArray;

    function  Clone: TGSimpleDiGraph;
    function  Reverse: TGSimpleDiGraph;
  end;

implementation
{$B-}{$COPERATORS ON}
uses
  bufstream;

{ TGSimpleDiGraph }

procedure TGSimpleDiGraph.DoRemoveVertex(aIndex: SizeInt);
var
  I, J: SizeInt;
  p: ^TAdjItem;
  CurrEdges: TAdjList.TAdjItemArray;
begin
  FEdgeCount -= AdjList[aIndex]^.Count;
  for p in AdjList[aIndex]^ do
    Dec(AdjList[p^.Destination]^.FInDegree);
  Delete(aIndex);
  for I := 0 to Pred(VertexCount) do
    begin
      CurrEdges := AdjList[I]^.ToArray;
      AdjList[I]^.MakeEmpty;
      for J := 0 to System.High(CurrEdges) do
        begin
          if CurrEdges[J].Destination <> aIndex then
            begin
              if CurrEdges[J].Destination > aIndex then
                Dec(CurrEdges[J].Destination);
              AdjList[I]^.Add(CurrEdges[J]);
            end;
        end;
    end;
  //FConnectedValid := False;
end;

function TGSimpleDiGraph.DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
var
  p: ^TAdjItem;
begin
  if aSrc = aDst then
    exit(False);
  //Result := not AdjList[aSrc]^.FindOrAdd(aDst, p);
  Result := AdjList[aSrc]^.Add(TAdjItem.Create(aDst, aData));
  if Result then
    begin
      //p^.Destination := aDst;
      //p^.Data := aData;
      Inc(AdjList[aDst]^.FInDegree);
      Inc(FEdgeCount);
      //FConnectedValid := False;
    end;
end;

function TGSimpleDiGraph.DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
begin
  if aSrc = aDst then
    exit(False);
  Result := AdjList[aSrc]^.Remove(aDst);
  if Result then
    begin
      Dec(AdjList[aDst]^.FInDegree);
      Dec(FEdgeCount);
      //FConnectedValid := False;
    end;
end;

function TGSimpleDiGraph.FindCycle(out aCycle: TIntVector): Boolean;
var
  Stack: TIntStack;
  Colors: TColorVector;
  v: TIntArray;
  Curr, Next: SizeInt;
begin
  Colors.Size := VertexCount;
  v := CreateIntArray;
  Curr := 0;
  repeat
    Colors[Curr] := vclGray;
    for Next in AdjVerticesI(Curr) do
      if Colors[Next] = vclNone then
        begin
          Colors[Next] := vclWhite;
          Stack.Push(Next);
          v[Next] := Curr;
        end
      else
        if Colors[Curr] = vclGray then
          begin
            aCycle := CycleChainFromTree(v, Next, Curr);
            exit(True);
          end;
      Colors[Curr] := vclBlack;
  until not Stack.TryPop(Curr);
  Result := False;
end;

function TGSimpleDiGraph.AddVertex(constref aVertex: TVertex; out aIndex: SizeInt): Boolean;
var
  p: PAdjList;
begin
  Result := not FindOrAdd(aVertex, aIndex);
  if Result then
    begin
      p := AdjList[aIndex];
      p^.FInDegree := 0;
      p^.FCompIndex := -1;
      //FConnectedValid := False;
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

procedure TGSimpleDiGraph.RemoveVertexI(aVtxIndex: SizeInt);
begin
  CheckIndexRange(aVtxIndex);
  DoRemoveVertex(aVtxIndex);
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
  Result := AddEdge(aSrc, aDst, CFData);
end;

function TGSimpleDiGraph.AddEdgeI(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  Result := DoAddEdge(aSrc, aDst, aData);
end;

function TGSimpleDiGraph.AddEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := AddEdgeI(aSrc, aDst, CFData);
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
    Header.Magic := LGRAPH_MAGIC;
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
      aWriteVertex(wbs, AdjList[I]^.Vertex);
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
    if h.Magic <> LGRAPH_MAGIC then
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
        Vertex := aReadVertex(rbs);
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
        e.Data := aReadData(rbs);
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

function TGSimpleDiGraph.InDegree(constref v: TVertex): SizeInt;
begin
  Result := InDegreeI(IndexOf(v));
end;

function TGSimpleDiGraph.InDegreeI(aVtxIndex: SizeInt): SizeInt;
begin
  CheckIndexRange(aVtxIndex);
  Result := AdjList[aVtxIndex]^.FInDegree;
end;

function TGSimpleDiGraph.OutDegree(constref aVertex: TVertex): SizeInt;
begin
  Result := OutDegreeI(IndexOf(aVertex));
end;

function TGSimpleDiGraph.OutDegreeI(aVtxIndex: SizeInt): SizeInt;
begin
  CheckIndexRange(aVtxIndex);
  Result := AdjList[aVtxIndex]^.Count;
end;

function TGSimpleDiGraph.Degree(constref aVertex: TVertex): SizeInt;
begin
  Result := DegreeI(IndexOf(aVertex));
end;

function TGSimpleDiGraph.DegreeI(aVtxIndex: SizeInt): SizeInt;
var
  p: PAdjList;
begin
  CheckIndexRange(aVtxIndex);
  p := AdjList[aVtxIndex];
  Result := p^.Count + p^.FInDegree;
end;

function TGSimpleDiGraph.Isolated(constref aVertex: TVertex): Boolean;
begin
  Result := Degree(aVertex) = 0;
end;

function TGSimpleDiGraph.IsolatedI(aVtxIndex: SizeInt): Boolean;
begin
  Result := DegreeI(aVtxIndex) = 0;
end;

function TGSimpleDiGraph.ContainsCycle(out aCycle: TIntVector): Boolean;
begin
  if VertexCount < 2 then
    exit(False);
  Result := FindCycle(aCycle);
end;

function TGSimpleDiGraph.ContainsEulerCycle: Boolean;
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

function TGSimpleDiGraph.FindEulerCycle: TIntArray;
var
  g: TGSimpleDiGraph = nil;
  Stack: TIntStack;
  CurrPath: TIntDeque;
  s, d: SizeInt;
begin
  if not ContainsEulerCycle then
    exit(nil);
  g := Clone;
  try
    s := 0;
    while g.DegreeI(s) = 0 do
      Inc(s);
    CurrPath.PushLast(s);
    repeat
      repeat
        if not g.AdjList[s]^.FindFirst(d) then
          break;
        Stack.Push(s);
        g.RemoveEdgeI(s, d);
        s := d;
      until False;
      if not Stack.TryPop(s) then
        break;
      CurrPath.PushFirst(s);
    until False;
     Result := CurrPath.ToArray;
  finally
    g.Free;
  end;
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
  //Result.FConnected := Connected;
  //Result.FConnectedValid := ConnectedValid;
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

