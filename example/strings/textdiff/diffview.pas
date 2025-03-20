unit DiffView;

{$MODE OBJFPC}{$H+}{$MODESWITCH ADVANCEDRECORDS}
{$WARN 5024 OFF : Parameter "$1" not used}
{$WARN 6058 OFF : Call to subroutine "$1" marked as inline is not inlined}
{$WARN 5091 OFF : Local variable "$1" of a managed type does not seem to be initialized}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, Grids, ActnList,
  LgSeqUtils, DiffUtils, LgHashMap;

type

  { TfrmDiffView }

  TfrmDiffView = class(TForm)
    acNextBlock: TAction;
    acPrevBlock: TAction;
    acFirstBlock: TAction;
    acLastBlock: TAction;
    aclstDiffView: TActionList;
    acShowLineDiffs: TAction;
    acClose: TAction;
    pnView: TPanel;
    sgSource: TStringGrid;
    Splitter1: TSplitter;
    sgTarget: TStringGrid;
    stbStatus: TStatusBar;
    ToolBar1: TToolBar;
    tlbtInfo: TToolButton;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
  //------------------
    procedure acCloseExecute(Sender: TObject);
    procedure acFirstBlockExecute(Sender: TObject);
    procedure acFirstBlockUpdate(Sender: TObject);
    procedure acLastBlockExecute(Sender: TObject);
    procedure acLastBlockUpdate(Sender: TObject);
    procedure acNextBlockExecute(Sender: TObject);
    procedure acNextBlockUpdate(Sender: TObject);
    procedure acPrevBlockExecute(Sender: TObject);
    procedure acPrevBlockUpdate(Sender: TObject);
    procedure acShowLineDiffsExecute(Sender: TObject);
  //------------------
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  //------------------
    procedure sgSourceDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure sgSourcePrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
    procedure sgSourceResize(Sender: TObject);
    procedure sgSourceSelection(Sender: TObject; aCol, aRow: Integer);
    procedure sgSourceTopLeftChanged(Sender: TObject);
    procedure sgTargetDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure sgTargetPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
    procedure sgTargetResize(Sender: TObject);
    procedure sgTargetSelection(Sender: TObject; aCol, aRow: Integer);
    procedure sgTargetTopLeftChanged(Sender: TObject);
    procedure stbStatusDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
  private
  type
    TLineDiffMapType = specialize TGLiteChainHashMap<Integer, TLineDiff, Integer>;
    TLineDiffMap     = TLineDiffMapType.TMap;
  const
    ColorMap: array[TSeqEditOp] of TColor = ($dbfdfb, $cacaff, $fed1d5, $d2f4d0);
    clGap: TColor     = clBtnFace;
    clFix: TColor     = clBtnFace;
    clLineDel: TColor = $70a7fa;
    clLineIns: TColor = $97d9fd;
    CurrChangeFmt     = '  Current change: %s line %s';
  var
    FOldAppHint: TNotifyEvent;
    FSourceLines,
    FTargetLines: TStrings;
    FStat: TSeqPatchStat;
    FEditList: array of TSeqEditOp;
    FChangeList: array of Integer;
    FLineDiffMap: TLineDiffMap;
    FFlags: TDiffFlags;
    FAlgo: TLcsAlgo;
    FCurrChangeIndex: Integer;
    FAlreadyShown: Boolean;
    function  MakeNodeList(aLines: TStrings): TNodeList;
    procedure FillGrids;
    procedure UpdateNavigate;
    procedure BlockChanged;
    procedure ShowStat;
    procedure ShowDiffs;
    procedure AppShowHint(Sender: TObject);
  public
    procedure SetViewData(aSrc, aTrg: TStrings; aFont: TFont; aFlags: TDiffFlags; Algo: TLcsAlgo);
  end;

var
  frmDiffView: TfrmDiffView;

implementation
{$B-}{$COPERATORS ON}
{$R *.lfm}
uses
  LgVector, LgHash, UnicodeData;

{ TfrmDiffView }

procedure TfrmDiffView.FormShow(Sender: TObject);
begin
  if FAlreadyShown then exit;
  FAlreadyShown := True;
  ShowDiffs;
end;

type
  TUcs4Pos = record
    Index: SizeInt;
    Code: Ucs4Char;
    class function HashCode(const aUp: TUcs4Pos): SizeInt; static; inline;
    class function Equal(const L, R: TUcs4Pos): Boolean; static; inline;
    constructor Make(aIndex: SizeInt; aCode: Ucs4Char);
  end;
  TUcs4PosSeq = array of TUcs4Pos;

class function TUcs4Pos.HashCode(const aUp: TUcs4Pos): SizeInt;
begin
  Result := JdkHash(DWord(aUp.Code));
end;

class function TUcs4Pos.Equal(const L, R: TUcs4Pos): Boolean;
begin
  Result := L.Code = R.Code;
end;

constructor TUcs4Pos.Make(aIndex: SizeInt; aCode: Ucs4Char);
begin
  Index := aIndex;
  Code := aCode;
end;

function IsWhiteSpaceUcs4(aChar: DWord): Boolean;
const
  WS = [9, 32, 160];
begin
  if aChar < $a1 then
    exit(aChar in WS)
  else
    if aChar > $167f then
      exit(UnicodeData.GetProps(aChar)^.WhiteSpace);
  Result := False;
end;

function Ucs4CharToLower(aChar: DWord): DWord; inline;
begin
  Result := UnicodeData.GetProps(aChar)^.SimpleLowerCase;
  if Result = 0 then Result := aChar;
end;

function FilterSeq(const s: TUcs4Seq; const aFlags: TDiffFlags): TUcs4PosSeq;
var
  r: TUcs4PosSeq;
  I, J, Len: SizeInt;
  c: Ucs4Char;
  IgnoreCase, IgnoreWS, IgnoreWSChange: Boolean;
begin
  if s = nil then exit(nil);
  Len := Length(s);
  SetLength(r, Length(s));
  IgnoreCase := dfIgnoreCase in aFlags;
  IgnoreWS := dfIgnoreWSChars in aFlags;
  IgnoreWSChange := dfIgnoreWSChange in aFlags;
  I := 0; J := 0;
  while I < Len do begin
    c := s[I];
    if IgnoreWS then begin
      if IsWhiteSpaceUcs4(c) then begin
        Inc(I); continue;
      end;
    end else
      if IgnoreWSChange and IsWhiteSpaceUcs4(c) then begin
        r[J] := TUcs4Pos.Make(I, c);
        Inc(I); Inc(J);
        while (I < Len) and IsWhiteSpaceUcs4(s[I]) do Inc(I);
        continue;
      end;
    if IgnoreCase then
      c := Ucs4Char(Ucs4CharToLower(c));
    r[J] := TUcs4Pos.Make(I, c);
    Inc(I); Inc(J);
  end;
  SetLength(r, J);
  Result := r;
end;

procedure MakeDiffOpt(const aSrc, aTrg: TUcs4Seq; const aFlags: TDiffFlags; var Diff: TUcs4Diff);
type
  TUtils = specialize TGSeqUtil<TUcs4Pos, TUcs4Pos>;
var
  s, t: TUcs4PosSeq;
  dif: TUtils.TDiffV;
  I: SizeInt;
begin
  s := FilterSeq(aSrc, aFlags);
  t := FilterSeq(aTrg, aFlags);
  TUtils.Diff(s, t, dif);
  Diff.SourceChanges.EnsureCapacity(Length(aSrc));
  for I in dif.SourceChanges do
    Diff.SourceChanges.UncBits[s[I].Index] := True;
  Diff.TargetChanges.EnsureCapacity(Length(aTrg));
  for I in dif.TargetChanges do
    Diff.TargetChanges.UncBits[t[I].Index] := True;
end;

procedure MakeLineDiff(p: PLineDiff; const aSrc, aTrg: string; const aFlags: TDiffFlags);
begin
  p^.Source := Utf8ToUcs4Seq(aSrc);
  p^.Target := Utf8ToUcs4Seq(aTrg);
  if aFlags - [dfIgnoreEmptyLines] = [] then
    TUcs4DiffUtil.Diff(p^.Source, p^.Target, p^.Diff)
  else
    MakeDiffOpt(p^.Source, p^.Target, aFlags, p^.Diff);
end;

procedure TfrmDiffView.sgSourceDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  x, y, I: Integer;
  p: PLineDiff;
  c: string;
begin
  if (gdSelected in aState)or(aCol <> 2)or(FEditList[aRow] <> seoReplace) then exit;
  if not FLineDiffMap.FindOrAddMutValue(aRow, p) then
    MakeLineDiff(p, sgSource.Cells[aCol, aRow], sgTarget.Cells[aCol, aRow], FFlags);
  y := aRect.Top +(aRect.Height - sgSource.Canvas.TextHeight('Ш')) div 2;
  x := aRect.Left + 4;
  for I := 0 to High(p^.Source) do begin
    c := p^.Source[I].ToUtf8;
    if p^.Diff.SourceChanges.UncBits[I] then
      sgSource.Canvas.Brush.Color := clLineDel
    else
      sgSource.Canvas.Brush.Color := ColorMap[seoReplace];
    sgSource.Canvas.TextOut(x, y, c);
    x += sgSource.Canvas.TextWidth(c);
  end;
end;

procedure TfrmDiffView.FormCreate(Sender: TObject);
begin
  FOldAppHint := Application.OnHint;
  Application.OnHint := @AppShowHint;
  Caption := 'Difference view';
end;

procedure TfrmDiffView.FormDestroy(Sender: TObject);
begin
  Application.OnHint := FOldAppHint;
end;

procedure TfrmDiffView.acNextBlockExecute(Sender: TObject);
begin
  Inc(FCurrChangeIndex);
  sgSource.Row := FChangeList[FCurrChangeIndex];
  BlockChanged;
end;

procedure TfrmDiffView.acFirstBlockExecute(Sender: TObject);
begin
  FCurrChangeIndex := 0;
  sgSource.Row := FChangeList[FCurrChangeIndex];
  BlockChanged;
end;

procedure TfrmDiffView.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmDiffView.acFirstBlockUpdate(Sender: TObject);
begin
  acFirstBlock.Enabled := (FChangeList <> nil) and (sgSource.Row <> FChangeList[0]);
end;

procedure TfrmDiffView.acLastBlockExecute(Sender: TObject);
begin
  FCurrChangeIndex := High(FChangeList);
  sgSource.Row := FChangeList[FCurrChangeIndex];
  BlockChanged;
end;

procedure TfrmDiffView.acLastBlockUpdate(Sender: TObject);
begin
  acLastBlock.Enabled := (FChangeList <> nil) and (sgSource.Row <> FChangeList[High(FChangeList)]);
end;

procedure TfrmDiffView.acNextBlockUpdate(Sender: TObject);
begin
  acNextBlock.Enabled := FCurrChangeIndex < High(FChangeList);
end;

procedure TfrmDiffView.acPrevBlockExecute(Sender: TObject);
begin
  Dec(FCurrChangeIndex);
  sgSource.Row := FChangeList[FCurrChangeIndex];
  BlockChanged;
end;

procedure TfrmDiffView.acPrevBlockUpdate(Sender: TObject);
begin
  acPrevBlock.Enabled := FCurrChangeIndex > 0;
end;

procedure TfrmDiffView.acShowLineDiffsExecute(Sender: TObject);
begin
  if acShowLineDiffs.Checked then begin
    sgSource.OnDrawCell := @sgSourceDrawCell;
    sgTarget.OnDrawCell := @sgTargetDrawCell;
  end else begin
    sgSource.OnDrawCell := nil;
    sgTarget.OnDrawCell := nil;
  end;
  Invalidate;
end;

procedure TfrmDiffView.sgSourcePrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
begin
  if gdSelected in aState then exit;
  case aCol of
    0:begin
        sgSource.Canvas.Brush.Color := clFix;
        sgSource.Canvas.Font.Color := clRed;
        sgSource.Canvas.Font.Style := sgSource.Canvas.Font.Style + [fsBold];
      end;
    1:begin
        sgSource.Canvas.Brush.Color := clFix;
        sgSource.Canvas.Font.Color := clNavy;
      end;
  else
    if FEditList[aRow] = seoInsert then
      sgSource.Canvas.Brush.Color := clGap
    else
      sgSource.Canvas.Brush.Color := ColorMap[FEditList[aRow]];
  end;
end;

procedure TfrmDiffView.sgSourceResize(Sender: TObject);
begin
  sgSource.ColWidths[2] := sgSource.ClientWidth - sgSource.ColWidths[0] - sgSource.ColWidths[1];
end;

procedure TfrmDiffView.sgSourceSelection(Sender: TObject; aCol, aRow: Integer);
begin
  sgSource.Col := 0;
  if sgTarget.Row <> aRow then
    sgTarget.Row := aRow;
end;

procedure TfrmDiffView.sgSourceTopLeftChanged(Sender: TObject);
begin
  if sgTarget.TopRow <> sgSource.TopRow then
    sgTarget.TopRow := sgSource.TopRow;
end;

procedure TfrmDiffView.sgTargetDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  x, y, I: Integer;
  p: PLineDiff;
  c: string;
begin
  if (gdSelected in aState)or(aCol <> 2)or(FEditList[aRow] <> seoReplace) then exit;
  if not FLineDiffMap.FindOrAddMutValue(aRow, p) then
    MakeLineDiff(p, sgSource.Cells[aCol, aRow], sgTarget.Cells[aCol, aRow], FFlags);
  y := aRect.Top +(aRect.Height - sgTarget.Canvas.TextHeight('Ш')) div 2;
  x := aRect.Left + 4;
  for I := 0 to High(p^.Target) do begin
    c := p^.Target[I].ToUtf8;
    if p^.Diff.TargetChanges.UncBits[I] then
      sgTarget.Canvas.Brush.Color := clLineIns
    else
      sgTarget.Canvas.Brush.Color := ColorMap[seoReplace];
    sgTarget.Canvas.TextOut(x, y, c);
    x += sgTarget.Canvas.TextWidth(c);
  end;
end;

procedure TfrmDiffView.sgTargetPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
begin
  if gdSelected in aState then exit;
  case aCol of
    0:begin
        sgTarget.Canvas.Brush.Color := clFix;
        sgTarget.Canvas.Font.Color := clRed;
        sgTarget.Canvas.Font.Style := sgTarget.Canvas.Font.Style + [fsBold];
      end;
    1:begin
        sgTarget.Canvas.Brush.Color := clFix;
        sgTarget.Canvas.Font.Color := clNavy;
      end;
  else
    if FEditList[aRow] = seoDelete then
      sgTarget.Canvas.Brush.Color := clGap
    else
      sgTarget.Canvas.Brush.Color := ColorMap[FEditList[aRow]];
  end;
end;

procedure TfrmDiffView.sgTargetResize(Sender: TObject);
begin
  sgTarget.ColWidths[2] := sgTarget.ClientWidth - sgTarget.ColWidths[0] - sgTarget.ColWidths[1];
end;

procedure TfrmDiffView.sgTargetSelection(Sender: TObject; aCol, aRow: Integer);
begin
  sgTarget.Col := 0;
  if sgSource.Row <> aRow then
    sgSource.Row := aRow;
end;

procedure TfrmDiffView.sgTargetTopLeftChanged(Sender: TObject);
begin
  if sgSource.TopRow <> sgTarget.TopRow then
    sgSource.TopRow := sgTarget.TopRow;
end;

procedure TfrmDiffView.stbStatusDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
var
  y: Integer;
begin
  stbStatus.Canvas.Brush.Color := ColorMap[TSeqEditOp(Panel.Index - 2)];
  stbStatus.Canvas.FillRect(Rect);
  y := (Rect.Height - stbStatus.Canvas.TextHeight('Ш'))div 2;
  stbStatus.Canvas.TextOut(Rect.Left + 4, Rect.Top + y, Panel.Text);
end;

function TfrmDiffView.MakeNodeList(aLines: TStrings): TNodeList;
var
  r: TNodeList;
  Opts: TStrCompareOptions = [];
  I, rCount: SizeInt;
  SkipEmptyLine: Boolean;
begin
  SkipEmptyLine := dfIgnoreEmptyLines in FFlags;
  if dfIgnoreCase in FFlags then Include(Opts, scoIgnoreCase);
  if dfIgnoreWSChange in FFlags then Include(Opts, scoIgnoreWSChange);
  if dfIgnoreWSChars in FFlags then Include(Opts, scoIgnoreWS);
  SetLength(r, aLines.Count);
  rCount := 0;
  for I := 0 to Pred(aLines.Count) do begin
    if (aLines[I] = '') and SkipEmptyLine then continue;
    r[rCount] := TTextNode.Make(Utf8HashText64(aLines[I], @TxxHash64LE.HashBuf, Opts), I);
    Inc(rCount);
  end;
  SetLength(r, rCount);
  Result := r;
end;

procedure TfrmDiffView.FillGrids;
var
  Source,
  Target: TNodeList;
  Patch: TSeqPatch;
  I, SrcIdx, TrgIdx, RowIdx: Integer;
begin
  Source := MakeNodeList(FSourceLines);
  Target := MakeNodeList(FTargetLines);
  Patch := TDiffUtil.MakePatch(Source, Target, FStat, FAlgo);
  SetLength(FEditList, Length(Source) + FStat[seoInsert]);
  sgSource.BeginUpdate;
  try
    sgTarget.BeginUpdate;
    try
      sgSource.RowCount := Length(FEditList);
      sgTarget.RowCount := Length(FEditList);
      SrcIdx := 0; TrgIdx := 0; RowIdx := 0;
      for I := 0 to High(Patch) do begin
        while SrcIdx < Patch[I].SourceIndex do begin
          sgSource.Cells[0, RowIdx] := ' ';
          sgSource.Cells[1, RowIdx] := Succ(Source[SrcIdx].LineNumber).ToString;
          sgSource.Cells[2, RowIdx] := FSourceLines[Source[SrcIdx].LineNumber];
          sgTarget.Cells[0, RowIdx] := ' ';
          sgTarget.Cells[1, RowIdx] := Succ(Target[TrgIdx].LineNumber).ToString;
          sgTarget.Cells[2, RowIdx] := FTargetLines[Target[TrgIdx].LineNumber];
          Inc(SrcIdx); Inc(TrgIdx); Inc(RowIdx);
        end;
        case Patch[I].Operation of
          seoDelete:
            begin
              FEditList[RowIdx] := seoDelete;
              sgSource.Cells[0, RowIdx] := '-';
              sgSource.Cells[1, RowIdx] := Succ(Patch[I].SourceValue.LineNumber).ToString;
              sgSource.Cells[2, RowIdx] := FSourceLines[Patch[I].SourceValue.LineNumber];
              Inc(SrcIdx); Inc(RowIdx);
            end;
          seoInsert:
            begin
              FEditList[RowIdx] := seoInsert;
              sgTarget.Cells[0, RowIdx] := '+';
              sgTarget.Cells[1, RowIdx] := Succ(Patch[I].TargetValue.LineNumber).ToString;
              sgTarget.Cells[2, RowIdx] := FTargetLines[Patch[I].TargetValue.LineNumber];
              Inc(TrgIdx); Inc(RowIdx);
            end;
        else // seoReplace
          FEditList[RowIdx] := seoReplace;
          sgSource.Cells[0, RowIdx] := '-';
          sgSource.Cells[1, RowIdx] := Succ(Patch[I].SourceValue.LineNumber).ToString;
          sgSource.Cells[2, RowIdx] := FSourceLines[Patch[I].SourceValue.LineNumber];
          sgTarget.Cells[0, RowIdx] := '+';
          sgTarget.Cells[1, RowIdx] := Succ(Patch[I].TargetValue.LineNumber).ToString;
          sgTarget.Cells[2, RowIdx] := FTargetLines[Patch[I].TargetValue.LineNumber];
          Inc(SrcIdx); Inc(TrgIdx); Inc(RowIdx);
        end;
      end;
      while SrcIdx < Length(Source) do begin
        sgSource.Cells[0, RowIdx] := ' ';
        sgSource.Cells[1, RowIdx] := Succ(Source[SrcIdx].LineNumber).ToString;
        sgSource.Cells[2, RowIdx] := FSourceLines[Source[SrcIdx].LineNumber];
        sgTarget.Cells[0, RowIdx] := ' ';
        sgTarget.Cells[1, RowIdx] := Succ(Target[TrgIdx].LineNumber).ToString;
        sgTarget.Cells[2, RowIdx] := FTargetLines[Target[TrgIdx].LineNumber];
        Inc(SrcIdx); Inc(TrgIdx); Inc(RowIdx);
      end;
      sgSource.ColWidths[0] := sgSource.Canvas.TextWidth('+') + 8;
      sgSource.ColWidths[1] := sgSource.Canvas.TextWidth(FSourceLines.Count.ToString) + 12;
      sgSource.ColWidths[2] := sgSource.ClientWidth - sgSource.ColWidths[0] - sgSource.ColWidths[1];
      sgTarget.ColWidths[0] := sgTarget.Canvas.TextWidth('+') + 8;
      sgTarget.ColWidths[1] := sgTarget.Canvas.TextWidth(FTargetLines.Count.ToString) + 12;
      sgTarget.ColWidths[2] := sgTarget.ClientWidth - sgTarget.ColWidths[0] - sgTarget.ColWidths[1];
    finally
      sgTarget.EndUpdate;
    end;
  finally
    sgSource.EndUpdate;
  end;
end;

procedure TfrmDiffView.UpdateNavigate;
var
  v: specialize TGLiteVector<Integer>;
  I, ChangeStart: Integer;
begin
  ChangeStart := -1;
  for I := 0 to High(FEditList) do
    if FEditList[I] = seoMatch  then begin
      if ChangeStart = -1 then continue;
      v.Add(ChangeStart);
      ChangeStart := -1;
    end else
      if ChangeStart = -1 then ChangeStart := I;
  if ChangeStart <> -1 then v.Add(ChangeStart);
  FChangeList := v.ToArray;
  FCurrChangeIndex := -1;
end;

procedure TfrmDiffView.BlockChanged;
begin
  if FCurrChangeIndex < 0 then exit;
  if FEditList[FChangeList[FCurrChangeIndex]] = seoInsert then
    stbStatus.Panels[0].Text := Format(CurrChangeFmt, ['target', sgTarget.Cells[1, sgTarget.Row]])
  else
    stbStatus.Panels[0].Text := Format(CurrChangeFmt, ['source', sgSource.Cells[1, sgSource.Row]]);
end;

procedure TfrmDiffView.ShowStat;
const
  Fmt  = '  %d %s';
  Fmt2 = '  Source %d lines, target %d lines';
begin
  stbStatus.Panels[0].Width :=
    stbStatus.Canvas.TextWidth(Format(CurrChangeFmt, ['source', sgSource.RowCount.ToString])) + 20;
  stbStatus.Panels[1].Text := Format(Fmt2, [FSourceLines.Count, FTargetLines.Count]);
  stbStatus.Panels[1].Width := stbStatus.Canvas.TextWidth(stbStatus.Panels[1].Text) + 20;
  stbStatus.Panels[2].Text := Format(Fmt, [FStat[seoMatch], 'matches']);
  stbStatus.Panels[2].Width := stbStatus.Canvas.TextWidth(stbStatus.Panels[2].Text) + 20;
  stbStatus.Panels[3].Text := Format(Fmt, [FStat[seoDelete], 'deletions']);
  stbStatus.Panels[3].Width := stbStatus.Canvas.TextWidth(stbStatus.Panels[3].Text) + 20;
  stbStatus.Panels[4].Text := Format(Fmt, [FStat[seoInsert], 'insertions']);
  stbStatus.Panels[4].Width := stbStatus.Canvas.TextWidth(stbStatus.Panels[4].Text) + 20;
  stbStatus.Panels[5].Text := Format(Fmt, [FStat[seoReplace], 'substitutions']);
  stbStatus.Panels[5].Width := stbStatus.Canvas.TextWidth(stbStatus.Panels[5].Text) + 20;
end;

procedure TfrmDiffView.ShowDiffs;
var
  Curs: TCursor;
begin
  Curs := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    FillGrids;
    UpdateNavigate;
  finally
    Screen.Cursor := Curs;
  end;
  if FChangeList <> nil then begin
    FCurrChangeIndex := 0;
    sgSource.Row := FChangeList[FCurrChangeIndex];
    BlockChanged;
  end;
  ShowStat;
end;

procedure TfrmDiffView.AppShowHint(Sender: TObject);
begin
  if Application.Hint <> '' then begin
    stbStatus.SimplePanel := True;
    stbStatus.SimpleText := Application.Hint;
  end else
    stbStatus.SimplePanel := False;
end;

procedure TfrmDiffView.SetViewData(aSrc, aTrg: TStrings; aFont: TFont; aFlags: TDiffFlags; Algo: TLcsAlgo);
begin
  FSourceLines := aSrc;
  FTargetLines := aTrg;
  if aFont <> nil then begin
    sgSource.Font := aFont;
    sgTarget.Font := aFont;
  end;
  FFlags := aFlags;
  FAlgo := Algo;
end;

end.

