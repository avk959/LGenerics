unit main;

{$MODE OBJFPC}{$H+}
{$WARN 6058 OFF : Call to subroutine "$1" marked as inline is not inlined}
{$WARN 5024 OFF : Parameter "$1" not used}

interface

uses
  Classes, SysUtils, Math, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Grids, Types,
  LclType, LgUtils, LgSeqUtils;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btDiffs: TButton;
    btIndelPatch: TButton;
    btPatch: TButton;
    edSource: TEdit;
    edTarget: TEdit;
    Label1: TLabel;
    lbInfo: TLabel;
    lbTarget: TLabel;
    lbSource: TLabel;
    pnDiff: TPanel;
    sgAlign: TStringGrid;
    sgStat: TStringGrid;
    splitDiff: TSplitter;
    procedure btDiffsClick(Sender: TObject);
    procedure btIndelPatchClick(Sender: TObject);
    procedure btPatchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgAlignDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure sgStatPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
  private
  type
    TSeqUtil = specialize TGSeqUtil<Ucs4Char, TUcs4Hasher>;
    TEditOp  = TSeqUtil.TSeqEditOp;
    TSeqStat = TSeqUtil.TSeqPatchStat;
  const
    ColorMap: array[TEditOp] of TColor = ($cffcfa, $aaaaff, $fdacb3, $aceba9);
  var
    FEditList: array of TEditOp;
    procedure DoUseDiffs;
    procedure DoUseIndelPatch;
    procedure DoUsePatch;
    procedure AdjustAlignCols;
    procedure UpdateStat(const aStat: TSeqStat; aIndel: Boolean);
    procedure CheckGridsVisible;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btDiffsClick(Sender: TObject);
begin
  DoUseDiffs;
  CheckGridsVisible;
end;

procedure TfrmMain.btIndelPatchClick(Sender: TObject);
begin
  DoUseIndelPatch;
  CheckGridsVisible;
end;

procedure TfrmMain.btPatchClick(Sender: TObject);
begin
  DoUsePatch;
  CheckGridsVisible;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Application.Title := 'String diff demo';
  Caption := 'String difference test';
  edSource.Text := 'это сравнение строк основано на нахождении LCS';
  edTarget.Text := 'что cmp равно устрой оно с новой не дождётся VCS';
  sgAlign.DefaultRowHeight := sgAlign.Canvas.TextHeight('Щ')+2;
  sgAlign.RowCount := 2;
  sgStat.Cells[0,0] := 'Matches';
  sgStat.Cells[0,1] := 'Deletions';
  sgStat.Cells[0,2] := 'Insertions';
end;

procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift <> [] then exit;
  if Key = VK_ESCAPE then Close;
end;

procedure TfrmMain.sgAlignDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  if FEditList = nil then exit;
  with sgAlign do begin
    Canvas.Brush.Color := ColorMap[FEditList[aCol]];
    Canvas.FillRect(aRect);
    Canvas.TextOut(aRect.Left+1, aRect.Top, Cells[aCol, aRow]);
  end;
end;

procedure TfrmMain.sgStatPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
begin
  if FEditList = nil then exit;
  sgStat.Canvas.Brush.Color := ColorMap[TEditOp(aRow)];
end;

procedure TfrmMain.DoUseDiffs;
var
  SrcSeq, TrgSeq: TUcs4Seq;
  Diff: TSeqUtil.TDiffV;
  EditList: array of TEditOp = nil;
  Stat: TSeqStat;
  SrcLen, TrgLen, SrcIdx, TrgIdx, ColIdx: Integer;
begin
  SrcSeq := Utf8ToUcs4Seq(edSource.Text);
  TrgSeq := Utf8ToUcs4Seq(edTarget.Text);
  TSeqUtil.Diff(SrcSeq, TrgSeq, Diff);
  Stat[seoDelete] := Diff.SourceChanges.PopCount;
  Stat[seoInsert] := Diff.TargetChanges.PopCount;
  Stat[seoMatch] := Length(SrcSeq) - Stat[seoDelete];
  Stat[seoReplace] := 0;
  SetLength(EditList, Length(SrcSeq) + Stat[seoInsert]);
  sgAlign.BeginUpdate;
  try
    SrcLen := Length(SrcSeq);
    TrgLen := Length(TrgSeq);
    SrcIdx := 0; TrgIdx := 0; ColIdx := 0;
    sgAlign.ColCount := 0;
    sgAlign.ColCount := Length(EditList);
    repeat
      while (SrcIdx < SrcLen) and not Diff.SourceChanges[SrcIdx] and
            (TrgIdx < TrgLen) and not Diff.TargetChanges[TrgIdx] do begin //matches
        sgAlign.Cells[ColIdx, 0] := SrcSeq[SrcIdx].ToUtf8;
        sgAlign.Cells[ColIdx, 1] := sgAlign.Cells[ColIdx, 0];
        EditList[ColIdx] := seoMatch;
        Inc(ColIdx); Inc(SrcIdx); Inc(TrgIdx);
      end;
      while (SrcIdx < SrcLen) and Diff.SourceChanges[SrcIdx] do begin //deletions
        sgAlign.Cells[ColIdx, 0] := SrcSeq[SrcIdx].ToUtf8;
        sgAlign.Cells[ColIdx, 1] := ' ';
        EditList[ColIdx] := seoDelete;
        Inc(ColIdx); Inc(SrcIdx);
      end;
      while (TrgIdx < TrgLen) and Diff.TargetChanges[TrgIdx] do begin //insertions
        sgAlign.Cells[ColIdx, 0] := ' ';
        sgAlign.Cells[ColIdx, 1] := TrgSeq[TrgIdx].ToUtf8;
        EditList[ColIdx] := seoInsert;
        Inc(ColIdx); Inc(TrgIdx);
      end;
    until (SrcIdx >= SrcLen) and (TrgIdx >= TrgLen);
    FEditList := EditList;
    AdjustAlignCols;
  finally
    sgAlign.EndUpdate;
  end;
  UpdateStat(Stat, True);
end;

procedure TfrmMain.DoUseIndelPatch;
var
  SrcSeq: TUcs4Seq;
  Patch: TSeqUtil.TSeqLcsPatch;
  EditList: array of TEditOp = nil;
  Stat: TSeqStat;
  TrgLen, I, SrcIdx, ColIdx: Integer;
begin
  SrcSeq := Utf8ToUcs4Seq(edSource.Text);
  Patch := TSeqUtil.MakeLcsPatch(SrcSeq, Utf8ToUcs4Seq(edTarget.Text));
  TrgLen := Utf8StrLen(edTarget.Text);
  Stat[seoMatch] := (TrgLen + Length(SrcSeq) - Length(Patch)) div 2;
  Stat[seoInsert] := TrgLen - Stat[seoMatch];
  Stat[seoDelete] := Length(SrcSeq) - Stat[seoMatch];
  Stat[seoReplace] := 0;
  SetLength(EditList, Length(SrcSeq) + Stat[seoInsert]);
  sgAlign.BeginUpdate;
  try
    sgAlign.ColCount := 0;
    sgAlign.ColCount := Length(EditList);
    ColIdx := 0; SrcIdx := 0;
    for I := 0 to High(Patch) do begin
      while SrcIdx < Patch[I].SourceIndex do begin
        sgAlign.Cells[ColIdx, 0] := SrcSeq[SrcIdx].ToUtf8;
        sgAlign.Cells[ColIdx, 1] := sgAlign.Cells[ColIdx, 0];
        Inc(ColIdx); Inc(SrcIdx);
      end;
      EditList[ColIdx] := Patch[I].Operation;
      if Patch[I].Operation = seoDelete then begin
        sgAlign.Cells[ColIdx, 0] := SrcSeq[SrcIdx].ToUtf8;
        sgAlign.Cells[ColIdx, 1] := ' ';
        Inc(SrcIdx);
      end else begin //seoInsert
        sgAlign.Cells[ColIdx, 0] := ' ';
        sgAlign.Cells[ColIdx, 1] := Patch[I].Value.ToUtf8;
      end;
      Inc(ColIdx);
    end;
    for I := SrcIdx to High(SrcSeq) do begin
      sgAlign.Cells[ColIdx, 0] := SrcSeq[I].ToUtf8;
      sgAlign.Cells[ColIdx, 1] := sgAlign.Cells[ColIdx, 0];
      Inc(ColIdx);
    end;
    FEditList := EditList;
    AdjustAlignCols;
  finally
    sgAlign.EndUpdate;
  end;
  UpdateStat(Stat, True);
end;

procedure TfrmMain.DoUsePatch;
var
  SrcSeq: TUcs4Seq;
  Patch: TSeqUtil.TSeqPatch;
  EditList: array of TEditOp = nil;
  Stat: TSeqStat;
  I, SrcIdx, ColIdx: Integer;
begin
  SrcSeq := Utf8ToUcs4Seq(edSource.Text);
  Patch := TSeqUtil.MakePatch(SrcSeq, Utf8ToUcs4Seq(edTarget.Text), Stat);
  SetLength(EditList, Length(SrcSeq) + Stat[seoInsert]);
  sgAlign.BeginUpdate;
  try
    sgAlign.ColCount := 0;
    sgAlign.ColCount := Length(EditList);
    ColIdx := 0; SrcIdx := 0;
    for I := 0 to High(Patch) do begin
      while SrcIdx < Patch[I].SourceIndex do begin
        sgAlign.Cells[ColIdx, 0] := SrcSeq[SrcIdx].ToUtf8;
        sgAlign.Cells[ColIdx, 1] := sgAlign.Cells[ColIdx, 0];
        Inc(ColIdx); Inc(SrcIdx);
      end;
      EditList[ColIdx] := Patch[I].Operation;
      case Patch[I].Operation of
        seoDelete:
          begin
            sgAlign.Cells[ColIdx, 0] := SrcSeq[SrcIdx].ToUtf8;
            sgAlign.Cells[ColIdx, 1] := ' ';
            Inc(SrcIdx);
          end;
        seoInsert:
          begin
            sgAlign.Cells[ColIdx, 0] := ' ';
            sgAlign.Cells[ColIdx, 1] := Patch[I].TargetValue.ToUtf8;
          end;
      else // seoReplace
        sgAlign.Cells[ColIdx, 0] := Patch[I].SourceValue.ToUtf8;
        sgAlign.Cells[ColIdx, 1] := Patch[I].TargetValue.ToUtf8;
        Inc(SrcIdx);
      end;
      Inc(ColIdx);
    end;
    for I := SrcIdx to High(SrcSeq) do begin
      sgAlign.Cells[ColIdx, 0] := SrcSeq[I].ToUtf8;
      sgAlign.Cells[ColIdx, 1] := sgAlign.Cells[ColIdx, 0];
      Inc(ColIdx);
    end;
    FEditList := EditList;
    AdjustAlignCols;
  finally
    sgAlign.EndUpdate;
  end;
  UpdateStat(Stat, False);
end;

procedure TfrmMain.AdjustAlignCols;
var
  I: Integer;
begin
  for I := 0 to sgAlign.ColCount - 1 do
    sgAlign.ColWidths[I] := Max(
      sgAlign.Canvas.TextWidth(sgAlign.Cells[I,0]),
      sgAlign.Canvas.TextWidth(sgAlign.Cells[I,1])
    )+4;
end;

procedure TfrmMain.UpdateStat(const aStat: TSeqStat; aIndel: Boolean);
begin
  sgStat.BeginUpdate;
  try
    sgStat.RowCount := 4 - Ord(aIndel);
    sgStat.Cells[1,0] := aStat[seoMatch].ToString;
    sgStat.Cells[1,1] := aStat[seoDelete].ToString;
    sgStat.Cells[1,2] := aStat[seoInsert].ToString;
    if not aIndel then begin
      sgStat.Cells[0,3] := 'Substitutions';
      sgStat.Cells[1,3] := aStat[seoReplace].ToString;
    end;
  finally
    sgStat.EndUpdate;
  end;
end;

procedure TfrmMain.CheckGridsVisible;
begin
  if not sgAlign.Visible then begin
    sgAlign.Visible := True;
    sgStat.Visible := True;
  end;
end;

end.

