unit main;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList, ExtCtrls, ComCtrls, SynEdit,
  Grids, DiffUtils;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    aclstMain: TActionList;
    acOpenSrcFile: TAction;
    acOpenTrgFile: TAction;
    acExit: TAction;
    acShowOpts: TAction;
    acShowDiff: TAction;
    acSrcCut: TAction;
    acSrcCopy: TAction;
    acSrcPaste: TAction;
    acSrcSelectAll: TAction;
    acSrcUndo: TAction;
    acSrcRedo: TAction;
    acSynEditFont: TAction;
    acDiffFont: TAction;
    acMoveFocus: TAction;
    acTrgRedo: TAction;
    acTrgUndo: TAction;
    acTrgSelectAll: TAction;
    acTrgPaste: TAction;
    acTrgCopy: TAction;
    acTrgCut: TAction;
    fdFont: TFontDialog;
    imlMain: TImageList;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem7: TMenuItem;
    Separator3: TMenuItem;
    odOpenFile: TOpenDialog;
    Separator2: TMenuItem;
    MenuItem6: TMenuItem;
    mmuMain: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Separator1: TMenuItem;
    mmiFile: TMenuItem;
    mmiView: TMenuItem;
    pnTarget: TPanel;
    pnSource: TPanel;
    pnMain: TPanel;
    Splitter1: TSplitter;
    sedSource: TSynEdit;
    sedTarget: TSynEdit;
    stbStatus: TStatusBar;
    tlbSource: TToolBar;
    tlbTarget: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
  //---------------------------
    procedure acDiffFontExecute(Sender: TObject);
    procedure acMoveFocusExecute(Sender: TObject);
    procedure acShowDiffExecute(Sender: TObject);
    procedure acShowDiffUpdate(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acOpenSrcFileExecute(Sender: TObject);
    procedure acOpenTrgFileExecute(Sender: TObject);
    procedure acShowOptsExecute(Sender: TObject);
    procedure acSrcCopyExecute(Sender: TObject);
    procedure acSrcCopyUpdate(Sender: TObject);
    procedure acSrcCutExecute(Sender: TObject);
    procedure acSrcCutUpdate(Sender: TObject);
    procedure acSrcPasteExecute(Sender: TObject);
    procedure acSrcPasteUpdate(Sender: TObject);
    procedure acSrcRedoExecute(Sender: TObject);
    procedure acSrcRedoUpdate(Sender: TObject);
    procedure acSrcSelectAllExecute(Sender: TObject);
    procedure acSrcSelectAllUpdate(Sender: TObject);
    procedure acSrcUndoExecute(Sender: TObject);
    procedure acSrcUndoUpdate(Sender: TObject);
    procedure acSynEditFontExecute(Sender: TObject);
    procedure acTrgCopyExecute(Sender: TObject);
    procedure acTrgCopyUpdate(Sender: TObject);
    procedure acTrgCutExecute(Sender: TObject);
    procedure acTrgCutUpdate(Sender: TObject);
    procedure acTrgPasteExecute(Sender: TObject);
    procedure acTrgPasteUpdate(Sender: TObject);
    procedure acTrgRedoExecute(Sender: TObject);
    procedure acTrgRedoUpdate(Sender: TObject);
    procedure acTrgSelectAllExecute(Sender: TObject);
    procedure acTrgSelectAllUpdate(Sender: TObject);
    procedure acTrgUndoExecute(Sender: TObject);
    procedure acTrgUndoUpdate(Sender: TObject);
  //--------------------------------
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sedSourceChange(Sender: TObject);
    procedure sedTargetChange(Sender: TObject);
  private
    FDiffFont: TFont;
    FDiffFlags: TDiffFlags;
    FAlgo: TLcsAlgo;
    procedure ShowStatus;
    procedure AppShowHint(Sender: TObject);
  public

  end;

var
  frmMain: TfrmMain;

implementation
{$B-}{$COPERATORS ON}
uses
  DiffOptDlg, DiffView;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Application.Title := 'Text diff demo';
  Application.OnHint := @AppShowHint;
  Caption := 'Text difference test';
  FAlgo := TLcsAlgo.laMyers;
  ShowStatus;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FDiffFont.Free;
end;

procedure TfrmMain.sedSourceChange(Sender: TObject);
begin
  ShowStatus;
end;

procedure TfrmMain.sedTargetChange(Sender: TObject);
begin
  ShowStatus;
end;

procedure TfrmMain.ShowStatus;
const
  Fmt = '  %s text: %d lines';
begin
  stbStatus.Panels[0].Text := Format(Fmt, ['Source', sedSource.Lines.Count]);
  stbStatus.Panels[1].Text := Format(Fmt, ['Target', sedTarget.Lines.Count]);
end;

procedure TfrmMain.AppShowHint(Sender: TObject);
begin
  if Application.Hint <> '' then begin
    stbStatus.SimplePanel := True;
    stbStatus.SimpleText := Application.Hint;
  end else
    stbStatus.SimplePanel := False;
end;

procedure TfrmMain.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.acShowDiffExecute(Sender: TObject);
begin
  with TfrmDiffView.Create(nil) do
    try
      SetViewData(sedSource.Lines, sedTarget.Lines, FDiffFont, FDiffFlags, FAlgo);
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfrmMain.acDiffFontExecute(Sender: TObject);
begin
  if FDiffFont = nil then begin
    FDiffFont := TFont.Create;
    with TStringGrid.Create(nil) do
      try
        FDiffFont.Assign(Font);
      finally
        Free;
      end;
  end;
  fdFont.Font := FDiffFont;
  if fdFont.Execute then
    FDiffFont.Assign(fdFont.Font);
end;

procedure TfrmMain.acMoveFocusExecute(Sender: TObject);
begin
  if sedSource.Focused then
    sedTarget.SetFocus
  else
    sedSource.SetFocus;
end;

procedure TfrmMain.acShowDiffUpdate(Sender: TObject);
begin
  acShowDiff.Enabled := sedSource.HasText and sedTarget.HasText;
end;

procedure TfrmMain.acOpenSrcFileExecute(Sender: TObject);
begin
  odOpenFile.Title := 'Open source text file';
  if not odOpenFile.Execute then exit;
  sedSource.Lines.LoadFromFile(odOpenFile.FileName);
  ShowStatus;
end;

procedure TfrmMain.acOpenTrgFileExecute(Sender: TObject);
begin
  odOpenFile.Title := 'Open target text file';
  if not odOpenFile.Execute then exit;
  sedTarget.Lines.LoadFromFile(odOpenFile.FileName);
  ShowStatus;
end;

procedure TfrmMain.acShowOptsExecute(Sender: TObject);
begin
  with TfrmDiffOptDlg.Create(nil) do
    try
      DiffFlags := FDiffFlags;
      DiffAlgo := FAlgo;
      if ShowModal = mrOk then begin
        FDiffFlags := DiffFlags;
        FAlgo := DiffAlgo;
      end;
    finally
      Free;
    end;
end;

procedure TfrmMain.acSrcCopyExecute(Sender: TObject);
begin
  sedSource.CopyToClipboard;
end;

procedure TfrmMain.acSrcCopyUpdate(Sender: TObject);
begin
  acSrcCopy.Enabled := sedSource.SelText <> '';
end;

procedure TfrmMain.acSrcCutExecute(Sender: TObject);
begin
  sedSource.CutToClipboard;
end;

procedure TfrmMain.acSrcCutUpdate(Sender: TObject);
begin
  acSrcCut.Enabled := sedSource.SelText <> '';
end;

procedure TfrmMain.acSrcPasteExecute(Sender: TObject);
begin
  sedSource.PasteFromClipboard;
end;

procedure TfrmMain.acSrcPasteUpdate(Sender: TObject);
begin
  acSrcPaste.Enabled := sedSource.CanPaste;
end;

procedure TfrmMain.acSrcRedoExecute(Sender: TObject);
begin
  sedSource.Redo;
end;

procedure TfrmMain.acSrcRedoUpdate(Sender: TObject);
begin
  acSrcRedo.Enabled := sedSource.CanRedo;
end;

procedure TfrmMain.acSrcSelectAllExecute(Sender: TObject);
begin
  sedSource.SelectAll;
end;

procedure TfrmMain.acSrcSelectAllUpdate(Sender: TObject);
begin
  acSrcSelectAll.Enabled := sedSource.HasText;
end;

procedure TfrmMain.acSrcUndoExecute(Sender: TObject);
begin
  sedSource.Undo;
end;

procedure TfrmMain.acSrcUndoUpdate(Sender: TObject);
begin
  acSrcUndo.Enabled := sedSource.CanUndo;
end;

procedure TfrmMain.acSynEditFontExecute(Sender: TObject);
begin
  fdFont.Font := sedSource.Font;
  if not fdFont.Execute then exit;
  sedSource.Font := fdFont.Font;
  sedTarget.Font := fdFont.Font;
end;

procedure TfrmMain.acTrgCopyExecute(Sender: TObject);
begin
  sedTarget.CopyToClipboard;
end;

procedure TfrmMain.acTrgCopyUpdate(Sender: TObject);
begin
  acTrgCopy.Enabled := sedTarget.SelText <> '';
end;

procedure TfrmMain.acTrgCutExecute(Sender: TObject);
begin
  sedTarget.CutToClipboard;
end;

procedure TfrmMain.acTrgCutUpdate(Sender: TObject);
begin
  acTrgCut.Enabled := sedTarget.SelText <> '';
end;

procedure TfrmMain.acTrgPasteExecute(Sender: TObject);
begin
  sedTarget.PasteFromClipboard;
end;

procedure TfrmMain.acTrgPasteUpdate(Sender: TObject);
begin
  acTrgPaste.Enabled := sedTarget.CanPaste;
end;

procedure TfrmMain.acTrgRedoExecute(Sender: TObject);
begin
  sedTarget.Redo;
end;

procedure TfrmMain.acTrgRedoUpdate(Sender: TObject);
begin
  acTrgRedo.Enabled := sedTarget.CanRedo;
end;

procedure TfrmMain.acTrgSelectAllExecute(Sender: TObject);
begin
  sedTarget.SelectAll;
end;

procedure TfrmMain.acTrgSelectAllUpdate(Sender: TObject);
begin
  acTrgSelectAll.Enabled := sedTarget.HasText;
end;

procedure TfrmMain.acTrgUndoExecute(Sender: TObject);
begin
  sedTarget.Undo;
end;

procedure TfrmMain.acTrgUndoUpdate(Sender: TObject);
begin
  acTrgUndo.Enabled := sedTarget.CanUndo;
end;

end.

