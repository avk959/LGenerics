unit sedmain;

{$MODE OBJFPC}{$H+}{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList, ComCtrls, ExtCtrls, StdCtrls, SynEdit,
  lgUtils, lgJson, lgJsonCfg, lgMiscUtils, lgHashMap, sedAppConf, lgJsonTypeDef, SynEditTypes,
  SynPluginSyncroEdit, sedJsonValidate, SynEditMiscClasses, SynCompletion, LCLType, Buttons, SynEditKeyCmds;

type

  TFormKind = TJtdSchema.TFormKind;

  { TfrmMain }
  TfrmMain = class(TForm)
    aclMainActions: TActionList;
    acSchemaNew: TAction;
    acOpen: TAction;
    acSave: TAction;
    acSaveAs: TAction;
    acAppExit: TAction;
    acSettings: TAction;
    acRedo: TAction;
    acCopy: TAction;
    acCut: TAction;
    acPaste: TAction;
    acNewFromSample: TAction;
    acGenUnit: TAction;
    acSyntaxCheck: TAction;
    acSelectAll: TAction;
    acFormat: TAction;
    acHelpOnline: TAction;
    acAbout: TAction;
    acFind: TAction;
    acReplace: TAction;
    acBeginObject: TAction;
    acBeginArray: TAction;
    acQuotes: TAction;
    acFindNext: TAction;
    acOpenInstance: TAction;
    acStartValidate: TAction;
    acSchemaTreeCollapse: TAction;
    acSchemaTreeExpand: TAction;
    acInstanceTreeExpand: TAction;
    acInstanceTreeCollapse: TAction;
    acUndo: TAction;
    AppProps: TApplicationProperties;
    cbMainTools: TCoolBar;
    edSchemaName: TEdit;
    ilMainImages: TImageList;
    ilSampleTrees: TImageList;
    ilSchemaTree: TImageList;
    lbCaretX: TLabel;
    lbCaretY: TLabel;
    lbState: TLabel;
    lbSyntaxState: TLabel;
    lbxErrorList: TListBox;
    MenuItem1: TMenuItem;
    ppmiCollapseInstTree: TMenuItem;
    ppmiExpandInstTree: TMenuItem;
    Separator13: TMenuItem;
    ppmiStartValidate: TMenuItem;
    Separator12: TMenuItem;
    ppmiOpenInst: TMenuItem;
    pnValidate: TPanel;
    pnStructRight: TPanel;
    ppmInstanceTree: TPopupMenu;
    ppmiSchemaTreeCollapse: TMenuItem;
    ppmiSchemaTreeExpand: TMenuItem;
    mmuiRecent: TMenuItem;
    ppmSchemaTree: TPopupMenu;
    ppmiFindNext: TMenuItem;
    mmuiFindNext: TMenuItem;
    mmuiReplace: TMenuItem;
    ppmiFind: TMenuItem;
    Separator11: TMenuItem;
    mmuiFilnd: TMenuItem;
    Separator10: TMenuItem;
    Separator9: TMenuItem;
    mmuiAbout: TMenuItem;
    mmuiHelpOnline: TMenuItem;
    ppmRecent: TPopupMenu;
    ppmiSyntaxCheck: TMenuItem;
    Separator8: TMenuItem;
    ppmiSelectAll: TMenuItem;
    ppmiFormat: TMenuItem;
    Separator6: TMenuItem;
    ppmiPaste: TMenuItem;
    ppmiCopi: TMenuItem;
    ppmiCut: TMenuItem;
    Separator1: TMenuItem;
    ppmiRedo: TMenuItem;
    ppmiUndo: TMenuItem;
    mmuiFormat: TMenuItem;
    mmuiSelectAll: TMenuItem;
    pcMainPages: TPageControl;
    pnCaretX: TPanel;
    pnCaretY: TPanel;
    pnSourceBottom: TPanel;
    pnState: TPanel;
    pnSyntaxState: TPanel;
    ppmEdit: TPopupMenu;
    seJsonEdit: TSynEdit;
    Separator7: TMenuItem;
    mmuiJsonSyntax: TMenuItem;
    mmuiSettings: TMenuItem;
    Separator5: TMenuItem;
    mmuiNewFromSample: TMenuItem;
    mmuiCreateUnit: TMenuItem;
    mmuiCopy: TMenuItem;
    MenuItem2: TMenuItem;
    mmuiUndo: TMenuItem;
    mmuiRedo: TMenuItem;
    Separator4: TMenuItem;
    mmuiCut: TMenuItem;
    mmuiAppExit: TMenuItem;
    Separator3: TMenuItem;
    mmuiSaveAs: TMenuItem;
    Separator2: TMenuItem;
    mmuiSave: TMenuItem;
    mmuiNew: TMenuItem;
    OD: TOpenDialog;
    SD: TSaveDialog;
    mmuiOpen: TMenuItem;
    mmuService: TMenuItem;
    mmuHelp: TMenuItem;
    mmuEdit: TMenuItem;
    mmuMain: TMainMenu;
    mmuFile: TMenuItem;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    stxValidateStatus: TStaticText;
    stbMainStatus: TStatusBar;
    SynCompletion: TSynCompletion;
    SynPluginSyncroEdit: TSynPluginSyncroEdit;
    tbInstance: TToolBar;
    tlbFile: TToolBar;
    tlbTools: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    tbtOpen: TToolButton;
    ToolButton12: TToolButton;
    tbtOpenInstance: TToolButton;
    ToolButton13: TToolButton;
    tbtStartValidate: TToolButton;
    ToolButton14: TToolButton;
    tbtInstTreeExpand: TToolButton;
    tbtInstTreeCollapse: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    tvInstanceTree: TTreeView;
    tsSource: TTabSheet;
    tsStruct: TTabSheet;
    tvSchemaTree: TTreeView;
    procedure acAboutExecute(Sender: TObject);
    procedure acAppExitExecute(Sender: TObject);
    procedure acBeginArrayExecute(Sender: TObject);
    procedure acBeginObjectExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acCopyUpdate(Sender: TObject);
    procedure acCutExecute(Sender: TObject);
    procedure acCutUpdate(Sender: TObject);
    procedure acFindExecute(Sender: TObject);
    procedure acFindNextExecute(Sender: TObject);
    procedure acFindNextUpdate(Sender: TObject);
    procedure acFindUpdate(Sender: TObject);
    procedure acFormatExecute(Sender: TObject);
    procedure acFormatUpdate(Sender: TObject);
    procedure acGenUnitExecute(Sender: TObject);
    procedure acGenUnitUpdate(Sender: TObject);
    procedure acHelpOnlineExecute(Sender: TObject);
    procedure acInstanceTreeCollapseExecute(Sender: TObject);
    procedure acInstanceTreeCollapseUpdate(Sender: TObject);
    procedure acInstanceTreeExpandExecute(Sender: TObject);
    procedure acInstanceTreeExpandUpdate(Sender: TObject);
    procedure acNewFromSampleExecute(Sender: TObject);
    procedure acOpenInstanceExecute(Sender: TObject);
    procedure acOpenInstanceUpdate(Sender: TObject);
    procedure acQuotesExecute(Sender: TObject);
    procedure acReplaceExecute(Sender: TObject);
    procedure acReplaceUpdate(Sender: TObject);
    procedure acStartValidateExecute(Sender: TObject);
    procedure acStartValidateUpdate(Sender: TObject);
    procedure acSyntaxCheckExecute(Sender: TObject);
    procedure acSyntaxCheckUpdate(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure acPasteUpdate(Sender: TObject);
    procedure acRedoExecute(Sender: TObject);
    procedure acRedoUpdate(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure acSaveAsUpdate(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSaveUpdate(Sender: TObject);
    procedure acSchemaNewExecute(Sender: TObject);
    procedure acSelectAllExecute(Sender: TObject);
    procedure acSelectAllUpdate(Sender: TObject);
    procedure acSettingsExecute(Sender: TObject);
    procedure acSchemaTreeCollapseExecute(Sender: TObject);
    procedure acSchemaTreeCollapseUpdate(Sender: TObject);
    procedure acSchemaTreeExpandExecute(Sender: TObject);
    procedure acSchemaTreeExpandUpdate(Sender: TObject);
    procedure acUndoExecute(Sender: TObject);
    procedure acUndoUpdate(Sender: TObject);
    procedure AppPropsHint(Sender: TObject);
    procedure AppPropsQueryEndSession(var Cancel: Boolean);
    procedure edSchemaNameChange(Sender: TObject);
    procedure edSchemaNameEditingDone(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbxErrorListClick(Sender: TObject);
    procedure lbxErrorListDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure mmuFileClick(Sender: TObject);
    procedure ppmRecentPopup(Sender: TObject);
    procedure seJsonEditChange(Sender: TObject);
    procedure seJsonEditReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: integer;
      var ReplaceAction: TSynReplaceAction);
    procedure seJsonEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure stbMainStatusResize(Sender: TObject);
    procedure tbtOpenArrowClick(Sender: TObject);
    procedure tsSourceShow(Sender: TObject);
    procedure tsStructShow(Sender: TObject);
  private
  type
    TSourceState       = (ssUnknown, ssInvalidJson, ssInvalidSchema, ssValidSchema);
    TJNode2TreeMapType = specialize TGLiteChainHashMap<TJsonNode, TTreeNode, TObject>;
    TJNode2TreeMap     = TJNode2TreeMapType.TMap;
  const
    ValidateStateNone = '<none>';
    FormKinds: array[TFormKind] of string = (
    'Unknown', 'Empty', 'Ref', 'Type', 'Enum', 'Elements', 'Properties', 'Values', 'Discriminator');
  var
    FCurrJson,
    FCurrInstance: TJsonNode;
    FRecentFiles: TStringList;
    FSchema2TreeMap,
    FInstance2TreeMap: TJNode2TreeMap;
    FValidateErrList: TJtdErrorList;
    FSuccessColor,
    FCurrLineColor,
    FErrorLineColor: TColor;
    FSearcing,
    FShowing: Boolean;
    FRecentFilesModified: Boolean;
    FSourceState: TSourceState;
    FSchemaFormKind: TFormKind;
    function  GetCurrFile: string;
    function  GetSchemaName: string;
    function  GetSchemaNameChanged: Boolean;
    procedure SetCurrFile(const aValue: string);
    procedure ReadConfig;
    procedure ReadPosition(aCfg: TAppConf);
    procedure ReadSettings(aCfg: TAppConf);
    procedure ReadDisplayStyle(aCfg: TAppConf);
    procedure ReadRecentFiles(aCfg: TAppConf);
    procedure AddRecentFile(const aFileName: string);
    procedure WritePosition;
    procedure WriteRecentFiles;
    function  HandleFileSave(const aFileName: string; out aSaved: string): Boolean;
    procedure LoadSchemaTreeView;
    procedure SchemaChanged;
    procedure SourceStatusChanged(const aResult: TValidateResult);
    procedure LoadNewSource;
    procedure TryLoadSchemaFile(const aFileName: string);
    procedure RecentFileClick(Sender: TObject);
    function  CheckModified: Boolean;
    procedure ShowSearchDialog;
    procedure SearchNext;
    procedure ShowReplaceDialog;
    procedure ExecBeginStruct(aObject: Boolean = True);
    procedure NewInstanceFile;
    procedure LoadInstanceTreeView(const aRootName: string);
  public
    property  CurrentFile: string read GetCurrFile write SetCurrFile;
    property  SchemaName: string read GetSchemaName;
    property  SchemaNameChanged: Boolean read GetSchemaNameChanged;
  end;

var
  frmMain: TfrmMain;

implementation
{$B-}{$J-}{$COPERATORS ON}{$POINTERMATH ON}
uses
  SynEditHighlighter, SynGutterLineNumber, FileUtil, LazFileUtils, LclIntf, SynHighlighterJTD,
  sedSettingsDlg, sedSearchDlg, sedReplaceDlg, sedAbout, sedStrConsts, lgJtdInfer, sedInferDlg,
  sedCodegenDlg;

{$R *.lfm}

function SpaceLine(aLen: Integer): string; inline;
begin
  Result := '';
  if aLen > 0 then
    begin
      SetLength(Result, aLen);
      FillChar(Pointer(Result)^, aLen, ' ');
    end;
end;

function GetIndent(const s: string): Integer;
var
  Len: Integer;
  I: Integer;
begin
  Len := Length(s);
  I := 1;
  while (I <= Len) and (s[I] = ' ') do
    Inc(I);
  Result := I - 1;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := 'JtdEd'; ////
  Application.Title := Caption;
  FRecentFiles := TStringList.Create;
  stxValidateStatus.Caption := ValidateStateNone;
  seJsonEdit.Highlighter := TSynJTDSyn.Create(Self);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  WritePosition;
  WriteRecentFiles;
  FCurrJson.Free;
  FCurrInstance.Free;
  FRecentFiles.Free;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  WritePosition;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if not FShowing then
    begin
      FShowing := True;
      ReadConfig;
    end;
end;

procedure TfrmMain.lbxErrorListClick(Sender: TObject);
var
  Node: TJsonNode = nil;
  TreeNode: TTreeNode;
  Idx: Integer;
begin
  Idx := lbxErrorList.ItemIndex;
  if Idx < 0 then exit;
  if FCurrJson.FindPathPtr(FValidateErrList[Idx].SchemaPath, Node) and
     FSchema2TreeMap.TryGetValue(Node, TreeNode) then
    begin
      tvSchemaTree.Selected := TreeNode;
      tvSchemaTree.MakeSelectionVisible;
    end;
  if FCurrInstance.FindPathPtr(FValidateErrList[Idx].InstancePath, Node) and
     FInstance2TreeMap.TryGetValue(Node, TreeNode) then
    begin
      tvInstanceTree.Selected := TreeNode;
      tvInstanceTree.MakeSelectionVisible;
    end;
end;

procedure TfrmMain.lbxErrorListDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  LColor: TColor;
begin
  if odSelected in State then
    LColor := FErrorLineColor
  else
    LColor := clDefault;
  lbxErrorList.Canvas.Brush.Color := LColor;
  lbxErrorList.Canvas.FillRect(ARect);
  lbxErrorList.Canvas.Font.Color := clDefault;
  lbxErrorList.Canvas.TextRect(ARect, 2, ARect.Top + 2, lbxErrorList.Items[Index]);
end;

procedure TfrmMain.mmuFileClick(Sender: TObject);
var
  I: Integer;
  Item: TMenuItem;
begin
  mmuiRecent.Enabled := FRecentFiles.Count > 0;
  mmuiRecent.Clear;
  if FRecentFiles.Count = 0 then exit;
  for I := 0 to Pred(FRecentFiles.Count) do
    begin
      Item := TMenuItem.Create(mmuiRecent);
      Item.Caption := FRecentFiles[I];
      Item.OnClick := @RecentFileClick;
      mmuIRecent.Add(Item);
    end;
end;

procedure TfrmMain.ppmRecentPopup(Sender: TObject);
var
  I: Integer;
  Item: TMenuItem;
begin
  ppmRecent.Items.Clear;
  if FRecentFiles.Count < 1 then exit;
  for I := 0 to Pred(FRecentFiles.Count) do
    begin
      Item := TMenuItem.Create(ppmRecent);
      Item.Caption := FRecentFiles[I];
      Item.OnClick := @RecentFileClick;
      ppmRecent.Items.Add(Item);
    end;
end;

procedure TfrmMain.seJsonEditChange(Sender: TObject);
begin
  FSourceState := ssUnknown;
  FSchemaFormKind := fkNone;
  SourceStatusChanged(TValidateResult.Valid);
end;

procedure TfrmMain.seJsonEditReplaceText(Sender: TObject; const ASearch, AReplace: string; Line,
  Column: integer; var ReplaceAction: TSynReplaceAction);
var
  Msg: string;
  Ret: TModalResult;
  p: TPoint;
const
  YYes2AllNoCancel = [mbYes, mbYesToAll, mbNo, mbCancel];
begin
  p := seJsonEdit.RowColumnToPixels(Point(Column, Line));
  Msg := SReplace + ' "' + ASearch + '" ' + SWith + ' "' + AReplace + '"?';
  Ret := MessageDlgPos(Msg, mtConfirmation, YYes2AllNoCancel, 0, Left + 100 + p.x, Top+100 + p.y);
  case Ret of
    mrYes:      ReplaceAction := raReplace;
    mrNo:       ReplaceAction := raSkip;
    mrAll,
    mrYesToAll: ReplaceAction := raReplaceAll;
  else
    ReplaceAction := raCancel;
  end;
end;

procedure TfrmMain.seJsonEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if scCaretX in Changes then lbCaretX.Caption := seJsonEdit.CaretX.ToString;
  if scCaretY in Changes then lbCaretY.Caption := seJsonEdit.CaretY.ToString;
  if scModified in Changes then
    if seJsonEdit.Modified then
      lbState.Caption := 'Modified'
    else
      lbState.Caption := '';
  if not(scSelection in Changes) then stbMainStatus.SimplePanel := False;
  seJsonEdit.LineHighlightColor.Background := FCurrLineColor;
end;

procedure TfrmMain.stbMainStatusResize(Sender: TObject);
begin
  stbMainStatus.Panels[0].Width := stbMainStatus.Width - 4;
end;

procedure TfrmMain.tbtOpenArrowClick(Sender: TObject);
begin
  ppmRecent.PopUp;
end;

procedure TfrmMain.tsSourceShow(Sender: TObject);
begin
  if pcMainPages.ActivePage = tsSource then
    if seJsonEdit.CanSetFocus then
      seJsonEdit.SetFocus;
end;

procedure TfrmMain.tsStructShow(Sender: TObject);
begin
  if pcMainPages.ActivePage = tsStruct then
    if tvSchemaTree.CanSetFocus then
      tvSchemaTree.SetFocus;
end;

procedure TfrmMain.ReadConfig;
var
  Cfg: TAppConf;
begin
  Cfg := GetAppConf;
  ReadPosition(Cfg);
  ReadSettings(Cfg);
end;

function TfrmMain.GetCurrFile: string;
begin
  Result := stbMainStatus.Panels[0].Text;
end;

function TfrmMain.GetSchemaName: string;
begin
  Result := edSchemaName.Text;
end;

function TfrmMain.GetSchemaNameChanged: Boolean;
begin
  Result := edSchemaName.Modified;
end;

procedure TfrmMain.SetCurrFile(const aValue: string);
begin
  stbMainStatus.Panels[0].Text := aValue;
end;

procedure TfrmMain.ReadPosition(aCfg: TAppConf);
var
  FormKey: string;
  State: TWindowState;
  c: Integer;
  Conf: TJsonConf;
begin
  FormKey := aCfg.GetComponentKey + '/' + ClassName;
  Conf := aCfg.Conf;
  if not Conf.TryOpenKey(FormKey, False) then
    begin
      Position := poScreenCenter;
      exit;
    end
  else
    Conf.CloseKey;
  Left := Conf.GetValue(FormKey + LEFT_KEY, Left);
  Top := Conf.GetValue(FormKey + TOP_KEY, Top);
  Width := Conf.GetValue(FormKey + WIDTH_KEY, Width);
  Height := Conf.GetValue(FormKey + HEIGHT_KEY, Height);
  Val(Conf.GetValue(FormKey + WND_STATE_KEY, 'wsNormal'), State, c);
  if c = 0 then
    WindowState := State;
end;

procedure TfrmMain.ReadSettings(aCfg: TAppConf);
begin
  seJsonEdit.BlockIndent := aCfg.JsonFormatStyle.IndentSize;
  if aCfg.JsonFormatStyle.LineBreak = jlbLF then
    seJsonEdit.Lines.LineBreak := #10
  else
    seJsonEdit.Lines.LineBreak := #13#10;
  FSuccessColor := aCfg.MiscSettings.SuccessColor;
  ReadDisplayStyle(aCfg);
  ReadRecentFiles(aCfg);
end;

procedure TfrmMain.ReadDisplayStyle(aCfg: TAppConf);
procedure ChangeStyle(Attri: TSynHighlighterAttributes; aEl: TFontStyle; aOn: Boolean); inline;
  begin
    if aOn then
      Attri.Style := Attri.Style + [aEl]
    else
      Attri.Style := Attri.Style - [aEl];
  end;
var
  Hl: TSynJTDSyn;
  Style: TDisplayStyle;
begin
  Style := aCfg.DisplayStyle;
  seJsonEdit.BeginUpdate;
  try
    Hl := TSynJTDSyn(seJsonEdit.Highlighter);
    seJsonEdit.Font.Name := Style.SynFontName;
    seJsonEdit.Font.Size := Style.SynFontSize;

    seJsonEdit.ExtraLineSpacing := Style.SynExtraLineSpace;
    seJsonEdit.ExtraCharSpacing := Style.SynExtraCharSpace;
    seJsonEdit.RightEdge := Style.SynRightEdge;
    TSynGutterLineNumber(seJsonEdit.Gutter.Parts[1]).ShowOnlyLineNumbersMultiplesOf := Style.SynShowEveryNthNumber;
    TSynGutterLineNumber(seJsonEdit.Gutter.Parts[1]).MarkupInfo.Foreground := Style.SynLineNumberColor;
    seJsonEdit.LineHighlightColor.Background := Style.SynCurrLineBackground;
    FCurrLineColor := Style.SynCurrLineBackground;
    FErrorLineColor := Style.SynErrorLineBackground;

    Hl.KeyAttri.Foreground := Style.SynKeyAttri.Foreground;
    ChangeStyle(Hl.KeyAttri, fsBold, Style.SynKeyAttri.Bold);
    ChangeStyle(Hl.KeyAttri, fsItalic, Style.SynKeyAttri.Italic);

    Hl.AttributeAttri.Foreground := Style.SynAttributeAttri.Foreground;
    ChangeStyle(Hl.AttributeAttri, fsBold, Style.SynAttributeAttri.Bold);
    ChangeStyle(Hl.AttributeAttri, fsItalic, Style.SynAttributeAttri.Italic);

    Hl.ReservedAttri.Foreground := Style.SynReservedAttri.Foreground;
    ChangeStyle(Hl.ReservedAttri, fsBold, Style.SynReservedAttri.Bold);
    ChangeStyle(Hl.ReservedAttri, fsItalic, Style.SynReservedAttri.Italic);

    Hl.SymbolAttri.Foreground := Style.SynSymbolAttri.Foreground;
    ChangeStyle(Hl.SymbolAttri, fsBold, Style.SynSymbolAttri.Bold);
    ChangeStyle(Hl.SymbolAttri, fsItalic, Style.SynSymbolAttri.Italic);

    Hl.CustomKeyAttri.Foreground := Style.SynCustomKeyAttri.Foreground;
    ChangeStyle(Hl.CustomKeyAttri, fsBold, Style.SynCustomKeyAttri.Bold);
    ChangeStyle(Hl.CustomKeyAttri, fsItalic, Style.SynCustomKeyAttri.Italic);

    Hl.IdentAttri.Foreground := Style.SynIdentAttri.Foreground;
    ChangeStyle(Hl.IdentAttri, fsBold, Style.SynIdentAttri.Bold);
    ChangeStyle(Hl.IdentAttri, fsItalic, Style.SynIdentAttri.Italic);
  finally
    seJsonEdit.EndUpdate;
  end;
end;

procedure TfrmMain.ReadRecentFiles(aCfg: TAppConf);
var
  Conf: TJsonConf;
begin
  Conf := aCfg.Conf;
  Conf.GetValue(aCfg.GetRecentFilesKey, FRecentFiles, '');
end;

procedure TfrmMain.AddRecentFile(const aFileName: string);
var
  I, MaxFiles: Integer;
begin
  MaxFiles := GetAppConf.MiscSettings.MaxRecentFiles;
  I := FRecentFiles.IndexOf(aFileName);
  if I >= 0 then
    FRecentFiles.Delete(I);
  FRecentFiles.Insert(0, aFileName);
  while FRecentFiles.Count > MaxFiles do
    FRecentFiles.Delete(Pred(FRecentFiles.Count));
  FRecentFilesModified := True;
end;

procedure TfrmMain.WritePosition;
var
  Cfg: TJsonConf;
  FormKey, s: string;
begin
  if WindowState = wsMinimized then exit;
  Cfg := GetAppConf.Conf;
  FormKey := GetAppConf.GetComponentKey + '/' + ClassName;
  if Self.WindowState = wsMaximized then
    begin
      Str(WindowState, s);
      Cfg.SetValue(FormKey + WND_STATE_KEY, s);
    end
  else
    begin
      Cfg.SetValue(FormKey + LEFT_KEY, Left);
      Cfg.SetValue(FormKey + TOP_KEY, Top);
      Cfg.SetValue(FormKey + WIDTH_KEY, Width);
      Cfg.SetValue(FormKey + HEIGHT_KEY, Height);
      Str(WindowState, s);
      Cfg.SetValue(FormKey + WND_STATE_KEY, s);
    end;
  Cfg.Flush;
end;

procedure TfrmMain.WriteRecentFiles;
var
  Conf: TAppConf;
begin
  if not FRecentFilesModified then exit;
  Conf := GetAppConf;
  Conf.Conf.SetValue(Conf.GetRecentFilesKey, FRecentFiles);
end;

procedure TfrmMain.acAppExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.acBeginArrayExecute(Sender: TObject);
begin
  ExecBeginStruct(False);
end;

procedure TfrmMain.acBeginObjectExecute(Sender: TObject);
begin
  ExecBeginStruct;
end;

procedure TfrmMain.acAboutExecute(Sender: TObject);
begin
  with TfrmAbout.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfrmMain.acCopyExecute(Sender: TObject);
begin
  seJsonEdit.CopyToClipboard;
end;

procedure TfrmMain.acCopyUpdate(Sender: TObject);
begin
  acCopy.Enabled := acCut.Enabled;
end;

procedure TfrmMain.acCutExecute(Sender: TObject);
begin
  seJsonEdit.CutToClipboard;
end;

procedure TfrmMain.acCutUpdate(Sender: TObject);
begin
  acCut.Enabled := (pcMainPages.ActivePage = tsSource) and (seJsonEdit.SelText <> '');
end;

procedure TfrmMain.acFindExecute(Sender: TObject);
begin
  ShowSearchDialog;
end;

procedure TfrmMain.acFindNextExecute(Sender: TObject);
begin
  SearchNext;
end;

procedure TfrmMain.acFindNextUpdate(Sender: TObject);
begin
  acFindNext.Enabled := FSearcing;
end;

procedure TfrmMain.acFindUpdate(Sender: TObject);
begin
  acFind.Enabled := acSelectAll.Enabled;
end;

procedure TfrmMain.acFormatExecute(Sender: TObject);
var
  Node: TJsonNode;
  p: TPoint;
  vr: TValidateResult;
  txt: string;
begin
  Screen.Cursor := crHourGlass;
  try
    Application.ProcessMessages;
    txt := seJsonEdit.Text;
    if txt.IsNullOrWhiteSpace(txt) then exit;
    vr := ValidateJson(txt);
    if vr.IsValid then
      begin
        Node := TJsonNode.NewJson(txt);
        try
          p := seJsonEdit.CaretXY;
          seJsonEdit.BeginUpdate;
          try
            seJsonEdit.SelectAll;
            seJsonEdit.SelText := Node.FormatJson(GetAppConf.JsonFormatStyle);
            seJsonEdit.CaretXY := p;
          finally
            seJsonEdit.EndUpdate;
          end;
        finally
          Node.Free;
        end;
      end;
  finally
    Screen.Cursor := crDefault;
  end;
  if not vr.IsValid then
    begin
      FSourceState := ssInvalidJson;
      SourceStatusChanged(vr);
      MessageDlg(Application.Title, SECantFormatInvalidJson, mtInformation, [mbOk], '');
    end;
  if seJsonEdit.CanSetFocus then
    seJsonEdit.SetFocus;
end;

procedure TfrmMain.acFormatUpdate(Sender: TObject);
begin
  acFormat.Enabled := (pcMainPages.ActivePage = tsSource) and acSelectAll.Enabled;
end;

procedure TfrmMain.acGenUnitExecute(Sender: TObject);
begin
  with TfrmCodegenDlg.Create(nil) do
    try
      CurrSchemaName := SchemaName;
      CurrSchema := FCurrJson.AsJson;
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfrmMain.acGenUnitUpdate(Sender: TObject);
begin
  acGenUnit.Enabled := FSourceState = ssValidSchema;
end;

procedure TfrmMain.acHelpOnlineExecute(Sender: TObject);
begin
  OpenUrl(GetJtdDocUrl);
end;

procedure TfrmMain.acInstanceTreeCollapseExecute(Sender: TObject);
begin
  if tvInstanceTree.Items.Count = 0 then exit;
  Screen.Cursor := crHourGlass;
  try
    tvInstanceTree.BeginUpdate;
    try
      tvInstanceTree.FullCollapse;
    finally
      tvInstanceTree.EndUpdate;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.acInstanceTreeCollapseUpdate(Sender: TObject);
begin
  acInstanceTreeCollapse.Enabled := tvInstanceTree.Items.Count <> 0;
end;

procedure TfrmMain.acInstanceTreeExpandExecute(Sender: TObject);
begin
  if tvInstanceTree.Items.Count = 0 then exit;
  Screen.Cursor := crHourGlass;
  try
    tvInstanceTree.BeginUpdate;
    try
      tvInstanceTree.FullExpand;
    finally
      tvInstanceTree.EndUpdate;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.acInstanceTreeExpandUpdate(Sender: TObject);
begin
  acInstanceTreeExpand.Enabled := tvInstanceTree.Items.Count <> 0;
end;

procedure TfrmMain.acNewFromSampleExecute(Sender: TObject);
var
  SampleFiles, Samples: TStringArray;
  Hints: array of TJtdInferrer.THint;
  I: Integer;
  Schema: TJsonNode;
begin
  if not CheckModified then exit;
  with TfrmInferDlg.Create(nil) do
    try
      if ShowModal <> mrOk then exit;
      SampleFiles := SampleFileList;
      if Length(SampleFiles) = 0 then exit;
      Hints := HintList;
    finally
      Free;
    end;
  //////////////
  CurrentFile := '';
  ////////////////////
  Screen.Cursor := crHourGlass;
  try
    Application.ProcessMessages;
    SetLength(Samples, Length(SampleFiles));
    with TStringStream.Create do
      try
        for I := 0 to High(SampleFiles) do
          begin
            LoadFromFile(SampleFiles[I]);
            Samples[I] := DataString;
          end;
      finally
        Free;
      end;
    Schema := TJtdInferrer.Infer(Samples, Hints);
    seJsonEdit.BeginUpdate;
    try
      seJsonEdit.Lines.Text := Schema.FormatJson(GetAppConf.JsonFormatStyle);
      Schema.Free;
      seJsonEdit.Modified := True;
      edSchemaName.Text := ExtractFileNameOnly(SampleFiles[0]);
      edSchemaNameChange(Sender);
      LoadNewSource;
    finally
      seJsonEdit.EndUpdate;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
  if seJsonEdit.CanSetFocus then
    seJsonEdit.SetFocus;
end;

procedure TfrmMain.acOpenInstanceExecute(Sender: TObject);
var
  Msg: string;
begin
  if not OD.Execute then exit;
  FreeAndNil(FCurrInstance);
  NewInstanceFile; /////////////////
  if not TJsonNode.TryParseFile(OD.FileName, FCurrInstance) then
    begin
      Msg := Format(SEInvalidJsonFileFmt, [OD.FileName]);
      MessageDlg(Application.Title, Msg, mtError, [mbOk], '');
      exit;
    end;
  LoadInstanceTreeView(ExtractFileNameOnly(OD.FileName));
end;

procedure TfrmMain.acOpenInstanceUpdate(Sender: TObject);
begin
  acOpenInstance.Enabled := (pcMainPages.ActivePage = tsStruct) and (tvSchemaTree.Items.Count <> 0);
end;

procedure TfrmMain.acQuotesExecute(Sender: TObject);
begin
  seJsonEdit.BeginUndoBlock;
  seJsonEdit.InsertTextAtCaret('""');
  seJsonEdit.CaretX := seJsonEdit.CaretX - 1;
  seJsonEdit.EndUndoBlock;
end;

procedure TfrmMain.acReplaceExecute(Sender: TObject);
begin
  ShowReplaceDialog;
end;

procedure TfrmMain.acReplaceUpdate(Sender: TObject);
begin
  acReplace.Enabled := acFind.Enabled;
end;

procedure TfrmMain.acStartValidateExecute(Sender: TObject);
var
  Schema: TJtdSchema;
  ve: TValidateError;
  I: Integer;
const
  Fmt = '%d. %s: "%s", %s: "%s"';
begin
  lbxErrorList.Clear;
  if not TJtdSchema.TryLoad(FCurrJson, Schema) then
    begin
      ShowMessage('Oops, something went wrong!');
      exit;
    end;
  try
    if Validate(FCurrInstance, Schema, FValidateErrList) = jvrOk then
      begin
        stxValidateStatus.Caption := SValidationOk;
        stxValidateStatus.Color := FSuccessColor;
      end
    else
      begin
        stxValidateStatus.Caption := Format(SEValitateErrsFmt, [Length(FValidateErrList)]);
        stxValidateStatus.Color := FErrorLineColor;
        I := 1;
        for ve in FValidateErrList do
          begin
            lbxErrorList.Items.Add(
              Format(Fmt, [I, SSchemaPath, ve.SchemaPath, SInstancePath, ve.InstancePath]));
            Inc(I);
          end;
      end;
  finally
    Schema.Free;
  end;
end;

procedure TfrmMain.acStartValidateUpdate(Sender: TObject);
begin
  acStartValidate.Enabled := (pcMainPages.ActivePage = tsStruct) and(tvInstanceTree.Items.Count <> 0);
end;

procedure TfrmMain.acSyntaxCheckExecute(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    Application.ProcessMessages;
    LoadNewSource;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.acSyntaxCheckUpdate(Sender: TObject);
begin
  acSyntaxCheck.Enabled := (pcMainPages.ActivePage = tsSource) and
    (FSourceState < ssValidSchema) and (seJsonEdit.Lines.Count > 0);
end;

procedure TfrmMain.acOpenExecute(Sender: TObject);
begin
  if not CheckModified then exit;
  if not OD.Execute then exit;
  CurrentFile := OD.FileName;
  edSchemaName.Text := ExtractFileNameOnly(CurrentFile);
  edSchemaName.Modified := False;
  edSchemaNameChange(Sender);
  FRecentFilesModified := False;
  TryLoadSchemaFile(OD.FileName);
end;

procedure TfrmMain.acPasteExecute(Sender: TObject);
begin
  seJsonEdit.PasteFromClipboard;
end;

procedure TfrmMain.acPasteUpdate(Sender: TObject);
begin
  acPaste.Enabled := (pcMainPages.ActivePage = tsSource) and seJsonEdit.CanPaste;
end;

procedure TfrmMain.acRedoExecute(Sender: TObject);
begin
  seJsonEdit.Redo;
end;

procedure TfrmMain.acRedoUpdate(Sender: TObject);
begin
  acRedo.Enabled := (pcMainPages.ActivePage = tsSource) and seJsonEdit.CanRedo;
end;

procedure TfrmMain.acSaveAsExecute(Sender: TObject);
var
  fn: string;
begin
  if CurrentFile <> '' then
    SD.InitialDir := ExtractFilePath(CurrentFile)
  else
    SD.InitialDir := ProgramDirectory;
  SD.FileName := SchemaName;
  if not SD.Execute then exit;
  if not HandleFileSave(SD.FileName, fn) then exit;
  seJsonEdit.Modified := False;
  if fn <> CurrentFile then
    begin
      CurrentFile := fn;
      AddRecentFile(fn);
      edSchemaName.Text := ExtractFileNameOnly(fn);
      edSchemaNameChange(Sender);
    end;
end;

procedure TfrmMain.acSaveAsUpdate(Sender: TObject);
begin
  acSaveAs.Enabled := seJsonEdit.Modified;
end;

procedure TfrmMain.acSaveExecute(Sender: TObject);
begin
  seJsonEdit.Lines.SaveToFile(CurrentFile);
  seJsonEdit.Modified := False;
  edSchemaName.Modified := False;
end;

procedure TfrmMain.acSaveUpdate(Sender: TObject);
begin
  acSave.Enabled := seJsonEdit.Modified and (FRecentFiles.Count <> 0) and (CurrentFile = FRecentFiles[0]);
end;

procedure TfrmMain.acSchemaNewExecute(Sender: TObject);
begin
  if not CheckModified then exit;
  CurrentFile := '';
  edSchemaName.Text := 'Unnamed';
  seJsonEdit.BeginUpdate(False);
  try
    seJsonEdit.BeginUndoBlock;
    seJsonEdit.SelectAll;
    seJsonEdit.SelText := '';
    seJsonEdit.InsertTextAtCaret('{');
    seJsonEdit.ExecuteCommand(ecLineBreak, ' ', nil);
    seJsonEdit.ExecuteCommand(ecLineBreak, ' ', nil);
    seJsonEdit.InsertTextAtCaret('}');
    seJsonEdit.CaretXY := Point(1, 2);
    seJsonEdit.InsertTextAtCaret(SpaceLine(seJsonEdit.BlockIndent));
    seJsonEdit.EndUndoBlock;
  finally
    seJsonEdit.EndUpdate;
  end;
  edSchemaNameChange(Sender);
  seJsonEdit.Modified := True;
  LoadNewSource;
end;

procedure TfrmMain.acSelectAllExecute(Sender: TObject);
begin
  seJsonEdit.SelectAll;
end;

procedure TfrmMain.acSelectAllUpdate(Sender: TObject);
begin
  acSelectAll.Enabled := (pcMainPages.ActivePage = tsSource) and ((seJsonEdit.Lines.Count > 1) or
    ((seJsonEdit.Lines.Count = 1)and(seJsonEdit.Lines[0] <> '')));
end;

procedure TfrmMain.acSettingsExecute(Sender: TObject);
var
  Conf: TAppConf;
  MaxFiles: Integer;
  Ok: Boolean;
begin
  with TfrmSettingsDlg.Create(nil) do
    try
      Ok := ShowModal = mrOk;
    finally
      Free;
    end;
  if Ok then
    begin
      Conf := GetAppConf;
      MaxFiles := Conf.MiscSettings.MaxRecentFiles;
      FSuccessColor := Conf.MiscSettings.SuccessColor;
      seJsonEdit.BlockIndent := Conf.JsonFormatStyle.IndentSize;
      if Conf.JsonFormatStyle.LineBreak = jlbLF then
        seJsonEdit.Lines.LineBreak := #10
      else
        seJsonEdit.Lines.LineBreak := #13#10;
      while FRecentFiles.Count > MaxFiles do
        FRecentFiles.Delete(0);
      ReadDisplayStyle(Conf);
    end;
end;

procedure TfrmMain.acSchemaTreeCollapseExecute(Sender: TObject);
begin
  if tvSchemaTree.Items.Count = 0 then exit;
  Screen.Cursor := crHourGlass;
  try
    tvSchemaTree.BeginUpdate;
    try
      tvSchemaTree.FullCollapse;
    finally
      tvSchemaTree.EndUpdate;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.acSchemaTreeCollapseUpdate(Sender: TObject);
begin
  acSchemaTreeCollapse.Enabled := (pcMainPages.ActivePage = tsStruct) and (tvSchemaTree.Items.Count <> 0);
end;

procedure TfrmMain.acSchemaTreeExpandExecute(Sender: TObject);
begin
  if tvSchemaTree.Items.Count = 0 then exit;
  Screen.Cursor := crHourGlass;
  try
    tvSchemaTree.BeginUpdate;
    try
      tvSchemaTree.FullExpand;
    finally
      tvSchemaTree.EndUpdate;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.acSchemaTreeExpandUpdate(Sender: TObject);
begin
  acSchemaTreeExpand.Enabled := (pcMainPages.ActivePage = tsStruct) and (tvSchemaTree.Items.Count <> 0);
end;

procedure TfrmMain.acUndoExecute(Sender: TObject);
begin
  seJsonEdit.Undo;
end;

procedure TfrmMain.acUndoUpdate(Sender: TObject);
begin
  acUndo.Enabled := (pcMainPages.ActivePage = tsSource) and seJsonEdit.CanUndo;
end;

procedure TfrmMain.AppPropsHint(Sender: TObject);
begin
  if Application.Hint <> '' then
    begin
      stbMainStatus.SimplePanel := True;
      stbMainStatus.SimpleText := '  ' + Application.Hint;
    end
  else
    stbMainStatus.SimplePanel := False;
end;

procedure TfrmMain.AppPropsQueryEndSession(var Cancel: Boolean);
begin
  CheckModified;
end;

procedure TfrmMain.edSchemaNameChange(Sender: TObject);
begin
  Caption := Application.Title + ' - ' + edSchemaName.Text;
  if edSchemaName.Text <> '' then
    if CurrentFile <> '' then
      CurrentFile := ExtractFilePath(CurrentFile) + edSchemaName.Text + '.json';
end;

procedure TfrmMain.edSchemaNameEditingDone(Sender: TObject);
begin
  if seJsonEdit.CanSetFocus then
    seJsonEdit.SetFocus;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Pred(aclMainActions.ActionCount) do
    aclMainActions.UpdateAction(aclMainActions.Actions[I]);
  if seJsonEdit.CanSetFocus then
    seJsonEdit.SetFocus;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckModified;
end;

function TfrmMain.HandleFileSave(const aFileName: string; out aSaved: string): Boolean;
var
  Msg, Fn, Ext, s: string;
  I: Integer;
begin
  if not FileExists(aFileName) then
    begin
      seJsonEdit.Lines.SaveToFile(aFileName);
      aSaved := aFileName;
      exit(True);
    end;
  Msg := Format(SFileOvewritePromptFmt, [aFileName]);
  case MessageDlg(Application.Title, Msg, mtConfirmation, mbYesNoCancel, '') of
    mrYes:
      begin
        seJsonEdit.Lines.SaveToFile(aFileName);
        aSaved := aFileName;
        Result := True;
      end;
    mrNo:
      begin
        Fn := ExtractFilePath(aFileName) + ExtractFileNameOnly(aFileName) + ' ' + SCopy;
        Ext := ExtractFileExt(aFileName);
        I := 0;
        s := Fn;
        if FileExists(s + Ext) then
          repeat
            Inc(I);
            s := Fn + I.ToString;
          until not FileExists(s + Ext);
        aSaved := s + Ext;
        seJsonEdit.Lines.SaveToFile(aSaved);
        Result := True;
      end
  else
    Result := False;
  end;
end;

procedure TfrmMain.LoadSchemaTreeView;
  function GetImageIndex(const aKey: string): Integer;
  begin
    case aKey of
      'additionalProperties': Result := 1;
      'boolean':              Result := 2;
      'discriminator':        Result := 3;
      'elements':             Result := 4;
      'enum':                 Result := 5;
      'float32',
      'float64',
      'int8',
      'int16',
      'int32':                Result := 6;
      'mapping':              Result := 7;
      'metadata':             Result := 8;
      'nullable':             Result := 9;
      'optionalProperties':   Result := 10;
      'properties':           Result := 11;
      'ref':                  Result := 12;
      'string':               Result := 13;
      'timestamp':            Result := 14;
      'type':                 Result := 15;
      'uint8',
      'uint16',
      'uint32':               Result := 6;
      'values':               Result := 16;
    else
      Result := 17;
    end;
  end;
  procedure VisitNode(aNode: TTreeNode; aSchema: TJsonNode);
  var
    p: TJsonNode.TPair;
    Node: TTreeNode;
    I: Integer;
    n: TJsonNode;
  begin
    case aSchema.Kind of
      jvkObject:
        for p in aSchema.Entries do
          begin
            Node := tvSchemaTree.Items.AddChild(aNode, p.Key);
            Node.ImageIndex := GetImageIndex(p.Key);
            Node.SelectedIndex := Node.ImageIndex + 19;
            FSchema2TreeMap.Add(p.Value, Node);
            VisitNode(Node, p.Value);
          end;
      jvkArray:
        for I := 0 to Pred(aSchema.Count) do
          begin
            n := aSchema.Items[I];
            Node := tvSchemaTree.Items.AddChild(aNode, I.ToString);
            Node.ImageIndex := 18;
            Node.SelectedIndex := Node.ImageIndex + 19;
            VisitNode(Node, n);
          end;
    else
      Node := tvSchemaTree.Items.AddChild(aNode, aSchema.ToString);
      Node.ImageIndex := GetImageIndex(aSchema.ToString);
      Node.SelectedIndex := Node.ImageIndex + 19;
      FSchema2TreeMap.Add(aSchema, Node);
    end;
  end;
var
  Root: TTreeNode;
begin
  tvSchemaTree.BeginUpdate;
  try
    tvSchemaTree.Items.Clear;
    FSchema2TreeMap.Clear;
    if FCurrJson = nil then exit;
    tvSchemaTree.AutoExpand := False;
    Root := tvSchemaTree.Items.Add(nil, SchemaName);
    Root.ImageIndex := 0;
    Root.SelectedIndex := 19;
    FSchema2TreeMap.Add(FCurrJson, Root);
    VisitNode(Root, FCurrJson);
    tvSchemaTree.AutoExpand := True;
  finally
    tvSchemaTree.EndUpdate;
  end;
end;

procedure TfrmMain.SchemaChanged;
begin
  LoadSchemaTreeView;
  if FCurrJson = nil then exit;
  seJsonEdit.BeginUpdate;
  try
    TSynJTDSyn(seJsonEdit.Highlighter).NewDoc(FCurrJson);
  finally
    seJsonEdit.EndUpdate;
  end;
  if seJsonEdit.CanSetFocus then
    seJsonEdit.SetFocus;
end;

procedure TfrmMain.SourceStatusChanged(const aResult: TValidateResult);
var
  Msg, Prop: string;
  Start, Fin: Integer;
begin
  case FSourceState of
    ssUnknown:
      begin
        lbSyntaxState.Caption := 'Unknown status';
        pnSourceBottom.Color := TColor(clDefault);
      end;
    ssInvalidJson:
      begin
        lbSyntaxState.Caption := 'JSON syntax error : ' + aResult.Message;
        if aResult.CaretY <> -1 then
          seJsonEdit.CaretY := aResult.CaretY;
        if aResult.CaretX <> -1 then
          seJsonEdit.CaretX := aResult.CaretX;
        seJsonEdit.LineHighlightColor.Background := FErrorLineColor;
        seJsonEdit.EnsureCursorPosVisible;
        pnSourceBottom.Color := FErrorLineColor;
      end;
    ssInvalidSchema:
      begin
        Msg := aResult.Message;
        lbSyntaxState.Caption := 'Schema syntax error : ' + Msg;
        Start := Pos('(', Msg);
        if Start > 0 then
          begin
            Inc(Start);
            Fin := Pos(')', Msg, Start);
            if Fin > 0 then
              begin
                Prop := Copy(Msg, Start, Fin-Start);
                Start := seJsonEdit.SearchReplaceEx(
                  Prop, '', [ssoMatchCase, ssoWholeWord, ssoEntireScope], Point(1, 1));
                if Start > 0 then
                  seJsonEdit.LineHighlightColor.Background := FErrorLineColor;
              end;
          end;
        pnSourceBottom.Color := FErrorLineColor;
      end;
    ssValidSchema:
      begin
        lbSyntaxState.Caption := 'Schema instance : ' + FormKinds[FSchemaFormKind];
        pnSourceBottom.Color := FSuccessColor;
      end;
  end;
  SchemaChanged;
  if seJsonEdit.CanSetFocus then
    seJsonEdit.SetFocus;
end;

procedure TfrmMain.LoadNewSource;
var
  Schema: TJtdSchema;
  vr: TValidateResult;
  SchemaLoaded: Boolean = False;
  txt: string;
begin
  tvSchemaTree.Items.Clear;
  FreeAndNil(FCurrJson);
  txt := seJsonEdit.Text;
  if string.IsNullOrWhiteSpace(txt) then exit;
  vr := ValidateJson(txt);
  if vr.IsValid then
    begin
      FCurrJson := TJsonNode.Create;
      FCurrJson.AsJson := txt;
      Schema := TJtdSchema.Create;
      try
        try
          Schema.Load(FCurrJson);
          SchemaLoaded := True;
        except
          on e: Exception do
            vr := TValidateResult.Make(e.Message);
        end;
        if SchemaLoaded then
          begin
            FSourceState := ssValidSchema;
            vr := TValidateResult.Valid;
            FSchemaFormKind := Schema.Kind;
          end
        else
          begin
            FSourceState := ssInvalidSchema;
            FSchemaFormKind := fkNone;
          end;
      finally
        Schema.Free;
      end;
    end
  else
    FSourceState := ssInvalidJson;
  SourceStatusChanged(vr);
end;

procedure TfrmMain.TryLoadSchemaFile(const aFileName: string);
var
  Msg: string;
begin
  Screen.Cursor := crHourGlass;
  try
    Application.ProcessMessages;
    AddRecentFile(aFileName);
    try
      seJsonEdit.BeginUpdate;
      try
        seJsonEdit.Lines.LoadFromFile(aFileName, TEncoding.UTF8);
      finally
        seJsonEdit.EndUpdate;
      end;
    except
      on e: Exception do
        begin
          Msg := Format(SECantLoadFileFmt, [aFileName, e.ClassName, e.Message]);
          MessageDlg(Application.Title, Msg, mtError, [mbOK], '');
          exit;
        end;
    end;
    LoadNewSource;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.RecentFileClick(Sender: TObject);
var
  Msg, fn: string;
  I: Integer;
begin
  if not(Sender is TMenuItem) then exit;
  fn := TMenuItem(Sender).Caption;
  if fn = CurrentFile then exit;
  if not FileExists(fn) then
    begin
      Msg := Format(SEFileNotFoundFmt, [fn]);
      MessageDlg(Application.Title, Msg, mtError, [mbOK], '');
      I := FRecentFiles.IndexOf(fn);
      if I >= 0 then
        begin
          FRecentFiles.Delete(I);
          FRecentFilesModified := True;
        end;
      exit;
    end;
  if CheckModified then
    begin
      CurrentFile := fn;
      edSchemaName.Text := ExtractFileNameOnly(CurrentFile);
      edSchemaName.Modified := False;
      edSchemaNameChange(Sender);
      TryLoadSchemaFile(fn);
    end;
end;

function TfrmMain.CheckModified: Boolean;
var
  Fn: string;
begin
  if not seJsonEdit.Modified then exit(True);
  case MessageDlg(Application.Title, SModifiedSavePrompt, mtConfirmation, mbYesNoCancel, '') of
    mrCancel: exit(False);
    mrNo:     exit(True);
  else
  end;
  if CurrentFile <> '' then
    SD.InitialDir := ExtractFilePath(CurrentFile)
  else
    SD.InitialDir := ProgramDirectory;
  SD.FileName := SchemaName;
  if not SD.Execute then exit(True);
  if not HandleFileSave(SD.FileName, Fn) then exit(False);
  seJsonEdit.Modified := False;
  if fn <> CurrentFile then
    begin
      CurrentFile := Fn;
      AddRecentFile(Fn);
      edSchemaName.Text := ExtractFileNameOnly(fn);
      edSchemaName.Modified := True;
    end;
  Result := True;
end;

procedure TfrmMain.ShowSearchDialog;
begin
  with TfrmSearchDlg.Create(nil) do
    try
      cbSearchText.Text := seJsonEdit.GetWordAtRowCol(seJsonEdit.CaretXY);
      if opSearchTextAtCaret then
        begin
          if seJsonEdit.SelAvail and (seJsonEdit.BlockBegin.Y = seJsonEdit.BlockEnd.Y) then
            SearchText := seJsonEdit.SelText
          else
            SearchText := seJsonEdit.GetWordAtRowCol(seJsonEdit.CaretXY);
        end;
      FSearcing := (ShowModal = mrOK) and (gsSearchText <> '');
    finally
      Free;
    end;
  SearchNext;
end;

procedure TfrmMain.SearchNext;
var
  Options: TSynSearchOptions;
  Msg: string;
begin
  if not FSearcing or (gsSearchText = '') then exit;
  Options := [];
  if opSearchBackwards then Include(Options, ssoBackwards);
  if opSearchCaseSensitive then Include(Options, ssoMatchCase);
  if not opSearchFromCaret then Include(Options, ssoEntireScope);
  if opSearchSelectionOnly then Include(Options, ssoSelectedOnly);
  if opSearchWholeWords then Include(Options, ssoWholeWord);
  if opSearchRegex then Include(Options, ssoRegExpr);
  if seJsonEdit.SearchReplace(gsSearchText, '', Options) = 0 then
    begin
      Msg := Format(STextNotFoundFmt, [gsSearchText]);
      MessageDlg(Application.Title, Msg, mtInformation, [mbOk], '');//todo: new search prompt ???
      FSearcing := False;
      if ssoBackwards in Options then
        seJsonEdit.BlockEnd := seJsonEdit.BlockBegin
      else
        seJsonEdit.BlockBegin := seJsonEdit.BlockEnd;
      seJsonEdit.CaretXY := seJsonEdit.BlockBegin;
    end;
  if seJsonEdit.CanSetFocus then
    seJsonEdit.SetFocus;
end;

procedure TfrmMain.ShowReplaceDialog;
var
  Options: TSynSearchOptions;
  Msg: string;
begin
  with TfrmReplaceDlg.Create(nil) do
    try
      cbSearchText.Text := seJsonEdit.GetWordAtRowCol(seJsonEdit.CaretXY);
      if opSearchTextAtCaret then
        if seJsonEdit.SelAvail and (seJsonEdit.BlockBegin.Y = seJsonEdit.BlockEnd.Y) then
          SearchText := seJsonEdit.SelText
        else
          SearchText := seJsonEdit.GetWordAtRowCol(seJsonEdit.CaretXY);
      ReplaceText := gsReplaceText;
      ReplaceTextHistory := gsReplaceTextHistory;
      if ShowModal = mrOK then
        begin
          gsReplaceText := ReplaceText;
          gsReplaceTextHistory := ReplaceTextHistory;
          if (gsSearchText <> '') then
            begin
              Options := [ssoPrompt, ssoReplace, ssoReplaceAll];
              if opSearchBackwards then Include(Options, ssoBackwards);
              if opSearchCaseSensitive then Include(Options, ssoMatchCase);
              if not opSearchFromCaret then Include(Options, ssoEntireScope);
              if opSearchSelectionOnly then Include(Options, ssoSelectedOnly);
              if opSearchWholeWords then Include(Options, ssoWholeWord);
              if opSearchRegex then Include(Options, ssoRegExpr);
              if seJsonEdit.SearchReplace(gsSearchText, gsReplaceText, Options) = 0 then
                begin
                  Msg := Format(STextNotFoundFmt, [gsSearchText]);
                  MessageDlg(Application.Title, Msg, mtInformation, [mbOk], '');//todo: new search prompt ???
                  if ssoBackwards in Options then
                    seJsonEdit.BlockEnd := seJsonEdit.BlockBegin
                  else
                    seJsonEdit.BlockBegin := seJsonEdit.BlockEnd;
                  seJsonEdit.CaretXY := seJsonEdit.BlockBegin;
                end;
              if seJsonEdit.CanSetFocus then
                seJsonEdit.SetFocus;
            end;
        end;
    finally
      Free;
    end;
end;

procedure TfrmMain.ExecBeginStruct(aObject: Boolean);
var
  CurrIndent, LineNum: Integer;
  Start, Fin: string;
  s: string;
begin
  if aObject then begin
    Start := '{'; Fin := '}';
  end else begin
    Start := '['; Fin := ']';
  end;
  s := seJsonEdit.LineText;
  CurrIndent := GetIndent(s);
  seJsonEdit.BeginUndoBlock;
  if not (jfoEgyptBrace in GetAppConf.JsonFormatStyle.Options) then begin
    if (CurrIndent = seJsonEdit.CaretX - 1) or (s = '') then
      seJsonEdit.InsertTextAtCaret(Start)
    else begin
      seJsonEdit.ExecuteCommand(ecLineBreak, ' ', nil);
      seJsonEdit.InsertTextAtCaret(SpaceLine(CurrIndent) + Start);
    end
  end else
    seJsonEdit.InsertTextAtCaret(Start);
  seJsonEdit.ExecuteCommand(ecLineBreak, ' ', nil);
  LineNum := seJsonEdit.CaretY;
  seJsonEdit.ExecuteCommand(ecLineBreak, ' ', nil);
  seJsonEdit.InsertTextAtCaret(SpaceLine(CurrIndent) + Fin);
  seJsonEdit.CaretXY := Point(1, LineNum);
  seJsonEdit.InsertTextAtCaret(SpaceLine(CurrIndent + seJsonEdit.BlockIndent));
  seJsonEdit.EndUndoBlock;
end;

procedure TfrmMain.NewInstanceFile;
begin
  lbxErrorList.Clear;
  stxValidateStatus.Color := clDefault;
  stxValidateStatus.Caption := ValidateStateNone;
end;

procedure TfrmMain.LoadInstanceTreeView(const aRootName: string);
  procedure VisitNode(aNode: TTreeNode; aJson: TJsonNode);
  var
    TreeNode: TTreeNode;
    jNode: TJsonNode;
    p: TJsonNode.TPair;
    I: Integer;
  begin
    case aJson.Kind of
      jvkUnknown:
        begin
          TreeNode := tvInstanceTree.Items.AddChild(aNode, '???');
          TreeNode.ImageIndex := 0;
          aNode.ImageIndex := 0;
        end;
      jvkNull:
        begin
          TreeNode := tvInstanceTree.Items.AddChild(aNode, 'null');
          TreeNode.ImageIndex := 1;
          aNode.ImageIndex := 1;
          FInstance2TreeMap.Add(aJson, TreeNode);
        end;
      jvkFalse:
        begin
          TreeNode := tvInstanceTree.Items.AddChild(aNode, 'false');
          TreeNode.ImageIndex := 2;
          TreeNode.SelectedIndex := TreeNode.ImageIndex + 9;
          aNode.ImageIndex := 2;
          FInstance2TreeMap.Add(aJson, TreeNode);
        end;
      jvkTrue:
        begin
          TreeNode := tvInstanceTree.Items.AddChild(aNode, 'true');
          TreeNode.ImageIndex := 2;
          TreeNode.SelectedIndex := TreeNode.ImageIndex + 9;
          aNode.ImageIndex := 2;
          FInstance2TreeMap.Add(aJson, TreeNode);
        end;
      jvkNumber:
        begin
          TreeNode := tvInstanceTree.Items.AddChild(aNode, aJson.ToString);
          TreeNode.ImageIndex := 3;
          TreeNode.SelectedIndex := TreeNode.ImageIndex + 9;
          aNode.ImageIndex := 3;
          FInstance2TreeMap.Add(aJson, TreeNode);
        end;
      jvkString:
        begin
          TreeNode := tvInstanceTree.Items.AddChild(aNode, aJson.ToString);
          TreeNode.ImageIndex := 4;
          TreeNode.SelectedIndex := TreeNode.ImageIndex + 9;
          aNode.ImageIndex := 4;
          FInstance2TreeMap.Add(aJson, TreeNode);
        end;
      jvkArray:
        begin
          aNode.ImageIndex := 5;
          FInstance2TreeMap.Add(aJson, aNode);
          for I := 0 to Pred(aJson.Count) do
            begin
              jNode := aJson.Items[I];
              TreeNode := tvInstanceTree.Items.AddChild(aNode, I.ToString);
              VisitNode(TreeNode, jNode);
            end;
        end;
      jvkObject:
        begin
          aNode.ImageIndex := 6;
          FInstance2TreeMap.Add(aJson, aNode);
          for p in aJson.Entries do
            begin
              TreeNode := tvInstanceTree.Items.AddChild(aNode, p.Key);
              TreeNode.SelectedIndex := TreeNode.ImageIndex + 9;
              VisitNode(TreeNode, p.Value);
            end;
        end;
    end;
    aNode.SelectedIndex := aNode.ImageIndex + 9;
  end;
var
  Root: TTreeNode;
begin
  tvInstanceTree.BeginUpdate;
  try
    tvInstanceTree.Items.Clear;
    FInstance2TreeMap.Clear;
    if FCurrInstance = nil then exit;
    tvInstanceTree.AutoExpand := False;
    Root := tvInstanceTree.Items.Add(nil, aRootName);
    VisitNode(Root, FCurrInstance);
    tvInstanceTree.AutoExpand := True;
  finally
    tvInstanceTree.EndUpdate;
  end;
end;

end.





