unit sedInferDlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, ActnList, Grids, Menus,
  SynEdit, lgDeque, lgJson, lgJtdInfer;

type

  THintList = array of TJtdInferrer.THint;

  { TfrmInferDlg }

  TfrmInferDlg = class(TForm)
    acAddSamples: TAction;
    acDeleteSample: TAction;
    acNodeExpand: TAction;
    acNodeCollapse: TAction;
    acCopyPath: TAction;
    acDeleteRow: TAction;
    alInferActions: TActionList;
    btOk: TButton;
    btCancel: TButton;
    Label1: TLabel;
    lbxSamples: TListBox;
    MenuItem1: TMenuItem;
    ppmCopyPath: TMenuItem;
    Separator1: TMenuItem;
    ppmExpand: TMenuItem;
    OdSamples: TOpenDialog;
    pnBottom: TPanel;
    pnCurrPath: TPanel;
    pnTopRight: TPanel;
    pnTopLeft: TPanel;
    pnTop: TPanel;
    ppmTree: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    sgHints: TStringGrid;
    tbSelectItemsCmd: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    tvSampleStruct: TTreeView;
    procedure acAddSamplesExecute(Sender: TObject);
    procedure acCopyPathExecute(Sender: TObject);
    procedure acCopyPathUpdate(Sender: TObject);
    procedure acDeleteRowExecute(Sender: TObject);
    procedure acDeleteSampleExecute(Sender: TObject);
    procedure acDeleteSampleUpdate(Sender: TObject);
    procedure acNodeCollapseExecute(Sender: TObject);
    procedure acNodeCollapseUpdate(Sender: TObject);
    procedure acNodeExpandExecute(Sender: TObject);
    procedure acNodeExpandUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbxSamplesSelectionChange(Sender: TObject; User: boolean);
    procedure pnCurrPathDblClick(Sender: TObject);
    procedure sgHintsGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
    procedure tvSampleStructSelectionChanged(Sender: TObject);
  private
    FSampleList: TStringList;
    FCurrJson: TJsonNode;
    FCurrPathHolder: specialize TGLiteDeque<string>;
    function  GetHintList: THintList;
    function  GetFileList: TStringArray;
    function  GetCurrPath: string;
    procedure SetCurrPath(const aValue: string);
    procedure ReadPosition;
    procedure WritePosition;
    procedure LoadPreview(const aRootName: string);
    property  CurrPath: string read GetCurrPath write SetCurrPath;
  public
    property  SampleFileList: TStringArray read GetFileList;
    property  HintList: THintList read GetHintList;
  end;

var
  frmInferDlg: TfrmInferDlg;

implementation
{$B-}{$COPERATORS ON}
uses
  ClipBrd, FileUtil, LazFileUtils, lgVector, sedAppConf, lgJsonCfg, sedStrConsts;

{$R *.lfm}

{ TfrmInferDlg }

procedure TfrmInferDlg.FormCreate(Sender: TObject);
begin
  FSampleList := TStringList.Create;
  Caption := Application.Title + ' - ' + SSelectSamples2Infer;
end;

procedure TfrmInferDlg.acAddSamplesExecute(Sender: TObject);
var
  s: string;
begin
  if not OdSamples.Execute then exit;
  for s in OdSamples.Files do
    if FSampleList.IndexOf(s) < 0 then
      begin
        FSampleList.Add(s);
        //lbxSamples.Items.Add(ExtractFileNameOnly(s));
        lbxSamples.Items.Add(ExtractFileName(s));
      end;
end;

procedure TfrmInferDlg.acCopyPathExecute(Sender: TObject);
{$IFDEF LCLGTK2}
var
  s: string;
begin
  s := ClipBoard.AsText;
{$ELSE !LCLGTK2}
begin
{$ENDIF LCLGTK2}
  ClipBoard.AsText := TJsonPtr.ToPointer(FCurrPathHolder.ToArray);
end;

procedure TfrmInferDlg.acCopyPathUpdate(Sender: TObject);
begin
  acCopyPath.Enabled := tvSampleStruct.Selected <> nil;
end;

procedure TfrmInferDlg.acDeleteRowExecute(Sender: TObject);
var
  Msg: string;
begin
  if sgHints.Row > 0 then
    begin
      Msg := Format(SDeleteDridRowPromptFmt, [sgHints.Row]);
      if MessageDlg(Application.Title, Msg, mtConfirmation, mbOkCancel, '') = mrOk then
        sgHints.DeleteRow(sgHints.Row);
    end;
end;

procedure TfrmInferDlg.acDeleteSampleExecute(Sender: TObject);
var
  Msg: string;
  Idx: Integer;
begin
  Idx := lbxSamples.ItemIndex;
  if Idx < 0 then exit;
  Msg := Format(SRemoveListItemPrompt, [FSampleList[Idx]]);
  if MessageDlg(Application.Title, Msg, mtConfirmation, mbYesNo, '') <> mrYes then
    exit;
  tvSampleStruct.Items.Clear;
  tvSampleStructSelectionChanged(Sender);
  lbxSamples.Items.Delete(Idx);
  FSampleList.Delete(Idx);
  FreeAndNil(FCurrJson);
end;

procedure TfrmInferDlg.acDeleteSampleUpdate(Sender: TObject);
begin
  acDeleteSample.Enabled := lbxSamples.ItemIndex >= 0;
end;

procedure TfrmInferDlg.acNodeCollapseExecute(Sender: TObject);
begin
  if tvSampleStruct.Items.Count = 0 then exit;
  Screen.BeginWaitCursor;
  try
    tvSampleStruct.BeginUpdate;
    try
      tvSampleStruct.FullCollapse;
    finally
      tvSampleStruct.EndUpdate;
    end;
  finally
    Screen.EndWaitCursor;
  end;
end;

procedure TfrmInferDlg.acNodeCollapseUpdate(Sender: TObject);
begin
  acNodeCollapse.Enabled := tvSampleStruct.Items.Count <> 0;
end;

procedure TfrmInferDlg.acNodeExpandExecute(Sender: TObject);
begin
  if tvSampleStruct.Items.Count = 0 then exit;
  Screen.BeginWaitCursor;
  try
    tvSampleStruct.BeginUpdate;
    try
      tvSampleStruct.FullExpand;
    finally
      tvSampleStruct.EndUpdate;
    end;
  finally
    Screen.EndWaitCursor;
  end;
end;

procedure TfrmInferDlg.acNodeExpandUpdate(Sender: TObject);
begin
  acNodeExpand.Enabled := tvSampleStruct.Items.Count <> 0;
end;

procedure TfrmInferDlg.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (ModalResult = mrOk) and (FSampleList.Count = 0) then
    CanClose := MessageDlg(Application.Title, SInferDlgClosePrompt, mtConfirmation, mbOkCancel, '') <> mrCancel;
end;

procedure TfrmInferDlg.FormDestroy(Sender: TObject);
begin
  WritePosition;
  FSampleList.Free;
  FCurrJson.Free;
end;

procedure TfrmInferDlg.FormShow(Sender: TObject);
begin
  ReadPosition;
  pnCurrPath.Height := tbSelectItemsCmd.Height;
end;

procedure TfrmInferDlg.lbxSamplesSelectionChange(Sender: TObject; User: boolean);
var
  Msg: string;
begin
  tvSampleStruct.BeginUpdate;
  try
    tvSampleStruct.Items.Clear;
  finally
    tvSampleStruct.EndUpdate;
  end;
  FreeAndNil(FCurrJson);
  if lbxSamples.ItemIndex < 0 then exit;
  Screen.BeginWaitCursor;
  try
    if not TJsonNode.TryParseFile(FSampleList[lbxSamples.ItemIndex], FCurrJson) then
      begin
        Msg := Format(SEInvalidJsonFileFmt, [FSampleList[lbxSamples.ItemIndex]]);
        MessageDlg(Application.Title, Msg, mtError, [mbOk], '');
        exit;
      end;
    LoadPreview(ExtractFileNameOnly(lbxSamples.Items[lbxSamples.ItemIndex]));
  finally
    Screen.EndWaitCursor;
  end;
end;

procedure TfrmInferDlg.pnCurrPathDblClick(Sender: TObject);
begin
  if tvSampleStruct.Selected = nil then
    exit;
  acCopyPath.Execute;
end;

procedure TfrmInferDlg.sgHintsGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
begin
  if ARow >= 0 then
    case ACol of
      0: HintText := 'Select hint kind';
      1: HintText := 'Select default number type';
      2: HintText := 'List of enum elements separated by semicolon';
      3: HintText := 'Path to element';
    else
      HintText := '';
    end;
end;

procedure TfrmInferDlg.tvSampleStructSelectionChanged(Sender: TObject);
var
  SelNode, Root: TTreeNode;
begin
  SelNode := tvSampleStruct.Selected;
  FCurrPathHolder.MakeEmpty;
  CurrPath := '/';
  Root := tvSampleStruct.Items.GetFirstNode;
  if (SelNode = nil) or (SelNode = Root) then exit;
  if SelNode.Count <> 0 then
    FCurrPathHolder.PushFirst(SelNode.Text);
  while SelNode.Parent <> nil do
    begin
      SelNode := SelNode.Parent;
      if SelNode <> Root then
        FCurrPathHolder.PushFirst(SelNode.Text);
    end;
  if FCurrPathHolder.IsEmpty then
    CurrPath := '/'
  else
    CurrPath := '/' + string.Join('/', FCurrPathHolder.ToArray);
end;

function TfrmInferDlg.GetCurrPath: string;
begin
  Result := pnCurrPath.Caption;
end;

procedure TfrmInferDlg.SetCurrPath(const aValue: string);
begin
  pnCurrPath.Caption := aValue;
end;

procedure TfrmInferDlg.ReadPosition;
var
  FormKey: string;
  Cfg: TAppConf;
  Conf: TJsonConf;
begin
  Cfg := GetAppConf;
  FormKey := Cfg.GetComponentKey + '/' + ClassName;
  Conf := Cfg.Conf;
  if not Conf.TryOpenKey(FormKey, False) then
    begin
      Position := poMainFormCenter;
      exit;
    end
  else
    Conf.CloseKey;
  Left := Conf.GetValue(FormKey + LEFT_KEY, Left);
  Top := Conf.GetValue(FormKey + TOP_KEY, Top);
  Width := Conf.GetValue(FormKey + WIDTH_KEY, Width);
  Height := Conf.GetValue(FormKey + HEIGHT_KEY, Height);
  pnTop.Height := Conf.GetValue(FormKey + '/' + pnTop.Name + HEIGHT_KEY, pnTop.Height);
  pnTopLeft.Width := Conf.GetValue(FormKey + '/' + pnTopLeft.Name + WIDTH_KEY, pnTopLeft.Width);
end;

function TfrmInferDlg.GetHintList: THintList;
var
  I, Idx: Integer;
  s: string;
  h: TJtdInferrer.THint;
  List: specialize TGLiteVector<TJtdInferrer.THint>;
begin
  if sgHints.RowCount < 2 then exit(nil);
  for I := 1 to Pred(sgHints.RowCount) do
    begin
      s := sgHints.Cells[0, I];
      Idx := sgHints.Columns[0].PickList.IndexOf(s);
      if Idx < 0 then continue;
      h.Kind := TJtdInferrer.THintKind(Succ(Idx));
      s := sgHints.Cells[1, I];
      if s <> '' then
        begin
          Idx := sgHints.Columns[1].PickList.IndexOf(s);
          if Idx < 0 then
            h.NumberType := ntNone
          else
            h.NumberType := TJtdInferrer.TNumberType(Succ(Idx));
        end
      else
        h.NumberType := ntNone;
      h.EnumList := sgHints.Cells[2, I].Split([';'], TStringSplitOptions.ExcludeLastEmpty);
      h.Path := TJsonPtr.ToSegments(sgHints.Cells[3, I]);
      List.Add(h)
    end;
  Result := List.ToArray;
end;

function TfrmInferDlg.GetFileList: TStringArray;
begin
  Result := FSampleList.ToStringArray;
end;

procedure TfrmInferDlg.WritePosition;
var
  FormKey: string;
  Cfg: TAppConf;
  Conf: TJsonConf;
begin
  Cfg := GetAppConf;
  Conf := Cfg.Conf;
  FormKey := Cfg.GetComponentKey + '/' + ClassName;
  Conf.SetValue(FormKey + LEFT_KEY, Left);
  Conf.SetValue(FormKey + TOP_KEY, Top);
  Conf.SetValue(FormKey + WIDTH_KEY, Width);
  Conf.SetValue(FormKey + HEIGHT_KEY, Height);
  Conf.SetValue(FormKey + '/' + pnTop.Name + HEIGHT_KEY, pnTop.Height);
  Conf.SetValue(FormKey + '/' + pnTopLeft.Name + WIDTH_KEY, pnTopLeft.Width);
end;

procedure TfrmInferDlg.LoadPreview(const aRootName: string);
  procedure VisitNode(aParent: TTreeNode; aNode: TJsonNode);
  var
    TreeNode: TTreeNode;
    p: TJsonNode.TPair;
    k: TJsValueKind;
    I: Integer;
  begin
    k := aNode.Kind;
    TreeNode := nil;
    if k < jvkArray then
      begin
        case k of
          jvkNull:
            begin
              TreeNode := tvSampleStruct.Items.AddChild(aParent, 'null');
              TreeNode.ImageIndex := 1;
            end;
          jvkFalse:
            begin
              TreeNode := tvSampleStruct.Items.AddChild(aParent, 'false');
              TreeNode.ImageIndex := 2;
            end;
          jvkTrue:
            begin
              TreeNode := tvSampleStruct.Items.AddChild(aParent, 'true');
              TreeNode.ImageIndex := 2;
            end;
          jvkNumber:
            begin
              TreeNode := tvSampleStruct.Items.AddChild(aParent, aNode.ToString);
              TreeNode.ImageIndex := 3;
            end;
          jvkString:
            begin
              TreeNode := tvSampleStruct.Items.AddChild(aParent, aNode.ToString);
              TreeNode.ImageIndex := 4;
            end;
        end;
        TreeNode.SelectedIndex := TreeNode.ImageIndex + 10;
      end
    else
      case k of
        jvkArray:
          begin
            aParent.ImageIndex := 5;
            aParent.SelectedIndex := aParent.ImageIndex + 10;
            for I := 0 to Pred(aNode.Count) do
              VisitNode(tvSampleStruct.Items.AddChild(aParent, I.ToString), aNode.Items[I]);
          end;
        jvkObject:
          begin
            aParent.ImageIndex := 6;
            aParent.SelectedIndex := aParent.ImageIndex + 10;
            for p in aNode.Entries do
              begin
                TreeNode := tvSampleStruct.Items.AddChild(aParent, p.Key);
                TreeNode.ImageIndex := 9;
                TreeNode.SelectedIndex := TreeNode.ImageIndex + 10;
                VisitNode(TreeNode, p.Value);
              end;
          end;
      else
      end;
  end;
var
  Root: TTreeNode;
begin
  CurrPath := '/';
  tvSampleStruct.BeginUpdate;
  try
    tvSampleStruct.Items.Clear;
    if FCurrJson = nil then exit;
    tvSampleStruct.AutoExpand := False;
    Root := tvSampleStruct.Items.Add(nil, aRootName);
    VisitNode(Root, FCurrJson);
    tvSampleStruct.AutoExpand := True;
  finally
    tvSampleStruct.EndUpdate;
  end;
  if tvSampleStruct.CanSetFocus then
    tvSampleStruct.SetFocus;
end;

end.

