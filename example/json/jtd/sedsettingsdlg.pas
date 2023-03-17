unit sedSettingsDlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, ColorBox,
  Spin, SynEdit, lgJson, lgJsonCfg, sedAppConf, SynEditMarkupSpecialLine;

type

  { TfrmSettingsDlg }

  TfrmSettingsDlg = class(TForm)
    btCancel: TButton;
    btOk: TButton;
    btShowDialog: TButton;
    chbBom: TCheckBox;
    chbReservBold: TCheckBox;
    chbAttriBold: TCheckBox;
    chbCustKeyBold: TCheckBox;
    chbSymbolBold: TCheckBox;
    chbReservItalic: TCheckBox;
    chbAttriItalic: TCheckBox;
    chbCustKeyItalic: TCheckBox;
    chbSymbolItalic: TCheckBox;
    chbKeywordBold: TCheckBox;
    chbIdentBold: TCheckBox;
    chbKeywordItalic: TCheckBox;
    chbIdentItalic: TCheckBox;
    chbFormatted: TCheckBox;
    chgFmtOptions: TCheckGroup;
    clbCurrLine: TColorBox;
    clbErrorLine: TColorBox;
    clbReserved: TColorBox;
    clbAttri: TColorBox;
    clbCustomKey: TColorBox;
    clbSuccessColor: TColorBox;
    clbSymbol: TColorBox;
    clbKeyword: TColorBox;
    clbIdentifier: TColorBox;
    clbLineNumberColor: TColorBox;
    cbLineBreak: TComboBox;
    edFontName: TEdit;
    FD: TFontDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    pnTop: TPanel;
    pnPreview: TPanel;
    pnMain: TPanel;
    pnButtons: TPanel;
    sedCharSpace: TSpinEdit;
    sedLineSpace: TSpinEdit;
    sedNthNumber: TSpinEdit;
    sedRightEdge: TSpinEdit;
    sePreview: TSynEdit;
    sedFontSize: TSpinEdit;
    sedOnelineArrayBound: TSpinEdit;
    sedIndentAfterColon: TSpinEdit;
    sedIndentBeforeColon: TSpinEdit;
    sedIndentAfterComma: TSpinEdit;
    sedIndent: TSpinEdit;
    sedOnelineObjectBound: TSpinEdit;
    spedMaxRecent: TSpinEdit;
    spltExample: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    procedure btShowDialogClick(Sender: TObject);
    procedure chbFontStyleChange(Sender: TObject);
    procedure chbFormattedChange(Sender: TObject);
    procedure chgFmtOptionsItemClick(Sender: TObject; Index: integer);
    procedure cbLineBreakChange(Sender: TObject);
    procedure clbCurrLineChange(Sender: TObject);
    procedure clbErrorLineChange(Sender: TObject);
    procedure clbLineNumberColorChange(Sender: TObject);
    procedure clbColorChange(Sender: TObject);
    procedure clbSuccessColorChange(Sender: TObject);
    procedure edFontNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sedAnyChange(Sender: TObject);
    procedure sedCharSpaceChange(Sender: TObject);
    procedure sedLineSpaceChange(Sender: TObject);
    procedure sedNthNumberChange(Sender: TObject);
    procedure sedRightEdgeChange(Sender: TObject);
    procedure sedFontSizeChange(Sender: TObject);
    procedure sePreviewSpecialLineColors(Sender: TObject; Line: integer; var Special: boolean; var FG,
      BG: TColor);
    procedure spedMaxRecentChange(Sender: TObject);
  private
    FFormatStyle: TJsonFormatStyle;
    FModified,
    FReading: Boolean;
    procedure FillCombo;
    procedure ReadConfig;
    procedure ReadPosition(aCfg: TAppConf);
    procedure ReadSettings(aCfg: TAppConf);
    procedure ReadDisplayOpts(aCfg: TAppConf);
    procedure ReadFormatOpts(aCfg: TAppConf);
    procedure ReadMiscOpts(aCfg: TAppConf);
    procedure OptionsChanged;
    procedure UpdateFormatControls;
    procedure UpdatePreview;
    procedure WriteConfig;
    procedure WritePosition(aCfg: TAppConf);
    procedure WriteSettings(aCfg: TAppConf);
    procedure WriteDisplayStyle(aCfg: TAppConf);
    procedure WriteFormatStyle(aCfg: TAppConf);
    procedure WriteMiscOpts(aCfg: TAppConf);
  public
    property FormatStyle: TJsonFormatStyle read FFormatStyle;
  end;

var
  frmSettingsDlg: TfrmSettingsDlg;

implementation
{$B-}{$J-}{$COPERATORS ON}{$POINTERMATH ON}
uses
  TypInfo, SynEditHighlighter, SynGutterLineNumber, SynHighlighterJTD, lgUtils, sedStrConsts;

{$R *.lfm}

{ TfrmSettingsDlg }

procedure TfrmSettingsDlg.FormCreate(Sender: TObject);
var
  Node: TJsonNode;
begin
  Caption := Application.Title + ' - ' + SSettings;
  sePreview.Highlighter := TSynJTDSyn.Create(Self);
  if TJsonNode.TryParse(sePreview.Highlighter.SampleSource, Node) then begin
    TSynJTDSyn(sePreview.Highlighter).NewDoc(Node);
    Node.Free;
  end;
  FFormatStyle := DefaultJsonFmtStyle;
  FillCombo;
end;

procedure TfrmSettingsDlg.chgFmtOptionsItemClick(Sender: TObject; Index: integer);
begin
  if FReading then exit;
  if chgFmtOptions.Checked[Index] then
    Include(FFormatStyle.Options, TJsFormatOption(Index))
  else
    Exclude(FFormatStyle.Options, TJsFormatOption(Index));
  btOk.Enabled := True;
  OptionsChanged;
end;

procedure TfrmSettingsDlg.btShowDialogClick(Sender: TObject);
begin
  FD.Font.Name := edFontName.Text;
  if not FD.Execute then exit;
  edFontName.Text := FD.Font.Name;
  edFontNameChange(Sender);
end;

procedure TfrmSettingsDlg.chbFontStyleChange(Sender: TObject);
  procedure ChangeAttri(Attri: TSynHighlighterAttributes; aEl: TFontStyle; aOn: Boolean); inline;
  begin
    if aOn then
      Attri.Style := Attri.Style + [aEl]
    else
      Attri.Style := Attri.Style - [aEl];
  end;
var
  Hl: TSynJTDSyn;
begin
  if FReading then exit;
  Hl := TSynJTDSyn(sePreview.Highlighter);
  if Sender = chbReservBold then
    ChangeAttri(Hl.ReservedAttri, fsBold, chbReservBold.Checked)
  else
    if Sender = chbAttriBold then
      ChangeAttri(Hl.AttributeAttri, fsBold, chbAttriBold.Checked)
    else
      if Sender = chbCustKeyBold then
        ChangeAttri(Hl.CustomKeyAttri, fsBold, chbCustKeyBold.Checked)
      else
        if Sender = chbSymbolBold then
          ChangeAttri(Hl.SymbolAttri, fsBold, chbSymbolBold.Checked)
        else
          if Sender = chbReservItalic then
            ChangeAttri(Hl.ReservedAttri, fsItalic, chbReservItalic.Checked)
          else
            if Sender = chbAttriItalic then
              ChangeAttri(Hl.AttributeAttri, fsItalic, chbAttriItalic.Checked)
            else
              if Sender = chbCustKeyItalic then
                ChangeAttri(Hl.CustomKeyAttri, fsItalic, chbCustKeyItalic.Checked)
              else
                if Sender = chbSymbolItalic then
                  ChangeAttri(Hl.SymbolAttri, fsItalic, chbSymbolItalic.Checked)
                else
                  if Sender = chbKeywordBold then
                    ChangeAttri(Hl.KeyAttri, fsBold, chbKeywordBold.Checked)
                  else
                    if Sender = chbIdentBold then
                      ChangeAttri(Hl.IdentAttri, fsBold, chbIdentBold.Checked)
                    else
                      if Sender = chbKeywordItalic then
                        ChangeAttri(Hl.KeyAttri, fsItalic, chbKeywordItalic.Checked)
                      else
                        if Sender = chbIdentItalic then
                          ChangeAttri(Hl.IdentAttri, fsItalic, chbIdentItalic.Checked)
                        else
                          exit;
  FModified := True;
  OptionsChanged;
end;

procedure TfrmSettingsDlg.chbFormattedChange(Sender: TObject);
begin
  if FReading then exit;
  FModified := True;
end;

procedure TfrmSettingsDlg.cbLineBreakChange(Sender: TObject);
begin
  if FReading then exit;
  FFormatStyle.LineBreak := TJsLineBreak(cbLineBreak.ItemIndex);
  FModified:= True;
end;

procedure TfrmSettingsDlg.clbCurrLineChange(Sender: TObject);
begin
  if FReading then exit;
  sePreview.LineHighlightColor.Background := clbCurrLine.Selected;
  FModified := True;
  OptionsChanged;
end;

procedure TfrmSettingsDlg.clbErrorLineChange(Sender: TObject);
begin
  if FReading then exit;
  FModified := True;
  sePreview.Invalidate;
end;

procedure TfrmSettingsDlg.clbLineNumberColorChange(Sender: TObject);
begin
  if FReading then exit;
  TSynGutterLineNumber(sePreview.Gutter.Parts[1]).MarkupInfo.Foreground := clbLineNumberColor.Selected;
  FModified := True;
  OptionsChanged;
end;

procedure TfrmSettingsDlg.clbColorChange(Sender: TObject);
var
  Hl: TSynJTDSyn;
begin
  if FReading then exit;
  Hl := TSynJTDSyn(sePreview.Highlighter);
  if Sender = clbKeyword then
    Hl.KeyAttri.Foreground := clbKeyword.Selected
  else
    if Sender = clbIdentifier then
      Hl.IdentAttri.Foreground := clbIdentifier.Selected
    else
      if Sender = clbReserved then
        Hl.ReservedAttri.Foreground := clbReserved.Selected
      else
        if Sender = clbAttri then
          Hl.AttributeAttri.Foreground := clbAttri.Selected
        else
          if Sender = clbCustomKey then
            Hl.CustomKeyAttri.Foreground := clbCustomKey.Selected
          else
            if Sender = clbSymbol then
              Hl.SymbolAttri.Foreground := clbSymbol.Selected
            else
              exit;
  FModified := True;
  OptionsChanged;
end;

procedure TfrmSettingsDlg.clbSuccessColorChange(Sender: TObject);
begin
  FModified := True;
end;

procedure TfrmSettingsDlg.edFontNameChange(Sender: TObject);
begin
  if FReading then exit;
  sePreview.Font.Name := edFontName.Text;
  FModified := True;
  OptionsChanged;
end;

procedure TfrmSettingsDlg.FormDestroy(Sender: TObject);
begin
  WriteConfig;
end;

procedure TfrmSettingsDlg.FormShow(Sender: TObject);
begin
  ReadConfig;
end;

procedure TfrmSettingsDlg.sedAnyChange(Sender: TObject);
begin
  if FReading then exit;
  if Sender = sedIndent then
    FFormatStyle.IndentSize := sedIndent.Value
  else
    if Sender = sedIndentAfterComma then
      FFormatStyle.IndentAfterComma := sedIndentAfterComma.Value
    else
      if Sender = sedIndentBeforeColon then
        FFormatStyle.IndentBeforeColon := sedIndentBeforeColon.Value
      else
        if Sender = sedIndentAfterColon then
          FFormatStyle.IndentAfterColon := sedIndentAfterColon.Value
        else
          if Sender = sedOnelineArrayBound then
            FFormatStyle.OneLineArrayBound := sedOnelineArrayBound.Value
          else
            if Sender = sedOnelineObjectBound then
              FFormatStyle.OneLineObjectBound := sedOnelineObjectBound.Value
            else
              exit;
  FModified := True;
  OptionsChanged;
end;

procedure TfrmSettingsDlg.sedCharSpaceChange(Sender: TObject);
begin
  if FReading then exit;
  sePreview.ExtraCharSpacing := sedCharSpace.Value;
  FModified := True;
  OptionsChanged;
end;

procedure TfrmSettingsDlg.sedLineSpaceChange(Sender: TObject);
begin
  if FReading then exit;
  sePreview.ExtraLineSpacing := sedLineSpace.Value;
  FModified := True;
  OptionsChanged;
end;

procedure TfrmSettingsDlg.sedNthNumberChange(Sender: TObject);
begin
  if FReading then exit;
  TSynGutterLineNumber(sePreview.Gutter.Parts[1]).ShowOnlyLineNumbersMultiplesOf := sedNthNumber.Value;
  FModified := True;
  OptionsChanged;
end;

procedure TfrmSettingsDlg.sedRightEdgeChange(Sender: TObject);
begin
  if FReading then exit;
  sePreview.RightEdge := sedRightEdge.Value;
  FModified := True;
  OptionsChanged;
end;

procedure TfrmSettingsDlg.sedFontSizeChange(Sender: TObject);
begin
  if FReading then exit;
  sePreview.Font.Size := sedFontSize.Value;
  FModified := True;
  OptionsChanged;
end;

procedure TfrmSettingsDlg.sePreviewSpecialLineColors(Sender: TObject; Line: integer; var Special: boolean;
  var FG, BG: TColor);
begin
  if Line = 6 then
    begin
      Special := True;
      BG := clbErrorLine.Selected;
    end;
end;

procedure TfrmSettingsDlg.spedMaxRecentChange(Sender: TObject);
begin
  if FReading then exit;
  FModified := True;
end;

procedure TfrmSettingsDlg.FillCombo;
var
  lb: TJsLineBreak;
begin
  cbLineBreak.Items.Clear;
  for lb in TJsLineBreak do
    cbLineBreak.Items.Add(GetEnumName(TypeInfo(lb), Integer(lb)));
  cbLineBreak.ItemIndex := -1;
  cbLineBreak.OnChange := @cbLineBreakChange;
end;

procedure TfrmSettingsDlg.ReadConfig;
var
  Config: TAppConf;
begin
  Config := GetAppConf;
  FReading := True;
  ReadPosition(Config);
  ReadSettings(Config);
  UpdateFormatControls;
  UpdatePreview;
  OptionsChanged;
  FReading := False;
end;

procedure TfrmSettingsDlg.ReadPosition(aCfg: TAppConf);
var
  FormKey: string;
  Conf: TJsonConf;
begin
  FormKey := aCfg.GetComponentKey + '/' + ClassName;
  Conf := aCfg.Conf;
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
  pnPreview.Height := Conf.GetValue(FormKey + '/' + pnPreview.Name + HEIGHT_KEY, pnPreview.Height);
end;

procedure TfrmSettingsDlg.ReadSettings(aCfg: TAppConf);
begin
  ReadDisplayOpts(aCfg);
  ReadFormatOpts(aCfg);
  ReadMiscOpts(aCfg);
end;

procedure TfrmSettingsDlg.ReadDisplayOpts(aCfg: TAppConf);
var
  Style: TDisplayStyle;
begin
  Style := aCfg.DisplayStyle;
  edFontName.Text := Style.SynFontName;
  sedFontSize.Value := Style.SynFontSize;

  sedLineSpace.Value := Style.SynExtraLineSpace;
  sedCharSpace.Value := Style.SynExtraCharSpace;
  sedRightEdge.Value := Style.SynRightEdge;
  sedNthNumber.Value := Style.SynShowEveryNthNumber;
  clbLineNumberColor.Selected := Style.SynLineNumberColor;
  clbCurrLine.Selected := Style.SynCurrLineBackground;
  clbErrorLine.Selected := Style.SynErrorLineBackground;

  clbKeyword.Selected := Style.SynKeyAttri.Foreground;
  chbKeywordBold.Checked := Style.SynKeyAttri.Bold;
  chbKeywordItalic.Checked := Style.SynKeyAttri.Italic;

  clbAttri.Selected := Style.SynAttributeAttri.Foreground;
  chbAttriBold.Checked := Style.SynAttributeAttri.Bold;
  chbAttriItalic.Checked := Style.SynAttributeAttri.Italic;

  clbReserved.Selected := Style.SynReservedAttri.Foreground;
  chbReservBold.Checked := Style.SynReservedAttri.Bold;
  chbReservItalic.Checked := Style.SynReservedAttri.Italic;

  clbSymbol.Selected := Style.SynSymbolAttri.Foreground;
  chbSymbolBold.Checked := Style.SynSymbolAttri.Bold;
  chbSymbolItalic.Checked := Style.SynSymbolAttri.Italic;

  clbCustomKey.Selected := Style.SynCustomKeyAttri.Foreground;
  chbCustKeyBold.Checked := Style.SynCustomKeyAttri.Bold;
  chbCustKeyItalic.Checked := Style.SynCustomKeyAttri.Italic;

  clbIdentifier.Selected := Style.SynIdentAttri.Foreground;
  chbIdentBold.Checked := Style.SynIdentAttri.Bold;
  chbIdentItalic.Checked := Style.SynIdentAttri.Italic;
end;

procedure TfrmSettingsDlg.ReadFormatOpts(aCfg: TAppConf);
begin
  FFormatStyle := aCfg.JsonFormatStyle;
end;

procedure TfrmSettingsDlg.ReadMiscOpts(aCfg: TAppConf);
var
  ms: TMiscSettings;
begin
  ms := aCfg.MiscSettings;
  spedMaxRecent.Value := ms.MaxRecentFiles;
  clbSuccessColor.Selected := ms.SuccessColor;
  chbFormatted.Checked := ms.SaveFormatted;
  chbBom.Checked := ms.AllowBom;
end;

procedure TfrmSettingsDlg.OptionsChanged;
var
  Node: TJsonNode;
begin
  if TJsonNode.TryParse(sePreview.Highlighter.SampleSource, Node) then
    begin
      sePreview.Text := Node.FormatJson(FFormatStyle);
      Node.Free;
    end
  else
    sePreview.Text := sePreview.Highlighter.SampleSource;
end;

procedure TfrmSettingsDlg.UpdateFormatControls;
var
  Opt: TJsFormatOption;
begin
  for Opt in FFormatStyle.Options do
    chgFmtOptions.Checked[Integer(Opt)] := True;
  sedIndent.Value := FFormatStyle.IndentSize;
  sedIndentAfterComma.Value := FFormatStyle.IndentAfterComma;
  sedIndentBeforeColon.Value := FFormatStyle.IndentBeforeColon;
  sedIndentAfterColon.Value := FFormatStyle.IndentAfterColon;
  sedOnelineArrayBound.Value := FFormatStyle.OneLineArrayBound;
  sedOnelineObjectBound.Value := FFormatStyle.OneLineObjectBound;
  cbLineBreak.ItemIndex := Integer(FFormatStyle.LineBreak);
end;

procedure TfrmSettingsDlg.UpdatePreview;
  procedure ChangeStyle(Attri: TSynHighlighterAttributes; aEl: TFontStyle; aOn: Boolean); inline;
  begin
    if aOn then
      Attri.Style := Attri.Style + [aEl]
    else
      Attri.Style := Attri.Style - [aEl];
  end;
var
  Hl: TSynJTDSyn;
begin
  sePreview.BeginUpdate;
  try
    Hl := TSynJTDSyn(sePreview.Highlighter);
    sePreview.Font.Name := edFontName.Text;
    sePreview.Font.Size := sedFontSize.Value;

    sePreview.ExtraLineSpacing := sedLineSpace.Value;
    sePreview.ExtraCharSpacing := sedCharSpace.Value;
    sePreview.RightEdge := sedRightEdge.Value;
    TSynGutterLineNumber(sePreview.Gutter.Parts[1]).ShowOnlyLineNumbersMultiplesOf := sedNthNumber.Value;
    TSynGutterLineNumber(sePreview.Gutter.Parts[1]).MarkupInfo.Foreground := clbLineNumberColor.Selected;
    sePreview.LineHighlightColor.Background := clbCurrLine.Selected;

    Hl.KeyAttri.Foreground := clbKeyword.Selected;
    ChangeStyle(Hl.KeyAttri, fsBold, chbKeywordBold.Checked);
    ChangeStyle(Hl.KeyAttri, fsItalic, chbKeywordItalic.Checked);

    Hl.AttributeAttri.Foreground := clbAttri.Selected;
    ChangeStyle(Hl.AttributeAttri, fsBold, chbAttriBold.Checked);
    ChangeStyle(Hl.AttributeAttri, fsItalic, chbAttriItalic.Checked);

    Hl.ReservedAttri.Foreground := clbReserved.Selected;
    ChangeStyle(Hl.ReservedAttri, fsBold, chbReservBold.Checked);
    ChangeStyle(Hl.ReservedAttri, fsItalic, chbReservItalic.Checked);

    Hl.SymbolAttri.Foreground := clbSymbol.Selected;
    ChangeStyle(Hl.SymbolAttri, fsBold, chbSymbolBold.Checked);
    ChangeStyle(Hl.SymbolAttri, fsItalic, chbSymbolItalic.Checked);

    Hl.CustomKeyAttri.Foreground := clbCustomKey.Selected;
    ChangeStyle(Hl.CustomKeyAttri, fsBold, chbCustKeyBold.Checked);
    ChangeStyle(Hl.CustomKeyAttri, fsItalic, chbCustKeyItalic.Checked);

    Hl.IdentAttri.Foreground := clbIdentifier.Selected;
    ChangeStyle(Hl.IdentAttri, fsBold, chbIdentBold.Checked);
    ChangeStyle(Hl.IdentAttri, fsItalic, chbIdentItalic.Checked);
  finally
    sePreview.EndUpdate;
  end;
end;

procedure TfrmSettingsDlg.WriteConfig;
var
  Config: TAppConf;
begin
  Config := GetAppConf;
  WritePosition(Config);
  if (ModalResult = mrOk) and FModified then
    WriteSettings(Config);
end;

procedure TfrmSettingsDlg.WritePosition(aCfg: TAppConf);
var
  FormKey: string;
  Conf: TJsonConf;
begin
  Conf := aCfg.Conf;
  FormKey := aCfg.GetComponentKey + '/' + ClassName;
  Conf.SetValue(FormKey + LEFT_KEY, Left);
  Conf.SetValue(FormKey + TOP_KEY, Top);
  Conf.SetValue(FormKey + WIDTH_KEY, Width);
  Conf.SetValue(FormKey + HEIGHT_KEY, Height);
  Conf.SetValue(FormKey + '/' + pnPreview.Name + HEIGHT_KEY, pnPreview.Height);
end;

procedure TfrmSettingsDlg.WriteSettings(aCfg: TAppConf);
begin
  WriteDisplayStyle(aCfg);
  WriteFormatStyle(aCfg);
  WriteMiscOpts(aCfg);
end;

procedure TfrmSettingsDlg.WriteDisplayStyle(aCfg: TAppConf);
var
  Style: TDisplayStyle;
  Hl: TSynJTDSyn;
begin

  Style.SynFontName := edFontName.Text;
  Style.SynFontSize := sedFontSize.Value;

  Style.SynExtraLineSpace := sePreview.ExtraLineSpacing;
  Style.SynExtraCharSpace := sePreview.ExtraCharSpacing;
  Style.SynRightEdge := sePreview.RightEdge;
  Style.SynShowEveryNthNumber := TSynGutterLineNumber(sePreview.Gutter.Parts[1]).ShowOnlyLineNumbersMultiplesOf;
  Style.SynLineNumberColor := TSynGutterLineNumber(sePreview.Gutter.Parts[1]).MarkupInfo.Foreground;
  Style.SynCurrLineBackground := sePreview.LineHighlightColor.Background;
  Style.SynErrorLineBackground := clbErrorLine.Selected;

  Hl := TSynJTDSyn(sePreview.Highlighter);

  Style.SynKeyAttri := TAttriStyle.Make(Hl.KeyAttri);
  Style.SynAttributeAttri := TAttriStyle.Make(Hl.AttributeAttri);
  Style.SynReservedAttri := TAttriStyle.Make(Hl.ReservedAttri);
  Style.SynSymbolAttri := TAttriStyle.Make(Hl.SymbolAttri);
  Style.SynCustomKeyAttri := TAttriStyle.Make(Hl.CustomKeyAttri);
  Style.SynIdentAttri := TAttriStyle.Make(Hl.IdentAttri);

  aCfg.DisplayStyle := Style;
end;

procedure TfrmSettingsDlg.WriteFormatStyle(aCfg: TAppConf);
begin
  aCfg.JsonFormatStyle := FFormatStyle;
end;

procedure TfrmSettingsDlg.WriteMiscOpts(aCfg: TAppConf);
var
  ms: TMiscSettings;
begin
  ms.MaxRecentFiles := spedMaxRecent.Value;
  ms.SuccessColor := clbSuccessColor.Selected;
  ms.SaveFormatted := chbFormatted.Checked;
  ms.AllowBom := chbBom.Checked;
  aCfg.MiscSettings := ms;
end;

end.

