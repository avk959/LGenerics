unit sedCodeGenDlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, EditBtn;

type

  { TfrmCodegenDlg }

  TfrmCodegenDlg = class(TForm)
    btOk: TButton;
    chbPasEnums: TCheckBox;
    ntCancel: TButton;
    chbComments: TCheckBox;
    edClassName: TEdit;
    edCustomHeader: TEdit;
    edUnitName: TEdit;
    fneDestFile: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lbCurrSchemaName: TLabel;
    sbtPreview: TSpeedButton;
    procedure fneDestFileButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PreviewClick(Sender: TObject);
  private
    FSchema: string;
    function  GetSchema: string;
    function  GetSchemaName: string;
    procedure ReadPosition;
    procedure SetSchema(const aValue: string);
    procedure SetSchemaName(const aValue: string);
    procedure WritePosition;
    function  GenCode: string;
  public
    property CurrSchemaName: string read GetSchemaName write SetSchemaName;
    property CurrSchema: string read GetSchema write SetSchema;
  end;

var
  frmCodegenDlg: TfrmCodegenDlg;

implementation
{$B-}{$COPERATORS ON}
uses
  FileUtil, LazFileUtils, sedAppConf, lgJsonCfg, lgJsonTypeDef, lgJtdCodegen, sedCodePreview, sedStrConsts;

{$R *.lfm}

{ TfrmCodegenDlg }

procedure TfrmCodegenDlg.FormCreate(Sender: TObject);
begin
  Caption := Application.Title + ' - ' + SGenUnit;
end;

procedure TfrmCodegenDlg.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult <> mrOk then exit;
  if fneDestFile.FileName = '' then
    CanClose := MessageDlg(Application.Title, SCodegenDlgClosePrompt, mtConfirmation, mbYesNo, '') = mrYes
  else
    with TStringStream.Create(GenCode) do
      try
        SaveToFile(fneDestFile.FileName);
      finally
        Free;
      end;
end;

procedure TfrmCodegenDlg.fneDestFileButtonClick(Sender: TObject);
begin
  if fneDestFile.FileName = '' then
    fneDestFile.FileName := edUnitName.Text;
end;

procedure TfrmCodegenDlg.FormDestroy(Sender: TObject);
begin
  WritePosition;
end;

procedure TfrmCodegenDlg.FormShow(Sender: TObject);
begin
  ReadPosition;
end;

procedure TfrmCodegenDlg.PreviewClick(Sender: TObject);
begin
  with TfrmUnitPreview.Create(nil) do
    try
      PreviewText := GenCode;
      ShowModal;
    finally
      Free;
    end;
end;

function TfrmCodegenDlg.GetSchemaName: string;
begin
  Result := lbCurrSchemaName.Caption;
end;

function TfrmCodegenDlg.GetSchema: string;
begin
  Result := FSchema;
end;

procedure TfrmCodegenDlg.ReadPosition;
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
end;

procedure TfrmCodegenDlg.SetSchema(const aValue: string);
begin
  FSchema := aValue;
end;

procedure TfrmCodegenDlg.SetSchemaName(const aValue: string);
begin
  lbCurrSchemaName.Caption := aValue;
  if aValue <> '' then
    begin
      edClassName.Text := UpCase(aValue[1]) + Copy(aValue, 2, Length(aValue));
      edUnitName.Text := edClassName.Text;
    end;
end;

procedure TfrmCodegenDlg.WritePosition;
var
  FormKey: string;
  Cfg: TAppConf;
  Conf: TJsonConf;
begin
  Cfg := GetAppConf;
  Conf := Cfg.Conf;
  FormKey := Cfg.GetComponentKey + '/' + Self.ClassName;
  Conf.SetValue(FormKey + LEFT_KEY, Self.Left);
  Conf.SetValue(FormKey + TOP_KEY, Self.Top);
  Conf.SetValue(FormKey + WIDTH_KEY, Self.Width);
  Conf.SetValue(FormKey + HEIGHT_KEY, Self.Height);
end;

function TfrmCodegenDlg.GenCode: string;
var
  Schema: TJtdSchema;
  Opts: TCodegenOptions;
begin
  Result := '';
  Screen.BeginWaitCursor;
  try
    Schema := TJtdSchema.Create;
    try
      Schema.Load(FSchema);
      Opts := [];
      if not chbComments.Checked then
        Include(Opts, cgoDisableComments);
      if not chbPasEnums.Checked then
        Include(Opts, cgoDisablePasEnums);
      with TJtdPasCodegen.Create(Schema, Opts) do
        try
          PreferRootClassName := edClassName.Text;
          UnitName := edUnitName.Text;
          CustomHeader := edCustomHeader.Text;
          Execute;
          Result := Source.Text;
        finally
          Free;
        end;
    finally
      Schema.Free;
    end;
  finally
    Screen.EndWaitCursor;
  end;
end;

end.

