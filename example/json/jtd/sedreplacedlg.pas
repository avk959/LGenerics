unit sedReplaceDlg;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, Dialogs, sedSearchDlg, StdCtrls, ExtCtrls;

type

  { TfrmReplaceDlg }

  TfrmReplaceDlg = class(TFrmSearchDlg)
    Label2: TLabel;
    cbReplaceText: TComboBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetReplaceText: string;
    function GetReplaceTextHistory: string;
    procedure SetReplaceText(Value: string);
    procedure SetReplaceTextHistory(Value: string);
    procedure ReadPosition;
    procedure WritePosition;
  public
    property ReplaceText: string read GetReplaceText write SetReplaceText;
    property ReplaceTextHistory: string read GetReplaceTextHistory write SetReplaceTextHistory;
  end;

implementation

uses
  lgJsonCfg, sedAppConf, sedStrConsts;

{$R *.lfm}

{ TfrmReplaceDlg }

function TfrmReplaceDlg.GetReplaceText: string;
begin
  Result := cbReplaceText.Text;
end;

function TfrmReplaceDlg.GetReplaceTextHistory: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to cbReplaceText.Items.Count - 1 do begin
    if i >= 10 then
      break;
    if i > 0 then
      Result := Result + LineEnding;
    Result := Result + cbReplaceText.Items[i];
  end;
end;

procedure TfrmReplaceDlg.SetReplaceText(Value: string);
begin
  cbReplaceText.Text := Value;
end;

procedure TfrmReplaceDlg.SetReplaceTextHistory(Value: string);
begin
  cbReplaceText.Items.Text := Value;
end;

procedure TfrmReplaceDlg.ReadPosition;
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

procedure TfrmReplaceDlg.WritePosition;
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
end;

procedure TfrmReplaceDlg.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  s: string;
  i: integer;
begin
  inherited;
  if ModalResult = mrOK then begin
    s := cbReplaceText.Text;
    if s <> '' then begin
      i := cbReplaceText.Items.IndexOf(s);
      if i > -1 then begin
        cbReplaceText.Items.Delete(i);
        cbReplaceText.Items.Insert(0, s);
        cbReplaceText.Text := s;
      end else
        cbReplaceText.Items.Insert(0, s);
    end;
  end;
end;

procedure TfrmReplaceDlg.FormCreate(Sender: TObject);
begin
  Caption := Application.Title + ' - ' + SReplaceTest;
  ReadProps;
end;

procedure TfrmReplaceDlg.FormShow(Sender: TObject);
begin
  ReadPosition;
end;

procedure TfrmReplaceDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  WritePosition;
  if ModalResult = mrOk then
    begin
      opSearchBackwards := SearchBackwards;
      opSearchCaseSensitive := SearchCaseSensitive;
      opSearchFromCaret := SearchFromCursor;
      opSearchSelectionOnly := SearchInSelectionOnly;
      opSearchWholeWords := SearchWholeWords;
      opSearchRegex := SearchRegularExpression;
      gsSearchText := SearchText;
      gsSearchTextHistory := SearchTextHistory;
    end;
end;

end.

 
