unit sedSearchDlg;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus;

type

  { TfrmSearchDlg }

  TfrmSearchDlg = class(TForm)
    Label1: TLabel;
    cbSearchText: TComboBox;
    rgSearchDirection: TRadioGroup;
    gbSearchOptions: TGroupBox;
    cbSearchCaseSensitive: TCheckBox;
    cbSearchWholeWords: TCheckBox;
    cbSearchFromCursor: TCheckBox;
    cbSearchSelectedOnly: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    cbRegularExpression: TCheckBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function  GetSearchBackwards: boolean;
    function  GetSearchCaseSensitive: boolean;
    function  GetSearchFromCursor: boolean;
    function  GetSearchInSelection: boolean;
    function  GetSearchText: string;
    function  GetSearchTextHistory: string;
    function  GetSearchWholeWords: boolean;
    procedure SetSearchBackwards(aValue: boolean);
    procedure SetSearchCaseSensitive(aValue: boolean);
    procedure SetSearchFromCursor(aValue: boolean);
    procedure SetSearchInSelection(aValue: boolean);
    procedure SetSearchText(Value: string);
    procedure SetSearchTextHistory(aValue: string);
    procedure SetSearchWholeWords(aValue: boolean);
    procedure SetSearchRegularExpression(const aValue: boolean);
    function  GetSearchRegularExpression: boolean;
    procedure ReadPosition;
    procedure WritePosition;
  protected
    procedure ReadProps;
  public
    property SearchBackwards: boolean read GetSearchBackwards write SetSearchBackwards;
    property SearchCaseSensitive: boolean read GetSearchCaseSensitive write SetSearchCaseSensitive;
    property SearchFromCursor: boolean read GetSearchFromCursor write SetSearchFromCursor;
    property SearchInSelectionOnly: boolean read GetSearchInSelection write SetSearchInSelection;
    property SearchText: string read GetSearchText write SetSearchText;
    property SearchTextHistory: string read GetSearchTextHistory write SetSearchTextHistory;
    property SearchWholeWords: boolean read GetSearchWholeWords write SetSearchWholeWords;
    property SearchRegularExpression: boolean read GetSearchRegularExpression write SetSearchRegularExpression;
  end;

var
  opSearchCaseSensitive: Boolean = True;
  opSearchBackwards,
  opSearchFromCaret,
  opSearchSelectionOnly,
  opSearchTextAtCaret,
  opSearchWholeWords,
  opSearchRegex: Boolean;

  gsSearchText,
  gsSearchTextHistory,
  gsReplaceText,
  gsReplaceTextHistory: string;
  

implementation

uses
  lgJsonCfg, sedAppConf, sedStrConsts;

{$R *.lfm}

{ TfrmSearchDlg }

function TfrmSearchDlg.GetSearchBackwards: boolean;
begin
  Result := rgSearchDirection.ItemIndex = 1;
end;

function TfrmSearchDlg.GetSearchCaseSensitive: boolean;
begin
  Result := cbSearchCaseSensitive.Checked;
end;

function TfrmSearchDlg.GetSearchFromCursor: boolean;
begin
  Result := cbSearchFromCursor.Checked;
end;

function TfrmSearchDlg.GetSearchInSelection: boolean;
begin
  Result := cbSearchSelectedOnly.Checked;
end;

function TfrmSearchDlg.GetSearchRegularExpression: boolean;
begin
  Result := cbRegularExpression.Checked;
end;

procedure TfrmSearchDlg.ReadPosition;
var
  FormKey: string;
  Cfg: TAppConf;
  Conf: TJsonConf;
begin
  Cfg := GetAppConf;
  FormKey := Cfg.GetComponentKey + '/' + Self.ClassName;
  Conf := Cfg.Conf;
  if not Conf.TryOpenKey(FormKey, False) then
    begin
      Self.Position := poMainFormCenter;
      exit;
    end
  else
    Conf.CloseKey;
  Self.Left := Conf.GetValue(FormKey + LEFT_KEY, Self.Left);
  Self.Top := Conf.GetValue(FormKey + TOP_KEY, Self.Top);
  Self.Width := Conf.GetValue(FormKey + WIDTH_KEY, Self.Width);
  Self.Height := Conf.GetValue(FormKey + HEIGHT_KEY, Self.Height);
end;

procedure TfrmSearchDlg.ReadProps;
begin
  SearchBackwards := opSearchBackwards;
  SearchCaseSensitive := opSearchCaseSensitive;
  SearchFromCursor := opSearchFromCaret;
  SearchInSelectionOnly := opSearchSelectionOnly;
  SearchText := gsSearchText;
  SearchTextHistory := gsSearchTextHistory;
  SearchWholeWords := opSearchWholeWords;
  SearchRegularExpression := opSearchRegex;
end;

procedure TfrmSearchDlg.WritePosition;
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

function TfrmSearchDlg.GetSearchText: string;
begin
  Result := cbSearchText.Text;
end;

function TfrmSearchDlg.GetSearchTextHistory: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to cbSearchText.Items.Count - 1 do begin
    if i >= 10 then
      break;
    if i > 0 then
      Result := Result + LineEnding;
    Result := Result + cbSearchText.Items[i];
  end;
end;

function TfrmSearchDlg.GetSearchWholeWords: boolean;
begin
  Result := cbSearchWholeWords.Checked;
end;

procedure TfrmSearchDlg.SetSearchBackwards(aValue: boolean);
begin
  rgSearchDirection.ItemIndex := Ord(aValue);
end;

procedure TfrmSearchDlg.SetSearchCaseSensitive(aValue: boolean);
begin
  cbSearchCaseSensitive.Checked := aValue;
end;

procedure TfrmSearchDlg.SetSearchFromCursor(aValue: boolean);
begin
  cbSearchFromCursor.Checked := aValue;
end;

procedure TfrmSearchDlg.SetSearchInSelection(aValue: boolean);
begin
  cbSearchSelectedOnly.Checked := aValue;
end;

procedure TfrmSearchDlg.SetSearchText(Value: string);
begin
  cbSearchText.Text := Value;
end;

procedure TfrmSearchDlg.SetSearchTextHistory(aValue: string);
begin
  cbSearchText.Items.Text := aValue;
end;

procedure TfrmSearchDlg.SetSearchWholeWords(aValue: boolean);
begin
  cbSearchWholeWords.Checked := aValue;
end;

procedure TfrmSearchDlg.SetSearchRegularExpression( const aValue: boolean);
begin
  cbRegularExpression.Checked := aValue;
end;

procedure TfrmSearchDlg.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  s: string;
  i: integer;
begin
  if ModalResult = mrOK then
    begin
      s := cbSearchText.Text;
      if s <> '' then
        begin
          i := cbSearchText.Items.IndexOf(s);
          if i > -1 then
            begin
              cbSearchText.Items.Delete(i);
              cbSearchText.Items.Insert(0, s);
              cbSearchText.Text := s;
            end
          else
            cbSearchText.Items.Insert(0, s);
        end;
      end;
end;

procedure TfrmSearchDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
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

procedure TfrmSearchDlg.FormCreate(Sender: TObject);
begin
  Caption := Application.Title + ' - ' + SSearchText;
  ReadProps;
end;

procedure TfrmSearchDlg.FormShow(Sender: TObject);
begin
  ReadPosition;
end;

end.

 
