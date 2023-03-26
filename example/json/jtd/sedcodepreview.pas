unit sedCodePreview;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, SynEdit, SynHighlighterPas;

type

  { TfrmUnitPreview }

  TfrmUnitPreview = class(TForm)
    btClose: TButton;
    pnMain: TPanel;
    sedUnitPreview: TSynEdit;
    SynFreePascalSyn1: TSynFreePascalSyn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure SetPreviewText(const aValue: string);
    procedure ReadPosition;
    procedure WritePosition;
  public
    property PreviewText: string write SetPreviewText;
  end;

var
  frmUnitPreview: TfrmUnitPreview;

implementation
{$B-}{$COPERATORS ON}
uses
  sedAppConf, lgJsonCfg, sedStrConsts;

{$R *.lfm}

{ TfrmUnitPreview }

procedure TfrmUnitPreview.FormCreate(Sender: TObject);
begin
  Caption := Application.Title + ' - ' + SUnitPreview;
end;

procedure TfrmUnitPreview.FormDestroy(Sender: TObject);
begin
  WritePosition;
end;

procedure TfrmUnitPreview.FormShow(Sender: TObject);
begin
  ReadPosition;
end;

procedure TfrmUnitPreview.SetPreviewText(const aValue: string);
begin
  Screen.BeginWaitCursor;
  try
    sedUnitPreview.BeginUpdate;
    try
      sedUnitPreview.Text := aValue;
    finally
      sedUnitPreview.EndUpdate;
    end;
  finally
    Screen.EndWaitCursor;
  end;
end;

procedure TfrmUnitPreview.ReadPosition;
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

procedure TfrmUnitPreview.WritePosition;
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

end.

