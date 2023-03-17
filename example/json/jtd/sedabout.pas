unit sedAbout;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btClose: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure Label2MouseEnter(Sender: TObject);
    procedure Label2MouseLeave(Sender: TObject);
  private

  public

  end;

var
  frmAbout: TfrmAbout;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}

uses
  LclIntf, sedAppConf, sedStrConsts;

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.FormClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  Caption := Application.Title + ' - ' + SAbout;
end;

procedure TfrmAbout.Label2Click(Sender: TObject);
begin
  OpenUrl(GetJtdHomeUrl);
end;

procedure TfrmAbout.Label2MouseEnter(Sender: TObject);
begin
  Label2.Font.Color := clBlue;
end;

procedure TfrmAbout.Label2MouseLeave(Sender: TObject);
begin
  Label2.Font.Color := clDefault;
end;

end.

