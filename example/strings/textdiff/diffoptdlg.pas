unit DiffOptDlg;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DiffUtils;

type

  { TfrmDiffOptDlg }

  TfrmDiffOptDlg = class(TForm)
    btOk: TButton;
    btCancel: TButton;
    chgCompareOpts: TCheckGroup;
    rgAlgo: TRadioGroup;
  private
    function  GetAlgo: TDiffAlgo;
    function  GetDiffFlags: TDiffFlags;
    procedure SetAlgo(const aValue: TDiffAlgo);
    procedure SetDiffFlags(const aValue: TDiffFlags);

  public
    property DiffFlags: TDiffFlags read GetDiffFlags write SetDiffFlags;
    property DiffAlgo: TDiffAlgo read GetAlgo write SetAlgo;
  end;

var
  frmDiffOptDlg: TfrmDiffOptDlg;

implementation
{$B-}{$COPERATORS ON}

{$R *.lfm}

{ TfrmDiffOptDlg }

function TfrmDiffOptDlg.GetAlgo: TDiffAlgo;
begin
  if rgAlgo.ItemIndex >= 0 then
    Result := TDiffAlgo(rgAlgo.ItemIndex)
  else
    Result := TDiffAlgo.daGus;
end;

function TfrmDiffOptDlg.GetDiffFlags: TDiffFlags;
var
  I: Integer;
begin
  Result := [];
  for I := 0 to chgCompareOpts.Items.Count - 1 do
    if chgCompareOpts.Checked[I] then
      Include(Result, TDiffFlag(I));
end;

procedure TfrmDiffOptDlg.SetAlgo(const aValue: TDiffAlgo);
begin
  rgAlgo.ItemIndex := Integer(aValue);
end;

procedure TfrmDiffOptDlg.SetDiffFlags(const aValue: TDiffFlags);
var
  f: TDiffFlag;
begin
  for f in aValue do
    chgCompareOpts.Checked[Ord(f)] := True;
end;

end.

