unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LGAsync;

type

  TChannel = specialize TGBlockChannel<string>;

  TSender = class(TThread)
  private
    FChannel: TChannel;
    FCurrData: string;
    procedure UpdateLabel;
  public
    constructor Create(aChannel: TChannel);
    procedure Execute; override;
  end;

  TReceiver = class(TThread)
  private
    FChannel: TChannel;
    FCurrData: string;
    procedure UpdateLabel;
  public
    constructor Create(aChannel: TChannel);
    procedure Execute; override;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    btStartSender: TButton;
    btStopSender: TButton;
    btStartReceiver: TButton;
    btStopReceiver: TButton;
    Label1: TLabel;
    Label2: TLabel;
    lbReceivedData: TLabel;
    lbSendedData: TLabel;
    procedure btStartReceiverClick(Sender: TObject);
    procedure btStartSenderClick(Sender: TObject);
    procedure btStopReceiverClick(Sender: TObject);
    procedure btStopSenderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  const
    CHAN_SIZE = 64;

  var
    FChannel: TChannel;
    MassageNumber: Integer;
    FSender: TSender;
    FReceiver: TReceiver;
    procedure StartSend;
    procedure StopSend;
    procedure StartReceive;
    procedure StopReceive;
  end;

var
  frmMain: TfrmMain;


implementation

{$R *.lfm}

{ TSender }

procedure TSender.UpdateLabel;
begin
  frmMain.lbSendedData.Caption := FCurrData;
end;

constructor TSender.Create(aChannel: TChannel);
begin
  inherited Create(True);
  FChannel := aChannel;
end;

procedure TSender.Execute;
var
  I: Integer;
begin
  I := frmMain.MassageNumber;
  while not Terminated do
    begin
      FCurrData := I.ToString;
      if FChannel.Send(FCurrData) then
        begin
          Synchronize(@UpdateLabel);
          Inc(I);
          Sleep(20);
        end;
    end;
  frmMain.MassageNumber := I;
end;

{ TReceiver }

procedure TReceiver.UpdateLabel;
begin
  frmMain.lbReceivedData.Caption := FCurrData;
end;

constructor TReceiver.Create(aChannel: TChannel);
begin
  inherited Create(True);
  FChannel := aChannel;
end;

procedure TReceiver.Execute;
begin
  while not Terminated do
    if FChannel.Receive(FCurrData) then
      begin
        Synchronize(@UpdateLabel);
        Sleep(50);
      end;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := 'Channel test';
  MassageNumber := 1;
  FChannel := TChannel.Create(CHAN_SIZE);
end;

procedure TfrmMain.btStartSenderClick(Sender: TObject);
begin
  StartSend;
end;

procedure TfrmMain.btStopReceiverClick(Sender: TObject);
begin
  StopReceive;
end;

procedure TfrmMain.btStartReceiverClick(Sender: TObject);
begin
  StartReceive;
end;

procedure TfrmMain.btStopSenderClick(Sender: TObject);
begin
  StopSend;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  StopSend;
  StopReceive;
  FChannel.Free;
end;

procedure TfrmMain.StartSend;
begin
  if not Assigned(FSender) then
    begin
      FSender := TSender.Create(FChannel);
      FSender.Start;
    end;
end;

procedure TfrmMain.StopSend;
begin
  if Assigned(FSender) then
    begin
      FSender.Terminate;
      FChannel.Close;
      FSender.WaitFor;
      FreeAndNil(FSender);
      FChannel.Open;
    end;
end;

procedure TfrmMain.StartReceive;
begin
  if not Assigned(FReceiver) then
    begin
      FReceiver := TReceiver.Create(FChannel);
      FReceiver.Start;
    end;
end;

procedure TfrmMain.StopReceive;
begin
  if Assigned(FReceiver) then
    begin
      FReceiver.Terminate;
      FChannel.Close;
      FReceiver.WaitFor;
      FreeAndNil(FReceiver);
      FChannel.Open;
    end;
end;

end.

