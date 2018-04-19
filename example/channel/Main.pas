unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LGAsync;

type

  TSize = class
  const
    Size = 64;
  end;

  TChannel = specialize TGBlockingChannel<string, TSize>;

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
    FChannel: TChannel;
    MassageNumber: Integer;
    FProducer: TSender;
    FConsumer: TReceiver;
    procedure StartSend;
    procedure StopSend;
    procedure StartReceive;
    procedure StopReceive;
    procedure Cleanup;
  public

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
  FreeOnTerminate := True;
end;

procedure TSender.Execute;
var
  I: Integer;
begin
  I := frmMain.MassageNumber;
  repeat
    FCurrData := I.ToString;
    if FChannel.Send(FCurrData) then
      begin
        Synchronize(@UpdateLabel);
        Inc(I);
        Sleep(20);
      end;
  until Terminated;
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
  FreeOnTerminate := True;
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
  FChannel := TChannel.Create;
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
  Cleanup;
  FChannel.Free;
end;

procedure TfrmMain.StartSend;
begin
  if Assigned(FProducer) then
    exit;
  FProducer := TSender.Create(FChannel);
  FProducer.Start;
end;

procedure TfrmMain.StopSend;
begin
  if Assigned(FProducer) then
    begin
      FProducer.Terminate;
      FChannel.Close;
      Sleep(10);
      FChannel.Open;
      FProducer := nil;
    end;
end;

procedure TfrmMain.StartReceive;
begin
  if Assigned(FConsumer) then
    exit;
  FConsumer := TReceiver.Create(FChannel);
  FConsumer.Start;
end;

procedure TfrmMain.StopReceive;
begin
  if Assigned(FConsumer) then
    begin
      FConsumer.Terminate;
      FChannel.Close;
      Sleep(50);
      FChannel.Open;
      FConsumer := nil;
    end;
end;

procedure TfrmMain.Cleanup;
begin
  if Assigned(FProducer) then
    FProducer.Terminate;
  if Assigned(FConsumer) then
    FConsumer.Terminate;
  FChannel.Close;
  Sleep(100);
end;

end.

