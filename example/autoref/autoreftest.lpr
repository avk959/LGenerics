program autoreftest;

{$MODE OBJFPC}{$H+}

uses
  SysUtils,
  lgUtils;

type

  TMyClass = class
  private
  class var
    Counter: Integer;
  var
    FNumber: Integer;
    class constructor Init;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Show;
    property Number: Integer read FNumber;
  end;

class constructor TMyClass.Init;
begin
  Counter := 1;
end;

constructor TMyClass.Create;
begin
  FNumber := Counter;
  Inc(Counter);
  WriteLn('Number ', Number, ' created');
end;

destructor TMyClass.Destroy;
begin
  WriteLn('Number ', Number, ' destroyed');
  inherited;
end;

procedure TMyClass.Show;
begin
  WriteLn('Number ', Number, ' here')
end;

procedure Show(aInst: TMyClass);
begin
  aInst.Show;
end;

procedure Test1;
var
  Ref: specialize TGAutoRef<TMyClass>;
begin
  Show(Ref);
end;

procedure Test2;
var
  Ref: specialize TGAutoRef<TMyClass>;
begin
  TMyClass(Ref).Show;
end;

procedure Test3;
var
  Ref: specialize TGAutoRef<TMyClass>;
  c: TMyClass = nil;
begin
  c := {%H-}Ref;
  c.Show;
end;

var
  Inst: TMyClass = nil;

procedure Test4;
var
  Ref: specialize TGAutoRef<TMyClass>;
begin
  WriteLn('Check if Ref has instance: ', {%H-}Ref.HasInstance);
  WriteLn('Let it create instance:');
  TMyClass(Ref).Show;
  WriteLn('We want to take Ref''s instance');
  Inst := Ref.ReleaseInstance;
  WriteLn('Check if Ref has an instance: ', Ref.HasInstance);
  Ref.Clear;
  WriteLn('Check if we have an instance: ');
  Inst.Show;
end;

begin
  Test1;
  Test3;
  Test2;
  Test4;
  WriteLn('Free the instance yourself:');
  Inst.Free;
  ReadLn;
end.

