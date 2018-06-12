program autoreftest;

{$MODE OBJFPC}{$H+}

uses
  SysUtils,
  LGUtils;

type

  TMyClass = class
  private
    FCreated: Boolean;
  public
    constructor Create;
    procedure IsHere;
  end;


constructor TMyClass.Create;
begin
  FCreated := True;
end;

procedure Test1;
var
  Ref: specialize TGAutoRef<TMyClass>;
  c: TMyClass;
begin
  c := {%H-}Ref;
  c.IsHere;
end;

procedure Test2;
var
  Ref: specialize TGAutoRef<TMyClass>;
begin
  {%H-}Ref.Instance.IsHere;
end;

procedure TMyClass.IsHere;
begin
  if FCreated then
    WriteLn(' yes')
  else
    WriteLn(' no');
end;

begin
  Test1;
  Test2;
  ReadLn;
end.

