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
    destructor Destroy; override;
    procedure IsHere;
  end;


constructor TMyClass.Create;
begin
  FCreated := True;
end;

destructor TMyClass.Destroy;
begin
  WriteLn(' destroyed');
  inherited;
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
  if (Self <> nil) and FCreated then
    WriteLn(' created')
  else
    WriteLn(' error!');
end;

begin
  Test1;
  Test2;
  ReadLn;
end.

