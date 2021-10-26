{ generates file uniform.txt }
program gen_uniform;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, lgUtils, lgJson;

const
  ExpectSize = 1000*1000*5;
  GenSeed    = 42;

function  NextRandomDouble: Double;
var
  q: QWord;
  d: Double absolute q;
const
  InfExp = QWord($7ff0000000000000);
begin
  repeat
    q := BJNextRandom64;
  until q and InfExp <> InfExp;
  Result := d;
end;

procedure Run;
var
  I: Integer;
begin
  BJSetSeed64(GenSeed);
  with TStringList.Create do
    try
      for I := 1 to ExpectSize do
        Add(Double2Str(NextRandomDouble));
      SaveToFile('uniform.txt');
    finally
      Free;
    end;
end;

begin
  Run;
  WriteLn('Done.');
end.

