program presdatabase;

{$mode objfpc}{$H+}

uses
  SysUtils,
  LGMultiMap;

type

  TNameList = specialize TGListMultiMap2<string, string>;

  TName = record
    FirstName,
    LastName: string;
  end;

var
  Database: TNameList;
  UserInput,
  LastName: string;
  d: TName;
  Names: array[0..44] of TName = {$I presidents.inc};

{$R *.res}

begin

  Database := TNameList.Create;
  for d in Names do
    Database.Add(LowerCase(d.FirstName), d.LastName);

  repeat
    Writeln('Print q to exit');
    Writeln('Print president first name to get list');
    Readln(UserInput);
    if LowerCase(UserInput) = 'q' then
      break;
    Writeln('>>>');
    for LastName in Database[LowerCase(UserInput)] do
      Writeln(LastName);
    Writeln('');
  until False;

  Database.Free;

end.

