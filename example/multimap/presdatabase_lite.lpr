program presdatabase_lite;

{$mode objfpc}{$H+}

uses
  SysUtils,
  lgMultiMap;

type

  TNameList = specialize TGLiteHashMultiMap<string, string, string>;

  TName = record
    FirstName,
    LastName: string;
  end;

var
  Database: TNameList;
  UserInput,
  LastName: string;
  Name: TName;
  Names: array[0..44] of TName = {$I presidents.inc};

{$R *.res}

begin

  for Name in Names do
    Database.Add(LowerCase(Name.FirstName), Name.LastName);

  repeat
    Writeln('Enter q to exit');
    Writeln('Enter president first name to get list');
    Readln(UserInput);
    if LowerCase(UserInput) = 'q' then
      break;
    Writeln('>>>');
    for LastName in Database[LowerCase(UserInput)] do
      Writeln(LastName);
    Writeln;
  until False;

end.

