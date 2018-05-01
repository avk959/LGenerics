program table_test;

{$MODE OBJFPC}{$H+}

uses
  SysUtils,
  LGTable;

type
  TTable    = specialize TGListTable2<string, string, Integer>;
  TCellData = TTable.TCellData;
  TRowData  = TTable.TRowData;
  TColData  = TTable.TColData;

const
  Data: array[0..99] of TCellData = {$I data.inc};

var
  UserInput: string;
  Table: TTable;
  RowData: TRowData;
  ColData: TColData;


  {$R *.res}

begin

  Table := TTable.Create;
  Table.AddAll(Data);

  repeat
    WriteLn;
    WriteLn('print q<quite> to exit');
    WriteLn('print 1=Company name to see list of branch offices location');
    WriteLn('print 2=Country name to see list of branch offices');
    ReadLn(UserInput);
    if Length(UserInput) > 0 then
    case LowerCase(UserInput[1]) of
      'q': break;
      '1':
        begin
          UserInput := Copy(UserInput, 3, Length(UserInput));
          WriteLn('-->');
          for RowData in Table.Rows[UserInput] do
            WriteLn('Location: ', RowData.Column, '', ', employees: ', RowData.Value);
        end;
      '2':
        begin
          UserInput := Copy(UserInput, 3, Length(UserInput));
          WriteLn('-->');
          for ColData in Table.Columns[UserInput] do
            WriteLn('Company: ', ColData.Row, '', ', employees: ', ColData.Value);
        end;
    end;
  until False;

  Table.Free;

end.

