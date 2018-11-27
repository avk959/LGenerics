program table_test;

{$MODE OBJFPC}{$H+}

uses
  SysUtils,
  LGTable2D;

type
  TTable    = specialize TGListTable2D2<string, string, Integer>;

const
  Data: array[0..99] of TTable.TCellData = {$I data.inc};

var
  UserInput: string;
  Table: TTable;
  RowData: TTable.TRowData;
  ColData: TTable.TColData;


  {$R *.res}

begin

  Table := TTable.Create;
  Table.AddCells(Data);

  repeat
    WriteLn;
    WriteLn('Enter q[uite] to exit');
    WriteLn('Enter 1=company name to get list of branch offices locations');
    WriteLn('Enter 2=country to get list of branch offices');
    ReadLn(UserInput);
    if Length(UserInput) > 0 then
    case LowerCase(UserInput[1]) of
      'q': break;
      '1':
        begin
          UserInput := Copy(UserInput, 3, Length(UserInput));
          WriteLn('>>>');
          for RowData in Table.RowMaps[UserInput] do
            WriteLn('Location: ', RowData.Column, ', employees: ', RowData.Value);
        end;
      '2':
        begin
          UserInput := Copy(UserInput, 3, Length(UserInput));
          WriteLn('>>>');
          for ColData in Table.Columns[UserInput] do
            WriteLn('Company: ', ColData.Row, ', employees: ', ColData.Value);
        end;
    end;
  until False;

  Table.Free;

end.

