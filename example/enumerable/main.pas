unit Main;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Spin, Menus,
  LGUtils,
  LGVector,
  LGHelpers,
  LGFunction;

type

  TEmployee = record
  private
  const
    PrintFmt = '%d'#9'%s'#9'%s'#9'%s'#9'%d'#9'%s'#9'%s'#9'%m';
  public
    ID: DWord;
    FirstName,
    LastName,
    Gender: string;
    Age: ShortInt;
    Location,
    Phone: string;
    Salary: Currency;
    function ToString: string;
  end;

  TEmployeeCompare = specialize TGOnCompare<TEmployee>;

  TEmployeeVector  = specialize TGVector<TEmployee>;

  { TfrmMain }

  TfrmMain = class(TForm)
    cbSelectByGenderAge: TButton;
    btFindNumbers: TButton;

    btExecSimple: TButton;
    btExec2: TButton;
    btSelectByLocation: TButton;
    btMinMaxSelected: TButton;
    btSort: TButton;
    btMaxByLocation: TButton;
    cbCmpKind: TComboBox;
    cbLocation: TComboBox;
    cbGender: TComboBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    miShowAll: TMenuItem;
    mmResult: TMemo;
    seLimit: TSpinEdit;
    seLoAge: TSpinEdit;
    seLimit2: TSpinEdit;
    seSkip: TSpinEdit;
    seHiAge: TSpinEdit;
    seSkip2: TSpinEdit;

    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    pcQueries: TPageControl;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    StaticText6: TStaticText;
    StaticText7: TStaticText;
    StaticText8: TStaticText;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miShowAllClick(Sender: TObject);
    procedure btExecSimpleClick(Sender: TObject);
    procedure btExec2Click(Sender: TObject);
    procedure btFindNumbersClick(Sender: TObject);
    procedure btSelectByLocationClick(Sender: TObject);
    procedure btMinMaxSelectedClick(Sender: TObject);
    procedure btSortClick(Sender: TObject);
    procedure btMaxByLocationClick(Sender: TObject);
    procedure cbSelectByGenderAgeClick(Sender: TObject);
  private
    FEmployees: TEmployeeVector;
    FComparators: array[0..5] of TEmployeeCompare;
    function  StringCmp(constref L, R: string): SizeInt;
    function  CmpByFirst(constref L, R: TEmployee): SizeInt;
    function  CmpByLast(constref L, R: TEmployee): SizeInt;
    function  CmpByAge(constref L, R: TEmployee): SizeInt;
    function  CmpByLocation(constref L, R: TEmployee): SizeInt;
    function  CmpByPhone(constref L, R: TEmployee): SizeInt;
    function  CmpBySalary(constref L, R: TEmployee): SizeInt;
    procedure FillcbCmpKind;
    procedure FillcbLocation;
    procedure FillcbGender;
    function  GetLocation(constref e: TEmployee): string;
    function  GenderIsMale(constref e: TEmployee): Boolean;

    procedure ShowAllRecords;
    procedure ExecSimpleQuery(SkipCount, LimitCount: SizeInt; aCmp: TEmployeeCompare);
    procedure ExecQuery2(SkipCount, LimitCount: SizeInt; aCmp: TEmployeeCompare);
    procedure SelectByLocation(const aLocation: string);
    procedure FindNumbersByLocation;
    procedure SelectMinMaxByLocation(const aLocation: string; aCmp: TEmployeeCompare);
    procedure Sort(const aCaption: string; aCmp: TEmployeeCompare);
    procedure SelectMaxByLocation(const aCaption: string; aCmp: TEmployeeCompare);
    procedure SelectGenderAndAge(const aGender: string; LoAge, HiAge: Integer);
  public

  end;

var
  frmMain: TfrmMain;
  EmployeeArray: array[0..199] of TEmployee = ({$I data.inc});

implementation

{$R *.lfm}

{ TEmployee }

function TEmployee.ToString: string;
begin
  if ID > 0 then
    Result := Format(PrintFmt, [ID, FirstName, LastName, Gender, Age, Location, Phone, Salary])
  else  // Default value
    Result := 'Null';
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FEmployees := TEmployeeVector.Create(EmployeeArray);
  FillcbCmpKind;
  FillcbLocation;
  FillcbGender;
  Caption := 'Employee enumerable example';
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FEmployees.Free;
end;

procedure TfrmMain.miShowAllClick(Sender: TObject);
begin
  ShowAllRecords;
end;

procedure TfrmMain.btExecSimpleClick(Sender: TObject);
begin
  ExecSimpleQuery(seSkip.Value, seLimit.Value, FComparators[cbCmpKind.ItemIndex]);
end;

procedure TfrmMain.btExec2Click(Sender: TObject);
begin
  ExecQuery2(seSkip2.Value, seLimit2.Value, FComparators[cbCmpKind.ItemIndex]);
end;

procedure TfrmMain.btFindNumbersClick(Sender: TObject);
begin
  FindNumbersByLocation;
end;

procedure TfrmMain.btSelectByLocationClick(Sender: TObject);
begin
  SelectByLocation(cbLocation.Text);
end;

procedure TfrmMain.btMinMaxSelectedClick(Sender: TObject);
begin
  SelectMinMaxByLocation(cbLocation.Text, FComparators[cbCmpKind.ItemIndex]);
end;

procedure TfrmMain.btSortClick(Sender: TObject);
begin
  Sort(cbCmpKind.Text, FComparators[cbCmpKind.ItemIndex]);
end;

procedure TfrmMain.btMaxByLocationClick(Sender: TObject);
begin
  SelectMaxByLocation(Copy(cbCmpKind.Text, 4, Length(cbCmpKind.Text)), FComparators[cbCmpKind.ItemIndex]);
end;

procedure TfrmMain.cbSelectByGenderAgeClick(Sender: TObject);
begin
  SelectGenderAndAge(cbGender.Text, seLoAge.Value, seHiAge.Value);
end;

function TfrmMain.StringCmp(constref L, R: string): SizeInt;
begin
  Result := ansistring.Compare(L, R);
end;

function TfrmMain.CmpByFirst(constref L, R: TEmployee): SizeInt;
begin
  Result := ansistring.Compare(L.FirstName, R.FirstName);
end;

function TfrmMain.CmpByLast(constref L, R: TEmployee): SizeInt;
begin
  Result := ansistring.Compare(L.LastName, R.LastName);
end;

function TfrmMain.CmpByAge(constref L, R: TEmployee): SizeInt;
begin
  Result := ShortInt.Compare(L.Age, R.Age);
end;

function TfrmMain.CmpByLocation(constref L, R: TEmployee): SizeInt;
begin
  Result := ansistring.Compare(L.Location, R.Location);
end;

function TfrmMain.CmpByPhone(constref L, R: TEmployee): SizeInt;
begin
  Result := ansistring.Compare(L.Phone, R.Phone);
end;

function TfrmMain.CmpBySalary(constref L, R: TEmployee): SizeInt;
begin
  Result := Currency.Compare(L.Salary, R.Salary);
end;

procedure TfrmMain.FillcbCmpKind;
begin
  cbCmpKind.Clear;
  FComparators[0] := @CmpByFirst;
  cbCmpKind.Items.Add('by first name');
  FComparators[1] := @CmpByLast;
  cbCmpKind.Items.Add('by last name');
  FComparators[2] := @CmpByAge;
  cbCmpKind.Items.Add('by age');
  FComparators[3] := @CmpByLocation;
  cbCmpKind.Items.Add('by location');
  FComparators[4] := @CmpByPhone;
  cbCmpKind.Items.Add('by phone number');
  FComparators[5] := @CmpBySalary;
  cbCmpKind.Items.Add('by salary');
  cbCmpKind.ItemIndex := 0;
end;

procedure TfrmMain.FillcbLocation;
type
  TMapping = specialize TGMapping<TEmployee, string>;
var
  s: string;
begin
  cbLocation.Clear;
  for s in TMapping.Apply(FEmployees, @GetLocation).Distinct(@StringCmp) do
    cbLocation.Items.Append(s);
  cbLocation.ItemIndex := 0;
end;

procedure TfrmMain.FillcbGender;
begin
  cbGender.Clear;
  cbGender.Items.Append('Male');
  cbGender.Items.Append('Female');
  cbGender.ItemIndex := 0;
end;

function TfrmMain.GetLocation(constref e: TEmployee): string;
begin
  Result := e.Location;
end;

function TfrmMain.GenderIsMale(constref e: TEmployee): Boolean;
begin
  Result := e.Gender = 'Male';
end;

procedure TfrmMain.ShowAllRecords;
var
  e: TEmployee;
begin
  mmResult.Lines.BeginUpdate;
  try
    mmResult.Clear;
    mmResult.Append('');
    for e in FEmployees do
      mmResult.Append(e.ToString);
  finally
    mmResult.Lines.EndUpdate;
  end;
end;

procedure TfrmMain.ExecSimpleQuery(SkipCount, LimitCount: SizeInt; aCmp: TEmployeeCompare);
var
  e: TEmployee;
begin
  mmResult.Lines.BeginUpdate;
  try
    mmResult.Lines.Clear;

    mmResult.Append('Executing FEmployees.First:');
    mmResult.Append(FEmployees.First.OrElseDefault.ToString);

    mmResult.Append('Executing FEmployees.Last:');
    mmResult.Append(FEmployees.Last.OrElseDefault.ToString);

    mmResult.Append('Executing FEmployees.Min:');
    mmResult.Append(FEmployees.Min(aCmp).OrElseDefault.ToString);

    mmResult.Append('Executing FEmployees.Max:');
    mmResult.Append(FEmployees.Max(aCmp).OrElseDefault.ToString);

    mmResult.Append('Executing FEmployees.Skip(SkipCount).Limit(LimitCount):');
    for e in FEmployees.Skip(SkipCount).Limit(LimitCount) do
      mmResult.Append(e.ToString);
  finally
    mmResult.Lines.EndUpdate;
  end;
end;

procedure TfrmMain.ExecQuery2(SkipCount, LimitCount: SizeInt; aCmp: TEmployeeCompare);
begin
  mmResult.Lines.BeginUpdate;
  try
    mmResult.Lines.Clear;

    mmResult.Append('Executing FEmployees.Skip(SkipCount).Limit(LimitCount).First:');
    mmResult.Append(FEmployees.Skip(SkipCount).Limit(LimitCount).First.OrElseDefault.ToString);

    mmResult.Append('Executing FEmployees.Skip(SkipCount).Limit(LimitCount).Last:');
    mmResult.Append(FEmployees.Skip(SkipCount).Limit(LimitCount).Last.OrElseDefault.ToString);

    mmResult.Append('Executing FEmployees.Skip(SkipCount).Limit(LimitCount).Min:');
    mmResult.Append(FEmployees.Skip(SkipCount).Limit(LimitCount).Min(aCmp).OrElseDefault.ToString);

    mmResult.Append('Executing FEmployees.Skip(SkipCount).Limit(LimitCount).Max:');
    mmResult.Append(FEmployees.Skip(SkipCount).Limit(LimitCount).Max(aCmp).OrElseDefault.ToString);
  finally
    mmResult.Lines.EndUpdate;
  end;
end;

procedure TfrmMain.SelectByLocation(const aLocation: string);
  function RightLocation(constref e: TEmployee): Boolean;
  begin
    Result := e.Location = aLocation;
  end;
var
  e: TEmployee;
begin
  mmResult.Lines.BeginUpdate;
  try
    mmResult.Lines.Clear;

    mmResult.Append('Executing FEmployees.Select(@RightLocation), where Location = ' + aLocation);
    for e in FEmployees.Select(@RightLocation) do
      mmResult.Append(e.ToString);
  finally
    mmResult.Lines.EndUpdate;
  end;
end;

procedure TfrmMain.FindNumbersByLocation;
type
  TMapping = specialize TGMapping<TEmployee, string>;
var
  s: string;
  function RightLocation(constref e: TEmployee): Boolean;
  begin
    Result := e.Location = s;
  end;
begin
  mmResult.Lines.BeginUpdate;
  try
    mmResult.Lines.Clear;

    mmResult.Append('Executing FEmployees.Select.Total, grouping by location:');
    for s in TMapping.Apply(FEmployees, @GetLocation).Distinct(@StringCmp) do
      mmResult.Append(s + #9 + FEmployees.Select(@RightLocation).Total.ToString);
  finally
    mmResult.Lines.EndUpdate;
  end;
end;

procedure TfrmMain.SelectMinMaxByLocation(const aLocation: string; aCmp: TEmployeeCompare);
  function RightLocation(constref e: TEmployee): Boolean;
  begin
    Result := e.Location = aLocation;
  end;
begin
  mmResult.Lines.BeginUpdate;
  try
    mmResult.Lines.Clear;

    mmResult.Append('Executing FEmployees.Select(@RightLocation).Min, where Location = ' + aLocation);
    mmResult.Append(FEmployees.Select(@RightLocation).Min(aCmp).OrElseDefault.ToString);

    mmResult.Append('Executing FEmployees.Select(@RightLocation).Max, where Location = ' + aLocation);
    mmResult.Append(FEmployees.Select(@RightLocation).Max(aCmp).OrElseDefault.ToString);
  finally
    mmResult.Lines.EndUpdate;
  end;
end;

procedure TfrmMain.Sort(const aCaption: string; aCmp: TEmployeeCompare);
var
  e: TEmployee;
begin
  mmResult.Lines.BeginUpdate;
  try
    mmResult.Lines.Clear;

    mmResult.Append('Executing FEmployees.Sorted ' + aCaption);
    for e in FEmployees.Sorted(aCmp) do
      mmResult.Append(e.ToString);
  finally
    mmResult.Lines.EndUpdate;
  end;
end;

procedure TfrmMain.SelectMaxByLocation(const aCaption: string; aCmp: TEmployeeCompare);
type
  TMapping = specialize TGMapping<TEmployee, string>;
var
  s: string;
  function RightLocation(constref e: TEmployee): Boolean;
  begin
    Result := e.Location = s;
  end;
begin
  mmResult.Lines.BeginUpdate;
  try
    mmResult.Lines.Clear;

    mmResult.Append('Executing FEmployees.FindMax of ' + aCaption + ', grouping by location:');
    for s in TMapping.Apply(FEmployees, @GetLocation).Distinct(@StringCmp) do
      mmResult.Append(FEmployees.Select(@RightLocation).Max(aCmp).OrElseDefault.ToString);
  finally
    mmResult.Lines.EndUpdate;
  end;
end;

procedure TfrmMain.SelectGenderAndAge(const aGender: string; LoAge, HiAge: Integer);
var
  s: string;
  function IsRightEmployee(constref e: TEmployee): Boolean;
  begin
    Result := (e.Location = s) and (e.Gender = aGender) and (e.Age >= LoAge) and (e.Age <= HiAge);
  end;
var
  e: TEmployee;
  Total: Integer;
begin
  mmResult.Lines.BeginUpdate;
  try
    mmResult.Lines.Clear;
    mmResult.Append('Executing employees selection by gender and age, groping by location:');
    for s in cbLocation.Items do
      begin
        Total := 0;
        for e in FEmployees.Select(@IsRightEmployee) do
          begin
            mmResult.Append(e.ToString);
            Inc(Total);
          end;
        mmResult.Append('Total by ' + s + #9 + Total.ToString + LineEnding);
      end;
  finally
    mmResult.Lines.EndUpdate;
  end;
end;

end.

