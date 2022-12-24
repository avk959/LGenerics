unit Main;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Spin, Menus,
  lgUtils,
  lgVector,
  lgHelpers,
  lgFunction;

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

  TEmployeeCompare = specialize TGOnLessCompare<TEmployee>;
  TEmployees       = specialize TGVector<TEmployee>;

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
    FEmployees: TEmployees;
    Comparators: array[0..5] of TEmployeeCompare;
    function  StringCmp(const L, R: string): Boolean;
    function  CmpByFirst(const L, R: TEmployee): Boolean;
    function  CmpByLast(const L, R: TEmployee): Boolean;
    function  CmpByAge(const L, R: TEmployee): Boolean;
    function  CmpByLocation(const L, R: TEmployee): Boolean;
    function  CmpByPhone(const L, R: TEmployee): Boolean;
    function  CmpBySalary(const L, R: TEmployee): Boolean;
    procedure FillcbCmpKind;
    procedure FillcbLocation;
    procedure FillcbGender;
    function  GetLocation(const e: TEmployee): string;
    function  GenderIsMale(const e: TEmployee): Boolean;
    procedure Append(const e: TEmployee);

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
  Employees: array of TEmployee = ({$I data.inc});

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
  FEmployees := TEmployees.From(Employees);
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
  ExecSimpleQuery(seSkip.Value, seLimit.Value, Comparators[cbCmpKind.ItemIndex]);
end;

procedure TfrmMain.btExec2Click(Sender: TObject);
begin
  ExecQuery2(seSkip2.Value, seLimit2.Value, Comparators[cbCmpKind.ItemIndex]);
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
  SelectMinMaxByLocation(cbLocation.Text, Comparators[cbCmpKind.ItemIndex]);
end;

procedure TfrmMain.btSortClick(Sender: TObject);
begin
  Sort(cbCmpKind.Text, Comparators[cbCmpKind.ItemIndex]);
end;

procedure TfrmMain.btMaxByLocationClick(Sender: TObject);
begin
  SelectMaxByLocation(Copy(cbCmpKind.Text, 4, Length(cbCmpKind.Text)), Comparators[cbCmpKind.ItemIndex]);
end;

procedure TfrmMain.cbSelectByGenderAgeClick(Sender: TObject);
begin
  SelectGenderAndAge(cbGender.Text, seLoAge.Value, seHiAge.Value);
end;

function TfrmMain.StringCmp(const L, R: string): Boolean;
begin
  Result := ansistring.Less(L, R);
end;

function TfrmMain.CmpByFirst(const L, R: TEmployee): Boolean;
begin
  Result := ansistring.Less(L.FirstName, R.FirstName);
end;

function TfrmMain.CmpByLast(const L, R: TEmployee): Boolean;
begin
  Result := ansistring.Less(L.LastName, R.LastName);
end;

function TfrmMain.CmpByAge(const L, R: TEmployee): Boolean;
begin
  Result := L.Age < R.Age;
end;

function TfrmMain.CmpByLocation(const L, R: TEmployee): Boolean;
begin
  Result := ansistring.Less(L.Location, R.Location);
end;

function TfrmMain.CmpByPhone(const L, R: TEmployee): Boolean;
begin
  Result := ansistring.Less(L.Phone, R.Phone);
end;

function TfrmMain.CmpBySalary(const L, R: TEmployee): Boolean;
begin
  Result := Currency.Less(L.Salary, R.Salary);
end;

procedure TfrmMain.FillcbCmpKind;
begin
  cbCmpKind.Clear;
  Comparators[0] := @CmpByFirst;
  cbCmpKind.Items.Add('by first name');
  Comparators[1] := @CmpByLast;
  cbCmpKind.Items.Add('by last name');
  Comparators[2] := @CmpByAge;
  cbCmpKind.Items.Add('by age');
  Comparators[3] := @CmpByLocation;
  cbCmpKind.Items.Add('by location');
  Comparators[4] := @CmpByPhone;
  cbCmpKind.Items.Add('by phone number');
  Comparators[5] := @CmpBySalary;
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

function TfrmMain.GetLocation(const e: TEmployee): string;
begin
  Result := e.Location;
end;

function TfrmMain.GenderIsMale(const e: TEmployee): Boolean;
begin
  Result := e.Gender = 'Male';
end;

procedure TfrmMain.Append(const e: TEmployee);
begin
  mmResult.Append(e.ToString);
end;

procedure TfrmMain.ShowAllRecords;
begin
  mmResult.Lines.BeginUpdate;
  try
    mmResult.Clear;
    mmResult.Append('');
    FEmployees.ForEach(@Append);
  finally
    mmResult.Lines.EndUpdate;
  end;
end;

procedure TfrmMain.ExecSimpleQuery(SkipCount, LimitCount: SizeInt; aCmp: TEmployeeCompare);
begin
  mmResult.Lines.BeginUpdate;
  try
    mmResult.Lines.Clear;

    mmResult.Append('Executing FEmployees.First:');
    mmResult.Append(FEmployees.First.OrElse(Default(TEmployee)).ToString);

    mmResult.Append('Executing FEmployees.Last:');
    mmResult.Append(FEmployees.Last.OrElse(Default(TEmployee)).ToString);

    mmResult.Append('Executing FEmployees.Min:');
    mmResult.Append(FEmployees.Min(aCmp).OrElse(Default(TEmployee)).ToString);

    mmResult.Append('Executing FEmployees.Max:');
    mmResult.Append(FEmployees.Max(aCmp).OrElse(Default(TEmployee)).ToString);

    mmResult.Append('Executing FEmployees.Skip(SkipCount).Limit(LimitCount):');
    FEmployees
     .Skip(SkipCount)
     .Limit(LimitCount)
     .ForEach(@Append);
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
    mmResult.Append(FEmployees.Skip(SkipCount).Limit(LimitCount).First.OrElse(Default(TEmployee)).ToString);

    mmResult.Append('Executing FEmployees.Skip(SkipCount).Limit(LimitCount).Last:');
    mmResult.Append(FEmployees.Skip(SkipCount).Limit(LimitCount).Last.OrElse(Default(TEmployee)).ToString);

    mmResult.Append('Executing FEmployees.Skip(SkipCount).Limit(LimitCount).Min:');
    mmResult.Append(FEmployees.Skip(SkipCount).Limit(LimitCount).Min(aCmp).OrElse(Default(TEmployee)).ToString);

    mmResult.Append('Executing FEmployees.Skip(SkipCount).Limit(LimitCount).Max:');
    mmResult.Append(FEmployees.Skip(SkipCount).Limit(LimitCount).Max(aCmp).OrElse(Default(TEmployee)).ToString);
  finally
    mmResult.Lines.EndUpdate;
  end;
end;

procedure TfrmMain.SelectByLocation(const aLocation: string);
  function RightLocation(const e: TEmployee): Boolean; begin Result := e.Location = aLocation end;
begin
  mmResult.Lines.BeginUpdate;
  try
    mmResult.Lines.Clear;

    mmResult.Append('Executing FEmployees.Select(@RightLocation), where Location = ' + aLocation);
    FEmployees
     .Select(@RightLocation)
     .ForEach(@Append);
  finally
    mmResult.Lines.EndUpdate;
  end;
end;

procedure TfrmMain.FindNumbersByLocation;
type
  TGroping = specialize TGGrouping<TEmployee, string>;
var
  g: TGroping.IGroup;
begin
  mmResult.Lines.BeginUpdate;
  try
    mmResult.Lines.Clear;

    mmResult.Append('Executing FEmployees.Select.Total, grouping by location:');
    for g in TGroping.Apply(FEmployees, @GetLocation) do
      mmResult.Append(TGroping.GroupKey(g, @GetLocation).OrElse('no such location') +
                      #9 + IntToStr(g.Total));
  finally
    mmResult.Lines.EndUpdate;
  end;
end;

procedure TfrmMain.SelectMinMaxByLocation(const aLocation: string; aCmp: TEmployeeCompare);
  function RightLocation(const e: TEmployee): Boolean; begin Result := e.Location = aLocation end;
begin
  mmResult.Lines.BeginUpdate;
  try
    mmResult.Lines.Clear;

    mmResult.Append('Executing FEmployees.Select(@RightLocation).Min, where Location = ' + aLocation);
    mmResult.Append(FEmployees.Select(@RightLocation).Min(aCmp).OrElse(Default(TEmployee)).ToString);

    mmResult.Append('Executing FEmployees.Select(@RightLocation).Max, where Location = ' + aLocation);
    mmResult.Append(FEmployees.Select(@RightLocation).Max(aCmp).OrElse(Default(TEmployee)).ToString);
  finally
    mmResult.Lines.EndUpdate;
  end;
end;

procedure TfrmMain.Sort(const aCaption: string; aCmp: TEmployeeCompare);
begin
  mmResult.Lines.BeginUpdate;
  try
    mmResult.Lines.Clear;

    mmResult.Append('Executing FEmployees.Sorted ' + aCaption);
    FEmployees
     .Sorted(aCmp, True)
     .ForEach(@Append);
  finally
    mmResult.Lines.EndUpdate;
  end;
end;

procedure TfrmMain.SelectMaxByLocation(const aCaption: string; aCmp: TEmployeeCompare);
type
  TGroping = specialize TGGrouping<TEmployee, string>;
var
  g: TGroping.IGroup;
begin
  mmResult.Lines.BeginUpdate;
  try
    mmResult.Lines.Clear;

    mmResult.Append('Executing FEmployees.FindMax of ' + aCaption + ', grouping by location:');
    for g in TGroping.Apply(FEmployees, @GetLocation) do
      mmResult.Append(g.Max(aCmp).OrElse(Default(TEmployee)).ToString);
  finally
    mmResult.Lines.EndUpdate;
  end;
end;

procedure TfrmMain.SelectGenderAndAge(const aGender: string; LoAge, HiAge: Integer);
type
  TGroping = specialize TGGrouping<TEmployee, string>;
  function IsRightEmployee(const e: TEmployee): Boolean;
  begin
    Result := (e.Gender = aGender) and (e.Age >= LoAge) and (e.Age <= HiAge);
  end;
var
  g: TGroping.IGroup;
  s: string;
  Total: Integer;
  procedure AppendNest(const e: TEmployee); begin mmResult.Append(e.ToString); Inc(Total) end;
begin
  mmResult.Lines.BeginUpdate;
  try
    mmResult.Lines.Clear;
    mmResult.Append('Executing employees selection by gender and age, grouping by location:');
    for g in TGroping.Apply(FEmployees, @GetLocation) do
      begin
        Total := 0;
        s := TGroping.GroupKey(g, @GetLocation).OrElse('no such location');
        g.Select(@IsRightEmployee)
         .ForEach(@AppendNest);
        mmResult.Append('Total by ' + s + #9 + Total.ToString + LineEnding);
      end;
  finally
    mmResult.Lines.EndUpdate;
  end;
end;

end.

