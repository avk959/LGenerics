unit lgCsvUtilsTest;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}
{$MODESWITCH TYPEHELPERS}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, typinfo,
  lgUtils,
  lgArrayHelpers,
  lgCsvUtils;

type

  { TRsHelper }

  TRsHelper = type helper for TCsvReader.TReadState
    function ToString: string;
  end;

  { TCeHelper }

  TCeHelper = type helper for TCsvEntity.TCsvEvent
    function ToString: string;
  end;

  { TCcpHelper }

  TCcpHelper = type helper for TCsvDoc.TColCountPolicy
    function ToString: string;
  end;

  { TTestCsvReader }

  TTestCsvReader = class(TTestCase)
  private
  type
    TProc = procedure is nested;
    TStrStream = class(TStringStream)
    private
      FProc: TProc;
    public
      constructor Create(const s: string; aProc: TProc); overload;
      destructor Destroy; override;
    end;
    TReader = specialize TGUniqRef<TCsvReader>;
    THelper = specialize TGComparableArrayHelper<string>;

  published
    procedure OwnsStream;
    procedure BufferSize;
    procedure SetDelimiter;
    procedure Delimiter;
    procedure SetQuoteMark;
    procedure SetCommentMark;
    procedure SetOptions;
    procedure Read;
    procedure ReadQuoted;
    procedure ReadCommented;
    procedure ReadLong;
    procedure ReadRow;
    procedure ReadRowMask;
    procedure ReadRowQuoted;
    procedure Skip;
    procedure SkipTimes;
    procedure SkipRow;
    procedure SkipRowTimes;
    procedure Iterate;
    procedure IterateRow;
    procedure IterateRowMask;
    procedure Bom;
    procedure DirtyQuote;
    procedure BareQuote;
    procedure UnclosedQuote;
    procedure TryReadRow;
    procedure Encoding;
  end;

  { TTestCsvWriter }

  TTestCsvWriter = class(TTestCase)
  private
  type
    TStrStream = specialize TGAutoRef<TStringStream>;
    TWriter    = specialize TGUniqRef<TCsvWriter>;
  published
    procedure Delimiter;
    procedure QuoteMark;
    procedure LineEndStyle;
    procedure QuotePolicy;
  end;

  { TTestCsvDoc }

  TTestCsvDoc = class(TTestCase)
  private
  type
    TDoc = specialize TGAutoRef<TCsvDoc>;
  published
    procedure CsvText;
    procedure HeaderColCount;
    procedure MinColCount;
    procedure MaxColCount;
    procedure ColumnCount;
    procedure AssignDoc;
    procedure InsertRow;
    procedure AddRow;
    procedure DeleteRow;
    procedure SwapRow;
    procedure InsertCell;
    procedure AddCell;
    procedure RowExists;
    procedure CellExists;
    procedure IndexOfCol;
    procedure IndexOfRow;
    procedure TrimTrailingBlanks;
  end;

implementation

{ TRsHelper }

function TRsHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(TCsvReader.TReadState), Integer(Self));
end;

{ TCeHelper }

function TCeHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(TCsvEntity.TCsvEvent), Integer(Self));
end;

{ TCcpHelper }

function TCcpHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(TCsvDoc.TColCountPolicy), Integer(Self));
end;

{ TTestCsvReader.TStrStream }

constructor TTestCsvReader.TStrStream.Create(const s: string; aProc: TProc);
begin
  inherited Create(s);
  FProc := aProc;
end;

destructor TTestCsvReader.TStrStream.Destroy;
begin
  if FProc <> nil then FProc();
  inherited;
end;

{ TTestCsvReader }

procedure TTestCsvReader.OwnsStream;
var
  I: Integer = 42;
  procedure ResetI; begin I := 0 end;
var
  Reader: TReader;
  ss: TStrStream;
begin
  ss := TStrStream.Create('', @ResetI);
  {%H-}Reader.Instance := TCsvReader.New(ss, 42, False);
  AssertTrue('Destruction of a used stream, when OwnsStream if False', I = 42);
  Reader.Instance := TCsvReader.New(ss, 42, True);
  Reader.Instance := nil;
  AssertTrue('Ignoring of a used stream, when OwnsStream if True', I = 0);
end;

procedure TTestCsvReader.BufferSize;
var
  Reader: TReader;
  ss: TStrStream;
  bs: Integer;
begin
  bs := 42;
  {%H-}Reader.Instance := TCsvReader.New(nil, bs, False);
  AssertTrue('Ignoring buffer size set in constructor', Reader.Instance.BufferSize = bs);
  bs := -42;
  ss := TStrStream.Create('abracadabra');
  Reader.Instance := TCsvReader.New(ss, bs, True);
  AssertTrue('Incorrectly set buffer size when set to value < 1 in the constructor',
             Reader.Instance.BufferSize = ss.Size);
end;

procedure TTestCsvReader.SetDelimiter;
var
  Reader: TReader;
  d: Char;
  Raised: Boolean = False;
begin
  d := ';';
  {%H-}Reader.Instance := TCsvReader
    .New(nil)
    .SetDelimiter(d);
  AssertTrue('Delimiter setter does not work properly', Reader.Instance.Delimiter = d);

  d := #9;
  {%H-}Reader.Instance := TCsvReader
    .New(nil)
    .SetDelimiter(d);
  AssertTrue('Refuses to set Tab as delimiter', Reader.Instance.Delimiter = d);

  d := #127;
  try
    Reader.Instance.Delimiter := d;
  except
    on e: ECsvSpecialMark do
      Raised := True;
  end;
  AssertTrue('Accepts a character greater than #126 as a delimiter', Raised);

  Raised := False;
  d := #10;
  try
    Reader.Instance.Delimiter := d;
  except
    on e: ECsvSpecialMark do
      Raised := True;
  end;
  AssertTrue('Accepts line feed as a delimiter', Raised);

  Raised := False;
  d := #13;
  try
    Reader.Instance.Delimiter := d;
  except
    on e: ECsvSpecialMark do
      Raised := True;
  end;
  AssertTrue('Accepts carriage return as a delimiter', Raised);

  Raised := False;
  d := Reader.Instance.QuoteMark;
  try
    Reader.Instance.Delimiter := d;
  except
    on e: ECsvSpecialMark do
      Raised := True;
  end;
  AssertTrue('Accepts quotation mark as a delimiter', Raised);

  Raised := False;
  d := Reader.Instance.CommentMark;
  try
    Reader.Instance.Delimiter := d;
  except
    on e: ECsvSpecialMark do
      Raised := True;
  end;
  AssertTrue('Accepts comment mark as a delimiter', Raised);
end;

procedure TTestCsvReader.Delimiter;
var
  Reader: TReader;
  s: string;
  row: TStringArray;
begin
  s := 'a;b;c;d';
  {%H-}Reader.Instance := TCsvReader
    .New(Pointer(s), Length(s))
    .SetDelimiter(';');
  AssertTrue('Can not read when delimiter is semicolon', Reader.Instance.ReadRow);
  row := Reader.Instance.CurrentRow;
  AssertTrue('Original row was read incorrectly', string.Join(';', row) = s);
end;

procedure TTestCsvReader.SetQuoteMark;
var
  Reader: TReader;
  d: Char;
  Raised: Boolean = False;
begin
  d := '`';
  {%H-}Reader.Instance := TCsvReader
    .New(nil)
    .SetQuoteMark(d);
  AssertTrue('Quotation mark setter does not work', Reader.Instance.QuoteMark = d);

  d := #127;
  try
    Reader.Instance.QuoteMark := d;
  except
    on e: ECsvSpecialMark do
      Raised := True;
  end;
  AssertTrue('Accepts a character greater than #126 as a quotation mark', Raised);

  Raised := False;
  d := #32;
  try
    Reader.Instance.QuoteMark := d;
  except
    on e: ECsvSpecialMark do
      Raised := True;
  end;
  AssertTrue('Accepts a character less than #33 as a quotation mark', Raised);

  Raised := False;
  d := Reader.Instance.Delimiter;
  try
    Reader.Instance.QuoteMark := d;
  except
    on e: ECsvSpecialMark do
      Raised := True;
  end;
  AssertTrue('Accepts delimiter as a quotation mark', Raised);

  Raised := False;
  d := Reader.Instance.CommentMark;
  try
    Reader.Instance.QuoteMark := d;
  except
    on e: ECsvSpecialMark do
      Raised := True;
  end;
  AssertTrue('Accepts comment mark as a quotation mark', Raised);
end;

procedure TTestCsvReader.SetCommentMark;
var
  Reader: TReader;
  d: Char;
  Raised: Boolean = False;
begin
  d := '%';
  {%H-}Reader.Instance := TCsvReader
    .New(nil)
    .SetCommentMark(d);
  AssertTrue('Comment mark setter does not work', Reader.Instance.CommentMark = d);

  d := #127;
  try
    Reader.Instance.CommentMark := d;
  except
    on e: ECsvSpecialMark do
      Raised := True;
  end;
  AssertTrue('Accepts a character greater than #126 as a comment mark', Raised);

  Raised := False;
  d := #32;
  try
    Reader.Instance.CommentMark := d;
  except
    on e: ECsvSpecialMark do
      Raised := True;
  end;
  AssertTrue('Accepts a character less than #33 as a comment mark', Raised);

  Raised := False;
  d := Reader.Instance.Delimiter;
  try
    Reader.Instance.CommentMark := d;
  except
    on e: ECsvSpecialMark do
      Raised := True;
  end;
  AssertTrue('Accepts delimiter as a comment mark', Raised);

  Raised := False;
  d := Reader.Instance.QuoteMark;
  try
    Reader.Instance.CommentMark := d;
  except
    on e: ECsvSpecialMark do
      Raised := True;
  end;
  AssertTrue('Accepts quotation mark as a comment mark', Raised);
end;

procedure TTestCsvReader.SetOptions;
var
  Reader: TReader;
  o: TCsvReader.TReadOptions;
begin
  o := [roSkipLeadSpaces];
  {%H-}Reader.Instance := TCsvReader
    .New(nil)
    .SetOptions(o);
  AssertTrue('Options setter does not work', Reader.Instance.Options = o);

  o := o + [roComments];
  Reader.Instance.Options := o;
  AssertTrue('Options setter does not work', Reader.Instance.Options = o);

  Reader.Instance.Options := [];
  AssertTrue('Options setter does not work', Reader.Instance.Options = []);
end;

procedure TTestCsvReader.Read;
var
  Reader: TReader;
  s: string;
  il: TCsvReader.TIssueList;
begin
  {%H-}Reader.Instance := TCsvReader.Create(TStrStream.Create, 42, True);
  AssertFalse('Reading from an empty stream returned True', Reader.Instance.Read);
  AssertEquals('Unexpected read state after reading from empty stream',
               rsEof.ToString, Reader.Instance.ReadState.ToString);
  AssertTrue('Empty stream read event not added to issues', Reader.Instance.HasIssues);
  il := Reader.Instance.IssueList;
  AssertEquals('Inappropriate event added to issues', ceEmptyDoc.ToString, il[0].Reason.ToString);

  s := 'element';
  Reader.Instance := TCsvReader.Create(TStrStream.Create(s), 0, True);
  AssertTrue('Reading from an non-empty stream returned False', Reader.Instance.Read);
  AssertEquals('Unexpected read state after reading non-empty stream', rsEof.ToString, Reader.Instance.ReadState.ToString);
  AssertEquals('Wrong column index after cell read', 0, Reader.Instance.ColIndex);
  AssertEquals('Wrong row index after cell read', 1, Reader.Instance.RowIndex);
  AssertEquals('Wrong cell parsing result', 'element', Reader.Instance.CurrentCell);
end;

procedure TTestCsvReader.ReadQuoted;
var
  Reader: TReader;
  s: string;
begin
  s := '"element"""';
  {%H-}Reader.Instance := TCsvReader.Create(TStrStream.Create(s), 0, True);
  AssertTrue('Reading from an non-empty stream returned False', Reader.Instance.Read);
  AssertEquals('Unexpected read state after non-empty stream reading', rsEof.ToString, Reader.Instance.ReadState.ToString);
  AssertEquals('Wrong column index after cell read', 0, Reader.Instance.ColIndex);
  AssertEquals('Wrong row index after cell read', 1, Reader.Instance.RowIndex);
  AssertEquals('Incorrect parsing of quoted cell with escaped quote at the end', 'element"', Reader.Instance.CurrentCell);

  s := '"""element"';
  Reader.Instance := TCsvReader.Create(TStrStream.Create(s), 0, True);
  AssertTrue(Reader.Instance.Read);
  AssertEquals('Incorrect parsing of quoted cell with escaped quote at the beginning', '"element', Reader.Instance.CurrentCell);

  s := '"elem""ent"';
  Reader.Instance := TCsvReader.Create(TStrStream.Create(s), 0, True);
  AssertTrue(Reader.Instance.Read);
  AssertEquals('Incorrect parsing of quoted cell with escaped quote inside', 'elem"ent', Reader.Instance.CurrentCell);

  s := '"elem'#13#10'ent"';
  Reader.Instance := TCsvReader.Create(TStrStream.Create(s), 0, True);
  AssertTrue(Reader.Instance.Read);
  AssertEquals('Incorrect parsing of quoted cell with CRLF inside', 'elem'#13#10'ent', Reader.Instance.CurrentCell);

  s := '"elem,ent"';
  Reader.Instance := TCsvReader.Create(TStrStream.Create(s), 0, True);
  AssertTrue(Reader.Instance.Read);
  AssertEquals('Incorrect parsing of quoted cell with delimiter inside', 'elem,ent', Reader.Instance.CurrentCell);

  s := '"elem#ent"';
  Reader.Instance := TCsvReader
    .New(TStrStream.Create(s), 0, True)
    .SetOptions([roComments]);
  AssertTrue(Reader.Instance.Read);
  AssertEquals('Incorrect parsing of quoted cell with comment mark inside', 'elem#ent', Reader.Instance.CurrentCell);

  s := '  "element"';
  Reader.Instance := TCsvReader
    .New(TStrStream.Create(s), 0, True)
    .SetOptions([roSkipLeadSpaces]);
  AssertTrue(Reader.Instance.Read);
  AssertEquals('Incorrect parsing of quoted cell with leading spaces', 'element', Reader.Instance.CurrentCell);
end;

procedure TTestCsvReader.ReadCommented;
var
  Reader: TReader;
  s: string;
begin
  s := '#comment'#10'element';
  {%H-}Reader.Instance := TCsvReader.Create(TStrStream.Create(s), 0, True);
  AssertTrue(Reader.Instance.Read);
  AssertEquals('Incorrect parsing of cell with comment mark when comments disabled', '#comment', Reader.Instance.CurrentCell);

  Reader.Instance := TCsvReader
    .New(TStrStream.Create(s), 0, True)
    .SetOptions([roComments]);
  AssertTrue(Reader.Instance.Read);
  AssertEquals('Incorrect parsing of cell with comment mark when comments enabled', 'element', Reader.Instance.CurrentCell);
end;

procedure TTestCsvReader.ReadLong;
var
  Reader: TReader;
  s: string;
  I: Integer;
  Res: TStringArray;
const
  Expect: TStringArray = (
    'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r');
begin
  s := string.Join(',', Expect);
  {%H-}Reader.Instance := TCsvReader.Create(Pointer(s), Length(s));
  while Reader.Instance.Read do;
  Res := Reader.Instance.CurrentRow;
  AssertTrue('Wrong row length after reading', Length(Res) = Length(Expect));
  for I := 0 to High(Expect) do
    AssertEquals('Wrong value of cell #'+I.ToString, Expect[I], Res[I]);
end;

procedure TTestCsvReader.ReadRow;
var
  Reader: TReader;
  s: string;
  row: TStringArray;
  il: TCsvReader.TIssueList;
begin
  {%H-}Reader.Instance := TCsvReader.Create(TStrStream.Create, 42, True);
  AssertFalse('Reading row from an empty stream returned True', Reader.Instance.ReadRow);
  AssertEquals('Unexpected read state after reading row from empty stream', rsEof.ToString, Reader.Instance.ReadState.ToString);
  AssertTrue('Empty stream read event not added to issues', Reader.Instance.HasIssues);
  il := Reader.Instance.IssueList;
  AssertEquals('Inappropriate event added to issues', ceEmptyDoc.ToString, il[0].Reason.ToString);

  s := 'a,b,c';
  Reader.Instance := TCsvReader.Create(TStrStream.Create(s), 0, True);
  AssertTrue('Can not read simple row', Reader.Instance.ReadRow);
  AssertEquals('Unexpected state after reading a simple row', rsEof.ToString, Reader.Instance.ReadState.ToString);
  row := Reader.Instance.CurrentRow;
  AssertTrue('Wrong row length after simple row read', Length(row) = 3);
  AssertTrue('Incorrect parsing of simple row', (row[0] = 'a') and (row[1] = 'b') and (row[2] = 'c'));

  s := 'a,b,c'#10;
  Reader.Instance := TCsvReader.Create(TStrStream.Create(s), 0, True);
  AssertTrue('Can not read simple row with line feed', Reader.Instance.ReadRow);
  AssertTrue('Unexpected state after reading row with line feed', Reader.Instance.ReadState = rsRead);
  row := Reader.Instance.CurrentRow;
  AssertTrue('Wrong row length after reading row with line feed', Length(row) = 3);
  AssertTrue('Incorrect parsing of row with line feed', (row[0] = 'a') and (row[1] = 'b') and (row[2] = 'c'));

  s := 'a,b,c'#13;
  Reader.Instance := TCsvReader.Create(TStrStream.Create(s), 0, True);
  AssertTrue('Can not read simple row with carriage return', Reader.Instance.ReadRow);
  AssertTrue('Unexpected state after reading row with carriage return', Reader.Instance.ReadState = rsRead);
  row := Reader.Instance.CurrentRow;
  AssertTrue('Wrong row length after reading row with carriage return', Length(row) = 3);
  AssertTrue('Incorrect parsing of row with carriage return', (row[0] = 'a') and (row[1] = 'b') and (row[2] = 'c'));

  s := 'a,,';
  Reader.Instance := TCsvReader.Create(TStrStream.Create(s), 0, True);
  AssertTrue('Can not read row with empty cells', Reader.Instance.ReadRow);
  AssertTrue('Unexpected state after reading row with empty cells', Reader.Instance.ReadState = rsEof);
  row := Reader.Instance.CurrentRow;
  AssertTrue('Wrong row length after reading a row with empty cells', Length(row) = 3);
  AssertTrue('Incorrect parsing of row with empty cells', (row[0] = 'a') and (row[1] = '') and (row[2] = ''));
end;

procedure TTestCsvReader.ReadRowMask;
var
  Reader: TReader;
  s, row: string;
  Count: Integer;
const
  Mask1: array of Boolean = (true);
  Mask2: array of Boolean = (false,true,false,false,false,true);
  Expect0: TStringArray = ('','','','','','');
  Expect1: TStringArray = ('a','','','','','');
  Expect2: TStringArray = ('','b','','','','f');
  Expect3: TStringArray = ('a','b','');
  Expect4: TStringArray = ('','e','f');
begin
  s := 'a,b,c,d,e,f';
  {%H-}Reader.Instance := TCsvReader.Create(Pointer(s), Length(s));
  Count := Reader.Instance.ReadRow([]);
  AssertTrue('Reading with an empty mask returns a non-zero value', Count = 0);
  row := string.Join(',', Reader.Instance.CurrentRow);
  AssertTrue('Reading with an empty mask results unexpected row: '+row,
             THelper.Same(Reader.Instance.CurrentRow, Expect0));

  Reader.Instance := TCsvReader.Create(Pointer(s), Length(s));
  Count := Reader.Instance.ReadRow(Mask1);
  AssertTrue('Reading with Mask1 returns the wrong value: ' + Count.ToString, Count = 1);
  row := string.Join(',', Reader.Instance.CurrentRow);
  AssertTrue('Reading with Mask1 results the unexpected row: ',
             THelper.Same(Reader.Instance.CurrentRow, Expect1));

  Reader.Instance := TCsvReader.Create(Pointer(s), Length(s));
  Count := Reader.Instance.ReadRow(Mask2);
  AssertTrue('Reading with Mask2 returns the wrong value: ' + Count.ToString,Count = 2);
  row := string.Join(',', Reader.Instance.CurrentRow);
  AssertTrue('Reading with Mask1 results the unexpected row: ' + row,
             THelper.Same(Reader.Instance.CurrentRow, Expect2));

  s := 'a,b,c'#10'd,e,f';
  Reader.Instance := TCsvReader.Create(Pointer(s), Length(s));
  Count := Reader.Instance.ReadRow([true,true]);
  AssertTrue('Reading the first row returns the wrong value: ' + Count.ToString, Count = 2);
  row := string.Join(',', Reader.Instance.CurrentRow);
  AssertTrue('Reading the first row results unexpected row: ' + row,
             THelper.Same(Reader.Instance.CurrentRow, Expect3));
  Reader.Instance.CurrRowPtr^[0] := '';
  Count := Reader.Instance.ReadRow([false,true,true]);
  AssertTrue('Reading the second row returns the wrong value: ' + Count.ToString, Count = 2);
  row := string.Join(',', Reader.Instance.CurrentRow);
  AssertTrue('Reading the second row results unexpected row: ' + row,
             THelper.Same(Reader.Instance.CurrentRow, Expect4));
end;

procedure TTestCsvReader.ReadRowQuoted;
var
  Reader: TReader;
  s: string;
  row: TStringArray;
begin
  s := '"a",b,"c"';
  {%H-}Reader.Instance := TCsvReader.Create(TStrStream.Create(s), 0, True);
  AssertTrue('Can not read row with some quoted cells', Reader.Instance.ReadRow);
  AssertTrue('Unexpected state after reading row with some quoted cells', Reader.Instance.ReadState = rsEof);
  row := Reader.Instance.CurrentRow;
  AssertTrue('Wrong row length after reading a row with some quoted cells', Length(row) = 3);
  AssertTrue('Incorrect parsing of row with some quoted cells', (row[0] = 'a') and (row[1] = 'b') and (row[2] = 'c'));

  s := '"a""","b",c'#10;
  Reader.Instance := TCsvReader.Create(TStrStream.Create(s), 0, True);
  AssertTrue(Reader.Instance.ReadRow);
  AssertTrue(Reader.Instance.ReadState = rsRead);
  row := Reader.Instance.CurrentRow;
  AssertTrue(Length(row) = 3);
  AssertTrue((row[0] = 'a"') and (row[1] = 'b') and (row[2] = 'c'));

  s := '"a","""b",  "c"'#10;
  Reader.Instance := TCsvReader
    .New(TStrStream.Create(s), 0, True)
    .SetOptions([roSkipLeadSpaces]);;
  AssertTrue(Reader.Instance.ReadRow);
  AssertTrue(Reader.Instance.ReadState = rsRead);
  row := Reader.Instance.CurrentRow;
  AssertTrue(Length(row) = 3);
  AssertTrue((row[0] = 'a') and (row[1] = '"b') and (row[2] = 'c'));

  s := '"""","",  ""'#10;
  Reader.Instance := TCsvReader
    .New(TStrStream.Create(s), 0, True)
    .SetOptions([roSkipLeadSpaces]);;
  AssertTrue(Reader.Instance.ReadRow);
  AssertTrue(Reader.Instance.ReadState = rsRead);
  row := Reader.Instance.CurrentRow;
  AssertTrue(Length(row) = 3);
  AssertTrue((row[0] = '"') and (row[1] = '') and (row[2] = ''));
end;

procedure TTestCsvReader.Skip;
var
  Reader: TReader;
  s: string;
  il: TCsvReader.TIssueList;
begin
  {%H-}Reader.Instance := TCsvReader.Create(TStrStream.Create, 42, True);
  AssertFalse('Skip on empty stream returned True', Reader.Instance.Skip);
  AssertEquals('Unexpected read state after Skip in empty stream', rsEof.ToString, Reader.Instance.ReadState.ToString);
  AssertTrue('Empty stream read event not added to issues', Reader.Instance.HasIssues);
  il := Reader.Instance.IssueList;
  AssertEquals('Inappropriate event added to issues', ceEmptyDoc.ToString, il[0].Reason.ToString);

  s := 'a,b,c,d';
  Reader.Instance := TCsvReader.Create(Pointer(s), Length(s));
  AssertTrue('Skip on non-empty data returned False', Reader.Instance.Skip);
  AssertTrue(Reader.Instance.Skip);
  AssertTrue(Reader.Instance.Skip);
  AssertTrue('Wrong column index after skip several cells', Reader.Instance.ColIndex = 2);
  AssertTrue('Can not read after skip several cells', Reader.Instance.Read);
  AssertEquals('Unexpected CurrentCell value', 'd', Reader.Instance.CurrentCell);

  s := 'a,,,d';
  Reader.Instance := TCsvReader.Create(Pointer(s), Length(s));
  AssertTrue(Reader.Instance.Skip);
  AssertTrue(Reader.Instance.Skip);
  AssertTrue(Reader.Instance.Skip);
  AssertTrue('Wrong column index after skip several empty cells', Reader.Instance.ColIndex = 2);
  AssertTrue('Can not read after skip several empty cells', Reader.Instance.Read);
  AssertEquals('Unexpected CurrentCell value', 'd', Reader.Instance.CurrentCell);

  s := 'a,b"b,c,d';
  Reader.Instance := TCsvReader.Create(Pointer(s), Length(s));
  while Reader.Instance.Skip do;
  AssertTrue(Reader.Instance.HasIssues);
  il := Reader.Instance.IssueList;
  AssertTrue(Length(il) = 1);
  AssertTrue((il[0].Row = 0) and (il[0].Column = 1) and (il[0].Reason = ceDirtyQuote));
end;

procedure TTestCsvReader.SkipTimes;
var
  Reader: TReader;
  s: string;
  il: TCsvReader.TIssueList;
begin
  s := 'a,b,c,d';
  {%H-}Reader.Instance := TCsvReader.Create(Pointer(s), Length(s));
  AssertTrue(Reader.Instance.Skip(-1) = 0);
  AssertTrue(Reader.Instance.Skip(3) = 3);
  AssertTrue(Reader.Instance.Skip(5) = 1);

  s := 'a,b,c,d';
  Reader.Instance := TCsvReader.Create(Pointer(s), Length(s));
  AssertTrue(Reader.Instance.Skip(7) = 4);

  s := 'a",b,c';
  Reader.Instance := TCsvReader.Create(Pointer(s), Length(s));
  AssertTrue(Reader.Instance.Skip(5) = 3);
  AssertTrue(Reader.Instance.HasIssues);
  il := Reader.Instance.IssueList;
  AssertTrue(Length(il) = 1);
  AssertTrue((il[0].Row = 0) and (il[0].Column = 0) and (il[0].Reason = ceDirtyQuote));
end;

procedure TTestCsvReader.SkipRow;
var
  Reader: TReader;
  s: string;
  row: TStringArray;
  il: TCsvReader.TIssueList;
begin
  {%H-}Reader.Instance := TCsvReader.Create(TStrStream.Create, 42, True);
  AssertFalse('SkipRow in an empty stream returns True', Reader.Instance.SkipRow);
  AssertEquals('Unexpected read state after SkipRow in empty stream', rsEof.ToString, Reader.Instance.ReadState.ToString);
  AssertTrue('Empty stream read event not added to issues', Reader.Instance.HasIssues);
  il := Reader.Instance.IssueList;
  AssertEquals('Inappropriate event added to issues', ceEmptyDoc.ToString, il[0].Reason.ToString);

  s := 'a,b,c'#10'1,2,3'#13'x,y,z';
  Reader.Instance := TCsvReader.Create(Pointer(s), Length(s));
  AssertTrue(Reader.Instance.SkipRow);
  AssertTrue(Reader.Instance.SkipRow);
  AssertTrue('Wrong row index after skip 2 rows', Reader.Instance.RowIndex = 2);
  AssertTrue('Can not read row after skip 2 rows', Reader.Instance.ReadRow);
  row := Reader.Instance.CurrentRow;
  AssertTrue(Length(row) = 3);
  AssertTrue('Unexpected CurrentRow value', (row[0] = 'x') and (row[1] = 'y') and (row[2] = 'z'));
end;

procedure TTestCsvReader.SkipRowTimes;
var
  Reader: TReader;
  s: string;
  il: TCsvReader.TIssueList;
begin
  s := 'a,b,c'#10'1,2,3'#13'p,"q"q",r'#13'x,y,z';
  {%H-}Reader.Instance := TCsvReader.Create(Pointer(s), Length(s));
  AssertTrue(Reader.Instance.SkipRow(-1) = 0);
  AssertTrue(Reader.Instance.SkipRow(1) = 1);
  AssertTrue(Reader.Instance.SkipRow(2) = 2);
  AssertTrue(Reader.Instance.SkipRow(3) = 1);
  il := Reader.Instance.IssueList;
  AssertTrue(Length(il) = 1);
  AssertTrue((il[0].Row = 2) and (il[0].Column = 1) and (il[0].Reason = ceBareQuote));
end;

procedure TTestCsvReader.Iterate;
var
  Line: string = '';
  function Callme(r: TCsvReader): Boolean;
  begin
    Line := Line + r.CurrentCell;
    Result := Length(Line) < 7;
  end;
var
  Reader: TReader;
  s: string;
begin
  {%H-}Reader.Instance := TCsvReader.Create(TStrStream.Create, 42, True);
  Reader.Instance.Iterate(@Callme);
  AssertTrue(Line = '');

  s := 'a,b,c'#10'1,2,3'#13'x,y,z';
  Reader.Instance := TCsvReader.Create(Pointer(s), Length(s));
  Reader.Instance.Iterate(@Callme);
  AssertTrue(Line = 'abc123x');
  AssertTrue(Reader.Instance.ColIndex = 0);
  AssertTrue(Reader.Instance.RowIndex = 2);
end;

procedure TTestCsvReader.IterateRow;
var
  Line: string = '';
  function Callme(r: TCsvReader): Boolean;
  begin
    if r.RowIndex < 3 then exit(True);
    Line := Line + r.Cell[0] + r.Cell[1];
    Result := False;
  end;
var
  Reader: TReader;
  s: string;
begin
  {%H-}Reader.Instance := TCsvReader.Create(TStrStream.Create, 42, True);
  Reader.Instance.IterateRow(@Callme);
  AssertTrue(Line = '');

  s := ' a, b'#10' c, d'#13' e, f'#10' g, h';
  Reader.Instance := TCsvReader
    .New(Pointer(s), Length(s))
    .SetOptions([roSkipLeadSpaces]);
  Reader.Instance.IterateRow(@Callme);
  AssertTrue(Line, Line = 'ef');
  AssertTrue('ColIndex '+Reader.Instance.ColIndex.ToString, Reader.Instance.ColIndex = 1);
  AssertTrue('RowIndex '+Reader.Instance.RowIndex.ToString, Reader.Instance.RowIndex = 3);
end;

procedure TTestCsvReader.IterateRowMask;
var
  Sum: Integer = 0;
  function Callme(r: TCsvReader): Boolean;
  begin
    if r.RowIndex < 2 then exit(True);
    Inc(Sum, r.CurrRowPtr^[1].ToInteger);
    Result := True;
  end;
var
  Reader: TReader;
  s: string;
  m: array[0..1] of Boolean = (false, true);
begin
  {%H-}Reader.Instance := TCsvReader.Create(TStrStream.Create, 42, True);
  Reader.Instance.IterateRow(@Callme);
  AssertTrue(Sum = 0);

  s := 'name, value, other'#10' first, 42, some'#13' second, 1001, any'#10'third, 7, all';
  Reader.Instance := TCsvReader
    .New(Pointer(s), Length(s))
    .SetOptions([roSkipLeadSpaces]);
  Reader.Instance.IterateRow(@Callme, m);
  AssertTrue('Column[0] was still read: '+Reader.Instance.Cell[0], Reader.Instance.Cell[0] = '');
  AssertTrue('Column[2] was still read: '+Reader.Instance.Cell[2], Reader.Instance.Cell[2] = '');
  AssertTrue('Unexpected Sum value: '+Sum.ToString, Sum = 1050);
end;

procedure TTestCsvReader.Bom;
var
  Reader: TReader;
  s: string;
  row: TStringArray;
  il: TCsvReader.TIssueList;
  Raised: Boolean = False;
  r: Integer;
begin
  s := #$ef#$bb#$bf'a,b,c';
  {%H-}Reader.Instance := TCsvReader.Create(TStrStream.Create(s), 0, True);
  Reader.Instance.Options := [roSkipBom];
  AssertTrue('Can not read row starting with UTF-8 BOM', Reader.Instance.ReadRow);
  AssertTrue('Unexpected state after reading a row starting with UTF-8 BOM', Reader.Instance.ReadState = rsEof);
  row := Reader.Instance.CurrentRow;
  AssertTrue(Length(row) = 3);
  AssertTrue((row[0] = 'a') and (row[1] = 'b') and (row[2] = 'c'));
  AssertTrue('Incorrectly recognized UTF-8 BOM', Reader.Instance.BOM = bkUtf8);

  s := #$ff#$fe'a,b,c';
  Reader.Instance := TCsvReader.Create(TStrStream.Create(s), 0, True);
  Reader.Instance.Options := [roSkipBom];
  AssertFalse('Reads row starting with UTF-16 BOM', Reader.Instance.ReadRow);
  AssertTrue('Read state not set to fatal', Reader.Instance.ReadState = rsFatal);
  AssertTrue('Incorrectly recognized UTF-16LE BOM', Reader.Instance.BOM = bkUtf16LE);

  AssertTrue('Event of detection a stream with UTF-16 specification not added to the issues list',
             Reader.Instance.HasIssues);
  il := Reader.Instance.IssueList;
  AssertEquals('Inappropriate event added to issues', ceUnsupportEncoding.ToString, il[0].Reason.ToString);

  Reader.Instance := TCsvReader
    .New(Pointer(s), Length(s))
    .SetOptions([roStrict, roSkipBom]);
  try
    Reader.Instance.ReadRow;
  except
    on e: ECsvRead do begin
      r := e.Reason;
      Raised := True;
    end;
  end;
  AssertTrue('Strict mode ignored', Raised);
  AssertTrue('Inappropriate event in raised exception', r = Integer(ceUnsupportEncoding));
end;

procedure TTestCsvReader.DirtyQuote;
var
  Reader: TReader;
  s: string;
  row: TStringArray;
  il: TCsvReader.TIssueList;
  Raised: Boolean = False;
  r: Integer;
begin
  s := 'aa,bb,c"c,dd';
  {%H-}Reader.Instance := TCsvReader.Create(Pointer(s), Length(s));
  AssertTrue('Unexpected ReadRow result', Reader.Instance.ReadRow);
  AssertTrue('Unrecognized misplaced quotation mark', Reader.Instance.HasIssues);
  il := Reader.Instance.IssueList;
  AssertEquals('Inappropriate event added to issues', ceDirtyQuote.ToString, il[0].Reason.ToString);
  row := Reader.Instance.CurrentRow;
  AssertTrue(row[2], row[2] = 'c"c');

  s := 'aa,bb,cc'#10'11,22,33'#10'xx,y"y,zz';
  Reader.Instance := TCsvReader
    .New(Pointer(s), Length(s))
    .SetOptions([roStrict]);
  try
    Reader.Instance.ReadRow;
    Reader.Instance.ReadRow;
    Reader.Instance.ReadRow;
  except
    on e: ECsvRead do begin
      r := e.Reason;
      Raised := True;
    end;
  end;
  AssertTrue('Strict mode ignored', Raised);
  AssertTrue('Inappropriate event in raised exception', r = Integer(ceDirtyQuote));
end;

procedure TTestCsvReader.BareQuote;
var
  Reader: TReader;
  s: string;
  row: TStringArray;
  il: TCsvReader.TIssueList;
  Raised: Boolean = False;
  r: Integer;
begin
  s := 'aa,"bb"bb",cc';
  {%H-}Reader.Instance := TCsvReader.Create(Pointer(s), Length(s));
  AssertTrue('Unexpected ReadRow result', Reader.Instance.ReadRow);
  AssertTrue('Unrecognized unescaped quotation mark', Reader.Instance.HasIssues);
  il := Reader.Instance.IssueList;
  AssertEquals('Inappropriate event added to issues', ceBareQuote.ToString, il[0].Reason.ToString);
  row := Reader.Instance.CurrentRow;
  AssertTrue(row[1], row[1] = 'bb"bb');

  s := 'aa,"bb",cc'#10'11,"22","33"'#10'"x"x",yy,zz';
  Reader.Instance := TCsvReader
    .New(Pointer(s), Length(s))
    .SetOptions([roStrict]);
  try
    Reader.Instance.ReadRow;
    Reader.Instance.ReadRow;
    Reader.Instance.ReadRow;
  except
    on e: ECsvRead do begin
      r := e.Reason;
      Raised := True;
    end;
  end;
  AssertTrue('Strict mode ignored', Raised);
  AssertTrue('Inappropriate event in raised exception', r = Integer(ceBareQuote));
end;

procedure TTestCsvReader.UnclosedQuote;
var
  Reader: TReader;
  s: string;
  row: TStringArray;
  il: TCsvReader.TIssueList;
  Raised: Boolean = False;
  r: Integer;
begin
  s := 'aa,"bb""bb,cc';
  {%H-}Reader.Instance := TCsvReader.Create(Pointer(s), Length(s));
  AssertTrue('Unexpected ReadRow result', Reader.Instance.ReadRow);
  AssertTrue('Unrecognized unclosed quote', Reader.Instance.HasIssues);
  il := Reader.Instance.IssueList;
  AssertEquals('Inappropriate event added to issues', ceHangQuote.ToString, il[0].Reason.ToString);
  row := Reader.Instance.CurrentRow;
  AssertTrue(Length(row) = 2);
  AssertTrue(row[1], row[1] = 'bb"bb,cc');

  s := 'aa,"bb",cc'#10'11,"22,33'#10'xx,yy,zz';
  Reader.Instance := TCsvReader
    .New(Pointer(s), Length(s))
    .SetOptions([roStrict]);
  try
    Reader.Instance.ReadRow;
    Reader.Instance.ReadRow;
    Reader.Instance.ReadRow;
  except
    on e: ECsvRead do begin
      r := e.Reason;
      Raised := True;
    end;
  end;
  AssertTrue('Strict mode ignored', Raised);
  AssertTrue('Inappropriate event in raised exception', r = Integer(ceHangQuote));
end;

procedure TTestCsvReader.TryReadRow;
var
  Reader: TReader;
  s: string = '';
  row: TStringArray;
  Count: SizeInt;
begin
  Count := TCsvReader.TryReadRow(Pointer(s), Length(s), row);
  AssertTrue(Count = Length(s));
  AssertTrue(row = nil);

  s := #13#10#13#10#13#10;
  Count := TCsvReader.TryReadRow(Pointer(s), Length(s), row);
  AssertTrue(Count = Length(s));
  AssertTrue(row = nil);

  s := s + 'a,b,c';
  Count := TCsvReader.TryReadRow(Pointer(s), Length(s), row);
  AssertTrue(Count = Length(s));
  AssertTrue(THelper.Same(row, ['a','b','c']));

  s := s + #10'1,2,3';
  Count := TCsvReader.TryReadRow(Pointer(s), Length(s), row);
  AssertTrue(Count = 12);
  AssertTrue(THelper.Same(row, ['a','b','c']));

  {%H-}Reader.Instance := TCsvReader
    .New(nil, 0)
    .SetDelimiter(';')
    .SetQuoteMark('`')
    .SetCommentMark('%')
    .SetOptions([roSkipLeadSpaces, roSkipBom, roComments]);

  s := #$ef#$bb#$bf#13#10#13#10'% this is comment'#10'   aa;    `b``b`;cc ';
  Count := TCsvReader.TryReadRow(Pointer(s), Length(s), row, Reader.Instance);
  AssertTrue(Count = Length(s));
  AssertTrue(THelper.Same(row, ['aa', 'b`b', 'cc ']));
end;

procedure TTestCsvReader.Encoding;
type
  Str1251 = type string(1251);
var
  Reader: TReader;
  ms: specialize TGAutoRef<TMemoryStream>;
  a: TStringArray;
  s1251: Str1251;
  s: string;
  us: unicodestring;
  b: TBytes;
  I: Integer;
begin
  a := ['а,б,в', 'г,д,е', 'ё,ж,з'];
  s1251 := string.Join(string(#10), a);
  ms.Instance.WriteBuffer(Pointer(s1251)^, Length(s1251));
  ms.Instance.Position := 0;
  {%H-}Reader.Instance := TCsvReader.CreateCp(ms.Instance, 1251);
  I := 0;
  while Reader.Instance.ReadRow do begin
    s := string.Join(',', Reader.Instance.CurrentRow);
    AssertTrue(s, s = a[I]);
    Inc(I);
  end;

  us := unicodestring(string.Join(string(#10), a));
  b := TEncoding.GetEncoding(StringCodePage(us)).GetPreamble;
  ms.Instance.Clear;
  ms.Instance.WriteBuffer(b[0], Length(b));
  ms.Instance.WriteBuffer(us[1], Length(us)*StringElementSize(us));
  ms.Instance.Position := 0;
  Reader.Instance := TCsvReader.CreateCp(ms.Instance, StringCodePage(us));
  I := 0;
  while Reader.Instance.ReadRow do begin
    s := string.Join(',', Reader.Instance.CurrentRow);
    AssertTrue(s, s = a[I]);
    Inc(I);
  end;
end;

{ TTestCsvWriter }

procedure TTestCsvWriter.Delimiter;
var
  Stream: TStrStream;
  Writer: TWriter;
  s: string;
  Raised: Boolean = False;
begin
  Writer.Instance := TCsvWriter.New(Stream.Instance);
  AssertTrue('Default delimiter is not a comma', Writer.Instance.Delimiter = ',');

  Writer.Instance.Delimiter := ';';
  AssertTrue('Delimiter setter does not work', Writer.Instance.Delimiter = ';');

  Writer.Instance.Delimiter := #9;
  AssertTrue('Refuses to set Tab as delimiter', Writer.Instance.Delimiter = #9);

  try
    Writer.Instance.Delimiter := #10;
  except
    on e: ECsvSpecialMark do
      Raised := True;
  end;
  AssertTrue('Accepts #10 as a delimiter', Raised);

  Writer.Instance.Delimiter := ':';

  Writer.Instance.AddCell('1');
  Writer.Instance.AddCell('2');
  Writer.Instance.AddCell('3');
  Writer.Clear;

  s := Stream.Instance.DataString;
  AssertTrue(s, s = '1:2:3');
end;

procedure TTestCsvWriter.QuoteMark;
var
  Stream: TStrStream;
  Writer: TWriter;
  s: string;
  Raised: Boolean = False;
begin
  Writer.Instance := TCsvWriter.New(Stream.Instance);
  AssertTrue('Default quotation mark is not a double quote', Writer.Instance.QuoteMark = '"');

  Writer.Instance.QuoteMark := '`';
  AssertTrue('Quotation mark setter does not work', Writer.Instance.QuoteMark = '`');

  try
    Writer.Instance.QuoteMark := #9;
  except
    on e: ECsvSpecialMark do
      Raised := True;
  end;
  AssertTrue('Accepts #9 as a quotation mark', Raised);

  Writer.Instance.AddCell('1');
  Writer.Instance.AddCell('2,3');
  Writer.Instance.AddCell('4');
  Writer.Clear;

  s := Stream.Instance.DataString;
  AssertTrue(s, s = '1,`2,3`,4');
end;

procedure TTestCsvWriter.LineEndStyle;
var
  Stream: TStrStream;
  Writer: TWriter;
  s: string;
begin
  {%H-}Writer.Instance := TCsvWriter.New(Stream.Instance);
  AssertTrue('Default line ending style is not lesCRLF', Writer.Instance.LineEndStyle = lesCRLF);

  Writer.Instance.AddCell('a');
  {%H-}Writer.Instance.AddEol;
  Writer.Clear;
  s := Stream.Instance.DataString;
  AssertTrue('lesCRLF ignored', s = 'a'#13#10);

  Stream.Clear;
  Writer.Instance := TCsvWriter.New(Stream.Instance).SetLineEndStyle(lesCR);
  Writer.Instance.AddCell('a');
  {%H-}Writer.Instance.AddEol;
  Writer.Clear;
  s := Stream.Instance.DataString;
  AssertTrue('lesCR ignored', s = 'a'#13);

  Stream.Clear;
  Writer.Instance := TCsvWriter.New(Stream.Instance).SetLineEndStyle(lesLF);
  Writer.Instance.AddCell('a');
  {%H-}Writer.Instance.AddEol;
  Writer.Clear;
  s := Stream.Instance.DataString;
  AssertTrue('lesCR ignored', s = 'a'#10);
end;

procedure TTestCsvWriter.QuotePolicy;
var
  Stream: TStrStream;
  Writer: TWriter;
  s: string;
begin
  {%H-}Writer.Instance := TCsvWriter.New(Stream.Instance);
  AssertTrue('Default quotation policy is not qpAsNeeded', Writer.Instance.QuotePolicy = qpAsNeeded);

  {%H-}Writer.Instance.AddRow(['aa', 'b,b']);
  Writer.Clear;
  s := Stream.Instance.DataString;
  AssertTrue('Quotation policy qpAsNeeded ignored', s = 'aa,"b,b"'#13#10);

  Stream.Clear;
  Writer.Instance := TCsvWriter.New(Stream.Instance).SetQuotePolicy(qpForce);
  {%H-}Writer.Instance.AddRow(['aa', 'b,b']);
  Writer.Clear;
  s := Stream.Instance.DataString;
  AssertTrue('Quotation policy qpForce ignored', s = '"aa","b,b"'#13#10);
end;

{ TTestCsvDoc }

procedure TTestCsvDoc.CsvText;
var
  Doc: TDoc;
  s, r: string;
begin
  s := 'a,b,c'#10'1,2,3'#10'x,"y;0",z';
  {%H-}Doc.Instance := TCsvDoc.New.SetCsvText(s);
  AssertTrue(Doc.Instance.RowCount = 3);
  with Doc.Instance do
    AssertTrue((ColCount[0] = 3) and (ColCount[1] = 3) and (ColCount[2] = 3));

  Doc.Instance.SetLineEndStyle(lesCR).SetDelimiter(';').SetQuoteMark('`');
  r := Doc.Instance.CsvText;
  AssertEquals('Unexpected CsvText', 'a;b;c'#13'1;2;3'#13'x;`y;0`;z'#13, r);

  Doc.Clear;
  Doc.Instance := TCsvDoc.New.SetCsvText(s, [false, true]);
  AssertEquals('Unexpected row count', 3, Doc.Instance.RowCount);
  with Doc.Instance do
    begin
      AssertEquals('Unexpected ColCount[0]', 1, ColCount[0]);
      AssertEquals('Unexpected ColCount[1]', 1, ColCount[1]);
      AssertEquals('Unexpected ColCount[2]', 1, ColCount[2]);
      AssertEquals('Unexpected Cells[0, 0]', 'b', Cells[0, 0]);
      AssertEquals('Unexpected Cells[1, 0]', '2', Cells[1, 0]);
      AssertEquals('Unexpected Cells[2, 0]', 'y;0', Cells[2, 0]);
    end;

  s := 'a,b,c,d'#10'1,2,3,4'#10'; some comment'#13'w,x,y,z';
  Doc.Instance := TCsvDoc
    .New.SetOptions([roComments])
    .SetCommentMark(';')
    .SetCsvText(s, [false, true, true]);
  AssertTrue(Doc.Instance.RowCount = 3);
  with Doc.Instance do
    AssertTrue((ColCount[0] = 2) and (ColCount[1] = 2) and (ColCount[2] = 2));
end;

procedure TTestCsvDoc.HeaderColCount;
var
  Doc: TDoc;
  s: string;
begin
  AssertEquals('Unexpected HeaderColCount of empty document', -1, Doc.Instance.HeaderColCount);

  s := 'a,b,c'#10'1,2'#10'x';
  Doc.Instance.CsvText := s;
  AssertEquals('Unexpected HeaderColCount', 3, Doc.Instance.HeaderColCount);
end;

procedure TTestCsvDoc.MinColCount;
var
  Doc: TDoc;
  s: string;
begin
  AssertEquals('Unexpected MinColCount of empty document', -1, Doc.Instance.MinColCount);

  s := 'a,b,c'#10'1,2'#10'x';
  Doc.Instance.CsvText := s;
  AssertEquals('Unexpected MinColCount', 1, Doc.Instance.MinColCount);
end;

procedure TTestCsvDoc.MaxColCount;
var
  Doc: TDoc;
  s: string;
begin
  AssertEquals('Unexpected MaxColCount of empty document', -1, Doc.Instance.MaxColCount);

  s := 'a,b,c'#10'1,2'#10'x';
  Doc.Instance.CsvText := s;
  AssertEquals('Unexpected MaxColCount', 3, Doc.Instance.MaxColCount);
end;

procedure TTestCsvDoc.ColumnCount;
var
  Doc: TDoc;
  s: string;
  Raised: Boolean = False;
  row, len: Integer;
  r: Integer;
const
  LoadFmt = 'Unexpected number of rows after load when ColCountPolicy = %s';
begin
  s := 'a,b,c'#13'c,d'#13'e';
  Doc.Instance.CsvText := s;
  AssertEquals(Format(LoadFmt, [Doc.Instance.ColCountPolicy.ToString]), 3, Doc.Instance.RowCount);
  with Doc.Instance do begin
    AssertEquals('Wrong number of cells in row 0(ccpAccept)', 3, ColCount[0]);
    AssertEquals('Wrong number of cells in row 1(ccpAccept)', 2, ColCount[1]);
    AssertEquals('Wrong number of cells in row 2(ccpAccept)', 1, ColCount[2]);
  end;

  Doc.Instance := TCsvDoc.New.SetColCountPolicy(ccpIgnore);
  Doc.Instance.CsvText := s;
  AssertEquals(Format(LoadFmt, [Doc.Instance.ColCountPolicy.ToString]), 1, Doc.Instance.RowCount);
  AssertEquals('Wrong number of cells in row 0(ccpIgnore)', 3, Doc.Instance.ColCount[0]);

  Doc.Instance := TCsvDoc.New.SetColCountPolicy(ccpError);
  try
    Doc.Instance.CsvText := s;
  except
    on e: ECsvRead do begin
      Raised := True;
      len := e.Column;
      row := e.Row;
      r := e.Reason;
    end;
  end;
  AssertTrue('ccpError policy ignored', Raised);
  AssertTrue('Inappropriate event in raised exception', r = Integer(ceColumnCount));
  AssertEquals('Wrong row length in exception', 2, len);
  AssertEquals('Wrong row index in exception', 1, row);

  s := 'a,b,c'#13'c,d,2'#13'e,3, 4';
  Raised := False;
  Doc.Instance := TCsvDoc.New.SetColCountPolicy(ccpError);
  try
    Doc.Instance.CsvText := s;
  except
    on e: ECsvRead do
      Raised := True;
  end;
  AssertFalse('Raises an exception in the correct document', Raised);
end;

procedure TTestCsvDoc.AssignDoc;
var
  d1, d2: TDoc;
  s: string;
  I, J: Integer;
  il: TCsvDoc.TIssueList;
begin
  s := 'a,b,c'#13'1,2,3'#13'x,y"y,z';
  {%H-}d1.Instance := TCsvDoc.New.SetCsvText(s);
  d2.Instance.Assign(d1.Instance);
  AssertTrue(d2.Instance.RowCount = d1.Instance.RowCount);
  for I := 0 to d1.Instance.RowCount - 1 do
    begin
      AssertTrue(d2.Instance.ColCount[I] = d1.Instance.ColCount[I]);
      for J := 0 to d2.Instance.ColCount[I] - 1 do
        AssertTrue(d2.Instance[I, J] = d1.Instance[I, J]);
    end;
  AssertTrue(d2.Instance.HasIssues);
  il := d2.Instance.IssueList;
  AssertTrue(Length(il) = 1);
  AssertTrue((il[0].Row = 2) and (il[0].Column = 1) and (il[0].Reason = ceDirtyQuote));
end;

procedure TTestCsvDoc.InsertRow;
var
  Doc: TDoc;
  Raised: Boolean = False;
  p: TCsvDoc.PRow;
begin
  try
    Doc.Instance.InsertRow(1, ['a', 'b']);
  except
    on e: EArgumentOutOfRangeException do
      Raised := True;
  end;
  AssertTrue(Raised);

  Doc.Instance.InsertRow(0, ['a', 'b']);
  AssertTrue(Doc.Instance.RowCount = 1);
  p := Doc.Instance.InsertRow(0, 2);
  p^[0] := '1';
  p^[1] := '2';
  AssertTrue(Doc.Instance.RowCount = 2);
  AssertTrue(Doc.Instance[0, 0] = '1');
  AssertTrue(Doc.Instance[0, 1] = '2');
end;

procedure TTestCsvDoc.AddRow;
var
  Doc: TDoc;
  p: TCsvDoc.PRow;
begin
  AssertTrue(Doc.Instance.RowCount = 0);
  AssertTrue(Doc.Instance.AddRow(['a', 'b']) = 0);
  AssertTrue(Doc.Instance.RowCount = 1);
  AssertTrue(Doc.Instance.AddRow(['c', 'd']) = 1);

  p := Doc.Instance.AddRow(-42);
  AssertTrue(Length(p^) = 0);
  AssertTrue(Doc.Instance.RowCount = 3);
  SetLength(p^, 2);
  p^[0] := '1';
  p^[1] := '2';
  AssertTrue(Doc.Instance[2, 0] = '1');
  AssertTrue(Doc.Instance[2, 1] = '2');

  p := Doc.Instance.AddRow(2);
  AssertTrue(Length(p^) = 2);
  AssertTrue(Doc.Instance.RowCount = 4);
  p^[0] := 'x';
  p^[1] := 'y';

  AssertTrue(Doc.Instance[3, 0] = 'x');
  AssertTrue(Doc.Instance[3, 1] = 'y');
end;

procedure TTestCsvDoc.DeleteRow;
var
  Doc: TDoc;
  row: TStringArray;
  Raised: Boolean = False;
begin
  try
    Doc.Instance.DeleteRow(0);
  except
    on e: EArgumentOutOfRangeException do
      Raised := True;
  end;
  AssertTrue(Raised);

  Doc.Instance.CsvText := 'a,b,c'#13'd,e,f'#13'x,y,z';
  AssertTrue(Doc.Instance.RowCount = 3);

  row := Doc.Instance.DeleteRow(1);
  AssertTrue(Doc.Instance.RowCount = 2);
  AssertTrue(Length(row) = 3);
  AssertTrue((row[0] = 'd') and (row[1] = 'e') and (row[2] = 'f'));
end;

procedure TTestCsvDoc.SwapRow;
var
  Doc: TDoc;
  I1, I2: Integer;
  p: TCsvDoc.PRow;
begin
  Doc.Instance.CsvText := 'a,b,c'#13'd,e,f'#13'x,y,z';
  I1 := 1;
  I2 := 3;
  AssertFalse(Doc.Instance.SwapRow(I1, I2));

  I1 := 0;
  I2 := 2;
  AssertTrue(Doc.Instance.SwapRow(I1, I2));
  p := Doc.Instance.MutRows[I1];
  AssertTrue((p^[0] = 'x') and (p^[1] = 'y') and (p^[2] = 'z'));
  p := Doc.Instance.MutRows[I2];
  AssertTrue((p^[0] = 'a') and (p^[1] = 'b') and (p^[2] = 'c'));
end;

procedure TTestCsvDoc.InsertCell;
var
  Doc: TDoc;
  Raised: Boolean = False;
begin
  try
    Doc.Instance.InsertCell(0, 0, 'a');
  except
    on e: EArgumentOutOfRangeException do
      Raised := True;
  end;
  AssertTrue(Raised);

  Raised := False;
  Doc.Instance.CsvText := 'a,b'#13'd,e'#13'x,y';
  try
    Doc.Instance.InsertCell(0, 3, 'a');
  except
    on e: EArgumentOutOfRangeException do
      Raised := True;
  end;
  AssertTrue(Raised);

  Doc.Instance.InsertCell(0, 0, '1');
  AssertTrue(Doc.Instance.ColCount[0] = 3);
  with Doc.Instance do
    AssertTrue((Cells[0, 0] = '1') and (Cells[0, 1] = 'a') and (Cells[0, 2] = 'b'));

  Doc.Instance.InsertCell(1, 2, 'f');
  AssertTrue(Doc.Instance.ColCount[1] = 3);
  with Doc.Instance do
    AssertTrue((Cells[1, 0] = 'd') and (Cells[1, 1] = 'e') and (Cells[1, 2] = 'f'));
end;

procedure TTestCsvDoc.AddCell;
var
  Doc: TDoc;
  Raised: Boolean = False;
begin
  try
    Doc.Instance.AddCell(0, 'a');
  except
    on e: EArgumentOutOfRangeException do
      Raised := True;
  end;
  AssertTrue(Raised);

  Doc.Instance.CsvText := 'a,b'#13'd,e'#13'x,y';
  Doc.Instance.AddCell(1, 'c');
  AssertTrue(Doc.Instance.ColCount[1] = 3);
  with Doc.Instance do
    AssertTrue((Cells[1, 0] = 'd') and (Cells[1, 1] = 'e') and (Cells[1, 2] = 'c'));
end;

procedure TTestCsvDoc.RowExists;
var
  Doc: TDoc;
begin
  AssertFalse(Doc.Instance.RowExists(0));

  Doc.Instance.CsvText := 'a,b'#13'd,e'#13'x,y';
  AssertFalse(Doc.Instance.RowExists(-1));
  AssertFalse(Doc.Instance.RowExists(42));
  with Doc.Instance do
    AssertTrue(RowExists(0) and RowExists(1) and RowExists(2));
end;

procedure TTestCsvDoc.CellExists;
var
  Doc: TDoc;
begin
  AssertFalse(Doc.Instance.CellExists(0, 0));

  Doc.Instance.CsvText := 'a,b'#13'd,e'#13'x,y';
  AssertFalse(Doc.Instance.CellExists(-1, 0){%H-});
  AssertFalse(Doc.Instance.CellExists(42, 5));
  AssertFalse(Doc.Instance.CellExists(0, 2));
  with Doc.Instance do begin
    AssertTrue(CellExists(0, 0) and CellExists(0, 1));
    AssertTrue(CellExists(1, 0) and CellExists(1, 1));
    AssertTrue(CellExists(2, 0) and CellExists(2, 1));
  end;
end;

procedure TTestCsvDoc.IndexOfCol;
var
  Doc: TDoc;
begin
  AssertTrue(Doc.Instance.IndexOfCol(0, 'a') = -1);

  Doc.Instance.CsvText := 'a,b,c'#13'd,e,f'#13'x,y,z';
  AssertTrue(Doc.Instance.IndexOfCol(0, 'e') = -1);
  AssertTrue(Doc.Instance.IndexOfCol(2, 'e') = -1);
  AssertTrue(Doc.Instance.IndexOfCol(5, 'e') = -1);
  AssertTrue(Doc.Instance.IndexOfCol(0, 'c') = 2);
  AssertTrue(Doc.Instance.IndexOfCol(1, 'e') = 1);
  AssertTrue(Doc.Instance.IndexOfCol(2, 'x') = 0);
end;

procedure TTestCsvDoc.IndexOfRow;
var
  Doc: TDoc;
begin
  AssertTrue(Doc.Instance.IndexOfRow(0, 'a') = -1);

  Doc.Instance.CsvText := 'a,b,c'#13'd,e,f'#13'x,y,z';
  AssertTrue(Doc.Instance.IndexOfRow(2, 'a') = -1);

  AssertTrue(Doc.Instance.IndexOfRow(1, 'b') = 0);
  AssertTrue(Doc.Instance.IndexOfRow(2, 'f') = 1);
  AssertTrue(Doc.Instance.IndexOfRow(0, 'x') = 2);
end;

procedure TTestCsvDoc.TrimTrailingBlanks;
var
  Doc: TDoc;
  p: TCsvDoc.PRow;
  I: Integer;
begin
  for I := 1 to 5 do
    begin
      p := Doc{%H-}.Instance.AddRow(5);
      p^[0] := I.ToString;
    end;
  AssertTrue(Doc.Instance.TrimTrailingBlanks(2) = 4);
  AssertTrue(Doc.Instance.TrimTrailingBlanks = 16);
  for I := 0 to Doc.Instance.RowCount - 1 do
    AssertTrue(Length(Doc.Instance.MutRows[I]^) = 1);
end;

initialization

  RegisterTest(TTestCsvReader);
  RegisterTest(TTestCsvWriter);
  RegisterTest(TTestCsvDoc);

end.

