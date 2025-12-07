{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Comma Separated Value (CSV) file format utilities,                      *
*   which try to follow the format described in RFC 4180.                   *
*                                                                           *
*   Copyright(c) 2022-2025 A.Koverdyaev(avk)                                *
*                                                                           *
*   This code is free software; you can redistribute it and/or modify it    *
*   under the terms of the Apache License, Version 2.0;                     *
*   You may obtain a copy of the License at                                 *
*     http://www.apache.org/licenses/LICENSE-2.0.                           *
*                                                                           *
*  Unless required by applicable law or agreed to in writing, software      *
*  distributed under the License is distributed on an "AS IS" BASIS,        *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *
*  See the License for the specific language governing permissions and      *
*  limitations under the License.                                           *
*                                                                           *
*****************************************************************************}
unit lgCsvUtils;

{$MODE OBJFPC}{$H+}{$MODESWITCH ADVANCEDRECORDS}{$MODESWITCH NESTEDCOMMENTS}
{$MODESWITCH ARRAYOPERATORS}{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils,
  lgUtils,
  lgVector,
  lgStrConst;

type

  ECsvSpecialMark = class(EArgumentException);
  ECsvRead = class(Exception)
  private
    FRow, FCol: SizeInt;
    FReason: Integer;
  public
    constructor Create(aRow, aCol: SizeInt; aReason: Integer; const aMsgFmt, aCell: string); overload;
    constructor Create(aRow, aCol: SizeInt; aReason: Integer; const aMsgFmt: string); overload;
    constructor Create(aReason: Integer; const aMsg: string); overload;
    property Reason: Integer read FReason;
    property Row: SizeInt read FRow;
    property Column: SizeInt read FCol;
  end;

  { TCsvEntity: abstract common ancestor class }
  TCsvEntity = class abstract(TPersistent)
  public
  type
    TReadOption    = (
      roSkipLeadSpaces,{ leading white spaces in cells will be skipped }
      roSkipBom,       { UTF-8 BOM will be skipped }
      roComments,      { single-line comments starting with a comment sign with no leading
                         white spaces are allowed(will just be ignored) }
      roStrict         { ECsvRead will be raised when a problem with reading a CSV document happens;
                         otherwise, the list of encountered problem cells can be viewed in the IssueList }
      );
    TReadOptions  = set of TReadOption;
    TLineEndStyle = (lesCRLF, lesLF, lesCR);
    TQuotePolicy  = (qpAsNeeded, qpForce);
    TCsvEvent     = (
      ceNone,            // no problem
      ceDirtyQuote,      // encountered a quotation mark in an unquoted cell
      ceBareQuote,       // encountered an unescaped quotation mark in a quoted cell
      ceHangQuote,       // missing the closing quotation mark of the quoted cell
      ceColumnCount,     // number of columns in the row does not match the header
      ceEmptyDoc,        // loading document does not contain a single byte of data
      ceUnsupportEncoding// when non-UTF-8 BOM is recognized
    );
    TCsvIssue = record
      Row, Column: SizeInt;
      Reason: TCsvEvent;
    private
      class function Make(aRow, aCol: SizeInt; e: TCsvEvent): TCsvIssue; static; inline;
    end;
    TIssueList = array of TCsvIssue;
    PRow       = ^TStringArray;

  const
    DEF_BUF_SIZE = 1 shl 15;// default read buffer size

  protected
    FDelimiter,
    FQuoteMark,
    FCommentMark: Char;
    FOptions: TReadOptions;
    FLEStyle: TLineEndStyle;
    FQuotePolicy: TQuotePolicy;
    procedure CheckDelimiterChar(d: Char); inline;
    procedure SetDelimiterChar(d: Char); virtual;
    procedure CheckQuoteChar(qc: Char); inline;
    procedure SetQuoteChar(qc: Char); virtual;
    procedure CheckCommentChar(cc: Char); inline;
    procedure SetCommentChar(cc: Char); virtual;
    procedure SetOpts(const ro: TReadOptions);
    procedure SetLEStyle(les: TLineEndStyle); virtual;
    procedure DoSetQuotePolicy(qp: TQuotePolicy);
  public
    constructor Create;
    procedure AssignCsvProps(aSource: TCsvEntity); virtual;
  { field delimiter, comma by default; must be a character from the first half of the ASCII table,
    #32 <= c <= #126(also #9 allowed), other than a quotation mark, comment mark, line feed,
    and carriage return; raises an exception when trying to set an invalid value }
    property  Delimiter: Char read FDelimiter write SetDelimiterChar;
  { quotation mark, double quote by default; must be a character from the first half of the
    ASCII table, greater than #32, less than #127, other than a delimiter mark, comment mark,
    line feed, and carriage return; raises an exception when trying to set an invalid value }
    property  QuoteMark: Char read FQuoteMark write SetQuoteChar;
  { comment mark, octothorpe by default; only makes sense if it is the first character in a row
    that does not contain leading spaces and roComments option is enabled; must be a character
    from the first half of the ASCII table, greater than #32, less than #127, other than a delimiter
    mark, quotation mark, line feed, and carriage return; raises an exception when trying
    to set an invalid value }
    property  CommentMark: Char read FCommentMark write SetCommentChar;
    property  Options: TReadOptions read FOptions write SetOpts;
  { characters used to terminate lines when writing, by default lesCRLF }
    property  LineEndStyle: TLineEndStyle read FLEStyle write SetLEStyle;
  { controls when quotes should be generated when writing fields, qpAsNeeded by default }
    property  QuotePolicy: TQuotePolicy read FQuotePolicy write DoSetQuotePolicy;
  end;

  TCsvReader      = class;
  TCsvReaderClass = class of TCsvReader;

  { TCsvDoc }
  TCsvDoc = class(TCsvEntity)
  public
  type
    TColCountPolicy = (ccpAccept, ccpIgnore, ccpError);
    TRowCompare     = function(const L, R: TStringArray): Boolean;
    TOnRowCompare   = function(const L, R: TStringArray): Boolean of object;
    TNestRowCompare = function(const L, R: TStringArray): Boolean is nested;

  const
    MT_MIN_CHUNK_SIZE = SizeInt($400000); //todo: need some tweaking

  private
  type
    TStrList2D  = specialize TGVector<TStringArray>;

  var
    FCells: TStrList2D;
    FIssueList: TIssueList;
    FCcPolicy: TColCountPolicy;
    procedure CheckRowIndex(aIndex: SizeInt); inline;
    procedure CheckRowInsertIndex(aIndex: SizeInt); inline;
    procedure CheckColIndex(aRow: PRow; aIndex: SizeInt); inline;
    procedure CheckColInsertIndex(aRow: PRow; aIndex: SizeInt); inline;
    function  GetCell(aRow, aCol: SizeInt): string; inline;
    procedure SetCell(aRow, aCol: SizeInt; const aValue: string);
    function  GetColCount(aRow: SizeInt): SizeInt; inline;
    function  GetMutRow(aIndex: SizeInt): PRow; inline;
    function  GetRow(aIndex: SizeInt): TStringArray;
    function  GetRowCount: SizeInt; inline;
    function  GetHasIssues: Boolean; inline;
    function  GetIssueCount: SizeInt; inline;
    function  GetIssueList: TIssueList; inline;
    function  GetHeadColCount: SizeInt; inline;
    function  GetMaxColCount: SizeInt;
    function  GetMinColCount: SizeInt;
    function  GetReaderClass: TCsvReaderClass;
    function  GetCsvText: string;
    procedure DoSetCsvText(const aValue: string);
    procedure LoadStream(s: TStream; aBufSize: SizeInt; aOwnsStream: Boolean);
    procedure LoadStreamCp(s: TStream; aCodePage: Integer; aOwnsStream: Boolean);
    procedure LoadStream(s: TStream; aBufSize: SizeInt; aOwnsStream: Boolean;
                         const aMask: array of Boolean);
    procedure LoadStreamCp(s: TStream; aCodePage: Integer; aOwnsStream: Boolean;
                           const aMask: array of Boolean);
    procedure LoadFile(const aFileName: string);
    procedure LoadFile(const aFileName: string; aCodePage: Integer);
    procedure LoadFile(const aFileName: string; const aMask: array of Boolean);
    procedure LoadFile(const aFileName: string; const aMask: array of Boolean; aCodePage: Integer);
    procedure DoLoadBuffer(aBuffer: PChar; aCount: SizeInt);
    procedure DoLoadBuffer(aBuffer: PChar; aCount: SizeInt; const aMask: array of Boolean);
  protected
    procedure AssignTo(aDest: TPersistent); override;
  public
    class function New: TCsvDoc; static; inline;
    class function New(const a: array of TStringArray): TCsvDoc; static;
    constructor Create;
    constructor Create(const a: array of TStringArray);
    destructor Destroy; override;
    function  SetDelimiter(d: Char): TCsvDoc; inline;
    function  SetQuoteMark(qm: Char): TCsvDoc; inline;
    function  SetCommentMark(cm: Char): TCsvDoc; inline;
    function  SetOptions(const op: TReadOptions): TCsvDoc; inline;
    function  SetLineEndStyle(les: TLineEndStyle): TCsvDoc; inline;
    function  SetColCountPolicy(ccp: TColCountPolicy): TCsvDoc; inline;
    function  SetQuotePolicy(qp: TQuotePolicy): TCsvDoc; inline;
    function  SetCsvText(const aValue: string): TCsvDoc;
  { reads only masked columns(i.e. aMask[Col]=True), ignoring the ColCountPolicy property;
    issues, if any, are only logged for masked columns; if aMask is empty, all columns
    are loaded; returns Self }
    function  SetCsvText(const aValue: string; const aMask: array of Boolean): TCsvDoc;
  { loads document from stream, aBufSize indicates the desired buffer size, if aBufSize
    is less than 1, the buffer size is set equal to the stream size; if aOwnsStream is set
    to True, the CsvDoc is responsible for destroying source stream; returns Self }
    function  LoadFromStream(aSource: TStream; aBufSize: SizeInt; aOwnsStream: Boolean): TCsvDoc;
    function  LoadFromStream(aSource: TStream; aOwnsStream: Boolean; aCodePage: Integer): TCsvDoc;
  { loads only masked columns(i.e. aMask[Col]=True) from the stream, ignoring the ColCountPolicy
    property; issues, if any, are only logged for masked columns; if aMask is empty,
    all columns are loaded; returns Self }
    function  LoadFromStream(aSource: TStream; aBufSize: SizeInt; aOwnsStream: Boolean;
                             const aMask: array of Boolean): TCsvDoc;
    function  LoadFromStream(aSource: TStream; aOwnsStream: Boolean; const aMask: array of Boolean;
                             aCodePage: Integer): TCsvDoc;
    function  LoadFromStream(aSource: TStream): TCsvDoc;
  { the same as above using the field mask }
    function  LoadFromStream(aSource: TStream; const aMask: array of Boolean): TCsvDoc;
  { will try to load document from a stream in multi-threaded mode; aThreadLimit sets the limit
    on the number of threads, any value less than 1 implies TThread.ProcessorCount; returns Self }
    function  LoadFromStreamMT(aSource: TStream; aThreadLimit: Integer = 0): TCsvDoc; experimental;
  { the same as above using the field mask }
    function  LoadFromStreamMT(aSource: TStream; const aMask: array of Boolean;
                               aThreadLimit: Integer = 0): TCsvDoc; experimental;
  { loads document from file; returns Self }
    function  LoadFromFile(const aFileName: string): TCsvDoc;
    function  LoadFromFile(const aFileName: string; aCodePage: Integer): TCsvDoc;
  { the same as above using the field mask }
    function  LoadFromFile(const aFileName: string; const aMask: array of Boolean): TCsvDoc;
    function  LoadFromFile(const aFileName: string; const aMask: array of Boolean;
                           aCodePage: Integer): TCsvDoc;
  { will try to load document from file in multi-threaded mode; returns Self }
    function  LoadFromFileMT(const aFileName: string; aThreadLimit: Integer = 0): TCsvDoc; experimental;
  { the same as above using the field mask }
    function  LoadFromFileMT(const aFileName: string; const aMask: array of Boolean;
                             aThreadLimit: Integer = 0): TCsvDoc; experimental;
  { loads the document directly from aSrcBuffer; returns Self }
    function  LoadBuffer(aSrcBuffer: PChar; aCount: SizeInt): TCsvDoc;
  { the same as above using the field mask }
    function  LoadBuffer(aSrcBuffer: PChar; aCount: SizeInt; const aMask: array of Boolean): TCsvDoc;
  { returns the number of bytes written }
    function  SaveToStream(aStream: TStream; aBufSize: SizeInt): SizeInt;
    function  SaveToStream(aStream: TStream): SizeInt;
    function  SaveToFile(const aFileName: string): SizeInt;

    procedure ClearIssues;
    procedure Clear;
    procedure TrimToFit; inline;
    function  IsEmpty: Boolean; inline;

    function  RowExists(aIndex: SizeInt): Boolean; inline;
    function  CellExists(aRow, aCol: SizeInt): Boolean; inline;
  { returns the column index for cell aValue at row aRow, -1 if not found }
    function  IndexOfCol(aRow: SizeInt; const aValue: string): SizeInt;
  { returns row index for cell aValue at column aCol, -1 if not found }
    function  IndexOfRow(aCol: SizeInt; const aValue: string): SizeInt;
  { appends the specified cell aValue to an existing row specified by the aRow index;
    returns the column index of the added cell; will raise exception if aRow is out of bounds }
    function  AddCell(aRow: SizeInt; const aValue: string): SizeInt;
  { inserts the specified cell aValue into an existing row specified by the aRow index;
    will raise exception if aRow or aCol is out of bounds(aCol = ColCount is allowed) }
    procedure InsertCell(aRow, aCol: SizeInt; const aValue: string);
  { adds the specified row to the end of the list of rows; returns the index of the added row }
    function  AddRow(const aRow: array of string): SizeInt;
  { adds a row consisting of Max(aColCount, 0) empty cells to the end of the list of rows
    and returns a pointer to it }
    function  AddRow(aColCount: SizeInt): PRow;
  { inserts the specified row at the position specified by the aIndex parameter;
    will raise exception if aIndex out of bounds(aIndex = RowCount is allowed) }
    procedure InsertRow(aIndex: SizeInt; const aRow: array of string);
  { inserts a row consisting of Max(aColCount, 0) empty cells at the position specified
    by the aIndex parameter and returns a pointer to it; will raise exception if aIndex
    out of bounds(aIndex = RowCount is allowed) }
    function  InsertRow(aIndex: SizeInt; aColCount: SizeInt): PRow;
  { removes the row specified in the aIndex parameter from the list and returns it as a result;
    will raise exception if aIndex out of bounds }
    function  DeleteRow(aIndex: SizeInt): TStringArray;
  { returns false if Row[L] or Row[R] does not exist }
    function  SwapRow(L, R: SizeInt): Boolean;
  { removes blank cells at the end of row with index aRow, if it exists; returns the number of
    removed cells }
    function  TrimTrailingBlanks(aRow: SizeInt): SizeInt;
  { removes blank cells at the end of rows in the entire document; returns the number of removed cells }
    function  TrimTrailingBlanks: SizeInt;
  { sorts rows using specified comparator c }
    procedure Sort(c: TRowCompare; aWithHeader: Boolean = False);
    procedure Sort(c: TOnRowCompare; aWithHeader: Boolean = False);
    procedure Sort(c: TNestRowCompare; aWithHeader: Boolean = False);
  { on document load defines behavior when rows are encountered whose number of columns
    does not match the number of header columns, ccpAccept by default.}
    property  ColCountPolicy: TColCountPolicy read FCcPolicy write FCcPolicy;
    property  MutRows[aIndex: SizeInt]: PRow read GetMutRow;
    property  Rows[aIndex: SizeInt]: TStringArray read GetRow;
    property  Cells[aRow, aCol: SizeInt]: string read GetCell write SetCell; default;
    property  ColCount[aRow: SizeInt]: SizeInt read GetColCount;
    property  RowCount: SizeInt read GetRowCount;
    property  HasIssues: Boolean read GetHasIssues;
    property  IssueCount: SizeInt read GetIssueCount;
    property  IssueList: TIssueList read GetIssueList;
    property  HeaderColCount: SizeInt read GetHeadColCount;
  { minimum column count, -1 if empty; O(N) }
    property  MinColCount: SizeInt read GetMinColCount;
  { maximum column count, -1 if empty; O(N) }
    property  MaxColCount: SizeInt read GetMaxColCount;
    property  CsvText: string read GetCsvText write DoSetCsvText;
  end;

  { TCsvReader: pull-style CSV reader, provides forward navigation through the CSV document;
    each TCsvReader instance can process a single CSV document }
  TCsvReader = class(TCsvEntity)
  public
  type
    TOnIterate   = function(r: TCsvReader): Boolean of object;
    TNestIterate = function(r: TCsvReader): Boolean is nested;

    TReadState = (rsStart, rsRead, rsEof, rsFatal);
    TParseState = (
      psBeginRow, //
      psLeadingWS,// leading white space
      psOpenQM,   // opening quotation mark
      psCell,     //
      psQmCell,   // maybe cell enclosed with quotes
      psEscape,   // maybe escaped quotation mark
      psEscapedQM,// escaped quotation mark
      psEndCell,  //
      psComment,  //
      psEndRow,   //
      psQmAfterWS,// quotation mark following a white space
      psDirtyQM,  // irrelevant quotation mark
      psBareQM,   // unescaped quotation mark
      psEmpty,    //
      psEncoding  //
    );
  private
  type
    TCsvToken = (ctChar, ctWhiteSpace, ctDelimiter, ctQuote, ctComment, ctEol);
    TCsvTable = array[char] of TCsvToken;

    TStrBuffer = record
    strict private
      FItems: specialize TGDynArray<Char>;
      FCount: SizeInt;
    private
      procedure Reset(aCapacity: SizeInt);
    public
      procedure MakeEmpty; inline;
      procedure EnsureCapacity(aCapacity: SizeInt); inline;
      procedure Append(c: Char); inline;
      procedure AppendCrLf; inline;
      function  ToString: string; inline;
      function  PieceUp2Char(c: Char): string;
      procedure Write2Stream(aStream: TStream); inline;
      property  Count: SizeInt read FCount;
    end;
    TIssueVector = specialize TGLiteVector<TCsvIssue>;

  const
    DEFAULT_CAP  = 128;

  var
    FSrcStream: TStream;
    FIssues: TIssueVector;
    FBuffer: PAnsiChar;
    FData: string;
    FBufSize,
    FBufCount,
    FBufIndex,
    FColIndex,
    FRowIndex: SizeInt;
    FCellBuf: TStrBuffer;
    FCurrRow: TStringArray;
    FParseState: TParseState;
    FReadState: TReadState;
    FOwnsStream,
    FBufferSource: Boolean;
    FBom: TBomKind;
    FTable: TCsvTable;
  protected
    procedure SetDelimiterChar(d: Char); override;
    procedure SetQuoteChar(qc: Char); override;
    procedure SetCommentChar(cc: Char); override;
    procedure ReadDoc(aDoc: TCsvDoc); virtual;
    procedure ReadDocMasked(aDoc: TCsvDoc; const aMask: array of Boolean); virtual;
  private
    procedure FillTokenTable; inline;
    function  GetCurrCell: string; inline;
    function  GetCurrRow: TStringArray; inline;
    function  GetCurrRowPtr: PRow; inline;
    function  GetHasIssues: Boolean; inline;
    function  GetIssueList: TIssueList; inline;
    function  ReadFirstChunk: TReadState;
    function  ReadNextChunk: TReadState;
    procedure Error(aRow, aCol: SizeInt; aState: TParseState; c: Char = '?');
    procedure HandleEvent(aRow, aCol: SizeInt; var aState: TParseState; c: Char = '?');
    procedure AddCell; inline;
    function  FlushLastCell: Boolean;
    function  DoRead(aSingleCol: Boolean): Boolean;
    procedure HandleSkipEvent(aRow, aCol: SizeInt; var aState: TParseState);
    function  SkipLastCell: Boolean; inline;
    function  DoSkip(aSingleCol: Boolean): Boolean;
    function  TryGetCell(aIndex: SizeInt): string; inline;
    function  GetEof: Boolean; inline;
    property  SourceIsBuffer: Boolean read FBufferSource;
    property  ReadBuffer: PChar read FBuffer;
    property  BufIndex: SizeInt read FBufIndex;
  public
    class function New(aSource: TStream; aBufSize: SizeInt; aOwnsStream: Boolean): TCsvReader; static;
    class function New(aSource: TStream; aOwnsStream: Boolean = False): TCsvReader; static;
    class function NewCp(aSource: TStream; aCodePage: Integer; aOwnsStream: Boolean = False): TCsvReader; static;
    class function New(aSrcBuffer: PChar; aCount: SizeInt): TCsvReader; static;
  { attempts to read a single row from the buffer using the CSV properties of aMaster
    if aMaster is not nil; returns the number of bytes read, -1 if the buffer cannot be read }
    class function TryReadRow(aBuffer: PChar; aCount: SizeInt; out aRow: TStringArray;
                              aMaster: TCsvEntity = nil): SizeInt; static;
  { aBufSize indicates the desired buffer size, if it is less than 1, the buffer size
    is set equal to the stream size; if aOwnsStream is set to True, the reader
    is responsible for destroying source stream }
    constructor Create(aSource: TStream; aBufSize: Sizeint; aOwnsStream: Boolean); virtual;
    constructor Create(aSource: TStream; aOwnsStream: Boolean = False); virtual;
    constructor CreateCp(aSource: TStream; aCodePage: Integer; aOwnsStream: Boolean = False); virtual;
    constructor Create(aSrcBuffer: PChar; aCount: SizeInt); virtual;
    destructor Destroy; override;
    procedure AssignCsvProps(aSource: TCsvEntity); override;
    function  SetDelimiter(d: Char): TCsvReader; inline;
    function  SetQuoteMark(qm: Char): TCsvReader; inline;
    function  SetCommentMark(cm: Char): TCsvReader; inline;
    function  SetOptions(const op: TReadOptions): TCsvReader; inline;
  { skips one cell, ignoring the end of the row; issues, if any, will be logged,
    exceptions will not be raised on any setting }
    function  Skip: Boolean; inline;
  { tries to execute Skip() aTimes times if possible; returns the number of successful passes;
    issues, if any, will be logged, exceptions will not be raised on any setting }
    function  Skip(aTimes: SizeInt): SizeInt;
  { skips all cells until the beginning of the next row; issues, if any, will be logged,
    exceptions will not be raised on any setting }
    function  SkipRow: Boolean; inline;
  { tries to execute SkipRow() aTimes times if possible; returns the number of successful passes;
    issues, if any, will be logged, exceptions will not be raised on any setting }
    function  SkipRow(aTimes: SizeInt): SizeInt;
  { reads one cell, ignoring the end of the row; its value is contained
    in the CurrentCell property }
    function  Read: Boolean; inline;
  { reads all cells up to the beginning of the next row; their values are contained
    in the CurrentRow property }
    function  ReadRow: Boolean; inline;
  { unmasked columns are skipped; the contents of the internal buffer cells for
    skipped columns are not changed }
    function  ReadRow(const aMask: array of Boolean): SizeInt;

    procedure Iterate(it: TOnIterate);
    procedure Iterate(it: TNestIterate);
    procedure IterateRow(it: TOnIterate);
    procedure IterateRow(it: TNestIterate);
    procedure IterateRow(it: TOnIterate; const aMask: array of Boolean);
    procedure IterateRow(it: TNestIterate; const aMask: array of Boolean);

    property  OwnsStream: Boolean read FOwnsStream;
    property  State: TParseState read FParseState;
    property  ReadState: TReadState read FReadState;
  { index of the current already read cell in the row(0 based), -1 if no cell
    in the current row has been read yet }
    property  ColIndex: SizeInt read FColIndex;
  { current read cell }
    property  CurrentCell: string read GetCurrCell;
    property  Cell[aIndex: SizeInt]: string read TryGetCell;
  { current row index; after the reading of the row is completed,
    it is immediately increased by 1, thus indicating the number of rows read }
    property  RowIndex: SizeInt read FRowIndex;
  { returns a copy of the row's internal buffer }
    property  CurrentRow: TStringArray read GetCurrRow;
  { returns a pointer to the row's internal buffer }
    property  CurrRowPtr: PRow read GetCurrRowPtr;
    property  BufferSize: SizeInt read FBufSize;
    property  BOM: TBomKind read FBom;
    property  HasIssues: Boolean read GetHasIssues;
    property  IssueList: TIssueList read GetIssueList;
    property  Eof: Boolean read GetEof;
  end;

  { TCsvWriter: each instance of TCsvWriter can produce a single CSV document }
  TCsvWriter = class(TCsvEntity)
  private
  var
    FOwnStream,
    FStream: TStream;
    FCellBuf: TCsvReader.TStrBuffer;
    FRowIndex,
    FColIndex: SizeInt;
    FLineBreak: string;
    FSpecChars: TSysCharset;
    function  GetLineBreak(les: TLineEndStyle): string; inline;
    procedure DoWriteEol;
    procedure DoWriteCell(const p: PAnsiChar; aCount: SizeInt);
    procedure DoWriteRow(const r: TStringArray);
    function  GetOwnsStream: Boolean; inline;
    function  WriteDoc(aDoc: TCsvDoc): SizeInt;
  protected
    procedure SetDelimiterChar(d: Char); override;
    procedure SetQuoteChar(qc: Char); override;
    procedure SetLEStyle(les: TLineEndStyle); override;
  public
    class function New(aStream: TStream; aBufSize: SizeInt; aOwnsStream: Boolean = False): TCsvWriter; static;
    class function New(aStream: TStream; aOwnsStream: Boolean = False): TCsvWriter; static;
    constructor Create(aStream: TStream; aBufSize: SizeInt; aOwnsStream: Boolean = False);
    constructor Create(aStream: TStream; aOwnsStream: Boolean = False);
    destructor Destroy; override;
    procedure AssignCsvProps(aSource: TCsvEntity); override;
    function SetDelimiter(d: Char): TCsvWriter; inline;
    function SetQuoteMark(qm: Char): TCsvWriter; inline;
    function SetLineEndStyle(les: TLineEndStyle): TCsvWriter; inline;
    function SetQuotePolicy(qp: TQuotePolicy): TCsvWriter; inline;

    function AddCell(const aValue: string): TCsvWriter; inline;
    function AddCell(const aBuffer; aCount: SizeInt): TCsvWriter; //inline;
    function AddEol: TCsvWriter; inline;
    function AddRow(const aRow: TStringArray): TCsvWriter; inline;
  { the column index of the currently written cell, -1 if no cells have been written
    in the current row yet }
    property ColIndex: SizeInt read FColIndex;
  { the index of the current row, immediately incremented by 1 after writing the end of the line }
    property RowIndex: SizeInt read FRowIndex;
    property OwnsStream: Boolean read GetOwnsStream;
  end;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}
uses
  Math, BufStream, lgArrayHelpers, lgAsync;

const
  chTab      = #9;
  chLF       = #10;
  chCR       = #13;
  chSpace    = ' ';
  chComma    = ',';
  chDblQuote = '"';
  chSharp    = '#';

{ ECsvRead }

constructor ECsvRead.Create(aRow, aCol: SizeInt; aReason: Integer; const aMsgFmt, aCell: string);
begin
  FRow := aRow;
  FCol := aCol;
  FReason := aReason;
  inherited CreateFmt(SEReadCsvErrorFmt, [Format(aMsgFmt, [aRow, aCol]), aCell]);
end;

constructor ECsvRead.Create(aRow, aCol: SizeInt; aReason: Integer; const aMsgFmt: string);
begin
  FRow := aRow;
  FCol := aCol;
  FReason := aReason;
  inherited CreateFmt(aMsgFmt, [aCol, aRow]);
end;

constructor ECsvRead.Create(aReason: Integer; const aMsg: string);
begin
  FRow := NULL_INDEX;
  FCol := NULL_INDEX;
  FReason := aReason;
  inherited Create(aMsg);
end;

{ TCsvEntity.TCsvReader.TCsvIssue }

class function TCsvEntity.TCsvIssue.Make(aRow, aCol: SizeInt; e: TCsvEvent): TCsvIssue;
begin
  Result.Row := aRow;
  Result.Column := aCol;
  Result.Reason := e;
end;

{ TCsvEntity }

procedure TCsvEntity.DoSetQuotePolicy(qp: TQuotePolicy);
begin
  FQuotePolicy := qp;
end;

procedure TCsvEntity.CheckDelimiterChar(d: Char);
begin
  if ((d < #32) and (d <> chTab)) or (d = QuoteMark) or (d = CommentMark) or (d = chLF) or (d = chCR) or (d > #126) then
    raise ECsvSpecialMark.CreateFmt(SEInvalidDelimiterFmt, [Ord(d)]);
end;

procedure TCsvEntity.SetDelimiterChar(d: Char);
begin
  if FDelimiter = d then exit;
  CheckDelimiterChar(d);
  FDelimiter := d;
end;

procedure TCsvEntity.CheckQuoteChar(qc: Char);
begin
  if (qc < #33) or (qc = Delimiter) or (qc = CommentMark) or (qc = chLF) or (qc = chCR) or (qc > #126) then
    raise ECsvSpecialMark.CreateFmt(SEInvalidQuoteMarkFmt, [Ord(qc)]);
end;

procedure TCsvEntity.SetQuoteChar(qc: Char);
begin
  if FQuoteMark = qc then exit;
  CheckQuoteChar(qc);
  FQuoteMark := qc;
end;

procedure TCsvEntity.CheckCommentChar(cc: Char);
begin
  if (cc < #33) or (cc = Delimiter) or (cc = QuoteMark) or (cc = chLF) or (cc = chCR) or (cc > #126) then
    raise ECsvSpecialMark.CreateFmt(SEInvalidCommentMarkFmt, [Ord(cc)]);
end;

procedure TCsvEntity.SetCommentChar(cc: Char);
begin
  if FCommentMark = cc then exit;
  CheckCommentChar(cc);
  FCommentMark := cc;
end;

procedure TCsvEntity.SetOpts(const ro: TReadOptions);
begin
  FOptions := ro;
end;

procedure TCsvEntity.SetLEStyle(les: TLineEndStyle);
begin
  FLEStyle := les;
end;

constructor TCsvEntity.Create;
begin
  FDelimiter := chComma;
  FQuoteMark := chDblQuote;
  FCommentMark := chSharp;
end;

procedure TCsvEntity.AssignCsvProps(aSource: TCsvEntity);
begin
  FDelimiter := aSource.Delimiter;
  FQuoteMark := aSource.QuoteMark;
  FCommentMark := aSource.CommentMark;
  Options := aSource.Options;
  LineEndStyle := aSource.LineEndStyle;
  QuotePolicy := aSource.QuotePolicy;
end;

type
  TAcceptingDocReader = class(TCsvReader)
  protected
    procedure DoReadDoc(aDoc: TCsvDoc); virtual;
    procedure DoReadDocMasked(aDoc: TCsvDoc; const aMask: array of Boolean);
  public
    procedure ReadDoc(aDoc: TCsvDoc); override;
    procedure ReadDocMasked(aDoc: TCsvDoc; const aMask: array of Boolean); override;
  end;

  TIgnoringDocReader = class(TAcceptingDocReader)
  protected
    procedure DoReadDoc(aDoc: TCsvDoc); override;
  end;

  TStrictDocReader = class(TAcceptingDocReader)
  protected
    procedure DoReadDoc(aDoc: TCsvDoc); override;
  end;

{ TCsvDoc }

procedure TCsvDoc.CheckRowIndex(aIndex: SizeInt);
begin
  if SizeUInt(aIndex) >= SizeUInt(FCells.Count) then
    raise EArgumentOutOfRangeException.CreateFmt(SERowIndexOutOfBoundsFmt, [aIndex]);
end;

procedure TCsvDoc.CheckRowInsertIndex(aIndex: SizeInt);
begin
  if SizeUInt(aIndex) > SizeUInt(FCells.Count) then
    raise EArgumentOutOfRangeException.CreateFmt(SERowIndexOutOfBoundsFmt, [aIndex]);
end;

procedure TCsvDoc.CheckColIndex(aRow: PRow; aIndex: SizeInt);
begin
  if SizeUInt(aIndex) >= SizeUInt(System.Length(aRow^)) then
    raise EArgumentOutOfRangeException.CreateFmt(SEColIndexOutOfBoundsFmt, [aIndex]);
end;

procedure TCsvDoc.CheckColInsertIndex(aRow: PRow; aIndex: SizeInt);
begin
  if SizeUInt(aIndex) > SizeUInt(System.Length(aRow^)) then
    raise EArgumentOutOfRangeException.CreateFmt(SEColIndexOutOfBoundsFmt, [aIndex]);
end;

function TCsvDoc.GetCell(aRow, aCol: SizeInt): string;
begin
  CheckRowIndex(aRow);
  CheckColIndex(FCells.UncMutable[aRow], aCol);
  Result := FCells.UncMutable[aRow]^[aCol];
end;

procedure TCsvDoc.SetCell(aRow, aCol: SizeInt; const aValue: string);
begin
  CheckRowIndex(aRow);
  CheckColIndex(FCells.UncMutable[aRow], aCol);
  FCells.UncMutable[aRow]^[aCol] := aValue;
end;

function TCsvDoc.GetColCount(aRow: SizeInt): SizeInt;
begin
  if SizeUInt(aRow) >= SizeUInt(FCells.Count) then exit(NULL_INDEX);
  Result := System.Length(FCells.UncMutable[aRow]^);
end;

function TCsvDoc.GetMutRow(aIndex: SizeInt): PRow;
begin
  CheckRowIndex(aIndex);
  Result := FCells.UncMutable[aIndex];
end;

function TCsvDoc.GetRow(aIndex: SizeInt): TStringArray;
begin
  CheckRowIndex(aIndex);
  Result := System.Copy(FCells.UncMutable[aIndex]^);
end;

function TCsvDoc.GetRowCount: SizeInt;
begin
  Result := FCells.Count;
end;

function TCsvDoc.GetHasIssues: Boolean;
begin
  Result := FIssueList <> nil;
end;

function TCsvDoc.GetIssueCount: SizeInt;
begin
  Result := System.Length(FIssueList);
end;

function TCsvDoc.GetIssueList: TIssueList;
begin
  Result := System.Copy(FIssueList);
end;

function TCsvDoc.GetHeadColCount: SizeInt;
begin
  if FCells.IsEmpty then exit(NULL_INDEX);
  Result := System.Length(FCells.UncMutable[0]^);
end;

function TCsvDoc.GetMaxColCount: SizeInt;
var
  I, Len: SizeInt;
begin
  if FCells.IsEmpty then exit(NULL_INDEX);
  Result := System.Length(FCells.UncMutable[0]^);
  for I := 1 to Pred(FCells.Count) do
    begin
      Len := System.Length(FCells.UncMutable[I]^);
      if Len > Result then Result := Len;
    end;
end;

function TCsvDoc.GetMinColCount: SizeInt;
var
  I, Len: SizeInt;
begin
  if FCells.IsEmpty then exit(NULL_INDEX);
  Result := System.Length(FCells.UncMutable[0]^);
  for I := 1 to Pred(FCells.Count) do
    begin
      Len := System.Length(FCells.UncMutable[I]^);
      if Len < Result then Result := Len;
    end;
end;

function TCsvDoc.GetReaderClass: TCsvReaderClass;
begin
  case ColCountPolicy of
    ccpAccept: Result := TAcceptingDocReader;
    ccpIgnore: Result := TIgnoringDocReader;
  else
    Result := TStrictDocReader;
  end;
end;

function TCsvDoc.GetCsvText: string;
var
  ss: TStringStream;
begin
  Result := '';
  ss := TStringStream.Create;
  try
    SaveToStream(ss);
    Result := ss.DataString;
  finally
    ss.Free;
  end;
end;

procedure TCsvDoc.DoSetCsvText(const aValue: string);
begin
  DoLoadBuffer(Pointer(aValue), System.Length(aValue));
end;

procedure TCsvDoc.LoadStream(s: TStream; aBufSize: SizeInt; aOwnsStream: Boolean);
begin
  with GetReaderClass.Create(s, aBufSize, aOwnsStream) do
    try
      Clear;
      ReadDoc(Self);
    finally
      Free;
    end;
end;

procedure TCsvDoc.LoadStreamCp(s: TStream; aCodePage: Integer; aOwnsStream: Boolean);
begin
  with GetReaderClass.CreateCp(s, aCodePage, aOwnsStream) do
    try
      Clear;
      ReadDoc(Self);
    finally
      Free;
    end;
end;

procedure TCsvDoc.LoadStream(s: TStream; aBufSize: SizeInt; aOwnsStream: Boolean;
  const aMask: array of Boolean);
begin
  with TAcceptingDocReader.Create(s, aBufSize, aOwnsStream) do
    try
      Clear;
      ReadDocMasked(Self, aMask);
    finally
      Free;
    end;
end;

procedure TCsvDoc.LoadStreamCp(s: TStream; aCodePage: Integer; aOwnsStream: Boolean; const aMask: array of Boolean);
begin
  with GetReaderClass.CreateCp(s, aCodePage, aOwnsStream) do
    try
      Clear;
      ReadDocMasked(Self, aMask);
    finally
      Free;
    end;
end;

procedure TCsvDoc.LoadFile(const aFileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadStream(fs, DEF_BUF_SIZE, False);
  finally
    fs.Free;
  end;
end;

procedure TCsvDoc.LoadFile(const aFileName: string; aCodePage: Integer);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadStreamCp(fs, aCodePage, False);
  finally
    fs.Free;
  end;
end;

procedure TCsvDoc.LoadFile(const aFileName: string; const aMask: array of Boolean);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadStream(fs, DEF_BUF_SIZE, False, aMask);
  finally
    fs.Free;
  end;
end;

procedure TCsvDoc.LoadFile(const aFileName: string; const aMask: array of Boolean; aCodePage: Integer);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadStreamCp(fs, aCodePage, False, aMask);
  finally
    fs.Free;
  end;
end;

procedure TCsvDoc.DoLoadBuffer(aBuffer: PChar; aCount: SizeInt);
begin
  with GetReaderClass.Create(aBuffer, aCount) do
    try
      Clear;
      ReadDoc(Self);
    finally
      Free;
    end;
end;

procedure TCsvDoc.DoLoadBuffer(aBuffer: PChar; aCount: SizeInt; const aMask: array of Boolean);
begin
  Clear;
  with TAcceptingDocReader.Create(aBuffer, aCount) do
    try
      ReadDocMasked(Self, aMask);
    finally
      Free;
    end;
end;

procedure TCsvDoc.AssignTo(aDest: TPersistent);
var
  doc: TCsvDoc;
  I: SizeInt;
begin
  if aDest is TCsvDoc then
    begin
      doc := TCsvDoc(aDest);
      doc.AssignCsvProps(Self);
      doc.FCcPolicy := ColCountPolicy;
      doc.FCells.Clear;
      doc.FCells.EnsureCapacity(FCells.Count);
      for I := 0 to Pred(FCells.Count) do
        doc.FCells.Add(System.Copy(FCells.UncMutable[I]^));
      doc.FIssueList := System.Copy(IssueList);
    end
  else
    inherited AssignTo(aDest);
end;

class function TCsvDoc.New: TCsvDoc;
begin
  Result := TCsvDoc.Create;
end;

class function TCsvDoc.New(const a: array of TStringArray): TCsvDoc;
begin
  Result := TCsvDoc.Create(a);
end;

constructor TCsvDoc.Create;
begin
  inherited;
  FCells := TStrList2D.Create;
end;

constructor TCsvDoc.Create(const a: array of TStringArray);
var
  I: SizeInt;
begin
  Create;
  for I := 0 to System.High(a) do
    FCells.Add(a[I]);
end;

destructor TCsvDoc.Destroy;
begin
  FCells.Free;
  inherited;
end;

function TCsvDoc.SetDelimiter(d: Char): TCsvDoc;
begin
  Delimiter := d;
  Result := Self;
end;

function TCsvDoc.SetQuoteMark(qm: Char): TCsvDoc;
begin
  QuoteMark := qm;
  Result := Self;
end;

function TCsvDoc.SetCommentMark(cm: Char): TCsvDoc;
begin
  CommentMark := cm;
  Result := Self;
end;

function TCsvDoc.SetOptions(const op: TReadOptions): TCsvDoc;
begin
  Options := op;
  Result := Self;
end;

function TCsvDoc.SetLineEndStyle(les: TLineEndStyle): TCsvDoc;
begin
  LineEndStyle := les;
  Result := Self;
end;

function TCsvDoc.SetColCountPolicy(ccp: TColCountPolicy): TCsvDoc;
begin
  ColCountPolicy := ccp;
  Result := Self;
end;

function TCsvDoc.SetQuotePolicy(qp: TQuotePolicy): TCsvDoc;
begin
  QuotePolicy := qp;
  Result := Self;
end;

function TCsvDoc.SetCsvText(const aValue: string): TCsvDoc;
begin
  CsvText := aValue;
  Result := Self;
end;

function TCsvDoc.SetCsvText(const aValue: string; const aMask: array of Boolean): TCsvDoc;
begin
  if System.Length(aMask) = 0 then exit(SetCsvText(aValue));
  DoLoadBuffer(Pointer(aValue), System.Length(aValue), aMask);
  Result := Self;
end;

function TCsvDoc.LoadFromStream(aSource: TStream; aBufSize: SizeInt; aOwnsStream: Boolean): TCsvDoc;
begin
  LoadStream(aSource, aBufSize, aOwnsStream);
  Result := Self;
end;

function TCsvDoc.LoadFromStream(aSource: TStream; aOwnsStream: Boolean; aCodePage: Integer): TCsvDoc;
begin
  LoadStreamCp(aSource, aCodePage, aOwnsStream);
  Result := Self;
end;

function TCsvDoc.LoadFromStream(aSource: TStream; aBufSize: SizeInt; aOwnsStream: Boolean;
  const aMask: array of Boolean): TCsvDoc;
begin
  if System.Length(aMask) = 0 then exit(LoadFromStream(aSource, aBufSize, aOwnsStream));
  LoadStream(aSource, aBufSize, aOwnsStream, aMask);
  Result := Self;
end;

function TCsvDoc.LoadFromStream(aSource: TStream; aOwnsStream: Boolean; const aMask: array of Boolean;
  aCodePage: Integer): TCsvDoc;
begin
  if System.Length(aMask) = 0 then exit(LoadFromStream(aSource, aOwnsStream, aCodePage));
  LoadStreamCp(aSource, aCodePage, aOwnsStream, aMask);
  Result := Self;
end;

function TCsvDoc.LoadFromStream(aSource: TStream): TCsvDoc;
begin
  Result := LoadFromStream(aSource, DEF_BUF_SIZE, False);
  Result := Self;
end;

function TCsvDoc.LoadFromStream(aSource: TStream; const aMask: array of Boolean): TCsvDoc;
begin
  if System.Length(aMask) = 0 then exit(LoadFromStream(aSource));
  Result := LoadFromStream(aSource, DEF_BUF_SIZE, False, aMask);
  Result := Self;
end;

function LoadBuffer2Doc(const aMaster: TCsvDoc; const aBuffer: PChar; const aCount: SizeInt): TCsvDoc;
begin
  Result := TCsvDoc.Create;
  try
    Result.AssignCsvProps(aMaster);
    Result.LoadBuffer(aBuffer, aCount);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

type
  TChrSlice = record
    Ptr: PChar;
    Count: SizeInt;
    constructor Make(p: PChar; aCount: SizeInt);
  end;

constructor TChrSlice.Make(p: PChar; aCount: SizeInt);
begin
  Ptr := p;
  Count := aCount;
end;

function LoadBufferMask(const aMaster: TCsvDoc; const aSlice: TChrSlice;
  const aMask: specialize TGArray<Boolean>): TCsvDoc;
begin
  Result := TCsvDoc.Create;
  try
    Result.AssignCsvProps(aMaster);
    Result.LoadBuffer(aSlice.Ptr, aSlice.Count, aMask);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ returns the number of bytes skipped }
function SkipDocRow(aMaster: TCsvDoc; const aSlice: TChrSlice; aTimes: SizeInt): SizeInt;
begin
  Result := 0;
  with TCsvReader.Create(aSlice.Ptr, aSlice.Count) do
    try
      AssignCsvProps(aMaster);
      Options := Options - [roStrict]; ////////////
      SkipRow(aTimes);
      Result += BufIndex;
    finally
      Free;
    end;
end;

function TryLoadBufferMT(aMaster: TCsvDoc; aBuffer: PChar; aCount: SizeInt; aLimit: Integer): TCsvDoc;
type
  TTask   = specialize TGAsyncTriadic<TCsvDoc, PChar, SizeInt, TCsvDoc>;
  TFuture = TTask.TFuture;
var
  Failed: Boolean = False;
  Fatal: Exception = nil;
  function CheckFuture(const f: TFuture): Boolean;
  begin
    if Failed then exit(True);
    Failed := f.State = atsFatal;
    Result := Failed;
    if Failed then
      Fatal := f.FatalException;
  end;
var
  Futures: array of TFuture;
  p, pCurr, pEnd: PChar;
  ChunkCount, ChunkSize, I: SizeInt;
  Next: TCsvDoc;
begin
  Result := nil;
  //here assume aCount >= MT_MIN_CHUNK_SIZE*2
  Assert(aCount >= TCsvDoc.MT_MIN_CHUNK_SIZE*2);
  case DetectBom(PByte(aBuffer), aCount) of
    bkNone: ;
    bkUtf8: begin
        aBuffer += UTF8_BOM_LEN;
        aCount -= UTF8_BOM_LEN;
      end;
  else //can not read document due to encoding
    Result := TCsvDoc.Create;
    Result.AssignCsvProps(aMaster);
    Result.FIssueList := [TCsvDoc.TCsvIssue.Make(0, 0, ceUnsupportEncoding)];
    exit;
  end;

  ChunkCount := Math.Min(aCount div TCsvDoc.MT_MIN_CHUNK_SIZE, aLimit);
  if ChunkCount < 2 then begin
    Result := TCsvDoc.Create;
    Result.AssignCsvProps(aMaster);
    exit(Result.LoadBuffer(aBuffer, aCount));
  end;
  ChunkSize := aCount div ChunkCount;

  System.SetLength(Futures, ChunkCount);
  p := aBuffer;
  pEnd := aBuffer + aCount;
  for I := 0 to System.Length(Futures) - 2 do begin
    pCurr := aBuffer + ChunkSize*Succ(I);
    //here some trick that may or may not work sometimes?
    pCurr += SkipDocRow(aMaster, TChrSlice.Make(pCurr, pEnd - pCurr), 2);
    Futures[I] := TTask.Call(@LoadBuffer2Doc, aMaster,  p, pCurr - p);
    p := pCurr;
  end;
  Futures[Pred(ChunkCount)] := TTask.Call(@LoadBuffer2Doc, aMaster, p, pEnd - p);

  Futures[0].WaitFor;
  CheckFuture(Futures[0]);
  if not Failed then
    Result := Futures[0].Value;

  for I := 1 to System.High(Futures) do begin
    Futures[I].WaitFor;
    if Failed then
      Futures[I].GetValue.Value.Free
    else
      if CheckFuture(Futures[I]) then continue;
    Next := Futures[I].Value;
    Result.FCells.Merge(Next.FCells);
    if Next.HasIssues then Result.FIssueList += Next.FIssueList;
    Next.Free;
  end;

  if Failed then begin
    FreeAndNil(Result);
    raise Fatal;
  end;
end;

function TryLoadBufferMT(aMaster: TCsvDoc; aBuffer: PChar; aCount: SizeInt; const aMask: array of Boolean;
  aLimit: Integer): TCsvDoc;
type
  TArray = array of Boolean;
  TTask   = specialize TGAsyncTriadic<TCsvDoc, TChrSlice, TArray, TCsvDoc>;
  TFuture = TTask.TFuture;
  THelper = specialize TGArrayHelpUtil<Boolean>;
var
  Failed: Boolean = False;
  Fatal: Exception = nil;
  function CheckFuture(const f: TFuture): Boolean;
  begin
    if Failed then exit(True);
    Failed := f.State = atsFatal;
    Result := Failed;
    if Failed then
      Fatal := f.FatalException;
  end;
var
  m: TArray;
  Futures: array of TFuture;
  p, pCurr, pEnd: PChar;
  ChunkCount, ChunkSize, I: SizeInt;
  Next: TCsvDoc;
begin
  Result := nil;
  //here assume aCount >= MT_MIN_CHUNK_SIZE*2
  Assert(aCount >= TCsvDoc.MT_MIN_CHUNK_SIZE*2);
  case DetectBom(PByte(aBuffer), aCount) of
    bkNone: ;
    bkUtf8: begin
        aBuffer += UTF8_BOM_LEN;
        aCount -= UTF8_BOM_LEN;
      end;
  else //can not read document due to encoding
    Result := TCsvDoc.Create;
    Result.AssignCsvProps(aMaster);
    Result.FIssueList := [TCsvDoc.TCsvIssue.Make(0, 0, ceUnsupportEncoding)];
    exit;
  end;

  ChunkCount := Math.Min(aCount div TCsvDoc.MT_MIN_CHUNK_SIZE, aLimit);
  if ChunkCount < 2 then begin
    Result := TCsvDoc.Create;
    Result.AssignCsvProps(aMaster);
    exit(Result.LoadBuffer(aBuffer, aCount, aMask));
  end;
  ChunkSize := aCount div ChunkCount;

  m := THelper.CreateCopy(aMask);

  System.SetLength(Futures, ChunkCount);
  p := aBuffer;
  pEnd := aBuffer + aCount;
  for I := 0 to System.Length(Futures) - 2 do begin
    pCurr := aBuffer + ChunkSize*(I+1);
    //here some trick that may or may not work sometimes?
    pCurr += SkipDocRow(aMaster, TChrSlice.Make(pCurr, pEnd - pCurr), 2);
    Futures[I] := TTask.Call(@LoadBufferMask, aMaster, TChrSlice.Make(p, pCurr - p), m);
    p := pCurr;
  end;
  Futures[Pred(ChunkCount)] :=
    TTask.Call(@LoadBufferMask, aMaster, TChrSlice.Make(p, pEnd - p), m);

  Futures[0].WaitFor;
  CheckFuture(Futures[0]);
  if not Failed then
    Result := Futures[0].Value;

  for I := 1 to System.High(Futures) do begin
    Futures[I].WaitFor;
    if Failed then
      Futures[I].GetValue.Value.Free
    else
      if CheckFuture(Futures[I]) then continue;
    Next := Futures[I].Value;
    Result.FCells.Merge(Next.FCells);
    if Next.HasIssues then Result.FIssueList += Next.FIssueList;
    Next.Free;
  end;

  if Failed then begin
    FreeAndNil(Result);
    raise Fatal;
  end;
end;

function TCsvDoc.LoadFromStreamMT(aSource: TStream; aThreadLimit: Integer): TCsvDoc;
var
  ms: specialize TGAutoRef<TMemoryStream>;
  doc: TCsvDoc = nil;
begin
  if aThreadLimit < 1 then
    aThreadLimit := TThread.ProcessorCount;
  if (aThreadLimit = 1) or (aSource.Size < MT_MIN_CHUNK_SIZE*2) then
    exit(LoadFromStream(aSource));
  Clear;
  ms.Instance.LoadFromStream(aSource);
  doc := TryLoadBufferMT(Self, ms.Instance.Memory, ms.Instance.Size, aThreadLimit);
  try
    ms.Clear;
    FCells.Free;
    FCells := doc.FCells;
    doc.FCells := nil;
    FIssueList := doc.FIssueList;
  finally
    doc.Free;
  end;
  Result := Self;
end;

{$PUSH}{$WARN 5063 OFF}
function TCsvDoc.LoadFromStreamMT(aSource: TStream; const aMask: array of Boolean;
  aThreadLimit: Integer): TCsvDoc;
var
  ms: specialize TGAutoRef<TMemoryStream>;
  doc: TCsvDoc = nil;
begin
  if System.Length(aMask) = 0 then
    exit(LoadFromStreamMT(aSource, aThreadLimit));
  if aThreadLimit < 1 then
    aThreadLimit := TThread.ProcessorCount;
  if (aThreadLimit = 1) or (aSource.Size < MT_MIN_CHUNK_SIZE*2) then
    exit(LoadFromStream(aSource, aMask));
  Clear;
  ms.Instance.LoadFromStream(aSource);
  doc := TryLoadBufferMT(Self, ms.Instance.Memory, ms.Instance.Size, aMask, aThreadLimit);
  try
    ms.Clear;
    FCells.Free;
    FCells := doc.FCells;
    doc.FCells := nil;
    FIssueList := doc.FIssueList;
  finally
    doc.Free;
  end;
  Result := Self;
end;
{$POP}

function TCsvDoc.LoadFromFile(const aFileName: string): TCsvDoc;
begin
  LoadFile(aFileName);
  Result := Self;
end;

function TCsvDoc.LoadFromFile(const aFileName: string; aCodePage: Integer): TCsvDoc;
begin
  LoadFile(aFileName, aCodePage);
  Result := Self;
end;

function TCsvDoc.LoadFromFile(const aFileName: string; const aMask: array of Boolean): TCsvDoc;
begin
  if System.Length(aMask) = 0 then exit(LoadFromFile(aFileName));
  LoadFile(aFileName, aMask);
  Result := Self;
end;

function TCsvDoc.LoadFromFile(const aFileName: string; const aMask: array of Boolean;
  aCodePage: Integer): TCsvDoc;
begin
  if System.Length(aMask) = 0 then exit(LoadFromFile(aFileName, aCodePage));
  LoadFile(aFileName, aMask, aCodePage);
  Result := Self;
end;

{$PUSH}{$WARN 5063 OFF}
function TCsvDoc.LoadFromFileMT(const aFileName: string; aThreadLimit: Integer): TCsvDoc;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStreamMT(fs, aThreadLimit);
  finally
    fs.Free;
  end;
end;

function TCsvDoc.LoadFromFileMT(const aFileName: string; const aMask: array of Boolean;
  aThreadLimit: Integer): TCsvDoc;
var
  fs: TFileStream;
begin
  if System.Length(aMask) = 0 then
    exit(LoadFromFileMT(aFileName, aThreadLimit));
  fs := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStreamMT(fs, aMask, aThreadLimit);
  finally
    fs.Free;
  end;
end;
{$POP}

function TCsvDoc.LoadBuffer(aSrcBuffer: PChar; aCount: SizeInt): TCsvDoc;
begin
  DoLoadBuffer(aSrcBuffer, aCount);
  Result := Self;
end;

function TCsvDoc.LoadBuffer(aSrcBuffer: PChar; aCount: SizeInt; const aMask: array of Boolean): TCsvDoc;
begin
  if System.Length(aMask) = 0 then exit(LoadBuffer(aSrcBuffer, aCount));
  DoLoadBuffer(aSrcBuffer, aCount, aMask);
  Result := Self;
end;

function TCsvDoc.SaveToStream(aStream: TStream; aBufSize: SizeInt): SizeInt;
begin
  Result := 0;
  with TCsvWriter.Create(aStream, aBufSize) do
    try
      Result += WriteDoc(Self);
    finally
      Free;
    end;
end;

function TCsvDoc.SaveToStream(aStream: TStream): SizeInt;
begin
  Result := SaveToStream(aStream, NULL_INDEX);
end;

function TCsvDoc.SaveToFile(const aFileName: string): SizeInt;
var
  fs: TFileStream;
begin
  Result := 0;
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    Result += SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TCsvDoc.ClearIssues;
begin
  FIssueList := nil;
end;

procedure TCsvDoc.Clear;
begin
  FCells.Clear;
  FIssueList := nil;
end;

procedure TCsvDoc.TrimToFit;
begin
  FCells.TrimToFit;
end;

function TCsvDoc.IsEmpty: Boolean;
begin
  Result := FCells.IsEmpty;
end;

function TCsvDoc.RowExists(aIndex: SizeInt): Boolean;
begin
  Result := SizeUInt(aIndex) < SizeUInt(FCells.Count);
end;

function TCsvDoc.CellExists(aRow, aCol: SizeInt): Boolean;
begin
  Result :=
    RowExists(aRow) and (SizeUInt(aCol) < SizeUInt(System.Length(FCells.UncMutable[aRow]^)));
end;

function TCsvDoc.IndexOfCol(aRow: SizeInt; const aValue: string): SizeInt;
var
  I: SizeInt;
  p: PRow;
begin
  Result := NULL_INDEX;
  if SizeUInt(aRow) >= SizeUInt(FCells.Count) then exit;
  p := FCells.UncMutable[aRow];
  for I := 0 to System.High(p^) do
    if p^[I] = aValue then exit(I);
end;

function TCsvDoc.IndexOfRow(aCol: SizeInt; const aValue: string): SizeInt;
var
  I: SizeInt;
  p: PRow;
begin
  Result := NULL_INDEX;
  if aCol < 0 then exit;
  for I := 0 to Pred(FCells.Count) do
    begin
      p := FCells.UncMutable[I];
      if (aCol < System.Length(p^)) and (p^[aCol] = aValue) then
        exit(I);
    end;
end;

function TCsvDoc.AddCell(aRow: SizeInt; const aValue: string): SizeInt;
begin
  CheckRowIndex(aRow);
  Result := System.Length(FCells.UncMutable[aRow]^);
  FCells.UncMutable[aRow]^ += [aValue];
end;

procedure TCsvDoc.InsertCell(aRow, aCol: SizeInt; const aValue: string);
var
  p: TStrList2D.PItem;
begin
  CheckRowIndex(aRow);
  p := FCells.UncMutable[aRow];
  CheckColInsertIndex(p, aCol);
  Insert([aValue], p^, aCol);
end;

function TCsvDoc.AddRow(const aRow: array of string): SizeInt;
begin
  Result := FCells.Add(specialize TGArrayHelpUtil<string>.CreateCopy(aRow));
end;

function TCsvDoc.AddRow(aColCount: SizeInt): PRow;
begin
  Result := FCells.UncMutable[FCells.Add(nil)];
  System.SetLength(Result^, Math.Max(aColCount, 0));
end;

procedure TCsvDoc.InsertRow(aIndex: SizeInt; const aRow: array of string);
begin
  CheckRowInsertIndex(aIndex);
  FCells.Insert(aIndex, specialize TGArrayHelpUtil<string>.CreateCopy(aRow));
end;

function TCsvDoc.InsertRow(aIndex: SizeInt; aColCount: SizeInt): PRow;
begin
  CheckRowInsertIndex(aIndex);
  FCells.Insert(aIndex, nil);
  Result := FCells.UncMutable[aIndex];
  System.SetLength(Result^, Math.Max(aColCount, 0));
end;

function TCsvDoc.DeleteRow(aIndex: SizeInt): TStringArray;
begin
  CheckRowIndex(aIndex);
  Result := FCells.Extract(aIndex);
end;

function TCsvDoc.SwapRow(L, R: SizeInt): Boolean;
var
  p: Pointer;
begin
  if not(RowExists(L) and RowExists(R)) then exit(False);
  p := Pointer(FCells.UncMutable[L]^);
  Pointer(FCells.UncMutable[L]^) := Pointer(FCells.UncMutable[R]^);
  Pointer(FCells.UncMutable[R]^) := p;
  Result := True;
end;

function TCsvDoc.TrimTrailingBlanks(aRow: SizeInt): SizeInt;
var
  I: SizeInt;
  p: PRow;
begin
  Result := 0;
  if not RowExists(aRow) then exit;
  p := FCells.UncMutable[aRow];
  I := System.High(p^);
  while (I >= 0) and (p^[I] = '') do
    Dec(I);
  if I <> System.High(p^) then
    begin
      Result += System.High(p^) - I;
      System.SetLength(p^, Succ(I));
    end;
end;

function TCsvDoc.TrimTrailingBlanks: SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I := 0 to Pred(FCells.Count) do
    Result += TrimTrailingBlanks(I);
end;

procedure TCsvDoc.Sort(c: TRowCompare; aWithHeader: Boolean);
begin
  if aWithHeader then
    specialize TGRegularArrayHelper<TStringArray>.Sort(FCells.UncMutable[0][0..Pred(FCells.Count)], c)
  else
    specialize TGRegularArrayHelper<TStringArray>.Sort(FCells.UncMutable[0][1..Pred(FCells.Count)], c)
end;

procedure TCsvDoc.Sort(c: TOnRowCompare; aWithHeader: Boolean);
begin
  if aWithHeader then
    specialize TGDelegatedArrayHelper<TStringArray>.Sort(FCells.UncMutable[0][0..Pred(FCells.Count)], c)
  else
    specialize TGDelegatedArrayHelper<TStringArray>.Sort(FCells.UncMutable[0][1..Pred(FCells.Count)], c)
end;

procedure TCsvDoc.Sort(c: TNestRowCompare; aWithHeader: Boolean);
begin
  if aWithHeader then
    specialize TGNestedArrayHelper<TStringArray>.Sort(FCells.UncMutable[0][0..Pred(FCells.Count)], c)
  else
    specialize TGNestedArrayHelper<TStringArray>.Sort(FCells.UncMutable[0][1..Pred(FCells.Count)], c)
end;

{ TCsvReader.TStrBuffer }

procedure TCsvReader.TStrBuffer.Reset(aCapacity: SizeInt);
begin
  if aCapacity > 0 then
    FItems.Length := lgUtils.RoundUpTwoPower(aCapacity)
  else
    FItems.Length := DEFAULT_CONTAINER_CAPACITY;
  FCount := 0;
end;

procedure TCsvReader.TStrBuffer.MakeEmpty;
begin
  FCount := 0;
end;

procedure TCsvReader.TStrBuffer.EnsureCapacity(aCapacity: SizeInt);
begin
  if aCapacity > FItems.Length then
    FItems.Length := lgUtils.RoundUpTwoPower(aCapacity);
end;

procedure TCsvReader.TStrBuffer.Append(c: Char);
begin
  EnsureCapacity(Count + 1);
  FItems[Count] := c;
  Inc(FCount);
end;

procedure TCsvReader.TStrBuffer.AppendCrLf;
begin
  EnsureCapacity(Count + 2);
  FItems[Count] := chCR;
  FItems[Count + 1] := chLF;
  Inc(FCount, 2)
end;

function TCsvReader.TStrBuffer.ToString: string;
begin
  if Count = 0 then exit('');
  System.SetLength(Result, Count);
  System.Move(FItems.Ptr^, Pointer(Result)^, Count);
  FCount := 0;
end;

function TCsvReader.TStrBuffer.PieceUp2Char(c: Char): string;
var
  I, Len: SizeInt;
begin
  Len := 0;
  for I := 0 to Pred(Count) do
    if FItems[I] = c then begin
      Len := Succ(I);
      break;
    end;
  if Len = 0 then exit('');
  System.SetLength(Result, Len);
  System.Move(FItems.Ptr^, Pointer(Result)^, Len);
  FCount := 0;
end;

procedure TCsvReader.TStrBuffer.Write2Stream(aStream: TStream);
begin
  if Count = 0 then exit;
  aStream.WriteBuffer(FItems.Ptr^, Count);
  FCount := 0;
end;

{ TCsvReader }

procedure TCsvReader.SetDelimiterChar(d: Char);
begin
  if Delimiter = d then exit;
  CheckDelimiterChar(d);
  if Delimiter < #33 then
    FTable[Delimiter] := ctWhiteSpace
  else
    FTable[Delimiter] := ctChar;
  FTable[d] := ctDelimiter;
  FDelimiter := d;
end;

procedure TCsvReader.SetQuoteChar(qc: Char);
begin
  if QuoteMark = qc then exit;
  CheckQuoteChar(qc);
  FTable[QuoteMark] := ctChar;
  FTable[qc] := ctQuote;
  FQuoteMark := qc;
end;

procedure TCsvReader.SetCommentChar(cc: Char);
begin
  if CommentMark = cc then exit;
  CheckCommentChar(cc);
  FTable[CommentMark] := ctChar;
  FTable[cc] := ctComment;
  FCommentMark := cc;
end;

procedure TCsvReader.ReadDoc(aDoc: TCsvDoc);
begin
  Assert(aDoc <> nil); //let the compiler not worry
end;

procedure TCsvReader.ReadDocMasked(aDoc: TCsvDoc; const aMask: array of Boolean);
begin
  Assert(aDoc <> nil);
  Assert(System.Length(aMask) <> 0);
end;

procedure TCsvReader.FillTokenTable;
begin
  FTable[chTab] := ctWhiteSpace;
  FTable[chLF] := ctEol;
  FTable[chCR] := ctEol;
  FTable[chSpace] := ctWhiteSpace;
  FTable[chSharp] := ctComment;
  FTable[Delimiter] := ctDelimiter;
  FTable[QuoteMark] := ctQuote;
end;

function TCsvReader.GetCurrCell: string;
begin
  if ColIndex = NULL_INDEX then exit('');
  Result := FCurrRow[ColIndex];
end;

function TCsvReader.GetCurrRow: TStringArray;
begin
  if ColIndex < 0 then exit(nil);
  Result := System.Copy(FCurrRow, 0, Succ(ColIndex));
end;

function TCsvReader.GetCurrRowPtr: PRow;
begin
  Result := @FCurrRow;
end;

function TCsvReader.GetHasIssues: Boolean;
begin
  Result := FIssues.NonEmpty;
end;

function TCsvReader.GetIssueList: TIssueList;
begin
  Result := FIssues.ToArray;
end;

function TCsvReader.ReadFirstChunk: TReadState;
begin
  FBufIndex := 0;
  if not SourceIsBuffer then
    FBufCount := FSrcStream.Read(FBuffer^, FBufSize);
  if (FBufSize = 0) or (FBufCount = 0) then begin
    FParseState := psEmpty;
    HandleEvent(0, 0, FParseState);
    FReadState := rsEof;
    exit(rsEof);
  end;
  if roSkipBom in Options then begin
    FBom := DetectBom(PByte(FBuffer), FBufCount);
    case FBom of
      bkNone: ;
      bkUtf8: FBufIndex += UTF8_BOM_LEN;
    else
      FParseState := psEncoding;
      HandleEvent(0, 0, FParseState);
      FReadState := rsFatal;
      exit(rsFatal);
    end;
  end;
  FReadState := rsRead;
  Result := rsRead;
end;

function TCsvReader.ReadNextChunk: TReadState;
begin
  case ReadState of
    rsStart: exit(ReadFirstChunk);
    rsRead:
      if not SourceIsBuffer then begin
        FBufIndex := 0;
        FBufCount := FSrcStream.Read(FBuffer^, FBufSize);
        if FBufCount = 0 then
          FReadState := rsEof;
      end else FReadState := rsEof;
  else
  end;
  Result := ReadState;
end;

procedure TCsvReader.Error(aRow, aCol: SizeInt; aState: TParseState; c: Char);
begin
  case aState of
    psQmCell:
        raise ECsvRead.Create(aRow, aCol, Integer(ceHangQuote), SEHangQuoteFmt,
                              QuoteMark + FCellBuf.PieceUp2Char(Delimiter));
    psQmAfterWS, psDirtyQM: begin
        FCellBuf.Append(c);
        raise ECsvRead.Create(aRow, aCol, Integer(ceDirtyQuote), SEDirtyQuoteFmt, FCellBuf.ToString);
      end;
    psBareQM: begin
        FCellBuf.Append(QuoteMark);
        FCellBuf.Append(c);
        raise ECsvRead.Create(aRow, aCol, Integer(ceBareQuote), SEBareQuoteFmt, FCellBuf.ToString);
      end;
    psEmpty: raise ECsvRead.Create(Integer(ceEmptyDoc), SEEmptyDoc);
    psEncoding: raise ECsvRead.Create(Integer(ceUnsupportEncoding),
                                      Format(SEEncodingNotSuppFmt, [ENCODING_NAMES[Bom]]));
  else
  end;
end;

procedure TCsvReader.HandleEvent(aRow, aCol: SizeInt; var aState: TParseState; c: Char);
begin
  if roStrict in Options then
    Error(aRow, aCol, aState, c);
  case aState of
    psQmCell: begin
      FIssues.Add(TCsvIssue.Make(aRow, aCol, ceHangQuote));
      aState := psEndCell;
    end;
    psQmAfterWS, psDirtyQM: begin
      FCellBuf.Append(c);
      FIssues.Add(TCsvIssue.Make(aRow, aCol, ceDirtyQuote));
      aState := psCell;
    end;
    psBareQM: begin
      FCellBuf.Append(QuoteMark);
      FCellBuf.Append(c);
      FIssues.Add(TCsvIssue.Make(aRow, aCol, ceBareQuote));
      aState := psQmCell;
    end;
    psEmpty:    FIssues.Add(TCsvIssue.Make(aRow, aCol, ceEmptyDoc));
    psEncoding: FIssues.Add(TCsvIssue.Make(aRow, aCol, ceUnsupportEncoding));
  else
  end;
end;

procedure TCsvReader.AddCell;
begin
  Inc(FColIndex);
  if ColIndex >= System.Length(FCurrRow) then
    System.SetLength(FCurrRow, lgUtils.RoundUpTwoPower(Succ(ColIndex)));
  FCurrRow[ColIndex] := FCellBuf.ToString;
end;

const
{$PUSH}{$J-}
  CsvTransTable: array[psBeginRow..psComment, ctChar..ctEol] of TCsvReader.TParseState = (
   {             ctChar  |  ctWhiteSpace | ctDelimiter |  ctQuote  |  ctComment | ctEol }
   {psBeginRow} (psCell,     psLeadingWS,   psEndCell,   psOpenQM,    psComment,  psBeginRow),
   {psLeadingWS}(psCell,     psLeadingWS,   psEndCell,   psQmAfterWS, psCell,     psEndRow),
   {psOpenQM}   (psQmCell,   psQmCell,      psQmCell,    psEscape,    psQmCell,   psQmCell),
   {psCell}     (psCell,     psCell,        psEndCell,   psDirtyQM,   psCell,     psEndRow),
   {psQmCell}   (psQmCell,   psQmCell,      psQmCell,    psEscape,    psQmCell,   psQmCell),
   {psEscape}   (psBareQM,   psBareQM,      psEndCell,   psEscapedQM, psBareQM,   psEndRow),
   {psEscapedQM}(psQmCell,   psQmCell,      psQmCell,    psEscape,    psQmCell,   psQmCell),
   {psEndCell}  (psCell,     psLeadingWS,   psEndCell,   psOpenQM,    psCell,     psEndRow),
   {psComment}  (psComment,  psComment,     psComment,   psComment,   psComment,  psBeginRow)
  );
{$POP}

function TCsvReader.FlushLastCell: Boolean;
begin
  if State = psQmCell then
    HandleEvent(RowIndex, Succ(ColIndex), FParseState);
  if State in [psLeadingWS..psEndCell] then begin
    AddCell;
    Inc(FRowIndex);
    FParseState := psEndRow;
    exit(True);
  end;
  Result := False;
end;

function TCsvReader.DoRead(aSingleCol: Boolean): Boolean;
var
  c: Char;
  NewState: TParseState;
  LeadSpaces, DisableComment: Boolean;
begin
  if ReadState > rsRead then exit(False);
  LeadSpaces := not(roSkipLeadSpaces in Options);
  DisableComment := not(roComments in Options);
  Result := False;
  if State = psBeginRow then
    FColIndex := NULL_INDEX;
  repeat
    if BufIndex >= FBufCount then
      if ReadNextChunk > rsRead then begin
        Result := FlushLastCell; break;
      end;
    while BufIndex < FBufCount do
      begin
        c := ReadBuffer[BufIndex];
        NewState := CsvTransTable[State, FTable[c]];
        case NewState of
          psLeadingWS:
            if LeadSpaces then FCellBuf.Append(c);
          psCell, psQmCell: FCellBuf.Append(c);
          psEscapedQM: begin
              FCellBuf.Append(c);
              NewState := psQmCell;
            end;
          psEndCell: begin
              AddCell;
              Result := aSingleCol;
            end;
          psComment:
            if DisableComment then begin
              FCellBuf.Append(c);
              NewState := psCell;
            end;
          psEndRow:
            if not(State in [psBeginRow, psComment]) then begin
              AddCell;
              Inc(FRowIndex);
              NewState := psBeginRow;
              Result := True;
            end;
          psQmAfterWS:
            if LeadSpaces then
              HandleEvent(RowIndex, Succ(ColIndex), NewState, c)
            else
              NewState := psOpenQM;
          psDirtyQM, psBareQM: HandleEvent(RowIndex, Succ(ColIndex), NewState, c);
        else
        end;
        FParseState := NewState;
        Inc(FBufIndex);
        if Result then break;
      end;
  until Result or (ReadState > rsRead);
end;

procedure TCsvReader.HandleSkipEvent(aRow, aCol: SizeInt; var aState: TParseState);
begin
  case aState of
    psQmCell: begin
      FIssues.Add(TCsvIssue.Make(aRow, aCol, ceHangQuote));
      aState := psEndCell;
    end;
    psQmAfterWS, psDirtyQM: begin
      FIssues.Add(TCsvIssue.Make(aRow, aCol, ceDirtyQuote));
      aState := psCell;
    end;
    psBareQM: begin
      FIssues.Add(TCsvIssue.Make(aRow, aCol, ceBareQuote));
      aState := psQmCell;
    end;
    psEmpty:    FIssues.Add(TCsvIssue.Make(aRow, aCol, ceEmptyDoc));
    psEncoding: FIssues.Add(TCsvIssue.Make(aRow, aCol, ceUnsupportEncoding));
  else
  end;
end;

function TCsvReader.SkipLastCell: Boolean;
begin
  if State = psQmCell then
    HandleSkipEvent(RowIndex, Succ(ColIndex), FParseState);
  if State in [psLeadingWS..psEndCell] then begin
    Inc(FColIndex);
    Inc(FRowIndex);
    FParseState := psEndRow;
    exit(True);
  end;
  Result := False;
end;

function TCsvReader.DoSkip(aSingleCol: Boolean): Boolean;
var
  NewState: TParseState;
  SkipLeadSpace, DisableComment: Boolean;
begin
  if ReadState > rsRead then exit(False);
  SkipLeadSpace := roSkipLeadSpaces in Options;
  DisableComment := not(roComments in Options);
  Result := False;
  if State = psBeginRow then
    FColIndex := NULL_INDEX;
  repeat
    if BufIndex >= FBufCount then
      if ReadNextChunk > rsRead then begin
        Result := SkipLastCell; break;
      end;
    while BufIndex < FBufCount do
      begin
        NewState := CsvTransTable[State, FTable[ReadBuffer[BufIndex]]];
        case NewState of
          psLeadingWS, psCell, psQmCell: ;
          psEscapedQM:
            NewState := psQmCell;
          psEndCell: begin
              Inc(FColIndex);
              Result := aSingleCol;
            end;
          psComment:
            if DisableComment then NewState := psCell;
          psEndRow:
            if not(State in [psBeginRow, psComment]) then begin
              Inc(FColIndex);
              Inc(FRowIndex);
              NewState := psBeginRow;
              Result := True;
            end;
          psQmAfterWS:
            if SkipLeadSpace then
              NewState := psOpenQM
            else
              HandleSkipEvent(RowIndex, Succ(ColIndex), NewState);
          psDirtyQM, psBareQM: HandleSkipEvent(RowIndex, Succ(ColIndex), NewState);
        else
        end;
        FParseState := NewState;
        Inc(FBufIndex);
        if Result then break;
      end;
  until Result or (ReadState > rsRead);
end;

function TCsvReader.TryGetCell(aIndex: SizeInt): string;
begin
  Result := '';
  if (ColIndex >= 0) and (aIndex <= ColIndex) then
    Result := FCurrRow[aIndex];
end;

function TCsvReader.GetEof: Boolean;
begin
  Result := FReadState > rsRead;
end;

class function TCsvReader.New(aSource: TStream; aBufSize: SizeInt; aOwnsStream: Boolean): TCsvReader;
begin
  Result := TCsvReader.Create(aSource, aBufSize, aOwnsStream);
end;

class function TCsvReader.New(aSource: TStream; aOwnsStream: Boolean): TCsvReader;
begin
  Result := TCsvReader.Create(aSource, aOwnsStream);
end;

class function TCsvReader.NewCp(aSource: TStream; aCodePage: Integer; aOwnsStream: Boolean): TCsvReader;
begin
  Result := TCsvReader.CreateCp(aSource, aCodePage, aOwnsStream);
end;

class function TCsvReader.New(aSrcBuffer: PChar; aCount: SizeInt): TCsvReader;
begin
  Result := TCsvReader.Create(aSrcBuffer, aCount);
end;

class function TCsvReader.TryReadRow(aBuffer: PChar; aCount: SizeInt; out aRow: TStringArray;
  aMaster: TCsvEntity): SizeInt;
begin
  aRow := nil;
  if aCount < 1 then exit(0);
  with TCsvReader.Create(aBuffer, aCount) do
    try
      if aMaster <> nil then
        AssignCsvProps(aMaster);
      DoRead(False);
      if ReadState <> rsFatal then
        begin
          aRow := CurrentRow;
          Result := BufIndex;
        end
      else
        Result := NULL_INDEX;
    finally
      Free;
    end;
end;

constructor TCsvReader.Create(aSource: TStream; aBufSize: Sizeint; aOwnsStream: Boolean);
begin
  inherited Create;
  System.SetLength(FCurrRow, DEFAULT_CONTAINER_CAPACITY);
  FSrcStream := aSource;
  FBufSize := Math.Max(aBufSize, 0);
  if FBufSize = 0 then
    FBufSize := FSrcStream.Size - FSrcStream.Position; //todo: ???
  FBufIndex := FBufSize;
  FColIndex := NULL_INDEX;
  FOwnsStream := aOwnsStream;
  FBuffer := System.GetMem(FBufSize);
  FCellBuf.Reset(DEFAULT_CAP);
  FillTokenTable;
end;

constructor TCsvReader.Create(aSource: TStream; aOwnsStream: Boolean);
begin
  Create(aSource, DEF_BUF_SIZE, aOwnsStream);
end;

constructor TCsvReader.CreateCp(aSource: TStream; aCodePage: Integer; aOwnsStream: Boolean);
var
  Encoding: TEncoding;
  Bytes: TBytes;
  ByteCount: SizeInt;
  BomLen: Integer;
begin
  inherited Create;
{$PUSH}{$Q+}{$R+}
  ByteCount := aSource.Size - aSource.Position;
{$POP}
  System.SetLength(Bytes, ByteCount);
  aSource.ReadBuffer(Pointer(Bytes)^, ByteCount);
  if aOwnsStream then
    aSource.Free;
  Encoding := TEncoding.GetEncoding(aCodePage);
  BomLen := TEncoding.GetBufferEncoding(Bytes, Encoding);
  FData := Encoding.GetAnsiString(Bytes, BomLen, ByteCount - BomLen);
  if not TEncoding.IsStandardEncoding(Encoding) then
    Encoding.Free;
  FBuffer := Pointer(FData);
  FBufSize := System.Length(FData);
  FBufCount := FBufSize;
  FBufIndex := FBufCount;
  FColIndex := NULL_INDEX;
  FBufferSource := True;
  if FBufSize > 0 then
    System.SetLength(FCurrRow, DEFAULT_CONTAINER_CAPACITY);
  FillTokenTable;
end;

constructor TCsvReader.Create(aSrcBuffer: PChar; aCount: SizeInt);
begin
  inherited Create;
  FBuffer := aSrcBuffer;
  if aCount < 0 then
    aCount := 0;
  FBufSize := aCount;
  FBufCount := aCount;
  FBufIndex := FBufCount;
  FColIndex := NULL_INDEX;
  FBufferSource := True;
  if FBufSize > 0 then
    System.SetLength(FCurrRow, DEFAULT_CONTAINER_CAPACITY);
  FillTokenTable;
end;

destructor TCsvReader.Destroy;
begin
  if not SourceIsBuffer then
    System.FreeMem(FBuffer);
  if OwnsStream then
    FSrcStream.Free;
  inherited;
end;

procedure TCsvReader.AssignCsvProps(aSource: TCsvEntity);
begin
  if aSource.Delimiter <> Delimiter then begin
    if Delimiter < #33 then
      FTable[Delimiter] := ctWhiteSpace
    else
      FTable[Delimiter] := ctChar;
    FTable[aSource.Delimiter] := ctDelimiter;
    FDelimiter := aSource.Delimiter;
  end;
  if aSource.QuoteMark <> QuoteMark then begin
    FTable[QuoteMark] := ctChar;
    FTable[aSource.QuoteMark] := ctQuote;
    FQuoteMark := aSource.QuoteMark;
  end;
  if aSource.CommentMark <> CommentMark then begin
    FTable[CommentMark] := ctChar;
    FTable[aSource.CommentMark] := ctComment;
    FCommentMark := aSource.CommentMark;
  end;
  Options := aSource.Options;
  LineEndStyle := aSource.LineEndStyle;
  QuotePolicy := aSource.QuotePolicy;
end;

function TCsvReader.SetDelimiter(d: Char): TCsvReader;
begin
  Delimiter := d;
  Result := Self;
end;

function TCsvReader.SetQuoteMark(qm: Char): TCsvReader;
begin
  QuoteMark := qm;
  Result := Self;
end;

function TCsvReader.SetCommentMark(cm: Char): TCsvReader;
begin
  CommentMark := cm;
  Result := Self;
end;

function TCsvReader.SetOptions(const op: TReadOptions): TCsvReader;
begin
  Options := op;
  Result := Self;
end;

function TCsvReader.Skip: Boolean;
begin
  Result := DoSkip(True);
end;

function TCsvReader.Skip(aTimes: SizeInt): SizeInt;
begin
  Result := 0;
  while aTimes > 0 do
    begin
      if not Skip() then break;
      Dec(aTimes);
      Inc(Result);
    end;
end;

function TCsvReader.SkipRow: Boolean;
begin
  Result := DoSkip(False);
end;

function TCsvReader.SkipRow(aTimes: SizeInt): SizeInt;
begin
  Result := 0;
  while aTimes > 0 do
    begin
      if not SkipRow() then break;
      Dec(aTimes);
      Inc(Result);
    end;
end;

function TCsvReader.Read: Boolean;
begin
  if ReadState > rsRead then exit(False);
  Result := DoRead(True);
end;

function TCsvReader.ReadRow: Boolean;
begin
  if ReadState > rsRead then exit(False);
  Result := DoRead(False);
end;

function TCsvReader.ReadRow(const aMask: array of Boolean): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  if ReadState > rsRead then exit;
  if State = psBeginRow then
    I := 0
  else
    I := Succ(ColIndex);
  repeat
    if (I < System.Length(aMask)) and aMask[I] then begin
      if not Read then break;
      Inc(Result);
    end else
      if not Skip then break;
    I := Succ(ColIndex);
  until State = psBeginRow;
end;

procedure TCsvReader.Iterate(it: TOnIterate);
begin
  while DoRead(True) do
    if not it(Self) then
      break;
end;

procedure TCsvReader.Iterate(it: TNestIterate);
begin
  while DoRead(True) do
    if not it(Self) then
      break;
end;

procedure TCsvReader.IterateRow(it: TOnIterate);
begin
  while DoRead(False) do
    if not it(Self) then
      break;
end;

procedure TCsvReader.IterateRow(it: TNestIterate);
begin
  while DoRead(False) do
    if not it(Self) then
      break;
end;

procedure TCsvReader.IterateRow(it: TOnIterate; const aMask: array of Boolean);
begin
  while not Eof do
    if ReadRow(aMask) > 0 then
      if not it(Self) then
        break else
    else
      break;
end;

procedure TCsvReader.IterateRow(it: TNestIterate; const aMask: array of Boolean);
begin
  while not Eof do
    if ReadRow(aMask) > 0 then
      if not it(Self) then
        break else
    else
      break;
end;

{ TAcceptingDocReader }
{$PUSH}{$MACRO ON}
{$DEFINE DoReadDocMacro :=
  LeadSpaces := not(roSkipLeadSpaces in Options);
  DisableComment := not(roComments in Options);
  ColIdx := 0;
  RowIdx := 0;
  OldState := psBeginRow;
  repeat
    if BufIndex >= FBufCount then
      if ReadNextChunk > rsRead then begin
          FColIndex := Pred(ColIdx);
          FRowIndex := RowIdx;
          FParseState := OldState;
          if FlushLastCell then begin
            AddLastRowMacro;
          end;
          break;
        end;
    p := ReadBuffer;
    pEnd := p + FBufCount;
    while p < pEnd do
      begin
        NewState := CsvTransTable[OldState, FTable[p^]];
        case NewState of
          psLeadingWS:
            if LeadSpaces then FCellBuf.Append(p^);
          psCell, psQmCell: FCellBuf.Append(p^);
          psEscapedQM: begin
              FCellBuf.Append(p^);
              NewState := psQmCell;
            end;
          psEndCell: begin
              AddCellMacro;
            end;
          psComment:
            if DisableComment then begin
              FCellBuf.Append(p^);
              NewState := psCell;
            end;
          psEndRow:
            if not(OldState in [psBeginRow, psComment]) then begin
              AddCellMacro;
              AddRowMacro;
              ColIdx := 0;
              Inc(RowIdx);
              NewState := psBeginRow;
            end;
          psQmAfterWS:
            if LeadSpaces then
              HandleEvent(RowIdx, ColIdx, NewState, p^)
            else
              NewState := psOpenQM;
          psDirtyQM, psBareQM: HandleEvent(RowIdx, ColIdx, NewState, p^);
        else
        end;
        Inc(p);
        OldState := NewState;
      end;
    FBufIndex += FBufCount;
  until ReadState > rsRead
}
{$DEFINE AddCellMacro :=
  if ColIdx >= System.Length(FCurrRow) then
    System.SetLength(FCurrRow, lgUtils.RoundUpTwoPower(Succ(ColIdx)));
  FCurrRow[ColIdx] := FCellBuf.ToString;
  Inc(ColIdx)
}
{$DEFINE AddRowMacro :=
  NewRow := aDoc.AddRow(ColIdx);
  System.Move(Pointer(FCurrRow)^, Pointer(NewRow^)^, SizeOf(string)*ColIdx);
  System.FillChar(Pointer(FCurrRow)^, SizeOf(string)*ColIdx, 0)
}
{$DEFINE AddLastRowMacro :=
  ColIdx := Succ(ColIndex);
  NewRow := aDoc.AddRow(ColIdx);
  System.Move(Pointer(FCurrRow)^, Pointer(NewRow^)^, SizeOf(string)*ColIdx);
  System.FillChar(Pointer(FCurrRow)^, SizeOf(string)*ColIdx, 0)
}
procedure TAcceptingDocReader.DoReadDoc(aDoc: TCsvDoc);
var
  OldState, NewState: TParseState;
  ColIdx, RowIdx: SizeInt;
  p, pEnd: PChar;
  NewRow: TCsvDoc.PRow;
  LeadSpaces, DisableComment: Boolean;
begin
  DoReadDocMacro;
end;

procedure TAcceptingDocReader.DoReadDocMasked(aDoc: TCsvDoc; const aMask: array of Boolean);
var
  OldState, NewState: TParseState;
  ColIdx, RealColIdx, RowIdx, MaskHigh: SizeInt;
  ColMasked: Boolean;
  p, pEnd: PChar;
  NewRow: TCsvDoc.PRow;
  LeadSpaces, DisableComment: Boolean;
begin
  LeadSpaces := not(roSkipLeadSpaces in Options);
  DisableComment := not(roComments in Options);
  MaskHigh := System.High(aMask);
  ColIdx := 0;
  RowIdx := 0;
  RealColIdx := 0;
  OldState := psBeginRow;
  ColMasked := (RealColIdx <= MaskHigh) and aMask[RealColIdx];
  repeat
    if BufIndex >= FBufCount then
      if ReadNextChunk > rsRead then begin
          if ColIdx > 0 then begin
            if ColMasked then begin
              FColIndex := Pred(ColIdx);
              FRowIndex := RowIdx;
              FParseState := OldState;
              FlushLastCell;
              ColIdx := Succ(ColIndex);
            end;
            NewRow := aDoc.AddRow(ColIdx);
            System.Move(Pointer(FCurrRow)^, Pointer(NewRow^)^, SizeOf(string)*ColIdx);
            System.FillChar(Pointer(FCurrRow)^, SizeOf(string)*ColIdx, 0);
          end;
          break;
        end;
    p := ReadBuffer;
    pEnd := p + FBufCount;
    while p < pEnd do
      begin
        NewState := CsvTransTable[OldState, FTable[p^]];
        case NewState of
          psLeadingWS:
            if LeadSpaces and ColMasked then FCellBuf.Append(p^);
          psCell, psQmCell:
            if ColMasked then FCellBuf.Append(p^);
          psEscapedQM: begin
              if ColMasked then
                FCellBuf.Append(p^);
              NewState := psQmCell;
            end;
          psEndCell: begin
              //////////
              if ColMasked then begin
                if ColIdx >= System.Length(FCurrRow) then
                  System.SetLength(FCurrRow, lgUtils.RoundUpTwoPower(Succ(ColIdx)));
                FCurrRow[ColIdx] := FCellBuf.ToString;
                Inc(ColIdx);
              end;
              ///////////
              Inc(RealColIdx);
              ColMasked := (RealColIdx <= MaskHigh) and aMask[RealColIdx];
            end;
          psComment:
            if DisableComment then begin
              if ColMasked then
                FCellBuf.Append(p^);
              NewState := psCell;
            end;
          psEndRow:
            if not(OldState in [psBeginRow, psComment]) then begin
              //////////
              if ColMasked then begin
                if ColIdx >= System.Length(FCurrRow) then
                  System.SetLength(FCurrRow, lgUtils.RoundUpTwoPower(Succ(ColIdx)));
                FCurrRow[ColIdx] := FCellBuf.ToString;
                Inc(ColIdx);
              end;
              //////////
              NewRow := aDoc.AddRow(ColIdx);
              System.Move(Pointer(FCurrRow)^, Pointer(NewRow^)^, SizeOf(string)*ColIdx);
              System.FillChar(Pointer(FCurrRow)^, SizeOf(string)*ColIdx, 0);
              ///////////
              ColIdx := 0;
              RealColIdx := 0;
              Inc(RowIdx);
              ColMasked := (RealColIdx <= MaskHigh) and aMask[RealColIdx];
              NewState := psBeginRow;
            end;
          psQmAfterWS:
            if LeadSpaces then
              if ColMasked then HandleEvent(RowIdx, ColIdx, NewState, p^)
              else NewState := psCell
            else
              NewState := psOpenQM;
          psDirtyQM, psBareQM:
            if ColMasked then HandleEvent(RowIdx, ColIdx, NewState, p^)
            else if NewState = psDirtyQM then NewState := psCell else NewState := psQMCell;
        else
        end;
        Inc(p);
        OldState := NewState;
      end;
    FBufIndex += FBufCount;
  until ReadState > rsRead;
end;

procedure TAcceptingDocReader.ReadDoc(aDoc: TCsvDoc);
begin
  AssignCsvProps(aDoc);
  DoReadDoc(aDoc);
  if HasIssues then
    aDoc.FIssueList := FIssues.ToArray;
end;

procedure TAcceptingDocReader.ReadDocMasked(aDoc: TCsvDoc; const aMask: array of Boolean);
begin
  AssignCsvProps(aDoc);
  DoReadDocMasked(aDoc, aMask);
  if HasIssues then
    aDoc.FIssueList := FIssues.ToArray;
end;

{ TIgnoringDocReader }
{$DEFINE AddRowMacro :=
  if HeaderLen = 0 then
    HeaderLen := ColIdx;
  if ColIdx = HeaderLen then begin
    NewRow := aDoc.AddRow(ColIdx);
    System.Move(Pointer(FCurrRow)^, Pointer(NewRow^)^, SizeOf(string)*ColIdx);
    System.FillChar(Pointer(FCurrRow)^, SizeOf(string)*ColIdx, 0);
  end
}
{$DEFINE AddLastRowMacro :=
  ColIdx := Succ(ColIndex);
  if HeaderLen = 0 then
    HeaderLen := ColIdx;
  if ColIdx = HeaderLen then begin
    NewRow := aDoc.AddRow(ColIdx);
    System.Move(Pointer(FCurrRow)^, Pointer(NewRow^)^, SizeOf(string)*ColIdx);
    System.FillChar(Pointer(FCurrRow)^, SizeOf(string)*ColIdx, 0);
  end
}
procedure TIgnoringDocReader.DoReadDoc(aDoc: TCsvDoc);
var
  OldState, NewState: TParseState;
  ColIdx, RowIdx, HeaderLen: SizeInt;
  p, pEnd: PChar;
  NewRow: TCsvDoc.PRow;
  LeadSpaces, DisableComment: Boolean;
begin
  HeaderLen := 0;
  DoReadDocMacro;
end;

{ TStrictDocReader }
{$DEFINE AddRowMacro :=
  if HeaderLen = 0 then
    HeaderLen := ColIdx;
  if ColIdx <> HeaderLen then
    raise ECsvRead.Create(RowIdx, ColIdx, Integer(ceColumnCount), SEColCountFmt);
  NewRow := aDoc.AddRow(ColIdx);
  System.Move(Pointer(FCurrRow)^, Pointer(NewRow^)^, SizeOf(string)*ColIdx);
  System.FillChar(Pointer(FCurrRow)^, SizeOf(string)*ColIdx, 0)
}
{$DEFINE AddLastRowMacro :=
  ColIdx := Succ(ColIndex);
  if HeaderLen = 0 then
    HeaderLen := ColIdx;
  if ColIdx <> HeaderLen then
    raise ECsvRead.Create(RowIndex, ColIdx, Integer(ceColumnCount), SEColCountFmt);
  NewRow := aDoc.AddRow(ColIdx);
  System.Move(Pointer(FCurrRow)^, Pointer(NewRow^)^, SizeOf(string)*ColIdx);
  System.FillChar(Pointer(FCurrRow)^, SizeOf(string)*ColIdx, 0)
}
procedure TStrictDocReader.DoReadDoc(aDoc: TCsvDoc);
var
  OldState, NewState: TParseState;
  ColIdx, RowIdx, HeaderLen: SizeInt;
  p, pEnd: PChar;
  NewRow: TCsvDoc.PRow;
  LeadSpaces, DisableComment: Boolean;
begin
  HeaderLen := 0;
  DoReadDocMacro;
end;
{$UNDEF DoReadDocMacro}{$UNDEF AddCellMacro}{$UNDEF AddRowMacro}{$UNDEF AddLastRowMacro}{$POP}

{ TCsvWriter }

function TCsvWriter.GetLineBreak(les: TLineEndStyle): string;
begin
  case les of
    lesCRLF: Result := chCR+chLF;
    lesCR:   Result := chCR;
  else //lesLF
    Result := chLF;
  end;
end;

procedure TCsvWriter.DoWriteEol;
begin
  if System.Length(FLineBreak) > 1 then
    FCellBuf.AppendCrLf
  else
    FCellBuf.Append(FLineBreak[1]);
  FCellBuf.Write2Stream(FStream);
  Inc(FRowIndex);
  FColIndex := NULL_INDEX;
end;

procedure TCsvWriter.DoWriteCell(const p: PAnsiChar; aCount: SizeInt);
var
  I: SizeInt;
  NeedQuotes: Boolean;
begin
  if ColIndex >= 0 then
    FCellBuf.Append(Delimiter);
  NeedQuotes := QuotePolicy = qpForce;
  if not NeedQuotes then
    for I := 0 to Pred(aCount) do
      if p[I] in FSpecChars then begin
        NeedQuotes := True;
        break;
      end;
  if NeedQuotes then FCellBuf.Append(QuoteMark);
  for I := 0 to Pred(aCount) do begin
    if p[I] = QuoteMark then
      FCellBuf.Append(QuoteMark);
    FCellBuf.Append(p[I]);
  end;
  if NeedQuotes then FCellBuf.Append(QuoteMark);
  Inc(FColIndex);
end;

procedure TCsvWriter.DoWriteRow(const r: TStringArray);
var
  I: SizeInt;
begin
  if ColIndex >= 0 then
    DoWriteEol;
  for I := 0 to System.High(r) do
    DoWriteCell(Pointer(r[I]), System.Length(r[I]));
  DoWriteEol;
end;

function TCsvWriter.GetOwnsStream: Boolean;
begin
  Result := FOwnStream <> nil;
end;

function TCsvWriter.WriteDoc(aDoc: TCsvDoc): SizeInt;
var
  I: SizeInt;
begin
  AssignCsvProps(aDoc);
  Result := FStream.Position;
  for I := 0 to Pred(aDoc.RowCount) do
    DoWriteRow(aDoc.FCells.UncMutable[I]^);
  Result := FStream.Position - Result;
end;

procedure TCsvWriter.SetDelimiterChar(d: Char);
begin
  if d = Delimiter then exit;
  CheckDelimiterChar(d);
  Exclude(FSpecChars, Delimiter);
  FDelimiter := d;
  Include(FSpecChars, Delimiter);
end;

procedure TCsvWriter.SetQuoteChar(qc: Char);
begin
  if qc = QuoteMark then exit;
  CheckQuoteChar(qc);
  Exclude(FSpecChars, QuoteMark);
  FQuoteMark := qc;
  Include(FSpecChars, QuoteMark);
end;

procedure TCsvWriter.SetLEStyle(les: TLineEndStyle);
begin
  if les = LineEndStyle then exit;
  FLEStyle := les;
  FLineBreak := GetLineBreak(LineEndStyle);
end;

class function TCsvWriter.New(aStream: TStream; aBufSize: SizeInt; aOwnsStream: Boolean): TCsvWriter;
begin
  Result := TCsvWriter.Create(aStream, aBufSize, aOwnsStream);
end;

class function TCsvWriter.New(aStream: TStream; aOwnsStream: Boolean): TCsvWriter;
begin
  Result := TCsvWriter.Create(aStream, aOwnsStream);
end;

constructor TCsvWriter.Create(aStream: TStream; aBufSize: SizeInt; aOwnsStream: Boolean);
begin
  inherited Create;
  if aOwnsStream then
    FOwnStream := aStream;
  if aBufSize > 0 then
    FStream := TWriteBufStream.Create(aStream, aBufSize)
  else
    FStream := TWriteBufStream.Create(aStream);
  FColIndex := NULL_INDEX;
  FLineBreak := GetLineBreak(LineEndStyle);
  FSpecChars := [chLF, chCR, Delimiter, QuoteMark];
  FCellBuf.Reset(TCsvReader.DEFAULT_CAP);
end;

constructor TCsvWriter.Create(aStream: TStream; aOwnsStream: Boolean);
begin
  Create(aStream, NULL_INDEX, aOwnsStream);
end;

destructor TCsvWriter.Destroy;
begin
  FCellBuf.Write2Stream(FStream);
  FStream.Free;
  FOwnStream.Free;
  inherited;
end;

procedure TCsvWriter.AssignCsvProps(aSource: TCsvEntity);
begin
  if aSource.Delimiter <> Delimiter then begin
    Exclude(FSpecChars, Delimiter);
    FDelimiter := aSource.Delimiter;
    Include(FSpecChars, Delimiter);
  end;
  if aSource.QuoteMark <> QuoteMark then begin
    Exclude(FSpecChars, QuoteMark);
    FQuoteMark := aSource.QuoteMark;
    Include(FSpecChars, QuoteMark);
  end;
  FCommentMark := aSource.CommentMark;
  Options := aSource.Options;
  LineEndStyle := aSource.LineEndStyle;
  QuotePolicy := aSource.QuotePolicy;
end;

function TCsvWriter.SetDelimiter(d: Char): TCsvWriter;
begin
  Delimiter := d;
  Result := Self;
end;

function TCsvWriter.SetQuoteMark(qm: Char): TCsvWriter;
begin
  QuoteMark := qm;
  Result := Self;
end;

function TCsvWriter.SetLineEndStyle(les: TLineEndStyle): TCsvWriter;
begin
  LineEndStyle := les;
  Result := Self;
end;

function TCsvWriter.SetQuotePolicy(qp: TQuotePolicy): TCsvWriter;
begin
  QuotePolicy := qp;
  Result := Self;
end;

function TCsvWriter.AddCell(const aValue: string): TCsvWriter;
begin
  DoWriteCell(Pointer(aValue), System.Length(aValue));
  Result := Self;
end;

function TCsvWriter.AddCell(const aBuffer; aCount: SizeInt): TCsvWriter;
begin
  DoWriteCell(@aBuffer, aCount);
  Result := Self;
end;

function TCsvWriter.AddEol: TCsvWriter;
begin
  DoWriteEol;
  Result := Self;
end;

function TCsvWriter.AddRow(const aRow: TStringArray): TCsvWriter;
begin
  DoWriteRow(aRow);
  Result := Self;
end;

end.

