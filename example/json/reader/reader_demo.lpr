program reader_demo;

{$mode delphi}
{$modeswitch nestedprocvars}

uses
  LazUtf8, Classes, SysUtils, lgUtils, lgJson;

const
  Json =
  '[' +
  '  {' +
  '    "props": {"top": 100, "left": 200, "width": 250, "height": 200},' +
  '    "items": ["textbox", "checkbox", "button"],' +
  '    "enabled": true' +
  '  },' +
  '  {' +
  '    "props": {"top": 210, "left": 250, "width": 200, "height": 150},' +
  '    "items": ["image", "textbox"],' +
  '    "enabled": false' +
  '  },' +
  '  {"any more": null},' +
  '  "the end"' +
  ']';

//auxiliary routines
/////////////////////////////////////////////////////////
function TokenName(aKind: TJsonReader.TTokenKind): string;
begin
  WriteStr(Result, aKind);
end;

function StructName(aKind: TJsonReader.TStructKind): string;
begin
  WriteStr(Result, aKind);
end;

function StateName(aState: TJsonReader.TReadState): string;
begin
  WriteStr(Result, aState);
end;

procedure PrintState(aReader: TJsonReader);
begin
  WriteLn(
    'token: ', TokenName(aReader.TokenKind),
    ', depth: ', aReader.Depth,
    ', struct: ', StructName(aReader.StructKind),
    ', name: ', aReader.Name,
    ', value: ', aReader.ValueToString
    );
end;
/////////////////////////////////////////////////////////
var
  Stream: TGUniqRef<TStringStream>;
  Reader: TGUniqRef<TJsonReader>;

procedure Read;
begin
  Stream.Instance.Position := 0;
  {%H-}Reader.Instance := TJsonReader.Create(Stream.Instance);

  WriteLn('In this example, we read all tokens from the stream one by one ' +
          'and watch the state of the reader change.');
  WriteLn('Reader state: ', StateName(Reader.Instance.ReadState));
  while Reader.Instance.Read do
    PrintState(Reader.Instance);
  WriteLn('Reader state: ', StateName(Reader.Instance.ReadState));

  WriteLn;
end;

procedure Count;
var
  Counter: Integer = 0;
begin
  Stream.Instance.Position := 0;
  Reader.Instance := TJsonReader.Create(Stream.Instance);

  WriteLn('Now let''s try to count the number of elements in the our instance.');
  if not Reader.Instance.Read then begin
    WriteLn('Something went wrong, reader state: ', StateName(Reader.Instance.ReadState));
    exit;
  end;
  WriteLn('done:');
  if not TJsonReader.IsStartToken(Reader.Instance.TokenKind) then begin
    Write('An instance contains a single element - ');
    case Reader.Instance.TokenKind of
      rtkNull:   WriteLn('null');
      rtkFalse:  WriteLn('false');
      rtkTrue:   WriteLn('true');
      rtkNumber: WriteLn('number ', Reader.Instance.ValueToString);
      rtkString: WriteLn('string ', Reader.Instance.ValueToString);
    else
    end;
  end else begin
    if Reader.Instance.TokenKind = rtkArrayBegin then
      Write('An instance is an array and contains ')
    else
      Write('An instance is an object and contains ');
    while Reader.Instance.MoveNext do
      Inc(Counter);
    WriteLn(Counter, ' elements.');
  end;

  WriteLn;
end;

procedure Iterate;
  function PrintValue(aReader: TJsonReader): Boolean;
  begin
    WriteLn(aReader.Name, ': ', aReader.ValueToString);
    Result := True;
  end;
begin
  Stream.Instance.Position := 0;
  Reader.Instance := TJsonReader.Create(Stream.Instance);

  WriteLn('Now let''s try to iterate over all value items.');
  Reader.Instance.Iterate(PrintValue);

  WriteLn;
end;

procedure Iterate2;
const
  IndentSize = 2;
  function Struct(aReader: TJsonReader): Boolean;
  var
    Indent: string = '';
  begin
    if aReader.ParentName <> '' then begin
      SetLength(Indent, IndentSize * Pred(aReader.Depth));
      FillChar(Pointer(Indent)^, Length(Indent), 32);
      WriteLn(Indent, aReader.ParentName, ': ');
    end;
    Result := True;
  end;
  function Value(aReader: TJsonReader): Boolean;
  var
    Indent: string = '';
  begin
    SetLength(Indent, IndentSize * aReader.Depth);
    FillChar(Pointer(Indent)^, Length(Indent), 32);
    WriteLn(Indent, aReader.Name, ': ', aReader.ValueToString);
    Result := True;
  end;
begin
  Stream.Instance.Position := 0;
  Reader.Instance := TJsonReader.Create(Stream.Instance);

  WriteLn('Now let''s try to iterate over all items.');
  Reader.Instance.Iterate(Struct, Value);

  WriteLn;
end;

procedure FindPath;
  function PrintValue(aReader: TJsonReader): Boolean;
  begin
    WriteLn(aReader.Name, ': ', aReader.ValueToString);
    Result := True;
  end;
  procedure Find(aReader: TJsonReader; const aPath: string);
  begin
    WriteLn('Search path: ', aPath);
    if aReader.FindPath(TJsonPtr.From(aPath)) then begin
      Write('Found item and it is ');
      if aReader.IsStartToken(aReader.TokenKind) then begin
        if aReader.TokenKind = rtkArrayBegin then
          WriteLn('an array.')
        else
          WriteLn('an object.');
        WriteLn('Now iterate over it:');
        aReader.Iterate(PrintValue);
      end else
        WriteLn('a value: ', aReader.ValueToString);
    end else
      WriteLn('Nothing was found in the specified path.');
  end;
begin
  Stream.Instance.Position := 0;
  Reader.Instance := TJsonReader.Create(Stream.Instance);

  WriteLn('Now let''s try to find an item by the specified path and iterate over all its elements.');
  Find(Reader.Instance, '/1/props');

  Stream.Instance.Position := 0;
  Reader.Instance := TJsonReader.Create(Stream.Instance);

  WriteLn('Let''s try another path');
  Find(Reader.Instance, '/3');

  WriteLn;
end;

procedure CopyStruct;
var
  s: string;
begin
  Stream.Instance.Position := 0;
  Reader.Instance := TJsonReader.Create(Stream.Instance);

  WriteLn('Now let''s try to find the structure at ''/0/items'' and copy it.');
  if Reader.Instance.FindPath(TJsonPtr.From('/0/items')) and Reader.Instance.CopyStruct(s) then
    begin
      WriteLn('done: ');
      WriteLn(s);
    end
  else
    WriteLn('Something went wrong, reader state: ', StateName(Reader.Instance.ReadState));

  WriteLn;
end;

procedure Formatting;
const
  IndentSize = 2;
var
  Indent: string = '';
  PrevToken: TJsonReader.TTokenKind = rtkNone;
  CurrToken: TJsonReader.TTokenKind;
  procedure MakeIndent(aSize: Integer);
  begin
    SetLength(Indent, IndentSize * aSize);
    FillChar(Pointer(Indent)^, Length(Indent), 32);
  end;
begin
  Stream.Instance.Position := 0;
  Reader.Instance := TJsonReader.Create(Stream.Instance);
  WriteLn('Now let''s try to print our JSON in formatted form.');
  WriteLn;
  while Reader.Instance.Read do begin
    CurrToken := Reader.Instance.TokenKind;
    if PrevToken <> rtkNone then
      if TJsonReader.IsStartToken(PrevToken) or TJsonReader.IsEndToken(CurrToken) then
        WriteLn
      else
        WriteLn(',');
    case CurrToken of
      rtkNull, rtkFalse, rtkTrue, rtkNumber, rtkString:
        begin
          MakeIndent(Reader.Instance.Depth);
          if Reader.Instance.StructKind = skArray then
            Write(Indent, Reader.Instance.ValueToString)
          else
            Write(Indent, Reader.Instance.Name, ': ', Reader.Instance.ValueToString);
        end;
      rtkArrayBegin:
        begin
          MakeIndent(Reader.Instance.Depth - 1);
          if Reader.Instance.ParentKind = skObject then
            WriteLn(Indent, Reader.Instance.ParentName, ': ');
          Write(Indent, '[');
        end;
      rtkObjectBegin:
        begin
          MakeIndent(Reader.Instance.Depth - 1);
          if Reader.Instance.ParentKind = skObject then
            WriteLn(Indent, Reader.Instance.ParentName, ': ');
          Write(Indent, '{');
        end;
      rtkArrayEnd:
        begin
          MakeIndent(Reader.Instance.Depth);
          Write(Indent, ']');
        end;
      rtkObjectEnd:
        begin
          MakeIndent(Reader.Instance.Depth);
          Write(Indent, '}');
        end;
    else
    end;
    PrevToken := CurrToken;
  end;
  WriteLn;
end;

procedure PrintInfo;
begin
  WriteLn('This demo contains several examples of using the TJsonReader class.');
  WriteLn('  If you are going to execute any of them,');
  WriteLn('  press the ''y'' key,');
  WriteLn('  otherwise press any other key.');
  Write('>>> ');
end;

procedure PrintMenu;
begin
  WriteLn('select the example you need:');
  WriteLn('  0 - sequential reading of tokens');
  WriteLn('  1 - counting the number of elements');
  WriteLn('  2 - iteration over all value items');
  WriteLn('  3 - iteration over all items');
  WriteLn('  4 - search for an element in the specified path');
  WriteLn('  5 - copying a nested structure');
  WriteLn('  6 - formatted document printing');
  WriteLn('      press any other key to exit');
  Write('>>> ');
end;

var
  Answer: string;
  Stop: Boolean = False;

begin
  {%H-}Stream.Instance := TStringStream.Create(Json);
  PrintInfo;
  ReadLn(Answer);
  if (Length(Answer) <> 1) or (LowerCase(Answer[1]) <> 'y') then
    Stop := True;
  while not Stop do begin
    WriteLn;
    PrintMenu;
    ReadLn(Answer);
    if (Length(Answer) = 1) and (Answer[1] in ['0','1','2','3','4','5','6']) then begin
      WriteLn;
      case Answer[1] of
        '0': Read;
        '1': Count;
        '2': Iterate;
        '3': Iterate2;
        '4': FindPath;
        '5': CopyStruct;
        '6': Formatting;
      end;
      Write('Press any key to finish ');
      ReadLn;
    end else
      Stop := True;
  end;
  WriteLn('JSON reader demo exit.');
end.

