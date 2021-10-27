unit LGJsonConfTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  lgUtils,
  lgJson,
  lgJsonCfg;

type

  { TTestJsonConf  just repeats tests for TJSONConfig}
  TTestJsonConf = class(TTestCase)
  private
    function  CreateConf(const aFileName: string): TJsonConf;
    procedure ClearConf(aConf: TJsonConf; aDeleteFile: Boolean = True);
    procedure AssertStrings(const aMsg: string; aList: TStrings; const v: array of string);
  published
    procedure TestDataTypes;
    procedure TestSubNodes;
    procedure TestEnumSubkeys;
    procedure TestEnumValues;
    procedure TestClear;
    procedure TestOpenKey;
    procedure TestStrings;
    procedure TestVarArray;
    procedure TestPairArray;
    procedure TestDuplicates;
  end;

implementation

function TTestJsonConf.CreateConf(const aFileName: string): TJsonConf;
begin
  Result := TJsonConf.Create(nil);
  Result.FileName := aFileName;
end;

procedure TTestJsonConf.ClearConf(aConf: TJsonConf; aDeleteFile: Boolean);
begin
  aConf.Clear;
  if aDeleteFile then
    DeleteFile(aConf.Filename);
  aConf.Free;
end;

procedure TTestJsonConf.AssertStrings(const aMsg: string; aList: TStrings; const v: array of string);
var
  I: Integer;
const
  Fmt = '%s: %s';
begin
  AssertNotNull(Format(Fmt, [aMsg, 'Have strings']), aList);
  AssertEquals(Format(Fmt, [aMsg, 'Correct element count']), Length(v), aList.Count);
  for I := 0 to Pred(aList.Count) do
    AssertEquals(Format(Fmt, [aMsg, 'element ' + IntToStr(I)]), v[I], aList[I]);
end;

procedure TTestJsonConf.TestDataTypes;
const
  a = Integer(1);
  b = 'A string';
  c = 1.23;
  d = True;
  e = Int64($FFFFFFFFFFFFF);
var
  Conf: TJsonConf;
begin
  Conf := CreateConf('test.json');
  try
    Conf.SetValue('a', a);
    Conf.SetValue('b', b);
    Conf.SetValue('c', c);
    Conf.SetValue('d', d);
    Conf.SetValue('e', e);

    AssertEquals('Integer Read/Write', a, Conf.GetValue('a', 0));
    AssertEquals('String Read/Write', b, Conf.GetValue('b', ''));
    AssertEquals('Double Read/Write', c, Conf.GetValue('c', 0.0), 0.01);
    AssertEquals('Boolean Read/Write', d, Conf.GetValue('d', False));
    AssertEquals('Int64 Read/Write', e, Conf.GetValue('e', Int64(0)));
  finally
    ClearConf(Conf, True);
  end;
end;

procedure TTestJsonConf.TestSubNodes;
var
  Conf: TJsonConf;
begin
  Conf := CreateConf('test.json');
  try
    Conf.SetValue('a', 1);
    Conf.SetValue('b/a', 2);
    Conf.SetValue('b/c/a', 3);
    AssertEquals('Read at root', 1, Conf.GetValue('a', 0));
    AssertEquals('Read at root', 2, Conf.GetValue('b/a', 2));
    AssertEquals('Read at root', 3, Conf.GetValue('b/c/a', 3));
  finally
    ClearConf(Conf, True);
  end;
end;

procedure TTestJsonConf.TestEnumSubkeys;
var
  Conf: TJsonConf;
  List: specialize TGAutoRef<TStringList>;
begin
  Conf := CreateConf('test.json');
  try
    Conf.SetValue('/a', 1);
    Conf.SetValue('/b/a', 2);
    Conf.SetValue('/b/b', 2);
    Conf.SetValue('/c/a', 3);
    Conf.SetValue('/c/b/a', 4);
    Conf.SetValue('/c/c/a', 4);
    Conf.SetValue('/c/d/d', 4);
    Conf.EnumSubKeys('/', List.Instance);
    AssertEquals('EnumSubkeys count', List.Instance.Count, 2);
    AssertEquals('EnumSubkeys first element', List.Instance[0], 'b');
    AssertEquals('EnumSubkeys second element', List.Instance[1], 'c');
  finally
    ClearConf(Conf, True);
  end;
end;

procedure TTestJsonConf.TestEnumValues;
var
  Conf: TJsonConf;
  List: specialize TGAutoRef<TStringList>;
begin
  Conf := CreateConf('test.json');
  try
    Conf.SetValue('/a', 1);
    Conf.SetValue('/b/a', 2);
    Conf.SetValue('/b/b', 2);
    Conf.SetValue('/c/a', 3);
    Conf.SetValue('/c/b/a', 4);
    Conf.SetValue('/c/c/a', 4);
    Conf.SetValue('/c/d/d', 4);
    Conf.EnumValues('/', List.Instance);
    AssertEquals('EnumValues count', 1, List.Instance.Count);
    AssertEquals('EnumValues first element', 'a', List.Instance[0]);
    List.Instance.Clear;
    Conf.EnumValues('/b', List.Instance);
    AssertEquals('EnumValues subkey count', 2, List.Instance.Count);
    AssertEquals('EnumValues subkey first element', 'a', List.Instance[0]);
    AssertEquals('EnumValues subkey second element', 'b', List.Instance[1]);
  finally
    ClearConf(Conf, True);
  end;
end;

procedure TTestJsonConf.TestClear;
var
  Conf: TJsonConf;
begin
  Conf := CreateConf('test.json');
  try
    Conf.SetValue('a', 1);
    Conf.Flush;
    Conf.DeleteValue('a');
    AssertEquals('Modified set', True, Conf.Modified);
    AssertEquals('Delete value', 0, Conf.GetValue('a', 0));
    Conf.SetValue('b/a', 1);
    Conf.SetValue('b/c', 2);
    Conf.DeleteValue('b/a');
    AssertEquals('Delete value in subkey', 0, Conf.GetValue('a', 0));
    AssertEquals('Delete value only clears deleted value', 2, Conf.GetValue('b/c', 0));
    Conf.SetValue('b/a', 1);
    Conf.Flush;
    Conf.DeletePath('b');
    AssertEquals('Modified set', True, Conf.Modified);
    AssertEquals('Delete path', 0, Conf.GetValue('b/a', 0));
    AssertEquals('Delete path deletes all values', 0, Conf.GetValue('b/c', 0));
    Conf.Clear;
    AssertEquals('Clear', 0, Conf.GetValue('/a', 0));
  finally
    ClearConf(Conf, True);
  end;
end;

procedure TTestJsonConf.TestOpenKey;
var
  Conf: TJsonConf;
  List: specialize TGAutoRef<TStringList>;
begin
  Conf := CreateConf('test.json');
  try
    Conf.SetValue('a', 1);
    Conf.SetValue('b/a', 2);
    Conf.SetValue('b/b', 2);
    Conf.SetValue('b/c/a', 3);
    Conf.SetValue('b/c/b', 3);
    Conf.OpenKey('/b', False);
    AssertEquals('Read relative to key a', 2, Conf.GetValue('a', 0));
    AssertEquals('Read relative to key b', 2,Conf.GetValue('b', 0));
    AssertEquals('Read in subkey relative to key a', 3, Conf.GetValue('c/a', 0));
    AssertEquals('Read in subkey relative to key b', 3, Conf.GetValue('c/b', 0));
    AssertEquals('Read absolute, disregarding key', 1, Conf.GetValue('/a', 0));
    AssertEquals('Read absolute in subkey, disregarding key', 2, Conf.GetValue('/b/a', 0));
    AssertEquals('Read absolute in subkeys, disregarding key', 3, Conf.GetValue('/b/c/a', 0));
    Conf.CloseKey;
    AssertEquals('CloseKey', 1, Conf.GetValue('a', 0));
    Conf.OpenKey('b', False);
    Conf.OpenKey('c', False);
    AssertEquals('Open relative key', 3, Conf.GetValue('a', 0));
    Conf.ResetKey;
    AssertEquals('ResetKey', 1, Conf.GetValue('a', 0));
    Conf.Clear;
    Conf.EnumSubKeys('/', List.Instance);
    AssertEquals('Clear failed', 0, List.Instance.Count);
    Conf.OpenKey('/a/b/c/d', True);
    Conf.EnumSubKeys('/a', List.Instance);
    AssertEquals('OpenKey with aForceKey, level 1', 1, List.Instance.Count);
    AssertEquals('OpenKey with aForceKey, level 1', 'b', List.Instance[0]);
    List.Instance.Clear;
    Conf.EnumSubKeys('/a/b', List.Instance);
    AssertEquals('OpenKey with aForceKey, level 2', 1, List.Instance.Count);
    AssertEquals('OpenKey with aForceKey, level 2', 'c', List.Instance[0]);
    List.Instance.Clear;
    Conf.EnumSubKeys('/a/b/c', List.Instance);
    AssertEquals('OpenKey with aForceKey, level 3', 1, List.Instance.Count);
    AssertEquals('OpenKey with aForceKey, level 3', 'd', List.Instance[0]);
  finally
    ClearConf(Conf, True);
  end;
end;

procedure TTestJsonConf.TestStrings;
var
  Conf: TJsonConf;
  List: specialize TGAutoRef<TStringList>;
begin
  Conf := CreateConf('test.json');
  try
    Conf.GetValue('list', List.Instance, '');
    AssertStrings('Clear, no default.', List.Instance, []);
    Conf.GetValue('list', List.Instance, 'text');
    AssertStrings('Use default.', List.Instance, ['text']);
    List.Instance.Clear;
    List.Instance.Add('abc');
    List.Instance.Add('def');
    Conf.SetValue('a', List.Instance);
    Conf.GetValue('a', List.Instance, '');
    AssertStrings('List', List.Instance, ['abc','def']);
    List.Instance.Clear;
    List.Instance.Add('abc=1');
    List.Instance.Add('def=2');
    Conf.SetValue('a', List.Instance, True);
    Conf.GetValue('a', List.Instance, '');
    AssertStrings('List', List.Instance, ['abc=1', 'def=2']);
    Conf.SetValue('a', 'abc');
    Conf.GetValue('a', List.Instance, '');
    AssertStrings('String', List.Instance, ['abc']);
    Conf.SetValue('a', Integer(1));
    Conf.GetValue('a', List.Instance, '');
    AssertStrings('Integer', List.Instance, ['1']);
    Conf.SetValue('a', True);
    Conf.GetValue('a', List.Instance, '');
    AssertStrings('Boolean', List.Instance, ['true']);
    Conf.SetValue('a', Int64(1));
    Conf.GetValue('a', List.Instance, '');
    AssertStrings('Int64', List.Instance, ['1']);
  finally
    ClearConf(Conf, True);
  end;
end;

procedure TTestJsonConf.TestVarArray;
var
  Conf: TJsonConf;
  a: TJVarArray;
begin
  Conf := CreateConf('test.json');
  try
    a := Conf.GetValue('/a', [1]);
    AssertEquals('GetValue, default length', 1, Length(a));
    AssertEquals('GetValue, default value', 1, Int64(a[0]));
    Conf.SetValue('/a', [42, False, 'item']);
    a := Conf.GetValue('/a', []);
    AssertEquals('GetValue, length', 3, Length(a));
    AssertEquals('GetValue, item[0], Kind', Integer(vkNumber), Integer(a[0].Kind));
    AssertEquals('GetValue, item[0], Value', 42, Int64(a[0]));
    AssertEquals('GetValue, item[1], Kind', Integer(vkBool), Integer(a[1].Kind));
    AssertEquals('GetValue, item[1], Value', False, Boolean(a[1]));
    AssertEquals('GetValue, item[2], Kind', Integer(vkString), Integer(a[2].Kind));
    AssertEquals('GetValue, item[2], Value', 'item', string(a[2]));
    Conf.SetValue('/a', TJVarArray(nil));
    a := Conf.GetValue('/a', [1]);
    AssertEquals('GetValue after set empty, length', 0, Length(a));
  finally
    ClearConf(Conf, True);
  end;
end;

procedure TTestJsonConf.TestPairArray;
var
  Conf: TJsonConf;
  I: Integer;
  I64: Int64;
begin
  Conf := CreateConf('test.json');
  try
    Conf.SetValue('/a', 1);
    Conf.SetValue('/b', [JPair('c', MaxInt), JPair('d', -9007199254740991)]);
    I := Conf.GetValue('/a', Integer(0));
    AssertEquals('GetValue a', 1, I);
    I := Conf.GetValue('/b', Integer(-1));
    AssertEquals('GetValue b', -1, I);
    I := Conf.GetValue('/b/c', Integer(0));
    AssertEquals('GetValue b/c', MaxInt, I);
    I64 := Conf.GetValue('/b/d', Int64(0));
    AssertEquals('GetValue b/c', -9007199254740991, I64);
  finally
    ClearConf(Conf, True);
  end;
end;

procedure TTestJsonConf.TestDuplicates;
var
  Conf: TJsonConf;
  List: specialize TGAutoRef<TStringList>;
  I: Integer;
  s: string;
  Raised: Boolean = False;
begin
  Conf := CreateConf('test.json');
  try
    Conf.SetValue('/a', [JPair('b', 42), JPair('c', 1001)]);
    List.Instance.Add('b=-1');
    List.Instance.Add('c=42');
    try
      Conf.SetValue('a', List.Instance, True);
    except
      on e: EJsonConfError do
        Raised := True;
    end;
    AssertEquals('SetValue(List), exception on duplicates', True, Raised);
    I := Conf.GetValue('a/b', Integer(0));
    AssertEquals('GetValue a/b after exception', 42, I);
    I := Conf.GetValue('a/c', Integer(0));
    AssertEquals('GetValue a/b after exception', 1001, I);

    Conf.Options := [coOverriteDuplicates];
    Raised := False;
    try
      Conf.SetValue('a', List.Instance, True);
    except
      on e: EJsonConfError do
        Raised := True;
    end;
    AssertEquals('SetValue(List), exception on duplicates with option', False, Raised);
    List.Clear;
    s := Conf.GetValue('a/b', '');
    AssertEquals('GetValue a/b after overrite', '-1', s);
    s := Conf.GetValue('a/c', '');
    AssertEquals('GetValue a/b after overrite', '42', s);

    Conf.Options := [];
    try
      Conf.SetValue('a', [JPair('b', 42), JPair('c', 1001)]);
    except
      on e: EJsonConfError do
        Raised := True;
    end;
    AssertEquals('SetValue(Array), exception on duplicates', True, Raised);
    s := Conf.GetValue('a/b', '');
    AssertEquals('GetValue a/b after exception', '-1', s);
    s := Conf.GetValue('a/c', '');
    AssertEquals('GetValue a/b after exception', '42', s);

    Conf.Options := [coOverriteDuplicates];
    Raised := False;
    try
      Conf.SetValue('a', [JPair('b', 42), JPair('c', 1001)]);
    except
      on e: EJsonConfError do
        Raised := True;
    end;
    AssertEquals('SetValue(Array), exception on duplicates with option', False, Raised);
    I := Conf.GetValue('a/b', Integer(0));
    AssertEquals('GetValue a/b after overrite', 42, I);
    I := Conf.GetValue('a/c', Integer(0));
    AssertEquals('GetValue a/b after overrite', 1001, I);
  finally
    ClearConf(Conf, True);
  end;
end;

initialization

  RegisterTest(TTestJsonConf);

end.

