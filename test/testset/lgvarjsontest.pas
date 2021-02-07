unit LGVarJsonTest;

{$mode objfpc}{$H+}
{$MODESWITCH TYPEHELPERS}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Variants,
  lgUtils,
  lgJson,
  lgVarJson;

type

  { TTestVarJson }

  TTestVarJson = class(TTestCase)
  published
    procedure IsVarJson;
    procedure TestCreate;
    procedure Kind;
    procedure Count;
    procedure IsScalar;
    procedure IsStruct;
    procedure IsInteger;
    procedure HasUniqueNames;
    procedure Clone;
    procedure TestToString;
    procedure ToValue;
    procedure AsJson;
    procedure GetItem;
    procedure GetValue;
    procedure GetProperty;
    procedure Cast;
    procedure CastTo;
    procedure SetItem;
    procedure SetValue;
  end;

const
  TestJson =
    '[{"userid":42,"name":"John","age":30,"online":false,"groups":["talk","humor","cook"],' +
    '"spouse":null},' +
    '{"userid":1001,"name":"Thomas","age":42,"online":true,"groups":["talk","games","math","art"],' +
    '"spouse":"Mary"}]';
  TestJson0 =
    '{"userid":42,"name":"John","age":30,"online":false,"groups":["talk","humor","cook"],"spouse":null}';
  TestJson1 =
    '{"userid":1001,"name":"Thomas","age":42,"online":true,"groups":["talk","games","math","art"],"spouse":"Mary"}';

implementation

{ TTestVarJson }

procedure TTestVarJson.IsVarJson;
var
  Json: Variant;
begin
  Json := 42;
  AssertFalse(TVarJson(Json).IsVarJson);
  Json := VarJsonCreate;
  AssertTrue(TVarJson(Json).IsVarJson);
end;

procedure TTestVarJson.TestCreate;
var
  Json: Variant;
begin
  Json := VarJsonCreate(Variant('bar'));
  AssertTrue(TVarJson(Json).IsVarJson);
  Json := VarJsonCreate(TestJson);
  AssertTrue(TVarJson(Json).IsVarJson);
end;

procedure TTestVarJson.Kind;
var
  Json, v: Variant;
begin
  Json := VarJsonCreate;
  v := Json.Kind;
  AssertTrue(v.IsJsValueKind);
  AssertTrue(v.AsJsValueKind = jvkUnknown);
  Json := VarJsonCreate(Variant('bar'));
  v := Json.Kind;
  AssertTrue(v.AsJsValueKind = jvkString);
  Json := VarJsonCreate(TestJson);
  v := Json.Kind;
  AssertTrue(v.IsJsValueKind);
  AssertTrue(v.AsJsValueKind = jvkArray);
end;

procedure TTestVarJson.Count;
var
  Json: Variant;
begin
  Json := VarJsonCreate(Variant('baz'));
  AssertTrue(Json.Count = 1);
  Json := VarJsonCreate(TestJson);
  AssertTrue(Json.Count = 2);
end;

procedure TTestVarJson.IsScalar;
var
  Json: Variant;
begin
  Json := VarJsonCreate(Variant('baz'));
  AssertTrue(Json.IsScalar);
  Json := VarJsonCreate(TestJson);
  AssertFalse(Json.IsScalar);
end;

procedure TTestVarJson.IsStruct;
var
  Json: Variant;
begin
  Json := VarJsonCreate(Variant('baz'));
  AssertFalse(Json.IsStruct);
  Json := VarJsonCreate(TestJson);
  AssertTrue(Json.IsStruct);
end;

procedure TTestVarJson.IsInteger;
var
  Json: Variant;
begin
  Json := VarJsonCreate;
  AssertFalse(Json.IsInteger);
  Json := VarJsonCreate(Variant(42));
  AssertTrue(Json.IsInteger);
end;

procedure TTestVarJson.HasUniqueNames;
var
  Json: Variant;
begin
  Json := VarJsonCreate('{"key":"value","key":"value1"}');
  AssertFalse(Json.HasUniqueNames);
  Json := VarJsonCreate(TestJson);
  AssertTrue(Json.HasUniqueNames);
end;

procedure TTestVarJson.Clone;
var
  Json, v, k: Variant;
begin
  Json := VarJsonCreate;
  k := Json.Kind;
  AssertTrue(k.AsJsValueKind = jvkUnknown);
  v := Json.Clone;
  k := v.Kind;
  AssertTrue(k.AsJsValueKind = jvkUnknown);
  Json := Null;
  k := v.Kind;
  AssertTrue(k.AsJsValueKind = jvkUnknown);
  Json := VarJsonCreate(TestJson);
  v := Json.Clone;
  k := Json.Kind;
  AssertTrue(k.AsJsValueKind = jvkArray);
  k := v.Kind;
  AssertTrue(k.AsJsValueKind = jvkArray);
  v := Null;
  k := Json.Kind;
  AssertTrue(k.AsJsValueKind = jvkArray);
end;

procedure TTestVarJson.TestToString;
var
  Json: Variant;
  s: string;
const
  c = '[42, "baz", false]';
begin
  Json := VarJsonCreate;
  s := Json.ToString;
  AssertTrue(s = '');
  Json := VarJsonCreate(Variant('bar'));
  s := Json.ToString;
  AssertTrue(s = 'bar');
  Json := VarJsonCreate(c);
  s := Json.ToString;
  AssertTrue(s = c);
end;

procedure TTestVarJson.ToValue;
var
  Json, v: Variant;
begin
  Json := VarJsonCreate;
  v := Json.ToValue;
  AssertTrue(VarIsNull(v));
  Json := VarJsonCreate(Variant(42));
  v := Json.ToValue;
  AssertTrue(v = 42);
end;

procedure TTestVarJson.AsJson;
var
  Json, v: Variant;
begin
  Json := VarJsonCreate;
  v := Json.AsJson;
  AssertTrue(v = '');
  Json := VarJsonCreate(TestJson);
  v := Json.AsJson;
  AssertTrue(v = TestJson);
end;

procedure TTestVarJson.GetItem;
var
  Json, v, k: Variant;
begin
  Json := VarJsonCreate(TestJson);
  v := Json.GetItem(0);
  AssertTrue(v.IsJson);
  AssertTrue(v.IsStruct);
  AssertTrue(v.Count = 6);
  k := v.Kind;
  AssertTrue(k.AsJsValueKind = jvkObject);
  v := Json.GetItem(1);
  AssertTrue(v.IsJson);
  AssertTrue(v.IsStruct);
  AssertTrue(v.Count = 6);
  k := v.Kind;
  AssertTrue(k.AsJsValueKind = jvkObject);
end;

procedure TTestVarJson.GetValue;
var
  Json, v, val: Variant;
  I: Integer;
begin
  Json := VarJsonCreate(TestJson);
  v := Json.GetItem(0);
  val := v.GetValue('userid');
  AssertTrue(val.IsJson);
  AssertTrue(val.IsScalar);
  AssertTrue(val.IsInteger);
  val := val.ToValue;
  I := val;
  AssertTrue(I = 42);
end;

procedure TTestVarJson.GetProperty;
var
  Json, v: Variant;
begin
  Json := VarJsonCreate(TestJson);
  v := Json.GetItem(0).userid.ToValue;
  AssertTrue(v = 42);
  v := Json.GetItem(1).groups;
  AssertTrue(v.IsJson);
  AssertTrue(v.IsStruct);
  AssertTrue(v.Count = 4);
  v := Json.GetItem(1).groups.ToValue;
  AssertTrue(v = '["talk","games","math","art"]');
end;

procedure TTestVarJson.Cast;
var
  Json: Variant;
  s: string = '';
  I: Integer;
begin
  Json := VarAsType(Integer(42), VarJson);
  AssertTrue(TVarJson(Json).IsVarJson);
  AssertTrue(Json.IsInteger);
  s := Json.ToValue;
  AssertTrue(s = '42');
  I := Json.ToValue;
  AssertTrue(I = 42);
end;

procedure TTestVarJson.CastTo;
var
  Json, v: Variant;
begin
  Json := VarJsonCreate(TestJson);
  v := Json.GetItem(1).userid;
  AssertTrue(v.IsJson);
  AssertTrue(v.IsInteger);
  v := VarAsType(v, varInteger);
  AssertTrue(v = 1001);
end;

procedure TTestVarJson.SetItem;
var
  Json, v, k: Variant;
begin
  Json := VarJsonCreate;
  AssertTrue(Json.Count = 0);
  Json.SetItem(0, 'baz');
  k := Json.Kind;
  AssertTrue(k.AsJsValueKind = jvkArray);
  AssertTrue(Json.Count = 1);
  v := Json.GetItem(0);
  AssertTrue(v.IsJson);
  AssertTrue(v.IsScalar);
  k := v.Kind;
  AssertTrue(k.AsJsValueKind = jvkString);
  AssertTrue(v.ToString = 'baz');
  Json := VarJsonCreate(TestJson);
  v := Json.SetItem(0, VarArrayOf([42, 1001]));
  AssertTrue(v.Count = 2);
  k := v.GetItem(0).Kind;
  AssertTrue(k.AsJsValueKind = jvkArray);
  AssertTrue(v.GetItem(0).GetItem(1).ToString = '1001');
end;

procedure TTestVarJson.SetValue;
var
  Json, v, k: Variant;
begin
  Json := VarJsonCreate;
  AssertTrue(Json.Count = 0);
  Json.SetValue('bar', 42);
  AssertTrue(Json.Count = 1);
  k := Json.Kind;
  AssertTrue(k.AsJsValueKind = jvkObject);
  v := Json.GetValue('bar');
  AssertTrue(v.IsJson);
  AssertTrue(v.IsScalar);
  AssertTrue(v.IsInteger);
  AssertTrue(v.ToValue = 42);
  Json := VarJsonCreate(TestJson);
  v := Json.GetItem(0).SetValue('name', 'Andrew');
  AssertTrue(v.Count = 6);
  AssertTrue(Json.GetItem(0).GetValue('name').ToValue = 'Andrew');
end;


initialization

  RegisterTest(TTestVarJson);

end.

