unit JtdCodegenTest;

{$mode objfpc}{$h+}{$warn 5089 off}

interface

uses
  SysUtils, fpcunit, testregistry,
  lgUtils, lgJson, lgJtdTypes,
{

All of the following units are automatically generated from test schemas obtained from
  https://github.com/jsontypedef/json-typedef-codegen/tree/master/crates/test/schemas/roundtrip_strict

}
  basic_discriminator, basic_enum, basic_properties, custom_overrides, definition_name_collisions,
  description, elements, empty_and_nonascii_definitions, empty_and_nonascii_enum_values,
  empty_and_nonascii_properties, enum_collisions, enum_variant_collisions, initialisms,
  keywords, nullable_discriminator, nullable_elements, nullable_enum, nullable_properties,
  nullable_references, property_name_collisions, reference, root_boolean, root_int16, root_int32,
  root_int8, root_nullable_string, root_string, root_uint16, root_uint32, root_uint8, type_collisions,
  values;

type

  { TTestRoundTreap }

  TTestRoundTreap = class(TTestCase)
  published
    procedure TestBasicDiscriminator;
    procedure TestBasicEnum;
    procedure TestBasicProperties;
    procedure TestCustomOverrides;
    procedure TestDefinitionNameCollisions;
    procedure TestDescription;
    procedure TestElements;
    procedure TestEmptyAndNonasciiDefinitions;
    procedure TestEmptyAndNonasciiEnumValues;
    procedure TestEmptyAndNonasciiProperties;
  end;

implementation

procedure TTestRoundTreap.TestBasicDiscriminator;
var
  Src, Dst: specialize TGAutoRef<TJsonNode>;
  bd: specialize TGAutoRef<TBasicDiscriminator>;
  s, r: string;
const
  TestValues: TStringArray = (
    '{"foo":"QUUX","quuz":"B"}',
    '{"baz":"/]3\"HR","foo":"BAR_BAZ"}',
    '{"baz":" jR WR)","foo":"BAR_BAZ"}',
    '{"foo":"QUUX","quuz":";?0"}',
    '{"baz":"fy","foo":"BAR_BAZ"}',
    '{"foo":"QUUX","quuz":"@)"}',
    '{"baz":"^%","foo":"BAR_BAZ"}',
    '{"baz":"s","foo":"BAR_BAZ"}',
    '{"foo":"QUUX","quuz":"+(.K+"}',
    '{"baz":"%g","foo":"BAR_BAZ"}'
  );
begin
  for s in  TestValues do
    begin
      AssertTrue(Src.Instance.Parse(s));
      {%H-}bd.Instance := TBasicDiscriminator.ReadJson(Src.Instance) as TBasicDiscriminator;
      r := bd.Instance.AsJson;
      AssertTrue(Dst.Instance.Parse(r));
      AssertTrue(TJsonNode.Equal(Dst.Instance, Src.Instance));
    end;
end;

procedure TTestRoundTreap.TestBasicEnum;
var
  Src, Dst: specialize TGAutoRef<TJsonNode>;
  be: specialize TGAutoRef<TBasicEnumElem>;
  s, r: string;
const
  TestValues: TStringArray = (
    '"Baz"',
    '"Foo"',
    '"Foo"',
    '"Baz"',
    '"Foo"',
    '"Bar"',
    '"Baz"',
    '"Foo"',
    '"Foo"',
    '"Bar"'
  );
begin
  for s in  TestValues do
    begin
      AssertTrue(Src.Instance.Parse(s));
      {%H-}be.Instance := TBasicEnumElem.ReadJson(Src.Instance) as TBasicEnumElem;
      r := be.Instance.AsJson;
      AssertTrue(Dst.Instance.Parse(r));
      AssertTrue(TJsonNode.Equal(Dst.Instance, Src.Instance));
    end;
end;

procedure TTestRoundTreap.TestBasicProperties;
var
  Src, Dst: specialize TGAutoRef<TJsonNode>;
  bp: specialize TGAutoRef<TBasicProperties>;
  s, r: string;
const
  TestValues: TStringArray = (
    '{"bar":"b/kul","baz":[false],"foo":true,"quux":[true,true,true,true,true,true,true]}',
    '{"bar":"|/]Tbf ","baz":[false,true,true,false,false],"foo":false,"quux":[false,false,true,false]}',
    '{"bar":"+6<$","baz":[false],"foo":false,"quux":[true,true,false,true,false,false]}',
    '{"bar":"$","baz":[true,true,true,true],"foo":false,"quux":[false]}',
    '{"bar":")","baz":[],"foo":false,"quux":[true]}',
    '{"bar":"ZY","baz":[true,false],"foo":true,"quux":[]}',
    '{"bar":"D9","baz":[true,true,false,true,true,true],"foo":true,"quux":[true,false,true]}',
    '{"bar":"bQs","baz":[false,true,false,false],"foo":true,"quux":[false,false,true,false]}',
    '{"bar":"p5)gqPz","baz":[true,true],"foo":true,"quux":[false,false]}',
    '{"bar":"\\;lzJ8","baz":[true],"foo":false,"quux":[false,false,true,false,false,true,true]}'
  );
begin
  for s in  TestValues do
    begin
      AssertTrue(Src.Instance.Parse(s));
      {%H-}bp.Instance := TBasicProperties.ReadJson(Src.Instance) as TBasicProperties;
      r := bp.Instance.AsJson;
      AssertTrue(Dst.Instance.Parse(r));
      AssertTrue(TJsonNode.Equal(Dst.Instance, Src.Instance));
    end;
end;

procedure TTestRoundTreap.TestCustomOverrides;
var
  Src, Dst: specialize TGAutoRef<TJsonNode>;
  co: specialize TGAutoRef<TCustomOverrides>;
  s, r: string;
const
  TestValues: TStringArray = (
  '{"override_elements_container":["syA%","kp@/KW]","C''=(","@`SW","qc|HM~~","","%c5"],"override_type_discriminator":{"foo":"baz"},"override_type_enum":"BAR","override_type_expr":"H!","override_type_properties":{},"override_values_container":{}}',
  '{"override_elements_container":["Gr@@","R"],"override_type_discriminator":{"foo":"baz"},"override_type_enum":"BAR","override_type_expr":"n","override_type_properties":{},"override_values_container":{")l7j#":"-4%j","/Bu+3=+":"in XUaa","2":"","4H":".","9K":"%=#e1"}}',
  '{"override_elements_container":["^e60?V","A\\","","L",";U\"","DOB&"],"override_type_discriminator":{"foo":"bar"},"override_type_enum":"FOO","override_type_expr":"e(AXcM\"","override_type_properties":{},"override_values_container":{"\"0":"",":]:az#\\":"O","C;`":"}^","O(7(,":"~","\\%L+":"=o`HGl"}}',
  '{"override_elements_container":["1ot","\"%","u]{]","E((g","wc>","-`5","o|<4M"],"override_type_discriminator":{"foo":"baz"},"override_type_enum":"BAR","override_type_expr":"","override_type_properties":{},"override_values_container":{"":"<HT",".":"JQ\"A","OEUh":"JjRl8X","OK,''9RX":"+K%f*","[e.o":"O>~","y}":"y4"}}',
  '{"override_elements_container":["U?zOt!i","S5","|","F\\","#\""],"override_type_discriminator":{"foo":"bar"},"override_type_enum":"FOO","override_type_expr":"Xl9","override_type_properties":{},"override_values_container":{",B:)-b":"",":Vt":"6[&i","DhTq^":"Vo","iK":"R-","p":"DgoG","tz$M{":"Nr","xn:W":"^t"}}',
  '{"override_elements_container":["m9xI(Tx",",5#\"H","gwJ^^","#07","6",">^",""],"override_type_discriminator":{"foo":"baz"},"override_type_enum":"BAR","override_type_expr":"A@","override_type_properties":{},"override_values_container":{}}',
  '{"override_elements_container":["","]DN5-v*","!G."],"override_type_discriminator":{"foo":"bar"},"override_type_enum":"FOO","override_type_expr":"Ml9$d","override_type_properties":{},"override_values_container":{" *":"4W]B","$rmj%iU":"IBpG*w}","+Q7q":"_Q}2y","bi~c:":"n11&-WA"}}',
  '{"override_elements_container":[],"override_type_discriminator":{"foo":"baz"},"override_type_enum":"FOO","override_type_expr":"+7?PKM","override_type_properties":{},"override_values_container":{"":"+","`W;":"","}`R,3":"]QHmei"}}',
  '{"override_elements_container":["\"](3","]*#(ue","&2fKL1T","_","f7jQF","K"],"override_type_discriminator":{"foo":"bar"},"override_type_enum":"FOO","override_type_expr":"1w;","override_type_properties":{},"override_values_container":{"+Rq":"","_$BXb":"O6X ","wWK9":"("}}',
  '{"override_elements_container":["_m","f5&"],"override_type_discriminator":{"foo":"bar"},"override_type_enum":"FOO","override_type_expr":"","override_type_properties":{},"override_values_container":{"6X){":"<u7D0","8":"P.6+O","TDS>*":"(V","[":"2''zM","v\"":"xw"}}'
  );
begin
  for s in  TestValues do
    begin
      AssertTrue(Src.Instance.Parse(s));
      {%H-}co.Instance := TCustomOverrides.ReadJson(Src.Instance) as TCustomOverrides;
      r := co.Instance.AsJson;
      AssertTrue(Dst.Instance.Parse(r));
      AssertTrue(TJsonNode.Equal(Dst.Instance, Src.Instance));
    end;
end;

procedure TTestRoundTreap.TestDefinitionNameCollisions;
var
  Src, Dst: specialize TGAutoRef<TJsonNode>;
  bs: specialize TGAutoRef<TBarString>;
  s, r: string;
const
  TestValues: TStringArray = (
    '"@"',
    '"%-QviO"',
    '"0iio/"',
    '"Gs "',
    '"izr)"',
    '"i>1O"',
    '"BYri,R?"',
    '"P"',
    '"Un@"',
    '"H"'
  );
begin
  for s in  TestValues do
    begin
      AssertTrue(Src.Instance.Parse(s));
      {%H-}bs.Instance := TBarString.ReadJson(Src.Instance) as TBarString;
      r := bs.Instance.AsJson;
      AssertTrue(Dst.Instance.Parse(r));
      AssertTrue(TJsonNode.Equal(Dst.Instance, Src.Instance));
    end;
end;

procedure TTestRoundTreap.TestDescription;
var
  Src, Dst: specialize TGAutoRef<TJsonNode>;
  ds: specialize TGAutoRef<TDescription>;
  s, r: string;
const
  TestValues: TStringArray = (
    '{"discriminator_with_description":{"foo":"bar"},"enum_with_description":"Z","long_description":"c=,qh","properties_with_description":{},"ref_with_description":"|","string_with_description":"~zF2Q"}',
    '{"discriminator_with_description":{"foo":"bar"},"enum_with_description":"Z","long_description":"t%>a","properties_with_description":{},"ref_with_description":"Q1-QHp","string_with_description":""}',
    '{"discriminator_with_description":{"foo":"bar"},"enum_with_description":"X","long_description":"*t","properties_with_description":{},"ref_with_description":"&","string_with_description":""}',
    '{"discriminator_with_description":{"foo":"bar"},"enum_with_description":"Y","long_description":"","properties_with_description":{},"ref_with_description":"li9f","string_with_description":"p&"}',
    '{"discriminator_with_description":{"foo":"bar"},"enum_with_description":"Y","long_description":"|SYL:*}","properties_with_description":{},"ref_with_description":",<","string_with_description":"fT,O&H"}',
    '{"discriminator_with_description":{"foo":"bar"},"enum_with_description":"Z","long_description":"[W>k","properties_with_description":{},"ref_with_description":"R`&eE^ ","string_with_description":"(I#le"}',
    '{"discriminator_with_description":{"foo":"bar"},"enum_with_description":"Z","long_description":" ","properties_with_description":{},"ref_with_description":"a1W=bF","string_with_description":"l\" Dke"}',
    '{"discriminator_with_description":{"foo":"bar"},"enum_with_description":"X","long_description":"?gi","properties_with_description":{},"ref_with_description":"$8w","string_with_description":"S1~1\"y"}',
    '{"discriminator_with_description":{"foo":"bar"},"enum_with_description":"Y","long_description":":`","properties_with_description":{},"ref_with_description":"","string_with_description":"K5m?)N*"}',
    '{"discriminator_with_description":{"foo":"bar"},"enum_with_description":"X","long_description":"r2wPS","properties_with_description":{},"ref_with_description":"GI#[i","string_with_description":"53407 "}'
  );
begin
  for s in  TestValues do
    begin
      AssertTrue(Src.Instance.Parse(s));
      {%H-}ds.Instance := TDescription.ReadJson(Src.Instance) as TDescription;
      r := ds.Instance.AsJson;
      AssertTrue(Dst.Instance.Parse(r));
      AssertTrue(TJsonNode.Equal(Dst.Instance, Src.Instance));
    end;
end;

procedure TTestRoundTreap.TestElements;
var
  Src, Dst: specialize TGAutoRef<TJsonNode>;
  el: specialize TGAutoRef<TElements>;
  s, r: string;
const
  TestValues: TStringArray = (
    '["qTNh"]',
    '["&9","&."]',
    '["Ea"]',
    '["@2/Z","04","(O",")","Aj~Yw"," 3|","-Jw"]',
    '["T","(aa","OpvF"]',
    '[]',
    '["^Mkc?8","tv6V+B(","L}(","s+","S (,","98`j@"]',
    '["","7eVh`"]',
    '["","9\"","N8K*"]',
    '["r"]'
  );
begin
  for s in  TestValues do
    begin
      AssertTrue(Src.Instance.Parse(s));
      {%H-}el.Instance := TElements.ReadJson(Src.Instance) as TElements;
      r := el.Instance.AsJson;
      AssertTrue(Dst.Instance.Parse(r));
      AssertTrue(TJsonNode.Equal(Dst.Instance, Src.Instance));
    end;
end;

procedure TTestRoundTreap.TestEmptyAndNonasciiDefinitions;
var
  Src, Dst: specialize TGAutoRef<TJsonNode>;
  enad: specialize TGAutoRef<TEmptyAndNonasciiDefinitionsString>;
  s, r: string;
const
  TestValues: TStringArray = (
    '"r:93"',
    '"ih.~iuj"',
    '"h^1eEgg"',
    '"bAJ"',
    '";"',
    '"[E"',
    '"h''w"',
    '""',
    '"SC-"',
    '"k9S`!&"'
  );
begin
  for s in  TestValues do
    begin
      AssertTrue(Src.Instance.Parse(s));
      {%H-}enad.Instance := TEmptyAndNonasciiDefinitionsString.ReadJson(Src.Instance) as TEmptyAndNonasciiDefinitionsString;
      r := enad.Instance.AsJson;
      AssertTrue(Dst.Instance.Parse(r));
      AssertTrue(TJsonNode.Equal(Dst.Instance, Src.Instance));
    end;
end;

procedure TTestRoundTreap.TestEmptyAndNonasciiEnumValues;
var
  Src, Dst: specialize TGAutoRef<TJsonNode>;
  enev: specialize TGAutoRef<TEmptyAndNonasciiEnumValues>;
  s, r: string;
const
  TestValues: TStringArray = (
    '"foo bar"',
    '""',
    '"$foo"',
    '"foo0bar"',
    '"foo\nbar"',
    '"$foo"',
    '"_foo"',
    '"foo bar"',
    '""',
    '"0foo"'
  );
begin
  for s in  TestValues do
    begin
      AssertTrue(Src.Instance.Parse(s));
      {%H-}enev.Instance := TEmptyAndNonasciiEnumValues.ReadJson(Src.Instance) as TEmptyAndNonasciiEnumValues;
      r := enev.Instance.AsJson;
      AssertTrue(Dst.Instance.Parse(r));
      AssertTrue(TJsonNode.Equal(Dst.Instance, Src.Instance));
    end;
end;

procedure TTestRoundTreap.TestEmptyAndNonasciiProperties;
var
  Src, Dst: specialize TGAutoRef<TJsonNode>;
  enp: specialize TGAutoRef<TEmptyAndNonasciiProperties>;
  s, r: string;
const
  TestValues: TStringArray = (
    '{"":">*2Vz]y","$foo":"!C8)","0foo":"_6","_foo":"m>o{X.:","foo\nbar":"+","foo bar":"##<c","foo0bar":"uO`","foo﷽bar":"1r9<i$b"}',
    '{"":"9zq2g","$foo":"TfESg9","0foo":"","_foo":":''\"c&","foo\nbar":"","foo bar":"TC1uvw","foo0bar":"F_N","foo﷽bar":"R4"}',
    '{"":"[fD|l}","$foo":"\\Op0","0foo":"","_foo":"%+","foo\nbar":"YnI](","foo bar":"RZlcKj","foo0bar":"\\","foo﷽bar":"0A"}',
    '{"":"x/dC","$foo":"EM9","0foo":"Rjc:M6","_foo":"sXG6mr","foo\nbar":"Qw+ieiZ","foo bar":"#CGtf4b","foo0bar":"nK","foo﷽bar":""}',
    '{"":"t/Z","$foo":"pOHQW! ","0foo":"_/_R","_foo":"g3y","foo\nbar":"X","foo bar":"U","foo0bar":" jRO","foo﷽bar":"L"}',
    '{"":"33(","$foo":"C)S ","0foo":"A_=Uq.i","_foo":"!","foo\nbar":"j+)\"w","foo bar":"ucYMmJ","foo0bar":"Efe\\\\H","foo﷽bar":"8w"}',
    '{"":"dE=wmJh","$foo":"?7","0foo":"T","_foo":"","foo\nbar":"<RQD4m.","foo bar":"1KWQjF","foo0bar":"2G!E_","foo﷽bar":"@Bb"}',
    '{"":"~(,`","$foo":"t^waIy5","0foo":"me?hkQ^","_foo":"oR3W]O#","foo\nbar":"cLx\"l@J","foo bar":"HtxS) ","foo0bar":"q)XGH","foo﷽bar":"!^x1"}',
    '{"":"X''5\"cgC","$foo":"HyN>H","0foo":"0~$pD}","_foo":"j6","foo\nbar":"[c","foo bar":"c7kB=$","foo0bar":"tfz","foo﷽bar":"1gI"}',
    '{"":"","$foo":"a","0foo":"V","_foo":"ZH7","foo\nbar":"","foo bar":"","foo0bar":"R","foo﷽bar":"/ml\\om["}'
  );
begin
  for s in  TestValues do
    begin
      AssertTrue(Src.Instance.Parse(s));
      {%H-}enp.Instance := TEmptyAndNonasciiProperties.ReadJson(Src.Instance) as TEmptyAndNonasciiProperties;
      r := enp.Instance.AsJson;
      AssertTrue(Dst.Instance.Parse(r));
      AssertTrue(TJsonNode.Equal(Dst.Instance, Src.Instance));
    end;
end;



initialization

  RegisterTest(TTestRoundTreap);

end.

