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
  values, optional_properties, nullable_optional_properties;

type

  { TTestRoundTreap }

  TTestRoundTreap = class(TTestCase)
  private
    procedure RunTestSet(const aValues: array of string; aTestClass: TJtdEntityClass);
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
    procedure TestEnumCollisions;
    procedure TestEnumVariantCollisions;
    procedure TestInitialisms;
    procedure TestKeywords;
    procedure TestNullableDiscriminator;
    procedure TestNullableElements;
    procedure TestNullableEnum;
    procedure TestNullableOptionalProperties;
    procedure TestNullableProperties;
    procedure TestNullableReferences;
    procedure TestOptionalProperties;
    procedure TestPropertyNameCollisions;
    procedure TestReference;
    procedure TestRootBoolean;
    procedure TestRootInt16;
    procedure TestRootInt32;
    procedure TestRootInt8;
    procedure TestRootNullableString;
    procedure TestRootString;
    procedure TestRootUint16;
    procedure TestRootUint32;
    procedure TestRootUint8;
    procedure TestTypeCollisions;
    procedure TestValuesUnit;
  end;

implementation

procedure TTestRoundTreap.RunTestSet(const aValues: array of string; aTestClass: TJtdEntityClass);
var
  Src, Dst: specialize TGAutoRef<TJsonNode>;
  Ref: specialize TGAutoRef<TJtdEntity>;
  s, d: string;
begin
  for s in  aValues do
    begin
      AssertTrue(Src.Instance.TryParse(s));
      {%H-}Ref.Instance := aTestClass.TryLoad(Src.Instance) as aTestClass;
      AssertTrue(Ref.Instance <> nil);
      d := Ref.Instance.AsJson;
      AssertTrue(Dst.Instance.TryParse(d));
      AssertTrue(TJsonNode.Equal(Dst.Instance, Src.Instance));

      Ref.Instance.Load(s);
      d := Ref.Instance.AsJson;
      AssertTrue(Dst.Instance.TryParse(d));
      AssertTrue(TJsonNode.Equal(Dst.Instance, Src.Instance));
    end;
end;

procedure TTestRoundTreap.TestBasicDiscriminator;
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
  RunTestSet(TestValues, basic_discriminator.TRootObject);
end;

procedure TTestRoundTreap.TestBasicEnum;
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
  RunTestSet(TestValues, basic_enum.TRootObject);
end;

procedure TTestRoundTreap.TestBasicProperties;
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
  RunTestSet(TestValues, basic_properties.TRootObject);
end;

procedure TTestRoundTreap.TestCustomOverrides;
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
  RunTestSet(TestValues, custom_overrides.TRootObject);
end;

procedure TTestRoundTreap.TestDefinitionNameCollisions;
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
  RunTestSet(TestValues, definition_name_collisions.TRootObject);
end;

procedure TTestRoundTreap.TestDescription;
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
  RunTestSet(TestValues, description.TRootObject);
end;

procedure TTestRoundTreap.TestElements;
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
  RunTestSet(TestValues, elements.TRootObject);
end;

procedure TTestRoundTreap.TestEmptyAndNonasciiDefinitions;
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
  RunTestSet(TestValues, empty_and_nonascii_definitions.TRootObject);
end;

procedure TTestRoundTreap.TestEmptyAndNonasciiEnumValues;
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
  RunTestSet(TestValues, empty_and_nonascii_enum_values.TRootObject);
end;

procedure TTestRoundTreap.TestEmptyAndNonasciiProperties;
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
  RunTestSet(TestValues, empty_and_nonascii_properties.TRootObject);
end;

procedure TTestRoundTreap.TestEnumCollisions;
const
  TestValues: TStringArray = (
    '{"foo":{"bar":"y"},"foo_bar":"x"}',
    '{"foo":{"bar":"y"},"foo_bar":"x"}',
    '{"foo":{"bar":"x"},"foo_bar":"y"}',
    '{"foo":{"bar":"x"},"foo_bar":"x"}',
    '{"foo":{"bar":"x"},"foo_bar":"x"}',
    '{"foo":{"bar":"x"},"foo_bar":"x"}',
    '{"foo":{"bar":"x"},"foo_bar":"y"}',
    '{"foo":{"bar":"y"},"foo_bar":"x"}',
    '{"foo":{"bar":"y"},"foo_bar":"x"}',
    '{"foo":{"bar":"y"},"foo_bar":"y"}'
  );
begin
  RunTestSet(TestValues, enum_collisions.TRootObject);
end;

procedure TTestRoundTreap.TestEnumVariantCollisions;
const
  TestValues: TStringArray = (
    '"foo"',
    '"Foo"',
    '"foo"',
    '"FOO"',
    '"Foo"',
    '"Foo"',
    '"Foo"',
    '"Foo"',
    '"Foo"',
    '"foo"'
  );
begin
  RunTestSet(TestValues, enum_variant_collisions.TRootObject);
end;

procedure TTestRoundTreap.TestInitialisms;
const
  TestValues: TStringArray = (
    '{"http":"","id":">$ldB","nested_id_initialism":{"json":"K -\\7y","normalword":"?["},"utf8":"+rn_C","word_with_embedded_id_initialism":"\\:3$/E","word_with_trailing_initialism_id":"W,."}',
    '{"http":"%j","id":"SiG$","nested_id_initialism":{"json":"og","normalword":"uU~<xx"},"utf8":"d7@|","word_with_embedded_id_initialism":"N''Q\\","word_with_trailing_initialism_id":""}',
    '{"http":"y5","id":"<msHh~j","nested_id_initialism":{"json":"cl<rq:","normalword":"6-M:DD"},"utf8":"f","word_with_embedded_id_initialism":"0iV","word_with_trailing_initialism_id":",d%!"}',
    '{"http":"?d:1","id":"","nested_id_initialism":{"json":"","normalword":">Z6"},"utf8":"B`Q[W","word_with_embedded_id_initialism":"Z","word_with_trailing_initialism_id":"bixnl"}',
    '{"http":"b","id":"","nested_id_initialism":{"json":"","normalword":"C,`vg(X"},"utf8":"No u","word_with_embedded_id_initialism":"","word_with_trailing_initialism_id":"pV30!R"}',
    '{"http":"","id":"5Ra`k~","nested_id_initialism":{"json":"5OyeL+","normalword":"Vsq"},"utf8":"#`Q*#3","word_with_embedded_id_initialism":"@y","word_with_trailing_initialism_id":"7?enW&"}',
    '{"http":",11I&;","id":"]We0OR","nested_id_initialism":{"json":"y{E","normalword":"h]m@` ."},"utf8":"b|U)g\\S","word_with_embedded_id_initialism":"m>{_Pb","word_with_trailing_initialism_id":"y"}',
    '{"http":"J`}2hbY","id":"W\"c&@Yj","nested_id_initialism":{"json":"","normalword":"#"},"utf8":"?aC2p","word_with_embedded_id_initialism":"!TB","word_with_trailing_initialism_id":"39L($~"}',
    '{"http":"L","id":"@b]T\"_","nested_id_initialism":{"json":"||","normalword":"4hkWRLb"},"utf8":"9","word_with_embedded_id_initialism":"Q#","word_with_trailing_initialism_id":":9{"}',
    '{"http":"xw","id":"&T5","nested_id_initialism":{"json":"7zIT%","normalword":"vA"},"utf8":"sx","word_with_embedded_id_initialism":"d","word_with_trailing_initialism_id":"P64"}'
  );
begin
  RunTestSet(TestValues, initialisms.TRootObject);
end;

procedure TTestRoundTreap.TestKeywords;
const
  TestValues: TStringArray = (
    '{"for":";''(","object":"D"}',
    '{"for":"s)nZ*=","object":"n;{j"}',
    '{"for":":@?","object":"rQxAc"}',
    '{"for":"~|jr2>P","object":"pX/"}',
    '{"for":"s","object":"NQe{V%S"}',
    '{"for":"","object":"<UJ94\""}',
    '{"for":"","object":"]H,"}',
    '{"for":"BQ''1(","object":""}',
    '{"for":"WL&p<","object":"["}',
    '{"for":"2","object":"ZhDx"}'
  );
begin
  RunTestSet(TestValues, keywords.TRootObject);
end;

procedure TTestRoundTreap.TestNullableDiscriminator;
const
  TestValues: TStringArray = (
    '{"foo":"quux","quuz":"."}',
    '{"foo":"quux","quuz":"t*"}',
    'null',
    'null',
    'null',
    'null',
    '{"baz":"($Dv","foo":"bar"}',
    '{"baz":"Im{W","foo":"bar"}',
    'null',
    '{"baz":"`\\U","foo":"bar"}'
  );
begin
  RunTestSet(TestValues, nullable_discriminator.TRootObject);
end;

procedure TTestRoundTreap.TestNullableElements;
const
  TestValues: TStringArray = (
    '["-MRk","-","H\\","Z"]',
    '["","N"]',
    'null',
    '["a","b","c"]',
    '["","","",""]',
    'null',
    'null',
    '["x"]',
    'null',
    '["HU","ym4","Pi7Y",""]'
  );
begin
  RunTestSet(TestValues, nullable_elements.TRootObject);
end;

procedure TTestRoundTreap.TestNullableEnum;
const
  TestValues: TStringArray = (
    '"Foo"',
    '"Baz"',
    'null',
    'null',
    '"Bar"',
    '"Foo"',
    '"Foo"',
    'null',
    '"Foo"',
    'null'
  );
begin
  RunTestSet(TestValues, nullable_enum.TRootObject);
end;

procedure TTestRoundTreap.TestNullableOptionalProperties;
const
  TestValues: TStringArray = (
    '{"baz":null}',
    '{"baz":null,"foo":null}',
    '{"baz":null,"foo":")L2"}',
    '{}',
    '{"foo":null}',
    '{"bar":["<u.>",null,"}"],"foo":"O"}',
    '{"bar":[null,null,"g&[w",null,"6p2L",null,"m6"],"foo":null}',
    '{"bar":["","# p0",null,"{<j"],"baz":false}',
    '{"bar":null}',
    '{"foo":null,"bar":null}'
  );
begin
  RunTestSet(TestValues, nullable_optional_properties.TRootObject);
end;

procedure TTestRoundTreap.TestNullableProperties;
const
  TestValues: TStringArray = (
    '{"bar":"6","baz":[true,true,false,false,false],"foo":true,"quux":[true,true,true,true,false,false]}',
    'null',
    '{"bar":"@zqldz","baz":[false,false],"foo":true,"quux":[true,true,true,true,false,false,true]}',
    'null',
    '{"bar":"-","baz":[],"foo":true,"quux":[]}',
    'null',
    '{"bar":"?","baz":[true],"foo":true,"quux":[false]}',
    'null',
    '{"bar":"@Hxz=","baz":[true,true,false,false,false],"foo":false,"quux":[true,false,false,false,false,false,true]}',
    'null'
  );
begin
  RunTestSet(TestValues, nullable_properties.TRootObject);
end;

procedure TTestRoundTreap.TestNullableReferences;
const
  TestValues: TStringArray = (
  '{"notnull_ref_notnull_string":"z(q","notnull_ref_null_string":"Z^qLU","notnull_string":"MqRm)_","null_ref_notnull_string":"APQ,","null_ref_null_string":null,"null_string":null}',
  '{"notnull_ref_notnull_string":"=","notnull_ref_null_string":"7HWZQ","notnull_string":"?SVoua","null_ref_notnull_string":null,"null_ref_null_string":null,"null_string":". Jn"}',
  '{"notnull_ref_notnull_string":"","notnull_ref_null_string":null,"notnull_string":"\"NV}#ku","null_ref_notnull_string":"#H<","null_ref_null_string":null,"null_string":null}',
  '{"notnull_ref_notnull_string":"","notnull_ref_null_string":null,"notnull_string":"n=u>#","null_ref_notnull_string":null,"null_ref_null_string":".GMSG","null_string":null}',
  '{"notnull_ref_notnull_string":"3U cPR","notnull_ref_null_string":"zA?)","notnull_string":"p?9h}","null_ref_notnull_string":null,"null_ref_null_string":"]UM8]","null_string":"^yrXL?/"}',
  '{"notnull_ref_notnull_string":"`]`\\?_|","notnull_ref_null_string":null,"notnull_string":"<{Xq;fr","null_ref_notnull_string":null,"null_ref_null_string":null,"null_string":null}',
  '{"notnull_ref_notnull_string":"MeKUh","notnull_ref_null_string":"0J^Xzs","notnull_string":"?ctlzL","null_ref_notnull_string":null,"null_ref_null_string":null,"null_string":null}',
  '{"notnull_ref_notnull_string":"XP<zr","notnull_ref_null_string":"$%]","notnull_string":"=D=aOU","null_ref_notnull_string":null,"null_ref_null_string":null,"null_string":""}',
  '{"notnull_ref_notnull_string":"9+-Rz2","notnull_ref_null_string":"N7s)Eg?","notnull_string":"$+q2B","null_ref_notnull_string":null,"null_ref_null_string":"wg]T","null_string":"l;Uc"}',
  '{"notnull_ref_notnull_string":"u","notnull_ref_null_string":"","notnull_string":"O","null_ref_notnull_string":null,"null_ref_null_string":null,"null_string":"9"}'
  );
begin
  RunTestSet(TestValues, nullable_references.TRootObject);
end;

procedure TTestRoundTreap.TestOptionalProperties;
const
  TestValues: TStringArray = (
    '{}',
    '{"bar":["}c","*[b","\\.","","","C=YSG2Q","_.?"],"baz":false}',
    '{"bar":["L9/","VE#Yy@"],"baz":false,"foo":"m"}',
    '{"bar":[],"foo":"1gzvX"}',
    '{"bar":["T~9<#","5i +","kII$Z","c;cQ{>"],"foo":";;l#y["}',
    '{"baz":true}',
    '{"bar":["D","Tt%","","_6\\","{NnV+."],"foo":""}',
    '{"bar":["j9#","","Qqd~jJ","","p,Aa.","y",",S"],"foo":"ug\"Pf"}',
    '{}',
    '{"foo":"kkX"}'
  );
begin
  RunTestSet(TestValues, optional_properties.TRootObject);
end;

procedure TTestRoundTreap.TestPropertyNameCollisions;
const
  TestValues: TStringArray = (
    '{"Foo":"''F1^v","foo":"g0PhuAK"}',
    '{"Foo":"e''4''4`","foo":"8M57"}',
    '{"Foo":"9(","foo":"ci_#"}',
    '{"Foo":"CASIci","foo":"WuN-WtG"}',
    '{"Foo":"fTG%|l9","foo":"l"}',
    '{"Foo":"uR?~*b/","foo":"Mn!xbBL"}',
    '{"Foo":"3/0h.*F","foo":""}',
    '{"Foo":"r","foo":""}',
    '{"Foo":"~yJ","foo":"|U.cz"}',
    '{"Foo":";p9Zt","foo":"-9BoQdj"}'
  );
begin
  RunTestSet(TestValues, property_name_collisions.TRootObject);
end;

procedure TTestRoundTreap.TestReference;
const
  TestValues: TStringArray = (
    '"A7+C"',
    '"!+kV*"',
    '"6njSq#9"',
    '"Y"',
    '"e +"',
    '"@0"',
    '"NZW\"`u"',
    '"Rb"',
    '"z;?cfP"',
    '"dj[iu"'
  );
begin
  RunTestSet(TestValues, reference.TRootObject);
end;

procedure TTestRoundTreap.TestRootBoolean;
const
  TestValues: TStringArray = (
    'false',
    'true',
    'true',
    'false'
  );
begin
  RunTestSet(TestValues, root_boolean.TRootObject);
end;

procedure TTestRoundTreap.TestRootInt16;
const
  TestValues: TStringArray = (
    '-21708',
    '-4852',
    '15700',
    '-15072',
    '-30542',
    '-10665',
    '15527',
    '-2509',
    '-9319',
    '19773'
  );
begin
  RunTestSet(TestValues, root_int16.TRootObject);
end;

procedure TTestRoundTreap.TestRootInt32;
const
  TestValues: TStringArray = (
    '-1793378960',
    '-568658495',
    '-261668640',
    '-555063746',
    '-1962502011',
    '1194318647',
    '1638241503',
    '-71941644',
    '367405686',
    '-2132175479'
  );
begin
  RunTestSet(TestValues, root_int32.TRootObject);
end;

procedure TTestRoundTreap.TestRootInt8;
const
  TestValues: TStringArray = (
    '33',
    '-73',
    '90',
    '80',
    '-82',
    '9',
    '-27',
    '-11',
    '-20',
    '-105'
  );
begin
  RunTestSet(TestValues, root_int8.TRootObject);
end;

procedure TTestRoundTreap.TestRootNullableString;
const
  TestValues: TStringArray = (
    '"+"',
    '"Ry$"',
    '"''5"',
    '"7r?5"',
    '"<:!sp#"',
    '")"',
    '"$qL2D8"',
    'null',
    '""',
    '"@Ob5."'
  );
begin
  RunTestSet(TestValues, root_nullable_string.TRootObject);
end;

procedure TTestRoundTreap.TestRootString;
const
  TestValues: TStringArray = (
    '""',
    '"pst"',
    '"8"',
    '"a"',
    '"704"',
    '"@Z"',
    '"A>8"',
    '"#$%"',
    '"wK}$Db"',
    '"0i)b}in"'
  );
begin
  RunTestSet(TestValues, root_string.TRootObject);
end;

procedure TTestRoundTreap.TestRootUint16;
const
  TestValues: TStringArray = (
    '18333',
    '49873',
    '14486',
    '28741',
    '0',
    '40291',
    '46615',
    '18338',
    '32634',
    '65535'
  );
begin
  RunTestSet(TestValues, root_uint16.TRootObject);
end;

procedure TTestRoundTreap.TestRootUint32;
const
  TestValues: TStringArray = (
    '939257830',
    '3694751507',
    '0',
    '3159607927',
    '4242312401',
    '342768777',
    '1489672024',
    '2668288273',
    '4294967295',
    '2561377967'
  );
begin
  RunTestSet(TestValues, root_uint32.TRootObject);
end;

procedure TTestRoundTreap.TestRootUint8;
const
  TestValues: TStringArray = (
    '230',
    '239',
    '0',
    '127',
    '91',
    '178',
    '255',
    '40',
    '78',
    '225'
  );
begin
  RunTestSet(TestValues, root_uint8.TRootObject);
end;

procedure TTestRoundTreap.TestTypeCollisions;
const
  TestValues: TStringArray = (
    '{"foo":{"bar":{"x":true}},"foo_bar":{"x":"a$.T"}}',
    '{"foo":{"bar":{"x":true}},"foo_bar":{"x":"%"}}',
    '{"foo":{"bar":{"x":true}},"foo_bar":{"x":"nNBUQC"}}',
    '{"foo":{"bar":{"x":false}},"foo_bar":{"x":"\\\"wp"}}',
    '{"foo":{"bar":{"x":true}},"foo_bar":{"x":"n(s "}}',
    '{"foo":{"bar":{"x":true}},"foo_bar":{"x":"#zr"}}',
    '{"foo":{"bar":{"x":true}},"foo_bar":{"x":"/Hb"}}',
    '{"foo":{"bar":{"x":true}},"foo_bar":{"x":"b"}}',
    '{"foo":{"bar":{"x":true}},"foo_bar":{"x":""}}',
    '{"foo":{"bar":{"x":false}},"foo_bar":{"x":"+b~k?Y"}}'
  );
begin
  RunTestSet(TestValues, type_collisions.TRootObject);
end;

procedure TTestRoundTreap.TestValuesUnit;
const
  TestValues: TStringArray = (
    '{"J|N":"B*","q%PVN+M":"KeY|)ki"}',
    '{"":"n0|","5t#o":"4","G1":"!","XXC5r":"#","]/qWdUd":"?h*","rQC0":"","y@":"w-I"}',
    '{"":"E&^Re","1":"","7":"VgJb","Yk''hk3":"g^5),\"","la":"","rUz":"W''+y{9","s":"^e\\M5n8"}',
    '{"1sGIj":"L_~7","2vC":"yh","8KQZ`_R":"4wQ[tEE","CRz,t%":"x$P6<\\Y"}',
    '{"W/bnm":"S","[B-":"EnvU.","cn@":"AOtO","d{#)tv;":"|"}',
    '{"MSI":"W\"~?aV","Mor;W":"/^=","V~@R":"","fYT":".<"}',
    '{"":"i{",",C[X)":"#Nb5","0g8|":"oP",">I@}":"c9p-iy2","NT=\\&LG":"{","_3 n--C":"hWY-b","j":"80-I"}',
    '{"(EEpdZ":"tkZ{E",",316u":")6ja","95gk]":"s-\"ZH","ADz":"ZOHn8","Ru":"8zBCC$","f{q1/":""}',
    '{"":"","%DD":"l-{pmI5",",v?+":""}',
    '{}'
  );
begin
  RunTestSet(TestValues, values.TRootObject);
end;



initialization

  RegisterTest(TTestRoundTreap);

end.

