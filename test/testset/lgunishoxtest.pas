unit LgUnishoxTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  LgUnishox;

type

  { TTestUnishox }

  TTestUnishox = class(TTestCase)
  private
  type
    TByte8 = array[0..7] of Byte;
  const
    UnicodePresets = [
      upsDefault, upsFavorAlpha, upsFavorDict, upsFavorSym, upsFavorUmlaut,
      upsNoDict, upsUrl, upsJson, upsXml, upsHtml
    ];
    SimpleBytes: TByte8 = (Byte('a'),Byte('b'),Byte('c'),Byte('d'),Byte('e'),Byte('f'),Byte('g'),Byte('h'));
    procedure TestCd(const s: string; pset: TUsxPreset);
  published
    procedure EncodeEmptySimple;
    procedure EncodeEmptyPreset;
    procedure OverflowSimple;
    procedure OverflowCompressPreset;
    procedure OverflowDecompressPreset;
    procedure TestBasic;
    procedure TestBasicEx;
    procedure TestNumbers;
    procedure TestSymbols;
    procedure TestRepeat;
    procedure TestNibbles;
    procedure TestTemplate;
    procedure TestUnicode;
    procedure TestUserTemplate;
  end;

implementation

procedure TTestUnishox.TestCd(const s: string; pset: TUsxPreset);
var
  r: string = '';
  buf: rawbytestring = '';
  Len: Integer;
begin
  SetLength(buf, Length(s) + Length(s) div 2);
  Len := unishox2_compress_preset(Pointer(s), Length(s), Pointer(buf), Length(buf), pset);
  AssertTrue(s+': compress buffer overflow', Len <= Length(buf));
  SetLength(r, Length(s));
  Len := unishox2_decompress_preset(Pointer(buf), Len, Pointer(r), Length(r), pset);
  AssertTrue(s+': decompress buffer overflow', Len <= Length(r));
  AssertTrue(s+': invalid decompressed length', Length(r) = Length(s));
  AssertTrue(s+': decompressing fail', r = s);
end;

procedure TTestUnishox.EncodeEmptySimple;
var
  buf1, buf2: TByte8;
  Len: Integer;
begin
  buf1 := SimpleBytes;
  Len := unishox2_compress_simple(@buf1, 0, @buf2, SizeOf(buf2));
  AssertTrue(Len = 1);
  Len := unishox2_decompress_simple(@buf2, Len, @buf1, SizeOf(buf1));
  AssertTrue(Len = 0);
  AssertTrue(buf1[0] = SimpleBytes[0]);
end;

procedure TTestUnishox.EncodeEmptyPreset;
var
  buf1, buf2: TByte8;
  Len: Integer;
  pset: TUsxPreset;
begin
  buf1 := SimpleBytes;
  for pset in TUsxPreset do
    begin
      Len := unishox2_compress_preset(@buf1, 0, @buf2, SizeOf(buf2), pset);
      AssertTrue(Len = 1);
      Len := unishox2_decompress_preset(@buf2, Len, @buf1, SizeOf(buf1), pset);
      AssertTrue(Len = 0);
      AssertTrue(buf1[0] = SimpleBytes[0]);
    end;
end;

procedure TTestUnishox.OverflowSimple;
var
  buf1, buf2: TByte8;
  Len: Integer;
begin
  buf1 := SimpleBytes;
  buf2 := Default(TByte8);
  Len := unishox2_compress_simple(@buf1, SizeOf(buf1), @buf2, 4);
  AssertTrue(Len = 5);
  AssertTrue(buf2[4] = 0);
  buf1 := Default(TByte8);
  Len := unishox2_decompress_simple(@buf2, 4, @buf1, 3);
  AssertTrue(Len = 4);
  AssertTrue(buf1[3] = 0);
end;

procedure TTestUnishox.OverflowCompressPreset;
var
  buf1, buf2: TByte8;
  Len: Integer;
  pset: TUsxPreset;
begin
  buf1 := SimpleBytes;
  for pset in TUsxPreset do begin
    buf2 := Default(TByte8);
    Len := unishox2_compress_preset(@buf1, SizeOf(buf1), @buf2, 4, pset);
    AssertTrue(Len = 5);
    AssertTrue(buf2[4] = 0);
  end;
end;

procedure TTestUnishox.OverflowDecompressPreset;
var
  buf1, buf2: TByte8;
  Len: Integer;
  pset: TUsxPreset;
begin
  buf1 := SimpleBytes;
  for pset in TUsxPreset do begin
    Len := unishox2_compress_preset(@SimpleBytes, SizeOf(SimpleBytes), @buf1, SizeOf(buf1), pset);
    buf2 := Default(TByte8);
    Len := unishox2_decompress_preset(@buf1, Len, @buf2, 5, pset);
    AssertTrue(Len = 6);
    AssertTrue(buf2[5] = 0);
  end;
end;

procedure TTestUnishox.TestBasic;
var
  pset: TUsxPreset;
  s: string;
const
  Samples: TStringArray = (
    'Hello', 'Hello World', 'The quick brown fox jumped over the lazy dog', 'HELLO WORLD',
    'HELLO WORLD HELLO WORLD', 'Lorem ipsum dolor sit amet consectetur adipiscing elit'
  );
begin
  for pset in TUsxPreset do
    for s in Samples do
      TestCd(s, pset);
end;

procedure TTestUnishox.TestBasicEx;
var
  pset: TUsxPreset;
  s: string;
const
  Samples: TStringArray = (
    'Cada buhonero alaba sus agujas. - A peddler praises his needles (wares).',
    'Cada gallo canta en su muladar. - Each rooster sings on its dung-heap.',
    'Cada martes tiene su domingo. - Each Tuesday has its Sunday.',
    'Cada uno habla de la feria como le va en ella. - Our way of talking about things reflects our relevant experience, good or bad.',
    'Donde comen dos, comen tres. - You can add one person more in any situation you are managing.',
    'El amor es ciego. - Love is blind.',
    'El amor todo lo iguala. - Love smoothes life out.',
    'El tiempo todo lo cura. - Time cures all.',
    'La avaricia rompe el saco. - Greed bursts the sack.',
    'La cara es el espejo del alma. - The face is the mirror of the soul.',
    'La diligencia es la madre de la buena ventura. - Diligence is the mother of good fortune.',
    'La mejor palabra siempre es la que queda por decir. - The best word is the one left unsaid.',
    'La sangre sin fuego hierve. - Blood boils without fire.',
    'La vida no es un camino de rosas. - Life is not a path of roses.',
    'Las burlas se vuelven veras. - Bad jokes become reality.',
    'Las desgracias nunca vienen solas. - Misfortunes never come one at a time.',
    'Lo comido es lo seguro. - You can only be really certain of what is already in your belly.',
    'Los celos son malos consejeros. - Jealousy is a bad counsellor.',
    'Los tiempos cambian. - Times change.',
    'No cantan dos gallos en un gallinero. - Two roosters do not crow in a henhouse.',
    'No hay harina sin salvado. - No flour without bran.',
    'No todas las verdades son para dichas. - Not every truth should be said.',
    'Perro ladrador, poco mordedor.. - A dog that barks often seldom bites.',
    'Todos los caminos llevan a Roma. - All roads lead to Rome.'
  );
begin
  for pset in TUsxPreset do
    if pset <> upsAlphaOnly then
      for s in Samples do
        TestCd(s, pset);
end;

procedure TTestUnishox.TestNumbers;
var
  pset: TUsxPreset;
  s: string;
const
  Samples: TStringArray = (
    'Hello1', 'Hello1 World2', 'Hello123', '12345678', '12345678 12345678',
    'HELLO WORLD 1234 hello world12', 'HELLO 234 WORLD', '9 HELLO, WORLD',
    'H1e2l3l4o5 w6O7R8L9D','8+80=88'
  );
begin
  for pset in TUsxPreset do
    if pset <> upsAlphaOnly then
      for s in Samples do
        TestCd(s, pset);
end;

procedure TTestUnishox.TestSymbols;
var
  pset: TUsxPreset;
  s: string;
const
  Samples: TStringArray = (
    '~!@#$%^&*()_+=-`;''\\|\":,./?><', 'if (!test_ushx_cd(\"H1e2l3l4o5 w6O7R8L9D\',
    'Hello\tWorld\tHow\tare\tyou?', 'Hello~World~How~are~you?','Hello\rWorld\rHow\rare\ryou?'
  );
begin
  for pset in TUsxPreset do
    if not(pset in [upsAlphaOnly, upsAlphaNumOnly]) then
      for s in Samples do
        TestCd(s, pset);
end;

procedure TTestUnishox.TestRepeat;
var
  pset: TUsxPreset;
  s: string;
const
  Samples: TStringArray = (
    '-----------------///////////////',
    '-----------------Hello World1111111111112222222abcdef12345abcde1234_////////Hello World///////',
    '-----------------///////////////\\\\\\\\\\\\\\\\\\\\\\\\\\',
    'Hello World------------------------------------'
  );
begin
  for pset in TUsxPreset do
    if not(pset in [upsAlphaOnly, upsAlphaNumOnly]) then
      for s in Samples do
        TestCd(s, pset);
end;

procedure TTestUnishox.TestNibbles;
var
  pset: TUsxPreset;
  s: string;
const
  Samples: TStringArray = (
    'fa01b51e-7ecc-4e3e-be7b-918a4c2c891c', 'Fa01b51e-7ecc-4e3e-be7b-918a4c2c891c',
    'fa01b51e-7ecc-4e3e-be7b-9182c891c', '760FBCA3-272E-4F1A-BF88-8472DF6BD994',
    '760FBCA3-272E-4F1A-BF88-8472DF6Bd994', '760FBCA3-272E-4F1A-BF88-8472DF6Bg994',
    'FBCA3-272E-4F1A-BF88-8472DF6BD994', 'Hello 1 5347a688-d8bf-445d-86d1-b470f95b007fHello World',
    '01234567890123'
  );
begin
  for pset in TUsxPreset do
    if pset <> upsAlphaOnly then
      for s in Samples do
        TestCd(s, pset);
end;

procedure TTestUnishox.TestTemplate;
var
  pset: TUsxPreset;
  s: string;
const
  Samples: TStringArray = (
    '2020-12-31', '1934-02', '2020-12-31T12:23:59.234Z', '1899-05-12T23:59:59.23434',
    '1899-05-12T23:59:59', '2020-12-31T12:23:59.234Zfa01b51e-7ecc-4e3e-be7b-918a4c2c891c',
    'HELLO(993) 345-3495WORLD', 'HELLO1899-05-12T23:59:59WORLD','Lorem ipsum (777) 123-4567'
  );
begin
  for pset in TUsxPreset do
    if not(pset in [upsAlphaOnly, upsAlphaNumOnly]) then
      for s in Samples do
        TestCd(s, pset);
end;

procedure TTestUnishox.TestUnicode;
var
  pset: TUsxPreset;
  s: string;
const
  Samples: TStringArray = (
    '案ずるより産むが易し。 - Giving birth to a baby is easier than worrying about it.',
    '出る杭は打たれる。 - The stake that sticks up gets hammered down.',
    '知らぬが仏。 - Not knowing is Buddha. - Ignorance is bliss.',
    '見ぬが花。 - Not seeing is a flower. - Reality can''t compete with imagination.',
    '花は桜木人は武士 - Of flowers, the cherry blossom; of men, the warrior.',
    '小洞不补，大洞吃苦 - A small hole not mended in time will become a big hole much more difficult to mend.',
    '读万卷书不如行万里路 - Reading thousands of books is not as good as traveling thousands of miles',
    '福无重至,祸不单行 - Fortune does not come twice. Misfortune does not come alone.',
    '风向转变时,有人筑墙,有人造风车 - When the wind changes, some people build walls and have artificial windmills.',
    '父债子还 - Father''s debt, son to give back.',
    '害人之心不可有 - Do not harbour intentions to hurt others.',
    '今日事，今日毕 - Things of today, accomplished today.',
    '空穴来风,未必无因 - Where there''s smoke, there''s fire.',
    '良药苦口 - Good medicine tastes bitter.',
    '人算不如天算 - Man proposes and God disposes',
    '师傅领进门，修行在个人 - Teachers open the door. You enter by yourself.',
    '授人以鱼不如授之以渔 - Teach a man to take a fish is not equal to teach a man how to fish.',
    '树倒猢狲散 - When the tree falls, the monkeys scatter.',
    '水能载舟，亦能覆舟 - Not only can water float a boat, it can sink it also.',
    '朝被蛇咬，十年怕井绳 - Once bitten by a snake for a snap dreads a rope for a decade.',
    '一分耕耘，一分收获 - If one does not plow, there will be no harvest.',
    '有钱能使鬼推磨 - If you have money you can make the devil push your grind stone.',
    '一失足成千古恨，再回头已百年身 - A single slip may cause lasting sorrow.',
    '自助者天助 - Those who help themselves, God will help.',
    '早起的鸟儿有虫吃 - Early bird gets the worm.',
    'This is first line,\r\nThis is second line',
    '{\"menu\": {\n  \"id\": \"file\",\n  \"value\": \"File\",\n  \"popup\": {\n    \"menuitem\": [\n      {\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"},\n      {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"},\n      {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"}\n    ]\n  }\n}}',
    '{\"menu\": {\r\n  \"id\": \"file\",\r\n  \"value\": \"File\",\r\n  \"popup\": {\r\n    \"menuitem\": [\r\n      {\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"},\r\n      {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"},\r\n      {\"value\":\"Close\", \"onclick\": \"CloseDoc()\"}\r\n    ]\r\n  }\r\n}}',
    'https://siara.cc',
    '符号\"δ\"表',
    '学者地”[3]。学者',
    '한데......아무',
    // English
    'Beauty is not in the face. Beauty is a light in the heart.',
    // Spanish
    'La belleza no está en la cara. La belleza es una luz en el corazón.',
    // French
    'La beauté est pas dans le visage. La beauté est la lumière dans le coeur.',
    // Portugese
    'A beleza não está na cara. A beleza é a luz no coração.',
    // Dutch
    'Schoonheid is niet in het gezicht. Schoonheid is een licht in het hart.',
    // German
    'Schönheit ist nicht im Gesicht. Schönheit ist ein Licht im Herzen.',
    // Spanish
    'La belleza no está en la cara. La belleza es una luz en el corazón.',
    // French
    'La beauté est pas dans le visage. La beauté est la lumière dans le coeur.',
    // Italian
    'La bellezza non è in faccia. La bellezza è la luce nel cuore.',
    // Swedish
    'Skönhet är inte i ansiktet. Skönhet är ett ljus i hjärtat.',
    // Romanian
    'Frumusețea nu este în față. Frumusețea este o lumină în inimă.',
    // Ukranian
    'Краса не в особі. Краса - це світло в серці.',
    // Greek
    'Η ομορφιά δεν είναι στο πρόσωπο. Η ομορφιά είναι ένα φως στην καρδιά.',
    // Turkish
    'Güzellik yüzünde değil. Güzellik, kalbin içindeki bir ışıktır.',
    // Polish
    'Piękno nie jest na twarzy. Piękno jest światłem w sercu.',
    // Africans
    'Skoonheid is nie in die gesig nie. Skoonheid is ''n lig in die hart',
    // Swahili
    'Beauty si katika uso. Uzuri ni nuru moyoni.',
    // Zulu
    'Ubuhle abukho ebusweni. Ubuhle bungukukhanya enhliziyweni.',
    // Somali
    'Beauty ma aha in wajiga. Beauty waa iftiin ah ee wadnaha.',
    // Russian
    'Красота - не в лице. Красота - это свет в сердце.',
    // Arabic
    '"الجمال ليس في الوجه. الجمال هو النور الذي في القلب.',
    // Persian
    'زیبایی در چهره نیست. زیبایی نور در قلب است.',
    // Pashto
    'ښکلا په مخ کې نه ده. ښکلا په زړه کی یوه رڼا ده.',
    // Azerbaijani
    'Gözəllik üzdə deyil. Gözəllik qəlbdə bir işıqdır.',
    // Uzbek
    'Go''zallik yuzida emas. Go''zallik - qalbdagi nur.',
    // Kurdish
    'Bedewî ne di rû de ye. Bedewî di dil de ronahiyek e.',
    // Urdu
    'خوبصورتی چہرے میں نہیں ہے۔ خوبصورتی دل میں روشنی ہے۔',
    // Chinese
    '美是不是在脸上。 美是心中的亮光。',
    // Javanese
    'Beauty ora ing pasuryan. Kaendahan iku cahya ing sajroning ati.',
    // Japanese
    '美は顔にありません。美は心の中の光です。',
    // Filipino
    'Ang kagandahan ay wala sa mukha. Ang kagandahan ay ang ilaw sa puso.',
    // Korean
    '아름다움은 얼굴에 없습니다。아름다움은 마음의 빛입니다。',
    // Vietnam
    'Vẻ đẹp không nằm trong khuôn mặt. Vẻ đẹp là ánh sáng trong tim.',
    // Malay
    'Kecantikan bukan di muka. Kecantikan adalah cahaya di dalam hati.',
    // Emoji
    '🤣🤣🤣🤣🤣🤣🤣🤣🤣🤣🤣',
    '😀😃😄😁😆😅🤣😂🙂🙃😉😊😇🥰😍🤩😘😗😚😙😋😛😜🤪😝🤑🤗🤭🤫🤔🤐🤨😐😑😶😏😒🙄😬🤥😌😔😪🤤😴😷🤒🤕🤢',
    // Binary
    'Hello'#$80#$83#$AE#$BC#$BD#$BE,
    'Hello world'#0'with nulls'#0
  );
begin
  for pset in UnicodePresets do
    for s in Samples do
      TestCd(s, pset);
end;

procedure TTestUnishox.TestUserTemplate;
var
  s, r: string;
  buf: rawbytestring = '';
  Len: Integer;
const
  Samples: TStringArray = (
    '2021-03-11 23:24:30',
    'this is datetime: 1012-07-21 11:24:30.',
    '28.02.2025 16.07.2023',
    'дата проведения: 15.01.2027',
    '1012-07-21 11:24:30 28.02.2025 26.01.1993'
  );
  Tmpl: TUsxTemplates = (
    'tfff-of-tf tf:rf:rf',
    'tf.of.tfff',
    nil,
    nil,
    nil
  );
begin
  for s in Samples do begin
    SetLength(buf, Length(s) + Length(s) div 2);
    Len := unishox2_compress(
      Pointer(s), Length(s), Pointer(buf), Length(buf), USX_HCODES_FAVOR_ALPHA, USX_HCODE_LENS_FAVOR_ALPHA, nil, @Tmpl);
    AssertTrue(s+': compress buffer overflow', Len <= Length(buf));
    SetLength(r, Length(s));
    Len := unishox2_decompress(
      Pointer(buf), Len, Pointer(r), Length(r), USX_HCODES_FAVOR_ALPHA, USX_HCODE_LENS_FAVOR_ALPHA, nil, @Tmpl);
    AssertTrue(s+': decompress buffer overflow', Len <= Length(r));
    AssertTrue(s+': invalid decompressed length', Length(r) = Length(s));
    AssertTrue(s+': decompressing fail', r = s);
  end;
end;



initialization

  RegisterTest(TTestUnishox);

end.

