program pdo2json;

{$MODE OBJFPC}{$H+}{$MODESWITCH ADVANCEDRECORDS}
{$OPTIMIZATION NOORDERFIELDS}

uses
  SysUtils, Variants, lgUtils, lgJson, lgPdo;

type
  TPerson = record
    Id,
    Age: Integer;
    FirstName,
    LastName,
    Phone: string;
    class procedure WriteJson(r: Pointer; aWriter: TJsonStrWriter); static;
    constructor Make(aId, aAge: Integer; const aName, aLast, aPhone: string);
  end;

  TIntArray = array of Integer;

  TMember = record
    MemberId: Integer;
    Meta: Variant;
    Person: TPerson;
    RegisterDate: string;
    Friends: TIntArray;
    Groups: TStringArray;
    constructor Make(
      const p: TPerson; aId: Integer; const aMeta: Variant; const d: string; const f: TIntArray;
      const g: TStringArray);
  end;
  TMemberList = array of TMember;

  TCommunity = record
    Name,
    Slogan: string;
    GroupList: TStringArray;
    MemberList: TMemberList;
    constructor Make(const aName, aSlogan: string; const gl: TStringArray; const ml: TMemberList);
  end;
  TCommunityList = array of TCommunity;

class procedure TPerson.WriteJson(r: Pointer; aWriter: TJsonStrWriter);
type
  PPerson = ^TPerson;
const
  cId    = 'id';
  cAge   = 'age';
  cName  = 'firstName';
  cLast  = 'lastName';
  cPhone = 'phone';
begin
  with PPerson(r)^ do
    aWriter
      .BeginObject
        .Add(cId, Id)
        .Add(cAge, Age)
        .Add(cName, FirstName)
        .Add(cLast, LastName)
        .Add(cPhone, Phone)
      .EndObject;
end;

constructor TPerson.Make(aId, aAge: Integer; const aName, aLast, aPhone: string);
begin
  Id := aId;
  Age := aAge;
  FirstName := aName;
  LastName := aLast;
  Phone := aPhone;
end;

constructor TMember.Make(const p: TPerson; aId: Integer; const aMeta: Variant; const d: string;
  const f: TIntArray; const g: TStringArray);
begin
  MemberId := aId;
  Meta := aMeta;
  Person := p;
  RegisterDate := d;
  Friends := f;
  Groups := g;
end;

constructor TCommunity.Make(const aName, aSlogan: string; const gl: TStringArray; const ml: TMemberList);
begin
  Name := aName;
  Slogan := aSlogan;
  GroupList := gl;
  MemberList := ml;
end;

var
  ComList: array of TCommunity = nil;

procedure MakeList;
begin
  ComList := [
    TCommunity.Make(
      'Earthlings', 'The further, the more',
      ['bead game', 'kitchen', 'fishing', 'beer', 'skates', 'cutting and sewing', 'choral singing'],
      [
        TMember.Make(
          TPerson.Make(
            42, 21, 'Walter', 'Reynolds', '(204) 537-5981'
          ),
          1, null, '2018-11-13', [7, 17], ['bead game', 'beer', 'skates', 'cutting and sewing']
        ),
        TMember.Make(
          TPerson.Make(
            11, 32, 'Leigh', 'Garza', '(686) 795-8242'
          ),
          7, VarArrayOf([42, pi]), '2020-01-10', [1, 17, 21], ['kitchen', 'fishing', 'cutting and sewing']
        ),
        TMember.Make(
          TPerson.Make(
            5, 19, 'Damon', 'Shelton', '1-404-576-3173'
          ),
          17, VarArrayOf(['none', 'some', null]), '2020-07-29', [1, 11, 21], ['bead game', 'kitchen', 'fishing']
        ),
        TMember.Make(
          TPerson.Make(
            3, 27, 'Nora', 'Potts', '(824) 554-0791'
          ),
          11, VarArrayOf(['meta', 1001]), '2019-03-21', [7, 17, 21], ['beer', 'skates', 'cutting and sewing']
        ),
        TMember.Make(
          TPerson.Make(
            24, 44, 'Shad', 'Campbell', '(874) 556-376'
          ),
          21, 42, '2021-05-20', [1, 7, 17, 11], ['fishing', 'beer', 'cutting and sewing']
        )
      ]
    ),
    TCommunity.Make(
      'Rabbit breeders', 'Who is not with us is not with us',
      ['dinosaur hunting', 'knitting', 'chess', 'car race', 'dancing'],
      [
        TMember.Make(
            TPerson.Make(
              5, 23, 'Mikayla', 'Ray', '1-565-598-5632'
            ),
            22, VarArrayOf(['first', null, 42]), '2018-02-23', [17, 71, 45], ['dinosaur hunting', 'knitting']
        ),
        TMember.Make(
          TPerson.Make(
            37, 35, 'Tyler', 'Moody', '(345) 727-5455'
          ),
          17, 1001, '2021-11-13', [45, 10, 33], ['chess', 'car race', 'dancing']
        ),
        TMember.Make(
          TPerson.Make(
            17, 54, 'Plato', 'Henson', '1-385-896-8851'
          ),
          71, VarArrayOf(['get','put',null]), '2020-12-11', [22, 17], ['knitting', 'chess', 'car race']
        ),
        TMember.Make(
          TPerson.Make(
            23, 19, 'Merrill', 'Joseph', '(634) 768-7274'
          ),
          45, null, '2022-01-15', [17, 71, 10], ['dinosaur hunting', 'chess', 'dancing']
        ),
        TMember.Make(
          TPerson.Make(
            12, 29, 'Jessica', 'Meyers', '(706) 844-0017'
          ),
          10, VarArrayOf(['stop','none']), '2017-04-30', [17, 71, 45], ['knitting', 'chess', 'car race', 'dancing']
        ),
        TMember.Make(
          TPerson.Make(
            7, 33, 'Boris', 'Norton', '(211) 688-1153'
          ),
          33, VarArrayOf([null, null]), '2019-08-07', [22, 17, 71], ['dinosaur hunting', 'knitting', 'chess', 'car race', 'dancing']
        )
      ]
    )
  ];
end;

var
  n: specialize TGAutoRef<TJsonNode>;
  s: string;
begin
  RegisterRecordJsonProc(TypeInfo(TPerson), @TPerson.WriteJson);
  RegisterRecordFields(TypeInfo(TMember), ['memberId', 'meta', 'person', 'registerDate', 'friends', 'groups']);
  RegisterRecordFields(TypeInfo(TCommunity), ['name', 'slogan', 'groupList', 'memberList']);

  MakeList;

  s := specialize PdoToJson<TCommunityList>(ComList);

  if not n.Instance.Parse(s) then
    WriteLn('Oops, something went wrong :(')
  else
    WriteLn(n.Instance.FormatJson([jfoEgyptBrace]));
end.

