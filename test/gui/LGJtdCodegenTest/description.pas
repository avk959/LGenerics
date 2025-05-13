{
  Source schema: description.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit description;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

{ A description for a definition }
  TBazString = TJtdString;

{ A description for enum }
  TEnumWithDescription = (
    X, // A description for X
    Y, // A description for Y
    Z // A description for Z
  );

{ Container for some TEnumWithDescription enumeration element }
  TEnumWithDescriptionElem = class sealed(specialize TJtdEnum<TEnumWithDescription>);

{ A description for properties }
  TPropertiesWithDescription = class sealed(TJtdObject)
  private
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
    procedure ClearFields; override;
  public
  end;

{ A description for discriminator variant }
  TBar = class sealed(TJtdObject)
  private
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
    procedure ClearFields; override;
  public
  end;

{ A description for discriminator }
  TDiscriminatorWithDescription = class sealed(TJtdUnion)
  protected
    function GetBar: TBar;
    procedure SetBar(aValue: TBar);
    class function GetTagJsonName: string; override;
    class function GetInstanceClass(const aTag: string): TJtdEntityClass; override;
  public
  { A description for discriminator variant
    matches the "bar" tag }
    property Bar: TBar read GetBar write SetBar;
  end;

  TDescription = class sealed(TJtdObject)
  private
    FLongDescription: TJtdString;
    FRefWithDescription: TBazString;
    FStringWithDescription: TJtdString;
    FEnumWithDescription: TEnumWithDescriptionElem;
    FPropertiesWithDescription: TPropertiesWithDescription;
    FDiscriminatorWithDescription: TDiscriminatorWithDescription;
    procedure SetLongDescription(aValue: TJtdString);
    procedure SetRefWithDescription(aValue: TBazString);
    procedure SetStringWithDescription(aValue: TJtdString);
    procedure SetEnumWithDescription(aValue: TEnumWithDescriptionElem);
    procedure SetPropertiesWithDescription(aValue: TPropertiesWithDescription);
    procedure SetDiscriminatorWithDescription(aValue: TDiscriminatorWithDescription);
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
    procedure ClearFields; override;
  public
  { Whereas disregard and contempt for human rights have resulted in barbarous 
    acts which have outraged the conscience of mankind, and the advent of 
    a world in which human beings shall enjoy freedom of speech and belief 
    and freedom from fear and want has been proclaimed as the highest aspiration 
    of the common people, refers to "long_description" JSON property }
    property LongDescription: TJtdString read FLongDescription write SetLongDescription;
  { A description for ref; refers to "ref_with_description" JSON property }
    property RefWithDescription: TBazString read FRefWithDescription write SetRefWithDescription;
  { A description for string; refers to "string_with_description" JSON property }
    property StringWithDescription: TJtdString read FStringWithDescription write SetStringWithDescription;
  { A description for enum; refers to "enum_with_description" JSON property }
    property EnumWithDescription: TEnumWithDescriptionElem read FEnumWithDescription write SetEnumWithDescription;
  { A description for properties; refers to "properties_with_description" 
    JSON property }
    property PropertiesWithDescription: TPropertiesWithDescription read FPropertiesWithDescription write SetPropertiesWithDescription;
  { A description for discriminator; refers to "discriminator_with_description" 
    JSON property }
    property DiscriminatorWithDescription: TDiscriminatorWithDescription read FDiscriminatorWithDescription write SetDiscriminatorWithDescription;
  end;

implementation

{ TPropertiesWithDescription }

{$PUSH}{$WARN 5057 OFF}
procedure TPropertiesWithDescription.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> tkObjectBegin then ExpectObject(aReader);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
  until False;
end;
{$POP}

procedure TPropertiesWithDescription.WriteFields(aWriter: TJsonStrWriter);
begin
end;

procedure TPropertiesWithDescription.DoClear;
begin
end;

procedure TPropertiesWithDescription.ClearFields;
begin
end;

procedure TPropertiesWithDescription.CreateFields;
begin
end;

{ TBar }

{$PUSH}{$WARN 5057 OFF}
procedure TBar.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> tkObjectBegin then ExpectObject(aReader);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
  until False;
end;
{$POP}

procedure TBar.WriteFields(aWriter: TJsonStrWriter);
begin
end;

procedure TBar.DoClear;
begin
end;

procedure TBar.ClearFields;
begin
end;

procedure TBar.CreateFields;
begin
end;

class function TDiscriminatorWithDescription.GetTagJsonName: string;
begin
  Result := 'foo';
end;

class function TDiscriminatorWithDescription.GetInstanceClass(const aTag: string): TJtdEntityClass;
begin
  case aTag of
    'bar': Result := TBar;
  else
    Result := nil;
  end;
end;

function TDiscriminatorWithDescription.GetBar: TBar;
begin
  Result := FInstance as TBar;
end;

procedure TDiscriminatorWithDescription.SetBar(aValue: TBar);
begin
  if aValue = FInstance then exit;
  FInstance.Free;
  FInstance := aValue;
  FTag := 'bar';
end;

{ TDescription }

procedure TDescription.SetLongDescription(aValue: TJtdString);
begin
  if aValue = FLongDescription then exit;
  FLongDescription.Free;
  FLongDescription := aValue;
end;

procedure TDescription.SetRefWithDescription(aValue: TBazString);
begin
  if aValue = FRefWithDescription then exit;
  FRefWithDescription.Free;
  FRefWithDescription := aValue;
end;

procedure TDescription.SetStringWithDescription(aValue: TJtdString);
begin
  if aValue = FStringWithDescription then exit;
  FStringWithDescription.Free;
  FStringWithDescription := aValue;
end;

procedure TDescription.SetEnumWithDescription(aValue: TEnumWithDescriptionElem);
begin
  if aValue = FEnumWithDescription then exit;
  FEnumWithDescription.Free;
  FEnumWithDescription := aValue;
end;

procedure TDescription.SetPropertiesWithDescription(aValue: TPropertiesWithDescription);
begin
  if aValue = FPropertiesWithDescription then exit;
  FPropertiesWithDescription.Free;
  FPropertiesWithDescription := aValue;
end;

procedure TDescription.SetDiscriminatorWithDescription(aValue: TDiscriminatorWithDescription);
begin
  if aValue = FDiscriminatorWithDescription then exit;
  FDiscriminatorWithDescription.Free;
  FDiscriminatorWithDescription := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TDescription.DoReadJson(aReader: TJsonReader);
var
  Flags: array[0..5] of Boolean;
  I: Integer;
begin
  if aReader.TokenKind <> tkObjectBegin then ExpectObject(aReader);
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
    case aReader.Name of
      'long_description':
        if not Flags[0] then begin
          FLongDescription.ReadJson(aReader);
          Flags[0] := True;
        end else DuplicateProp(aReader);
      'ref_with_description':
        if not Flags[1] then begin
          FRefWithDescription.ReadJson(aReader);
          Flags[1] := True;
        end else DuplicateProp(aReader);
      'string_with_description':
        if not Flags[2] then begin
          FStringWithDescription.ReadJson(aReader);
          Flags[2] := True;
        end else DuplicateProp(aReader);
      'enum_with_description':
        if not Flags[3] then begin
          FEnumWithDescription.ReadJson(aReader);
          Flags[3] := True;
        end else DuplicateProp(aReader);
      'properties_with_description':
        if not Flags[4] then begin
          FPropertiesWithDescription.ReadJson(aReader);
          Flags[4] := True;
        end else DuplicateProp(aReader);
      'discriminator_with_description':
        if not Flags[5] then begin
          FDiscriminatorWithDescription.ReadJson(aReader);
          Flags[5] := True;
        end else DuplicateProp(aReader);
    else
      UnknownProp(aReader.Name, aReader);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('long_description', aReader);
        1: PropNotFound('ref_with_description', aReader);
        2: PropNotFound('string_with_description', aReader);
        3: PropNotFound('enum_with_description', aReader);
        4: PropNotFound('properties_with_description', aReader);
        5: PropNotFound('discriminator_with_description', aReader);
      end;
end;
{$POP}

procedure TDescription.WriteFields(aWriter: TJsonStrWriter);
begin
  aWriter.AddName('long_description');
  LongDescription.WriteJson(aWriter);
  aWriter.AddName('ref_with_description');
  RefWithDescription.WriteJson(aWriter);
  aWriter.AddName('string_with_description');
  StringWithDescription.WriteJson(aWriter);
  aWriter.AddName('enum_with_description');
  EnumWithDescription.WriteJson(aWriter);
  aWriter.AddName('properties_with_description');
  PropertiesWithDescription.WriteJson(aWriter);
  aWriter.AddName('discriminator_with_description');
  DiscriminatorWithDescription.WriteJson(aWriter);
end;

procedure TDescription.DoClear;
begin
end;

procedure TDescription.ClearFields;
begin
  FLongDescription.Free;
  FRefWithDescription.Free;
  FStringWithDescription.Free;
  FEnumWithDescription.Free;
  FPropertiesWithDescription.Free;
  FDiscriminatorWithDescription.Free;
end;

procedure TDescription.CreateFields;
begin
  FLongDescription := TJtdString.Create;
  FRefWithDescription := TBazString.Create;
  FStringWithDescription := TJtdString.Create;
  FEnumWithDescription := TEnumWithDescriptionElem.Create;
  FPropertiesWithDescription := TPropertiesWithDescription.Create;
  FDiscriminatorWithDescription := TDiscriminatorWithDescription.Create;
end;

end.
