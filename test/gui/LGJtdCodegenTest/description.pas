{
  Source schema: description.jtd.json

  This unit was automatically created by JtdPasCodegen, do not edit.
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
  TEnumWithDescriptionElem = class sealed(specialize TJtdEnum<TEnumWithDescription>)
    class function GetJtdClass: TJtdEntityClass; override;
  end;

{ A description for properties }
  TPropertiesWithDescription = class sealed(TJtdObject)
  private
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
  end;

{ A description for discriminator variant }
  TBar = class sealed(TJtdObject)
  private
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
  end;

{ A description for discriminator }
  TDiscriminatorWithDescription = class sealed(TJtdVariant)
  protected
    function GetBar: TBar;
    procedure SetBar(aValue: TBar);
    class function GetTagJsonName: string; override;
    class function ValidTagValue(const aValue: string): Boolean; override;
    class function GetInstanceClass(const aTag: string): TJtdEntityClass; override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
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
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
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

{ TEnumWithDescriptionElem }

class function TEnumWithDescriptionElem.GetJtdClass: TJtdEntityClass;
begin
  Result := TEnumWithDescriptionElem;
end;

{ TPropertiesWithDescription }

class function TPropertiesWithDescription.GetJtdClass: TJtdEntityClass;
begin
  Result := TPropertiesWithDescription;
end;

procedure TPropertiesWithDescription.Clear;
begin
end;

{$PUSH}{$WARN 5057 OFF}
procedure TPropertiesWithDescription.DoReadJson(aNode: TJsonNode);
begin
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TPropertiesWithDescription.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> tkObjectBegin then ReadError;
  Clear;
  repeat
    if not aReader.Read then ReadError;
    if aReader.TokenKind = tkObjectEnd then break;
  until False;
end;
{$POP}

procedure TPropertiesWithDescription.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.EndObject;
end;

{ TBar }

class function TBar.GetJtdClass: TJtdEntityClass;
begin
  Result := TBar;
end;

procedure TBar.Clear;
begin
end;

{$PUSH}{$WARN 5057 OFF}
procedure TBar.DoReadJson(aNode: TJsonNode);
begin
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TBar.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> tkObjectBegin then ReadError;
  Clear;
  repeat
    if not aReader.Read then ReadError;
    if aReader.TokenKind = tkObjectEnd then break;
  until False;
end;
{$POP}

procedure TBar.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.EndObject;
end;

{ TDiscriminatorWithDescription }

class function TDiscriminatorWithDescription.GetJtdClass: TJtdEntityClass;
begin
  Result := TDiscriminatorWithDescription;
end;

class function TDiscriminatorWithDescription.GetTagJsonName: string;
begin
  Result := 'foo';
end;

class function TDiscriminatorWithDescription.ValidTagValue(const aValue: string): Boolean;
begin
  case aValue of
    'bar': Result := True;
  else
    Result := False;
  end;
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

class function TDescription.GetJtdClass: TJtdEntityClass;
begin
  Result := TDescription;
end;

procedure TDescription.Clear;
begin
  FreeAndNil(FLongDescription);
  FreeAndNil(FRefWithDescription);
  FreeAndNil(FStringWithDescription);
  FreeAndNil(FEnumWithDescription);
  FreeAndNil(FPropertiesWithDescription);
  FreeAndNil(FDiscriminatorWithDescription);
end;

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
procedure TDescription.DoReadJson(aNode: TJsonNode);
var
  p: TJsonNode.TPair;
  Flags: array[0..5] of Boolean;
  I: Integer;
begin
  if not aNode.IsObject then ReadError;
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  for p in aNode.Entries do
    case p.Key of
      'long_description':
        begin
          FLongDescription := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[0] := True;
        end;
      'ref_with_description':
        begin
          FRefWithDescription := TBazString(TBazString.ReadJson(p.Value));
          Flags[1] := True;
        end;
      'string_with_description':
        begin
          FStringWithDescription := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[2] := True;
        end;
      'enum_with_description':
        begin
          FEnumWithDescription := TEnumWithDescriptionElem(TEnumWithDescriptionElem.ReadJson(p.Value));
          Flags[3] := True;
        end;
      'properties_with_description':
        begin
          FPropertiesWithDescription := TPropertiesWithDescription(TPropertiesWithDescription.ReadJson(p.Value));
          Flags[4] := True;
        end;
      'discriminator_with_description':
        begin
          FDiscriminatorWithDescription := TDiscriminatorWithDescription(TDiscriminatorWithDescription.ReadJson(p.Value));
          Flags[5] := True;
        end;
    else
      UnknownProp(p.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('long_description');
        1: PropNotFound('ref_with_description');
        2: PropNotFound('string_with_description');
        3: PropNotFound('enum_with_description');
        4: PropNotFound('properties_with_description');
        5: PropNotFound('discriminator_with_description');
      else
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TDescription.DoReadJson(aReader: TJsonReader);
var
  Flags: array[0..5] of Boolean;
  I: Integer;
begin
  if aReader.TokenKind <> tkObjectBegin then ReadError;
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReadError;
    if aReader.TokenKind = tkObjectEnd then break;
    case aReader.Name of
      'long_description':
        begin
          FLongDescription := TJtdString(TJtdString.ReadJson(aReader));
          Flags[0] := True;
        end;
      'ref_with_description':
        begin
          FRefWithDescription := TBazString(TBazString.ReadJson(aReader));
          Flags[1] := True;
        end;
      'string_with_description':
        begin
          FStringWithDescription := TJtdString(TJtdString.ReadJson(aReader));
          Flags[2] := True;
        end;
      'enum_with_description':
        begin
          FEnumWithDescription := TEnumWithDescriptionElem(TEnumWithDescriptionElem.ReadJson(aReader));
          Flags[3] := True;
        end;
      'properties_with_description':
        begin
          FPropertiesWithDescription := TPropertiesWithDescription(TPropertiesWithDescription.ReadJson(aReader));
          Flags[4] := True;
        end;
      'discriminator_with_description':
        begin
          FDiscriminatorWithDescription := TDiscriminatorWithDescription(TDiscriminatorWithDescription.ReadJson(aReader));
          Flags[5] := True;
        end;
    else
      UnknownProp(aReader.Name);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('long_description');
        1: PropNotFound('ref_with_description');
        2: PropNotFound('string_with_description');
        3: PropNotFound('enum_with_description');
        4: PropNotFound('properties_with_description');
        5: PropNotFound('discriminator_with_description');
      else
      end;
end;
{$POP}

procedure TDescription.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
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
  aWriter.EndObject;
end;

end.
