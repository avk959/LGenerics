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
  TBaz = TJtdString;

{ A description for enum }
  TEnumWithDescriptionType = (
    X, // A description for X
    Y, // A description for Y
    Z // A description for Z
  );

{ Container for some TEnumWithDescriptionType enumeration element }
  TEnumWithDescription = class sealed(specialize TJtdEnum<TEnumWithDescriptionType>);

{ A description for properties }
  TPropertiesWithDescription = class sealed(TJtdObject)
  private
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
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
    procedure Clear; override;
  end;

{ A description for discriminator }
  TDiscriminatorWithDescription = class sealed(TJtdUnion)
  protected
    function  GetBar: TBar;
    procedure SetBar(aValue: TBar);
    class function GetTagJsonName: string; override;
    class function GetInstanceClass(const aTag: string): TJtdEntityClass; override;
  public
  { matches the "bar" tag; A description for discriminator variant }
    property Bar: TBar read GetBar write SetBar;
  end;

  TRootObject = class sealed(TJtdObject)
  private
    FLongDescription: TJtdString;
    FRefWithDescription: TBaz;
    FStringWithDescription: TJtdString;
    FEnumWithDescription: TEnumWithDescription;
    FPropertiesWithDescription: TPropertiesWithDescription;
    FDiscriminatorWithDescription: TDiscriminatorWithDescription;
    procedure SetLongDescription(aValue: TJtdString);
    procedure SetRefWithDescription(aValue: TBaz);
    procedure SetStringWithDescription(aValue: TJtdString);
    procedure SetEnumWithDescription(aValue: TEnumWithDescription);
    procedure SetPropertiesWithDescription(aValue: TPropertiesWithDescription);
    procedure SetDiscriminatorWithDescription(aValue: TDiscriminatorWithDescription);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    procedure Clear; override;
  { refers to "long_description" JSON property; Whereas disregard and contempt for 
    human rights have resulted in barbarous acts which have outraged the conscience 
    of mankind, and the advent of a world in which human beings shall enjoy freedom 
    of speech and belief and freedom from fear and want has been proclaimed as the 
    highest aspiration of the common people, }
    property LongDescription: TJtdString read FLongDescription write SetLongDescription;
  { refers to "ref_with_description" JSON property; A description for ref }
    property RefWithDescription: TBaz read FRefWithDescription write SetRefWithDescription;
  { refers to "string_with_description" JSON property; A description for string }
    property StringWithDescription: TJtdString read FStringWithDescription write SetStringWithDescription;
  { refers to "enum_with_description" JSON property; A description for enum }
    property EnumWithDescription: TEnumWithDescription read FEnumWithDescription write SetEnumWithDescription;
  { refers to "properties_with_description" JSON property; A description for properties }
    property PropertiesWithDescription: TPropertiesWithDescription read FPropertiesWithDescription write SetPropertiesWithDescription;
  { refers to "discriminator_with_description" JSON property; A description for 
    discriminator }
    property DiscriminatorWithDescription: TDiscriminatorWithDescription read FDiscriminatorWithDescription write SetDiscriminatorWithDescription;
  end;

implementation

{ TPropertiesWithDescription }

{$PUSH}{$WARN 5057 OFF}
procedure TPropertiesWithDescription.DoReadJson(aNode: TJsonNode);
begin
  if not aNode.IsObject then ExpectObject(aNode);
end;
{$POP}

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

procedure TPropertiesWithDescription.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.EndObject;
end;

procedure TPropertiesWithDescription.Clear;
begin
end;

{ TBar }

{$PUSH}{$WARN 5057 OFF}
procedure TBar.DoReadJson(aNode: TJsonNode);
begin
  if not aNode.IsObject then ExpectObject(aNode);
end;
{$POP}

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

procedure TBar.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.EndObject;
end;

procedure TBar.Clear;
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

{ TRootObject }

procedure TRootObject.SetLongDescription(aValue: TJtdString);
begin
  if aValue = FLongDescription then exit;
  FLongDescription.Free;
  FLongDescription := aValue;
end;

procedure TRootObject.SetRefWithDescription(aValue: TBaz);
begin
  if aValue = FRefWithDescription then exit;
  FRefWithDescription.Free;
  FRefWithDescription := aValue;
end;

procedure TRootObject.SetStringWithDescription(aValue: TJtdString);
begin
  if aValue = FStringWithDescription then exit;
  FStringWithDescription.Free;
  FStringWithDescription := aValue;
end;

procedure TRootObject.SetEnumWithDescription(aValue: TEnumWithDescription);
begin
  if aValue = FEnumWithDescription then exit;
  FEnumWithDescription.Free;
  FEnumWithDescription := aValue;
end;

procedure TRootObject.SetPropertiesWithDescription(aValue: TPropertiesWithDescription);
begin
  if aValue = FPropertiesWithDescription then exit;
  FPropertiesWithDescription.Free;
  FPropertiesWithDescription := aValue;
end;

procedure TRootObject.SetDiscriminatorWithDescription(aValue: TDiscriminatorWithDescription);
begin
  if aValue = FDiscriminatorWithDescription then exit;
  FDiscriminatorWithDescription.Free;
  FDiscriminatorWithDescription := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TRootObject.DoReadJson(aNode: TJsonNode);
var
  Flags: array[0..5] of Boolean;
  e: TJsonNode.TPair;
  I: Integer;
begin
  if not aNode.IsObject then ExpectObject(aNode);
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  for e in aNode.Entries do
    case e.Key of
      'long_description':
        if not Flags[0] then begin
          FLongDescription := TJtdString(TJtdString.LoadInstance(e.Value));
          Flags[0] := True;
        end else DuplicateProp(e.Key);
      'ref_with_description':
        if not Flags[1] then begin
          FRefWithDescription := TBaz(TBaz.LoadInstance(e.Value));
          Flags[1] := True;
        end else DuplicateProp(e.Key);
      'string_with_description':
        if not Flags[2] then begin
          FStringWithDescription := TJtdString(TJtdString.LoadInstance(e.Value));
          Flags[2] := True;
        end else DuplicateProp(e.Key);
      'enum_with_description':
        if not Flags[3] then begin
          FEnumWithDescription := TEnumWithDescription(TEnumWithDescription.LoadInstance(e.Value));
          Flags[3] := True;
        end else DuplicateProp(e.Key);
      'properties_with_description':
        if not Flags[4] then begin
          FPropertiesWithDescription := TPropertiesWithDescription(TPropertiesWithDescription.LoadInstance(e.Value));
          Flags[4] := True;
        end else DuplicateProp(e.Key);
      'discriminator_with_description':
        if not Flags[5] then begin
          FDiscriminatorWithDescription := TDiscriminatorWithDescription(TDiscriminatorWithDescription.LoadInstance(e.Value));
          Flags[5] := True;
        end else DuplicateProp(e.Key);
    else
      UnknownProp(e.Key);
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
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TRootObject.DoReadJson(aReader: TJsonReader);
var
  Flags: array[0..5] of Boolean;
  I: Integer;
begin
  if aReader.TokenKind <> tkObjectBegin then ExpectObject(aReader);
  Clear;
  System.FillChar(Flags, SizeOf(Flags), 0);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
    case aReader.Name of
      'long_description':
        if not Flags[0] then begin
          FLongDescription := TJtdString(TJtdString.LoadInstance(aReader));
          Flags[0] := True;
        end else DuplicateProp(aReader);
      'ref_with_description':
        if not Flags[1] then begin
          FRefWithDescription := TBaz(TBaz.LoadInstance(aReader));
          Flags[1] := True;
        end else DuplicateProp(aReader);
      'string_with_description':
        if not Flags[2] then begin
          FStringWithDescription := TJtdString(TJtdString.LoadInstance(aReader));
          Flags[2] := True;
        end else DuplicateProp(aReader);
      'enum_with_description':
        if not Flags[3] then begin
          FEnumWithDescription := TEnumWithDescription(TEnumWithDescription.LoadInstance(aReader));
          Flags[3] := True;
        end else DuplicateProp(aReader);
      'properties_with_description':
        if not Flags[4] then begin
          FPropertiesWithDescription := TPropertiesWithDescription(TPropertiesWithDescription.LoadInstance(aReader));
          Flags[4] := True;
        end else DuplicateProp(aReader);
      'discriminator_with_description':
        if not Flags[5] then begin
          FDiscriminatorWithDescription := TDiscriminatorWithDescription(TDiscriminatorWithDescription.LoadInstance(aReader));
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

procedure TRootObject.DoWriteJson(aWriter: TJsonStrWriter);
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

procedure TRootObject.Clear;
begin
  FreeAndNil(FLongDescription);
  FreeAndNil(FRefWithDescription);
  FreeAndNil(FStringWithDescription);
  FreeAndNil(FEnumWithDescription);
  FreeAndNil(FPropertiesWithDescription);
  FreeAndNil(FDiscriminatorWithDescription);
end;

end.
