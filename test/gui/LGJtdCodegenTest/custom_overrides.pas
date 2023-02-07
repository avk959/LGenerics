{
  Source schema: custom_overrides.jtd.json

  This unit was automatically created by JtdPasCodegen, do not edit.
}
unit custom_overrides;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TOverrideElementsContainer = class sealed(specialize TJtdList<TJtdString>)
    class function GetJtdClass: TJtdEntityClass; override;
  end;

  TOverrideValuesContainer = class sealed(specialize TJtdMap<TJtdString>)
    class function GetJtdClass: TJtdEntityClass; override;
  end;

  TOverrideTypeEnum = (FOO, BAR);

{ Container for some TOverrideTypeEnum enumeration element }
  TOverrideTypeEnumElem = class sealed(specialize TJtdEnum<TOverrideTypeEnum>)
    class function GetJtdClass: TJtdEntityClass; override;
  end;

  TOverrideTypeProperties = class sealed(TJtdObject)
  private
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
  end;

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

  TBaz = class sealed(TJtdObject)
  private
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
  end;

  TOverrideTypeDiscriminator = class sealed(TJtdVariant)
  protected
    function GetBar: TBar;
    function GetBaz: TBaz;
    procedure SetBar(aValue: TBar);
    procedure SetBaz(aValue: TBaz);
    class function GetTagJsonName: string; override;
    class function ValidTagValue(const aValue: string): Boolean; override;
    class function GetInstanceClass(const aTag: string): TJtdEntityClass; override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
  { matches the "bar" tag }
    property Bar: TBar read GetBar write SetBar;
  { matches the "baz" tag }
    property Baz: TBaz read GetBaz write SetBaz;
  end;

  TCustomOverrides = class sealed(TJtdObject)
  private
    FOverrideTypeExpr: TJtdString;
    FOverrideElementsContainer: TOverrideElementsContainer;
    FOverrideValuesContainer: TOverrideValuesContainer;
    FOverrideTypeEnum: TOverrideTypeEnumElem;
    FOverrideTypeProperties: TOverrideTypeProperties;
    FOverrideTypeDiscriminator: TOverrideTypeDiscriminator;
    procedure SetOverrideTypeExpr(aValue: TJtdString);
    procedure SetOverrideElementsContainer(aValue: TOverrideElementsContainer);
    procedure SetOverrideValuesContainer(aValue: TOverrideValuesContainer);
    procedure SetOverrideTypeEnum(aValue: TOverrideTypeEnumElem);
    procedure SetOverrideTypeProperties(aValue: TOverrideTypeProperties);
    procedure SetOverrideTypeDiscriminator(aValue: TOverrideTypeDiscriminator);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
    procedure Clear; override;
  { refers to "override_type_expr" JSON property }
    property OverrideTypeExpr: TJtdString read FOverrideTypeExpr write SetOverrideTypeExpr;
  { refers to "override_elements_container" JSON property }
    property OverrideElementsContainer: TOverrideElementsContainer read FOverrideElementsContainer write SetOverrideElementsContainer;
  { refers to "override_values_container" JSON property }
    property OverrideValuesContainer: TOverrideValuesContainer read FOverrideValuesContainer write SetOverrideValuesContainer;
  { refers to "override_type_enum" JSON property }
    property OverrideTypeEnum: TOverrideTypeEnumElem read FOverrideTypeEnum write SetOverrideTypeEnum;
  { refers to "override_type_properties" JSON property }
    property OverrideTypeProperties: TOverrideTypeProperties read FOverrideTypeProperties write SetOverrideTypeProperties;
  { refers to "override_type_discriminator" JSON property }
    property OverrideTypeDiscriminator: TOverrideTypeDiscriminator read FOverrideTypeDiscriminator write SetOverrideTypeDiscriminator;
  end;

implementation

{ TOverrideElementsContainer }

class function TOverrideElementsContainer.GetJtdClass: TJtdEntityClass;
begin
  Result := TOverrideElementsContainer;
end;

{ TOverrideValuesContainer }

class function TOverrideValuesContainer.GetJtdClass: TJtdEntityClass;
begin
  Result := TOverrideValuesContainer;
end;

{ TOverrideTypeEnumElem }

class function TOverrideTypeEnumElem.GetJtdClass: TJtdEntityClass;
begin
  Result := TOverrideTypeEnumElem;
end;

{ TOverrideTypeProperties }

class function TOverrideTypeProperties.GetJtdClass: TJtdEntityClass;
begin
  Result := TOverrideTypeProperties;
end;

procedure TOverrideTypeProperties.Clear;
begin
end;

{$PUSH}{$WARN 5057 OFF}
procedure TOverrideTypeProperties.DoReadJson(aNode: TJsonNode);
begin
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TOverrideTypeProperties.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> tkObjectBegin then ReadError;
  Clear;
  repeat
    if not aReader.Read then ReadError;
    if aReader.TokenKind = tkObjectEnd then break;
  until False;
end;
{$POP}

procedure TOverrideTypeProperties.DoWriteJson(aWriter: TJsonStrWriter);
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

{ TBaz }

class function TBaz.GetJtdClass: TJtdEntityClass;
begin
  Result := TBaz;
end;

procedure TBaz.Clear;
begin
end;

{$PUSH}{$WARN 5057 OFF}
procedure TBaz.DoReadJson(aNode: TJsonNode);
begin
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TBaz.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> tkObjectBegin then ReadError;
  Clear;
  repeat
    if not aReader.Read then ReadError;
    if aReader.TokenKind = tkObjectEnd then break;
  until False;
end;
{$POP}

procedure TBaz.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.EndObject;
end;

{ TOverrideTypeDiscriminator }

class function TOverrideTypeDiscriminator.GetJtdClass: TJtdEntityClass;
begin
  Result := TOverrideTypeDiscriminator;
end;

class function TOverrideTypeDiscriminator.GetTagJsonName: string;
begin
  Result := 'foo';
end;

class function TOverrideTypeDiscriminator.ValidTagValue(const aValue: string): Boolean;
begin
  case aValue of
    'bar',
    'baz': Result := True;
  else
    Result := False;
  end;
end;

class function TOverrideTypeDiscriminator.GetInstanceClass(const aTag: string): TJtdEntityClass;
begin
  case aTag of
    'bar': Result := TBar;
    'baz': Result := TBaz;
  else
    Result := nil;
  end;
end;

function TOverrideTypeDiscriminator.GetBar: TBar;
begin
  Result := FInstance as TBar;
end;

function TOverrideTypeDiscriminator.GetBaz: TBaz;
begin
  Result := FInstance as TBaz;
end;

procedure TOverrideTypeDiscriminator.SetBar(aValue: TBar);
begin
  if aValue = FInstance then exit;
  FInstance.Free;
  FInstance := aValue;
  FTag := 'bar';
end;

procedure TOverrideTypeDiscriminator.SetBaz(aValue: TBaz);
begin
  if aValue = FInstance then exit;
  FInstance.Free;
  FInstance := aValue;
  FTag := 'baz';
end;

{ TCustomOverrides }

class function TCustomOverrides.GetJtdClass: TJtdEntityClass;
begin
  Result := TCustomOverrides;
end;

procedure TCustomOverrides.Clear;
begin
  FreeAndNil(FOverrideTypeExpr);
  FreeAndNil(FOverrideElementsContainer);
  FreeAndNil(FOverrideValuesContainer);
  FreeAndNil(FOverrideTypeEnum);
  FreeAndNil(FOverrideTypeProperties);
  FreeAndNil(FOverrideTypeDiscriminator);
end;

procedure TCustomOverrides.SetOverrideTypeExpr(aValue: TJtdString);
begin
  if aValue = FOverrideTypeExpr then exit;
  FOverrideTypeExpr.Free;
  FOverrideTypeExpr := aValue;
end;

procedure TCustomOverrides.SetOverrideElementsContainer(aValue: TOverrideElementsContainer);
begin
  if aValue = FOverrideElementsContainer then exit;
  FOverrideElementsContainer.Free;
  FOverrideElementsContainer := aValue;
end;

procedure TCustomOverrides.SetOverrideValuesContainer(aValue: TOverrideValuesContainer);
begin
  if aValue = FOverrideValuesContainer then exit;
  FOverrideValuesContainer.Free;
  FOverrideValuesContainer := aValue;
end;

procedure TCustomOverrides.SetOverrideTypeEnum(aValue: TOverrideTypeEnumElem);
begin
  if aValue = FOverrideTypeEnum then exit;
  FOverrideTypeEnum.Free;
  FOverrideTypeEnum := aValue;
end;

procedure TCustomOverrides.SetOverrideTypeProperties(aValue: TOverrideTypeProperties);
begin
  if aValue = FOverrideTypeProperties then exit;
  FOverrideTypeProperties.Free;
  FOverrideTypeProperties := aValue;
end;

procedure TCustomOverrides.SetOverrideTypeDiscriminator(aValue: TOverrideTypeDiscriminator);
begin
  if aValue = FOverrideTypeDiscriminator then exit;
  FOverrideTypeDiscriminator.Free;
  FOverrideTypeDiscriminator := aValue;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TCustomOverrides.DoReadJson(aNode: TJsonNode);
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
      'override_type_expr':
        begin
          FOverrideTypeExpr := TJtdString(TJtdString.ReadJson(p.Value));
          Flags[0] := True;
        end;
      'override_elements_container':
        begin
          FOverrideElementsContainer := TOverrideElementsContainer(TOverrideElementsContainer.ReadJson(p.Value));
          Flags[1] := True;
        end;
      'override_values_container':
        begin
          FOverrideValuesContainer := TOverrideValuesContainer(TOverrideValuesContainer.ReadJson(p.Value));
          Flags[2] := True;
        end;
      'override_type_enum':
        begin
          FOverrideTypeEnum := TOverrideTypeEnumElem(TOverrideTypeEnumElem.ReadJson(p.Value));
          Flags[3] := True;
        end;
      'override_type_properties':
        begin
          FOverrideTypeProperties := TOverrideTypeProperties(TOverrideTypeProperties.ReadJson(p.Value));
          Flags[4] := True;
        end;
      'override_type_discriminator':
        begin
          FOverrideTypeDiscriminator := TOverrideTypeDiscriminator(TOverrideTypeDiscriminator.ReadJson(p.Value));
          Flags[5] := True;
        end;
    else
      UnknownProp(p.Key);
    end;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('override_type_expr');
        1: PropNotFound('override_elements_container');
        2: PropNotFound('override_values_container');
        3: PropNotFound('override_type_enum');
        4: PropNotFound('override_type_properties');
        5: PropNotFound('override_type_discriminator');
      else
      end;
end;
{$POP}

{$PUSH}{$WARN 5057 OFF}
procedure TCustomOverrides.DoReadJson(aReader: TJsonReader);
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
      'override_type_expr':
        begin
          FOverrideTypeExpr := TJtdString(TJtdString.ReadJson(aReader));
          Flags[0] := True;
        end;
      'override_elements_container':
        begin
          FOverrideElementsContainer := TOverrideElementsContainer(TOverrideElementsContainer.ReadJson(aReader));
          Flags[1] := True;
        end;
      'override_values_container':
        begin
          FOverrideValuesContainer := TOverrideValuesContainer(TOverrideValuesContainer.ReadJson(aReader));
          Flags[2] := True;
        end;
      'override_type_enum':
        begin
          FOverrideTypeEnum := TOverrideTypeEnumElem(TOverrideTypeEnumElem.ReadJson(aReader));
          Flags[3] := True;
        end;
      'override_type_properties':
        begin
          FOverrideTypeProperties := TOverrideTypeProperties(TOverrideTypeProperties.ReadJson(aReader));
          Flags[4] := True;
        end;
      'override_type_discriminator':
        begin
          FOverrideTypeDiscriminator := TOverrideTypeDiscriminator(TOverrideTypeDiscriminator.ReadJson(aReader));
          Flags[5] := True;
        end;
    else
      UnknownProp(aReader.Name);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('override_type_expr');
        1: PropNotFound('override_elements_container');
        2: PropNotFound('override_values_container');
        3: PropNotFound('override_type_enum');
        4: PropNotFound('override_type_properties');
        5: PropNotFound('override_type_discriminator');
      else
      end;
end;
{$POP}

procedure TCustomOverrides.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.AddName('override_type_expr');
  OverrideTypeExpr.WriteJson(aWriter);
  aWriter.AddName('override_elements_container');
  OverrideElementsContainer.WriteJson(aWriter);
  aWriter.AddName('override_values_container');
  OverrideValuesContainer.WriteJson(aWriter);
  aWriter.AddName('override_type_enum');
  OverrideTypeEnum.WriteJson(aWriter);
  aWriter.AddName('override_type_properties');
  OverrideTypeProperties.WriteJson(aWriter);
  aWriter.AddName('override_type_discriminator');
  OverrideTypeDiscriminator.WriteJson(aWriter);
  aWriter.EndObject;
end;

end.
