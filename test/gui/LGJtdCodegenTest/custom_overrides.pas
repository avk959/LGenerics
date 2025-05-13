{
  Source schema: custom_overrides.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit custom_overrides;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TOverrideElementsContainer = class sealed(specialize TJtdList<TJtdString>);

  TOverrideValuesContainer = class sealed(specialize TJtdMap<TJtdString>);

  TOverrideTypeEnum = (FOO, BAR);

{ Container for some TOverrideTypeEnum enumeration element }
  TOverrideTypeEnumElem = class sealed(specialize TJtdEnum<TOverrideTypeEnum>);

  TOverrideTypeProperties = class sealed(TJtdObject)
  private
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
    procedure ClearFields; override;
  public
  end;

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

  TBaz = class sealed(TJtdObject)
  private
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
    procedure ClearFields; override;
  public
  end;

  TOverrideTypeDiscriminator = class sealed(TJtdUnion)
  protected
    function GetBar: TBar;
    function GetBaz: TBaz;
    procedure SetBar(aValue: TBar);
    procedure SetBaz(aValue: TBaz);
    class function GetTagJsonName: string; override;
    class function GetInstanceClass(const aTag: string): TJtdEntityClass; override;
  public
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
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure WriteFields(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    procedure CreateFields; override;
    procedure ClearFields; override;
  public
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

{ TOverrideTypeProperties }

{$PUSH}{$WARN 5057 OFF}
procedure TOverrideTypeProperties.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> tkObjectBegin then ExpectObject(aReader);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
  until False;
end;
{$POP}

procedure TOverrideTypeProperties.WriteFields(aWriter: TJsonStrWriter);
begin
end;

procedure TOverrideTypeProperties.DoClear;
begin
end;

procedure TOverrideTypeProperties.ClearFields;
begin
end;

procedure TOverrideTypeProperties.CreateFields;
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

{ TBaz }

{$PUSH}{$WARN 5057 OFF}
procedure TBaz.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> tkObjectBegin then ExpectObject(aReader);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
  until False;
end;
{$POP}

procedure TBaz.WriteFields(aWriter: TJsonStrWriter);
begin
end;

procedure TBaz.DoClear;
begin
end;

procedure TBaz.ClearFields;
begin
end;

procedure TBaz.CreateFields;
begin
end;

class function TOverrideTypeDiscriminator.GetTagJsonName: string;
begin
  Result := 'foo';
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
procedure TCustomOverrides.DoReadJson(aReader: TJsonReader);
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
      'override_type_expr':
        if not Flags[0] then begin
          FOverrideTypeExpr.ReadJson(aReader);
          Flags[0] := True;
        end else DuplicateProp(aReader);
      'override_elements_container':
        if not Flags[1] then begin
          FOverrideElementsContainer.ReadJson(aReader);
          Flags[1] := True;
        end else DuplicateProp(aReader);
      'override_values_container':
        if not Flags[2] then begin
          FOverrideValuesContainer.ReadJson(aReader);
          Flags[2] := True;
        end else DuplicateProp(aReader);
      'override_type_enum':
        if not Flags[3] then begin
          FOverrideTypeEnum.ReadJson(aReader);
          Flags[3] := True;
        end else DuplicateProp(aReader);
      'override_type_properties':
        if not Flags[4] then begin
          FOverrideTypeProperties.ReadJson(aReader);
          Flags[4] := True;
        end else DuplicateProp(aReader);
      'override_type_discriminator':
        if not Flags[5] then begin
          FOverrideTypeDiscriminator.ReadJson(aReader);
          Flags[5] := True;
        end else DuplicateProp(aReader);
    else
      UnknownProp(aReader.Name, aReader);
    end;
  until False;
  for I := 0 to System.High(Flags) do
    if not Flags[I] then
      case I of
        0: PropNotFound('override_type_expr', aReader);
        1: PropNotFound('override_elements_container', aReader);
        2: PropNotFound('override_values_container', aReader);
        3: PropNotFound('override_type_enum', aReader);
        4: PropNotFound('override_type_properties', aReader);
        5: PropNotFound('override_type_discriminator', aReader);
      end;
end;
{$POP}

procedure TCustomOverrides.WriteFields(aWriter: TJsonStrWriter);
begin
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
end;

procedure TCustomOverrides.DoClear;
begin
end;

procedure TCustomOverrides.ClearFields;
begin
  FOverrideTypeExpr.Free;
  FOverrideElementsContainer.Free;
  FOverrideValuesContainer.Free;
  FOverrideTypeEnum.Free;
  FOverrideTypeProperties.Free;
  FOverrideTypeDiscriminator.Free;
end;

procedure TCustomOverrides.CreateFields;
begin
  FOverrideTypeExpr := TJtdString.Create;
  FOverrideElementsContainer := TOverrideElementsContainer.Create;
  FOverrideValuesContainer := TOverrideValuesContainer.Create;
  FOverrideTypeEnum := TOverrideTypeEnumElem.Create;
  FOverrideTypeProperties := TOverrideTypeProperties.Create;
  FOverrideTypeDiscriminator := TOverrideTypeDiscriminator.Create;
end;

end.
