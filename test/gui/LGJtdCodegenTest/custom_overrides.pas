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

  TOverrideTypeEnumType = (FOO, BAR);

{ Container for some TOverrideTypeEnumType enumeration element }
  TOverrideTypeEnum = class sealed(specialize TJtdEnum<TOverrideTypeEnumType>);

  TOverrideTypeProperties = class sealed(TJtdObject)
  private
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    procedure Clear; override;
  end;

  TBar = class sealed(TJtdObject)
  private
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    procedure Clear; override;
  end;

  TBaz = class sealed(TJtdObject)
  private
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    procedure Clear; override;
  end;

  TOverrideTypeDiscriminator = class sealed(TJtdUnion)
  protected
    function  GetBar: TBar;
    function  GetBaz: TBaz;
    procedure SetBar(aValue: TBar);
    procedure SetBaz(aValue: TBaz);
    class function GetTagJsonName: string; override;
    class function GetInstanceClass(const aTag: string): TJtdEntityClass; override;
  public
  { matches the "bar" tag; }
    property Bar: TBar read GetBar write SetBar;
  { matches the "baz" tag; }
    property Baz: TBaz read GetBaz write SetBaz;
  end;

  TRootObject = class sealed(TJtdObject)
  private
    FOverrideTypeExpr: TJtdString;
    FOverrideElementsContainer: specialize TJtdList<TJtdString>;
    FOverrideValuesContainer: specialize TJtdMap<TJtdString>;
    FOverrideTypeEnum: TOverrideTypeEnum;
    FOverrideTypeProperties: TOverrideTypeProperties;
    FOverrideTypeDiscriminator: TOverrideTypeDiscriminator;
    procedure SetOverrideTypeExpr(aValue: TJtdString);
    procedure SetOverrideElementsContainer(aValue: specialize TJtdList<TJtdString>);
    procedure SetOverrideValuesContainer(aValue: specialize TJtdMap<TJtdString>);
    procedure SetOverrideTypeEnum(aValue: TOverrideTypeEnum);
    procedure SetOverrideTypeProperties(aValue: TOverrideTypeProperties);
    procedure SetOverrideTypeDiscriminator(aValue: TOverrideTypeDiscriminator);
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    procedure Clear; override;
  { refers to "override_type_expr" JSON property }
    property OverrideTypeExpr: TJtdString read FOverrideTypeExpr write SetOverrideTypeExpr;
  { refers to "override_elements_container" JSON property }
    property OverrideElementsContainer: specialize TJtdList<TJtdString> read FOverrideElementsContainer write SetOverrideElementsContainer;
  { refers to "override_values_container" JSON property }
    property OverrideValuesContainer: specialize TJtdMap<TJtdString> read FOverrideValuesContainer write SetOverrideValuesContainer;
  { refers to "override_type_enum" JSON property }
    property OverrideTypeEnum: TOverrideTypeEnum read FOverrideTypeEnum write SetOverrideTypeEnum;
  { refers to "override_type_properties" JSON property }
    property OverrideTypeProperties: TOverrideTypeProperties read FOverrideTypeProperties write SetOverrideTypeProperties;
  { refers to "override_type_discriminator" JSON property }
    property OverrideTypeDiscriminator: TOverrideTypeDiscriminator read FOverrideTypeDiscriminator write SetOverrideTypeDiscriminator;
  end;

implementation

{ TOverrideTypeProperties }

{$PUSH}{$WARN 5057 OFF}
procedure TOverrideTypeProperties.DoReadJson(aNode: TJsonNode);
begin
  if not aNode.IsObject then ExpectObject(aNode);
end;
{$POP}

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

procedure TOverrideTypeProperties.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.EndObject;
end;

procedure TOverrideTypeProperties.Clear;
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

{ TBaz }

{$PUSH}{$WARN 5057 OFF}
procedure TBaz.DoReadJson(aNode: TJsonNode);
begin
  if not aNode.IsObject then ExpectObject(aNode);
end;
{$POP}

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

procedure TBaz.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.EndObject;
end;

procedure TBaz.Clear;
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

{ TRootObject }

procedure TRootObject.SetOverrideTypeExpr(aValue: TJtdString);
begin
  if aValue = FOverrideTypeExpr then exit;
  FOverrideTypeExpr.Free;
  FOverrideTypeExpr := aValue;
end;

procedure TRootObject.SetOverrideElementsContainer(aValue: specialize TJtdList<TJtdString>);
begin
  if aValue = FOverrideElementsContainer then exit;
  FOverrideElementsContainer.Free;
  FOverrideElementsContainer := aValue;
end;

procedure TRootObject.SetOverrideValuesContainer(aValue: specialize TJtdMap<TJtdString>);
begin
  if aValue = FOverrideValuesContainer then exit;
  FOverrideValuesContainer.Free;
  FOverrideValuesContainer := aValue;
end;

procedure TRootObject.SetOverrideTypeEnum(aValue: TOverrideTypeEnum);
begin
  if aValue = FOverrideTypeEnum then exit;
  FOverrideTypeEnum.Free;
  FOverrideTypeEnum := aValue;
end;

procedure TRootObject.SetOverrideTypeProperties(aValue: TOverrideTypeProperties);
begin
  if aValue = FOverrideTypeProperties then exit;
  FOverrideTypeProperties.Free;
  FOverrideTypeProperties := aValue;
end;

procedure TRootObject.SetOverrideTypeDiscriminator(aValue: TOverrideTypeDiscriminator);
begin
  if aValue = FOverrideTypeDiscriminator then exit;
  FOverrideTypeDiscriminator.Free;
  FOverrideTypeDiscriminator := aValue;
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
      'override_type_expr':
        if not Flags[0] then begin
          FOverrideTypeExpr := TJtdString(TJtdString.LoadInstance(e.Value));
          Flags[0] := True;
        end else DuplicateProp(e.Key);
      'override_elements_container':
        if not Flags[1] then begin
          FOverrideElementsContainer := specialize TJtdList<TJtdString>(specialize TJtdList<TJtdString>.LoadInstance(e.Value));
          Flags[1] := True;
        end else DuplicateProp(e.Key);
      'override_values_container':
        if not Flags[2] then begin
          FOverrideValuesContainer := specialize TJtdMap<TJtdString>(specialize TJtdMap<TJtdString>.LoadInstance(e.Value));
          Flags[2] := True;
        end else DuplicateProp(e.Key);
      'override_type_enum':
        if not Flags[3] then begin
          FOverrideTypeEnum := TOverrideTypeEnum(TOverrideTypeEnum.LoadInstance(e.Value));
          Flags[3] := True;
        end else DuplicateProp(e.Key);
      'override_type_properties':
        if not Flags[4] then begin
          FOverrideTypeProperties := TOverrideTypeProperties(TOverrideTypeProperties.LoadInstance(e.Value));
          Flags[4] := True;
        end else DuplicateProp(e.Key);
      'override_type_discriminator':
        if not Flags[5] then begin
          FOverrideTypeDiscriminator := TOverrideTypeDiscriminator(TOverrideTypeDiscriminator.LoadInstance(e.Value));
          Flags[5] := True;
        end else DuplicateProp(e.Key);
    else
      UnknownProp(e.Key);
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
      'override_type_expr':
        if not Flags[0] then begin
          FOverrideTypeExpr := TJtdString(TJtdString.LoadInstance(aReader));
          Flags[0] := True;
        end else DuplicateProp(aReader);
      'override_elements_container':
        if not Flags[1] then begin
          FOverrideElementsContainer := specialize TJtdList<TJtdString>(specialize TJtdList<TJtdString>.LoadInstance(aReader));
          Flags[1] := True;
        end else DuplicateProp(aReader);
      'override_values_container':
        if not Flags[2] then begin
          FOverrideValuesContainer := specialize TJtdMap<TJtdString>(specialize TJtdMap<TJtdString>.LoadInstance(aReader));
          Flags[2] := True;
        end else DuplicateProp(aReader);
      'override_type_enum':
        if not Flags[3] then begin
          FOverrideTypeEnum := TOverrideTypeEnum(TOverrideTypeEnum.LoadInstance(aReader));
          Flags[3] := True;
        end else DuplicateProp(aReader);
      'override_type_properties':
        if not Flags[4] then begin
          FOverrideTypeProperties := TOverrideTypeProperties(TOverrideTypeProperties.LoadInstance(aReader));
          Flags[4] := True;
        end else DuplicateProp(aReader);
      'override_type_discriminator':
        if not Flags[5] then begin
          FOverrideTypeDiscriminator := TOverrideTypeDiscriminator(TOverrideTypeDiscriminator.LoadInstance(aReader));
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

procedure TRootObject.DoWriteJson(aWriter: TJsonStrWriter);
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

procedure TRootObject.Clear;
begin
  FreeAndNil(FOverrideTypeExpr);
  FreeAndNil(FOverrideElementsContainer);
  FreeAndNil(FOverrideValuesContainer);
  FreeAndNil(FOverrideTypeEnum);
  FreeAndNil(FOverrideTypeProperties);
  FreeAndNil(FOverrideTypeDiscriminator);
end;

end.
