unit sedAppConf;

{$MODE OBJFPC}{$H+}{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Graphics, SynEditHighlighter, lgJsonCfg;

type

  { TAttriStyle }
  TAttriStyle = record
    Foreground: TColor;
    Bold,
    Italic: Boolean;
    constructor Make(aForeground: TColor);
    constructor Make(Attri: TSynHighlighterAttributes);
  end;

  { TDisplayStyle }
  TDisplayStyle = record
    SynFontName: string;
    SynFontSize,
    SynExtraLineSpace,
    SynExtraCharSpace,
    SynRightEdge,
    SynShowEveryNthNumber: Integer;
    SynLineNumberColor,
    SynCurrLineBackground,
    SynErrorLineBackground: TColor;
    SynKeyAttri,
    SynIdentAttri,
    SynReservedAttri,
    SynAttributeAttri,
    SynSymbolAttri,
    SynCustomKeyAttri: TAttriStyle;
    class function Default: TDisplayStyle; static;
  end;

  { TMiscSettings }
  TMiscSettings = record
    MaxRecentFiles: Integer;
    SuccessColor: TColor;
    SaveFormatted,
    AllowBom: Boolean;
    class function Default: TMiscSettings; static;
  end;

  { TAppConf }
  TAppConf = class
  private
    FConf: TJsonConf;
    function  GetDisplayStyle: TDisplayStyle;
    function  GetJsonFormatStyle: TJsonFormatStyle;
    function  GetMiscSettings: TMiscSettings;
    procedure SetDisplayStyle(const aValue: TDisplayStyle);
    procedure SetJsonFormatStyle(const aValue: TJsonFormatStyle);
    procedure SetMiscSettings(const aValue: TMiscSettings);
    function  GetJsonFormatKey: string;
    function  GetSettingsKey: string;
    function  GetMaxRecentKey: string;
    function  GetAppearanceKey: string;
    function  GetSynEditKey: string;
    function  GetHighlightKey: string;
    function  GetMiscKey: string;
  public
    constructor Create;
    destructor Destroy; override;
    function  GetComponentKey: string;
    function  GetRecentFilesKey: string;
    property  JsonFormatStyle: TJsonFormatStyle read GetJsonFormatStyle write SetJsonFormatStyle;
    property  DisplayStyle: TDisplayStyle read GetDisplayStyle write SetDisplayStyle;
    property  MiscSettings: TMiscSettings read GetMiscSettings write SetMiscSettings;
    property  Conf: TJsonConf read FConf;
  end;

const
  TOP_KEY              = '/top';
  LEFT_KEY             = '/left';
  WIDTH_KEY            = '/width';
  HEIGHT_KEY           = '/height';
  WND_STATE_KEY        = '/windowState';

  function GetAppConf: TAppConf;
  function GetDefaultFontName: string;
  function GetJtdDocUrl: string;
  function GetJtdHomeUrl: string;

implementation
{$B-}{$J-}{$COPERATORS ON}{$POINTERMATH ON}
uses
  SynHighlighterJTD, FileUtil, LazFileUtils, lgJson;

var
  AppConf: TAppConf = nil;

const
  JSON_FORMAT_KEY      = '/jsonFormat';
  FMT_OPTS_KEY         = '/options';
  INDENT_SIZE_KEY      = '/indentSize';
  INDENT_AFTER_COMMA   = '/indentAfterComma';
  INDENT_BEFORE_COLON  = '/indentBeforeColon';
  INDENT_AFTER_COLON   = '/IndentAfterColon';
  ONELINE_ARRAY_BOUND  = '/oneLineArrayBound';
  ONELINE_OBJECT_BOUND = '/OneLineObjectBound';
  LINE_BREAK_KEY       = '/lineBreak';
  COMPONENTS_KEY       = '/components';
  SETTINGS_KEY         = '/settings';
  APPIARANCE_KEY       = '/appearance';
  SYN_EDIT_KEY         = '/synEdit';
  HIGHLIGHT_KEY        = '/highlight';
  FONT_KEY             = '/font';
  NAME_KEY             = '/name';
  SIZE_KEY             = '/size';
  STYLE_KEY            = '/style';
  FOREGROUND_KEY       = '/foreground';
  BOLD_KEY             = '/bold';
  ITALIC_KEY           = '/italic';
  EXTRA_LINE_SPACE     = '/extraLineSpacing';
  EXTRA_CHAR_SPACE     = '/extraCharSpacing';
  RIGHT_EDGE_KEY       = '/rightEdge';
  NTH_NUMBER_KEY       = '/showNthLineNumber';
  LINE_NUM_COLOR_KEY   = '/lineNumberColor';
  CURR_LINE_COLOR_KEY  = '/currentLineColor';
  ERROR_LINE_COLOR_KEY = '/errorLineColor';
  SUCCESS_COLOR_KEY    = '/successColor';
  MISC_KEY             = '/miscellaneous';
  FORMATTED_KEY        = '/formatted';
  ALLOW_BOM_KEY        = '/allowBom';
  MAX_RECENT_KEY       = '/maxRecentFiles';
  RECENT_FILES_KEY     = '/recentFiles';

function GetAppConfigFileName: string;
begin
  Result := GetAppConfigFileUTF8(False, False, True);
end;

function GetAppConf: TAppConf;
begin
  if AppConf = nil then
    begin
      AppConf := TAppConf.Create;
      AppConf.FConf.FileName := GetAppConfigFileName;
      AppConf.FConf.Formatted := AppConf.MiscSettings.SaveFormatted;
    end;
  Result := AppConf;
end;

function GetDefaultFontName: string;
begin
  Result := 'Courier New';
end;

function GetJtdDocUrl: string;
begin
  Result := 'https://jsontypedef.com/docs/';
end;

function GetJtdHomeUrl: string;
begin
  Result := 'https://jsontypedef.com/';
end;

{ TMiscSettings }

class function TMiscSettings.Default: TMiscSettings;
begin
  Result.MaxRecentFiles := 5;
  Result.SuccessColor := $00E4FFCA;
  Result.SaveFormatted := False;
  Result.AllowBom := False;
end;

{ TDisplayStyle }

class function TDisplayStyle.Default: TDisplayStyle;
begin
  Result.SynFontName := GetDefaultFontName;
  Result.SynFontSize := 9;
  Result.SynExtraLineSpace := 0;
  Result.SynExtraCharSpace := 0;
  Result.SynRightEdge := 80;
  Result.SynShowEveryNthNumber := 5;
  Result.SynLineNumberColor := $00FF3535;
  Result.SynCurrLineBackground := $00CEF8F9;
  Result.SynErrorLineBackground := $00AABBF7;
  Result.SynKeyAttri := TAttriStyle.Make(TSynJTDSyn.KEYWORD_DEF_COLOR);
  Result.SynIdentAttri := TAttriStyle.Make(TSynJTDSyn.IDENTIFIER_DEF_COLOR);
  Result.SynReservedAttri := TAttriStyle.Make(TSynJTDSyn.RESERVED_DEF_COLOR);
  Result.SynAttributeAttri := TAttriStyle.Make(TSynJTDSyn.ATTRIBUTE_DEF_COLOR);
  Result.SynSymbolAttri := TAttriStyle.Make(TSynJTDSyn.SYMBOL_DEF_COLOR);
  Result.SynCustomKeyAttri := TAttriStyle.Make(TSynJTDSyn.CUSTOM_KEY_DEF_COLOR);
end;

{ TAttriConf }

constructor TAttriStyle.Make(aForeground: TColor);
begin
  Foreground := aForeground;
  Bold := False;
  Italic := False;
end;

constructor TAttriStyle.Make(Attri: TSynHighlighterAttributes);
begin
  Foreground := Attri.Foreground;
  Bold := fsBold in Attri.Style;
  Italic := fsItalic in Attri.Style;
end;

{ TAppConf }

function TAppConf.GetJsonFormatStyle: TJsonFormatStyle;
var
  FmtKey: string;
  Opt: TJsFormatOption;
  Opts: TJsFormatOptions;
  s: string;
begin
  Result := DefaultJsonFmtStyle;
  FmtKey := GetJsonFormatKey;
  if not FConf.TryOpenKey(FmtKey, False) then
    exit
  else
    FConf.CloseKey;

  Opts := [];
  for Opt in TJsFormatOption do
    begin
      Str(Opt, s);
      s := '/' + s;
      if FConf.GetValue(FmtKey + FMT_OPTS_KEY + s, False) then
        Include(Opts, Opt);
    end;

  Result.Options := Opts;
  Result.IndentSize := FConf.GetValue(FmtKey + INDENT_SIZE_KEY, Result.IndentSize);
  Result.IndentAfterComma := FConf.GetValue(FmtKey + INDENT_AFTER_COMMA, Result.IndentAfterComma);
  Result.IndentBeforeColon := FConf.GetValue(FmtKey + INDENT_BEFORE_COLON, Result.IndentBeforeColon);
  Result.IndentAfterColon := FConf.GetValue(FmtKey + INDENT_AFTER_COLON, Result.IndentAfterColon);
  Result.OneLineArrayBound := FConf.GetValue(FmtKey + ONELINE_ARRAY_BOUND, Result.OneLineArrayBound);
  Result.OneLineObjectBound := FConf.GetValue(FmtKey + ONELINE_OBJECT_BOUND, Result.OneLineObjectBound);
  Result.LineBreak := TJsLineBreak(FConf.GetValue(FmtKey + LINE_BREAK_KEY, Integer(Result.LineBreak)));
end;

function TAppConf.GetMiscSettings: TMiscSettings;
begin
  Result := TMiscSettings.Default;
  Result.MaxRecentFiles := FConf.GetValue(MISC_KEY + MAX_RECENT_KEY, Result.MaxRecentFiles);
  Result.SuccessColor := TColor(FConf.GetValue(MISC_KEY + SUCCESS_COLOR_KEY, Integer(Result.SuccessColor)));
  Result.SaveFormatted := FConf.GetValue(MISC_KEY + FORMATTED_KEY, Result.SaveFormatted);
  Result.AllowBom := FConf.GetValue(MISC_KEY + ALLOW_BOM_KEY, Result.AllowBom);
  FConf.Formatted := Result.SaveFormatted;
end;

function TAppConf.GetDisplayStyle: TDisplayStyle;
var
  SynEditKey, FontKey, HlKey, CurrKey: string;
begin
  Result := TDisplayStyle.Default;
  SynEditKey := GetSynEditKey;
  FontKey := SynEditKey + FONT_KEY;

  Result.SynFontName := FConf.GetValue(FontKey + NAME_KEY, Result.SynFontName);
  Result.SynFontSize := FConf.GetValue(FontKey + SIZE_KEY, Result.SynFontSize);

  Result.SynExtraLineSpace := FConf.GetValue(SynEditKey + EXTRA_LINE_SPACE, Result.SynExtraLineSpace);
  Result.SynExtraCharSpace := FConf.GetValue(SynEditKey + EXTRA_CHAR_SPACE, Result.SynExtraCharSpace);
  Result.SynRightEdge := FConf.GetValue(SynEditKey + RIGHT_EDGE_KEY, Result.SynRightEdge);
  Result.SynShowEveryNthNumber := FConf.GetValue(SynEditKey + NTH_NUMBER_KEY, Result.SynShowEveryNthNumber);
  Result.SynLineNumberColor := TColor(FConf.GetValue(SynEditKey + LINE_NUM_COLOR_KEY, Integer(Result.SynLineNumberColor)));
  Result.SynCurrLineBackground := TColor(FConf.GetValue(SynEditKey + CURR_LINE_COLOR_KEY, Integer(Result.SynCurrLineBackground)));
  Result.SynErrorLineBackground := TColor(FConf.GetValue(SynEditKey + ERROR_LINE_COLOR_KEY, Integer(Result.SynErrorLineBackground)));

  HlKey := GetHighlightKey;

  CurrKey := HlKey + TSynJTDSyn.GetKeyAttriKey;
  Result.SynKeyAttri.Foreground := TColor(FConf.GetValue(CurrKey + FOREGROUND_KEY, Integer(Result.SynKeyAttri.Foreground)));
  CurrKey := CurrKey + STYLE_KEY;
  Result.SynKeyAttri.Bold := FConf.GetValue(CurrKey + BOLD_KEY, Result.SynKeyAttri.Bold);
  Result.SynKeyAttri.Italic := FConf.GetValue(CurrKey + ITALIC_KEY, Result.SynKeyAttri.Italic);

  CurrKey := HlKey + TSynJTDSyn.GetAttributeAttriKey;
  Result.SynAttributeAttri.Foreground := TColor(FConf.GetValue(CurrKey + FOREGROUND_KEY, Integer(Result.SynAttributeAttri.Foreground)));
  CurrKey := CurrKey + STYLE_KEY;
  Result.SynAttributeAttri.Bold := FConf.GetValue(CurrKey + BOLD_KEY, Result.SynAttributeAttri.Bold);
  Result.SynAttributeAttri.Italic := FConf.GetValue(CurrKey + ITALIC_KEY, Result.SynAttributeAttri.Italic);

  CurrKey := HlKey + TSynJTDSyn.GetReservedAttriKey;
  Result.SynReservedAttri.Foreground := TColor(FConf.GetValue(CurrKey + FOREGROUND_KEY, Integer(Result.SynReservedAttri.Foreground)));
  CurrKey := CurrKey + STYLE_KEY;
  Result.SynReservedAttri.Bold := FConf.GetValue(CurrKey + BOLD_KEY, Result.SynReservedAttri.Bold);
  Result.SynReservedAttri.Italic := FConf.GetValue(CurrKey + ITALIC_KEY, Result.SynReservedAttri.Italic);

  CurrKey := HlKey + TSynJTDSyn.GetSymbolAttriKey;
  Result.SynSymbolAttri.Foreground := TColor(FConf.GetValue(CurrKey + FOREGROUND_KEY, Integer(Result.SynSymbolAttri.Foreground)));
  CurrKey := CurrKey + STYLE_KEY;
  Result.SynSymbolAttri.Bold := FConf.GetValue(CurrKey + BOLD_KEY, Result.SynSymbolAttri.Bold);
  Result.SynSymbolAttri.Italic := FConf.GetValue(CurrKey + ITALIC_KEY, Result.SynSymbolAttri.Italic);

  CurrKey := HlKey + TSynJTDSyn.GetCustomKeyAttriKey;
  Result.SynCustomKeyAttri.Foreground := TColor(FConf.GetValue(CurrKey + FOREGROUND_KEY, Integer(Result.SynCustomKeyAttri.Foreground)));
  CurrKey := CurrKey + STYLE_KEY;
  Result.SynCustomKeyAttri.Bold := FConf.GetValue(CurrKey + BOLD_KEY, Result.SynCustomKeyAttri.Bold);
  Result.SynCustomKeyAttri.Italic := FConf.GetValue(CurrKey + ITALIC_KEY, Result.SynCustomKeyAttri.Italic);

  CurrKey := HlKey + TSynJTDSyn.GetIdentAttriKey;
  Result.SynIdentAttri.Foreground := TColor(FConf.GetValue(CurrKey + FOREGROUND_KEY, Integer(Result.SynIdentAttri.Foreground)));
  CurrKey := CurrKey + STYLE_KEY;
  Result.SynIdentAttri.Bold := FConf.GetValue(CurrKey + BOLD_KEY, Result.SynIdentAttri.Bold);
  Result.SynIdentAttri.Italic := FConf.GetValue(CurrKey + ITALIC_KEY, Result.SynIdentAttri.Italic);
end;

procedure TAppConf.SetDisplayStyle(const aValue: TDisplayStyle);
var
  SynEditKey, FontKey, HlKey, CurrKey: string;
begin
  SynEditKey := GetSynEditKey;
  FontKey := SynEditKey + FONT_KEY;

  FConf.SetValue(FontKey + NAME_KEY, aValue.SynFontName);
  FConf.SetValue(FontKey + SIZE_KEY, aValue.SynFontSize);

  FConf.SetValue(SynEditKey + EXTRA_LINE_SPACE, aValue.SynExtraLineSpace);
  FConf.SetValue(SynEditKey + EXTRA_CHAR_SPACE, aValue.SynExtraCharSpace);
  FConf.SetValue(SynEditKey + RIGHT_EDGE_KEY, aValue.SynRightEdge);
  FConf.SetValue(SynEditKey + NTH_NUMBER_KEY, aValue.SynShowEveryNthNumber);
  FConf.SetValue(SynEditKey + LINE_NUM_COLOR_KEY, Integer(aValue.SynLineNumberColor));
  FConf.SetValue(SynEditKey + CURR_LINE_COLOR_KEY, Integer(aValue.SynCurrLineBackground));
  FConf.SetValue(SynEditKey + ERROR_LINE_COLOR_KEY, Integer(aValue.SynErrorLineBackground));

  HlKey := GetHighlightKey;

  CurrKey := HlKey + TSynJTDSyn.GetKeyAttriKey;
  FConf.SetValue(CurrKey + FOREGROUND_KEY, Integer(aValue.SynKeyAttri.Foreground));
  CurrKey := CurrKey + STYLE_KEY;
  FConf.SetValue(CurrKey + BOLD_KEY, aValue.SynKeyAttri.Bold);
  FConf.SetValue(CurrKey + ITALIC_KEY, aValue.SynKeyAttri.Italic);

  CurrKey := HlKey + TSynJTDSyn.GetAttributeAttriKey;
  FConf.SetValue(CurrKey + FOREGROUND_KEY, Integer(aValue.SynAttributeAttri.Foreground));
  CurrKey := CurrKey + STYLE_KEY;
  FConf.SetValue(CurrKey + BOLD_KEY, aValue.SynAttributeAttri.Bold);
  FConf.SetValue(CurrKey + ITALIC_KEY, aValue.SynAttributeAttri.Italic);

  CurrKey := HlKey + TSynJTDSyn.GetReservedAttriKey;
  FConf.SetValue(CurrKey + FOREGROUND_KEY, Integer(aValue.SynReservedAttri.Foreground));
  CurrKey := CurrKey + STYLE_KEY;
  FConf.SetValue(CurrKey + BOLD_KEY, aValue.SynReservedAttri.Bold);
  FConf.SetValue(CurrKey + ITALIC_KEY, aValue.SynReservedAttri.Italic);

  CurrKey := HlKey + TSynJTDSyn.GetSymbolAttriKey;
  FConf.SetValue(CurrKey + FOREGROUND_KEY, Integer(aValue.SynSymbolAttri.Foreground));
  CurrKey := CurrKey + STYLE_KEY;
  FConf.SetValue(CurrKey + BOLD_KEY, aValue.SynSymbolAttri.Bold);
  FConf.SetValue(CurrKey + ITALIC_KEY, aValue.SynSymbolAttri.Italic);

  CurrKey := HlKey + TSynJTDSyn.GetCustomKeyAttriKey;
  FConf.SetValue(CurrKey + FOREGROUND_KEY, Integer(aValue.SynCustomKeyAttri.Foreground));
  CurrKey := CurrKey + STYLE_KEY;
  FConf.SetValue(CurrKey + BOLD_KEY, aValue.SynCustomKeyAttri.Bold);
  FConf.SetValue(CurrKey + ITALIC_KEY, aValue.SynCustomKeyAttri.Italic);

  CurrKey := HlKey + TSynJTDSyn.GetIdentAttriKey;
  FConf.SetValue(CurrKey + FOREGROUND_KEY, Integer(aValue.SynIdentAttri.Foreground));
  CurrKey := CurrKey + STYLE_KEY;
  FConf.SetValue(CurrKey + BOLD_KEY, aValue.SynIdentAttri.Bold);
  FConf.SetValue(CurrKey + ITALIC_KEY, aValue.SynIdentAttri.Italic);
  FConf.Flush;
end;

procedure TAppConf.SetJsonFormatStyle(const aValue: TJsonFormatStyle);
var
  FmtKey: string;
  Opt: TJsFormatOption;
  o: string;
begin
  FmtKey := GetJsonFormatKey;
  for Opt in TJsFormatOption do
    begin
      Str(Opt, o);
      o := '/' + o;
      if Opt in aValue.Options then
        FConf.SetValue(FmtKey + FMT_OPTS_KEY + o, True)
      else
        FConf.SetValue(FmtKey + FMT_OPTS_KEY + o, False);
    end;
  FConf.SetValue(FmtKey + INDENT_SIZE_KEY, aValue.IndentSize);
  FConf.SetValue(FmtKey + INDENT_AFTER_COMMA, aValue.IndentAfterComma);
  FConf.SetValue(FmtKey + INDENT_BEFORE_COLON, aValue.IndentBeforeColon);
  FConf.SetValue(FmtKey + INDENT_AFTER_COLON, aValue.IndentAfterColon);
  FConf.SetValue(FmtKey + ONELINE_ARRAY_BOUND, aValue.OneLineArrayBound);
  FConf.SetValue(FmtKey + ONELINE_OBJECT_BOUND, aValue.OneLineObjectBound);
  FConf.SetValue(FmtKey + LINE_BREAK_KEY, Integer(aValue.LineBreak));
  FConf.Flush;
end;

procedure TAppConf.SetMiscSettings(const aValue: TMiscSettings);
begin
  FConf.Formatted := aValue.SaveFormatted;
  FConf.SetValue(MISC_KEY + MAX_RECENT_KEY, aValue.MaxRecentFiles);
  FConf.SetValue(MISC_KEY + SUCCESS_COLOR_KEY, Integer(aValue.SuccessColor));
  FConf.SetValue(MISC_KEY + FORMATTED_KEY, aValue.SaveFormatted);
  FConf.SetValue(MISC_KEY + ALLOW_BOM_KEY, aValue.AllowBom);
  FConf.Flush;
end;

function TAppConf.GetJsonFormatKey: string;
begin
  Result := GetAppearanceKey + JSON_FORMAT_KEY;
end;

function TAppConf.GetSettingsKey: string;
begin
  Result := SETTINGS_KEY;
end;

function TAppConf.GetMaxRecentKey: string;
begin
  Result := GetSettingsKey + MAX_RECENT_KEY;
end;

function TAppConf.GetAppearanceKey: string;
begin
  Result := GetSettingsKey + APPIARANCE_KEY;
end;

function TAppConf.GetSynEditKey: string;
begin
  Result := GetAppearanceKey + SYN_EDIT_KEY;
end;

function TAppConf.GetHighlightKey: string;
begin
  Result := GetSynEditKey + HIGHLIGHT_KEY;
end;

function TAppConf.GetMiscKey: string;
begin
  Result := GetSettingsKey + MISC_KEY;
end;

constructor TAppConf.Create;
begin
  FConf := TJsonConf.Create(nil);
end;

destructor TAppConf.Destroy;
begin
  FConf.Free;
  inherited;
end;

function TAppConf.GetComponentKey: string;
begin
  Result := COMPONENTS_KEY;
end;

function TAppConf.GetRecentFilesKey: string;
begin
  Result := GetSettingsKey + RECENT_FILES_KEY;
end;

initialization

finalization
  AppConf.Free;
end.

