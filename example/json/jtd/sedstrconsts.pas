unit sedStrConsts;

{$MODE OBJFPC}{$H+}

interface

resourcestring
  SEInvalidJsonFileFmt      = 'JSON file "%s" is invalid.';
  SEInvalidJtdSchemaFileFmt = 'JTD schema file "%s" is invalid.';
  SEIllegalSymbolFmt        = 'Illegal JSON symbol(#u)';
  SEFileNotFoundFmt         = 'File "%s" not found.';
  SEInternStackOverflow     = 'Internal stack overflow';
  SEEmptySrc                = 'Empty source';
  SEUnexpectBomFmt          = 'Unexpected BOM(%s)';
  SEUnexpectEof             = 'Unexpected end of file';
  SEUnexpectSymbolFmt       = 'Line %u, pos %u : unexpected symbol (%s) - #%u';
  SECantLoadFileFmt         = 'Failed to load file "%s", error: %s with message "%s".';
  SECantFormatInvalidJson   = 'Cannot format invalid JSON.';
  SRemoveListItemPrompt     = 'Do you want to remove "%s" from the list?';
  SModifiedSavePrompt       = 'Schema source has been changed.' + LineEnding + 'Do you want to save changes?';
  SFileOvewritePromptFmt    = 'File "%s" already exists.' + LineEnding + 'Do you want to overwrite the file?';
  SInferDlgClosePrompt      = 'No samples are selected.' + LineEnding + 'Close the dialog window anyway?';
  SCodegenDlgClosePrompt    = 'Unit file name is not set.' + LineEnding + 'Close the dialog window anyway?';
  SDeleteDridRowPromptFmt   = 'Delete row %d from the grid?';
  SEValitateErrsFmt         = 'Validation errors: %d';
  SValidationOk             = 'Validation OK';
  SCopy                     = 'copy';
  SWith                     = 'with';
  SReplace                  = 'Replace';
  STextNotFoundFmt          = 'Text "%s" not found.';
  SAbout                    = 'About';
  SGenUnit                  = 'Generate Pascal unit';
  SUnitPreview              = 'Unit preview';
  SSelectSamples2Infer      = 'Select JSON samples to infer schema';
  SSearchText               = 'Search text';
  SReplaceTest              = 'Replace text';
  SSettings                 = 'Settings';
  SSchemaPath               = 'schema path';
  SInstancePath             = 'instance path';

implementation

end.

