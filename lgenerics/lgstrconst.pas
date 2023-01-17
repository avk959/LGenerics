{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Common string resources.                                                *
*                                                                           *
*   Copyright(c) 2018-2023 A.Koverdyaev(avk)                                *
*                                                                           *
*   This code is free software; you can redistribute it and/or modify it    *
*   under the terms of the Apache License, Version 2.0;                     *
*   You may obtain a copy of the License at                                 *
*     http://www.apache.org/licenses/LICENSE-2.0.                           *
*                                                                           *
*  Unless required by applicable law or agreed to in writing, software      *
*  distributed under the License is distributed on an "AS IS" BASIS,        *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *
*  See the License for the specific language governing permissions and      *
*  limitations under the License.                                           *
*                                                                           *
*****************************************************************************}
unit lgStrConst;

{$mode objfpc}{$H+}

interface

resourcestring

  SEOptionalValueEmpty     = 'No value assigned';
  SECopyInadmissible       = 'Copying inadmissible';
  SEOwnRequired            = 'Ownership required';
  SEArgumentTooBigFmt      = 'Argument of %s is too big(%d)';
  SEArrayTooBigFmt         = 'Array size is too big(%d)';
  SEClassAccessEmptyFmt    = 'Can not access element of empty %s';
  SECantAccessEmpty        = 'Can not access element of empty container';
  SEClassCapacityExceedFmt = '%s maximum capacity exceeded(%d)';
  SECapacityExceedFmt      = 'Maximum capacity exceeded(%d)';
  SECantUpdDuringIterFmt   = 'Can not update %s during enumeration';
  SEArrIndexOutOfBoundsFmt = 'Array index out of bounds(%d)';
  SEClassIdxOutOfBoundsFmt = '%s index out of bounds(%d)';
  SEIndexOutOfBoundsFmt    = 'Index out of bounds(%d)';
  SEKeyNotFound            = 'Key not found';
  SEValueNotFound          = 'Value not found';
  SECantAcceptNegCountFmt  = 'The %s''s TEntry.Count can not accept negative value';
  SECantAcceptNegCount     = 'Can not accept negative TEntry.Count value';
  SECantAcceptNegLen       = 'Can not accept negative length value';
  SEInternalDataInconsist  = 'Internal data inconsistency';
  SEInvalidParamFmt        = 'Invalid parameter "%s" value';
  SEInputShouldAtLeastFmt  = 'Parameter "%s" should be at least %d';
  SEValueAlreadyExist      = 'Value already exists';
  SEKeyAlreadyExist        = 'Key already exists';
  SEResultUnknownFatal     = 'Result is unknown due to fatal exception';
  SEResultUnknownCancel    = 'Result is unknown due to task cancelled';
  SECellNotFoundFmt        = 'Specified cell of %s not found';
  SEEdgeNotFoundFmt        = 'Edge (%d, %d) not found';
  SECallbackMissed         = 'Callback missed';
  SEStreamWriteVertMissed  = 'OnWriteVertex callback missed';
  SEStreamWriteDataMissed  = 'OnWriteData callback missed';
  SEStreamReadVertMissed   = 'OnReadVertex callback missed';
  SEStreamReadDataMissed   = 'OnReadData callback missed';
  SEUnknownGraphStreamFmt  = 'Unknown graph stream format';
  SEUnsuppGraphFmtVersion  = 'Unsupported graph stream format version';
  SEGraphStreamCorrupt     = 'Graph stream data corrupted';
  SEGraphStreamReadIntern  = 'Graph stream read intenal error';
  SEStrLenExceedFmt        = 'Maximum string length exceeded(%d)';
  SEBitMatrixSizeExceedFmt = 'Maximum bit matrix size exceeded(%d)';
  SEUnableOpenFileFmt      = 'Failed to open file "%s"';
  SEUnableOpenFileFmt3     = 'Failed to open file "%s":' + LineEnding + 'exception %s with message "%s"';
  SEUnexpectEol            = 'Unexpected end of line';
  SEStreamSizeExceedFmt    = 'Maximum stream size exceeded(%d)';
  SEMethodNotApplicable    = 'Method is not applicable';
  SEInputMatrixTrivial     = 'Input matrix is trivial';
  SENonSquareInputMatrix   = 'Input matrix is not square';
  SEInputMatrixNegElem     = 'Input matrix contains negative element';
  SEInvalidTreeInst        = 'Invalid tree instance';
  SENoSuchEdgeFmt          = 'No such edge (%d, %d)';
  SEInputIsNotProperPermut = 'Input array is not proper permutation';
  SEVertexNonUnique        = 'Vertex label is not unique';
  SECantConvertFmt         = 'Cannot convert %s to %s';
  SEInvalidJsPtr           = 'Invalid JSON Pointer instance';
  SECantParseJsStr         = 'Cannot parse JSON string';
  SEUnknownJsNodeKind      = 'Unknown JsonNode kind';
  SEUnknownJsDataType      = 'Unknown JsonData type';
  SEJsonInstNotObj         = 'Cannot retrieve pair from non-object instance';
  SESortProcNotAssigned    = 'Sorting procedure not assigned';
  SERowIndexOutOfBoundsFmt = 'Row index out of bounds(%d)';
  SEColIndexOutOfBoundsFmt = 'Column index out of bounds(%d)';
  SEDirtyQuoteFmt          = 'Irrelevant quotation mark in non-quoted cell [%d, %d]';
  SEHangQuoteFmt           = 'Extra or missing quotation mark in quoted cell [%d, %d]';
  SEBareQuoteFmt           = 'Unescaped quotation mark in quoted cell [%d, %d]';
  SEColCountFmt            = 'Number of cells(%d) in row #%d does not match header';
  SEEncodingNotSuppFmt     = 'Encoding %s not supported';
  SEInvalidDelimiterFmt    = 'Invalid delimiter mark(#%u)';
  SEInvalidQuoteMarkFmt    = 'Invalid quotation mark(#%u)';
  SEInvalidCommentMarkFmt  = 'Invalid comment mark(#%u)';
  SEEmptyDoc               = 'Document contains no data';
  SEReadCsvErrorFmt        = '%s'  + LineEnding + 'near <%s>';
  SEVariantNotSupportFmt   = 'Variant type(%u) is not supported';
  SEPdoTypeNotSupportFmt   = 'Type "%s"("%s") is not supported';
  SEUnsupportPdoKindFmt    = 'Unsupported istance kind(%s)';
  SEUnsupportPdoTypeFmt    = 'Unsupported istance type(%s)';
  SEUnexpectJsonEnd        = 'Unexpected end of JSON';
  SEInvalidJsonInst        = 'Invalid JSON instance';
  SEUnexpectJsonTokenFmt   = 'Expected JSON token kind "%s", but got "%s"';
  SEUnexpectJsonTokenFmt2  = 'Expected JSON token kind "%s" or "%s", but got "%s"';
  SEJsonStrNotMatchEnumFmt = 'JSON string "%s" does not match any %s enumeration value';
  SEUnexpectJsonStrLenFmt  = 'JSON string length(%d) does not match SizeOf(Char)';
  SECantReadJson2VarFmt    = 'Can not assign JSON type "%s" to Variant instance';
  SEJsonStrExceedSSLenFmt  = 'Length of the JSON string(%d) exceeds the maximum ShortString length(%u)';
  SECantAssignJsonNumFmt   = 'Can not assign JSON float number to instance of kind "%s"'; //???
  SEPdoRangeErrorFmt       = 'JSON value out of %s range(%d)';
  SEPdoOverflowErrorFmt    = 'JSON value leads to %s overflow(%e)';
  SEStrLenNotMatchSizeFmt  = '%s length(%d) does not match size of %s';
  SERecordFieldNotFoundFmt = '%s record field "%s" not found';
  SEClassPropNotFoundFmt   = '%s class property "%s" not found';
  SEJsonNullsNotAcceptable = 'JSON null values are not acceptable';
  SEJsonArraySizeNotFitFmt = 'JSON array size does not match the size of static PDO array(%d)';
  SEUnssignedClassInstFmt  = 'Unassigned class instance(%s)';
//JTD consts
  SECantLoadJsStream         = 'Cannot load JSON stream';
  SECantLoadJsFile           = 'Cannot load JSON file';
  SESchemaNotObjectFmt       = 'JTD schema must be an object, but got "%s"';
  SEJtdRefNotStrFmt          = 'Ref form must be a string, but got "%s"';
  SEJtdEnumNotArrayFmt       = 'Enum form must be an array, but got "%s"';
  SEEnumMustNotEmpty         = 'Enum must not be empty';
  SEEnumElemNotStrFmt        = 'Enum''s elements must be a string, but got "%s"';
  SEPropsNotObjectFmt        = 'Properties form must be an object, but got "%s"';
  SEAddtPropsNotBoolFmt      = 'AdditionalProperties must be a boolean, but got "%s"';
  SEValuesNotObjectFmt       = 'Values form must be an object, but got "%s"';
  SEDiscriminNotStrFmt       = 'Discriminator must be a string, but got "%s"';
  SEMappingNotObjectFmt      = 'Mapping must be an object, but got "%s"';
  SEDefsNotObjectFmt         = 'Definitions must be an object, but got "%s"';
  SENullableNotBoolFmt       = 'Nullable must be a boolean, but got "%s"';
  SEMetaNotObjectFmt         = 'Metadata must be an object, but got "%s"';
  SEUnknownKeywordFmt        = 'Unknown keyword(%s)';
  SEIrrelevantFormKwdFmt     = 'Irrelevant form keyword(%s)';
  SESchemaKeysNotUniq        = 'JTD schema contains non-unique keys';
  SEKindAforeAssigned        = 'Property Kind is already assigned';
  SENonUniqEnumElemFmt       = 'Non-unique enum''s element(%s)';
  SENonRootSchemaDefs        = 'Non-root schema cannot have definitions';
  SEJtdInvalidForm           = 'Invalid JTD form';
  SEJtdNonRootDefs           = 'Non-root definitions';
  SERefDefNonExistent        = 'Reference to non-existent definition';
  SEPropsOptPropsKeyShare    = 'Properties share keys with optionalProperties';
  SEMappingIsNullable        = 'Mapping value is nullable';
  SEMappingNotPropForm       = 'Mapping value is not a properties form';
  SEDiscrMapPropsKeyShare    = 'Discriminator shares key with mapping properties';
  SEDiscrMapOptPropsKeyShare = 'Discriminator shares key with mapping optionalProperties';

implementation

end.

