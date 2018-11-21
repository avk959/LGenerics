{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Common string resources.                                                *
*                                                                           *
*   Copyright(c) 2018 A.Koverdyaev(avk)                                     *
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
unit LGStrConst;

{$mode objfpc}{$H+}

interface

resourcestring

  SEOptionalValueEmpty     = 'Optional value is empty';
  SEArgumentTooBigFmt      = 'Argument of %s is too big(%d)';
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
  SEInternalDataInconsist  = 'Internal data inconsistency';
  SEValueAlreadyExist      = 'Value already exists';
  SEKeyAlreadyExist        = 'Key already exists';
  SEThreadTimeOut          = 'Thread timeout exceeded';
  SEResultUnknown          = 'Result is unknown';
  SECellNotFoundFmt        = 'Specified cell of %s not found';
  SEEdgeNotFoundFmt        = 'Edge (%d, %d) not found';
  SECallbackMissed         = 'Callback missed';
  SEStreamWriteVertMissed  = 'OnStreamWriteVertex property missed';
  SEStreamWriteDataMissed  = 'OnStreamWriteData property missed';
  SEStreamReadVertMissed   = 'OnStreamReadVertex property missed';
  SEStreamReadDataMissed   = 'OnStreamReadData property missed';
  SEUnknownGraphStreamFmt  = 'Unknown graph stream format';
  SEUnsuppGraphFmtVersion  = 'Unsupported graph stream format version';
  SEGraphStreamCorrupt     = 'Graph stream data corrupted';
  SEGraphStreamReadIntern  = 'Graph stream read intenal error';
  SEStrLenExceedFmt        = 'Maximum string length exceeded(%d)';
  SEBitMatrixSizeExceedFmt = 'Maximum bit matrix size exceeded(%d)';
  SEUnableOpenFileFmt      = 'Failed to open file "%s"';
  SEUnexpectEol            = 'Unexpected end of line';
  SEStreamSizeExceedFmt    = 'Maximum stream size exceeded(%d)';
  SEMethodNotApplicable    = 'Method is not applicable';
  SENonSquareInputMatrix   = 'Input matrix is not square';

implementation

end.

