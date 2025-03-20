unit DiffUtils;

{$MODE OBJFPC}{$H+}{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, LgSeqUtils;

type
  TDiffFlag = (
    dfIgnoreCase,      // ignore character case
    dfIgnoreEmptyLines,// ignore empty lines
    dfIgnoreWSChange,  // ignore if whitespace characters were added or removed
    dfIgnoreWSChars    // ignore whitespace characters
  );
  TDiffFlags = set of TDiffFlag;


  TTextNode = record
    Code: QWord;
    LineNumber: SizeInt;
    constructor Make(aCode: QWord; aNumber: SizeInt);
    class function HashCode(const aNode: TTextNode): SizeInt; inline; static;
    class function Equal(const L, R: TTextNode): Boolean; inline; static;
  end;
  TNodeList = array of TTextNode;

  TDiffUtil     = specialize TGSeqUtil<TTextNode, TTextNode>;
  TUcs4DiffUtil = specialize TGSeqUtil<Ucs4Char, TUcs4Hasher>;
  TSeqEditOp    = TDiffUtil.TSeqEditOp;
  TSeqEdit      = TDiffUtil.TSeqEdit;
  TSeqPatch     = TDiffUtil.TSeqPatch;
  TSeqPatchStat = TDiffUtil.TSeqPatchStat;
  TLcsAlgo      = TDiffUtil.TLcsAlgo;
  TUcs4Diff     = TUcs4DiffUtil.TDiffV;

  TLineDiff = record
    Source,
    Target: TUcs4Seq;
    Diff: TUcs4Diff;
  end;
  PLineDiff = ^TLineDiff;


implementation
{$B-}{$COPERATORS ON}

{ TTextNode }

constructor TTextNode.Make(aCode: QWord; aNumber: SizeInt);
begin
  Code := aCode;
  LineNumber := aNumber;
end;

class function TTextNode.HashCode(const aNode: TTextNode): SizeInt;
begin
  Result := aNode.Code;
end;

class function TTextNode.Equal(const L, R: TTextNode): Boolean;
begin
  Result := L.Code = R.Code;
end;

end.

