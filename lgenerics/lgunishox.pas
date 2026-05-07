{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Free Pascal port of Unisox2, a C library for compressing short strings, *
*   https://github.com/siara-cc/Unishox2                                    *
*                                                                           *
*   Copyright (C) 2020 Siara Logics (cc)                                    *
*   Copyright (C) 2026 A.Koverdyaev(avk)                                    *
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
unit LgUnishox;

{$MODE OBJFPC}{$J-}{$INLINE ON}

interface

uses
  SysUtils;  // StrLen

type
  PUSLinkList = ^TUSLinkList;
  TUSLinkList = record
    data: PAnsiChar;
    previous: PUSLinkList;
  end;

  // Horizontal codes and their lengths (always 5 elements)
  TUsxHCodes    = array[0..4] of Byte;
  TUsxHCodeLens = array[0..4] of Byte;

  // Frequently occurring sequences (6 elements)
  TUsxFreqSeq   = array[0..5] of PAnsiChar;
  PUsxFreqSeq   = ^TUsxFreqSeq;
  // Templates (5 elements)
  TUsxTemplates = array[0..4] of PAnsiChar;
  PUsxTemplates = ^TUsxTemplates;

const
  UNISHOX_VERSION       = '2.0';
  UNISHOX_MAGIC_BITS    = $FF;
  UNISHOX_MAGIC_BIT_LEN = 1;

{ Default Horizontal codes. When composition of text is know beforehand,
  the other hcodes in this section can be used to achieve more compression.}
  USX_HCODES_DFLT: TUsxHCodes                      = ($00, $40, $80, $C0, $E0);
// Length of each default hcode
  USX_HCODE_LENS_DFLT: TUsxHCodeLens               = (2, 2, 2, 3, 3);
// Horizontal codes preset for English Alphabet content only
  USX_HCODES_ALPHA_ONLY: TUsxHCodes                = ($00, $00, $00, $00, $00);
// Length of each Alpha only hcode
  USX_HCODE_LENS_ALPHA_ONLY: TUsxHCodeLens         = (0, 0, 0, 0, 0);
// Horizontal codes preset for Alpha Numeric content only
  USX_HCODES_ALPHA_NUM_ONLY: TUsxHCodes            = ($00, $00, $80, $00, $00);
// Length of each Alpha numeric hcode
  USX_HCODE_LENS_ALPHA_NUM_ONLY: TUsxHCodeLens     = (1, 0, 1, 0, 0);
// Horizontal codes preset for Alpha Numeric and Symbol content only
  USX_HCODES_ALPHA_NUM_SYM_ONLY: TUsxHCodes        = ($00, $80, $C0, $00, $00);
// Length of each Alpha numeric and symbol hcodes
  USX_HCODE_LENS_ALPHA_NUM_SYM_ONLY: TUsxHCodeLens = (1, 2, 2, 0, 0);
// Horizontal codes preset favouring Alphabet content
  USX_HCODES_FAVOR_ALPHA: TUsxHCodes               = ($00, $80, $A0, $C0, $E0);
// Length of each hcode favouring Alpha content
  USX_HCODE_LENS_FAVOR_ALPHA: TUsxHCodeLens        = (1, 3, 3, 3, 3);
// Horizontal codes preset favouring repeating sequences
  USX_HCODES_FAVOR_DICT: TUsxHCodes                = ($00, $40, $C0, $80, $E0);
// Length of each hcode favouring repeating sequences
  USX_HCODE_LENS_FAVOR_DICT: TUsxHCodeLens         = (2, 2, 3, 2, 3);
// Horizontal codes preset favouring symbols
  USX_HCODES_FAVOR_SYM: TUsxHCodes                 = ($80, $00, $A0, $C0, $E0);
// Length of each hcode favouring symbols
  USX_HCODE_LENS_FAVOR_SYM: TUsxHCodeLens          = (3, 1, 3, 3, 3);
// Horizontal codes preset favouring umlaut letters
  USX_HCODES_FAVOR_UMLAUT: TUsxHCodes              = ($80, $A0, $C0, $E0, $00);
// Length of each hcode favouring umlaut letters
  USX_HCODE_LENS_FAVOR_UMLAUT: TUsxHCodeLens       = (3, 3, 3, 3, 1);
// Horizontal codes preset for no repeating sequences
  USX_HCODES_NO_DICT: TUsxHCodes                   = ($00, $40, $80, $00, $C0);
// Length of each hcode for no repeating sequences
  USX_HCODE_LENS_NO_DICT: TUsxHCodeLens            = (2, 2, 2, 0, 2);
// Horizontal codes preset for no Unicode characters
  USX_HCODES_NO_UNI: TUsxHCodes                    = ($00, $40, $80, $C0, $00);
// Length of each hcode for no Unicode characters
  USX_HCODE_LENS_NO_UNI: TUsxHCodeLens             = (2, 2, 2, 2, 0);

// --- Frequently occurring sequences ------------------------------------
  USX_FREQ_SEQ_DFLT: TUsxFreqSeq = ('": "', '": ', '</', '="', '":"', '://');
  USX_FREQ_SEQ_TXT: TUsxFreqSeq  = (' the ', ' and ', 'tion', ' with', 'ing', 'ment');
  USX_FREQ_SEQ_URL: TUsxFreqSeq  = ('https://', 'www.', '.com', 'http://', '.org', '.net');
  USX_FREQ_SEQ_JSON: TUsxFreqSeq = ('": "', '": ', '",', '}}}', '":"', '}}');
  USX_FREQ_SEQ_HTML: TUsxFreqSeq = ('</', '="', 'div', 'href', 'class', '<p>');
  USX_FREQ_SEQ_XML: TUsxFreqSeq  = ('</', '="', '">', '<?xml version="1.0"', 'xmlns:', '://');

// --- Templates ---------------------------------------------------------
  USX_TEMPLATES: TUsxTemplates = (
    'tfff-of-tfTtf:rf:rf.fffZ',
    'tfff-of-tf',
    '(fff) fff-ffff',
    'tf:rf:rf',
    nil
  );

{ Simple API for compressing a string
    inbuf    Input ASCII / UTF-8 string
    ilen     Length of the input in bytes
    outbuf   Output buffer - should be large enough to hold compressed output
    olen     Length of the output buffer in bytes }
  function unishox2_compress_simple(const inbuf: PAnsiChar; ilen: Int32; outbuf: PAnsiChar; olen: Int32): Int32;

{ Simple API for decompressing a string
    inbuf    Input compressed bytes (output of unishox2_compress functions)
    ilen     Length of the inbuf in bytes
    outbuf   Output buffer for ASCII / UTF-8 string - should be large enough
    olen     Length of the output buffer in bytes }
  function unishox2_decompress_simple(const inbuf: PAnsiChar; ilen: Int32; outbuf: PAnsiChar; olen: Int32): Int32;

{ Comprehensive API for compressing a string
    inbuf          Input ASCII / UTF-8 string
    ilen           Length of the inbuf in bytes
    outbuf         Output buffer - should be large enough to hold compressed output
    olen           Length of the outbuf in bytes
    usx_hcodes     Horizontal codes (array of bytes)
    usx_hcode_lens Length of each element in usx_hcodes array
    usx_freq_seq   Frequently occuring sequences
    usx_templates  Templates of frequently occuring patterns }
  function unishox2_compress(const inbuf: PAnsiChar; ilen: Int32; outbuf: PAnsiChar; olen: Int32;
                             const usx_hcodes: TUsxHCodes; const usx_hcode_lens: TUsxHCodeLens;
                             usx_freq_seq: PUsxFreqSeq; usx_templates: PUsxTemplates): Int32;
{ Comprehensive API for de-compressing a string
    inbuf          Input compressed bytes (output of unishox2_compress functions)
    ilen           Llength of the inbuf in bytes
    outbuf         Output buffer - should be large enough to hold de-compressed output
    olen           Length of the outbuf in bytes
    usx_hcodes     Horizontal codes (array of bytes)
    usx_hcode_lens Length of each element in usx_hcodes array
    usx_freq_seq   Frequently occuring sequences
    usx_templates  Templates of frequently occuring patterns }
  function unishox2_decompress(const inbuf: PAnsiChar; ilen: Int32; outbuf: PAnsiChar; olen: Int32;
                               const usx_hcodes: TUsxHCodes; const usx_hcode_lens: TUsxHCodeLens;
                               usx_freq_seq: PUsxFreqSeq; usx_templates: PUsxTemplates): Int32;

{ More Comprehensive API for compressing array of strings.
    See unishox2_compress() function for parameter definitions.
    This function takes an additional parameter, i.e. 'prev_lines' - pointer to the TUSLinkList structure.
    This function is used when an array of strings need to be compressed and stored in a compressed
    array of bytes for use as a constant in other programs where each element of the array can be
    decompressed and used at runtime. }
  function unishox2_compress_lines(const inbuf: PAnsiChar; ilen: Int32; outbuf: PAnsiChar; olen: Int32;
                                   const usx_hcodes: TUsxHCodes; const usx_hcode_lens: TUsxHCodeLens;
                                   usx_freq_seq: PUsxFreqSeq; usx_templates: PUsxTemplates;
                                   prev_lines: PUSLinkList): Int32;

{ More Comprehensive API for de-compressing array of strings.
    This function is not be used in conjuction with unishox2_compress_lines().
    See unishox2_decompress() function for parameter definitions.
    Typically an array is compressed using unishox2_compress_lines() and
    a header (.h) file is generated using the resultant compressed array.
    This header file can be used in another program with another decompress routine
    which takes this compressed array as parameter and index to be decompressed. }
  function unishox2_decompress_lines(const inbuf: PAnsiChar; ilen: Int32; outbuf: PAnsiChar; olen: Int32;
                                     const usx_hcodes: TUsxHCodes; const usx_hcode_lens: TUsxHCodeLens;
                                     usx_freq_seq: PUsxFreqSeq; usx_templates: PUsxTemplates;
                                     prev_lines: PUSLinkList): Int32;
type
{ Pre-defined parameter sets for different types of input data }
  TUsxPreset = (
    upsDefault,         {  Default preset parameter set.
                           When composition of text is know beforehand, the other parameter sets
                           in this section can be used to achieve better compression }
    upsAlphaOnly,       // Preset parameter set for English Alphabet only content
    upsAlphaNumOnly,    // Preset parameter set for Alpha numeric content
    upsAlphaNumSymOnly, // Preset parameter set for Alpha numeric and symbol content
    upsAlphaNumSymTxt,  // Preset parameter set for Alpha numeric symbol content having predominantly text
    upsFavorAlpha,      // Preset parameter set favouring Alphabet content
    upsFavorDict,       // Preset parameter set favouring repeating sequences
    upsFavorSym,        // Preset parameter set favouring symbols
    upsFavorUmlaut,     // Preset parameter set favouring unlaut letters
    upsNoDict,          // Preset parameter set for when there are no repeating sequences
    upsNoUni,           // Preset parameter set for when there are no unicode symbols
    upsNoUniFavorText,  // Preset parameter set for when there are no unicode symbols favouring text
    upsUrl,             // Preset parameter set favouring URL content
    upsJson,            // Preset parameter set favouring JSON content
    upsJsonNoUni,       // Preset parameter set favouring JSON content having no Unicode symbols
    upsXml,             // Preset parameter set favouring XML content
    upsHtml             // Preset parameter set favouring HTML content
  );

{ Convenience compressing overload using presets.
  See unishox2_compress() function for other parameter definitions.}
  function unishox2_compress_preset(const inbuf: PAnsiChar; ilen: Int32; outbuf: PAnsiChar; olen: Int32;
                                    preset: TUsxPreset): Int32;

{ Convenience de-compressing overload using presets.
  See unishox2_decompress() function for other parameter definitions.}
  function unishox2_decompress_preset(const inbuf: PAnsiChar; ilen: Int32; outbuf: PAnsiChar; olen: Int32;
                                      preset: TUsxPreset): Int32;


implementation
{$Q-}{$R-}{$COPERATORS ON}{$POINTERMATH ON}


const
//Minimum length to consider as repeating sequence
  NICE_LEN = 5;

// possible horizontal sets and states
  USX_ALPHA = Byte(0);
  USX_SYM   = Byte(1);
  USX_NUM   = Byte(2);
  USX_DICT  = Byte(3);
  USX_DELTA = Byte(4);

// Set (USX_NUM - 2) and vertical code (26) for encoding repeating letters
  RPT_CODE      = Byte((2 shl 5) + 26);
// Set (USX_NUM - 2) and vertical code (27) for encoding terminator
  TERM_CODE     = Byte((2 shl 5) + 27);
// Set (USX_SYM - 1) and vertical code (7) for encoding Line feed
  LF_CODE       = Byte((1 shl 5) + 7);
// Set (USX_NUM - 1) and vertical code (8) for encoding CrLf
  CRLF_CODE     = Byte((1 shl 5) + 8);
// Set (USX_NUM - 1) and vertical code (22) for encoding Cr
  CR_CODE       = Byte((1 shl 5) + 22);
// Set (USX_NUM - 1) and vertical code (14) for encoding Tab
  TAB_CODE      = Byte((1 shl 5) + 14);
// Set (USX_NUM - 2) and vertical code (17) for space character when it appears in USX_NUM state
  NUM_SPC_CODE  = Byte((2 shl 5) + 17);

// Code for special code (11111) when state = USX_DELTA
  UNI_STATE_SPL_CODE     = $F8;
// Length of Code for special code when state = USX_DELTA
  UNI_STATE_SPL_CODE_LEN = 5;
// Code for switch code when state = USX_DELTA
  UNI_STATE_SW_CODE      = $80;
// Length of Code for Switch code when state = USX_DELTA
  UNI_STATE_SW_CODE_LEN  = 2;

// Switch code in USX_ALPHA and USX_NUM
  SW_CODE                = 0;
// Length of Switch code
  SW_CODE_LEN            = 2;
// Terminator bit sequence for Preset 1. Length varies depending on state.
  TERM_BYTE_PRESET_1     = 0;
// Length of Terminator bit sequence when state is lower
  TERM_BYTE_PRESET_1_LEN_LOWER = 6;
// Length of Terminator bit sequence when state is upper
  TERM_BYTE_PRESET_1_LEN_UPPER = 4;

// Offset at which usx_code_94 starts
  USX_OFFSET_94 = 33;

// Mask for retrieving each code to be encoded according to its length.
  usx_mask: array[0..7] of Byte = ($80, $C0, $E0, $F0, $F8, $FC, $FE, $FF);

// count encoding tables
  // Length of bits used to represent count for each level
  count_bit_lens: array[0..4] of Byte = (2, 4, 7, 11, 16);
  // Cumulative counts represented at each level
  count_adder: array[0..4] of Int32   = (4, 20, 148, 2196, 67732);
  // Codes used to specify the level that the count belongs to
  count_codes: array[0..4] of Byte    = ($01, $82, $C3, $E4, $F4);

// Unicode delta coding tables
  // Length of bits used to represent delta code for each level
  uni_bit_len: array[0..4] of Byte = (6, 12, 14, 16, 21);
  // Cumulative delta codes represented at each level
  uni_adder: array[0..4] of Int32  = (0, 64, 4160, 20544, 86080);

// The list of veritical codes is split into 5 sections. Used by readVCodeIdx()
  // Used by readVCodeIdx() for finding the section under which the code read using read8bitCode() falls
  usx_vsections      : array[0..4] of Byte = ($7F, $BF, $DF, $EF, $FF);
  // Used by readVCodeIdx() for finding the section vertical position offset
  usx_vsection_pos   : array[0..4] of Byte = (0, 4, 8, 12, 20);
  // Used by readVCodeIdx() for masking the code read by read8bitCode()
  usx_vsection_mask  : array[0..4] of Byte = ($7F, $3F, $1F, $0F, $0F);
  // Used by readVCodeIdx() for shifting the code read by read8bitCode() to obtain the vpos
  usx_vsection_shift : array[0..4] of Byte = (5, 4, 3, 1, 0);

{ Vertical decoder lookup table - 3 bits code len, 5 bytes vertical pos;
  code len is one less as 8 cannot be accommodated in 3 bits }
  usx_vcode_lookup: array[0..35] of Byte = (
    (1 shl 5) + 0,  (1 shl 5) + 0,  (2 shl 5) + 1,  (2 shl 5) + 2, // Section 1
    (3 shl 5) + 3,  (3 shl 5) + 4,  (3 shl 5) + 5,  (3 shl 5) + 6, // Section 2
    (3 shl 5) + 7,  (3 shl 5) + 7,  (4 shl 5) + 8,  (4 shl 5) + 9, // Section 3
    (5 shl 5) + 10, (5 shl 5) + 10, (5 shl 5) + 11, (5 shl 5) + 11,// Section 4
    (5 shl 5) + 12, (5 shl 5) + 12, (6 shl 5) + 13, (6 shl 5) + 14,
    (6 shl 5) + 15, (6 shl 5) + 15, (6 shl 5) + 16, (6 shl 5) + 16,// Section 5
    (6 shl 5) + 17, (6 shl 5) + 17, (7 shl 5) + 18, (7 shl 5) + 19,
    (7 shl 5) + 20, (7 shl 5) + 21, (7 shl 5) + 22, (7 shl 5) + 23,
    (7 shl 5) + 24, (7 shl 5) + 25, (7 shl 5) + 26, (7 shl 5) + 27
  );

// nibble types
  // USX_NIB_NUM means value between 'a' to 'f'
  USX_NIB_NUM        = 0;
  // USX_NIB_HEX_LOWER means value between 'a' to 'f'
  USX_NIB_HEX_LOWER  = 1;
  // USX_NIB_HEX_UPPER means value between 'A' to 'F'
  USX_NIB_HEX_UPPER  = 2;
  USX_NIB_NOT        = 3;

  //usx_sets: array[0..2, 0..27] of AnsiChar = (
  //  #0' etaoinsrlcdhupmbgwfyvkqjxz',
  //  '"{}_<>:'#10#0'[]\;'''#9'@*&?!^|'#13'~`'#0#0#0,
  //  #0',.01925-/34678() =+$%#'#0#0#0#0#0
  //);
{ This 2D array has the characters for the sets USX_ALPHA, USX_SYM and USX_NUM.
  Where a character cannot fit into a uint8_t, 0 is used and handled in code. }
  usx_sets: array[0..2, 0..27] of Byte = (
    (
              0, Byte(' '), Byte('e'), Byte('t'), Byte('a'), Byte('o'), Byte('i'),
      Byte('n'), Byte('s'), Byte('r'), Byte('l'), Byte('c'), Byte('d'), Byte('h'),
      Byte('u'), Byte('p'), Byte('m'), Byte('b'), Byte('g'), Byte('w'), Byte('f'),
      Byte('y'), Byte('v'), Byte('k'), Byte('q'), Byte('j'), Byte('x'), Byte('z')
    ),
    (
      Byte('"'), Byte('{'), Byte('}'), Byte('_'), Byte('<'), Byte('>'), Byte(':'),
             10,         0, Byte('['), Byte(']'), Byte('\'), Byte(';'), Byte(''''),
              9, Byte('@'), Byte('*'), Byte('&'), Byte('?'), Byte('!'), Byte('^'),
      Byte('|'),        13, Byte('~'), Byte('`'),         0,         0,         0
    ),
    (
              0, Byte(','), Byte('.'), Byte('0'), Byte('1'), Byte('9'), Byte('2'),
      Byte('5'), Byte('-'), Byte('/'), Byte('3'), Byte('4'), Byte('6'), Byte('7'),
      Byte('8'), Byte('('), Byte(')'), Byte(' '), Byte('='), Byte('+'), Byte('$'),
      Byte('%'), Byte('#'),         0,         0,         0,         0,         0
    )
  );

var
{ Stores position of letter in usx_sets. First 3 bits - position in usx_hcodes,
  next 5 bits - position in usx_vcodes }
  usx_code_94: array[0..93] of Byte;

  usx_vcodes : array[0..27] of Byte = (
    $00, $40, $60, $80, $90, $A0, $B0,
    $C0, $D0, $D8, $E0, $E4, $E8, $EC,
    $EE, $F0, $F2, $F4, $F6, $F7, $F8,
    $F9, $FA, $FB, $FC, $FD, $FE, $FF
  );

  usx_vcode_lens: array[0..27] of Byte = (
    2, 3, 3, 4, 4, 4, 4,
    4, 5, 5, 6, 6, 6, 7,
    7, 7, 7, 7, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8
  );
  usx_freq_codes: array[0..5] of Byte = (
    (1 shl 5) + 25, (1 shl 5) + 26, (1 shl 5) + 27,
    (2 shl 5) + 23, (2 shl 5) + 24, (2 shl 5) + 25
  );


{ Appends specified number of bits to the output (out).
  If maximum limit (olen) is reached, -1 is returned.
  Otherwise clen bits in code are appended to out starting with MSB }
function append_bits(outbuf: PAnsiChar; olen, ol: Int32; code: Byte; clen: Int32): Int32;
var
  oidx, blen, cur_bit: Int32;
  a_byte: Byte;
begin
  while clen > 0 do begin
    cur_bit := ol and 7;
    blen := clen;
    a_byte := (code and usx_mask[blen - 1]) shr cur_bit;
    if blen + cur_bit > 8 then
      blen := 8 - cur_bit;
    oidx := ol shr 3;
    if (oidx < 0) or (olen <= oidx) then exit(-1);
    if cur_bit = 0 then
      outbuf[oidx] := AnsiChar(a_byte)
    else
      outbuf[oidx] := AnsiChar(Byte(outbuf[oidx]) or a_byte);
    code := code shl blen;
    Inc(ol, blen);
    Dec(clen, blen);
  end;
  Result := ol;
end;

{$PUSH}{$MACRO ON}{$DEFINE CheckRet := if ol < 0 then exit(ol)}
{ Appends switch code to out depending on the state (USX_DELTA or other) }
function append_switch_code(outbuf: PAnsiChar; olen, ol: Int32; state: Byte): Int32;
begin
  if state = USX_DELTA then begin
    ol := append_bits(outbuf, olen, ol, UNI_STATE_SPL_CODE, UNI_STATE_SPL_CODE_LEN); CheckRet;
    ol := append_bits(outbuf, olen, ol, UNI_STATE_SW_CODE, UNI_STATE_SW_CODE_LEN);
  end else
    ol := append_bits(outbuf, olen, ol, SW_CODE, SW_CODE_LEN);
  Result := ol;
end;

{ Appends given horizontal and veritical code bits to out }
function append_code(outbuf: PAnsiChar; olen, ol: Int32; code: Byte; var state: Byte;
  const usx_hcodes: TUsxHCodes; const usx_hcode_lens: TUsxHCodeLens): Int32;
var
  hcode, vcode: Byte;
begin
  hcode := code shr 5;
  vcode := code and $1F;
  if (usx_hcode_lens[hcode] = 0) and (hcode <> USX_ALPHA) then exit(ol);
  case hcode of
    USX_ALPHA:
      if state <> USX_ALPHA then begin
        ol := append_switch_code(outbuf, olen, ol, state); CheckRet;
        ol := append_bits(outbuf, olen, ol, usx_hcodes[USX_ALPHA], usx_hcode_lens[USX_ALPHA]); CheckRet;
        state := USX_ALPHA;
      end;
    USX_SYM: begin
      ol := append_switch_code(outbuf, olen, ol, state); CheckRet;
      ol := append_bits(outbuf, olen, ol, usx_hcodes[USX_SYM], usx_hcode_lens[USX_SYM]); CheckRet;
    end;
    USX_NUM:
      if state <> USX_NUM then begin
        ol := append_switch_code(outbuf, olen, ol, state); CheckRet;
        ol := append_bits(outbuf, olen, ol, usx_hcodes[USX_NUM], usx_hcode_lens[USX_NUM]); CheckRet;
        if usx_sets[hcode][vcode] in [Byte('0')..Byte('9')] then state := USX_NUM;
      end;
  end;
  Result := append_bits(outbuf, olen, ol, usx_vcodes[vcode], usx_vcode_lens[vcode]);
end;

{ Encodes given count to out }
function encodeCount(outbuf: PAnsiChar; olen, ol, count: Int32): Int32;
var
  i: Int32;
  count16: Word;
begin
  for i := 0 to 4 do begin
    if count < count_adder[i] then begin
      ol := append_bits(outbuf, olen, ol, (count_codes[i] and $F8), count_codes[i] and $07); CheckRet;
      if i = 0 then
        count16 := Word(count) shl (16 - count_bit_lens[i])
      else
        count16 := Word(count - count_adder[i - 1]) shl (16 - count_bit_lens[i]);
      if count_bit_lens[i] > 8 then begin
        ol := append_bits(outbuf, olen, ol, count16 shr 8, 8); CheckRet;
        exit(append_bits(outbuf, olen, ol, count16 and $FF, count_bit_lens[i] - 8));
      end else
        exit(append_bits(outbuf, olen, ol, count16 shr 8, count_bit_lens[i]));
    end;
  end;
  Result := ol; // should not reach
end;

{ Encodes the unicode code point given by code to out; prev_code is used to calculate the delta }
function encodeUnicode(outbuf: PAnsiChar; olen, ol, code, prev_code: Int32): Int32;
const
  codes: array[0..5] of Byte = ($01, $82, $C3, $E4, $F5, $FD);
var
  i, diff, till, val: Int32;
begin
  till := 0;
  diff := code - prev_code;
  if diff < 0 then diff := -diff;
  for i := 0 to 4 do begin
    Inc(till, Int32(1) shl uni_bit_len[i]);
    if diff < till then begin
      ol := append_bits(outbuf, olen, ol, codes[i] and $F8, codes[i] and $07); CheckRet;
      if prev_code > code then
        ol := append_bits(outbuf, olen, ol, $80, 1)
      else
        ol := append_bits(outbuf, olen, ol, 0, 1);
      CheckRet;
      val := diff - uni_adder[i];
      if uni_bit_len[i] > 16 then begin
        val := val shl (24 - uni_bit_len[i]);
        ol := append_bits(outbuf, olen, ol, val shr 16, 8); CheckRet;
        ol := append_bits(outbuf, olen, ol, (val shr 8) and $FF, 8); CheckRet;
        ol := append_bits(outbuf, olen, ol, val and $FF, uni_bit_len[i] - 16);
      end else
        if uni_bit_len[i] > 8 then begin
          val := val shl (16 - uni_bit_len[i]);
          ol := append_bits(outbuf, olen, ol, val shr 8, 8); CheckRet;
          ol := append_bits(outbuf, olen, ol, val and $FF, uni_bit_len[i] - 8);
        end else begin
          val := val shl (8 - uni_bit_len[i]);
          ol := append_bits(outbuf, olen, ol, val and $FF, uni_bit_len[i]);
        end;
      exit(ol);
    end;
  end;
  Result := ol;
end;

{ Reads UTF-8 character from in. Also returns the number of bytes occupied by the UTF-8
  character in utf8len }
function readUTF8(const inbuf: PAnsiChar; len, l: Int32; out utf8len: Int32): Int32;
begin
  Result := 0;
  utf8len := 1;
  if (l < len - 1) and ((Ord(inbuf[l]) and $E0) = $C0) and ((Ord(inbuf[l + 1]) and $C0) = $80) then begin
    utf8len := 2;
    Result := (Ord(inbuf[l]) and $1F) shl 6;
    Inc(Result, Ord(inbuf[l + 1]) and $3F);
    if Result < $80 then Result := 0;
  end else
    if (l < len - 2) and ((Ord(inbuf[l]) and $F0) = $E0) and
       ((Ord(inbuf[l + 1]) and $C0) = $80) and ((Ord(inbuf[l + 2]) and $C0) = $80) then begin
      utf8len := 3;
      Result := (Ord(inbuf[l]) and $0F) shl 12;
      Inc(Result, (Ord(inbuf[l + 1]) and $3F) shl 6);
      Inc(Result, Ord(inbuf[l + 2]) and $3F);
      if Result < $800 then Result := 0;
    end else
      if (l < len - 3) and ((Ord(inbuf[l]) and $F8) = $F0) and
         ((Ord(inbuf[l + 1]) and $C0) = $80) and ((Ord(inbuf[l + 2]) and $C0) = $80) and
         ((Ord(inbuf[l + 3]) and $C0) = $80) then begin
        utf8len := 4;
        Result := (Ord(inbuf[l]) and $07) shl 18;
        Inc(Result, (Ord(inbuf[l + 1]) and $3F) shl 12);
        Inc(Result, (Ord(inbuf[l + 2]) and $3F) shl 6);
        Inc(Result, Ord(inbuf[l + 3]) and $3F);
        if Result < $10000 then Result := 0;
      end;
end;

{ Finds the longest matching sequence from the beginning of the string.
  If a match is found and it is longer than NICE_LEN, it is encoded as a repeating sequence to out.
  This is also used for Unicode strings. This is a crude implementation that is not optimized.
  Assuming only short strings are encoded, this is not much of an issue }
function matchOccurance(const inbuf: PAnsiChar; len, l: Int32; outbuf: PAnsiChar; olen: Int32; var ol: Int32;
  var state: Byte; const usx_hcodes: TUsxHCodes; const usx_hcode_lens: TUsxHCodeLens): Int32;
var
  j, k, match_len, match_dist, longest_dist, longest_len: Int32;
begin
  longest_dist := 0;
  longest_len  := 0;
  for j := l - NICE_LEN downto 0 do begin
    k := l;
    while (k < len) and (j + k - l < l) do begin
      if inbuf[k] <> inbuf[j + k - l] then break;
      Inc(k);
    end;
    while (((Ord(inbuf[k]) shr 6) = 2)) do Dec(k); // Skip partial UTF-8 matches
    if (k - l) > (NICE_LEN - 1) then begin
      match_len  := k - l - NICE_LEN;
      match_dist := l - j - NICE_LEN + 1;
      if match_len > longest_len then begin
        longest_len  := match_len;
        longest_dist := match_dist;
      end;
    end;
  end;

  if longest_len > 0 then begin
    ol := append_switch_code(outbuf, olen, ol, state); CheckRet;
    ol := append_bits(outbuf, olen, ol, usx_hcodes[USX_DICT], usx_hcode_lens[USX_DICT]); CheckRet;
    ol := encodeCount(outbuf, olen, ol, longest_len); CheckRet;
    ol := encodeCount(outbuf, olen, ol, longest_dist); CheckRet;
    Result := Pred(l + longest_len + NICE_LEN);
  end else
    Result := -l;
end;

{ This is used only when encoding a string array. Finds the longest matching sequence
  from the previous array element to the beginning of the string array.
  If a match is found and it is longer than NICE_LEN, it is encoded as a repeating sequence to out.
  This is also used for Unicode strings. This is a crude implementation that is not optimized.
  Assuming only short strings are encoded, this is not much of an issue. }
function matchLine(const inbuf: PAnsiChar; len, l: Int32; outbuf: PAnsiChar; olen: Int32; var ol: Int32;
  prev_lines: PUSLinkList; var state: Byte; const usx_hcodes: TUsxHCodes; const usx_hcode_lens: TUsxHCodeLens): Int32;
var
  last_ol, last_len, last_dist, last_ctx, line_ctr, j, i, k, line_len, limit: Int32;
  cur_prev: PUSLinkList;
begin
  last_ol := ol; last_len := 0;
  last_dist := 0; last_ctx := 0;
  line_ctr := 0; j := 0;
  cur_prev := prev_lines;

  while (cur_prev <> nil) and (cur_prev^.data <> nil) do begin
    line_len := StrLen(cur_prev^.data);
    if line_ctr = 0 then
      limit := l
    else
      limit := line_len;

    while j < limit do begin
      i := l;
      k := j;
      while (k < line_len) and (i < len) do begin
        if cur_prev^.data[k] <> inbuf[i] then break;
        Inc(k);
        Inc(i);
      end;
      while (((Ord(cur_prev^.data[k]) shr 6) = 2)) do Dec(k); // Skip partial UTF-8 matches
      if (k - j) >= NICE_LEN then begin
        if last_len > 0 then begin
          if j > last_dist then continue;
          ol := last_ol;
        end;
        last_len  := k - j;
        last_dist := j;
        last_ctx  := line_ctr;
        ol := append_switch_code(outbuf, olen, ol, state); CheckRet;
        ol := append_bits(outbuf, olen, ol, usx_hcodes[USX_DICT], usx_hcode_lens[USX_DICT]); CheckRet;
        ol := encodeCount(outbuf, olen, ol, last_len - NICE_LEN); CheckRet;
        ol := encodeCount(outbuf, olen, ol, last_dist); CheckRet;
        ol := encodeCount(outbuf, olen, ol, last_ctx); CheckRet;
        Inc(j, last_len);
      end;
    end;
    Inc(line_ctr);
    cur_prev := cur_prev^.previous;
  end;

  if last_len > 0 then
    Result := Pred(l + last_len)
  else
    Result := -l;
end;

function min_of(a, b: Int64): Int64; inline;
begin
  if a < b then Result := a else Result := b;
end;

{ Appends the terminator code depending on the state, preset and whether full
  terminator needs to be encoded to out or not }
function append_final_bits(outbuf: PAnsiChar; olen, ol: Int32; state, is_all_upper: Byte;
  const usx_hcodes: TUsxHCodes; const usx_hcode_lens: TUsxHCodeLens): Int32;
begin
  if usx_hcode_lens[USX_ALPHA] <> 0 then begin
    if state <> USX_NUM then begin
      ol := append_switch_code(outbuf, olen, ol, state); CheckRet;
      ol := append_bits(outbuf, olen, ol, usx_hcodes[USX_NUM], usx_hcode_lens[USX_NUM]); CheckRet;
    end;
    ol := append_bits(outbuf, olen, ol, usx_vcodes[TERM_CODE and $1F], usx_vcode_lens[TERM_CODE and $1F]);
    CheckRet;
  end else begin
    if is_all_upper <> 0 then
      ol := append_bits(outbuf, olen, ol, TERM_BYTE_PRESET_1, TERM_BYTE_PRESET_1_LEN_UPPER)
    else
      ol := append_bits(outbuf, olen, ol, TERM_BYTE_PRESET_1, TERM_BYTE_PRESET_1_LEN_LOWER);
    CheckRet;
  end;
  if (ol = 0) or (Int8(outbuf[(ol-1) shr 3]) shl ((ol-1) and 7) >= 0) then
      Result := append_bits(outbuf, olen, ol, 0, 8 - ol and 7)
    else
      Result := append_bits(outbuf, olen, ol, $FF, 8 - ol and 7);
end;

{ Starts coding of nibble sets }
function append_nibble_escape(outbuf: PAnsiChar; olen, ol: Int32; state: Byte;
  const usx_hcodes: TUsxHCodes; const usx_hcode_lens: TUsxHCodeLens): Int32;
begin
  ol := append_switch_code(outbuf, olen, ol, state); CheckRet;
  ol := append_bits(outbuf, olen, ol, usx_hcodes[USX_NUM], usx_hcode_lens[USX_NUM]); CheckRet;
  Result := append_bits(outbuf, olen, ol, 0, 2);
end;
{$UNDEF CheckRet}
{$POP}

{ Returns 4 bit code assuming ch falls between '0' to '9', 'A' to 'F' or 'a' to 'f' }
function getBaseCode(ch: AnsiChar): Byte; inline;
begin
  case ch of
    '0'..'9': Result := (Ord(ch) - Ord('0')) shl 4;
    'A'..'F': Result := (Ord(ch) - Ord('A') + 10) shl 4;
    'a'..'f': Result := (Ord(ch) - Ord('a') + 10) shl 4;
  else        Result := 0;
  end;
end;

{ Gets 4 bit code assuming ch falls between '0' to '9', 'A' to 'F' or 'a' to 'f' }
function getNibbleType(ch: AnsiChar): Int32; inline;
begin
  case ch of
    '0'..'9': Result := USX_NIB_NUM;
    'a'..'f': Result := USX_NIB_HEX_LOWER;
    'A'..'F': Result := USX_NIB_HEX_UPPER;
  else        Result := USX_NIB_NOT;
  end;
end;

{ Returns hex character corresponding to the 4 bit nibble }
function getHexChar(nibble, hex_type: Int32): AnsiChar; inline;
begin
  if (nibble >= 0) and (nibble <= 9) then
    Result := AnsiChar(Ord('0') + nibble)
  else
    if hex_type < USX_NIB_HEX_UPPER then
      Result := AnsiChar(Ord('a') + nibble - 10)
    else
      Result := AnsiChar(Ord('A') + nibble - 10);
end;

{$PUSH}{$MACRO ON}{$DEFINE CheckRet := if ol < 0 then exit(olen + 1)}
function unishox2_compress_lines(const inbuf: PAnsiChar; ilen: Int32; outbuf: PAnsiChar; olen: Int32;
  const usx_hcodes: TUsxHCodes; const usx_hcode_lens: TUsxHCodeLens; usx_freq_seq: PUsxFreqSeq;
  usx_templates: PUsxTemplates; prev_lines: PUSLinkList): Int32;
var
  l, ll, ol, prev_uni, utf8len, seq_len, i, j, k, rem, bin_count,
  rpt_count, uid_pos, hex_len, hex_type, nib_type, uni, uni2: Int32;
  c_in, c_next, c_t: AnsiChar;
  state: Byte;
  is_upper, is_all_upper: Boolean;
begin
  ol := 0;
  prev_uni := 0;
  state := USX_ALPHA;
  is_all_upper := False;

  // append magic bits
  ol := append_bits(outbuf, olen, ol, UNISHOX_MAGIC_BITS, UNISHOX_MAGIC_BIT_LEN); CheckRet;

  l := 0;
  while l < ilen do begin
    // dictionary matching
    if (usx_hcode_lens[USX_DICT] <> 0) and (l < (ilen - NICE_LEN + 1)) then
      if prev_lines <> nil then begin
        l := matchLine(inbuf, ilen, l, outbuf, olen, ol, prev_lines, state, usx_hcodes, usx_hcode_lens);
        if l > 0 then begin
          Inc(l); continue;
        end;
        if (l < 0) and (ol < 0) then exit(olen + 1);
        l := -l;
      end else begin
        l := matchOccurance(inbuf, ilen, l, outbuf, olen, ol, state, usx_hcodes, usx_hcode_lens);
        if l > 0 then begin
          Inc(l); continue;
        end;
        if (l < 0) and (ol < 0) then exit(olen + 1);
        l := -l;
      end;

    c_in := inbuf[l];

    // repetition of 5+ identical characters
    if (l > 0) and (ilen > 4) and (l < ilen - 4) and (usx_hcode_lens[USX_NUM] <> 0) then
      if (c_in = inbuf[l - 1]) and (c_in = inbuf[l + 1]) and (c_in = inbuf[l + 2]) and (c_in = inbuf[l + 3]) then begin
        rpt_count := l + 4;
        while (rpt_count < ilen) and (inbuf[rpt_count] = c_in) do Inc(rpt_count);
        rpt_count -= l;
        ol := append_code(outbuf, olen, ol, RPT_CODE, state, usx_hcodes, usx_hcode_lens); CheckRet;
        ol := encodeCount(outbuf, olen, ol, rpt_count - 4); CheckRet;
        l += rpt_count;
        continue;
      end;

    // GUID matching
    if (l <= ilen - 36) and (usx_hcode_lens[USX_NUM] <> 0) then
      if (inbuf[l + 8] = '-') and (inbuf[l + 13] = '-') and (inbuf[l + 18] = '-') and (inbuf[l + 23] = '-') then begin
        hex_type := USX_NIB_NUM;
        uid_pos := l;
        while uid_pos < l + 36 do begin
          c_in := inbuf[uid_pos];
          if (c_in = '-') and ((uid_pos = 8) or (uid_pos = 13) or (uid_pos = 18) or (uid_pos = 23)) then begin
            Inc(uid_pos); continue;
          end;
          nib_type := getNibbleType(c_in);
          if nib_type = USX_NIB_NOT then break;
          if nib_type <> USX_NIB_NUM then begin
            if (hex_type <> USX_NIB_NUM) and (hex_type <> nib_type) then break;
            hex_type := nib_type;
          end;
          Inc(uid_pos);
        end;
        if uid_pos = l + 36 then begin
          ol := append_nibble_escape(outbuf, olen, ol, state, usx_hcodes, usx_hcode_lens); CheckRet;
          if hex_type = USX_NIB_HEX_LOWER then
            ol := append_bits(outbuf, olen, ol, $C0, 3)
          else
            ol := append_bits(outbuf, olen, ol, $F0, 5);
          CheckRet;
          for uid_pos := l to l + 35 do begin
            c_in := inbuf[uid_pos];
            if c_in <> '-' then begin
              ol := append_bits(outbuf, olen, ol, getBaseCode(c_in), 4); CheckRet;
            end;
          end;
          l += 36; continue;
        end;
      end;

    // hex encoding detection
    if (l < ilen - 5) and (usx_hcode_lens[USX_NUM] <> 0) then begin
      hex_type := USX_NIB_NUM;
      hex_len := 0;
      repeat
        nib_type := getNibbleType(inbuf[l + hex_len]);
        if nib_type = USX_NIB_NOT then break;
        if nib_type <> USX_NIB_NUM then begin
          if (hex_type <> USX_NIB_NUM) and (hex_type <> nib_type) then break;
          hex_type := nib_type;
        end;
        Inc(hex_len);
      until l + hex_len >= ilen;
      if (hex_len > 10) and (hex_type = USX_NIB_NUM) then
        hex_type := USX_NIB_HEX_LOWER;
      if ((hex_type = USX_NIB_HEX_LOWER) or (hex_type = USX_NIB_HEX_UPPER)) and (hex_len > 3) then begin
        ol := append_nibble_escape(outbuf, olen, ol, state, usx_hcodes, usx_hcode_lens); CheckRet;
        if hex_type = USX_NIB_HEX_LOWER then
          ol := append_bits(outbuf, olen, ol, $80, 2)
        else
          ol := append_bits(outbuf, olen, ol, $E0, 4);
        CheckRet;
        ol := encodeCount(outbuf, olen, ol, hex_len); CheckRet;
        while hex_len > 0 do begin
          ol := append_bits(outbuf, olen, ol, getBaseCode(inbuf[l]), 4); CheckRet;
          Inc(l); Dec(hex_len);
        end;
        continue;
      end;
    end;

    // templates
    if usx_templates <> nil then
      if usx_templates^[0] <> nil then begin
        i := 0;
        while i < 5 do begin
          if usx_templates^[i] <> nil then begin
            rem := StrLen(usx_templates^[i]);
            j := 0;
            while (j < rem) and (l + j < ilen) do begin
              c_t := usx_templates^[i][j];
              c_in := inbuf[l + j];
              case c_t of
                'F': if (getNibbleType(c_in) <> USX_NIB_HEX_UPPER) and
                        (getNibbleType(c_in) <> USX_NIB_NUM) then break;
                'f': if (getNibbleType(c_in) <> USX_NIB_HEX_LOWER) and
                        (getNibbleType(c_in) <> USX_NIB_NUM) then break;
                'o': if (c_in <> '0') and (c_in <> '1') then break;
                'r': if not(c_in in ['0'..'7']) then break;
                't': if not(c_in in ['0'..'3']) then break;
              else
                if c_t <> c_in then break;
              end;
              Inc(j);
            end;
            if rem = 0 then rem := 1;
            if j / rem > 0.66 then begin
              rem := rem - j;
              ol := append_nibble_escape(outbuf, olen, ol, state, usx_hcodes, usx_hcode_lens); CheckRet;
              ol := append_bits(outbuf, olen, ol, 0, 1); CheckRet;
              ol := append_bits(outbuf, olen, ol, (count_codes[i] and $F8), count_codes[i] and $07); CheckRet;
              ol := encodeCount(outbuf, olen, ol, rem); CheckRet;
              for k := 0 to j - 1 do begin
                c_t := usx_templates^[i][k];
                if c_t in ['F', 'f', 'o', 'r', 't'] then begin
                  case c_t of
                    'F', 'f': ol := append_bits(outbuf, olen, ol, getBaseCode(inbuf[l + k]), 4);
                    'r': ol := append_bits(outbuf, olen, ol, (Ord(inbuf[l + k]) - Ord('0')) shl 5, 3);
                    't': ol := append_bits(outbuf, olen, ol, (Ord(inbuf[l + k]) - Ord('0')) shl 6, 2);
                  else
                    ol := append_bits(outbuf, olen, ol, (Ord(inbuf[l + k]) - Ord('0')) shl 7, 1);
                  end; CheckRet;
                end;
              end;
              l += j; break;
            end;
          end;
          Inc(i);
        end;
        if i < 5 then continue;
      end;

    // frequent sequences
    if usx_freq_seq <> nil then begin
      i := 0;
      while i < 6 do begin
        seq_len := StrLen(usx_freq_seq^[i]);
        if (ilen - seq_len >= 0) and (l <= ilen - seq_len) then begin
          if (CompareByte(inbuf[l], usx_freq_seq^[i]^, seq_len) = 0) and
              (usx_hcode_lens[usx_freq_codes[i] shr 5] <> 0) then begin
            ol := append_code(outbuf, olen, ol, usx_freq_codes[i], state, usx_hcodes, usx_hcode_lens); CheckRet;
            l += seq_len; break;
          end;
        end;
        Inc(i);
      end;
      if i < 6 then continue;
    end;

    c_in := inbuf[l];
    is_upper := False;
    if c_in in ['A'..'Z'] then
      is_upper := True
    else
      if is_all_upper then begin
        is_all_upper := False;
        ol := append_switch_code(outbuf, olen, ol, state); CheckRet;
        ol := append_bits(outbuf, olen, ol, usx_hcodes[USX_ALPHA], usx_hcode_lens[USX_ALPHA]); CheckRet;
        state := USX_ALPHA;
      end;

    if is_upper and not is_all_upper then begin
      if state = USX_NUM then begin
        ol := append_switch_code(outbuf, olen, ol, state); CheckRet;
        ol := append_bits(outbuf, olen, ol, usx_hcodes[USX_ALPHA], usx_hcode_lens[USX_ALPHA]); CheckRet;
        state := USX_ALPHA;
      end;
      ol := append_switch_code(outbuf, olen, ol, state); CheckRet;
      ol := append_bits(outbuf, olen, ol, usx_hcodes[USX_ALPHA], usx_hcode_lens[USX_ALPHA]); CheckRet;
      if state = USX_DELTA then begin
        state := USX_ALPHA;
        ol := append_switch_code(outbuf, olen, ol, state); CheckRet;
        ol := append_bits(outbuf, olen, ol, usx_hcodes[USX_ALPHA], usx_hcode_lens[USX_ALPHA]); CheckRet;
      end;
    end;

    if l + 1 < ilen then
      c_next := inbuf[l + 1]
    else
      c_next := #0;

    if c_in in [' '..'~'] then begin
      if is_upper and not is_all_upper then begin
        ll := l + 4;
        while (ll >= l) and (ll < ilen) do begin
          if not (inbuf[ll] in ['A'..'Z']) then break;
          Dec(ll);
        end;
        if ll = l - 1 then begin
          ol := append_switch_code(outbuf, olen, ol, state); CheckRet;
          ol := append_bits(outbuf, olen, ol, usx_hcodes[USX_ALPHA], usx_hcode_lens[USX_ALPHA]); CheckRet;
          state := USX_ALPHA;
          is_all_upper := True;
        end;
      end;

      if (state = USX_DELTA) and (c_in in [' ', '.', ',']) then begin
        // special codes in delta state
        ol := append_bits(outbuf, olen, ol, UNI_STATE_SPL_CODE, UNI_STATE_SPL_CODE_LEN); CheckRet;
        case c_in of
          ' ': ol := append_bits(outbuf, olen, ol, $00, 1);
          ',': ol := append_bits(outbuf, olen, ol, $C0, 3);
          '.': ol := append_bits(outbuf, olen, ol, $E0, 4);
        else end; CheckRet;
        Inc(l); continue;
      end;
      c_in := AnsiChar(Ord(c_in) - 32);
      if is_all_upper and is_upper then
        c_in := AnsiChar(Ord(c_in) + 32);
      if c_in = #0 then begin
        if state = USX_NUM then
          ol := append_bits(outbuf, olen, ol, usx_vcodes[NUM_SPC_CODE and $1F], usx_vcode_lens[NUM_SPC_CODE and $1F])
        else
          ol := append_bits(outbuf, olen, ol, usx_vcodes[1], usx_vcode_lens[1]);
      end else begin
        c_in := AnsiChar(Ord(c_in) - 1);
        ol := append_code(outbuf, olen, ol, usx_code_94[Ord(c_in)], state, usx_hcodes, usx_hcode_lens);
      end;
      CheckRet;
    end else
      if (c_in = #13) and (c_next = #10) then begin
        ol := append_code(outbuf, olen, ol, CRLF_CODE, state, usx_hcodes, usx_hcode_lens); CheckRet;
        Inc(l);
      end else
        if c_in = #10 then begin
          if state = USX_DELTA then begin
            ol := append_bits(outbuf, olen, ol, UNI_STATE_SPL_CODE, UNI_STATE_SPL_CODE_LEN); CheckRet;
            ol := append_bits(outbuf, olen, ol, $F0, 4);
          end else
            ol := append_code(outbuf, olen, ol, LF_CODE, state, usx_hcodes, usx_hcode_lens);
          CheckRet;
        end else
          if c_in = #13 then begin
            ol := append_code(outbuf, olen, ol, CR_CODE, state, usx_hcodes, usx_hcode_lens); CheckRet;
          end else
            if c_in = #9 then begin
              ol := append_code(outbuf, olen, ol, TAB_CODE, state, usx_hcodes, usx_hcode_lens); CheckRet;
            end else begin
              uni := readUTF8(inbuf, ilen, l, utf8len);
              if uni <> 0 then begin
                Inc(l, utf8len);
                if state <> USX_DELTA then begin
                  uni2 := readUTF8(inbuf, ilen, l, utf8len);
                  if uni2 <> 0 then begin
                    if state <> USX_ALPHA then begin
                      ol := append_switch_code(outbuf, olen, ol, state); CheckRet;
                      ol := append_bits(outbuf, olen, ol, usx_hcodes[USX_ALPHA], usx_hcode_lens[USX_ALPHA]); CheckRet;
                    end;
                    ol := append_switch_code(outbuf, olen, ol, state); CheckRet;
                    ol := append_bits(outbuf, olen, ol, usx_hcodes[USX_ALPHA], usx_hcode_lens[USX_ALPHA]); CheckRet;
                    ol := append_bits(outbuf, olen, ol, usx_vcodes[1], usx_vcode_lens[1]); CheckRet; // space
                    state := USX_DELTA;
                  end else begin
                    ol := append_switch_code(outbuf, olen, ol, state); CheckRet;
                    ol := append_bits(outbuf, olen, ol, usx_hcodes[USX_DELTA], usx_hcode_lens[USX_DELTA]); CheckRet;
                  end;
                end;
                ol := encodeUnicode(outbuf, olen, ol, uni, prev_uni); CheckRet;
                prev_uni := uni;
                Dec(l);
              end else begin
                bin_count := 1;
                for i := l + 1 to ilen - 1 do begin
                  if readUTF8(inbuf, ilen, i, utf8len) <> 0 then break;
                  if (i < ilen - 4) and (inbuf[i] = inbuf[i - 1]) and (inbuf[i] = inbuf[i + 1]) and
                     (inbuf[i] = inbuf[i + 2]) and (inbuf[i] = inbuf[i + 3]) then break;
                  Inc(bin_count);
                end;
                ol := append_nibble_escape(outbuf, olen, ol, state, usx_hcodes, usx_hcode_lens); CheckRet;
                ol := append_bits(outbuf, olen, ol, $F8, 5); CheckRet;
                ol := encodeCount(outbuf, olen, ol, bin_count); CheckRet;
                for i := 1 to bin_count do begin
                  ol := append_bits(outbuf, olen, ol, Ord(inbuf[l]), 8); CheckRet;
                  Inc(l);
                end;
                Dec(l);
              end;
            end;
    Inc(l);
  end;

  Result := (ol + 7) shr 3;
  append_final_bits(outbuf, Result, ol, state, Byte(is_all_upper), usx_hcodes, usx_hcode_lens);
end;
{$UNDEF CheckRet}
{$POP}

{ Reads one bit from inbuf }
function readBit(const inbuf: PAnsiChar; bit_no: Int32): Int32; inline;
begin
  Result := Integer((Byte(inbuf[bit_no shr 3]) and ($80 shr (bit_no and 7))) <> 0);
end;

{ Reads next 8 bits, if available }
function read8bitCode(const inbuf: PAnsiChar; len_bits, bit_no: Int32): Byte;
var
  bit_pos, char_pos, byte_len: Int32;
begin
  bit_pos := bit_no and 7;
  char_pos := bit_no shr 3;
  byte_len := len_bits shr 3;
  Result := Byte(inbuf[char_pos]) shl bit_pos;
  Inc(char_pos);
  if char_pos < byte_len then
    Result := Result or (Byte(inbuf[char_pos]) shr (8 - bit_pos))
  else
    Result := Result or ($FF shr (8 - bit_pos));
end;

{ des the vertical code from the given bitstream at inbuf }
function readVCodeIdx(const inbuf: PAnsiChar; len_bits: Int32; var bit_no: Int32): Int32;
var
  i: Int32;
  code, vcode: Byte;
begin
  if bit_no < len_bits then begin
    code := read8bitCode(inbuf, len_bits, bit_no);
    for i := 0 to 4 do begin
      if code <= usx_vsections[i] then begin
        vcode := usx_vcode_lookup[usx_vsection_pos[i] + ((code and usx_vsection_mask[i]) shr usx_vsection_shift[i])];
        Inc(bit_no, (vcode shr 5) + 1);
        if bit_no > len_bits then exit(99);
        exit(vcode and $1F);
      end;
    end;
  end;
  Result := 99;
end;

{ codes the horizontal code from the given bitstream at inbuf }
function readHCodeIdx(const inbuf: PAnsiChar; len_bits: Int32; var bit_no: Int32; const usx_hcodes: TUsxHCodes;
  const usx_hcode_lens: TUsxHCodeLens): Int32;
var
  code_pos: Int32;
  code: Byte;
begin
  if usx_hcode_lens[USX_ALPHA] = 0 then exit(USX_ALPHA);
  if bit_no < len_bits then begin
    code := read8bitCode(inbuf, len_bits, bit_no);
    for code_pos := 0 to 4 do begin
      if (usx_hcode_lens[code_pos] <> 0) and
         ((code and usx_mask[usx_hcode_lens[code_pos] - 1]) = usx_hcodes[code_pos]) then begin
        bit_no += usx_hcode_lens[code_pos];
        exit(code_pos);
      end;
    end;
  end;
  Result := 99;
end;

{ Returns the position of step code (0, 10, 110, etc.) encountered in the stream }
function getStepCodeIdx(const inbuf: PAnsiChar; len_bits: Int32; var bit_no: Int32; limit: Int32): Int32;
begin
  Result := 0;
  while (bit_no < len_bits) and (readBit(inbuf, bit_no) <> 0) do begin
    Inc(Result);
    Inc(bit_no);
    if Result = limit then exit;
  end;
  if bit_no >= len_bits then exit(99);
  Inc(bit_no);
end;

{ Reads specified number of bits and builds the corresponding integer }
function getNumFromBits(const inbuf: PAnsiChar; len_bits, bit_no, count: Int32): Int32;
var
  ret: Int32;
begin
  ret := 0;
  while (count > 0) and (bit_no < len_bits) do begin
    Dec(count);
    if readBit(inbuf, bit_no) <> 0 then ret += 1 shl count;
    Inc(bit_no);
  end;
  //if count <= 0 then
  if count = 0 then
    Result := ret
  else
    Result := -1;
end;

{ Decodes the count from the given bit stream at inbuf. Also updates bit_no }
function readCount(const inbuf: PAnsiChar; var bit_no: Int32; len_bits: Int32): Int32;
var
  idx: Int32;
begin
  idx := getStepCodeIdx(inbuf, len_bits, bit_no, 4);
  if idx = 99 then exit(-1);
  if bit_no + count_bit_lens[idx] - 1 >= len_bits then exit(-1);
  Result := getNumFromBits(inbuf, len_bits, bit_no, count_bit_lens[idx]);
  if idx > 0 then Result += count_adder[idx - 1];
  bit_no += count_bit_lens[idx];
end;

{ Decodes the Unicode codepoint from the given bit stream at inbuf. Also updates bit_no.
  When the step code is 5, reads the next step code to find out the special code. }
function readUnicode(const inbuf: PAnsiChar; var bit_no: Int32; len_bits: Int32): Int32;
var
  idx, sign, count: Int32;
begin
  idx := getStepCodeIdx(inbuf, len_bits, bit_no, 5);
  if idx = 99 then exit($7FFFFF00 + 99);
  if idx = 5 then begin
    idx := getStepCodeIdx(inbuf, len_bits, bit_no, 4);
    exit($7FFFFF00 + idx);
  end;
  if idx >= 0 then begin
    if bit_no < len_bits then
      sign := readBit(inbuf, bit_no)
    else
      sign := 0;
    Inc(bit_no);
    if bit_no + uni_bit_len[idx] - 1 >= len_bits then exit($7FFFFF00 + 99);
    count := getNumFromBits(inbuf, len_bits, bit_no, uni_bit_len[idx]);
    count += uni_adder[idx];
    bit_no += uni_bit_len[idx];
    if sign <> 0 then
      exit(-count)
    else
      exit(count);
  end;
  Result := 0;
end;

{ Write given unicode code point to outbuf as a UTF-8 sequence }
function writeUTF8(outbuf: PAnsiChar; olen, ol, uni: Int32): Int32;
begin
  if uni < (1 shl 11) then begin
    if olen <= ol then exit(olen + 1);
    outbuf[ol] := AnsiChar($C0 + (uni shr 6));
    Inc(ol);
    if olen <= ol then exit(olen + 1);
    outbuf[ol] := AnsiChar($80 + (uni and $3F));
    Inc(ol);
  end else
    if uni < (1 shl 16) then begin
      if olen <= ol then exit(olen + 1);
      outbuf[ol] := AnsiChar($E0 + (uni shr 12));
      Inc(ol);
      if olen <= ol then exit(olen + 1);
      outbuf[ol] := AnsiChar($80 + ((uni shr 6) and $3F));
      Inc(ol);
      if olen <= ol then exit(olen + 1);
      outbuf[ol] := AnsiChar($80 + (uni and $3F));
      Inc(ol);
    end else begin
      if olen <= ol then exit(olen + 1);
      outbuf[ol] := AnsiChar($F0 + (uni shr 18));
      Inc(ol);
      if olen <= ol then exit(olen + 1);
      outbuf[ol] := AnsiChar($80 + ((uni shr 12) and $3F));
      Inc(ol);
      if olen <= ol then exit(olen + 1);
      outbuf[ol] := AnsiChar($80 + ((uni shr 6) and $3F));
      Inc(ol);
      if olen <= ol then exit(olen + 1);
      outbuf[ol] := AnsiChar($80 + (uni and $3F));
      Inc(ol);
    end;
  Result := ol;
end;

{ Decode repeating sequence and appends to outbuf. }
function decodeRepeat(const inbuf: PAnsiChar; len_bits: Int32; outbuf: PAnsiChar; olen, ol: Int32;
  var bit_no: Int32; prev_lines: PUSLinkList): Int32;
var
  dict_len, dist, ctx, left: Int32;
  cur_line: PUSLinkList;
begin
  if prev_lines <> nil then begin
    dict_len := readCount(inbuf, bit_no, len_bits) + NICE_LEN;
    if dict_len < NICE_LEN then exit(-1);
    dist := readCount(inbuf, bit_no, len_bits);
    if dist < 0 then exit(-1);
    ctx := readCount(inbuf, bit_no, len_bits);
    if ctx < 0 then exit(-1);
    cur_line := prev_lines;
    while (ctx > 0) and (cur_line <> nil) do begin
      cur_line := cur_line^.previous;
      Dec(ctx);
    end;
    if cur_line = nil then exit(-1);
    left := olen - ol;
    if left <= 0 then exit(olen + 1);
    if dist >= StrLen(cur_line^.data) then exit(-1);
    System.Move(cur_line^.data[dist], outbuf[ol], min_of(left, dict_len));
    if left < dict_len then exit(olen + 1);
    Inc(ol, dict_len);
  end else begin
    dict_len := readCount(inbuf, bit_no, len_bits) + NICE_LEN;
    if dict_len < NICE_LEN then exit(-1);
    dist := readCount(inbuf, bit_no, len_bits) + NICE_LEN - 1;
    if dist < NICE_LEN - 1 then exit(-1);
    left := olen - ol;
    if left <= 0 then exit(olen + 1);
    if ol - dist < 0 then exit(-1);
    System.Move(outbuf[ol - dist], outbuf[ol], min_of(left, dict_len));
    if left < dict_len then exit(olen + 1);
    Inc(ol, dict_len);
  end;
  Result := ol;
end;

function unishox2_decompress_lines(const inbuf: PAnsiChar; ilen: Int32; outbuf: PAnsiChar; olen: Int32;
  const usx_hcodes: TUsxHCodes; const usx_hcode_lens: TUsxHCodeLens; usx_freq_seq: PUsxFreqSeq;
  usx_templates: PUsxTemplates; prev_lines: PUSLinkList): Int32;
var
  bit_no, ol, orig_bit_no, j, rem, tlen, left, freqlen, idx, v, rpt_ret, delta,
  spl_code_idx, raw_char, count, nibble_count, nibble, prev_uni, bin_count: Int32;
  dstate, h, b: Byte;
  c, c_t: AnsiChar;
  is_upper, is_all_upper, eof: Boolean;
begin
  ol := 0;
  bit_no := UNISHOX_MAGIC_BIT_LEN;// ignore the magic bit
  dstate := USX_ALPHA;
  h := USX_ALPHA;
  is_all_upper := False;
  prev_uni := 0;
  ilen := ilen shl 3; // in bits

  while bit_no < ilen do begin
    orig_bit_no := bit_no;
    if (dstate = USX_DELTA) or (h = USX_DELTA) then begin
      if dstate <> USX_DELTA then
        h := dstate;
      delta := readUnicode(inbuf, bit_no, ilen);
      if (delta shr 8) = $7FFFFF then begin
        spl_code_idx := delta and $FF;
        if spl_code_idx = 99 then break;
        case spl_code_idx of
          0: begin
               if olen <= ol then exit(olen + 1);
               outbuf[ol] := ' ';
               Inc(ol);
               continue;
             end;
          1: begin
               h := readHCodeIdx(inbuf, ilen, bit_no, usx_hcodes, usx_hcode_lens);
               if h = 99 then begin bit_no := ilen; continue; end;
               if (h = USX_DELTA) or (h = USX_ALPHA) then begin
                 dstate := h;
                 continue;
               end;
               if h = USX_DICT then begin
                 rpt_ret := decodeRepeat(inbuf, ilen, outbuf, olen, ol, bit_no, prev_lines);
                 if rpt_ret < 0 then exit(ol);
                 ol := rpt_ret;
                 if ol > olen then exit(olen + 1);
                 h := dstate;
                 continue;
               end;
             end;
          2: begin
               if olen <= ol then exit(olen + 1);
               outbuf[ol] := ',';
               Inc(ol);
               continue;
             end;
          3: begin
               if olen <= ol then exit(olen + 1);
               outbuf[ol] := '.';
               Inc(ol);
               continue;
             end;
          4: begin
               if olen <= ol then exit(olen + 1);
               outbuf[ol] := #10;
               Inc(ol);
               continue;
             end;
        end;
      end else begin
        Inc(prev_uni, delta);
        ol := writeUTF8(outbuf, olen, ol, prev_uni);
        if ol > olen then exit(olen + 1);
      end;
      if (dstate = USX_DELTA) and (h = USX_DELTA) then
        continue;
    end else
      h := dstate;

    c := #0;
    is_upper := is_all_upper;
    v := readVCodeIdx(inbuf, ilen, bit_no);
    if (v = 99) or (h = 99) then begin
      bit_no := orig_bit_no;
      break;
    end;

    if (v = 0) and (h <> USX_SYM) then begin
      if bit_no >= ilen then break;
      if (h <> USX_NUM) or (dstate <> USX_DELTA) then begin
        h := readHCodeIdx(inbuf, ilen, bit_no, usx_hcodes, usx_hcode_lens);
        if (h = 99) or (bit_no >= ilen) then begin
          bit_no := orig_bit_no; break;
        end;
      end;
      if h = USX_ALPHA then begin
        if dstate = USX_ALPHA then begin
          if usx_hcode_lens[USX_ALPHA] = 0 then begin
            b := read8bitCode(inbuf, ilen, bit_no - SW_CODE_LEN);
            if is_all_upper then
              b := b and Byte(($FF shl (8 - TERM_BYTE_PRESET_1_LEN_UPPER)) and $FF)
            else
              b := b and Byte(($FF shl (8 - TERM_BYTE_PRESET_1_LEN_LOWER)) and $FF);
            if b = TERM_BYTE_PRESET_1 then break;// Terminator for preset 1
          end;
          if is_all_upper then begin
            is_upper := False;
            is_all_upper := False;
            continue;
          end;
          v := readVCodeIdx(inbuf, ilen, bit_no);
          if v = 99 then begin bit_no := orig_bit_no; break; end;
          if v = 0 then begin
            h := readHCodeIdx(inbuf, ilen, bit_no, usx_hcodes, usx_hcode_lens);
            if h = 99 then begin
              bit_no := orig_bit_no; break;
            end;
            if h = USX_ALPHA then begin
              is_all_upper := True; continue;
            end;
          end;
          is_upper := True;
        end else begin
          dstate := USX_ALPHA;
          continue;
        end;
      end else
        if h = USX_DICT then begin
          rpt_ret := decodeRepeat(inbuf, ilen, outbuf, olen, ol, bit_no, prev_lines);
          if rpt_ret < 0 then break;
          ol := rpt_ret;
          if ol > olen then exit(olen + 1);
          continue;
        end else
          if h = USX_DELTA then begin
            continue;
          end else begin
            if (h <> USX_NUM) or (dstate <> USX_DELTA) then
              v := readVCodeIdx(inbuf, ilen, bit_no);
            if v = 99 then begin
              bit_no := orig_bit_no; break;
            end;
            if (h = USX_NUM) and (v = 0) then begin
              idx := getStepCodeIdx(inbuf, ilen, bit_no, 5);
              if idx = 99 then break;
              if idx = 0 then begin
                idx := getStepCodeIdx(inbuf, ilen, bit_no, 4);
                if idx >= 5 then break;
                if (idx < 0) or (idx > 4) or (usx_templates^[idx] = nil) then break;
                tlen := StrLen(usx_templates^[idx]);
                rem := readCount(inbuf, bit_no, ilen);
                if rem < 0 then break;
                if rem > tlen then break;
                rem := tlen - rem;
                eof := False;
                for j := 0 to rem - 1 do begin
                  c_t := usx_templates^[idx][j];
                  if c_t in ['F', 'f', 'r', 'o', 't'] then begin
                    case c_t of
                      'F', 'f': nibble_count := 4;
                      'r':      nibble_count := 3;
                      't':      nibble_count := 2;
                    else        nibble_count := 1;
                    end;
                    raw_char := getNumFromBits(inbuf, ilen, bit_no, nibble_count);
                    if raw_char < 0 then begin
                      eof := True; break;
                    end;
                    if olen <= ol then exit(olen + 1);
                    if c_t = 'f' then
                      outbuf[ol] := getHexChar(raw_char, USX_NIB_HEX_LOWER)
                    else
                      outbuf[ol] := getHexChar(raw_char, USX_NIB_HEX_UPPER);
                    Inc(ol);
                    Inc(bit_no, nibble_count);
                  end else begin
                    if olen <= ol then exit(olen + 1);
                    outbuf[ol] := c_t;
                    Inc(ol);
                  end;
                end;
                if eof then break; // reach input eof
              end else
                if idx = 5 then begin
                  bin_count := readCount(inbuf, bit_no, ilen);
                  if bin_count <= 0 then break;
                  while bin_count > 0 do begin
                    raw_char := getNumFromBits(inbuf, ilen, bit_no, 8);
                    if raw_char < 0 then break;
                    if olen <= ol then exit(olen + 1);
                    outbuf[ol] := AnsiChar(raw_char);
                    Inc(ol);
                    bit_no += 8;
                    Dec(bin_count);
                  end;
                  if bin_count > 0 then break; // reach input eof
                end else begin
                  if (idx = 2) or (idx = 4) then
                    nibble_count := 32
                  else begin
                    nibble_count := readCount(inbuf, bit_no, ilen);
                    if nibble_count <= 0 then break;
                  end;
                  while nibble_count > 0 do begin
                    nibble := getNumFromBits(inbuf, ilen, bit_no, 4);
                    if nibble < 0 then break;
                    if olen <= ol then exit(olen + 1);
                    if idx < 3 then
                      outbuf[ol] := getHexChar(nibble, USX_NIB_HEX_LOWER)
                    else
                      outbuf[ol] := getHexChar(nibble, USX_NIB_HEX_UPPER);
                    Inc(ol);
                    if ((idx = 2) or (idx = 4)) and ((nibble_count = 25) or
                       (nibble_count = 21) or (nibble_count = 17) or (nibble_count = 13)) then begin
                      if olen <= ol then exit(olen + 1);
                      outbuf[ol] := '-';
                      Inc(ol);
                    end;
                    Inc(bit_no, 4);
                    Dec(nibble_count);
                  end;
                  if nibble_count > 0 then break; // reach input eof
                end;
                if dstate = USX_DELTA then h := USX_DELTA;
                continue;
              end;
            end;
    end;

    if is_upper and (v = 1) then begin // continuous delta coding
      h := USX_DELTA;
      dstate := USX_DELTA;
      continue;
    end;

    if (h < 3) and (v < 28) then
      c := AnsiChar(usx_sets[h][v]);

    if c in ['a'..'z'] then begin
      dstate := USX_ALPHA;
      if is_upper then
        c := AnsiChar(Ord(c) - 32);
    end else begin
      if c in ['0'.. '9'] then
        dstate := USX_NUM
      else
        if c = #0 then begin
          if v = 8 then begin
            if olen <= ol + 1 then exit(olen + 1);
            outbuf[ol] := #13; Inc(ol);
            outbuf[ol] := #10; Inc(ol);
          end else
            if (h = USX_NUM) and (v = 26) then begin
              count := readCount(inbuf, bit_no, ilen);
              if count < 0 then break;
              count := count + 4;
              if ol <= 0 then exit(0);
              c := outbuf[ol - 1];
              while count > 0 do begin
                if olen <= ol then exit(olen + 1);
                outbuf[ol] := c;
                Inc(ol);
                Dec(count);
              end;
            end else
              if (h = USX_SYM) and (v > 24) then begin
                v := v - 25;
                if (v >= 0) and (v <= 5) then begin
                  freqlen := StrLen(usx_freq_seq^[v]);
                  left := olen - ol;
                  if left <= 0 then exit(olen + 1);
                  System.Move(usx_freq_seq^[v][0], outbuf[ol], min_of(left, freqlen));
                  if left < freqlen then exit(olen + 1);
                  Inc(ol, freqlen);
                end;
              end else
                if (h = USX_NUM) and (v > 22) and (v < 26) then begin
                  v := v - 20;
                  freqlen := StrLen(usx_freq_seq^[v]);
                  left := olen - ol;
                  if left <= 0 then exit(olen + 1);
                  System.Move(usx_freq_seq^[v][0], outbuf[ol], min_of(left, freqlen));
                  if left < freqlen then exit(olen + 1);
                  Inc(ol, freqlen);
                end else
                  break; // terminator
          if dstate = USX_DELTA then h := USX_DELTA;
          continue;
        end;
    end;

    if dstate = USX_DELTA then h := USX_DELTA;
    if olen <= ol then exit(olen + 1);
    outbuf[ol] := c;
    Inc(ol);
  end;

  Result := ol;
end;

function unishox2_compress(const inbuf: PAnsiChar; ilen: Int32; outbuf: PAnsiChar; olen: Int32;
  const usx_hcodes: TUsxHCodes; const usx_hcode_lens: TUsxHCodeLens; usx_freq_seq: PUsxFreqSeq;
  usx_templates: PUsxTemplates): Int32;
begin
  Result := unishox2_compress_lines(inbuf, ilen, outbuf, olen, usx_hcodes, usx_hcode_lens,
                                   usx_freq_seq, usx_templates, nil);
end;

function unishox2_compress_simple(const inbuf: PAnsiChar; ilen: Int32; outbuf: PAnsiChar; olen: Int32): Int32;
begin
  Result := unishox2_compress(inbuf, ilen, outbuf, olen, USX_HCODES_DFLT, USX_HCODE_LENS_DFLT,
                              @USX_FREQ_SEQ_DFLT, @USX_TEMPLATES);
end;

function unishox2_decompress(const inbuf: PAnsiChar; ilen: Int32; outbuf: PAnsiChar; olen: Int32;
  const usx_hcodes: TUsxHCodes; const usx_hcode_lens: TUsxHCodeLens; usx_freq_seq: PUsxFreqSeq;
  usx_templates: PUsxTemplates): Int32;
begin
  Result := unishox2_decompress_lines(inbuf, ilen, outbuf, olen, usx_hcodes, usx_hcode_lens,
                                      usx_freq_seq, usx_templates, nil);
end;

function unishox2_decompress_simple(const inbuf: PAnsiChar; ilen: Int32; outbuf: PAnsiChar; olen: Int32): Int32;
begin
  Result := unishox2_decompress(inbuf, ilen, outbuf, olen, USX_HCODES_DFLT, USX_HCODE_LENS_DFLT,
                                @USX_FREQ_SEQ_DFLT, @USX_TEMPLATES);
end;

procedure AdjustPreset(preset: TUsxPreset; out hcodes: TUsxHCodes; out hcode_lens: TUsxHCodeLens;
  out freq_seq: PUsxFreqSeq; out templates: PUsxTemplates);
begin
  case preset of
    upsAlphaOnly:
      begin
        hcodes := USX_HCODES_ALPHA_ONLY;
        hcode_lens := USX_HCODE_LENS_ALPHA_ONLY;
        freq_seq := @USX_FREQ_SEQ_TXT;
      end;
    upsAlphaNumOnly:
      begin
        hcodes := USX_HCODES_ALPHA_NUM_ONLY;
        hcode_lens := USX_HCODE_LENS_ALPHA_NUM_ONLY;
        freq_seq := @USX_FREQ_SEQ_TXT;
      end;
    upsAlphaNumSymOnly:
      begin
        hcodes := USX_HCODES_ALPHA_NUM_SYM_ONLY;
        hcode_lens := USX_HCODE_LENS_ALPHA_NUM_SYM_ONLY;
        freq_seq := @USX_FREQ_SEQ_DFLT;
      end;
    upsAlphaNumSymTxt:
      begin
        hcodes := USX_HCODES_ALPHA_NUM_SYM_ONLY;
        hcode_lens := USX_HCODE_LENS_ALPHA_NUM_SYM_ONLY;
        freq_seq := @USX_FREQ_SEQ_DFLT;
      end;
    upsFavorAlpha:
      begin
        hcodes := USX_HCODES_FAVOR_ALPHA;
        hcode_lens := USX_HCODE_LENS_FAVOR_ALPHA;
        freq_seq := @USX_FREQ_SEQ_TXT;
      end;
    upsFavorDict:
      begin
        hcodes := USX_HCODES_FAVOR_DICT;
        hcode_lens := USX_HCODE_LENS_FAVOR_DICT;
        freq_seq := @USX_FREQ_SEQ_DFLT;
      end;
    upsFavorSym:
      begin
        hcodes := USX_HCODES_FAVOR_SYM;
        hcode_lens := USX_HCODE_LENS_FAVOR_SYM;
        freq_seq := @USX_FREQ_SEQ_DFLT;
      end;
    upsFavorUmlaut:
      begin
        hcodes := USX_HCODES_FAVOR_UMLAUT;
        hcode_lens := USX_HCODE_LENS_FAVOR_UMLAUT;
        freq_seq := @USX_FREQ_SEQ_DFLT;
      end;
    upsNoDict:
      begin
        hcodes := USX_HCODES_NO_DICT;
        hcode_lens := USX_HCODE_LENS_NO_DICT;
        freq_seq := @USX_FREQ_SEQ_DFLT;
      end;
    upsNoUni:
      begin
        hcodes := USX_HCODES_NO_UNI;
        hcode_lens := USX_HCODE_LENS_NO_UNI;
        freq_seq := @USX_FREQ_SEQ_DFLT;
      end;
    upsNoUniFavorText:
      begin
        hcodes := USX_HCODES_NO_UNI;
        hcode_lens := USX_HCODE_LENS_NO_UNI;
        freq_seq := @USX_FREQ_SEQ_TXT;
      end;
    upsUrl:
      begin
        hcodes := USX_HCODES_DFLT;
        hcode_lens := USX_HCODE_LENS_DFLT;
        freq_seq := @USX_FREQ_SEQ_URL;
      end;
    upsJson:
      begin
        hcodes := USX_HCODES_DFLT;
        hcode_lens := USX_HCODE_LENS_DFLT;
        freq_seq := @USX_FREQ_SEQ_JSON;
      end;
    upsJsonNoUni:
      begin
        hcodes := USX_HCODES_NO_UNI;
        hcode_lens := USX_HCODE_LENS_NO_UNI;
        freq_seq := @USX_FREQ_SEQ_JSON;
      end;
    upsXml:
      begin
        hcodes := USX_HCODES_DFLT;
        hcode_lens := USX_HCODE_LENS_DFLT;
        freq_seq := @USX_FREQ_SEQ_XML;
      end;
    upsHtml:
      begin
        hcodes := USX_HCODES_DFLT;
        hcode_lens := USX_HCODE_LENS_DFLT;
        freq_seq := @USX_FREQ_SEQ_HTML;
      end;
  else //upsDefault
    hcodes := USX_HCODES_DFLT;
    hcode_lens := USX_HCODE_LENS_DFLT;
    freq_seq := @USX_FREQ_SEQ_DFLT;
  end;
  templates := @USX_TEMPLATES;
end;

function unishox2_compress_preset(const inbuf: PAnsiChar; ilen: Int32; outbuf: PAnsiChar; olen: Int32;
  preset: TUsxPreset): Int32;
var
  hcodes: TUsxHCodes;
  hlens: TUsxHCodeLens;
  freq: PUsxFreqSeq;
  tmpl: PUsxTemplates;
begin
   AdjustPreset(preset, hcodes, hlens, freq, tmpl);
   Result := unishox2_compress(inbuf, ilen, outbuf, olen, hcodes, hlens, freq, tmpl)
end;

function unishox2_decompress_preset(const inbuf: PAnsiChar; ilen: Int32; outbuf: PAnsiChar; olen: Int32;
  preset: TUsxPreset): Int32;
var
  hcodes: TUsxHCodes;
  hlens: TUsxHCodeLens;
  freq: PUsxFreqSeq;
  tmpl: PUsxTemplates;
begin
  AdjustPreset(preset, hcodes, hlens, freq, tmpl);
  Result := unishox2_decompress(inbuf, ilen, outbuf, olen, hcodes, hlens, freq, tmpl);
end;

{ Fills the usx_code_94 94 letter array based on sets of characters at usx_sets
  For each element in usx_code_94, first 3 msb bits is set (USX_ALPHA / USX_SYM / USX_NUM)
  and the rest 5 bits indicate the vertical position in the corresponding set }
procedure init_coder;
var
  i, j: Integer;
  c: Byte;
begin
  FillChar(usx_code_94, SizeOf(usx_code_94), 0);
  for i := 0 to 2 do
    for j := 0 to 27 do begin
      c := usx_sets[i][j];
      if c > 32 then begin
        usx_code_94[c - USX_OFFSET_94] := (i shl 5) + j;
        if c in [Byte('a')..Byte('z')] then
          usx_code_94[c - USX_OFFSET_94 - (Byte('a') - Byte('A'))] := (i shl 5) + j;
      end;
    end;
end;

initialization
  init_coder;
end.
