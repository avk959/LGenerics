{
  This is a slightly modified example from my post on the Lazarus forum.
  https://forum.lazarus.freepascal.org/index.php/topic,53906.msg399695.html#msg399695

  Let Memo1 contain a list of words separated by tabs, spaces, commas, semicolons, or just line breaks.
  When Button1 is pressed, it is required to split this list into words, convert these words to lower
  case and display in Memo1 non-repeating words consisting of three or more characters and which are
  palindromes(in lexicographic order, each word on a separate line),
  or display a message that there are no palindromes.
}
unit main;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LazUtf8,
  lgStrHelpers;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
  function ToLower(const s: string): string; begin ToLower := Utf8LowerCase(s) end;
  function Fits(const s: string): Boolean; begin Fits := (Utf8Length(s)>2)and(s=Utf8ReverseString(s)) end;
  function Less(const L, R: string): Boolean; begin Less := UTF8CompareStr(L, R) < 0 end;
  function Join(const L, R: string): string; begin Join := R + LineEnding + L end;
begin
  Memo1.Append(Memo1.Lines.Text
    .Words([#9, #10, #13, ' ', ',', ';'])
    .Map(@ToLower)
    .Select(@Fits)
    .Distinct(@Less)
    .Fold(@Join)
    .OrElse('no palindromes')
  );
end;

end.

