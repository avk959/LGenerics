{ generates file citylots.txt;
  it expects a citylots.json file(https://github.com/zemirco/sf-city-lots-json/blob/master/citylots.json)
  in the same folder as the binary;
  to compile you need the JsonTools unit in the project folder.
  (https://github.com/sysrpl/JsonTools)}
program gen_citylots;

{$mode delphi}

uses
  Classes, SysUtils, lgUtils, lgJson, JsonTools;

procedure Run;
var
  List: TStringList = nil;
  procedure TraverseNode(aNode: TJsonNode);
  var
    n: TJsonNode;
    d: Double;
  begin
    if aNode.Kind in [nkObject, nkArray] then
      for n in aNode do
        TraverseNode(n)
    else
      case aNode.Kind of
        nkNumber:
          begin
            d := aNode.AsNumber;
            if not IsExactInt(d) then
              List.Add(aNode.Value);
          end;
      else
      end;
  end;
var
  ListRef: TGAutoRef<TStringList>;
  Node: TGAutoRef<TJsonNode>;
begin
  Node.Instance.LoadFromFile('citylots.json');
  List := ListRef;
  DefaultFormatSettings.DecimalSeparator := '.';
  TraverseNode(Node.Instance);
  List.SaveToFile('citylots.txt');
end;

begin
  Run;
  WriteLn('Done.');
end.

