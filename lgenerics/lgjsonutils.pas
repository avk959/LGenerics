unit lgJsonUtils;

{$MODE OBJFPC}{$H+}

interface

uses
  SysUtils, FpJson,
  lgUtils,
  lgJson,
  lgStrConst;

  function JsonNode2Data(aNode: TJsonNode): TJsonData;
  function JsonData2Node(aData: TJsonData): TJsonNode;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}

function JsonNode2Data(aNode: TJsonNode): TJsonData;
  function CopyNode(aNode: TJsonNode): TJsonData;
  var
    I: SizeInt;
  begin
    Result := nil;
    case aNode.Kind of
      jvkNull:    Result := CreateJson;
      jvkFalse:   Result := CreateJson(False);
      jvkTrue:    Result := CreateJson(True);
      jvkNumber:  Result := CreateJson(aNode.AsNumber);
      jvkString:  Result := CreateJson(aNode.AsString);
      jvkArray:
        begin
          Result := TJsonArray.Create;
          for I := 0 to Pred(aNode.Count) do
            TJsonArray(Result).Add(CopyNode(aNode.Items[I]));
        end;
      jvkObject:
        begin
          Result := TJsonObject.Create;
          for I := 0 to Pred(aNode.Count) do
            with aNode.Pairs[I] do
              TJsonObject(Result).Add(Key, CopyNode(Value));
        end;
    end;
  end;
begin
  if aNode = nil then exit(nil);
  Result := CopyNode(aNode);
end;

function JsonData2Node(aData: TJsonData): TJsonNode;
  procedure CopyData(aSrc: TJsonData; aDst: TJsonNode);
  var
    I: SizeInt;
    e: TJsonEnum;
  begin
    case aSrc.JSONType of
      jtUnknown: raise EJsException.Create(SEUnknownJsDataType);
      jtNull:    aDst.AsNull;
      jtBoolean: aDst.AsBoolean := aSrc.AsBoolean;
      jtNumber:  aDst.AsNumber := aSrc.AsFloat;
      jtString:  aDst.AsString := aSrc.AsString;
      jtArray:
        begin
          aDst.AsArray;
          for I := 0 to Pred(aSrc.Count) do
            CopyData(aSrc.Items[I], aDst.AddNode);
        end;
      jtObject:
        begin
          aDst.AsObject;
          for e in aSrc do
            CopyData(e.Value, aDst.AddNode(e.Key));
        end;
    end;
  end;
begin
  if aData = nil then exit(nil);
  Result := TJsonNode.Create;
  CopyData(aData, Result);
end;

end.

