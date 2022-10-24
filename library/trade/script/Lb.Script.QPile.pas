unit Lb.Script.QPile;

interface


///<summary>
/// ��������� ��������� �������
///</summary>
function GetResourceScritpQPL(const ATitle, ADescription, ASecCode, AClassCode: String; const AInterval, ACountCandel: Integer): String;

implementation

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes;



function GetResourceScritpSQL(const AResName: String): String;
var
  xRS: TResourceStream;
  xStr: TStrings;
begin
  Result := '';
  xRS := TResourceStream.Create(HInstance,AResName,RT_RCDATA);
  try
    xStr := TStringList.Create;
    try
      xStr.LoadFromStream(xRS);
      Result := xStr.Text;
    finally
      FreeAndNil(xStr);
    end;
  finally
    FreeAndNil(xRS);
  end;
end;


function GetResourceScritpQPL(const ATitle, ADescription, ASecCode, AClassCode: String; const AInterval, ACountCandel: Integer): String;
var
  xScriptQPL: String;
begin
  Result := '';

  if ASecCode.IsEmpty then
    raise Exception.Create('Error Message: �� ������ ����������');

  if AClassCode.IsEmpty then
    raise Exception.Create('Error Message: �� ������ ����� �����������');

  if AInterval <= 0 then
    raise Exception.Create('Error Message: �� ������ ��������');

  if ACountCandel <= 0 then
    raise Exception.Create('Error Message: �� ������ ����� ���������� ������');

  xScriptQPL := GetResourceScritpSQL('CANDELS');
  if not xScriptQPL.IsEmpty then
  begin
    xScriptQPL := StringReplace(xScriptQPL,'{SCRIPT_TITLE}',ATitle,[rfIgnoreCase]);
    xScriptQPL := StringReplace(xScriptQPL,'{SCRIPT_DESCRIPTION}',ADescription,[rfIgnoreCase]);
    xScriptQPL := StringReplace(xScriptQPL,'{SEC_CODE}',ASecCode,[rfIgnoreCase]);
    xScriptQPL := StringReplace(xScriptQPL,'{CLASS_CODE}',AClassCode,[rfIgnoreCase]);
    xScriptQPL := StringReplace(xScriptQPL,'{INTERVAL}',IntToStr(AInterval),[rfIgnoreCase]);
    xScriptQPL := StringReplace(xScriptQPL,'{COUNT_BAR}',IntToStr(ACountCandel),[rfIgnoreCase]);
    Result := xScriptQPL;
  end;
end;

end.
