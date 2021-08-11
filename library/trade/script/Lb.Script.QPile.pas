unit Lb.Script.QPile;

interface

/// <summary>
/// ��������� ������ - ������� �� ������� ��������
/// </summary>
function GetResourceScritpSQL(const AResName: String): String;

/// <summary>
/// ��������� ��������� �������
/// </summary>
/// <remarks>
///
/// </remarks>
function GetResourceScritpQPL(const ACaption, AInstrument, AClassCode, AParamMA: String; const AInterval, ACountCandel: Integer): String;

implementation

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes;

function GetResourceScritpQPL(const ACaption, AInstrument, AClassCode, AParamMA: String; const AInterval, ACountCandel: Integer): String;
var
  xScriptQPL: String;
begin
  Result := '';

  if AInstrument.IsEmpty then
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
    xScriptQPL := StringReplace(xScriptQPL,'{CAPTION}',ACaption + '_' + AInstrument + '_' + AClassCode,[rfReplaceAll]);

    xScriptQPL := StringReplace(xScriptQPL,'{INSTRUMENT}',AInstrument,[rfReplaceAll]);
    xScriptQPL := StringReplace(xScriptQPL,'{CLASSCODE}',AClassCode,[rfReplaceAll]);

    xScriptQPL := StringReplace(xScriptQPL,'{INTERVAL}',IntToStr(AInterval),[rfReplaceAll]);
    xScriptQPL := StringReplace(xScriptQPL,'{COUNT_CANDEL}',IntToStr(ACountCandel),[rfReplaceAll]);

    xScriptQPL := StringReplace(xScriptQPL,'{MA}',AParamMA,[rfReplaceAll]);

    Result := xScriptQPL;
  end;
end;

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

end.
