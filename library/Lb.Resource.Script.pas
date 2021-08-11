(*******************************************************************************
* ������ ������� �� ������� ���������
*******************************************************************************)
unit Lb.Resource.Script;

interface

/// <summary>
/// ��������� SQL - ������� �� ������� ��������
/// </summary>
function GetResourceScritp(const AResName: String): String;

implementation

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes;

function GetResourceScritp(const AResName: String): String;
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
