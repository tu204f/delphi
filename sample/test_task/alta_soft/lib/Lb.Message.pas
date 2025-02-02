unit Lb.Message;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Dialogs;

type
  TMessageApp = record
    /// <summary>
    /// ИСпользуем для вывода информационого сообщения
    /// </summary>
    /// <remarks>
    /// Использовать в качестве подтверждение действия
    /// </remarks>
    class function GetInfoMessageDlg(const AText, ACaption: String): Boolean; static;

    /// <summary>
    /// Ошибочные действия пользователя
    /// </summary>
    class procedure SetErrorUser(const AText: String; const ACaption: String = ''); static;
  end;

implementation

{ TMessageDlg }

class function TMessageApp.GetInfoMessageDlg(const AText, ACaption: String): Boolean;
var
  xResult: Integer;
begin
  xResult := MessageBox(0,PChar(AText),PChar(ACaption),MB_YESNO or MB_ICONINFORMATION);
  Result := (xResult = IDYES);
end;

class procedure TMessageApp.SetErrorUser(const AText, ACaption: String);
var
  xCaption: String;
begin
  if ACaption.IsEmpty then
    xCaption := 'Ошибочные действия пользователя'
  else
    xCaption := ACaption;
  MessageBox(0,PChar(AText),PChar(xCaption),MB_OK or MB_ICONWARNING);
end;

end.
