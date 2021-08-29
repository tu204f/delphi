unit Lb.ApplicationVersion;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes;

///<summary>¬ерси€ приложение</summary>
function GetApplicationVersion: String;

implementation

function GetApplicationVersion: String;
type
  TVerInfo=packed record
    Nevazhno: array[0..47] of byte; // ненужные нам 48 байт
    Minor,Major,Build,Release: word; // а тут верси€
  end;
var
  s:TResourceStream;
  v:TVerInfo;
begin
  Result:='';
  try
    s := TResourceStream.Create(HInstance,'#1',RT_VERSION); // достаЄм ресурс
    if s.Size>0 then begin
      s.Read(v,SizeOf(v)); // читаем нужные нам байты
      result:=IntToStr(v.Major)+'.'+IntToStr(v.Minor)+'.'+ // вот и верси€...
              IntToStr(v.Release)+'.'+IntToStr(v.Build);
    end;
    s.Free;
  except;
  end;
end;

end.
