unit Lb.Logger;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants;

type
  TTypeLogger = (tlErorr,tlWaring,tlInfo,tlConnet);

  TLogger = record
    /// <summary>
    /// Записываем лог проведенных операциий и зарпосов
    /// </summary>
    class procedure Log(S: String = ''); static;
    /// <summary>Вывод текстовых сообщений в виде дерева, без временной индикации</summary>
    class procedure LogTreeText(AChild: Integer; S: String); static;
    /// <summary>Вывод текстовых сообщений в виде дерева</summary>
    class procedure LogTree(AChild: Integer; S: String); static;
    /// <summary>Сохроняем Текст</summary>
    class procedure LogText(S: String = ''); overload; static;

    /// <summary>
    /// Сохроняем Текст
    /// </summary>
    class procedure LogText(S: String;  ACount: Integer);  overload; static;

    /// <summary>
    /// Записать лог в зависемости
    /// </summary>
    class procedure LogType(AType: TTypeLogger; S: String); static;
    class procedure LogForm(AFromCaption: String; S: String); static;
    /// <summary>
    /// Удалить или очистить список log
    /// </summary>
    class procedure ClearLog; static;
    /// <summary>
    /// Вывод вариантного массива
    /// </summary>
    class procedure LogArray(const AValue: Variant); static;
  end;

implementation

uses
  System.SyncObjs;

var
  IndexLog: Integer = 0;
  LogCS: TCriticalSection;

function GetChildToString(const AChild: Integer): String;
var
  xS: String;
  xChild: Integer;
begin
  xS := '';
  xChild := AChild;
  while (xChild > 0) do
  begin
    xS := xS + '  ';
    Dec(xChild);
  end;
  Result := xS;
end;

{ TLogger }

class procedure TLogger.Log(S: String);
var
  xS: String;
begin
  LogCS.Enter;
  try
    xS := FormatDateTime('hh:mm:ss.zzz',Time) + '||' + S;
    TLogger.LogText(xS);
  finally
    LogCS.Leave;
  end;
end;

class procedure TLogger.LogText(S: String);
var
  F: TextFile;
  xPath: String;
begin
  LogCS.Enter;
  try
    xPath := ExtractFilePath(ParamStr(0)) + 'log.txt';
    AssignFile(f,xPath);
    if FileExists(xPath) then
      Append(F)
    else
      Rewrite(F);
    WriteLn(F, S);
    CloseFile(F);
  finally
    LogCS.Leave;
  end;
end;

class procedure TLogger.LogText(S: String; ACount: Integer);
var
  xS: String;
  i: Integer;
begin
  if ACount > 0 then
  begin
    xS := '';
    for i := 0 to ACount - 1 do
      xS := xS + S;
    TLogger.LogText(xS);
  end;
end;

class procedure TLogger.LogTree(AChild: Integer; S: String);
var
  xS: String;
begin
  xS := GetChildToString(AChild) + S;
  TLogger.Log(xS);
end;

class procedure TLogger.LogTreeText(AChild: Integer; S: String);
var
  xS: String;
begin
  xS := GetChildToString(AChild) + S;
  TLogger.LogText(xS);
end;

class procedure TLogger.LogType(AType: TTypeLogger; S: String);
var
  xS: String;
begin
  case AType of
    tlErorr: xS := '[Erorr]';
    tlWaring: xS := '[WARING]';
    tlInfo: xS := '[INFO]';
    tlConnet: xS := '[CONNECT]';
  else
    xS := '[NON]';
  end;
  xS := xS + S;
  TLogger.Log(xS);
end;

class procedure TLogger.LogForm(AFromCaption, S: String);
var
  xS: String;
begin
  xS := '[' + AFromCaption + '] ' + S;
  TLogger.Log(xS);
end;

class procedure TLogger.ClearLog;
var
  xPath: String;
begin
  xPath := ExtractFilePath(ParamStr(0)) + 'log.txt';
  DeleteFile(xPath);
end;

class procedure TLogger.LogArray(const AValue: Variant);
var
  i, l, h: Integer;
begin
  TLogger.Log('Вариант массива:');
  l := VarArrayLowBound(AValue,1);
  h := VarArrayHighBound(AValue,1);
  for i := l to h do
    TLogger.Log('   ' + VarToStr(AValue[i]));
end;

initialization
  LogCS := TCriticalSection.Create;

finalization
  FreeAndNil(LogCS);

end.
