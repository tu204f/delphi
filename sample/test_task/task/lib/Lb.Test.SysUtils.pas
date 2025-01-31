unit Lb.Test.SysUtils;

interface

{$I test.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes;

const
  TREE_NODE_DIR   = 0;
  TREE_NODE_VALUE = 1;


type
  DevopException = Exception;

  TInfoFilmParser = record
    Name: String;
    Year: String;
    Author: String;
    Path: String;
    Text: String;
  public
    procedure SetText(const AValue: String);
  end;

function GetApplicationPath: String;
function SetLoadList(const AFileName: String; const AStrings: TStrings): Boolean;
procedure SetParserPath(const ASource: TStrings; const AValue: String);
procedure SetParserLine(const ASource: TStrings; const AValue: String);

implementation

uses
{$IFDEF DEBUG}
  Lb.Logger,
{$ENDIF}
  Lb.Setting;

function GetApplicationPath: String;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function SetLoadList(const AFileName: String; const AStrings: TStrings): Boolean;
var
  xErrorMsg: String;
begin
  try
    AStrings.LoadFromFile(AFileName);
    Result := True;
  except
    on E: Exception do
    begin
      xErrorMsg := E.Message;
      {$IFDEF LIFT_DBG}
      TLogger.LogTree(0,'Ошибка загрузка списка');
      TLogger.LogTreeText(3,'>> Путь к файлу:' + AFileName);
      TLogger.LogTreeText(3,'>> Сообщение:' + xErrorMsg);
      {$ENDIF}
      Result := False;
    end;
  end;
end;

procedure SetParserPath(const ASource: TStrings; const AValue: String);
var
  xC: Char;
  xS: String;
begin
  xS := '';
  for xC in AValue do
  begin
    if xC = ',' then
    begin
      if not xS.IsEmpty then
        ASource.Add(xS);
      xS := '';
    end
    else
      xS := xS + xC;
  end;
  if not xS.IsEmpty then
    ASource.Add(xS);
  TStringList(ASource).Sort;
end;

procedure SetParserLine(const ASource: TStrings; const AValue: String);
var
  xC: Char;
  xS: String;
begin
  xS := '';
  for xC in AValue do
  begin
    if xC = PathDelim then
    begin
      if not xS.IsEmpty then
        ASource.Add(xS);
      xS := '';
    end
    else
      xS := xS + xC;
  end;
  if not xS.IsEmpty then
    ASource.Add(xS);
end;

{ TInfoFilmParser }

procedure TInfoFilmParser.SetText(const AValue: String);

  procedure _ParserStr(ASource: TStrings; AValue: String);
  var
    xC: Char;
    xTmp: String;
  begin
    xTmp := '';
    for xC in AValue do
    begin
      if xC = #9 then
      begin
        ASource.Add(xTmp);
        xTmp := '';
      end
      else
        xTmp := xTmp + xC;
    end;
    if not xTmp.IsEmpty then
      ASource.Add(xTmp);
  end;

  function _GetValue(const ASource: TStrings; AIndex: Integer): String;
  begin
    Result := '';
    if (AIndex < ASource.Count) then
      Result := Trim(ASource[AIndex]);
  end;

var
  xStr: TStrings;
begin
  Text := AValue;
  xStr := TStringList.Create;
  try
    _ParserStr(xStr,AValue);

    Name   := _GetValue(xStr,0);
    Year   := _GetValue(xStr,1);
    Author := _GetValue(xStr,2);
    Path   := _GetValue(xStr,3);

  finally
    FreeAndNil(xStr);
  end;
end;

end.
