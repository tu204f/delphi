(*******************************************************************************
* Процедура парсинга значение работы, настройки
*******************************************************************************)
unit Lb.Strings.IniFiles;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes;


/// <summary>
/// Получение значение
/// </summary>
function ReadString(const AStrings: TStrings; const AParam: String): String;
/// <summary>
/// Список параметров по секции
/// </summary>
procedure ReadStrings(const AStrings, AParams: TStrings; const ASection: String);


implementation

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

type
  TParam = record
    Section: String;
    Ident: String;
  private
    procedure SetParam(const Value: String);
  public
    property Param: String write SetParam;
  end;

{ TParam }

procedure TParam.SetParam(const Value: String);
var
  xPosInd: Integer;
begin
  xPosInd := Pos('.',Value);
  if xPosInd > 0 then
  begin
    Section := Copy(Value,1,xPosInd - 1);
    Ident := Copy(Value,xPosInd + 1,256);
  end
  else
  begin
    Section := 'sys';
    Ident := Value;
  end;
end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

function GetPosString(const AStrings: TStrings; const Sub: String; AIndex: Integer = 0): Integer;
var
  xS: String;
  i, Count: Integer;
begin
  Result := -1;
  if AIndex >= 0 then
  begin
    Count := AStrings.Count;
    if Count > 0 then
      for i := AIndex to Count - 1 do
      begin
        xS := AStrings[i];
        if Pos(Sub,xS) > 0 then
        begin
          Result := i;
          Break;
        end;
      end;
  end;
end;

procedure ReadStrings(const AStrings, AParams: TStrings; const ASection: String);
var
  xS: string;
  xBeginIndex: Integer;
  i, Count: Integer;
begin
  AParams.Clear;
  xBeginIndex := GetPosString(AStrings,'[' + ASection + ']');
  if xBeginIndex >= 0 then
  begin
    Inc(xBeginIndex);
    Count := AStrings.Count;
    if Count > 0 then
      for i := xBeginIndex to Count - 1 do
      begin
        xS := AStrings[i];
        AParams.Add(xS);
      end;
  end;
end;

function ReadString(const AStrings: TStrings; const AParam: String): String;

  function GetValue(S: String): String;
  var
    xPos: Integer;
  begin
    xPos := Pos('=',S);
    Result := Copy(S,xPos + 1,S.Length);
  end;

var
  xS: String;
  xParam: TParam;
  xSectionInd, xIdentInd: Integer;
begin
  Result := '';
  if Assigned(AStrings) then
  begin
    xParam.SetParam(AParam);
    xSectionInd := GetPosString(AStrings,xParam.Section);
    xIdentInd   := GetPosString(AStrings,xParam.Ident,xSectionInd);
    if xIdentInd >= 0 then
    begin
      xS := AStrings[xIdentInd];
      Result := GetValue(xS);
    end;
  end;
end;

end.
