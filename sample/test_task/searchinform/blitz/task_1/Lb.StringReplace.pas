unit Lb.StringReplace;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Variants;

///<summary>
/// Аналог функции - StringReplace
/// Source     - Исходная строка;
/// OldPattern - Старый паттерн;
/// NewPattern - Новый паттерн;
/// SizeBuffer - Фиксируем размер строки;
/// ReplaceAll - Заменить все найденные паттерны
///</summary>
function GetStringReplace(
  const Source, OldPattern, NewPattern: string;
  const SizeBuffer: Integer = 0;
  const ReplaceAll: Boolean = False;
  const IgnoreCase: Boolean = False): String;

implementation



function GetStringReplace(const Source, OldPattern, NewPattern: string;
  const SizeBuffer: Integer = 0;
  const ReplaceAll: Boolean = False;
  const IgnoreCase: Boolean = False): String;

const
  SIZE_BUFFER = 4096;

  function GetIsChar(AValue1, AValue2: Char): Boolean;
  begin
    if IgnoreCase then
      Result := UpCase(AValue1) = UpCase(AValue2)
    else
      Result := AValue1 = AValue2
  end;

  function GetPosOldParrent(AIndexSource, ALengthOldPattern: Integer): Integer;
  var
    xOldC, xSourceC: Char;
    i: Integer;
  begin
    Result := -1;
    for i := 1 to ALengthOldPattern do
    begin
      Result := i;
      xOldC := OldPattern[i];
      xSourceC := Source[AIndexSource + (i - 1)];
      if not GetIsChar(xOldC,xSourceC) then
      begin
        Result := -1;
        Break;
      end;
    end;
  end;

function GetCountReplace(const ALengthSource, ALengthOldPattern: Integer): Integer;
var
  xIndex: Integer;
  xCount: Integer;
begin
  // Количество повторений Pattern
  xCount := 0;
  xIndex := 1;
  while xIndex <= ALengthSource do
  begin
    if GetPosOldParrent(xIndex,ALengthOldPattern) > 0 then
    begin
      xIndex := xIndex + ALengthOldPattern;
      Inc(xCount);
    end;
    Inc(xIndex);
  end;
  if xCount <= 0 then
    xCount := 1;
  Result := xCount;
end;

var
  i: Integer;
  xBuffer: String;
var
  xLengthSource: Integer;
  xLengthBuffer: Integer;
  xLengthOldPattern: Integer;
  xLengthNewPattern: Integer;

  xIndexSource: Integer;
  xIndexBuffer: Integer;
var
  xCountReplaceAll: Integer;

begin
  // -------------------------------------------------------------------------
  xIndexSource      := 1;
  xIndexBuffer      := 1;

  xLengthSource     := Length(Source);
  xLengthOldPattern := Length(OldPattern);
  xLengthNewPattern := Length(NewPattern);

  // -------------------------------------------------------------------------
  if xLengthOldPattern = 0 then
  begin
    Result := Source;
    Exit;
  end;

  // -------------------------------------------------------------------------
  // Определяем размер буфера

  if ReplaceAll then
    xCountReplaceAll := 255
  else
    xCountReplaceAll := 1;

  if SizeBuffer < 0 then
    xLengthBuffer := SIZE_BUFFER
  else if SizeBuffer = 0 then
  begin
    if ReplaceAll then
      xCountReplaceAll := GetCountReplace(xLengthSource,xLengthOldPattern);
    xLengthBuffer := xLengthSource + xCountReplaceAll * (xLengthNewPattern - xLengthOldPattern);
  end else
    xLengthBuffer := SizeBuffer;
  xBuffer := StringOfChar(#0,xLengthBuffer);

  while xIndexSource <= xLengthSource do
  begin
    if (GetPosOldParrent(xIndexSource,xLengthOldPattern) > 0) and (xCountReplaceAll > 0) then
    begin
      for i := 1 to xLengthNewPattern do
      begin
        xBuffer[xIndexBuffer] := NewPattern[i];
        Inc(xIndexBuffer);
      end;
      xIndexSource := xIndexSource + xLengthOldPattern;
      Dec(xCountReplaceAll);
    end
    else
    begin
      xBuffer[xIndexBuffer] := Source[xIndexSource];
      Inc(xIndexBuffer);
      Inc(xIndexSource);
    end;
  end;

  Result := Trim(xBuffer);
end;


end.
