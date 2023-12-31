(******************************************************************************)
(* „итаем заначение св€чей                                                    *)
(******************************************************************************)
unit Lb.ReadPrice;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.SysUtils;

type
  TSourceCandels = class(TStringList)
  private
    function GetCandels(Index: Integer): TCandel;
  public
    property Candels[Index: Integer]: TCandel read GetCandels;
  end;

  TSourceTikets = class(TStringList)
  private
    function GetTikets(Index: Integer): TTiket;
  public
    property Tikets[Index: Integer]: TTiket read GetTikets;
  end;


implementation

(*******************************************************************************
// ѕарсим без даты и времени
<DATE>;<TIME>;<OPEN>;<HIGH>;<LOW>;<CLOSE>;<VOL>
231213;090000;25857.0000000;25897.0000000;25765.0000000;25802.0000000;2715
231213;090500;25802.0000000;25813.0000000;25791.0000000;25797.0000000;688
*******************************************************************************)

function GetCandelToStr(const S: String): TCandel;

  procedure _Paser(const S: String; ASource: TStrings);
  begin
    ASource.Clear;
    ASource.Delimiter := ';';
    ASource.DelimitedText := S;
  end;

var
  xCandel: TCandel;
  xSource: TStrings;
  xOldChar: Char;
begin
  xSource := TStringList.Create;
  try
    _Paser(S,xSource);
    xOldChar := FormatSettings.DecimalSeparator;
    FormatSettings.DecimalSeparator := '.';
    xCandel.Open  := StrToFloatDef(xSource[2],0);
    xCandel.High  := StrToFloatDef(xSource[3],0);
    xCandel.Low   := StrToFloatDef(xSource[4],0);
    xCandel.Close := StrToFloatDef(xSource[5],0);
    xCandel.Vol   := StrToFloatDef(xSource[6],0);

    Result := xCandel;

    FormatSettings.DecimalSeparator := xOldChar;
  finally
    FreeAndNil(xSource);
  end;
end;

(*******************************************************************************
// ѕарсим без даты и времени
<DATE>;<TIME>;<LAST>;<VOL>
231213;085933;25848.000000000;2
231213;085933;25848.000000000;1
*******************************************************************************)

function GetTikitToStr(const S: String): TTiket;

  procedure _Paser(const S: String; ASource: TStrings);
  begin
    ASource.Clear;
    ASource.Delimiter := ';';
    ASource.DelimitedText := S;
  end;

  function _TimeToStr(const S: String): Integer;
  var
    xH, xM, xS: String;
  begin
    xH := Copy(S,0,2);
    xM := Copy(S,3,2);
    xS := Copy(S,5,2);
    Result :=
      3600 * StrToIntDef(xH,0) +
      60 * StrToIntDef(xM,0) +
      StrToIntDef(xS,0);
  end;

var
  xTikit: TTiket;
  xSource: TStrings;
  xOldChar: Char;
begin
  xSource := TStringList.Create;
  try
    _Paser(S,xSource);
    xOldChar := FormatSettings.DecimalSeparator;
    FormatSettings.DecimalSeparator := '.';
    xTikit.Time := _TimeToStr(xSource[1]);
    xTikit.Last := StrToFloatDef(xSource[2],0);
    xTikit.Vol  := StrToFloatDef(xSource[3],0);
    Result := xTikit;
    FormatSettings.DecimalSeparator := xOldChar;
  finally
    FreeAndNil(xSource);
  end;
end;

{ TSourceCandels }

function TSourceCandels.GetCandels(Index: Integer): TCandel;
var
  xS: String;
begin
  xS := Self.Strings[Index];
  Result := GetCandelToStr(xS);
end;

{ TSourceTikets }

function TSourceTikets.GetTikets(Index: Integer): TTiket;
var
  xS: String;
begin
  xS := Self.Strings[Index];
  Result := GetTikitToStr(xS);
end;

end.
