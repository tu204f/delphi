unit Lb.ReadPrice;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  TCandel = record
    Date : TDateTime;
    Time : TDateTime;
    Open : Double;
    High : Double;
    Low  : Double;
    Close: Double;
    Vol  : Double;
  end;

  ///<summary>Источние данных</summary>
  TSource = class(TStringList)
  private
    function GetCandels(Index: Integer): TCandel;
  public

    property Candels[Index: Integer]: TCandel read GetCandels;
  end;


implementation

uses
  System.DateUtils;

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


{ TSource }

function TSource.GetCandels(Index: Integer): TCandel;
begin
  var xS := Self.Strings[Index];
  Result := GetCandelToStr(xS);
end;

end.
