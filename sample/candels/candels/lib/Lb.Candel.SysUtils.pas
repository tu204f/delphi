unit Lb.Candel.SysUtils;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  ///<summary>Свеча</summary>
  TCandel = record
    Date: TDateTime;
    Time: TDateTime;
    Open: Double;
    High: Double;
    Low: Double;
    Close: Double;
    Vol: Integer;
  end;
  TCandelList = TList<TCandel>;

  ///<summary>Векторная представленеи свечи</summary>
  TVectorCandel = record
    Date: TDateTime;
    Time: TDateTime;
    Open: Double;
    High: Double;
    Low: Double;
    Close: Double;
    Vol: Double;
  end;
  TVectorCandelList = TList<TVectorCandel>;

function GetVectorValueToCandel(ACandel: TCandel; APriceClose: Double; AVol: Integer = 0): TVectorCandel;
/// <summary>С равниваем два числа</summary>
function GetSameValue(const AValue1, AValue2: Double; const AEpsilon: Integer): Boolean;

implementation

uses
  System.Math;

function GetRoundTo(const AValue: Double): Double;
const
  RANGE = 1000000;
begin
  Result := Trunc(AValue * RANGE)/RANGE;
end;

function GetVectorValueToCandel(ACandel: TCandel; APriceClose: Double; AVol: Integer = 0): TVectorCandel;
begin
  with Result do
  begin
    Date := ACandel.Date;
    Time := ACandel.Time;
    Open := GetRoundTo(ACandel.Open/APriceClose);
    High := GetRoundTo(ACandel.High/APriceClose);
    Low  := GetRoundTo(ACandel.Low/APriceClose);
    Close:= GetRoundTo(ACandel.Close/APriceClose);
    if AVol > 0 then
      Vol  := GetRoundTo(ACandel.Vol/AVol)
    else
      Vol := 0;
  end;
end;


function GetSameValue(const AValue1, AValue2: Double; const AEpsilon: Integer): Boolean;
var
  xEpsilon: Double;
  //xValue,
  xLowValue, xHighValue: Double;
begin
  Result := False;
  if (AEpsilon > 0)  and (AEpsilon < 100) then
  begin
    xLowValue := Min(AValue1,AValue2);
    xHighValue := Max(AValue1,AValue2);
    xEpsilon := (xHighValue - xLowValue)/xHighValue;
    Result := AEpsilon > (xEpsilon * 100);
  end;
end;

end.
