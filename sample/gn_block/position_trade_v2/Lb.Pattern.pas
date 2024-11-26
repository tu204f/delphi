(******************************************************************************)
(* Здесь задем патерн который нужно найти                                     *)
(******************************************************************************)
unit Lb.Pattern;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.ReadPrice;

type
  TParrentCandel = record
    Open : Double;
    High : Double;
    Low  : Double;
    Close: Double;
    Vol  : Double;
  public
    function ToStr: String;
  end;
  TParrentCandelList = TList<TParrentCandel>;

///<summary>Список свячей и к приведеному списку</summary>
procedure ToCandelParrent(ACandels: TCandelList; AParrent: TParrentCandelList; IsSingleValue: Boolean = True);

///<summary>Приведение значение свечей в единичному значению или от 0 до 100</summary>
function ToParrentCandel(AMaxPrice, AMinPrice, AMaxVol, AMinVol: Double; ACurrentCandel: TCandel; AIsSingleValue: Boolean = True): TParrentCandel;

implementation

function ToParrentCandel(AMaxPrice, AMinPrice, AMaxVol, AMinVol: Double; ACurrentCandel: TCandel; AIsSingleValue: Boolean): TParrentCandel;

  function _CastingValue(const AValue, AMaxValue, AMinValue: Double; IsSingleValue: Boolean): Double;
  var
    xValue: Double;
  begin
    if (AMaxValue <> AMinValue) and (AMaxValue > 0) and (AMinValue > 0) then
    begin
      xValue := (AValue - AMinValue)/(AMaxValue - AMinValue);
      if not IsSingleValue then
        xValue := Round(xValue * 100);
      Result := xValue;
    end
    else
      Result := 0;
  end;

var
  xParrent: TParrentCandel;
begin
  xParrent.Open  := _CastingValue(ACurrentCandel.Open , AMaxPrice, AMinPrice, AIsSingleValue);
  xParrent.High  := _CastingValue(ACurrentCandel.High , AMaxPrice, AMinPrice, AIsSingleValue);
  xParrent.Low   := _CastingValue(ACurrentCandel.Low  , AMaxPrice, AMinPrice, AIsSingleValue);
  xParrent.Close := _CastingValue(ACurrentCandel.Close, AMaxPrice, AMinPrice, AIsSingleValue);
  xParrent.Vol   := _CastingValue(ACurrentCandel.Vol  , AMaxVol  , AMinVol  , AIsSingleValue);
  Result := xParrent;
end;


procedure ToCandelParrent(ACandels: TCandelList; AParrent: TParrentCandelList; IsSingleValue: Boolean);

  procedure _MaxAndMin(const ACandels: TCandelList; var AMaxPrice, AMinPrice, AMaxVol, AMinVol: Double);
  var
    xCandel: TCandel;
  begin
    AMaxPrice := 0;
    AMinPrice := 0;
    AMaxVol   := 0;
    AMinVol   := 0;

    if ACandels.Count > 0 then
    begin
      xCandel := ACandels[0];

      AMaxPrice := xCandel.High;
      AMinPrice := xCandel.Low;
      AMaxVol := xCandel.Vol;
      AMinVol := xCandel.Vol;

      for var i := 1 to ACandels.Count - 1 do
      begin
        xCandel := ACandels[i];

        if AMaxPrice < xCandel.High then
          AMaxPrice := xCandel.High;
        if AMinPrice > xCandel.Low then
          AMinPrice := xCandel.Low;

        if xCandel.Vol > AMaxVol then
          AMaxVol := xCandel.Vol;
        if xCandel.Vol < AMinVol then
          AMinVol := xCandel.Vol;

      end;
    end;
  end;

var
  xClose, xCurrentCandel: TCandel;
  xParrent: TParrentCandel;
  i, iCount: Integer;
  xMaxValue, xMinValue, xMaxVol, xMinVol: Double;
begin
  AParrent.Clear;
  _MaxAndMin(ACandels, xMaxValue, xMinValue, xMaxVol, xMinVol);

  iCount := ACandels.Count;
  if iCount > 0 then
  begin
    for i := 0 to iCount - 1 do
    begin
      xCurrentCandel := ACandels[i];

      xParrent := ToParrentCandel(
        xMaxValue,
        xMinValue,
        xMaxVol,
        xMinVol,
        xCurrentCandel,
        IsSingleValue
      );

      AParrent.Add(xParrent);
    end;
  end;
end;

{ TParrentCandel }

function TParrentCandel.ToStr: String;
var
  xS: String;
begin
  xS :=
    'Open: '  + Open.ToString + '; ' +
    'High: '  + High.ToString + '; ' +
    'Low: '   + Low.ToString  + '; ' +
    'Close: ' + Close.ToString + '; ' +
    'Vol: '   + Vol.ToString + ';';
  Result := xS;
end;

end.
