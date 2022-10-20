unit Lb.Patern;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.Candel.SysUtils;

const
  PATERN_PRICE = 100;

type
  ///<summary>«начение работы - св€чи</summary>
  TValueCandel = record
    Open : Double;
    High : Double;
    Low  : Double;
    Close: Double;
    Vol  : Double;
  end;

  ///<summary>—веча параметра</summary>
  TPaternCandel = record
    Open : Integer;
    High : Integer;
    Low  : Integer;
    Close: Integer;
    Vol  : Integer;
  end;

implementation

function GetPriceToPatern(const APrice, APaternPrice: Double): Integer;
var
  xValue: Double;
begin
  xValue := (APrice * PATERN_PRICE)/APaternPrice;
  Result := Trunc(xValue);
end;

function GetParentToPrice(const APrice, APaternPrice: Double): Integer;
var
  xValue: Double;
begin
  xValue := (APrice * PATERN_PRICE)/APaternPrice;
  Result := Trunc(xValue);
end;

function ToParentCandel(const AValueCandel: TValueCandel; const APrice: Double): TPaternCandel;
begin
  Result.Open  := GetPriceToPatern(AValueCandel.Open,APrice);
  Result.High  := GetPriceToPatern(AValueCandel.High,APrice);
  Result.Low   := GetPriceToPatern(AValueCandel.Low,APrice);
  Result.Close := GetPriceToPatern(AValueCandel.Close,APrice);
end;

function ToValueCandel(const APaternCandel: TPaternCandel; const APrice: Double): TValueCandel;
begin
  Result.Open  := (APaternCandel.Open * APrice)/PATERN_PRICE;
  Result.High  := (APaternCandel.High * APrice)/PATERN_PRICE;
  Result.Low   := (APaternCandel.Low  * APrice)/PATERN_PRICE;
  Result.Close := (APaternCandel.Close * APrice)/PATERN_PRICE;
end;

end.
