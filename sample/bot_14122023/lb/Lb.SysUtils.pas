unit Lb.SysUtils;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants;

type
  TCandel = record
    Open : Double;
    High : Double;
    Low  : Double;
    Close: Double;
    Vol  : Double;
  end;

  TTiket = record
    Time: Integer; // секундах
    Last: Double;
    Vol: Double;
  end;

  TParam = record
    T: Double;  // Значение 1 - быки, 0 - медведи, 0,5 - не рыба не мяса
    H: Double;  // Верзня тень
    B: Double;  // Тело
    L: Double;  // Нижня тень
  public
    function GetSum: Double;
  end;

function GetParamToCandel(const ACandel: TCandel): TParam;
//function GetRandomMode: TMode;

implementation

//function GetRandomMode: TMode;
//var
//  xMode: Integer;
//begin
//  // Здесь модуль принимает решение
//  xMode := Random(3);
//  Result := TMode(xMode);
//end;


function GetParamToCandel(const ACandel: TCandel): TParam;
var
  xParam: TParam;
  xHL: Double;
begin
  xHL := ACandel.High - ACandel.Low;
  if xHL > 0 then
  begin
    if ACandel.Open < ACandel.Close then
    begin
      // Рынок растет
      xParam.T := 1;
      xParam.H := (ACandel.High  - ACandel.Close)/xHL;
      xParam.B := (ACandel.Close - ACandel.Open)/xHL;
      xParam.L := (ACandel.Open  - ACandel.Low)/xHL;
    end else if ACandel.Open > ACandel.Close then
    begin
      // Падение рынка
      xParam.T := 0;
      xParam.H := (ACandel.High  - ACandel.Open)/xHL;
      xParam.B := (ACandel.Open  - ACandel.Close)/xHL;
      xParam.L := (ACandel.Close - ACandel.Low)/xHL;
    end else
    begin
      // Цена открытие равное цена закрытие
      xParam.T := 0.5;
      xParam.H := (ACandel.High - ACandel.Open)/xHL;
      xParam.B := 0;
      xParam.L := (ACandel.Open - ACandel.Low)/xHL;
    end;
    Result := xParam;
  end
  else
    raise Exception.Create('Error Message: Очень грубая ошибка');
end;

{ TParam }

function TParam.GetSum: Double;
begin
  // Для впроверки состояние
  Result := H + B + L;
end;

end.
