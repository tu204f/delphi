unit Lb.SysUtils;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  ILogger = interface
    procedure Log(S: String);
  end;

  IStockMarket = interface
    procedure DoBegin;
    procedure DoEnd;
  end;

function PriceToY(const AHeight, APrice, AMaxPrice, AMinPrice: Double): Double; inline;

implementation


function PriceToY(const AHeight, APrice, AMaxPrice, AMinPrice: Double): Double;
var
  xDelta: Double;
begin
  xDelta := (APrice - AMinPrice)/(AMaxPrice - AMinPrice);
  Result := AHeight - xDelta * AHeight;
end;

end.
