unit Lb.Crossing;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  ///<summary>
  /// Прошли верхнию границу
  ///</summary>
  TTypeCrossing = (
    tcNull,
    tcHigh,
    tcLow
  );

  ///<summary>
  /// Событие пересечение значение
  ///</summary>
  TEventOnCrossing = procedure(ASender: TObject; APrice: Double; ATypeCrossing: TTypeCrossing) of object;

  ///<summary>
  /// Объект пересечение объекта
  ///</summary>
  TCrossing = class(TObject)
  private
    FRate: Double;
    FPrice: Double;
    FPriceHigh: Double;
    FPriceLow: Double;
    FDeviation: Double;
    FTypeCrossing: TTypeCrossing;
    FOldTypeCrossing: TTypeCrossing;
  private
    FOnCrossing: TEventOnCrossing;
    FOnCrossingValue: TEventOnCrossing;
  protected
    procedure DoValueHigh;
    procedure DoValueLow;
    procedure DoCrossingValue;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetNewPrice(const APrice, ADeviation, ARate: Double); virtual;
    procedure SetPrice(const APrice: Double); virtual;
    property Rate: Double read FRate;
    property Price: Double read FPrice;
    property PriceHigh: Double read FPriceHigh;
    property PriceLow: Double read FPriceLow;
    property Deviation: Double read FDeviation;
    property TypeCrossing: TTypeCrossing read FTypeCrossing;
  public
    property OnCrossing: TEventOnCrossing write FOnCrossing;
    property OnCrossingValue: TEventOnCrossing write FOnCrossingValue;
  end;

implementation

{ TCrossing }

constructor TCrossing.Create;
begin
  FRate := 0;
  FPrice := 0;
  FPriceHigh := 0;
  FPriceLow := 0;
  FDeviation := 0;
  FTypeCrossing := TTypeCrossing.tcNull;
  FOldTypeCrossing := TTypeCrossing.tcNull;
end;

destructor TCrossing.Destroy;
begin

  inherited;
end;


procedure TCrossing.DoValueHigh;
begin
  FTypeCrossing := TTypeCrossing.tcHigh;
  if Assigned(FOnCrossing) then
    FOnCrossing(Self,FPrice,FTypeCrossing);
  DoCrossingValue;
end;

procedure TCrossing.DoValueLow;
begin
  FTypeCrossing := TTypeCrossing.tcLow;
  if Assigned(FOnCrossing) then
    FOnCrossing(Self,FPrice,FTypeCrossing);
  DoCrossingValue;
end;

procedure TCrossing.DoCrossingValue;
begin
  if FOldTypeCrossing <> FTypeCrossing then
  begin
    if Assigned(FOnCrossingValue) then
      FOnCrossingValue(Self, FPrice, FTypeCrossing);
    FOldTypeCrossing := FTypeCrossing;
  end;
end;

procedure TCrossing.SetNewPrice(const APrice, ADeviation, ARate: Double);
begin
  if (APrice = 0) or (ARate = 0) or (ADeviation = 0) then
    raise Exception.Create('Error Message: Значение равное нулю');

  FRate := ARate;
  FPrice := APrice;
  FDeviation := ADeviation;
  FPriceHigh := FPrice + FDeviation * FRate;
  FPriceLow := FPrice - FDeviation * FRate;
end;

procedure TCrossing.SetPrice(const APrice: Double);
begin
  FPrice := APrice;
  if FPriceHigh < FPrice then
    DoValueHigh;
  if FPriceLow  > FPrice then
    DoValueLow;
end;

end.
