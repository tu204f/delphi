unit Lb.Breakdown;

interface

{$I debug_app.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils;

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
  TEventOnClose = procedure(ASender: TObject) of object;

  ///<summary>
  /// Объект пересечение объекта
  ///</summary>
  TBreakdown = class(TObject)
  private
    FCandel: TCandel;
  private
    FRate: Double;
    FPrice: Double;
    FPriceHigh: Double;
    FPriceLow: Double;
    FDeviation: Double;
    FTypeCrossing: TTypeCrossing;
    FOldTypeCrossing: TTypeCrossing;
    FCountOpen: Integer;
  private
    FOnCrossing: TEventOnCrossing;
    FOnCrossingValue: TEventOnCrossing;
    FOnClosePosition: TEventOnClose;
  protected
    procedure DoValueHigh;
    procedure DoValueLow;
    procedure DoCrossingValue;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetCandel(const ACandel: TCandel; const ADeviation, ARate: Double); virtual;
    property Candel: TCandel read FCandel;
    property Rate: Double read FRate;
    property Price: Double read FPrice;
    property PriceHigh: Double read FPriceHigh;
    property PriceLow: Double read FPriceLow;
    property Deviation: Double read FDeviation;
    property TypeCrossing: TTypeCrossing read FTypeCrossing;
  public
    property OnCrossing: TEventOnCrossing write FOnCrossing;
    property OnCrossingValue: TEventOnCrossing write FOnCrossingValue;
    property OnClosePosition: TEventOnClose write FOnClosePosition;
  end;

implementation

uses
{$IFDEF DEBUG}
  Lb.Logger,
{$ENDIF}
  System.DateUtils;

{ TBreakdown }

constructor TBreakdown.Create;
begin
  FRate := 0;
  FPrice := 0;
  FPriceHigh := 0;
  FPriceLow := 0;
  FDeviation := 0;
  FTypeCrossing := TTypeCrossing.tcNull;
  FOldTypeCrossing := TTypeCrossing.tcNull;
  FCountOpen := 0;
end;

destructor TBreakdown.Destroy;
begin

  inherited;
end;

procedure TBreakdown.DoValueHigh;
begin
  if FCountOpen <= 1 then
    Exit;
{$IFDEF DBG_BREAKDOWN_HIGH}
  TLogger.LogTree(0,'TBreakdown.DoValueHigh');
{$ENDIF}
  FTypeCrossing := TTypeCrossing.tcHigh;
  if Assigned(FOnCrossing) then
    FOnCrossing(Self,FPrice,FTypeCrossing);
  DoCrossingValue;
end;

procedure TBreakdown.DoValueLow;
begin
  if FCountOpen <= 1 then
    Exit;
{$IFDEF DBG_BREAKDOWN_HIGH}
  TLogger.LogTree(0,'TBreakdown.DoValueLow');
{$ENDIF}
  FTypeCrossing := TTypeCrossing.tcLow;
  if Assigned(FOnCrossing) then
    FOnCrossing(Self,FPrice,FTypeCrossing);
  DoCrossingValue;
end;

procedure TBreakdown.DoCrossingValue;
begin
  if FOldTypeCrossing <> FTypeCrossing then
  begin
    {$IFDEF DBG_BREAKDOWN_CROSSING}
    TLogger.LogTree(0,'TBreakdown.DoCrossingValue');
    {$ENDIF}
    if Assigned(FOnCrossingValue) then
      FOnCrossingValue(Self, FPrice, FTypeCrossing);
    FOldTypeCrossing := FTypeCrossing;
  end;
end;

procedure TBreakdown.SetCandel(const ACandel: TCandel; const ADeviation, ARate: Double);

  procedure _OpenPrice(const APrice, ADeviation, ARate: Double);
  begin
    if (APrice = 0) or (ARate = 0) or (ADeviation = 0) then
      raise Exception.Create('Error Message: Значение равное нулю');
    {$IFDEF DBG_BREAKDOWN}
    TLogger.LogTree(0,'BEGIN.TBreakdown.SetCandel._OpenPrice');
    {$ENDIF}
    Inc(FCountOpen);
    FRate := ARate;
    FPrice := APrice;
    FDeviation := ADeviation;
    FPriceHigh := FPrice + FDeviation * FRate;
    FPriceLow := FPrice - FDeviation * FRate;
    {$IFDEF DBG_BREAKDOWN}
    TLogger.LogTreeText(3,'>> Rate: ' + FRate.ToString);
    TLogger.LogTreeText(3,'>> Price: ' + FPrice.ToString);
    TLogger.LogTreeText(3,'>> Deviation: ' + FDeviation.ToString);
    TLogger.LogTreeText(3,'>> PriceHigh: ' + FPriceHigh.ToString);
    TLogger.LogTreeText(3,'>> PriceLow: ' + FPriceLow.ToString);
    {$ENDIF}
  end;

  procedure _UpDataPrice(const APrice: Double);
  begin
    {$IFDEF DBG_BREAKDOWN}
    TLogger.LogTree(0,'BEGIN.TBreakdown.SetCandel._UpDataPrice');
    {$ENDIF}
    FPrice := APrice;
    if FPriceHigh < FPrice then
      DoValueHigh;
    if FPriceLow  > FPrice then
      DoValueLow;
  end;

begin
  {$IFDEF DBG_BREAKDOWN}
  TLogger.LogTree(0,'BEGIN.TBreakdown.SetCandel');
  {$ENDIF}
  if FCandel.Time = ACandel.Time then
    _UpDataPrice(ACandel.Close)
  else
    _OpenPrice(ACandel.Open, ADeviation, ARate);
  FCandel := ACandel;
  {$IFDEF DBG_BREAKDOWN}
  TLogger.LogTree(0,'BEGIN.TBreakdown.SetCandel');
  {$ENDIF}
end;

end.
