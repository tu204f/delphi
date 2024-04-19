unit Lb.Agent;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.Bybit.Kline;

type
  ///<summary>
  /// Единичная структура данных
  ///</summary>
  TStructurePrice = class(TObject)
  public type
    TCandel = record
      Time : Double;
      Week : Double;
      Open : Double;
      High : Double;
      Low  : Double;
      Close: Double;
      Vol  : Double;
    end;
    TCandelList = TList<TCandel>;
  private
    FCandels: TCandelList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetCandels(ACountCandel: Integer; ABybitCandels: TCandelObjectList);
    property Candels: TCandelList read FCandels;
  end;

  ///<summary>
  /// Сделка
  ///</summary>
  TTrade = class(TObject)
  private
    FOpenStructure: TStructurePrice;
    FCloseStructure: TStructurePrice;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>
    /// Структура открытия
    ///</summary>
    property OpenStructure: TStructurePrice read FOpenStructure;
    ///<summary>
    /// Закрытия
    ///</summary>
    property CloseStructure: TStructurePrice read FCloseStructure;
  end;


  ///<summary>
  /// Агент который фиксирует оперци, с историческими данными
  ///</summary>
  TAgent = class(TObject)
  private
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

uses
  System.DateUtils;

{ TStructurePrice }

constructor TStructurePrice.Create;
begin
  FCandels := TCandelList.Create;
end;

destructor TStructurePrice.Destroy;
begin
  FreeAndNil(FCandels);
  inherited;
end;

procedure TStructurePrice.SetCandels(ACountCandel: Integer; ABybitCandels: TCandelObjectList);

  function _GetConvertTime(const ATime: TDateTime): Double;
  var
    xValueTime: Double;
    xHour, xMin, xSec, xMSec: Word;
  begin
    DecodeTime(ATime, xHour, xMin, xSec, xMSec);
    xValueTime := xHour * 3600 + xMin * 60 + xSec;
    Result := xValueTime/(24 * 3600);
  end;

  function _GetConvertWeek(const AData: TDateTime): Double;
  var
    xWeekIndex: Integer;
  begin
    xWeekIndex := DayOfTheWeek(AData);
    Result := xWeekIndex/7;
  end;

  function _GetBybitToCandel(ACandelObject: TCandelObject): TStructurePrice.TCandel;
  var
    xCandel: TStructurePrice.TCandel;
  begin
    xCandel.Time  := _GetConvertTime(ACandelObject.DateTime);
    xCandel.Week  := _GetConvertWeek(ACandelObject.DateTime);
    xCandel.Open  := ACandelObject.Open;
    xCandel.High  := ACandelObject.High;
    xCandel.Low   := ACandelObject.Low;
    xCandel.Close := ACandelObject.Close;
    Result := xCandel;
  end;

  procedure _MaxAndMinValue(ACandels: TCandelList; var AMaxPrice, AMinPrice, AMaxVol, AMinVol: Double);
  var
    i: Integer;
    xCandel: TStructurePrice.TCandel;
  begin
    AMaxPrice := 0;
    AMinPrice := 0;
    AMaxVol := 0;
    AMinVol := 0;
    if ACandels.Count > 0 then
    begin
      xCandel := ACandels[0];
      AMaxPrice := xCandel.High;
      AMinPrice := xCandel.Low;
      AMaxVol   := xCandel.Vol;
      AMinVol   := xCandel.Vol;
      for i := 1 to ACandels.Count - 1 do
      begin
        xCandel := ACandels[i];
        if xCandel.High > AMaxPrice then
          AMaxPrice := xCandel.High;
        if xCandel.Low < AMinPrice then
          AMinPrice := xCandel.Low;

        if xCandel.Vol > AMaxVol then
          AMaxVol := xCandel.Vol;
        if xCandel.Vol < AMinVol then
          AMinVol := xCandel.Vol;
      end;
    end;
  end;

  function _GetRateValue(AValue: Double; AMaxValue, AMinValue: Double): Double;
  begin
    Result := 0;
    if (AMaxValue > AMinValue) then
      Result := (AValue - AMinValue)/(AMaxValue - AMinValue);
  end;

  function _GetRateCandel(ACandel: TStructurePrice.TCandel; AMaxPrice, AMinPrice, AMaxVol, AMinVol: Double): TStructurePrice.TCandel;
  var
    xCandel: TStructurePrice.TCandel;
  begin
    xCandel.Time := ACandel.Time;
    xCandel.Week := ACandel.Week;
    xCandel.Open  := _GetRateValue(ACandel.Open ,AMaxPrice,AMinPrice);
    xCandel.High  := _GetRateValue(ACandel.High ,AMaxPrice,AMinPrice);
    xCandel.Low   := _GetRateValue(ACandel.Low  ,AMaxPrice,AMinPrice);
    xCandel.Close := _GetRateValue(ACandel.Close,AMaxPrice,AMinPrice);
    xCandel.Vol   := _GetRateValue(ACandel.Vol  ,AMaxVol  ,AMinVol);
    Result := xCandel;
  end;

var
  i: Integer;
  xCandelObject: TCandelObject;
  xCandel: TStructurePrice.TCandel;
  xCandels: TCandelList;
  xMaxPrice, xMinPrice: Double;
  xMaxVol  , xMinVol: Double;
begin
  FCandels.Clear;
  xCandels := TCandelList.Create;
  try
    if ACountCandel > 0 then
    begin
      for i := 0 to ACountCandel - 1 do
      begin
        xCandelObject := ABybitCandels.Items[i];
        xCandel := _GetBybitToCandel(xCandelObject);
        xCandels.Add(xCandel);
      end;
      _MaxAndMinValue(xCandels, xMaxPrice, xMinPrice, xMaxVol, xMinVol);
      if (xMaxPrice > xMinPrice) and (xMaxVol > xMinVol) then
      begin
        for xCandel in xCandels do
          FCandels.Add(_GetRateCandel(xCandel, xMaxPrice, xMinPrice, xMaxVol, xMinVol));
      end;
    end;
  finally
    FreeAndNil(xCandels);
  end;
end;

{ TTrade }

constructor TTrade.Create;
begin
  FOpenStructure  := TStructurePrice.Create;
  FCloseStructure := TStructurePrice.Create;
end;

destructor TTrade.Destroy;
begin
  FreeAndNil(FOpenStructure);
  FreeAndNil(FCloseStructure);
  inherited;
end;

{ TAgent }

constructor TAgent.Create;
begin

end;

destructor TAgent.Destroy;
begin

  inherited;
end;


end.
