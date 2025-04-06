unit Lb.Bot.V2;

interface

{$IFDEF DEBUG}
  {$DEFINE CANDEL_DBG}
{$ENDIF}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils,
  Lb.Platform,
  Lb.Journal.Trading;

type
  ///<summary>
  /// Производим оценку волотильности по рынку
  ///</summary>
  TWorkBotDeviation = class(TObject)
  private
    FCountTiket: Integer;
    FCandel: TCandel;
    FDeviation: Double;
    FRate: Double;
  private
    FIsReversPosition: Boolean;
    FRatio: Double;
    FOnLongOpen: TNotifyEvent;
    FOnShortOpen: TNotifyEvent;
    FJournalManager: TJournalManager;
    FTradingPlatform: TTradingPlatform;
  private
    FPriceHigh: Double;
    FPriceLow: Double;
    FPosition: TJournalPosition;
  protected
    procedure DoLongOpen;
    procedure DoShortOpen;
    procedure PositionClose(const AJournalPosition: TJournalPosition);
    procedure OpenPosition(const ASide: TTypeBuySell);
    procedure ClosePosition;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>
    /// Объект работы значение работы
    ///</summary>
    procedure SetUpDataParam; overload;
    ///<summary>
    /// Журнал торговых операций
    ///</summary>
    property JournalManager: TJournalManager read FJournalManager;
    ///<summary>
    /// Торговая плаформа
    ///</summary>
    property TradingPlatform: TTradingPlatform read FTradingPlatform write FTradingPlatform;
    property OnLongOpen: TNotifyEvent write FOnLongOpen;
    property OnShortOpen: TNotifyEvent write FOnShortOpen;
    property Ratio: Double read FRatio write FRatio;
    ///<summary>
    /// Переворачивать позицию квертомашкой
    ///</summary>
    property IsReversPosition: Boolean read FIsReversPosition write FIsReversPosition;
    ///<summary>
    /// Верхняя ценовая границу
    ///</summary>
    property PriceHigh: Double read FPriceHigh;
    ///<summary>
    /// Нижняя ценновая граница
    ///</summary>
    property PriceLog: Double read FPriceLow;
    ///<summary>Количество шагов обновления</summary>
    property CountTiket: Integer read FCountTiket;
    property Candel: TCandel read FCandel;
    property Deviation: Double read FDeviation;
    property Rate: Double read FRate;
  public
    procedure SetNewCandel;
    procedure SetParamValue(const ACandel: TCandel; const ADeviation, ARate: Double);
  end;

  ///<summary>
  /// Список объектов
  ///</summary>
  TWorkBotDeviationList = TObjectList<TWorkBotDeviation>;

function GetFloatToStr(const AValue: Double): String;

implementation

{$IFDEF DEBUG}
uses
  Lb.Logger;
{$ENDIF}

function GetFloatToStr(const AValue: Double): String;
var
  xS: String;
  xL: Integer;
begin
  xS := FloatToStr(AValue);
  xL := Length(xS);
  case xL of
    4: xS := xS + ',0000';
    6: xS := xS + '000';
    7: xS := xS + '00';
    8: xS := xS + '0';
  end;
  Result := xS;
end;

{ TWorkBotDeviation }

constructor TWorkBotDeviation.Create;
begin
  FCountTiket := 0;
  FPosition := nil;
  FRatio := 1;
  FJournalManager := TJournalManager.Create;
  FTradingPlatform := nil;
  FIsReversPosition := False;
  FPriceHigh := 0;
  FPriceLow := 0;
end;

destructor TWorkBotDeviation.Destroy;
begin
  FreeAndNil(FJournalManager);
  inherited;
end;

procedure TWorkBotDeviation.DoLongOpen;
begin
  {$IFDEF CANDEL_DBG}
  TLogger.LogTree(0,'TWorkBotDeviation.DoLongOpen:');
  {$ENDIF}

  if Assigned(FPosition) and (FPosition.Side = TTypeBuySell.tsSell) then
    ClosePosition;

  if not Assigned(FPosition) then
  begin
    Self.OpenPosition(TTypeBuySell.tsBuy);
    if Assigned(FOnLongOpen) then
      FOnLongOpen(Self);
  end;
end;

procedure TWorkBotDeviation.DoShortOpen;
begin
  {$IFDEF CANDEL_DBG}
  TLogger.LogTree(0,'TWorkBotDeviation.DoShortOpen:');
  {$ENDIF}
  if Assigned(FPosition) and (FPosition.Side = TTypeBuySell.tsBuy) then
    ClosePosition;
  if not Assigned(FPosition) then
  begin
    Self.OpenPosition(TTypeBuySell.tsSell);
    if Assigned(FOnShortOpen) then
      FOnShortOpen(Self);
  end;
end;

procedure TWorkBotDeviation.OpenPosition(const ASide: TTypeBuySell);

  function GetInfoValue: String;
  var
    xS: String;
    xCandel: TCandel;
    xDeviation: Double;
  begin
    xS := '';
    if Assigned(FTradingPlatform) then
      if FTradingPlatform.StateMarket.Candels.Count > 0 then
      begin
        xCandel    := FTradingPlatform.StateMarket.Candels[0];
        xDeviation := FTradingPlatform.ValueVolatility.DeviationValue;
        FPriceHigh := xCandel.Open + FRatio * xDeviation;
        FPriceLow  := xCandel.Open - FRatio * xDeviation;
        xS :=
          GetFloatToStr(FPriceHigh) + ';' +
          GetFloatToStr(xCandel.Close) + ';' +
          GetFloatToStr(FPriceLow) + ';' +
          xDeviation.ToString + ';' +
          FloatToStr(xCandel.Vol) + ';' +
          FloatToStr(FRatio) + ';';
      end;
    Result := xS;
  end;

var
  xPrice, xQty: Double;
begin
  {$IFDEF CANDEL_DBG}
  TLogger.LogTree(0,'TWorkBotDeviation.OpenPosition:');
  {$ENDIF}
  if Assigned(FPosition) and (FPosition.Side = ASide) then
    Exit;

  case ASide of
    tsBuy: xPrice := TradingPlatform.StateMarket.Ask;
    tsSell: xPrice := TradingPlatform.StateMarket.Bid;
  else
    xPrice := 0;
  end;

  if xPrice > 0 then
  begin
    if Assigned(FPosition) then
      xQty := 2
    else
      xQty := 1;

    FPosition := FJournalManager.GetCreateJournalPosition;
    FPosition.OnClose := PositionClose;
    with FPosition do
    begin
      OpenTime := GetNewDateTime;
      OpenPrice := xPrice;
      Qty := xQty;
      Side := ASide;
      IsActive := True;
      TypeTrade := TTypeTrade.ttOpen;
      RatesSL := 10;
      RatesTK := 30;
      InfoValue := GetInfoValue;
      DoOpen;
    end;

    {todo: открытие позции}
    TradingPlatform.SendTrade(
      FPosition.OpenTime,
      FPosition.OpenPrice,
      FPosition.Qty,
      FPosition.Side
    );

  end;
end;

procedure TWorkBotDeviation.ClosePosition;
var
  xPrice: Double;
begin
  {$IFDEF CANDEL_DBG}
  TLogger.LogTree(0,'TWorkBotDeviation.ClosePosition:');
  {$ENDIF}
  if Assigned(FPosition) then
    if FPosition.TypeTrade = TTypeTrade.ttOpen then
    begin
      with FPosition do
      begin
        case Side of
          TTypeBuySell.tsBuy : xPrice := TradingPlatform.StateMarket.Bid;
          TTypeBuySell.tsSell: xPrice := TradingPlatform.StateMarket.Ask;
        else
          xPrice := 0;
        end;

        if xPrice <= 0 then
          Exit;

        CloseTime := GetNewDateTime;
        ClosePrice := xPrice;
        IsActive := False;
        TypeTrade := TTypeTrade.ttClose;
        // DoClose;
      end;

      {todo: открытие позции}
      TradingPlatform.SendTrade(
        FPosition.CloseTime,
        FPosition.ClosePrice,
        FPosition.Qty,
        GetCrossSide(FPosition.Side)
      );
      FPosition.DoClose;

    end;
end;


procedure TWorkBotDeviation.PositionClose(const AJournalPosition: TJournalPosition);
begin
  FPosition := nil;
end;

procedure TWorkBotDeviation.SetNewCandel;
var
  xCandel: TCandel;
  xDeviation: Double;
{$IFDEF CANDEL_DBG}
  xS: String;
{$ENDIF}
begin
  FCountTiket := 0;
  {$IFDEF CANDEL_DBG}
  TLogger.LogText('*',80);
  {$ENDIF}
  if Assigned(FTradingPlatform) then
    if FTradingPlatform.StateMarket.Candels.Count > 0 then
    begin
      xCandel    := FTradingPlatform.StateMarket.Candels[0];
      xDeviation := FTradingPlatform.ValueVolatility.DeviationValue;
      FPriceHigh  := xCandel.Open + FRatio * xDeviation;
      FPriceLow   := xCandel.Open - FRatio * xDeviation;
      {$IFDEF CANDEL_DBG}
      xS :=
        'PriceHigh: ' + GetFloatToStr(FPriceHigh) + ' ' +
        'CloseLast: ' + GetFloatToStr(xCandel.Close) + ' ' +
        'FPriceLow: ' + GetFloatToStr(FPriceLow);
      TLogger.LogTree(0,'new_candel ' + xS);
      {$ENDIF}
    end;
end;

procedure TWorkBotDeviation.SetParamValue(const ACandel: TCandel; const ADeviation, ARate: Double);
begin
  Inc(FCountTiket);
  FCandel := ACandel;
  FDeviation := ADeviation;
  FRate := ARate;
  FPriceHigh    := FCandel.Open + FDeviation * FRate;
  FPriceLow     := FCandel.Open - FDeviation * FRate;
end;

procedure TWorkBotDeviation.SetUpDataParam;
var
  xCandel: TCandel;
{$IFDEF CANDEL_DBG}
  xS: String;
{$ENDIF}
begin
  if (FPriceHigh > 0) and (FPriceLow > 0) and (FPriceHigh > FPriceLow) then
  begin
    if Assigned(FTradingPlatform) then
      if FTradingPlatform.StateMarket.Candels.Count > 0 then
      begin
        xCandel    := FTradingPlatform.StateMarket.Candels[0];
        if xCandel.Close > FPriceHigh then
        begin
          {$IFDEF CANDEL_DBG}
          xS :=
            'PriceHigh: ' + GetFloatToStr(FPriceHigh) + ' ' +
            'CloseLast: ' + GetFloatToStr(xCandel.Close) + ' ' +
            'FPriceLow: ' + GetFloatToStr(FPriceLow);
          TLogger.LogTree(0,'up_data_param ' + xS);
          {$ENDIF}
          DoLongOpen;
        end;
        if xCandel.Close < FPriceLow then
        begin
          {$IFDEF CANDEL_DBG}
          xS :=
            'PriceHigh: ' + GetFloatToStr(FPriceHigh) + ' ' +
            'CloseLast: ' + GetFloatToStr(xCandel.Close) + ' ' +
            'FPriceLow: ' + GetFloatToStr(FPriceLow);
          TLogger.LogTree(0,'up_data_param ' + xS);
          {$ENDIF}
          DoShortOpen;
        end;
      end;
  end;
end;

end.
