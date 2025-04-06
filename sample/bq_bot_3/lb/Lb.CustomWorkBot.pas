unit Lb.CustomWorkBot;

interface

{$I debug_volt.inc}

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
  /// Базовой объект работы
  ///</summary>
  TCustomWorkBot = class(TObject)
  private
    FStateMarket: TStateMarket;
    FTradingPlatform: TTradingPlatform;
    FJournalManager: TJournalManager;
  private
    FCurrentPosition: TJournalPosition;
  protected
    procedure OpenPosition(const APrice: Double; const ASide: TTypeBuySell);
    procedure OpenPositionBuy;
    procedure OpenPositionSell;
    procedure ClosePosition;
  private
    procedure EventPositionOpen(const AJournalPosition: TJournalPosition);
    procedure EventPositionClose(const AJournalPosition: TJournalPosition);
  protected
    function GetInfoValue: String; virtual;
    procedure SetCloseCurrentPosition(const APosition: TJournalPosition); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure TradingNewCandel; virtual;
    procedure TradingUpDataCandel(const ATradingPlatform: TTradingPlatform); virtual;
    property JournalManager: TJournalManager read FJournalManager;
    property StateMarket: TStateMarket read FStateMarket;
  end;

implementation

uses
{$IFDEF DEBUG}
  Lb.Logger,
{$ENDIF}
  System.DateUtils;

{ TCustomWorkBot }

constructor TCustomWorkBot.Create;
begin
  FStateMarket := nil;
  FTradingPlatform := nil;
  FCurrentPosition := nil;
  FJournalManager  := TJournalManager.Create;
end;

destructor TCustomWorkBot.Destroy;
begin
  if Assigned(FJournalManager) then
    FreeAndNil(FJournalManager);
  inherited;
end;

function TCustomWorkBot.GetInfoValue: String;
begin
  Result := '-- ** --';
end;


procedure TCustomWorkBot.SetCloseCurrentPosition(const APosition: TJournalPosition);
begin
  // Устанавливаем параметры закрытие позиции
end;

procedure TCustomWorkBot.TradingNewCandel;
begin
  // Новая свеча
end;

procedure TCustomWorkBot.TradingUpDataCandel(const ATradingPlatform: TTradingPlatform);
begin
  FTradingPlatform := ATradingPlatform;
  if Assigned(FTradingPlatform) then
  begin
    FStateMarket := FTradingPlatform.StateMarket;
  end;
end;

procedure TCustomWorkBot.OpenPosition(const APrice: Double; const ASide: TTypeBuySell);
begin
  // Базовая процедура открытие позиции
  {$IFDEF DBG_OPEN_POSITION}
  TLogger.LogTree(0,'TWorkBot.OpenPosition');
  {$ENDIF}
  ClosePosition;
  if APrice > 0 then
  begin
    FCurrentPosition := JournalManager.GetCreateJournalPosition;
    FCurrentPosition.OnOpen  := EventPositionOpen;
    FCurrentPosition.OnClose := EventPositionClose;
    with FCurrentPosition do
    begin
      OpenTime := GetNewDateTime;
      OpenPrice := APrice;
      Qty := 1;
      Side := ASide;
      IsActive := True;
      TypeTrade := TTypeTrade.ttOpen;
      Self.SetCloseCurrentPosition(FCurrentPosition);
      InfoValue := GetInfoValue;
      DoOpen;
    end;
  end;
end;

procedure TCustomWorkBot.OpenPositionBuy;
var
  xPrice: Double;
  xSide: TTypeBuySell;
begin
  // Открытие по покупку
  if Assigned(FCurrentPosition) then
  begin
    if (FCurrentPosition.TypeTrade = TTypeTrade.ttOpen) and (FCurrentPosition.Side = TTypeBuySell.tsBuy) then
      Exit;
  end;

  if not Assigned(FTradingPlatform) then
    raise Exception.Create('Торговая платформа не определена');
  {$IFDEF DBG_OPEN_POSITION}
  TLogger.LogTree(0,'TWorkBot.OpenPositionBuy');
  {$ENDIF}
  xSide := TTypeBuySell.tsBuy;
  xPrice := FTradingPlatform.StateMarket.Ask;
  OpenPosition(xPrice, xSide);
end;

procedure TCustomWorkBot.OpenPositionSell;
var
  xPrice: Double;
  xSide: TTypeBuySell;
begin
  // Открытие на прадажу
  if Assigned(FCurrentPosition) then
  begin
    if (FCurrentPosition.TypeTrade = TTypeTrade.ttOpen) and (FCurrentPosition.Side = TTypeBuySell.tsSell) then
      Exit;
  end;

  if not Assigned(FTradingPlatform) then
    raise Exception.Create('Торговая платформа не определена');
  {$IFDEF DBG_OPEN_POSITION}
  TLogger.LogTree(0,'TWorkBot.OpenPositionSell');
  {$ENDIF}
  xSide := TTypeBuySell.tsSell;
  xPrice := FTradingPlatform.StateMarket.Bid;
  OpenPosition(xPrice, xSide);
end;

procedure TCustomWorkBot.ClosePosition;
var
  xPrice: Double;
  xPosition: TJournalPosition;
  i, iCount: Integer;
begin
  // Процедура закрытие текущий позиции
  if not Assigned(FTradingPlatform) then
    raise Exception.Create('Торговая платформа не определена');
  {$IFDEF DBG_OPEN_POSITION}
  TLogger.LogTree(0,'TWorkBot.ClosePosition');
  {$ENDIF}
  iCount := JournalManager.Positions.Count;
  if iCount > 0 then
    for i := iCount - 1 downto 0 do
    begin
      xPosition := JournalManager.Positions[i];
      if xPosition.TypeTrade = TTypeTrade.ttOpen then
      begin

        with xPosition do
        begin
          case Side of
            TTypeBuySell.tsBuy : xPrice := FTradingPlatform.StateMarket.Bid;
            TTypeBuySell.tsSell: xPrice := FTradingPlatform.StateMarket.Ask;
          else
            xPrice := 0;
          end;

          if xPrice <= 0 then
            Exit;

          CloseTime := GetNewDateTime;
          ClosePrice := xPrice;
          IsActive := False;
          TypeTrade := TTypeTrade.ttClose;
          DoClose;
        end;
      end;
    end;
end;

procedure TCustomWorkBot.EventPositionOpen(const AJournalPosition: TJournalPosition);
{$IFDEF DBG_SEND_TRADE}
var
  xOpenPrice: Double;
{$ENDIF}
begin
  {$IFDEF DBG_WORK_BOT}
  TLogger.LogTree(0,'TWorkBot.EventPositionOpen');
  {$ENDIF}
  {$IFDEF DBG_SEND_TRADE}
  xOpenPrice := 0;
  case AJournalPosition.Side of
    TTypeBuySell.tsBuy : xOpenPrice := AJournalPosition.OpenPrice + 2;
    TTypeBuySell.tsSell: xOpenPrice := AJournalPosition.OpenPrice - 2;
  end;
  if xOpenPrice > 0 then
  begin
    AJournalPosition.OpenLinkID := FTradingPlatform.SendTrade(
      AJournalPosition.OpenTime,
      xOpenPrice,
      AJournalPosition.Qty,
      AJournalPosition.Side
    );
  end;
  {$ENDIF}
end;


procedure TCustomWorkBot.EventPositionClose(const AJournalPosition: TJournalPosition);
{$IFDEF DBG_SEND_TRADE}
var
  xOpenPrice: Double;
{$ENDIF}
begin
  {$IFDEF DBG_WORK_BOT}
  TLogger.LogTree(0,'TWorkBot.PositionClose');
  {$ENDIF}
  {$IFDEF DBG_SEND_TRADE}
  xClosePrice := 0;
  xSide := GetCrossSide(AJournalPosition.Side);
  case xSide of
    TTypeBuySell.tsBuy : xClosePrice := AJournalPosition.ClosePrice + 2;
    TTypeBuySell.tsSell: xClosePrice := AJournalPosition.ClosePrice - 2;
  end;
  if xClosePrice > 0 then
    AJournalPosition.CloseLinkId := FTradingPlatform.SendTrade(
      AJournalPosition.CloseTime,
      xClosePrice,
      AJournalPosition.Qty,
      xSide
    );
  {$ENDIF}
end;

end.
