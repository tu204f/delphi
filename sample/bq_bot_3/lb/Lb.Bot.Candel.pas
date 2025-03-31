unit Lb.Bot.Candel;

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
  /// Рабочий бот
  ///</summary>
  ///<remarks>
  /// Укаждого бота свой набор заявок
  ///</remarks>
  TWorkBot = class(TObject)
  private
    FStateMarket: TStateMarket;
    FTradingPlatform: TTradingPlatform;
    FJournalManager: TJournalManager;
  protected
    function GetCurrentJournalPosition: TJournalPosition;
  private
    procedure OpenPosition(const APrice: Double; const ASide: TTypeBuySell);
    procedure OpenPositionBuy;
    procedure OpenPositionSell;
    procedure ClosePosition;

    procedure EventPositionOpen(const AJournalPosition: TJournalPosition);
    procedure EventPositionClose(const AJournalPosition: TJournalPosition);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    ///<summary>
    /// Новая свеча
    ///</summary>
    procedure SetTradingNewCandel;

    ///<summary>
    /// Событие обновления данных, платформы
    ///</summary>
    procedure SetTradingPlatform(const ATradingPlatform: TTradingPlatform);
  public
    ///<summary>
    /// Журнал торговых операций
    ///</summary>
    property JournalManager: TJournalManager read FJournalManager;
  end;

implementation

uses
{$IFDEF DEBUG}
  Lb.Logger,
{$ENDIF}
  System.DateUtils;


{ TWorkBot }

constructor TWorkBot.Create;
begin
  FTradingPlatform := nil;
  FJournalManager  := TJournalManager.Create;
end;

destructor TWorkBot.Destroy;
begin
  FreeAndNil(FJournalManager);
  inherited;
end;

procedure TWorkBot.OpenPosition(const APrice: Double; const ASide: TTypeBuySell);
var
  xPosition: TJournalPosition;
begin
  {$IFDEF DBG_OPEN_POSITION}
  TLogger.LogTree(0,'TWorkBot.OpenPosition');
  {$ENDIF}
  ClosePosition;
  if APrice > 0 then
  begin
    xPosition := JournalManager.GetCreateJournalPosition;
    xPosition.OnOpen  := EventPositionOpen;
    xPosition.OnClose := EventPositionClose;
    with xPosition do
    begin
      OpenTime := GetNewDateTime;
      OpenPrice := APrice;
      Qty := 1;
      Side := ASide;
      IsActive := True;
      TypeTrade := TTypeTrade.ttOpen;
      RatesSL := 10;
      RatesTK := 30;
      DoOpen;
    end;
  end;
end;

function TWorkBot.GetCurrentJournalPosition: TJournalPosition;
var
  Count: Integer;
  xJournalPosition: TJournalPosition;
begin
  Result := nil;
  Count := FJournalManager.Positions.Count;
  if Count > 0 then
  begin
    xJournalPosition := FJournalManager.Positions[Count - 1];
    Result := xJournalPosition;
  end;
end;

procedure TWorkBot.OpenPositionBuy;
var
  xPrice: Double;
  xSide: TTypeBuySell;
  xJournalPosition: TJournalPosition;
begin
  xJournalPosition := GetCurrentJournalPosition;
  if Assigned(xJournalPosition) then
  begin
    if (xJournalPosition.TypeTrade = TTypeTrade.ttOpen) and (xJournalPosition.Side = TTypeBuySell.tsBuy) then
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

procedure TWorkBot.OpenPositionSell;
var
  xPrice: Double;
  xSide: TTypeBuySell;
  xJournalPosition: TJournalPosition;
begin
  xJournalPosition := GetCurrentJournalPosition;
  if Assigned(xJournalPosition) then
  begin
    if (xJournalPosition.TypeTrade = TTypeTrade.ttOpen) and (xJournalPosition.Side = TTypeBuySell.tsSell) then
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


procedure TWorkBot.ClosePosition;
var
  xPrice: Double;
  xPosition: TJournalPosition;
  i, iCount: Integer;
begin
  if not Assigned(FTradingPlatform) then
    raise Exception.Create('Торговая платформа не определена');
  {$IFDEF DBG_OPEN_POSITION}
  TLogger.LogTree(0,'TWorkBot.ClosePosition');
  {$ENDIF}

  iCount := JournalManager.Positions.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
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

procedure TWorkBot.EventPositionOpen(const AJournalPosition: TJournalPosition);
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




procedure TWorkBot.EventPositionClose(const AJournalPosition: TJournalPosition);
{$IFDEF DBG_SEND_TRADE}
var
  xClosePrice: Double;
  xSide: TTypeBuySell;
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


procedure TWorkBot.SetTradingNewCandel;

{$IFDEF DBG_TRADING_NEW_CANDEL}
  procedure _LogCandel(const ACandel: TCandel);
  begin
    TLogger.LogTree(3,'Candel:>>' + ACandel.GetToStr);
  end;
{$ENDIF}

  function _GetIsTranding: Integer;
  var
    xCandel: TCandel;
    i, Count, xCountTranding: Integer;
  begin
    xCountTranding := 0;
    Count := FStateMarket.Candels.Count;
    if Count > 0 then
      for i := 1 to 2 do
      begin
        xCandel := FStateMarket.Candels[i];
        case xCandel.TypeCandel of
          tcGreen: xCountTranding := xCountTranding + 1;
          tcRed: xCountTranding := xCountTranding - 1;
        end;
      end;
    Result := xCountTranding;
  end;

  function _GetIsTrandBuy: Boolean;
  begin
    Result := (_GetIsTranding = 2);
  end;

  function _GetIsTrandSell: Boolean;
  begin
    Result := (_GetIsTranding = -2);
  end;

begin
  {$IFDEF DBG_TRADING_NEW_CANDEL}
  TLogger.LogTree(0,'TWorkBot.SetTradingNewCandel');
  {$ENDIF}
  if Assigned(FStateMarket) then
    if (FStateMarket.Candels.Count > 0) and (FStateMarket.Ask > 0) and (FStateMarket.Bid > 0) then
    begin
      if _GetIsTrandBuy then
        OpenPositionBuy
      else if _GetIsTrandSell then
        OpenPositionSell;
    end;
end;

procedure TWorkBot.SetTradingPlatform(const ATradingPlatform: TTradingPlatform);
begin
  FTradingPlatform := ATradingPlatform;
  {$IFDEF DBG_TRADING_UP_DATA_CANDEL}
  TLogger.LogTree(0,'TWorkBot.SetTradingPlatform()');
  {$ENDIF}
  if Assigned(FTradingPlatform) then
  begin
    {$IFDEF DBG_WORK_BOT}
    TLogger.LogTree(0,'BEGIN.SetTradingPlatform');
    {$ENDIF}
    FStateMarket := FTradingPlatform.StateMarket;
    {$IFDEF DBG_WORK_BOT}
    TLogger.LogTree(0,'END.SetTradingPlatform');
    {$ENDIF}
  end;
end;

end.
