unit Lb.Bot;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils,
  Lb.Journal.Trading,
  Lb.Breakdown;

type
  ///<summary>
  /// Рабочий бот
  ///</summary>
  ///<remarks>
  /// Укаждого бота свой набор заявок
  ///</remarks>
  TWorkBot = class(TObject)
  private
    FIsRevers: Boolean;
    FRate: Double;
    FBreakdown: TBreakdown;
    FJournalManager: TJournalManager;
    procedure BreakdownOnCrossingValue(ASender: TObject; APrice: Double; ATypeCrossing: TTypeCrossing);
  private
    FSourceLine: TSourceLine;
    function GetQty: Double;
    procedure EventPositionClose(ASender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>
    /// Событие обновления данных, платформы
    ///</summary>
    procedure SetTradingPlatform(const ASourceLine: TSourceLine);
    ///<summary>
    /// Включить реверс операции
    ///</summary>
    property IsRevers: Boolean read FIsRevers write FIsRevers;
  public
    ///<summary>
    /// Отклонение работы
    ///</summary>
    property Rate: Double read FRate write FRate;
    ///<summary>
    /// Журнал торговых операций
    ///</summary>
    property JournalManager: TJournalManager read FJournalManager;
    ///<summary>
    /// Значение пересечение
    ///</summary>
    property Breakdown: TBreakdown read FBreakdown;
  public
    procedure OpenPosition(const APrice: Double; const ASide: TTypeBuySell);
    procedure OpenPositionBuy;
    procedure OpenPositionSell;
    procedure ClosePosition;
  end;

implementation

uses
  System.DateUtils;


{ TWorkBot }

constructor TWorkBot.Create;
begin
  FIsRevers := False;
  FRate := 1;
  FJournalManager  := TJournalManager.Create;

  FBreakdown  := TBreakdown.Create;
  FBreakdown.OnCrossing := nil;
  FBreakdown.OnCrossingValue := BreakdownOnCrossingValue;
end;

destructor TWorkBot.Destroy;
begin
  FreeAndNil(FBreakdown);
  FreeAndNil(FJournalManager);
  inherited;
end;

procedure TWorkBot.BreakdownOnCrossingValue(ASender: TObject; APrice: Double; ATypeCrossing: TTypeCrossing);
begin
  // Событие пересечение объекта
  {$IFDEF DBG_CROSSING_VALUE}
  TLogger.LogTree(0,'BEGIN.BreakdownOnCrossingValue');
  {$ENDIF}
  case ATypeCrossing of
    tcHigh: begin
      OpenPositionBuy;
    end;
    tcLow: begin
      OpenPositionSell;
    end;
  end;
  {$IFDEF DBG_CROSSING_VALUE}
  TLogger.LogTree(0,'END.BreakdownOnCrossingValue');
  {$ENDIF}
end;

function TWorkBot.GetQty: Double;
var
  xCount: INteger;
  xPosition: TJournalPosition;
begin
  Result := 1;
  xCount := JournalManager.Positions.Count;
  if xCount > 0 then
  begin
    xPosition := JournalManager.Positions[xCount - 1];
    if xPosition.TypeTrade = TTypeTrade.ttOpen then
      Result := 2;
  end;
end;

procedure TWorkBot.OpenPosition(const APrice: Double; const ASide: TTypeBuySell);
var
{$IFDEF DBG_SEND_TRADE}
  xQty  : Double;
{$ENDIF}
  xPosition: TJournalPosition;
begin
  {$IFDEF DBG_OPEN_POSITION}
  TLogger.LogTree(0,'TWorkBot.OpenPosition');
  {$ENDIF}
  if APrice > 0 then
  begin
    {$IFDEF DBG_SEND_TRADE}
    xQty := GetQty;
    {$ENDIF}
    xPosition := JournalManager.GetCreateJournalPosition;
    xPosition.OnClose := EventPositionClose;
    with xPosition do
    begin
      OpenTime := GetNewDateTime;
      OpenPrice := APrice;
      Qty := 1;
      Side := ASide;
      IsActive := True;
      TypeTrade := TTypeTrade.ttOpen;
      Triling := 20;
      //RatesSL := 10;
      //RatesTK := 10;
      DoOpen;
    end;
    {$IFDEF DBG_SEND_TRADE}
    FTradingPlatform.SendTrade(
      xPosition.OpenTime,
      xPosition.OpenPrice + 2,
      xQty,
      xPosition.Side
    );
    {$ENDIF}
  end;
end;

procedure TWorkBot.OpenPositionBuy;
var
  xPrice: Double;
  xSide: TTypeBuySell;
begin
  {$IFDEF DBG_OPEN_POSITION}
  TLogger.LogTree(0,'TWorkBot.OpenPositionBuy');
  {$ENDIF}
  ClosePosition;

  if FIsRevers then
  begin
    xSide := TTypeBuySell.tsBuy;
    xPrice := FSourceLine.Ask;
  end
  else
  begin
    xSide := TTypeBuySell.tsSell;
    xPrice := FSourceLine.Bid;
  end;
  OpenPosition(xPrice, xSide);
end;

procedure TWorkBot.OpenPositionSell;
var
  xPrice: Double;
  xSide: TTypeBuySell;
begin
  {$IFDEF DBG_OPEN_POSITION}
  TLogger.LogTree(0,'TWorkBot.OpenPositionSell');
  {$ENDIF}
  ClosePosition;

  if FIsRevers then
  begin
    xSide := TTypeBuySell.tsSell;
    xPrice := FSourceLine.Bid;
  end
  else
  begin
    xSide := TTypeBuySell.tsBuy;
    xPrice := FSourceLine.Ask;
  end;
  OpenPosition(xPrice, xSide);
end;


procedure TWorkBot.ClosePosition;
var
  xPrice: Double;
  xPosition: TJournalPosition;
  i, iCount: Integer;
begin
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
            TTypeBuySell.tsBuy : xPrice := FSourceLine.Bid;
            TTypeBuySell.tsSell: xPrice := FSourceLine.Ask;
          else
            xPrice := 0;
          end;

          if xPrice <= 0 then
            Exit;

          CloseTime := GetNewDateTime;
          ClosePrice := xPrice;
          IsActive := False;
          TypeTrade := TTypeTrade.ttCloseUser;
          DoClose;
        end;
      end;
    end;

end;

procedure TWorkBot.EventPositionClose(ASender: TObject);
begin
  {$IFDEF DBG_WORK_BOT}
  TLogger.LogTree(0,'TWorkBot.PositionClose');
  {$ENDIF}
end;

procedure TWorkBot.SetTradingPlatform(const ASourceLine: TSourceLine);
var
  xCandel: TCandel;
  xDeviation: Double;
begin
  FSourceLine := ASourceLine;
  {$IFDEF DBG_WORK_BOT}
  TLogger.LogTree(0,'BEGIN.SetTradingPlatform');
  {$ENDIF}
  xDeviation := FSourceLine.Deviation;
  xCandel := FSourceLine.Candel;
  FBreakdown.SetCandel(
    xCandel,
    xDeviation,
    FRate
  );
  {$IFDEF DBG_WORK_BOT}
  TLogger.LogTree(0,'END.SetTradingPlatform');
  {$ENDIF}
end;

end.
