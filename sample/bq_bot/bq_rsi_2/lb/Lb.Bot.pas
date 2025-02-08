unit Lb.Bot;

interface

{$I debug.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils,
  Lb.Platform,
  Lb.TradeBox,
  Lb.Journal.Trading_OLD;

type
  ///<summary>
  /// Определяет напровление торговли
  ///</summary>
  TTypeBot = (
    tbLong,   // бот торгует только в лог
    tbShort   // Торгуеь только в короткую позици
  );

  ///<summary>
  /// Событие
  ///</summary>
  TEventOnSendTrade = procedure(
    ASender: TObject;
    const ATime: TDateTime;
    const APrice, AQty: Double;
    ASide: TTypeBuySell
  ) of object;


  ///<summary>
  /// Механиз фиксации опериции не зависемо отплатформы
  ///</summary>
  TCustomTraginBot = class(TObject)
  private
    FTradeBox: TTradeBox;
    FManager: TJournalManager;
    procedure TradeBoxOnTradeBox(ASender: TObject; ATypeDirection: TTypeDirection; ATypeTrade: TTypeTrade);
  protected
    procedure DoTradeBox(ATypeDirection: TTypeDirection; ATypeTrade: TTypeTrade); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>
    /// Настройка работы, бокса
    ///</summary>
    procedure SetTradeBox(AOpenLong, ACloseLong, AOpenShort, ACloseShort: Double);
    ///<summary>
    /// Журнал торговых операций
    ///</summary>
    property Manager: TJournalManager read FManager;
    ///<summary>
    /// Настройка бокса
    ///</summary>
    property TradeBox: TTradeBox read FTradeBox;
  end;

  ///<summary>
  /// Бот - для торговли
  ///</summary>
  TBot = class(TCustomTraginBot)
  private
    FTradingPlatform: TTradingPlatform;
  private
    FQty: Double;
    FPeriod: Integer;
    FValueRSI: Double;
  protected
    procedure DoTradeBox(ATypeDirection: TTypeDirection; ATypeTrade: TTypeTrade); override;
    ///<summary>Совершение торговых операций</summary>
    procedure DoSendTrade(const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell);
  public
    constructor Create; override;
    destructor Destroy; override;
    ///<summary>Запрос на совершение торговых операций </summary>
    procedure SetSelected;
    ///<summary>Торговая платформа</summary>
    property TradingPlatform: TTradingPlatform read FTradingPlatform write FTradingPlatform;
    ///<summary>Период оценки определение состояние рынка</summary>
    property Period: Integer read FPeriod write FPeriod;
    ///<summary>Количество заявок</summary>
    property Qty: Double read FQty write FQty;
  end;

  ///<summary>Список ботов</summary>
  TBotList = TObjectList<TBot>;

  ///<summary>Список которой торгует виртуально</summary>
  TManagerBot = class(TObject)
  private
    FItems: TBotList;
    FTradingPlatform: TTradingPlatform;
    ///<summary>Событие нового бара</summary>
    procedure TradingPlatformOnNewCandel(Sender: TObject);
    procedure SetTradingPlatform(const Value: TTradingPlatform);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure SetSelected;
    function AddBot: TBot;
    property Items: TBotList read FItems;
    property TradingPlatform: TTradingPlatform read FTradingPlatform write SetTradingPlatform;
  end;


function GetStrToTypeBot(const ATypeBot: TTypeBot): String;

implementation

uses
{$IFDEF DEBUG}
  Lb.Logger,
{$ENDIF}
  Lb.Indiсator;

(******************************************************************************)
(* *)

function GetStrToTypeBot(const ATypeBot: TTypeBot): String;
begin
  case ATypeBot of
    tbLong: Result := 'long';
    tbShort: Result := 'short';
  else
    Result := '';
  end;
end;


{ TCustomTraginBot }

constructor TCustomTraginBot.Create;
begin
  FTradeBox := TTradeBox.Create;
  FTradeBox.OpenLong := 50;
  FTradeBox.CloseLong := 80;
  FTradeBox.OpenShort := 50;
  FTradeBox.CloseShort := 20;
  FTradeBox.OnTradeBox := TradeBoxOnTradeBox;

  FManager := TJournalManager.Create;
end;

destructor TCustomTraginBot.Destroy;
begin
  FreeAndNil(FManager);
  FreeAndNil(FTradeBox);
  inherited;
end;

procedure TCustomTraginBot.TradeBoxOnTradeBox(ASender: TObject; ATypeDirection: TTypeDirection; ATypeTrade: TTypeTrade);
begin
  DoTradeBox(ATypeDirection, ATypeTrade);
end;

procedure TCustomTraginBot.SetTradeBox(AOpenLong, ACloseLong, AOpenShort, ACloseShort: Double);
begin
  FTradeBox.OpenLong   := AOpenLong;
  FTradeBox.CloseLong  := ACloseLong;
  FTradeBox.OpenShort  := AOpenShort;
  FTradeBox.CloseShort := ACloseShort;
end;

procedure TCustomTraginBot.DoTradeBox(ATypeDirection: TTypeDirection; ATypeTrade: TTypeTrade);
begin
  // Управление данных - значение работы
end;

{ TBot }

constructor TBot.Create;
begin
  inherited;
  FQty := 1;
  FPeriod := 14;
  FTradingPlatform := nil;
end;

destructor TBot.Destroy;
begin
  inherited;
end;

procedure TBot.DoSendTrade(const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell);
begin
end;

procedure TBot.SetSelected;
begin
{$IFDEF DEBUG}
  TLogger.LogTree(0,'TBot.SetSelected: ');
{$ENDIF}
  {todo: Все вычисление вынести в отдельный модуль или объект, в целя исключение повторного вычисления}
  if not Assigned(FTradingPlatform) then
  begin
    raise Exception.Create('Error Message: Не определена платформа');
    Exit;
  end;

  if FPeriod <= 0 then
  begin
    raise Exception.Create('Error Message: Не определен период рынка');
    Exit;
  end;

  FValueRSI := FTradingPlatform.ValueRSI.RSI;
  FTradeBox.SetUpDateValue(FValueRSI);
end;

procedure TBot.DoTradeBox(ATypeDirection: TTypeDirection; ATypeTrade: TTypeTrade);

  procedure _TypeDirectionLong(ATypeTrade: TTypeTrade; AQty: Double; AAsk, ABid: Double);
  begin
  {$IFDEF DEBUG}
    TLogger.LogTreeText(3,'>> _TypeDirectionLong');
  {$ENDIF}
    case ATypeTrade of
      ttOpen: begin
        if Manager.IsCurrentPosition then
        begin
          if Manager.CurrentPosition.Side = TTypeBuySell.tsSell then
          begin
            var xCurrentDateTime := GetNewDateTime;
            {$IFDEF DEBUG}
            var xS := '>> revers' +
                ' time: ' + DateTimeToStr(xCurrentDateTime) + ' ' +
                ' price: ' + ABid.ToString;
            TLogger.LogTreeText(3,xS);
            {$ENDIF}
            Manager.ReverseTrade(
              xCurrentDateTime,
              ABid,
              FTradingPlatform.StateMarket.Candels
            );
          end;
        end
        else
        begin
          var xCurrentDateTime := GetNewDateTime;
          {$IFDEF DEBUG}
          var xS := '>> open' +
              ' time: ' + DateTimeToStr(xCurrentDateTime) + ' ' +
              ' price: ' + AAsk.ToString + ' ' +
              ' qty: ' + AQty.ToString + ' ' +
              ' side: buy';
          TLogger.LogTreeText(3,xS);
          {$ENDIF}
          Manager.OpenTrade(
            xCurrentDateTime,
            AAsk,
            AQty,
            TTypeBuySell.tsBuy,
            FTradingPlatform.StateMarket.Candels
          );
        end;
      end;
      ttClose: begin
        if Manager.IsCurrentPosition then
          if Manager.CurrentPosition.Side = TTypeBuySell.tsBuy then
          begin
            var xCurrentDateTime := GetNewDateTime;
            {$IFDEF DEBUG}
            var xS := '>> close' +
                ' time: ' + DateTimeToStr(xCurrentDateTime) + ' ' +
                ' price: ' + ABid.ToString;
            TLogger.LogTreeText(3,xS);
            {$ENDIF}
            Manager.ReverseTrade(
              xCurrentDateTime,
              ABid,
              FTradingPlatform.StateMarket.Candels
            );
            Manager.CloseTrade(
              xCurrentDateTime,
              ABid,
              FTradingPlatform.StateMarket.Candels
            );
          end;
      end;
    end;
  end;

  procedure _TypeDirectionShort(ATypeTrade: TTypeTrade; AQty: Double; AAsk, ABid: Double);
  begin
  {$IFDEF DEBUG}
  TLogger.LogTreeText(3,'>> _TypeDirectionShort');
  {$ENDIF}
    case ATypeTrade of
      ttOpen: begin
        if Manager.IsCurrentPosition then
        begin
          var xCurrentDateTime := GetNewDateTime;
          {$IFDEF DEBUG}
          var xS := '>> revers' +
              ' time: ' + DateTimeToStr(xCurrentDateTime) + ' ' +
              ' price: ' + AAsk.ToString;
          TLogger.LogTreeText(3,xS);
          {$ENDIF}
          if Manager.CurrentPosition.Side = TTypeBuySell.tsBuy then
          begin
            Manager.ReverseTrade(
              xCurrentDateTime,
              AAsk,
              FTradingPlatform.StateMarket.Candels
            );
          end;
        end
        else
        begin
          var xCurrentDateTime := GetNewDateTime;
          {$IFDEF DEBUG}
          var xS := '>> open' +
              ' time: ' + DateTimeToStr(xCurrentDateTime) + ' ' +
              ' price: ' + ABid.ToString + ' ' +
              ' qty: ' + AQty.ToString + ' ' +
              ' side: sell';
          TLogger.LogTreeText(3,xS);
          {$ENDIF}
          Manager.OpenTrade(
            GetNewDateTime,
            ABid,
            AQty,
            TTypeBuySell.tsSell,
            FTradingPlatform.StateMarket.Candels
          );
        end;
      end;
      ttClose: begin
        if Manager.IsCurrentPosition then
          if Manager.CurrentPosition.Side = TTypeBuySell.tsSell then
          begin
            var xCurrentDateTime := GetNewDateTime;
            {$IFDEF DEBUG}
            var xS := '>> close' +
                ' time: ' + DateTimeToStr(xCurrentDateTime) + ' ' +
                ' price: ' + AAsk.ToString;
            TLogger.LogTreeText(3,xS);
            {$ENDIF}
            Manager.CloseTrade(
              xCurrentDateTime,
              AAsk,
              FTradingPlatform.StateMarket.Candels
            );
          end;
      end;
    end;
  end;

var
  xAsk, xBid: Double;
begin
{$IFDEF DEBUG}
  TLogger.LogTree(0,'TBot.DoTradeBox: ' + GetStrToTypeDirection(ATypeDirection) + ' :: ' + GetStrToTypeTrade(ATypeTrade) +
    '(' + FValueRSI.ToString + ')'
  );
{$ENDIF}
  xAsk := FTradingPlatform.StateMarket.Ask;
  xBid := FTradingPlatform.StateMarket.Bid;

  case ATypeDirection of
    tdLong: _TypeDirectionLong(ATypeTrade, FQty, xAsk, xBid);
    tdShort: _TypeDirectionShort(ATypeTrade, FQty, xAsk, xBid);
  end;
end;

{ TManagerBot }

constructor TManagerBot.Create;
begin
  FItems := TBotList.Create;
end;

destructor TManagerBot.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TManagerBot.Clear;
begin
  FItems.Clear;
end;

function TManagerBot.AddBot: TBot;
var
  xBot: TBot;
begin
  xBot := TBot.Create;
  xBot.TradingPlatform := FTradingPlatform;
  Result := xBot;
  FItems.Add(xBot);
end;

procedure TManagerBot.SetSelected;
var
  i, iCount: Integer;
begin
  iCount := FItems.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
      FItems[i].SetSelected;
end;

procedure TManagerBot.SetTradingPlatform(const Value: TTradingPlatform);
begin
  FTradingPlatform := Value;
  FTradingPlatform.OnNewCandel := TradingPlatformOnNewCandel;
end;

procedure TManagerBot.TradingPlatformOnNewCandel(Sender: TObject);
begin
  Self.SetSelected;
end;

end.
