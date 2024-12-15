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
  Lb.Platform,
  Lb.Category,
  Lb.Buffer.Trading;

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
    FBufferTrading: TBufferTrading;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>
    /// Торгуем по стратегии
    ///</summary>
    property Trading: TBufferTrading read FBufferTrading;
  end;

  ///<summary>
  /// Бот - для торговли
  ///</summary>
  TBot = class(TCustomTraginBot)
  private
    FTradingPlatform: TTradingPlatform;
    FTradeBox: TTradeBox;
    procedure TradeBoxOnTradeBox(ASender: TObject; ATypeDirection: TTypeDirection; ATypeTrade: TTypeTrade);
  private
    FValueRSI: Double;
    FTypeBot: TTypeBot;
  protected
    FOnSendTrade: TEventOnSendTrade;
    ///<summary>
    /// Проверка возможности совершение торговых операций
    ///</summary>
    function IsTrading: Boolean;
    ///<summary>
    /// Есть активня позиция
    ///</summary>
    function IsActivePosition: Boolean;
    ///<summary>
    /// Отправляем торговый приказ
    ///</summary>
    procedure DoSendTrade(const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell);
  public
    constructor Create; override;
    destructor Destroy; override;
    ///<summary>
    /// Запрос — торговой ситуации
    ///</summary>
    procedure SetSelected; overload;
    procedure SetSelected(const AValueRSI: Double); overload;
    ///<summary>
    /// На какой платформе работает — бот программы
    ///</summary>
    property TradingPlatform: TTradingPlatform read FTradingPlatform write FTradingPlatform;
    property TradeBox: TTradeBox read FTradeBox write FTradeBox;
    property OnSendTrade: TEventOnSendTrade write FOnSendTrade;
  end;

  ///<summary>Список ботов</summary>
  TBotList = TObjectList<TBot>;

  ///<summary>Список которой торгует виртуально</summary>
  TManagerBot = class(TObject)
  private
    FItems: TBotList;
    FTradingPlatform: TTradingPlatform;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure SetSelected;
    function AddBot: TBot;
    property Items: TBotList read FItems;
    property TradingPlatform: TTradingPlatform read FTradingPlatform write FTradingPlatform;
  end;

function GetSMA(const AValue: TDoubleList): Double;
function GetATR(const APeriod: Integer; ACandels: TCandelList): Double;
function GetRSI(const APeriod: Integer; ACandels: TCandelList): Double;
function GetStrToTypeBot(const ATypeBot: TTypeBot): String;

implementation

uses
  Lb.Logger;

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


(******************************************************************************)
(* Процедуры которые помогают оценить состояние рынка                         *)

function GetSMA(const AValue: TDoubleList): Double;
var
  xSum: Double;
  i, iCount: Integer;
begin
  Result := 0;
  iCount := AValue.Count;
  if iCount > 0 then
  begin
    xSum := 0;
    for i := 0 to iCount - 1 do
      xSum := xSum + AValue[i];
    Result := xSum/iCount;
  end;
end;

///<summary>Определяем валатиность рынка</summary>
function GetATR(const APeriod: Integer; ACandels: TCandelList): Double;

  function _MAX(const AValue1, AValue2, AValue3: Double): Double;
  var
    xValue: Double;
  begin
    xValue := AValue1;
    if xValue < AValue2 then
      xValue := AValue2;
    if xValue < AValue3 then
      xValue := AValue3;
    Result := xValue;
  end;

var
  xDelta: Double;
  xCandel1, xCandel2: TCandel;
  xTR: TDoubleList;
begin
  Result := 0;
  if APeriod > ACandels.Count then
    Exit;

  if APeriod > 0 then
  begin
    xTR := TDoubleList.Create;
    try
      for var i := 0 to APeriod - 1 do
      begin
        if i > 0 then
        begin
          xCandel1 := ACandels[i - 1];
          xCandel2 := ACandels[i];
          xDelta := _MAX(
            xCandel2.High - xCandel2.Low,
            xCandel2.High - xCandel1.Close,
            xCandel1.Close - xCandel2.Low
          );
          xTR.Add(xDelta);
        end
        else
        begin
          xCandel1 := ACandels[i];
          xDelta := xCandel1.High - xCandel1.Low;
          xTR.Add(xDelta);
        end;
      end;
      Result := GetSMA(xTR);
    finally
      FreeAndNil(xTR);
    end;
  end;
end;

///<summary>Расчет индикатора</summary>
function GetRSI(const APeriod: Integer; ACandels: TCandelList): Double;
var
  xDelta: Double;
  xCandel1, xCandel2: TCandel;
  xU, xD: TDoubleList;
  xMaU, xMaD, xRS: Double;
begin
  Result := 0;
  if APeriod > ACandels.Count then
    Exit;

  if APeriod > 0 then
  begin
    xU := TDoubleList.Create;
    xD := TDoubleList.Create;
    try
      xU.Add(0);
      xD.Add(0);
      for var i := 1 to APeriod - 1 do
      begin
        xCandel1 := ACandels[i - 1];
        xCandel2 := ACandels[i];
        xDelta := xCandel1.Close - xCandel2.Close;
        if xDelta > 0 then
        begin
          xU.Add(xDelta);
          xD.Add(0);
        end
        else
        begin
          xU.Add(0);
          xD.Add(Abs(xDelta));
        end;
      end;

      xMaU := GetSMA(xU);
      xMaD := GetSMA(xD);
      xRS  := xMaU/xMaD;
      Result := 100 - 100/(1 + xRS);
    finally
      FreeAndNil(xD);
      FreeAndNil(xU);
    end;
  end;
end;

(******************************************************************************)

{ TCustomTraginBot }

constructor TCustomTraginBot.Create;
begin
  FBufferTrading := TBufferTrading.Create;
end;

destructor TCustomTraginBot.Destroy;
begin
  FreeAndNil(FBufferTrading);
  inherited;
end;


{ TBot }

constructor TBot.Create;
begin
  inherited;
  FTradingPlatform := nil;

  // Прямой объект
  FTypeBot := TTypeBot.tbLong;

  FTradeBox := TTradeBox.Create;
  FTradeBox.OpenLong := 50;
  FTradeBox.CloseLong := 80;
  FTradeBox.OpenShort := 50;
  FTradeBox.CloseShort := 20;
  FTradeBox.OnTradeBox := TradeBoxOnTradeBox;
end;

destructor TBot.Destroy;
begin
  FreeAndNil(FTradeBox);
  inherited;
end;

function TBot.IsActivePosition: Boolean;
begin
  Result := False;
  if Assigned(FTradingPlatform) then
    Result := Trading.IsPosition;
end;

function TBot.IsTrading: Boolean;
begin
  // 1. Временной интервал, который можно торговать
  // 2. Размер полученного убытка
  // 3. Количество торговых операция — за торговый период
  // 4. Период заморозки торговли после получение убытка
  Result := True;
end;

procedure TBot.DoSendTrade(const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell);

  procedure _IsCrossStrage(const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell);
  var
    xSide: TTypeBuySell;
  begin
    {todo: Передается один формат структуры}
    {todo: Нужно переделать с ожиданием ответа торговой платформы}
    xSide := ASide;
//
//    // Отправляем в торгую платформу
//    FTradingPlatform.SendTrade(
//      ATime,
//      APrice,
//      AQty,
//      GetCrossSide(xSide)
//    );

    // Часть системы расчета прибыли
    Trading.OpenTrade(
      ATime,
      APrice,
      AQty,
      xSide
    );
  end;

begin
  // Открытие позиции
  if Assigned(FTradingPlatform) then
  begin

    _IsCrossStrage(
      ATime,
      APrice,
      AQty,
      ASide
    );

    // Бот сообщает что он совершил торговую операцию
    if Assigned(FOnSendTrade) then
      FOnSendTrade(
        Self,
        ATime,
        APrice,
        AQty,
        ASide
      );

  end;
end;

procedure TBot.SetSelected;
begin
  {todo: Все вычисление вынести в отдельный модуль или объект, в целя исключение повторного вычисления}
  if not Assigned(FTradingPlatform) then
    Exit;

  FValueRSI := FTradingPlatform.ValueRSI;
  FTradeBox.SetUpDateValue(FValueRSI);
end;

procedure TBot.SetSelected(const AValueRSI: Double);
begin
  FValueRSI := AValueRSI;
  FTradeBox.SetUpDateValue(FValueRSI);
end;

procedure TBot.TradeBoxOnTradeBox(ASender: TObject; ATypeDirection: TTypeDirection; ATypeTrade: TTypeTrade);

  function _GetNew: TDateTime;
  begin
    Result := Date + Time;
  end;

begin
  {$IFDEF DEBUG}
  TLogger.LogText;
  TLogger.LogTree(0,'TBot.TradeBoxOnTradeBox:');
  TLogger.LogTreeText(3,'>> Direction: ' + GetStrToTypeDirection(ATypeDirection));
  TLogger.LogTreeText(3,'>> ATypeTrade: ' + GetStrToTypeTrade(ATypeTrade));
  TLogger.LogTreeText(3,'>> ValueRSI: ' + FValueRSI.ToString);
  {$ENDIF}
  if Trading.IsPosition then
  begin
    // Позиция отрыта
    // Можно только закрыть или перевернуть
    case ATypeTrade of

      ttOpen: begin
        case ATypeDirection of
          tdLong: begin
            Trading.ReverseTrade(
              _GetNew,
              TradingPlatform.StateMarket.Ask
            )
          end;
          tdShort: begin
            Trading.ReverseTrade(
              _GetNew,
              TradingPlatform.StateMarket.Bid
            )
          end;
        end;
      end;

      ttClose: begin
        case ATypeDirection of
          tdLong: begin
            Trading.CloseTrade(
              _GetNew,
              TradingPlatform.StateMarket.Ask
            )
          end;
          tdShort: begin
            Trading.CloseTrade(
              _GetNew,
              TradingPlatform.StateMarket.Bid
            )
          end;
        end;
      end;

    end;
  end
  else
  begin
    // Можно только открыть
    if ATypeTrade = TTypeTrade.ttOpen then
    begin
      case ATypeDirection of
        tdLong: DoSendTrade(
          _GetNew,
          TradingPlatform.StateMarket.Ask,
          0.01,
          TTypeBuySell.tsBuy
        );
        tdShort:DoSendTrade(
          _GetNew,
          TradingPlatform.StateMarket.Bid,
          0.01,
          TTypeBuySell.tsSell
        );
      end;
    end;
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

end.
