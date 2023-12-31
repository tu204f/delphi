unit Lb.Bot.Tiket;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils.Candel;

//{$IFDEF DEBUG}
//  {$DEFINE DEB_POSTION}
//  {$DEFINE DEB_TB}     // Открываем не зависмое не отчего
//  {$DEFINE DEB_SL_TB}  // Фиксирваный столосс
//  {$DEFINE DEB_SL_TR}  // Следящий стоп лосс
//{$ENDIF}


type
  ///<summary>Целях отслеживание общий позиции</summary>
  TPosition = class(TObject)
  public const
    TRD_OPEN  = 1;
    TRD_CLOSE = 2;
  public type
    TTrade = record
      Time: TDateTime;
      Price: Double;
      Quantity: Double;
      BuySell: Char;
      Status: Integer;
    private
      function GetVol: Double;
    public
      constructor Create(
        ATime: TDateTime;  // Время локальная опеарция
        APrice,            // Цена
        AQuantity: Double; // Размер позиции
        ABuySell: Char;    // Напровление объекта
        AStatus: Integer   // Статус
      );
      property Vol: Double read GetVol;
    end;
    TTradeList = TList<TTrade>;
  private
    FCurrentTrade: TTrade;
    FTrades: TTradeList;
    function GetBuySell: Char;
    function GetPrice: Double;
    function GetQuantity: Double;
    function GetProfit: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(APrice, AQuantity: Double; ABuySell: Char; AStatus: Integer);
    property Trades: TTradeList read FTrades;
    property Profit: Double read GetProfit;
  public  {общие состояние}
    property Price: Double read GetPrice;
    property Quantity: Double read GetQuantity;
    property BuySell: Char read GetBuySell;
  end;

type
  TMode = (tmNull = 0, tmBuy, tmSell);

  ///<summary>Тиковый бот</summary>
  TTiketBot = class(TObject)
  private
    FOpenPrice: Double;
    FIsPosition: Boolean;
    FMode: TMode;
    FHealthPoints: Double;
    FQuantity: Double;
  private
    FPosition: TPosition;
  protected
    FOnOpenPosition: TNotifyEvent;
    FOnClosePosition: TNotifyEvent;
    procedure DoOpenPosition;
    procedure DoClosePosition;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>Восстановление состояние по умолчанию</summary>
    procedure DefaultState;
    ///<summary>Принять решение о направление</summary>
    procedure SetMode; virtual;

    procedure SetOpenPosition(const ALast: Double); virtual;
    procedure SetUpPosition(const ALast: Double); virtual;
    procedure SetClosePosition(const ALast: Double); virtual;

    property IsPosition: Boolean read FIsPosition;
    property OpenPrice: Double read FOpenPrice;
    property Mode: TMode read FMode;
    property HealthPoints: Double read FHealthPoints;

    property Position: TPosition read FPosition;
  public
    property Quantity: Double read FQuantity write FQuantity;

    property OnOpenPosition: TNotifyEvent write FOnOpenPosition;
    property OnClosePosition: TNotifyEvent write FOnClosePosition;
  end;

  ///<summary>С фиксированным убытком</summary>
  TStopLostTiketBot = class(TTiketBot)
  private
    FStopLoss: Double;
    FCountStop: Integer;
    FTmpCountStop: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetMode; override;
    procedure SetOpenPosition(const ALast: Double); override;
    procedure SetUpPosition(const ALast: Double); override;
    ///<summary>Установка убытка, размер возможно убытка</summary>
    property StopLoss: Double read FStopLoss write FStopLoss;
    property CountStop: Integer read FCountStop write FCountStop;
  end;

  ///<summary>Изменяемым значением стопа</summary>
  TTrelingStopTiketBot = class(TStopLostTiketBot)
  private
    FFixProfit: Double;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetOpenPosition(const ALast: Double); override;
    procedure SetUpPosition(const ALast: Double); override;
    ///<summary>Размер фиксирующего стоплоса</summary>
    property FixProfit: Double read FFixProfit;
  end;

  ///<summary>Бот с учетом работы </summary>
  TTakeProfitTiketBot = class(TStopLostTiketBot)
  private
    FTakeProfit: Double;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetUpPosition(const ALast: Double); override;
    property TakeProfit: Double read FTakeProfit write FTakeProfit;
  end;

implementation

{$IFDEF DEBUG}
uses
  Lb.Logger;
{$ENDIF}

function GetRandomMode: TMode;
var
  xMode: Integer;
begin
  // Здесь модуль принимает решение
  xMode := Random(3);
  Result := TMode(xMode);
end;

function GetStatusToStr(const AStatus: Integer): String;
begin
  case AStatus of
    TPosition.TRD_OPEN: Result := 'open';
    TPosition.TRD_CLOSE: Result := 'close';
  else
    Result := 'null';
  end;
end;

{ TPosition.TTrade }

constructor TPosition.TTrade.Create(ATime: TDateTime; APrice, AQuantity: Double; ABuySell: Char; AStatus: Integer);
begin
  Time     := ATime;
  Price    := APrice;
  Quantity := AQuantity;
  BuySell  := ABuySell;
  Status   := AStatus;
  {$IFDEF DEB_POSTION}
  TLogger.LogTree(0,'TPosition.TTrade.Create()');
  TLogger.LogTreeText(3,'>> Time: ' + FormatDateTime('hh:mm:ss.zzz',Time));
  TLogger.LogTreeText(3,'>> Price: ' + FloatToStr(Price));
  TLogger.LogTreeText(3,'>> Quantity: ' + FloatToStr(Quantity));
  TLogger.LogTreeText(3,'>> BuySell: ' + BuySell);
  TLogger.LogTreeText(3,'>> Status: ' + GetStatusToStr(AStatus));
  {$ENDIF}
end;

function TPosition.TTrade.GetVol: Double;
begin
  Result := Price * Quantity;
end;

{ TPosition }

constructor TPosition.Create;
begin
  {$IFDEF DEB_POSTION}
  TLogger.LogTree(0,'TPosition.Create');
  {$ENDIF}
  FTrades := TTradeList.Create;
end;

destructor TPosition.Destroy;
begin
  FreeAndNil(FTrades);
  {$IFDEF DEB_POSTION}
  TLogger.LogTree(0,'TPosition.Destroy');
  {$ENDIF}
  inherited;
end;

procedure TPosition.Clear;
begin
  {$IFDEF DEB_POSTION}
  TLogger.LogTree(0,'TPosition.Clear');
  {$ENDIF}
  FCurrentTrade := TTrade.Create(0,0,0,#0,0);
  FTrades.Clear;
end;

function TPosition.GetPrice: Double;
begin
  Result := FCurrentTrade.Price;
end;

function TPosition.GetProfit: Double;
var
  xTrade, xOpenTrade: TTrade;
  i, iCount: Integer;
  xProfit: Double;
begin
  xProfit := 0;
  iCount := FTrades.Count;
  if iCount > 0 then
  begin
    xProfit := 0;
    for i := 0 to iCount - 1 do
    begin
      xTrade := FTrades[i];
      case xTrade.Status of
        TPosition.TRD_OPEN: xOpenTrade := xTrade;
        TPosition.TRD_CLOSE: begin
          case xOpenTrade.BuySell of
            'B': xProfit := xProfit + (xTrade.Vol - xOpenTrade.Vol);
            'S': xProfit := xProfit + (xOpenTrade.Vol - xTrade.Vol);
          end;
        end;
      end;
    end;
  end;
  Result := xProfit;
end;

function TPosition.GetQuantity: Double;
begin
  Result := FCurrentTrade.Quantity;
end;

function TPosition.GetBuySell: Char;
begin
  Result := FCurrentTrade.BuySell;
end;

procedure TPosition.Add(APrice, AQuantity: Double; ABuySell: Char; AStatus: Integer);
begin
  {todo: Не учитывается усреднение позиции}
  {$IFDEF DEB_POSTION}
  TLogger.LogTree(0,'TPosition.Add');
  {$ENDIF}
  FCurrentTrade := TTrade.Create(Time,APrice,AQuantity,ABuySell,AStatus);
  FTrades.Add(FCurrentTrade);
end;

{ TTiketBot }

constructor TTiketBot.Create;
begin
  DefaultState;
  FPosition := TPosition.Create;
end;

destructor TTiketBot.Destroy;
begin
  FreeAndNil(FPosition);
  inherited;
end;

procedure TTiketBot.DoOpenPosition;
begin
  if Assigned(FOnOpenPosition) then
    FOnOpenPosition(Self);
end;

procedure TTiketBot.DoClosePosition;
begin
  if Assigned(FOnClosePosition) then
    FOnClosePosition(Self);
  DefaultState;
end;

procedure TTiketBot.DefaultState;
begin
  FOpenPrice := 0;
  FIsPosition := False;
  FMode := tmNull;
end;

procedure TTiketBot.SetMode;
begin
  {todo: организовать стретегию входа}
  FMode := GetRandomMode;
end;

procedure TTiketBot.SetOpenPosition(const ALast: Double);
begin
  {$IFDEF DEB_TB}
  TLogger.LogTree(0,'TTiketBot.SetOpenPosition');
  TLogger.LogTreeText(3,'>> Last ' + ALast.ToString);
  {$ENDIF}
  // открываем позицию
  if not FIsPosition then
  begin
    case FMode of
      tmBuy: begin
        FOpenPrice := ALast;
        FIsPosition := True;
        FPosition.Add(FOpenPrice,FQuantity,'B',TPosition.TRD_OPEN);
      end;
      tmSell: begin
        FOpenPrice := ALast;
        FIsPosition := True;
        FPosition.Add(FOpenPrice,FQuantity,'S',TPosition.TRD_OPEN);
      end
    else
      {$IFDEF DEB_TB}
      TLogger.LogTreeText(3,'>> Открываем позицию[Писец ошибки: Так не должно быть]');
      {$ENDIF}
    end;
    DoOpenPosition;
  end;
end;

procedure TTiketBot.SetUpPosition(const ALast: Double);
begin
  // Обновляем условия позиции
end;

procedure TTiketBot.SetClosePosition(const ALast: Double);
var
  xHP: Double;
begin
  {$IFDEF DEB_TB}
  TLogger.LogTree(0,'TTiketBot.SetClosePosition');
  if FMode = tmNull then
    TLogger.LogTreeText(3,'>> Игнорируем операцию')
  else
    TLogger.LogTreeText(3,'>> ALast: ' + ALast.ToString);
  {$ENDIF}
  if FOpenPrice <= 0 then
    raise Exception.Create('Error Message: Критическая ошибка');
  // ------------------------------------------------------------
  // Принудительное закрытие
  xHP := 0;
  if FIsPosition then
  begin
    {$IFDEF DEB_TB}
    TLogger.LogTreeText(3,'>> Закрываем позицию');
    {$ENDIF}
    // Закрываем позицию
    case FMode of
      tmBuy: begin
        xHP := ALast - FOpenPrice;
        FPosition.Add(ALast,FQuantity,'S',TPosition.TRD_CLOSE);
      end;
      tmSell: begin
        xHP := FOpenPrice - ALast;
        FPosition.Add(ALast,FQuantity,'B',TPosition.TRD_CLOSE);
      end;
    end;
    FHealthPoints := FHealthPoints + xHP * FQuantity;
    {$IFDEF DEB_TB}
    TLogger.LogTreeText(3,'>> FHealthPoints: ' + FHealthPoints.ToString);
    {$ENDIF}
    FIsPosition := False;
    DoClosePosition;
  end;
  FOpenPrice := 0;
end;


{ TStopLostTiketBot }

constructor TStopLostTiketBot.Create;
begin
  inherited;
  FStopLoss := -20;
  FTmpCountStop := 0;
end;

destructor TStopLostTiketBot.Destroy;
begin

  inherited;
end;

procedure TStopLostTiketBot.SetMode;
begin
  {$IFDEF DEB_SL_TB}
  TLogger.LogTree(0,'TStopLostTiketBot.SetMode: Принятие решение');
  TLogger.LogTreeText(3,'>> TmpCountStop: ' + FTmpCountStop.ToString);
  {$ENDIF}
  FMode := tmNull;
  if FTmpCountStop > 0 then
    Dec(FTmpCountStop)
  else
    inherited SetMode;
  {$IFDEF DEB_SL_TB}
  case FMode of
    tmNull: TLogger.LogTreeText(3,'>> Null');
    tmBuy: TLogger.LogTreeText(3,'>> Buy');
    tmSell: TLogger.LogTreeText(3,'>> Sell');
  end;
  {$ENDIF}
end;

procedure TStopLostTiketBot.SetOpenPosition(const ALast: Double);
begin
  inherited SetOpenPosition(ALast);
  {$IFDEF DEB_SL_TB}
  TLogger.LogTree(0,'TStopLostTiketBot.SetOpenPosition: Процедура открытие позиции');
  if FMode = tmNull then
    TLogger.LogTreeText(3,'>> Игнорируем операцию')
  else
    TLogger.LogTreeText(3,'>> ALast: ' + ALast.ToString);
  {$ENDIF}
  if FIsPosition then
    FTmpCountStop := 0;
end;

procedure TStopLostTiketBot.SetUpPosition(const ALast: Double);
var
  xProfit: Double;
begin
  // При достижение определеного уровня убытка
  // Закрываем позицию
  xProfit := 0;
  if FIsPosition then
  begin
    {$IFDEF DEB_SL_TB}
    TLogger.LogTree(0,'TStopLostTiketBot.SetUpPosition: Проверяем возможность закрытие позиции');
    if FMode = tmNull then
      TLogger.LogTreeText(3,'>> Игнорируем операцию')
    else
      TLogger.LogTreeText(3,'>> ALast: ' + ALast.ToString);
    {$ENDIF}
    case FMode of
      tmBuy : xProfit := ALast - FOpenPrice;
      tmSell: xProfit := FOpenPrice - ALast;
    end;
    {$IFDEF DEB_SL_TB}
    TLogger.LogTreeText(3,'>> Profit: ' + xProfit.ToString);
    {$ENDIF}
    if xProfit <= FStopLoss then
    begin
      {$IFDEF DEB_SL_TB}
      TLogger.LogText('#',30);
      TLogger.LogTreeText(3,'>> StopLoss: ' + FStopLoss.ToString);
      {$ENDIF}
      FTmpCountStop := FCountStop;
      SetClosePosition(ALast)
    end;

  end;
end;

{ TTrelingStopTiketBot }

constructor TTrelingStopTiketBot.Create;
begin
  inherited;
end;

destructor TTrelingStopTiketBot.Destroy;
begin
  inherited;
end;

procedure TTrelingStopTiketBot.SetOpenPosition(const ALast: Double);
begin
  inherited SetOpenPosition(ALast);
  {$IFDEF DEB_SL_TR}
  TLogger.LogTree(0,'TTrelingStopTiketBot.SetOpenPosition: Процедура открытие позиции');
  if FMode = tmNull then
    TLogger.LogTreeText(3,'>> Игнорируем операцию')
  else
    TLogger.LogTreeText(3,'>> ALast: ' + ALast.ToString);
  {$ENDIF}
  if FIsPosition then
  begin
    FFixProfit := FStopLoss;
    {$IFDEF DEB_SL_TR}
    TLogger.LogTreeText(3,'>> FixProfit: ' + FFixProfit.ToString);
    {$ENDIF}
  end;
end;

procedure TTrelingStopTiketBot.SetUpPosition(const ALast: Double);
var
  xProfit: Double;
  xFixProfit: Double;
begin
  {$IFDEF DEB_SL_TR}
  TLogger.LogTree(0,'TTrelingStopTiketBot.SetUpPosition:');
  if FMode = tmNull then
    TLogger.LogTreeText(3,'>> Игнорируем операцию')
  else
    TLogger.LogTreeText(3,'>> ALast: ' + ALast.ToString);
  {$ENDIF}

  xProfit := 0;
  if FIsPosition then
  begin
    case FMode of
      tmBuy : xProfit := ALast - FOpenPrice;
      tmSell: xProfit := FOpenPrice - ALast;
    end;
    {$IFDEF DEB_SL_TB}
    TLogger.LogTreeText(3,'>> Profit: ' + xProfit.ToString);
    {$ENDIF}
    xFixProfit := xProfit + FStopLoss;
    if xFixProfit > FFixProfit then
      FFixProfit := xFixProfit;
    {$IFDEF DEB_SL_TB}
    TLogger.LogTreeText(3,'>> FixProfit: ' + FFixProfit.ToString);
    {$ENDIF}
    if xProfit < FFixProfit then
    begin
      if FFixProfit < 0 then
        FTmpCountStop := FCountStop;
      SetClosePosition(ALast);
    end;
  end;
end;

{ TTakeProfitTiketBot }

constructor TTakeProfitTiketBot.Create;
begin
  inherited;

end;

destructor TTakeProfitTiketBot.Destroy;
begin

  inherited;
end;

procedure TTakeProfitTiketBot.SetUpPosition(const ALast: Double);
var
  xProfit: Double;
begin
  xProfit := 0;
  if FIsPosition then
  begin
    case FMode of
      tmBuy : xProfit := ALast - FOpenPrice;
      tmSell: xProfit := FOpenPrice - ALast;
    end;

    if xProfit > FTakeProfit then
    begin
      SetClosePosition(ALast)
    end
    else if xProfit < FStopLoss then
    begin
      FTmpCountStop := FCountStop;
      SetClosePosition(ALast)
    end;

  end;
end;




end.
