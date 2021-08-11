unit QuikTransOrder;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections;

(******************************************************************************)
const
  THEAD_SLEEP = 3;     //  Задержка для отклика

type { не где не используется }
  TCustomThread = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

  ///	<summary>
  ///	  Объект прокладка для событий транзакций - SetTransactionReplyCallback()
  ///	</summary>
  TTransactionThread = class(TCustomThread)
    TransID: Cardinal; //  Номер транзакции
    OrderNo: Int64;    //  Номер заявки
    MsgResult: String; //  Сообщение заявки
  end;

  ///	<summary>
  ///	  Объект прокладка для события о прошедших заявок -
  ///	  SetOrdersStatusCallback()
  ///	</summary>
  TOrderThread = class(TCustomThread)
    TransID: Cardinal; //  Номер транзакции
    OrderNo: Int64;    //  Номер заявку
    ClassCode: String; //  Класс инструмента
    SecCode: String;   //  Код инструмента
    BuySell: String;   //  Напровление позиции
    Price: Double;     //  Цена
    Balance: Integer;  //  Остаток
    Quantity: Integer; //  Количество
    Status: Integer;   //  Статус операции
  end;

  ///	<summary>
  ///	  Объект прокладка для событие о прошедших сделок -
  ///	  SetTradesStatusCallback()
  ///	</summary>
  TTradeThread = class(TCustomThread)
    TradeNo: Int64;    //  Номер сделки
    OrderNo: Int64;    //  Номер заявки
    ClassCode: String; //  Класс инстурмента
    SecCode: String;   //  Код инстурмента
    BuySell: String;   //  Напровление позиции
    Price: Double;     //  Цена позиции
    Quantity: Integer; //  Количество
  end;

(******************************************************************************)
type { отрабатываем сообщение от торгового терминала }
  TOnEventResultMessage = procedure(Sender: TObject;
      const pResultMessage: String) of object;
  TOnEventTransactions = procedure(Sender: TObject; const ATransID: LongWord;
      const AOrderNo: Int64; const AMsgResult: String) of object;
  TOnEventOrders = procedure(Sender: TObject; const ATransID: LongWord;
      const AOrderNo: Int64; const AClassCode, ASecCode, ABuySell: String;
      const APrice: Double; const ABalance, AQuantity,
      AStatus: LongInt) of object;
  TOnEventTrades = procedure(Sender: TObject; const ATradeNo, AOrderNo: Int64;
      const AClassCode, ASecCode, ABuySell: String; const APrice: Double;
      const AQuantity: LongInt) of object;
  TOnEventTransID = procedure(Sender: TObject; const ATransID: LongWord) of object;

type
  TTransQuik = class;

  ///	<summary>
  ///	  Объект управление событиями заявками
  ///	</summary>
  TEventsOrder = class(TObject)
  private
    FOnEventTransactions: TOnEventTransactions;
    FOnEventOrders: TOnEventOrders;
    FOnEventTrades: TOnEventTrades;
    FOnNewOrder: TOnEventTransID;
    FOnMoveOrder: TOnEventTransID;
    FOnDeleteOrder: TOnEventTransID;
  protected
    procedure DoEventTransReplyCallback(const ATransID: LongWord;
      const AOrderNo: Int64; const AMsgResult: String); virtual;
    procedure DoEventOrdersCallback(const ATransID: LongWord;
      const AOrderNo: Int64; const AClassCode, ASecCode, ABuySell: String;
      const APrice: Double; const ABalance, AQuantity, AStatus: LongInt); virtual;
    procedure DoEventTradesCallback(const ATradeNo, AOrderNo: Int64;
      const AClassCode, ASecCode, ABuySell: String; const APrice: Double;
      const AQuantity: LongInt); virtual;
    procedure DoNewOrder(const ATransID: LongWord);
    procedure DoMoveOrder(const ATransID: LongWord);
    procedure DoDeleteOrder(const ATransID: LongWord);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    /// <summary>
    /// Событие о проведённой транзакции
    /// </summary>
    property OnEventTransactions: TOnEventTransactions write FOnEventTransactions;
    /// <summary>
    /// Событие регистрации заявки
    /// </summary>
    property OnEventOrders: TOnEventOrders write FOnEventOrders;
    /// <summary>
    /// Событие регистрации исполнение сделки
    /// </summary>
    property OnEventTrades: TOnEventTrades write FOnEventTrades;
    /// <summary>
    /// Событие регистрации новой заявки
    /// </summary>
    property OnNewOrder: TOnEventTransID write FOnNewOrder;
    /// <summary>
    /// Событие регистрации обновление условие заявки
    /// </summary>
    property OnMoveOrder: TOnEventTransID write FOnMoveOrder;
    /// <summary>
    /// Событие регистрации удаление заявки
    /// </summary>
    property OnDeleteOrder: TOnEventTransID write FOnDeleteOrder;
  end;

  ///	<summary>
  ///	  Базовый объект для работы стратегии и заявку
  ///	</summary>
  TCustomOrder = class(TEventsOrder)
  private
    FTrdaccID: String;  //  Торговый счет операций
    FClassCode: String; //  Класс инстрмента
    FSecCode: String;   //  Код инструмента
  private
    FIndex: Integer;    //  Индекс в массиве заявок
  public
    constructor Create; override;
    destructor Destroy; override;
  (****************************************************************************)
    /// <summary>
    /// Обратная сообщение о транзакции
    /// </summary>
    procedure SetTransReplyCallback(const ATransID: LongWord;
      const AOrderNo: Int64; const AMsgResult: String); virtual;
    /// <summary>
    /// Обратное сообщение о заявках
    /// </summary>
    procedure SetOrdersCallback(const ATransID: LongWord; const AOrderNo: Int64;
      const AClassCode, ASecCode, ABuySell: String; const APrice: Double;
      const ABalance, AQuantity, AStatus: LongInt); virtual;
    /// <summary>
    /// Обратное сообщение о сделках
    /// </summary>
    procedure SetTradesCallback(const ATradeNo, AOrderNo: Int64;
      const AClassCode, ASecCode, ABuySell: String; const APrice: Double;
      const AQuantity: LongInt); virtual;
  (****************************************************************************)
    property TrdaccID: String read FTrdaccID write FTrdaccID;
    property ClassCode: String read FClassCode write FClassCode;
    property SecCode: String read FSecCode write FSecCode;
  (****************************************************************************)
  public  {снять все заяки}
    /// <summary>
    /// Снять все заяки
    /// </summary>
    function GetKillAllOrders(var pMessageResult: String): Int64;
    /// <summary>
    /// Снять все стоп заяки
    /// </summary>
    function GetKillAllStopOrders(var pMessageResult: String): Int64;
    /// <summary>
    /// Снять все заявки на ФОРТС
    /// </summary>
    function GetKillAllFuturesOrders(var pMessageResult: String): Int64;
  end;
  TCustomOrders = TObjectList<TCustomOrder>;


  ///	<summary>
  ///	  Реализуем работу Quik.Trans2QuikAPI
  ///	</summary>
  TTransQuik = class(TObject)
  private
    // Массив объекты за отвечающие за испольнение заявок
    FOrders: TCustomOrders;
    FOnResultMessage: TOnEventResultMessage;
  (****************************************************************************)
    FAutoIncTransID: LongWord;  //  Ведем упровление транзакции
    FActiveConnect: Boolean;    //  Опеределяем состояние соединение
  (****************************************************************************)
    FConnectSUBSCRIBE: Boolean;
    FConnectionParams: String;
    function GetTransactionsReplyCallback: Boolean;
    procedure SetResultMessage(const pResultMessage: String);
    procedure SetConnectSUBSCRIBE;
    procedure SetDisConnectSUBSCRIBE;
  (****************************************************************************)
    procedure SetConnectOrdersSUBSCRIBE;
    procedure SetConnectTradesSUBSCRIBE;
  (****************************************************************************)
  public
    constructor Create;
    destructor Destroy; override;
    function Connect(const AConnectionParams: String): Boolean;
    procedure DisConnect;
    procedure SetClearAutoIncTransID;
    function GetAutoIncTransID: LongWord;
  (****************************************************************************)
    procedure Clear;
    function GetAdd(const ACustomOrder: TCustomOrder): Integer;
    procedure SetDeleteOrder(const AIndex: Integer);
  (****************************************************************************)
    { Сбытие от торгового терминала }
    procedure SetTransReplyCallback(const ATransID: LongWord;
      const AOrderNo: Int64; const AMsgResult: String);
    procedure SetOrdersCallback(const ATransID: LongWord; const AOrderNo: Int64;
      const AClassCode, ASecCode, ABuySell: String; const APrice: Double;
      const ABalance, AQuantity, AStatus: LongInt);
    procedure SetTradesCallback(const ATradeNo, AOrderNo: Int64;
      const AClassCode, ASecCode, ABuySell: String; const APrice: Double;
      const AQuantity: LongInt);
  (****************************************************************************)
    { Потоковые событие }
    procedure TransEventThread(Sender: TObject);
    procedure OrderEventThread(Sender: TObject);
    procedure TradeEventThread(Sender: TObject);
  (****************************************************************************)
    function GetSendSyncTransaction(const ASend: String;
      var pMessageResult: String): Int64;
    function GetSendAsyncTransaction(ASend: String): Boolean;
  (****************************************************************************)
  { Синхроные операции }
    function GetNewOrderSyncQuik(const APrice: Double; const AQuantity: Integer;
      const ATransID: LongWord; const ABuySell, ATrdaccID, AClassCode,
      ASecCode: String; var pMessageResult: String): Int64;
    function GetMoveOrderSyncQuik(const APrice: Double;
      const AQuantity: Integer; const ATransID: LongWord; const AOrderNo: Int64;
      const AClassCode, ASecCode: String; var pMessageResult: String): Int64;
    function GetDeleteOrderSyncQuik(const AOrderNo: Int64;
      const ATransID: LongWord; const AClassCode, ASecCode: String;
      var pMessageResult: String): Int64;
  (****************************************************************************)
  { Асинхроные операции }
    function GetNewOrderAsyncQuik(const APrice: Double;
      const AQuantity: Integer; const ATransID: LongWord; const ABuySell,
      ATrdaccID, AClassCode, ASecCode: String): Boolean;
    function GetMoveOrderAsyncQuik(const APrice: Double;
      const AQuantity: Integer; const ATransID: LongWord; const AOrderNo: Int64;
      const AClassCode, ASecCode: String): Boolean;
    function GetDeleteOrderAsyncQuik(const AOrderNo: Int64;
      const ATransID: LongWord; const AClassCode, ASecCode: String): Boolean;
  (****************************************************************************)
  { Стоп - заявки }
    /// <summary>
    ///  Стоп лимит
    /// </summary>
    function GetNewStopLimitOrder(const APrice, AStopPrice: Double;
      const ABuySell, ATrdaccID, AClassCode, ASecCode: String;
      const ATransID: LongWord; const AQuantity: Integer;
      var pMessageResult: String): Int64;
    /// <summary>
    /// Снять стоп заявку
    /// </summary>
    function GetDeleteStopLimitOrder(const AStopOrderNo: Int64;
      const ATransID: LongWord; const ATrdaccID, AClassCode, ASecCode: String;
      var pMessageResult: String): Int64;
  (****************************************************************************)
    /// <summary>
    ///  Стоп лимит
    /// </summary>
    function GetAnsyNewStopLimitOrder(const APrice, AStopPrice: Double;
      const ABuySell, ATrdaccID, AClassCode, ASecCode: String;
      const ATransID: LongWord; const AQuantity: Integer): Boolean;
    /// <summary>
    /// Снять стоп заявку
    /// </summary>
    function GetAnsyDeleteStopLimitOrder(const AStopOrderNo: Int64;
      const ATransID: LongWord; const ATrdaccID, AClassCode, ASecCode: String): Boolean;
  (****************************************************************************)
  public {снять все заявки}
    /// <summary>
    /// Снять все заяки
    /// </summary>
    function GetKillAllOrders(const ATransID: LongWord; const ATrdaccID,
      AClassCode: String; var pMessageResult: String): Int64;
    /// <summary>
    /// Снять все стоп заяки
    /// </summary>
    function GetKillAllStopOrders(const ATransID: LongWord; const ATrdaccID,
      AClassCode: String; var pMessageResult: String): Int64;
    /// <summary>
    /// Снять все заявки на ФОРТС
    /// </summary>
    function GetKillAllFuturesOrders(const ATransID: LongWord; const ATrdaccID,
      AClassCode: String; var pMessageResult: String): Int64;
  (****************************************************************************)
    property OnResultMessage: TOnEventResultMessage write FOnResultMessage;
    ///	<summary>
    ///	  Опеределяем состояние соединение с событиями транзакции заявок и
    ///	  сделок
    ///	</summary>
    property ActiveConnect: Boolean read FActiveConnect;
  end;

type { Базовый объект заявками стратегии }
  ///	<summary>
  ///	  Объект организует Асинхроную работу испольнение заявку
  ///	</summary>
  TCustomAsyncOrder = class(TCustomOrder)
  public
    ///	<summary>
    ///	  Новая заявка
    ///	</summary>
    /// <param name="APrice">Цена заявку</param>
    /// <param name="AQuantity">Объем заявки</param>
    /// <param name="ABuySell">Направление заявки</param>
    /// <remarks>
    ///   Номер транзакции
    /// </remarks>
    function GetNewOrder(const APrice: Double; const AQuantity: Integer;
      const ABuySell: String): LongWord;
    ///	<summary>
    ///	  Изменение условие заявки
    ///	</summary>
    /// <param name="APrice">Цена заявки</param>
    /// <param name="">
    function GetMoveOrder(const APrice: Double; const AQuantity: Integer;
      const AOrderNo: Int64): LongWord;
    ///	<summary>
    ///	  Удаление условия заявки
    ///	</summary>
    function GetDeleteOrder(const AOrderNo: Int64): LongWord;
  public {стоп заявки}
    /// <summary>
    ///  Стоп лимит
    /// </summary>
    function GetNewStopLimitOrder(const APrice, AStopPrice: Double;
      const AQuantity: Integer; const ABuySell: String): LongWord;
    /// <summary>
    /// Снять стоп заявку
    /// </summary>
    function GetDeleteStopLimitOrder(const AStopOrderNo: Int64): LongWord;
  end;

type{ Базовый объект заявками стратегии }
  ///	<summary>
  ///	  Организует синхроную работу заявок
  ///	</summary>
  TCustomSyncOrder = class(TCustomOrder)
  public {Обычные заявки}
    ///	<summary>
    ///	  Новая заявка, вслучае успеха возрощается не нулевое значение, а номер
    ///	  заявки OrderNo
    ///	</summary>
    function GetNewOrder(const APrice: Double; const AQuantity: Integer;
      const ABuySell: String; var pMessageResult: String): Int64;
    ///	<summary>
    ///	  Изменение условие заявки, вслучае успеха возрощается не нулевое
    ///	  значение, а номер заявки OrderNo
    ///	</summary>
    function GetMoveOrder(const APrice: Double; const AQuantity: Integer;
      const AOrderNo: Int64; var pMessageResult: String): Int64;
    ///	<summary>
    ///	  Удаление условия заявки, вслучае успеха возрощается не нулевое
    ///	  значение, а номер заявки OrderNo
    ///	</summary>
    function GetDeleteOrder(const AOrderNo: Int64;
      var pMessageResult: String): Int64;
  public {стоп заявки}
    /// <summary>
    ///  Стоп лимит
    /// </summary>
    function GetNewStopLimitOrder(const APrice, AStopPrice: Double;
      const AQuantity: Integer; const ABuySell: String;
      var pMessageResult: String): Int64;
    /// <summary>
    /// Снять стоп заявку
    /// </summary>
    function GetDeleteStopLimitOrder(const AStopOrderNo: Int64;
      var pMessageResult: String): Int64;
  end;

///	<summary>
///	  Процедура установки соединения, с библиотекой trans2quik.dll
///	</summary>
/// <remarks>
///   Нужно указать путь на торговый терминал
/// </remarks>
function GetConnectQUIK(ADirPath: String): Boolean;

implementation

uses
  QuikTrans2QuikAPI;

var
  TransQuik: TTransQuik = nil; //  Объект реализует работу с API функции

(******************************************************************************)
(*                          Приведение цены вид                               *)
function FloatToStrP(const Value: Double): String; inline;
begin
  // FormatSettings.DecimalSeparator := '.';
  Result := FloatToStr(Value);
  // FormatSettings.DecimalSeparator := ',';
end;


(******************************************************************************)
(*   Процедура установки соединение *)
function GetConnectQUIK(ADirPath: String): Boolean;
begin
  if TransQuik.ActiveConnect then begin
    Result := TransQuik.ActiveConnect;
  end else begin
    Result := TransQuik.Connect(ADirPath);
  end;
end;

(******************************************************************************)
(*   Функции обратного вызова, получение сообщение от сервера                 *)
(*   SetTransactionReplyCallback - Транзакции                                 *)
(*   SetOrdersStatusCallback     - Заявки                                     *)
(*   SetTradesStatusCallback     - Сделки                                     *)

(******************************************************************************)
(*   Для того чтобы программа не зависала нужно сделать временную задержку    *)
(*   Процедура обратного вызова                                               *)

procedure SetTransactionReplyCallback(nTransactionResult: LongInt;
  nTransactionExtendedErrorCode: LongInt; nTransactionReplyCode: LongInt;
  dwTransId: DWord; dOrderNum: Double;
  lpcstrTransactionReplyMessage: LPCSTR); stdcall;
var
  MsgResult: String;
begin
  { Позрошает результат транзакции }
  try
    if TransQuik <> nil then begin
       MsgResult := '(' + IntToStr(dwTransId) + ')  ' +
                String(lpcstrTransactionReplyMessage);
       TransQuik.SetTransReplyCallback(dwTransId,Trunc(dOrderNum),MsgResult);
    end;
  except
    raise Exception.Create('Error Message'#10#13'SetTransactionReplyCallback');
  end;
end;

function GetToBuySell(nIsSell: LongInt): String; inline;
begin
  case nIsSell of
    0: Result := 'B';
    1: Result := 'S';
  else
    Result := '';
    raise Exception.Create('Error Message'#10#13'Не определено ' +
      'направление позиции');
  end;
end;

procedure SetOrdersStatusCallback(nMode: LongInt; dwTransId: DWord;
  dNumber: Double; lpstrClassCode: LPSTR; lpstrSecCode: LPSTR; dPrice: Double;
  nBalance: LongInt; dValue: Double; nIsSell: LongInt; nStatus: LongInt;
  nOrderDescriptor: LongInt); stdcall;
var
  Quantity: LongInt;
begin
  try
    if TransQuik <> nil then begin
       Quantity := t2qOrderQty(nOrderDescriptor);
       TransQuik.SetOrdersCallback(dwTransId,Trunc(dNumber),
         String(lpstrClassCode),String(lpstrSecCode),GetToBuySell(nIsSell),
         dPrice,nBalance,Quantity,nStatus);
    end;
  except
    raise Exception.Create('Error Message'#10#13'SetOrdersStatusCallback');
  end;
end;

procedure SetTradesStatusCallback(nMode: LongInt; dNumber: Double;
  dOrderNum: Double; lpstrClassCode, lpstrSecCode: LPSTR; dPrice: Double;
  nQty: LongInt; dValue: Double; nIsSell: LongInt;
  nTradeDescriptor: LongInt); stdcall;
begin
  try
    if TransQuik <> nil then
    begin
      TransQuik.SetTradesCallback(Trunc(dNumber),Trunc(dOrderNum),
        String(lpstrClassCode),String(lpstrSecCode),GetToBuySell(nIsSell),
        dPrice,nQty);
    end;
  except
    raise Exception.Create('Error Message'#10#13'SetTradesStatusCallback');
  end;
end;

(******************************************************************************)

{ TCustomThread }

constructor TCustomThread.Create;
begin
  inherited Create(True);  // Поток запускается при старте Self.Start;
  FreeOnTerminate := True; // Объект сам разрушится после
  Priority := tpNormal;
end;

procedure TCustomThread.Execute;
begin
  System.SysUtils.Sleep(THEAD_SLEEP);
end;

{ TEventsOrder }

procedure TEventsOrder.DoEventTransReplyCallback(const ATransID: LongWord;
  const AOrderNo: Int64; const AMsgResult: String);
begin
  if Assigned(FOnEventTransactions) then
    FOnEventTransactions(Self,ATransID,AOrderNo,AMsgResult);
end;

procedure TEventsOrder.DoEventOrdersCallback(const ATransID: LongWord;
  const AOrderNo: Int64; const AClassCode, ASecCode, ABuySell: String;
  const APrice: Double; const ABalance, AQuantity, AStatus: Integer);
begin
  if Assigned(FOnEventOrders) then
    FOnEventOrders(Self,ATransID,AOrderNo,AClassCode,ASecCode,
        ABuySell,APrice,ABalance,AQuantity,AStatus);
end;

procedure TEventsOrder.DoEventTradesCallback(const ATradeNo, AOrderNo: Int64;
  const AClassCode, ASecCode, ABuySell: String; const APrice: Double;
  const AQuantity: Integer);
begin
  if Assigned(FOnEventTrades) then
    FOnEventTrades(Self,ATradeNo,AOrderNo,AClassCode,ASecCode,
        ABuySell,APrice,AQuantity);
end;

procedure TEventsOrder.DoNewOrder(const ATransID: LongWord);
begin
  if Assigned(FOnNewOrder) then
    FOnNewOrder(Self,ATransID);
end;

procedure TEventsOrder.DoMoveOrder(const ATransID: LongWord);
begin
  if Assigned(FOnMoveOrder) then
    FOnMoveOrder(Self,ATransID);
end;

procedure TEventsOrder.DoDeleteOrder(const ATransID: LongWord);
begin
  if Assigned(FOnDeleteOrder) then
    FOnDeleteOrder(Self,ATransID);
end;

constructor TEventsOrder.Create;
begin
end;

destructor TEventsOrder.Destroy;
begin
  inherited;
end;

{ TCustomOrder }

constructor TCustomOrder.Create;
begin
  inherited Create;
  FIndex := TransQuik.GetAdd(Self);
end;

destructor TCustomOrder.Destroy;
begin
  if FIndex >= 0 then TransQuik.SetDeleteOrder(FIndex);
  inherited Destroy;
end;

procedure TCustomOrder.SetTransReplyCallback(const ATransID: LongWord;
  const AOrderNo: Int64; const AMsgResult: String);
begin
  DoEventTransReplyCallback(ATransID,AOrderNo,AMsgResult);
end;

procedure TCustomOrder.SetOrdersCallback(const ATransID: LongWord;
  const AOrderNo: Int64; const AClassCode, ASecCode, ABuySell: String;
  const APrice: Double; const ABalance, AQuantity, AStatus: Integer);
begin
  DoEventOrdersCallback(ATransID,AOrderNo,AClassCode,ASecCode,ABuySell,
    APrice,ABalance,AQuantity,AStatus);
end;

procedure TCustomOrder.SetTradesCallback(const ATradeNo, AOrderNo: Int64;
  const AClassCode, ASecCode, ABuySell: String; const APrice: Double;
  const AQuantity: Integer);
begin
  DoEventTradesCallback(ATradeNo,AOrderNo,AClassCode,ASecCode,ABuySell,APrice,
    AQuantity);
end;

function TCustomOrder.GetKillAllOrders(var pMessageResult: String): Int64;
var
  TransID: Cardinal;
begin
  TransID := TransQuik.GetAutoIncTransID;
  Result := TransQuik.GetKillAllOrders(TransID,FTrdaccID,FClassCode,pMessageResult);
end;

function TCustomOrder.GetKillAllStopOrders(var pMessageResult: String): Int64;
var
  TransID: Cardinal;
begin
  TransID := TransQuik.GetAutoIncTransID;
  Result := TransQuik.GetKillAllStopOrders(TransID,FTrdaccID,FClassCode,pMessageResult);
end;

function TCustomOrder.GetKillAllFuturesOrders(var pMessageResult: String): Int64;
var
  TransID: Cardinal;
begin
  TransID := TransQuik.GetAutoIncTransID;
  Result := TransQuik.GetKillAllFuturesOrders(TransID,FTrdaccID,FClassCode,pMessageResult);
end;

{ TTransQuik }

constructor TTransQuik.Create;
begin
  FAutoIncTransID := 0;
  FActiveConnect := False;
  FOrders := TCustomOrders.Create(False);
end;

destructor TTransQuik.Destroy;
begin
  FOrders.Free;
  inherited;
end;

function TTransQuik.Connect(const AConnectionParams: String): Boolean;
var
  liResult: LongInt;
  liCodeError: LongInt;
  acMessagError: array [0 .. MAX_PATH - 1] of AnsiChar;
begin
  Result := False;
  FConnectSUBSCRIBE := False;
  FConnectionParams := AConnectionParams;
  if GetTransactionsReplyCallback then begin
     FillChar(acMessagError, MAX_PATH - 1, 0);
     liResult := t2qConnect(PAnsiChar(AnsiString(AConnectionParams)),
        liCodeError,acMessagError, MAX_PATH - 1);
     if liResult <> 0 then
      Self.SetResultMessage(String(acMessagError));
     case liResult of
        TRANS2QUIK_SUCCESS:
          begin
            FActiveConnect := True;
            Result := True;
            Self.SetResultMessage('соединение установлено успешно');
            Self.SetConnectSUBSCRIBE;
          end;
        TRANS2QUIK_QUIK_TERMINAL_NOT_FOUND:
          begin
            raise Exception.Create('Error Message' +
              #10#13'TGlobalOrder.GetConnect' +
              #10#13'В указанном каталоге либо отсутствует INFO.EXE, ' +
              #10#13'либо у него не запущен сервис обработки внешних подключений' +
              sLineBreak + String(acMessagError));
            Exit;
          end;
        TRANS2QUIK_DLL_VERSION_NOT_SUPPORTED:
          begin
            raise Exception.Create('Error Message' +
              #10#13'TGlobalOrder.GetConnect' + #10#13'Используемая версия ' +
              'Trans2QUIK.dll не поддерживается указанным INFO.EXE' +
              sLineBreak + String(acMessagError));
            Exit;
          end;
        TRANS2QUIK_ALREADY_CONNECTED_TO_QUIK:
          begin
            raise Exception.Create('Error Message' +
              #10#13'TGlobalOrder.GetConnect' +
              #10#13'соединение уже установлено' +
              sLineBreak + String(acMessagError));
            Exit;
          end;
        TRANS2QUIK_FAILED:
          begin
            raise Exception.Create('Error Message' +
              #10#13'TGlobalOrder.GetConnect' +
              #10#13'произошла ошибка при установлении соединения' +
              sLineBreak + String(acMessagError));
            Exit;
          end;
     end;
  end;
end;

procedure TTransQuik.DisConnect;
var
  liResult: LongInt;
  liCodeError: LongInt;
  acMessagError: array [0 .. MAX_PATH - 1] of AnsiChar;
begin
  liCodeError := 0;
  FillChar(acMessagError, MAX_PATH - 1, 0);
  liResult := t2qDisConnect(liCodeError,acMessagError,MAX_PATH - 1);
  if liResult = TRANS2QUIK_SUCCESS then begin
    Self.SetDisConnectSUBSCRIBE;
    Self.SetResultMessage(String(acMessagError));
    Self.SetResultMessage('Cоединение библиотеки ' +
      'Trans2QUIK.dll с рабочим местом QUIK разорвано успешно');
  end;
end;

procedure TTransQuik.SetClearAutoIncTransID;
begin
  FAutoIncTransID := 0;
end;

function TTransQuik.GetAutoIncTransID: LongWord;
begin
  if FAutoIncTransID > 200 then FAutoIncTransID := 0;
  FAutoIncTransID := FAutoIncTransID + 1;
  Result := FAutoIncTransID;
end;

procedure TTransQuik.Clear;
begin
  FOrders.Clear;
end;

function TTransQuik.GetAdd(const ACustomOrder: TCustomOrder): Integer;
begin
  Result := FOrders.Add(ACustomOrder);
end;


procedure TTransQuik.SetDeleteOrder(const AIndex: Integer);
begin
  FOrders.Delete(AIndex)
end;

function TTransQuik.GetTransactionsReplyCallback: Boolean;
var
  liResult: LongInt;
  liErrorCode: LongInt;
  acMessagError: array [0 .. MAX_PATH - 1] of AnsiChar;
begin
  FillChar(acMessagError, MAX_PATH - 1, 0);
  liResult := t2qSetTransactionsReplyCallback(SetTransactionReplyCallback,
    liErrorCode,acMessagError,MAX_PATH - 1);
  if liResult <> 0 then begin
     Self.SetResultMessage(String(acMessagError));
     Result := False;
  end else Result := True;
end;

procedure TTransQuik.SetConnectSUBSCRIBE;
begin
  FConnectSUBSCRIBE := True;
  Self.SetConnectOrdersSUBSCRIBE;
  Self.SetConnectTradesSUBSCRIBE;
end;

procedure TTransQuik.SetConnectOrdersSUBSCRIBE;
var
  liResult: LongInt;
begin
  liResult := t2qSubscribeTrades(PAnsiChar(''), PAnsiChar(''));
  if liResult = TRANS2QUIK_SUCCESS then t2qStartTrades(SetTradesStatusCallback);
  case liResult of
    TRANS2QUIK_SUCCESS:
      Self.SetResultMessage('подписка проведена успешно');
    TRANS2QUIK_DLL_NOT_CONNECTED:
      begin
        raise Exception.Create('Error Message' +
          #10#13'TGlobalOrder.SetConnectSUBSCRIBE' +
          #10#13'Не установлена связь ' +
          'библиотеки Trans2QUIK.dll с терминалом QUIK.');
        Exit;
      end;
    TRANS2QUIK_QUIK_NOT_CONNECTED:
      begin
        raise Exception.Create('Error Message' +
          #10#13'TGlobalOrder.SetConnectSUBSCRIBE' +
          #10#13'Не установлена связь ' +
          'между Рабочим местом QUIK и сервером.');
        Exit;
      end;
    TRANS2QUIK_FAILED:
      begin
        raise Exception.Create('Error Message' +
          #10#13'TGlobalOrder.SetConnectSUBSCRIBE' +
          #10#13'попытка подписки завершилась неуспешно');
        Exit;
      end;
  end;
end;

procedure TTransQuik.SetConnectTradesSUBSCRIBE;
var
  liResult: LongInt;
begin
  liResult := t2qSubscribeOrders(PAnsiChar(''),PAnsiChar(''));
  if liResult = TRANS2QUIK_SUCCESS then t2qStartOrders(SetOrdersStatusCallback);
  case liResult of
    // TRANS2QUIK_SUCCESS: Log('подписка проведена успешно');
    TRANS2QUIK_DLL_NOT_CONNECTED:
      begin
        raise Exception.Create('Error Message' +
          #10#13'TGlobalOrder.SetConnectSUBSCRIBE' +
          #10#13'Не установлена связь ' +
          'библиотеки Trans2QUIK.dll с терминалом QUIK.');
        Exit;
      end;
    TRANS2QUIK_QUIK_NOT_CONNECTED:
      begin
        raise Exception.Create('Error Message' +
          #10#13'TGlobalOrder.SetConnectSUBSCRIBE' +
          #10#13'Не установлена связь между Рабочим местом QUIK и сервером.');
        Exit;
      end;
    TRANS2QUIK_FAILED:
      begin
        raise Exception.Create('Error Message' +
          #10#13'TGlobalOrder.SetConnectSUBSCRIBE' +
          #10#13'Попытка подписки завершилась неуспешно');
        Exit;
      end;
  end;
end;

procedure TTransQuik.SetDisConnectSUBSCRIBE;
begin
  if FConnectSUBSCRIBE then begin
    t2qUnSubscribeOrders;
    t2qUnSubscribeTrades;
  end;
end;

procedure TTransQuik.SetResultMessage(const pResultMessage: String);
begin
  if Assigned(FOnResultMessage) then FOnResultMessage(Self, pResultMessage);
end;

procedure TTransQuik.SetTransReplyCallback(const ATransID: LongWord;
  const AOrderNo: Int64; const AMsgResult: String);
begin
  { Результат транзакции }
  with TTransactionThread.Create do begin
    TransID  := ATransID;
    OrderNo  := AOrderNo;
    MsgResult:= AMsgResult;
    OnTerminate := TransEventThread;
    Start;
  end;
end;

procedure TTransQuik.SetOrdersCallback(const ATransID: LongWord;
  const AOrderNo: Int64; const AClassCode, ASecCode, ABuySell: String;
  const APrice: Double; const ABalance, AQuantity, AStatus: Integer);
begin
  { Событие заявки }
  with TOrderThread.Create do begin
    TransID  := ATransID;   //  Номер транзакции
    OrderNo  := AOrderNo;   //  Номер заявку
    ClassCode:= AClassCode; //  Класс инструмента
    SecCode  := ASecCode;   //  Код инструмента
    BuySell  := ABuySell;   //  Напровление позиции
    Price    := APrice;     //  Цена
    Balance  := ABalance;   //  Остаток
    Quantity := AQuantity;  //  Количество
    Status   := AStatus;    //  Статус операции
    OnTerminate := OrderEventThread;
    Start;
  end;
end;

procedure TTransQuik.SetTradesCallback(const ATradeNo, AOrderNo: Int64;
  const AClassCode, ASecCode, ABuySell: String; const APrice: Double;
  const AQuantity: Integer);
begin
  { Событие сделки }
  with TTradeThread.Create do begin
    TradeNo   := ATradeNo;  //  Номер сделки
    OrderNo   := AOrderNo;  //  Номер заявки
    ClassCode := AClassCode;//  Класс инстурмента
    SecCode   := ASecCode;  //  Код инстурмента
    BuySell   := ABuySell;  //  Напровление позиции
    Price     := APrice;    //  Цена позиции
    Quantity  := AQuantity; //  Количество
    OnTerminate := TradeEventThread;
    Start;
  end;
end;

procedure TTransQuik.TransEventThread(Sender: TObject);
var
  TT: TTransactionThread;
  CO: TCustomOrder;
  i, Count: Integer;
begin
  TT := TTransactionThread(Sender);
  Count := FOrders.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      CO := FOrders[i];
      CO.SetTransReplyCallback(TT.TransID,TT.OrderNo,TT.MsgResult);
    end;
end;

procedure TTransQuik.OrderEventThread(Sender: TObject);
var
  OT: TOrderThread;
  CO: TCustomOrder;
  i, Count: Integer;
begin
  OT := TOrderThread(Sender);
  Count := FOrders.Count;
  if Count > 0 then for i := 0 to Count - 1 do begin
     CO := FOrders[i];
     CO.SetOrdersCallback(OT.TransID,OT.OrderNo,OT.ClassCode,OT.SecCode,
          OT.BuySell,OT.Price,OT.Balance,OT.Quantity,OT.Status);
  end;
end;

procedure TTransQuik.TradeEventThread(Sender: TObject);
var
  TT: TTradeThread;
  CO: TCustomOrder;
  i, Count: Integer;
begin
  TT := TTradeThread(Sender);
  Count := FOrders.Count;
  if Count > 0 then for i := 0 to Count - 1 do begin
     CO := FOrders[i];
     CO.SetTradesCallback(TT.TradeNo,TT.OrderNo,TT.ClassCode,TT.SecCode,
          TT.BuySell,TT.Price,TT.Quantity);
  end;
end;

function TTransQuik.GetSendSyncTransaction(const ASend: String;
  var pMessageResult: String): Int64;
var
  liResult: LongInt;
  pdOrderNum: Double;
  pdwTransID: DWord;
  CodeError, pnReplyCode: LongInt;
  anMessagError, lpstrResultMessage: array [0 .. MAX_PATH - 1] of AnsiChar;
  BuffSend: UTF8String;
begin
  { Синхронная отправка приказа }
  if not FActiveConnect then begin
    raise Exception.Create('Error Message'#10#13 +
                           'Не установлено соединение с библиотекой');
    Exit;
  end;

  CodeError := 0;
  pnReplyCode := 0;
  pdwTransID := 0;
  pdOrderNum := 0;

  BuffSend := UTF8Encode(ASend);
  liResult := t2qSendSyncTransaction(PAnsiChar(BuffSend), pnReplyCode,
    pdwTransID, pdOrderNum, lpstrResultMessage, MAX_PATH - 1, CodeError,
    anMessagError, MAX_PATH - 1);

  pMessageResult := String(lpstrResultMessage);
  case liResult of
    TRANS2QUIK_SUCCESS: begin
      Self.SetResultMessage('транзакция успешно отправлена на сервер');
    end;
    TRANS2QUIK_WRONG_SYNTAX:
      begin
        raise Exception.Create('Error Message'#10#13'Cтрока транзакции ' +
          'заполнена неверно'#10#13 + String(anMessagError) + #10#13 +
          String(ASend));
      end;
    TRANS2QUIK_DLL_NOT_CONNECTED:
      begin
        raise Exception.Create('Error Message'#10#13'Отсутствует соединение ' +
          'между библиотекой Trans2QUIK.dll и терминалом QUIK' + #10#13 +
          String(anMessagError));
      end;
    TRANS2QUIK_QUIK_NOT_CONNECTED:
      begin
        raise Exception.Create('Error Message'#10#13'Отсутствует соединение ' +
          'между терминалом QUIK и сервером' + #10#13 + String(anMessagError));
      end;
    TRANS2QUIK_FAILED:
      begin
        raise Exception.Create('Error Message'#10#13'транзакцию передать ' +
          'не удалось'#10#13 + String(anMessagError) + #10#13' pnReplyCode ' +
          IntToStr(pnReplyCode));
      end;
  end;
  Result := Trunc(pdOrderNum);
end;

function TTransQuik.GetSendAsyncTransaction(ASend: String): Boolean;
var
  pnReplyCode, liResult: LongInt;
  anMessagError: array [0 .. MAX_PATH - 1] of AnsiChar;
  BuffSend: UTF8String;
begin
  { Асинхронная отправка приказа }
  if not FActiveConnect then begin
    raise Exception.Create('Error Message'#10#13 +
                           'Не установлено соединение с библиотекой');
    Exit;
  end;

  pnReplyCode := 0;
  BuffSend := UTF8Encode(ASend);
  liResult := t2qSendAsyncTransaction(PAnsiChar(BuffSend), pnReplyCode,
    anMessagError, MAX_PATH - 1);

  case liResult of
    TRANS2QUIK_SUCCESS:
      Self.SetResultMessage('Транзакция успешно отправлена на сервер');
    TRANS2QUIK_WRONG_SYNTAX:
      begin
        raise Exception.Create('Error Message'#10#13'Cтрока транзакции ' +
          'заполнена неверно'#10#13 + String(anMessagError));
      end;
    TRANS2QUIK_DLL_NOT_CONNECTED:
      begin
        raise Exception.Create('Error Message'#10#13'Отсутствует соединение ' +
          'между библиотекой Trans2QUIK.dll и терминалом QUIK' + #10#13 +
          String(anMessagError));
      end;
    TRANS2QUIK_QUIK_NOT_CONNECTED:
      begin
        raise Exception.Create('Error Message'#10#13'Отсутствует соединение ' +
          'между терминалом QUIK и сервером' + #10#13 + String(anMessagError));
      end;
    TRANS2QUIK_FAILED:
      begin
        raise Exception.Create('Error Message'#10#13'транзакцию передать ' +
          'не удалось'#10#13 + String(anMessagError) + #10#13' pnReplyCode ' +
          IntToStr(pnReplyCode));
      end;
  end;
  Result := (liResult = TRANS2QUIK_SUCCESS);
end;

function TTransQuik.GetNewOrderSyncQuik(const APrice: Double;
  const AQuantity: Integer; const ATransID: LongWord; const ABuySell,
  ATrdaccID, AClassCode, ASecCode: String;
  var pMessageResult: String): Int64;
var
  Send: String;
begin
  Send := 'ACCOUNT=' + ATrdaccID + ';' +
          'TYPE=L;' +
          'TRANS_ID=' + IntToStr(ATransID) + ';' +
          'CLASSCODE=' + AClassCode + ';' +
          'SECCODE=' + ASecCode + ';' +
          'ACTION=NEW_ORDER;' +
          'OPERATION=' + ABuySell + ';' +
          'PRICE=' + FloatToStrP(APrice) + ';' +
          'QUANTITY=' + IntToStr(AQuantity) + ';';
  Result := Self.GetSendSyncTransaction(Send, pMessageResult);
end;

function TTransQuik.GetMoveOrderSyncQuik(const APrice: Double;
  const AQuantity: Integer; const ATransID: LongWord; const AOrderNo: Int64;
  const AClassCode, ASecCode: String;
  var pMessageResult: String): Int64;
var
  Send: String;
begin
  Send := 'ACTION=MOVE_ORDERS;' +
          'TRANS_ID=' + IntToStr(ATransID) + ';' +
          'CLASSCODE=' + AClassCode + ';' +
          'SECCODE=' + ASecCode + ';' +
          'MODE=1;' +
          'FIRST_ORDER_NUMBER=' + IntToStr(AOrderNo) + ';' +
          'FIRST_ORDER_NEW_PRICE=' + FloatToStrP(APrice) + ';' +
          'FIRST_ORDER_NEW_QUANTITY=' + IntToStr(AQuantity) + ';' +
          'SECOND_ORDER_NUMBER=;' +
          'SECOND_ORDER_NEW_PRICE=;' +
          'SECOND_ORDER_NEW_QUANTITY=;';
  Result := GetSendSyncTransaction(Send, pMessageResult);
end;

function TTransQuik.GetDeleteOrderSyncQuik(const AOrderNo: Int64;
  const ATransID: LongWord; const AClassCode, ASecCode: String;
  var pMessageResult: String): Int64;
var
  Send: String;
begin
  Send := 'CLASSCODE=' + AClassCode + ';' +
          'SECCODE=' + ASecCode + ';' +
          'TRANS_ID=' + IntToStr(ATransID) + ';' +
          'ACTION=KILL_ORDER;' +
          'ORDER_KEY=' + IntToStr(AOrderNo) + ';';
  Result := GetSendSyncTransaction(Send, pMessageResult);
end;

function TTransQuik.GetNewOrderAsyncQuik(const APrice: Double;
  const AQuantity: Integer; const ATransID: LongWord; const ABuySell, ATrdaccID,
  AClassCode, ASecCode: String): Boolean;
var
  Send: String;
begin
  Send := 'ACCOUNT=' + ATrdaccID + ';' +
          'TYPE=L;' +
          'TRANS_ID=' + IntToStr(ATransID) + ';' +
          'CLASSCODE=' + AClassCode + ';' +
          'SECCODE=' + ASecCode + ';' +
          'ACTION=NEW_ORDER;' +
          'OPERATION=' + ABuySell + ';' +
          'PRICE=' + FloatToStrP(APrice) + ';' +
          'QUANTITY=' + IntToStr(AQuantity) + ';';
  Result := Self.GetSendAsyncTransaction(Send);
end;

function TTransQuik.GetMoveOrderAsyncQuik(const APrice: Double;
  const AQuantity: Integer; const ATransID: LongWord; const AOrderNo: Int64;
  const AClassCode, ASecCode: String): Boolean;
var
  Send: String;
begin
  Send := 'ACTION=MOVE_ORDERS;' +
          'TRANS_ID=' + IntToStr(ATransID) + ';' +
          'CLASSCODE=' + AClassCode + ';' +
          'SECCODE=' + ASecCode + ';' +
          'MODE=1;' +
          'FIRST_ORDER_NUMBER=' + IntToStr(AOrderNo) + ';' +
          'FIRST_ORDER_NEW_PRICE=' + FloatToStrP(APrice) + ';' +
          'FIRST_ORDER_NEW_QUANTITY=' + IntToStr(AQuantity) + ';' +
          'SECOND_ORDER_NUMBER=;' +
          'SECOND_ORDER_NEW_PRICE=;' +
          'SECOND_ORDER_NEW_QUANTITY=;';
  Result := GetSendAsyncTransaction(Send);
end;

function TTransQuik.GetDeleteOrderAsyncQuik(const AOrderNo: Int64;
  const ATransID: LongWord; const AClassCode, ASecCode: String): Boolean;
var
  Send: String;
begin
  Send := 'CLASSCODE=' + AClassCode + ';' +
          'SECCODE=' + ASecCode + ';' +
          'TRANS_ID=' + IntToStr(ATransID) + ';' +
          'ACTION=KILL_ORDER;' +
          'ORDER_KEY=' + IntToStr(AOrderNo) + ';';
  Result := GetSendAsyncTransaction(Send);
end;

function TTransQuik.GetNewStopLimitOrder(const APrice, AStopPrice: Double;
  const ABuySell, ATrdaccID, AClassCode, ASecCode: String;
  const ATransID: LongWord; const AQuantity: Integer;
  var pMessageResult: String): Int64;
var
  Send: String;
begin
  Send := 'ACTION=NEW_STOP_ORDER;' +
          'ACCOUNT=' + ATrdaccID + ';' +
          'TRANS_ID=' + IntToStr(ATransID) + ';' +
          'CLASSCODE=' + AClassCode + ';' +
          'SECCODE=' + ASecCode + ';' +
          'OPERATION=' + ABuySell + ';' +
          'QUANTITY=' + IntToStr(AQuantity) + ';' +
          'PRICE=' + FloatToStrP(APrice) + ';' +
          'STOPPRICE=' + FloatToStrP(AStopPrice) + ';' +
          'EXPIRY_DATE=' + FormatDateTime('yyyymmdd',Date) + ';';
  Result := GetSendSyncTransaction(Send, pMessageResult);
end;

function TTransQuik.GetDeleteStopLimitOrder(const AStopOrderNo: Int64;
  const ATransID: LongWord; const ATrdaccID, AClassCode, ASecCode: String;
  var pMessageResult: String): Int64;
var
  Send: String;
begin
  Send := 'CLASSCODE=' + AClassCode + ';' +
          'SECCODE=' + ASecCode + ';' +
          'ACCOUNT=' + ATrdaccID + ';' +
          'TRANS_ID=' + IntToStr(ATransID) + ';' +
          'ACTION=KILL_STOP_ORDER;' +
          'STOP_ORDER_KEY=' + IntToStr(AStopOrderNo) + ';';
  Result := GetSendSyncTransaction(Send, pMessageResult);
end;

function TTransQuik.GetKillAllOrders(const ATransID: LongWord; const ATrdaccID,
  AClassCode: String; var pMessageResult: String): Int64;
var
  Send: String;
begin
  Send := 'CLASSCODE=' + AClassCode + ';' +
          'ACCOUNT=' + ATrdaccID + ';' +
          'TRANS_ID=' + IntToStr(ATransID) + ';' +
          'ACTION=KILL_ALL_ORDERS;';
  Result := GetSendSyncTransaction(Send, pMessageResult);
end;

function TTransQuik.GetAnsyDeleteStopLimitOrder(const AStopOrderNo: Int64;
  const ATransID: LongWord; const ATrdaccID, AClassCode, ASecCode: String): Boolean;
var
  Send: String;
begin
  Send := 'CLASSCODE=' + AClassCode + ';' +
          'SECCODE=' + ASecCode + ';' +
          'ACCOUNT=' + ATrdaccID + ';' +
          'TRANS_ID=' + IntToStr(ATransID) + ';' +
          'ACTION=KILL_STOP_ORDER;' +
          'STOP_ORDER_KEY=' + IntToStr(AStopOrderNo) + ';';
  Result := Self.GetSendAsyncTransaction(Send);
end;

function TTransQuik.GetAnsyNewStopLimitOrder(const APrice, AStopPrice: Double;
  const ABuySell, ATrdaccID, AClassCode, ASecCode: String;
  const ATransID: LongWord; const AQuantity: Integer): Boolean;
var
  Send: String;
begin
   Send := 'CLASSCODE=' + AClassCode + ';' +
          'ACCOUNT=' + ATrdaccID + ';' +
          'TRANS_ID=' + IntToStr(ATransID) + ';' +
          'ACTION=KILL_ALL_ORDERS;';
   Result := Self.GetSendAsyncTransaction(Send);
end;


function TTransQuik.GetKillAllStopOrders(const ATransID: LongWord;
  const ATrdaccID, AClassCode: String; var pMessageResult: String): Int64;
var
  Send: String;
begin
  Send := 'CLASSCODE=' + AClassCode + ';' +
          'ACCOUNT=' + ATrdaccID + ';' +
          'TRANS_ID=' + IntToStr(ATransID) + ';' +
          'ACTION=KILL_ALL_STOP_ORDERS;';
  Result := GetSendSyncTransaction(Send, pMessageResult);
end;

function TTransQuik.GetKillAllFuturesOrders(const ATransID: LongWord;
  const ATrdaccID, AClassCode: String; var pMessageResult: String): Int64;
var
  Send: String;
begin
  Send := 'CLASSCODE=' + AClassCode + ';' +
          'ACCOUNT=' + ATrdaccID + ';' +
          'TRANS_ID=' + IntToStr(ATransID) + ';' +
          'ACTION=KILL_ALL_FUTURES_ORDERS;';
  Result := GetSendSyncTransaction(Send, pMessageResult);
end;

{ TCustomAsyncOrder }

function TCustomAsyncOrder.GetNewOrder(const APrice: Double;
  const AQuantity: Integer; const ABuySell: String): LongWord;
var
  bResult: Boolean;
  TransID: LongWord;
begin
  TransID := TransQuik.GetAutoIncTransID;
  bResult := TransQuik.GetNewOrderAsyncQuik(APrice,AQuantity,TransID,ABuySell,
               FTrdaccID,FClassCode,FSecCode);
  if bResult then Result := TransID else Result := 0;
  Self.DoNewOrder(TransID);
end;


function TCustomAsyncOrder.GetMoveOrder(const APrice: Double;
  const AQuantity: Integer; const AOrderNo: Int64): LongWord;
var
  bResult: Boolean;
  TransID: LongWord;
begin
  TransID := TransQuik.GetAutoIncTransID;
  bResult := TransQuik.GetMoveOrderAsyncQuik(APrice,AQuantity,TransID,AOrderNo,
               FClassCode,FSecCode);
  if bResult then Result := TransID else Result := 0;
  Self.DoMoveOrder(TransID);
end;

function TCustomAsyncOrder.GetDeleteOrder(const AOrderNo: Int64): LongWord;
var
  bResult: Boolean;
  TransID: LongWord;
begin
  TransID := TransQuik.GetAutoIncTransID;
  bResult := TransQuik.GetDeleteOrderAsyncQuik(AOrderNo,TransID,FClassCode,
               FSecCode);
  if bResult then Result := TransID else Result := 0;
  Self.DoDeleteOrder(TransID);
end;

function TCustomAsyncOrder.GetNewStopLimitOrder(const APrice,
  AStopPrice: Double; const AQuantity: Integer;
  const ABuySell: String): LongWord;
var
  bResult: Boolean;
  TransID: LongWord;
begin
  TransID := TransQuik.GetAutoIncTransID;
  bResult := TransQuik.GetAnsyNewStopLimitOrder(APrice,AStopPrice,ABuySell,
    FTrdaccID,FClassCode,FSecCode,TransID,AQuantity);
  if bResult then Result := TransID else Result := 0;
  Self.DoDeleteOrder(TransID);
end;


function TCustomAsyncOrder.GetDeleteStopLimitOrder(
  const AStopOrderNo: Int64): LongWord;
var
  bResult: Boolean;
  TransID: LongWord;
begin
  TransID := TransQuik.GetAutoIncTransID;
  bResult := TransQuik.GetAnsyDeleteStopLimitOrder(AStopOrderNo,TransID,FTrdaccID,FClassCode,FSecCode);
  if bResult then Result := TransID else Result := 0;
  Self.DoDeleteOrder(TransID);
end;


{ TCustomSyncOrder }

function TCustomSyncOrder.GetNewOrder(const APrice: Double;
  const AQuantity: Integer; const ABuySell: String;
  var pMessageResult: String): Int64;
var
  TransID: LongWord;
begin
  TransID := TransQuik.GetAutoIncTransID;
  Result := TransQuik.GetNewOrderSyncQuik(APrice,AQuantity,TransID,ABuySell,
    FTrdaccID,FClassCode,FSecCode,pMessageResult);
  Self.DoNewOrder(TransID);
end;

function TCustomSyncOrder.GetMoveOrder(const APrice: Double;
  const AQuantity: Integer; const AOrderNo: Int64;
  var pMessageResult: String): Int64;
var
  TransID: LongWord;
begin
  TransID := TransQuik.GetAutoIncTransID;
  Result := TransQuik.GetMoveOrderSyncQuik(APrice,AQuantity,TransID,AOrderNo,
    FClassCode,FSecCode,pMessageResult);
  Self.DoMoveOrder(TransID);
end;

function TCustomSyncOrder.GetDeleteOrder(const AOrderNo: Int64; var pMessageResult: String): Int64;
var
  TransID: LongWord;
begin
  TransID := TransQuik.GetAutoIncTransID;
  Result := TransQuik.GetDeleteOrderSyncQuik(AOrderNo,TransID,FClassCode,FSecCode,pMessageResult);
  Self.DoDeleteOrder(TransID);
end;

function TCustomSyncOrder.GetNewStopLimitOrder(const APrice, AStopPrice: Double;
  const AQuantity: Integer; const ABuySell: String;
  var pMessageResult: String): Int64;
var
  TransID: LongWord;
begin
  TransID := TransQuik.GetAutoIncTransID;
  Result := TransQuik.GetNewStopLimitOrder(APrice,AStopPrice,ABuySell,
    FTrdaccID,FClassCode,FSecCode,TransID,AQuantity,pMessageResult);
end;

function TCustomSyncOrder.GetDeleteStopLimitOrder(const AStopOrderNo: Int64;
  var pMessageResult: String): Int64;
var
  TransID: LongWord;
begin
  TransID := TransQuik.GetAutoIncTransID;
  Result := TransQuik.GetDeleteStopLimitOrder(AStopOrderNo,TransID,FTrdaccID,
    FClassCode,FSecCode,pMessageResult);
end;

initialization
  TransQuik := TTransQuik.Create;

finalization
  FreeAndNil(TransQuik);

end.
