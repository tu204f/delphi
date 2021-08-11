unit QuikTrans2Order;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, System.Generics.Collections,
  System.SyncObjs, QuikTrans2QuikAPI;

type
  TBuySell = (bsNull,bsBuy,bsSell);
  TStatusOrder = (soExecute,soActive,soShot);

  TOnMessageResult = procedure(ASender: TObject;
    const AMessageResult: String) of object;
  TOnTransaction = procedure(ASender: TObject; const ATransID: LongWord;
    const AOrderNo: Int64; const AMsgResult: String) of object;
  TOnOrders = procedure(ASender: TObject; const ATransID: LongWord;
    const AOrderNo: Int64; const ABuySell: TBuySell; const APrice: Double;
    const ABalance, AQuantity: Integer; const AStatus: TStatusOrder) of object;
  TOnTrades = procedure(ASender: TObject; const ATradeNo, AOrderNo: Int64;
    const ABuySell: TBuySell; const APrice: Double;
    const AQuantity: LongInt) of object;

  TOrder = record
    TransID: LongWord;
    OrderNo: Int64;
    BuySell: TBuySell;
    Price: Double;
    Balance: LongInt;
    Quantity: LongInt;
    Status: TStatusOrder
  end;
  TOrderList = TList<TOrder>;

  TTrade = record
    TradeNo: Int64;
    OrderNo: Int64;
    BuySell: TBuySell;
    Price: Double;
    Quantity: LongInt
  end;
  TTradeList = TList<TTrade>;

  ///<summary>
  ///  Работа объекта с конкретным инструментом
  ///</summary>
  TEventOrderCode = class(TObject)
  private
    FIndexOrder: Integer;
    FOnTransaction: TOnTransaction;
    FOnOrders: TOnOrders;
    FOnTrades: TOnTrades;
    FOnMessageResult: TOnMessageResult;
    FTrdaccID, FClassCode, FSecCode: String;
  protected
    FOrders: TOrderList;
    FTrades: TTradeList;
    function GetCodeOk(const AClassCode, ASecCode: String): Boolean;
    function GetIndexOfTransID(const ATransID: LongWord): Integer;
    function GetIndexOfOrderNo(const AOrderNo: Int64): Integer;
    {пока оставим нужна для получение ответа от сервера в случаи синхронной работы}
    {Не реализовано полностью}
    procedure DoMessageResult(const AMessageResult: String); virtual;
    procedure DoTransaction(const ATransID: LongWord; const AOrderNo: Int64;
      const AMsgResult: String); virtual;
    procedure DoOrders(const ATransID: LongWord; const AOrderNo: Int64;
      const AClassCode, ASecCode: String; const ABuySell: TBuySell; const
      APrice: Double; const ABalance, AQuantity: Integer;
      const AStatus: TStatusOrder); virtual;
    procedure DoTrades(const ATradeNo, AOrderNo: Int64; const AClassCode,
      ASecCode: String; const ABuySell: TBuySell; const APrice: Double;
      const AQuantity: LongInt); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure AddTransID(const ATransID: LongWord);
    ///<summary>
    ///  Результат работы API
    ///</summary>
    property OnMessageResult: TOnMessageResult read FOnMessageResult write FOnMessageResult;
    ///<summary>
    ///  Событие транзакции
    ///</summary>
    property OnTransaction: TOnTransaction read FOnTransaction write FOnTransaction;
    ///<summary>
    ///  Событие заявка
    ///</summary>
    property OnOrders: TOnOrders read FOnOrders write FOnOrders;
    ///<summary>
    ///  Событие сделка
    ///</summary>
    property OnTrades: TOnTrades read FOnTrades write FOnTrades;
    ///<summary>
    ///  Массив заявок
    ///</summary>
    property Orders: TOrderList read FOrders;
    ///<summary>
    ///  Массив сделок
    ///</summary>
    property Trades: TTradeList read FTrades;
    ///<summary>
    ///  Код торгового счета
    ///</summary>
    property TrdaccID: String read FTrdaccID write FTrdaccID;
    ///<summary>
    ///  Класс кода инструмента
    ///</summary>
    property ClassCode: String read FClassCode write FClassCode;
    ///<summary>
    ///  Код инструмента
    ///</summary>
    property SecCode: String read FSecCode write FSecCode;
    ///<summary>
    ///  Индекс в массиве заявок
    ///</summary>
    property IndexOrder: Integer read FIndexOrder write FIndexOrder;
  end;
  TEventOrderCodeList = TObjectList<TEventOrderCode>;

type
  ///<summary>
  /// Реализация API
  ///</summary>
  TTrans2Quik = class(TObject)
  private
    FEventOrdersCode: TEventOrderCodeList;
    procedure SetConnectOrdersSUBSCRIBE;
    procedure SetConnectTradesSUBSCRIBE;
  protected
    FAutoIncTransID: LongWord;
    FActiveConnect: Boolean;
    FConnectSUBSCRIBE: Boolean;
    FConnectionParams: String;
    function GetCreateTransID: LongWord;
    ///<summary>
    /// Подписываем на событие транзакции
    ///</summary>
    function GetTransactionsReplyCallback: Boolean;
    ///<summary>
    /// Отправляем сообщение
    ///</summary>
    procedure SetResultMessage(const pResultMessage: String);
    ///<summary>
    /// Устанавливаем соединение с заявками  и сделкам
    ///</summary>
    procedure SetConnectSUBSCRIBE;
    ///<summary>
    /// Разорвать соединение
    ///</summary>
    procedure SetDisConnectSUBSCRIBE;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddOrderCode(AOrderCode: TEventOrderCode): Integer;
    function IndexOfOrderCode(AOrderCode: TEventOrderCode): Integer;
    procedure DeleteOrderCode(AOrderCode: TEventOrderCode);
    ///<summary>
    /// Установить соединение
    ///</summary>
    ///<param name="AConnectionParams">
    /// Путь к торговому терминалу
    ///</param>
    ///<returns>
    /// Подписываемся на все инструменты ClassCode, SecCode
    ///</returns>
    function Connect(const AConnectionParams: String): Boolean;
    ///<summary>
    /// Разорвать соединение
    ///</summary>
    procedure DisConnect;
    //**************************************************************************
    ///<summary>
    /// Отрабатываем событие транзакции
    ///</summary>
    procedure SetTransReplyCallback(const ATransID: LongWord;
      const AOrderNo: Int64; const AMsgResult: String);
    ///<summary>
    /// Отрабатываем событие заявки
    ///</summary>
    procedure SetOrdersCallback(const ATransID: LongWord; const AOrderNo: Int64;
      const AClassCode, ASecCode: String; const ABuySell: TBuySell;
      const APrice: Double; const ABalance, AQuantity: Integer;
      const AStatus: TStatusOrder);
    ///<summary>
    /// Отрабатываем событие сделка
    ///</summary>
    procedure SetTradesCallback(const ATradeNo, AOrderNo: Int64;
      const AClassCode, ASecCode: String; const ABuySell: TBuySell;
      const APrice: Double; const AQuantity: LongInt);
    //**************************************************************************
    ///<summary>
    /// Отправляем сообщение синхронно с основным потоком
    ///</summary>
    function GetSendSyncTransaction(const ASend: String;
      var pMessageResult: String): Int64;
    ///<summary>
    /// Отправляем асинхронно сообщение, не зависимо от основного потока
    ///</summary>
    function GetSendAsyncTransaction(ASend: String): Boolean;
    //**************************************************************************
    function GetNewOrderSyncQuik(const APrice: Double; const AQuantity: Integer;
      const ATransID: LongWord; const ABuySell, ATrdaccID, AClassCode,
      ASecCode: String; var pMessageResult: String): Int64;
    function GetMoveOrderSyncQuik(const APrice: Double;
      const AQuantity: Integer; const AOrderNo: Int64; const ATransID: LongWord;
      const AClassCode, ASecCode: String; var pMessageResult: String): Int64;
    function GetMoveDoubOrderSyncQuik(const APrice1, APrice2: Double;
      const AQuantity1, AQuantity2: Integer; const AOrderNo1, AOrderNo2: Int64;
      const ATransID: LongWord; const AClassCode, ASecCode: String;
      var pMessageResult: String): Int64;
    function GetDeleteOrderSyncQuik(const AOrderNo: Int64;
      const ATransID: LongWord; const AClassCode, ASecCode: String;
      var pMessageResult: String): Int64;
    //**************************************************************************
    function GetNewOrderAsyncQuik(const APrice: Double;
      const AQuantity: Integer; const ATransID: LongWord; const ABuySell,
      ATrdaccID, AClassCode, ASecCode: String): Boolean;
    function GetMoveOrderAsyncQuik(const APrice: Double;
      const AQuantity: Integer; const AOrderNo: Int64; const ATransID: LongWord;
      const AClassCode, ASecCode: String): Boolean;
    function GetMoveDoubOrderAsyncQuik(const APrice1, APrice2: Double;
      const AQuantity1, AQuanrity2: Integer; const AOrderNo1, AOrderNo2: Int64;
      const ATransID: LongWord; const AClassCode, ASecCode: String): Boolean;
    function GetDeleteOrderAsyncQuik(const AOrderNo: Int64;
      const ATransID: LongWord; const AClassCode, ASecCode: String): Boolean;
    //**************************************************************************
    ///	<summary>
    ///	  Опеределяем состояние соединение с событиями транзакции заявок и
    ///	  сделок
    ///	</summary>
    property ActiveConnect: Boolean read FActiveConnect;
  end;

type
  ///	<summary>
  ///	  Объект организует Асинхроную работу испольнение заявку
  ///	</summary>
  TCustomAsyncOrder = class(TEventOrderCode)
  public
    ///	<summary>
    ///	  Новая заявка
    ///	</summary>
    function GetNewOrder(const APrice: Double; const AQuantity: Integer;
      const ABuySell: TBuySell): LongWord; virtual;
    ///	<summary>
    ///	  Изменение условие заявки
    ///	</summary>
    function GetMoveOrder(const APrice: Double; const AQuantity: Integer;
      const AOrderNo: Int64): LongWord; virtual;
    ///	<summary>
    ///	  Удаление условия заявки
    ///	</summary>
    function GetDeleteOrder(const AOrderNo: Int64): LongWord; virtual;
  end;

type
  ///	<summary>
  ///	  Организует синхроную работу заявок
  ///	</summary>
  TCustomSyncOrder = class(TEventOrderCode)
  public
    ///	<summary>
    ///	  Новая заявка, вслучае успеха возрощается не нулевое значение, а номер
    ///	  заявки OrderNo
    ///	</summary>
    function GetNewOrder(const APrice: Double; const AQuantity: Integer;
      const ABuySell: TBuySell; var pMessageResult: String): Int64;
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
  end;

///<summary>
/// Установить соединение
///</summary>
function GetConnectQUIK(ADirPath: String): Boolean;

function GetBuySellToStr(ABuySell: TBuySell): String;
function GetStatusOrderToStr(AStatus: TStatusOrder): String;

implementation

var
  Trans2Quik: TTrans2Quik = nil;

function GetConnectQUIK(ADirPath: String): Boolean;
begin
  if Trans2Quik.ActiveConnect then begin
    //  Соединение уже было установлено
    Result := Trans2Quik.ActiveConnect;
  end else begin
    //  Установить соединение
    Result := Trans2Quik.Connect(ADirPath);
  end;
end;

function GetBuySellToStr(ABuySell: TBuySell): String;
begin
  case ABuySell of
    bsBuy: Result := 'B';
    bsSell: Result := 'S';
  else
    Result := '';
    //raise Exception.Create('Error Message: Направление не определено');
  end;
end;

function GetStatusOrderToStr(AStatus: TStatusOrder): String;
begin
  case AStatus of
    soExecute: Result := 'Исполнилось';
    soActive: Result := 'Активна';
    soShot: Result := 'Снята';
  else
    raise Exception.Create('Error Message: Не определен статус');
  end;
end;

//******************************************************************************
//******************************************************************************
procedure SetTransactionReplyCallback(nTransactionResult: LongInt;
  nTransactionExtendedErrorCode: LongInt; nTransactionReplyCode: LongInt;
  dwTransId: DWord; dOrderNum: Double;
  lpcstrTransactionReplyMessage: LPCSTR); stdcall;
var
  MsgResult: String;
begin
  { Позрошает результат транзакции }
  try
    if Trans2Quik <> nil then begin
       MsgResult := '(' + IntToStr(dwTransId) + ')  ' +
                String(lpcstrTransactionReplyMessage);
       Trans2Quik.SetTransReplyCallback(dwTransId,Trunc(dOrderNum),MsgResult);
    end;
  except
    raise Exception.Create('Error Message'#10#13'SetTransactionReplyCallback');
  end;
end;

//******************************************************************************
//******************************************************************************

procedure SetOrdersStatusCallback(nMode: LongInt; dwTransId: DWord;
  dNumber: Double; lpstrClassCode: LPSTR; lpstrSecCode: LPSTR; dPrice: Double;
  nBalance: LongInt; dValue: Double; nIsSell: LongInt; nStatus: LongInt;
  nOrderDescriptor: LongInt); stdcall;
var
  Quantity: LongInt;
begin
  try
    if Trans2Quik <> nil then begin
       Quantity := t2qOrderQty(nOrderDescriptor);
       Trans2Quik.SetOrdersCallback(dwTransId,Trunc(dNumber),
         String(lpstrClassCode),String(lpstrSecCode),TBuySell(nIsSell + 1),
         dPrice,nBalance,Quantity,TStatusOrder(nStatus));
    end;
  except
    raise Exception.Create('Error Message'#10#13'SetOrdersStatusCallback');
  end;
end;

//******************************************************************************
//******************************************************************************

procedure SetTradesStatusCallback(nMode: LongInt; dNumber: Double;
  dOrderNum: Double; lpstrClassCode, lpstrSecCode: LPSTR; dPrice: Double;
  nQty: LongInt; dValue: Double; nIsSell: LongInt;
  nTradeDescriptor: LongInt); stdcall;
begin
  try
    if Trans2Quik <> nil then begin
       Trans2Quik.SetTradesCallback(Trunc(dNumber),Trunc(dOrderNum),
       String(lpstrClassCode),String(lpstrSecCode),TBuySell(nIsSell + 1),
       dPrice,nQty);
    end;
  except
    raise Exception.Create('Error Message'#10#13'SetTradesStatusCallback');
  end;
end;

//******************************************************************************
//******************************************************************************

{ TEventOrderCode }

constructor TEventOrderCode.Create;
begin
  FOrders := TOrderList.Create;
  FTrades := TTradeList.Create;
  Clear;
  //*******************************
  Trans2Quik.AddOrderCode(Self)
end;

destructor TEventOrderCode.Destroy;
begin
  Trans2Quik.DeleteOrderCode(Self);
  //*******************************
  Clear;
  FOrders.Free;
  FTrades.Free;
  inherited;
end;

procedure TEventOrderCode.Clear;
begin
  FOrders.Clear;
  FTrades.Clear;
end;

procedure TEventOrderCode.AddTransID(const ATransID: LongWord);
var
  xO: TOrder;
begin
  if ATransID > 0 then
    if GetIndexOfTransID(ATransID) < 0 then begin
      FillChar(xO,SizeOf(xO),0);
      xO.TransID := ATransID;
      FOrders.Add(xO);
    end;
end;

function TEventOrderCode.GetCodeOk(const AClassCode, ASecCode: String): Boolean;
begin
  Result := False;
  if (Trim(UpperCase(AClassCode)) = Trim(UpperCase(FClassCode))) and
     (Trim(UpperCase(ASecCode)) = Trim(UpperCase(FSecCode))) then
      Result := True;
end;

function TEventOrderCode.GetIndexOfTransID(const ATransID: LongWord): Integer;
var
  O: TOrder;
  i, Count: Integer;
begin
  Result := -1;
  Count := FOrders.Count;
  if Count > 0 then for i := 0 to Count - 1 do begin
    O := FOrders[i];
    if O.TransID = ATransID then begin
      Result := i;
      Break;
    end;
  end;
end;

function TEventOrderCode.GetIndexOfOrderNo(const AOrderNo: Int64): Integer;
var
  O: TOrder;
  i, Count: Integer;
begin
  Result := -1;
  Count := FOrders.Count;
  if Count > 0 then for i := 0 to Count - 1 do begin
    O := FOrders[i];
    if O.OrderNo = AOrderNo then begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TEventOrderCode.DoMessageResult(const AMessageResult: String);
begin
  if Assigned(FOnMessageResult) then FOnMessageResult(Self,AMessageResult);
end;

procedure TEventOrderCode.DoTransaction(const ATransID: LongWord;
  const AOrderNo: Int64; const AMsgResult: String);
var
  xO: TOrder;
  xInd: Integer;
begin
  xInd := GetIndexOfTransID(ATransID);
  if xInd >= 0 then begin
    //*************************
    xO := FOrders[xInd];
    xO.TransID := ATransID;
    xO.OrderNo := AOrderNo;
    FOrders[xInd] := xO;
    //*************************
    if Assigned(FOnTransaction) then
      FOnTransaction(Self,ATransID,AOrderNo,AMsgResult);
  end;
end;

procedure TEventOrderCode.DoOrders(const ATransID: LongWord; const AOrderNo: Int64;
  const AClassCode, ASecCode: String; const ABuySell: TBuySell;
  const APrice: Double; const ABalance, AQuantity: Integer;
  const AStatus: TStatusOrder);
var
  xO: TOrder;
  xInd: Integer;
begin
  xInd := GetIndexOfTransID(ATransID);
  if xInd >= 0 then begin
    //*************************
    xO := FOrders[xInd];
    xO.TransID := ATransID;
    xO.OrderNo := AOrderNo;
    xO.BuySell := ABuySell;
    xO.Price   := APrice;
    xO.Balance := ABalance;
    xO.Quantity:= AQuantity;
    xO.Status  := AStatus;
    FOrders[xInd] := xO;
    //*************************
    if Assigned(FOnOrders) then FOnOrders(Self,ATransID,AOrderNo,ABuySell,
      APrice,ABalance,AQuantity,AStatus);
  end;
end;

procedure TEventOrderCode.DoTrades(const ATradeNo, AOrderNo: Int64;
  const AClassCode, ASecCode: String; const ABuySell: TBuySell;
  const APrice: Double; const AQuantity: Integer);
var
  xT: TTrade;
  xInd: Integer;
begin
  xInd := GetIndexOfOrderNo(AOrderNo);
  if xInd >= 0 then begin
    with xT do begin
      TradeNo := ATradeNo;
      OrderNo := AOrderNo;
      BuySell := ABuySell;
      Price   := APrice;
      Quantity:= AQuantity;
    end;
    FTrades.Add(xT);
    if Assigned(FOnTrades) then FOnTrades(Self,ATradeNo,AOrderNo,ABuySell,
      APrice,AQuantity);
  end;
end;

{ TTrans2Quik }

constructor TTrans2Quik.Create;
begin
  FAutoIncTransID := 0;
  FEventOrdersCode := TEventOrderCodeList.Create;
end;

destructor TTrans2Quik.Destroy;
begin
  FEventOrdersCode.Free;
  inherited;
end;

procedure TTrans2Quik.Clear;
begin
  FEventOrdersCode.Clear;
end;

function TTrans2Quik.GetCreateTransID: LongWord;
begin
  FAutoIncTransID := FAutoIncTransID + 1;
  Result := FAutoIncTransID;
end;

function TTrans2Quik.AddOrderCode(AOrderCode: TEventOrderCode): Integer;
var
  xInd: Integer;
begin
  xInd := FEventOrdersCode.Add(AOrderCode);
  AOrderCode.IndexOrder := xInd;
  Result := xInd;
end;

function TTrans2Quik.IndexOfOrderCode(AOrderCode: TEventOrderCode): Integer;
begin
  Result := FEventOrdersCode.IndexOf(AOrderCode);
end;

procedure TTrans2Quik.DeleteOrderCode(AOrderCode: TEventOrderCode);
var
  xInd: Integer;
begin
  xInd := IndexOfOrderCode(AOrderCode);
  if xInd >= 0 then FEventOrdersCode.Delete(xInd);
end;

function TTrans2Quik.Connect(const AConnectionParams: String): Boolean;
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
     if liResult <> 0 then Self.SetResultMessage(String(acMessagError));
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
              #10#13'либо у него не запущен сервис обработки внешних ' +
              'подключений');
            Exit;
          end;
        TRANS2QUIK_DLL_VERSION_NOT_SUPPORTED:
          begin
            raise Exception.Create('Error Message' +
              #10#13'TGlobalOrder.GetConnect' + #10#13'Используемая версия ' +
              'Trans2QUIK.dll не поддерживается указанным INFO.EXE');
            Exit;
          end;
        TRANS2QUIK_ALREADY_CONNECTED_TO_QUIK:
          begin
            raise Exception.Create('Error Message' +
              #10#13'TGlobalOrder.GetConnect' +
              #10#13'соединение уже установлено');
            Exit;
          end;
        TRANS2QUIK_FAILED:
          begin
            raise Exception.Create('Error Message' +
              #10#13'TGlobalOrder.GetConnect' +
              #10#13'произошла ошибка при установлении соединения');
            Exit;
          end;
     end;
  end;
end;

procedure TTrans2Quik.DisConnect;
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

procedure TTrans2Quik.SetTransReplyCallback(const ATransID: LongWord;
  const AOrderNo: Int64; const AMsgResult: String);
var
  i, Count: Integer;
begin
  Count := FEventOrdersCode.Count;
  if Count > 0 then for i := 0 to Count - 1 do
    FEventOrdersCode[i].DoTransaction(ATransID,AOrderNo,AMsgResult);
end;

procedure TTrans2Quik.SetOrdersCallback(const ATransID: LongWord;
  const AOrderNo: Int64; const AClassCode, ASecCode: String;
  const ABuySell: TBuySell; const APrice: Double; const ABalance,
  AQuantity: Integer; const AStatus: TStatusOrder);
var
  i, Count: Integer;
begin
  Count := FEventOrdersCode.Count;
  if Count > 0 then for i := 0 to Count - 1 do
    FEventOrdersCode[i].DoOrders(ATransID,AOrderNo,AClassCode,ASecCode,ABuySell,
      APrice,ABalance,AQuantity,AStatus);
end;

procedure TTrans2Quik.SetTradesCallback(const ATradeNo, AOrderNo: Int64;
  const AClassCode, ASecCode: String; const ABuySell: TBuySell;
  const APrice: Double; const AQuantity: Integer);
var
  i, Count: Integer;
begin
  Count := FEventOrdersCode.Count;
  if Count > 0 then for i := 0 to Count - 1 do
    FEventOrdersCode[i].DoTrades(ATradeNo,AOrderNo,AClassCode,ASecCode,ABuySell,
      APrice,AQuantity);
end;

procedure TTrans2Quik.SetResultMessage(const pResultMessage: String);
begin
  // Нужно транслировать сообщение
  // Записываем
  // raise Exception.Create('Error Message: ' + pResultMessage);
end;

function TTrans2Quik.GetTransactionsReplyCallback: Boolean;
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

procedure TTrans2Quik.SetConnectSUBSCRIBE;
begin
  FConnectSUBSCRIBE := True;
  Self.SetConnectOrdersSUBSCRIBE;
  Self.SetConnectTradesSUBSCRIBE;
end;

procedure TTrans2Quik.SetConnectOrdersSUBSCRIBE;
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

procedure TTrans2Quik.SetConnectTradesSUBSCRIBE;
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

procedure TTrans2Quik.SetDisConnectSUBSCRIBE;
begin
  if FConnectSUBSCRIBE then begin
    t2qUnSubscribeOrders;
    t2qUnSubscribeTrades;
  end;
end;

function TTrans2Quik.GetSendSyncTransaction(const ASend: String;
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
          'заполнена неверно'#10#13 +
          //String(anMessagError) + #10#13 +
          String(lpstrResultMessage) + #10#13 +
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

function TTrans2Quik.GetSendAsyncTransaction(ASend: String): Boolean;
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
          'заполнена неверно'#10#13 +
          String(anMessagError));
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

//******************************************************************************
//  Отправляем заявку и получаем ответ
//******************************************************************************
function TTrans2Quik.GetNewOrderSyncQuik(const APrice: Double;
  const AQuantity: Integer; const ATransID: LongWord; const ABuySell, ATrdaccID,
  AClassCode, ASecCode: String; var pMessageResult: String): Int64;
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
          'PRICE=' + FloatToStr(APrice) + ';' +
          'QUANTITY=' + IntToStr(AQuantity) + ';';
  Result := Self.GetSendSyncTransaction(Send, pMessageResult);
end;

function TTrans2Quik.GetMoveOrderSyncQuik(const APrice: Double;
  const AQuantity: Integer; const AOrderNo: Int64; const ATransID: LongWord;
  const AClassCode, ASecCode: String; var pMessageResult: String): Int64;
var
  Send: String;
begin
  Send := 'ACTION=MOVE_ORDERS;' +
          'TRANS_ID=' + IntToStr(ATransID) + ';' +
          'CLASSCODE=' + AClassCode + ';' +
          'SECCODE=' + ASecCode + ';' +
          'MODE=1;' +
          'FIRST_ORDER_NUMBER=' + IntToStr(AOrderNo) + ';' +
          'FIRST_ORDER_NEW_PRICE=' + FloatToStr(APrice) + ';' +
          'FIRST_ORDER_NEW_QUANTITY=' + IntToStr(AQuantity) + ';' +
          'SECOND_ORDER_NUMBER=;' +
          'SECOND_ORDER_NEW_PRICE=;' +
          'SECOND_ORDER_NEW_QUANTITY=;';
  Result := GetSendSyncTransaction(Send, pMessageResult);
end;

function TTrans2Quik.GetMoveDoubOrderSyncQuik(const APrice1, APrice2: Double;
  const AQuantity1, AQuantity2: Integer; const AOrderNo1, AOrderNo2: Int64;
  const ATransID: LongWord; const AClassCode, ASecCode: String;
  var pMessageResult: String): Int64;
var
  Send: String;
begin
  Send := 'ACTION=MOVE_ORDERS;' +
          'TRANS_ID=' + IntToStr(ATransID) + ';' +
          'CLASSCODE=' + AClassCode + ';' +
          'SECCODE=' + ASecCode + ';' +
          'MODE=1;' +
          'FIRST_ORDER_NUMBER=' + IntToStr(AOrderNo1) + ';' +
          'FIRST_ORDER_NEW_PRICE=' + FloatToStr(APrice1) + ';' +
          'FIRST_ORDER_NEW_QUANTITY=' + IntToStr(AQuantity1) + ';' +
          'SECOND_ORDER_NUMBER=' + IntToStr(AOrderNo2) + ';' +
          'SECOND_ORDER_NEW_PRICE=' + FloatToStr(APrice2) + ';' +
          'SECOND_ORDER_NEW_QUANTITY=' + IntToStr(AQuantity2) + ';';
  Result := GetSendSyncTransaction(Send, pMessageResult);
end;

function TTrans2Quik.GetDeleteOrderSyncQuik(const AOrderNo: Int64;
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

//******************************************************************************
//  Отправляем заявку и не ждем ответа
//******************************************************************************
function TTrans2Quik.GetNewOrderAsyncQuik(const APrice: Double;
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
          'PRICE=' + FloatToStr(APrice) + ';' +
          'QUANTITY=' + IntToStr(AQuantity) + ';';
  Result := Self.GetSendAsyncTransaction(Send);
end;

function TTrans2Quik.GetMoveOrderAsyncQuik(const APrice: Double;
  const AQuantity: Integer; const AOrderNo: Int64; const ATransID: LongWord;
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
          'FIRST_ORDER_NEW_PRICE=' + FloatToStr(APrice) + ';' +
          'FIRST_ORDER_NEW_QUANTITY=' + IntToStr(AQuantity) + ';' +
          'SECOND_ORDER_NUMBER=;' +
          'SECOND_ORDER_NEW_PRICE=;' +
          'SECOND_ORDER_NEW_QUANTITY=;';
  Result := GetSendAsyncTransaction(Send);
end;

function TTrans2Quik.GetMoveDoubOrderAsyncQuik(const APrice1, APrice2: Double;
  const AQuantity1, AQuanrity2: Integer; const AOrderNo1, AOrderNo2: Int64;
  const ATransID: LongWord; const AClassCode, ASecCode: String): Boolean;
var
  Send: String;
begin
  Send := 'ACTION=MOVE_ORDERS;' +
          'TRANS_ID=' + IntToStr(ATransID) + ';' +
          'CLASSCODE=' + AClassCode + ';' +
          'SECCODE=' + ASecCode + ';' +
          'MODE=1;' +
          'FIRST_ORDER_NUMBER=' + IntToStr(AOrderNo1) + ';' +
          'FIRST_ORDER_NEW_PRICE=' + FloatToStr(APrice1) + ';' +
          'FIRST_ORDER_NEW_QUANTITY=' + IntToStr(AQuantity1) + ';' +
          'SECOND_ORDER_NUMBER=' + IntToStr(AOrderNo2) + ';' +
          'SECOND_ORDER_NEW_PRICE=' + FloatToStr(APrice2) + ';' +
          'SECOND_ORDER_NEW_QUANTITY=' + IntToStr(AQuanrity2) + ';';
  Result := GetSendAsyncTransaction(Send);
end;

function TTrans2Quik.GetDeleteOrderAsyncQuik(const AOrderNo: Int64;
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

{ TCustomAsyncOrder }

function TCustomAsyncOrder.GetNewOrder(const APrice: Double;
  const AQuantity: Integer; const ABuySell: TBuySell): LongWord;
var
  xResult: Boolean;
  xTransID: LongWord;
begin
  xTransID := Trans2Quik.GetCreateTransID;
  xResult := Trans2Quik.GetNewOrderAsyncQuik(APrice,AQuantity,xTransID,
    GetBuySellToStr(ABuySell),FTrdaccID,FClassCode,FSecCode);
  if xResult then begin
    Result := xTransID;
    Self.AddTransID(xTransID);
  end else begin
    Result := 0;
  end;
end;

function TCustomAsyncOrder.GetMoveOrder(const APrice: Double;
  const AQuantity: Integer; const AOrderNo: Int64): LongWord;
var
  xResult: Boolean;
  xTransID: LongWord;
begin
  xTransID := Trans2Quik.GetCreateTransID;
  xResult := Trans2Quik.GetMoveOrderAsyncQuik(APrice,AQuantity,AOrderNo,
    xTransID,FClassCode,FSecCode);
  if xResult then begin
    Result := xTransID;
    Self.AddTransID(xTransID);
  end else begin
    Result := 0;
  end;
end;

function TCustomAsyncOrder.GetDeleteOrder(const AOrderNo: Int64): LongWord;
var
  xResult: Boolean;
  xTransID: LongWord;
begin
  xTransID := Trans2Quik.GetCreateTransID;
  xResult := Trans2Quik.GetDeleteOrderAsyncQuik(AOrderNo,xTransID,
    FClassCode,FSecCode);
  if xResult then begin
    Result := xTransID;
    Self.AddTransID(xTransID);
  end else begin
    Result := 0;
  end;
end;

{ TCustomSyncOrder }

function TCustomSyncOrder.GetNewOrder(const APrice: Double;
  const AQuantity: Integer; const ABuySell: TBuySell;
  var pMessageResult: String): Int64;
var
  xTransID: LongWord;
begin
  xTransID := Trans2Quik.GetCreateTransID;
  Result := Trans2Quik.GetNewOrderSyncQuik(APrice,AQuantity,xTransID,
    GetBuySellToStr(ABuySell),FTrdaccID,FClassCode,FSecCode,pMessageResult);
  if Result > 0 then Self.AddTransID(xTransID);
end;

function TCustomSyncOrder.GetMoveOrder(const APrice: Double;
  const AQuantity: Integer; const AOrderNo: Int64;
  var pMessageResult: String): Int64;
var
  xTransID: LongWord;
begin
  xTransID := Trans2Quik.GetCreateTransID;
  Result := Trans2Quik.GetMoveOrderSyncQuik(APrice,AQuantity,AOrderNo,xTransID,
    FClassCode,FSecCode,pMessageResult);
  if Result > 0 then Self.AddTransID(xTransID);
end;

function TCustomSyncOrder.GetDeleteOrder(const AOrderNo: Int64;
  var pMessageResult: String): Int64;
var
  xTransID: LongWord;
begin
  xTransID := Trans2Quik.GetCreateTransID;
  Result := Trans2Quik.GetDeleteOrderSyncQuik(AOrderNo,xTransID,
    FClassCode,FSecCode,pMessageResult);
  if Result > 0 then Self.AddTransID(xTransID);
end;

initialization
  Trans2Quik := TTrans2Quik.Create;

finalization
  Trans2Quik.Free;


end.
