unit QuikTrans2QuikAPI;

(*******************************************************************************
    Данный модуль является обверткой для библиотеке trans2quik.dll
    Даная библиотека перднозначена для осуществление торговых операций
  вторговом терминале QUIK}
*******************************************************************************)
interface

uses
  Winapi.Windows,
  System.SysUtils;

const
  TRANS2QUIK_SUCCESS = 0;
  TRANS2QUIK_FAILED = 1;
  TRANS2QUIK_QUIK_TERMINAL_NOT_FOUND = 2;
  TRANS2QUIK_DLL_VERSION_NOT_SUPPORTED = 3;
  TRANS2QUIK_ALREADY_CONNECTED_TO_QUIK = 4;
  TRANS2QUIK_WRONG_SYNTAX = 5;
  TRANS2QUIK_QUIK_NOT_CONNECTED = 6;
  TRANS2QUIK_DLL_NOT_CONNECTED = 7;
  TRANS2QUIK_QUIK_CONNECTED = 8;
  TRANS2QUIK_QUIK_DISCONNECTED = 9;
  TRANS2QUIK_DLL_CONNECTED = 10;
  TRANS2QUIK_DLL_DISCONNECTED = 11;
  TRANS2QUIK_MEMORY_ALLOCATION_ERROR = 12;
  TRANS2QUIK_WRONG_CONNECTION_HANDLE = 13;
  TRANS2QUIK_WRONG_INPUT_PARAMS = 14;

(******************************************************************************)

type
  TRANS2QUIK_CONNECTION_STATUS_CALLBACK = procedure(nConnectionEvent: LongInt;
    nExtendedErrorCode: LongInt; lpcstrInfoMessage: LPCSTR); stdcall;
  ///	<summary>
  ///	  Функции обратного вызова для контроля за состоянием соединения между
  ///	  Trans2QUIK.dll и используемым терминалом QUIK и между используемым
  ///	  терминалом QUIK и сервером
  ///	</summary>
  TConnectionStatusCallback = TRANS2QUIK_CONNECTION_STATUS_CALLBACK;

type
  TRANS2QUIK_TRANSACTION_REPLY_CALLBACK = procedure(nTransactionResult: LongInt;
    nTransactionExtendedErrorCode: LongInt; nTransactionReplyCode: LongInt;
    dwTransId: DWORD; dOrderNum: Double;
    lpcstrTransactionReplyMessage: LPCSTR); stdcall;
  ///	<summary>
  ///	  Функции обратного вызова для обработки полученной информации об
  ///	  отправленной транзакции
  ///	</summary>
  TTransactionReplyCallback = TRANS2QUIK_TRANSACTION_REPLY_CALLBACK;

type
  TRANS2QUIK_ORDER_STATUS_CALLBACK = procedure(nMode: LongInt; dwTransId: DWORD;
    dNumber: Double; lpstrClassCode: LPSTR; lpstrSecCode: LPSTR; dPrice: Double;
    nBalance: LongInt; dValue: Double; nIsSell: LongInt; nStatus: LongInt;
    nOrderDescriptor: LongInt); stdcall;
  ///	<summary>
  ///	  Функции обратного вызова для обработки полученной информации по заявке
  ///	</summary>
  TOrderStatusCallback = TRANS2QUIK_ORDER_STATUS_CALLBACK;

type
  TRANS2QUIK_TRADE_STATUS_CALLBACK = procedure(nMode: LongInt; dNumber: Double;
    dOrderNum: Double; lpstrClassCode, lpstrSecCode: LPSTR; dPrice: Double;
    nQty: LongInt; dValue: Double; nIsSell: LongInt;
    nTradeDescriptor: LongInt); stdcall;
  ///	<summary>
  ///	  Функции обратного вызова для обработки полученной информации по сделке
  ///	</summary>
  TTradesStatusCallback = TRANS2QUIK_TRADE_STATUS_CALLBACK;

(******************************************************************************)
type
///	<summary>
///	  Установление связи библиотеки Trans2QUIK.dll с Рабочим местом QUIK
///	</summary>
///	<param name="lpcstrConnectionParamsString">
///	  Полный путь к каталогу с исполняемым файлом INFO.EXE, с которым
///	  устанавливается соединение
///	</param>
///	<param name="pnExtendedErrorCode">
///	  В случае возникновения ошибки может содержать расширенный код ошибки
///	</param>
///	<param name="lpstrErrorMessage">
///	  В случае возникновения ошибки может получать сообщение о возникшей ошибке
///	</param>
///	<param name="dwErrorMessageSize">
///	  Содержит длину строки, на которую ссылается указатель lpstrErrorMessage
///	</param>
  TLibaryConnect = function(lpcstrConnectionParamsString: LPCSTR;
    var pnExtendedErrorCode: LongInt; lpstrErrorMessage: LPSTR;
    dwErrorMessageSize: DWORD): LongInt; stdcall;

///	<summary>
///	  Разрыв соединения библиотеки Trans2QUIK.dll с Рабочим местом QUIK
///	</summary>
///	<param name="pnExtendedErrorCode">
///	  В случае возникновения ошибки может содержать расширенный код ошибки
///	</param>
///	<param name="lpstrErrorMessage">
///	  В случае возникновения ошибки может получать сообщение о возникшей ошибке
///	</param>
///	<param name="dwErrorMessageSize">
///	  Содержит длину строки, на которую ссылается указатель lpstrErrorMessage
///	</param>
  TLibaryDisConnect = function(var pnExtendedErrorCode: LongInt;
    lpstrErrorMessage: LPSTR; dwErrorMessageSize: DWORD): LongInt; stdcall;

///	<summary>
///	  Проверка соединения библиотеки Trans2QUIK.dll с Рабочим местом QUIK
///	</summary>
///	<param name="pnExtendedErrorCode">
///	  В случае возникновения ошибки может содержать расширенный код ошибки
///	</param>
///	<param name="lpstrErrorMessage">
///	  В случае возникновения ошибки может получать сообщение о возникшей ошибке
///	</param>
///	<param name="dwErrorMessageSize">
///	  Содержит длину строки, на которую ссылается указатель lpstrErrorMessage
///	</param>
  TLibaryIsQuikConnected = function(var pnExtendedErrorCode: LongInt;
    lpstrErrorMessage: LPSTR; dwErrorMessageSize: DWORD): LongInt; stdcall;

///	<summary>
///	  Проверка соединения библиотеки Trans2QUIK.dll с Рабочим местом QUIK
///	</summary>
///	<param name="pnExtendedErrorCode">
///	  В случае возникновения ошибки может содержать расширенный код ошибки
///	</param>
///	<param name="lpstrErrorMessage">
///	  В случае возникновения ошибки может получать сообщение о возникшей ошибке
///	</param>
///	<param name="dwErrorMessageSize">
///	  Содержит длину строки, на которую ссылается указатель lpstrErrorMessage
///	</param>
  TLibaryIsDllConnected = function(var pnExtendedErrorCode: LongInt;
    lpstrErrorMessage: LPSTR; dwErrorMessageSize: DWORD): LongInt; stdcall;

///	<summary>
///	  Отправка синхронной транзакции
///	</summary>
///	<param name="lpstTransactionString">
///	  Строка с описанием транзакции. Формат строки тот же самый, что и при
///	  отправке транзакций через файл
///	</param>
///	<param name="pnReplyCode">
///	  Получает статус выполнения транзакции
///	</param>
///	<param name="pdwTransId">
///	  Получает значение TransID транзакции, указанной пользователем
///	</param>
///	<param name="pdOrderNum">
///	  В случае успеха получает номер заявки в торговой системе
///	</param>
///	<param name="lpstrResultMessage">
///	  В случае успеха содержит сообщение торговой системы
///	</param>
///	<param name="dwResultMessageSize">
///	  Содержит длину строки, на которую ссылается указатель lpstrResultMessage
///	</param>
///	<param name="pnExtendedErrorCode">
///	  В случае возникновения ошибки может содержать расширенный код ошибки
///	</param>
///	<param name="lpstErrorMessage">
///	  В случае возникновения ошибки может получать сообщение о возникшей ошибке
///	</param>
///	<param name="dwErrorMessageSize">
///	  Содержит длину строки, на которую ссылается указатель lpstrErrorMessage
///	</param>
  TLibarySendSyncTransaction = function(lpstTransactionString: LPSTR;
    var pnReplyCode: LongInt; var pdwTransId: DWORD; var pdOrderNum: Double;
    lpstrResultMessage: LPSTR; dwResultMessageSize: DWORD;
    var pnExtendedErrorCode: LongInt; lpstErrorMessage: LPSTR;
    dwErrorMessageSize: DWORD): LongInt; stdcall;

///	<summary>
///	  Отправка асинхронной транзакции
///	</summary>
///	<param name="lpstTransactionString">
///	  Строка с описанием транзакции
///	</param>
///	<param name="pnExtendedErrorCode">
///	  В случае возникновения ошибки может содержать расширенный код ошибки
///	</param>
///	<param name="lpstErrorMessage">
///	  В случае возникновения ошибки может получать сообщение о возникшей ошибке
///	</param>
///	<param name="dwErrorMessageSize">
///	  Содержит длину строки, на которую ссылается указатель lpstrErrorMessage
///	</param>
  TLibarySendAsyncTransaction = function(lpstTransactionString: LPSTR;
    pnExtendedErrorCode: LongInt; lpstErrorMessage: LPSTR;
    dwErrorMessageSize: DWORD): LongInt; stdcall;

///	<summary>
///	  Функции обратного вызова для обработки полученной информации о соединении
///	</summary>
///	<param name="pfConnectionStatusCallback">
///	  Функции обратного вызова для контроля за состоянием соединения между
///	  Trans2QUIK.dll и используемым терминалом QUIK и между используемым
///	  терминалом QUIK и сервером
///	</param>
///	<param name="pnExtendedErrorCode">
///	  В случае возникновения ошибки может содержать расширенный код ошибки
///	</param>
///	<param name="lpstrErrorMessage">
///	  В случае возникновения ошибки может получать сообщение о возникшей ошибке
///	</param>
///	<param name="dwErrorMessageSize">
///	  Содержит длину строки, на которую ссылается указатель lpstrErrorMessage
///	</param>
  TLibarySetConnectionStatusCallbak = function(pfConnectionStatusCallback:
    TConnectionStatusCallback; var pnExtendedErrorCode: LongInt;
    lpstrErrorMessage: LPSTR; dwErrorMessageSize: DWORD): LongInt; stdcall;

///	<summary>
///	  Функция устанавливает функцию обратного вызова
///	  TRANS2QUIK_TRANSACTIONS_REPLY_CALLBACK
///	</summary>
///	<param name="pfTransactionReplyCallback">
///	  Функции обратного вызова для обработки полученной информации об
///	  отправленной транзакции
///	</param>
///	<param name="pnExtendedErrorCode">
///	  В случае возникновения ошибки может содержать расширенный код ошибки
///	</param>
///	<param name="lpstrErrorMessage">
///	  В случае возникновения ошибки может получать сообщение о возникшей ошибке
///	</param>
///	<param name="dwErrorMessageSize">
///	  Содержит длину строки, на которую ссылается указатель lpstrErrorMessage
///	</param>
  TLibarySetTransactionsReplyCallback = function(pfTransactionReplyCallback:
    TTransactionReplyCallback; var pnExtendedErrorCode: LongInt;
    lpstrErrorMessage: LPSTR; dwErrorMessageSize: DWORD): LongInt; stdcall;

///	<summary>
///	  Функция создания списка инструментов по классам, по которому будут
///	  приниматься заявки
///	</summary>
///	<param name="lpstrClassCode">
///	  Код класса, для которого будут заказаны заявки, если в качестве обоих
///	  входных параметров указаны пустые строки, то это означает, что заказано
///	  получение заявок по всем доступным инструментам
///	</param>
///	<param name="lpstrSeccodes">
///	  Список кодов бумаг, разделенных символом «|», по которым будут заказаны
///	  заявки. Если в качестве значения указана пустая строка, то это означает,
///	  что заказано получение заявок по классу, указанному в параметре
///	  lpstrClassCode
///	</param>
  TLibarySubscribeOrders = function(lpstrClassCode: LPSTR;
    lpstrSeccodes: LPSTR): LongInt; stdcall;

///	<summary>
///	  Функция создания списка инструментов по классам, по которому будут
///	  приниматься сделки
///	</summary>
///	<param name="lpstrClassCode">
///	  Код класса, для которого будут заказаны сделки, если в качестве обоих
///	  входных параметров указаны пустые строки, то это означает, что заказано
///	  получение сделок по всем доступным инструментам
///	</param>
///	<param name="lpstrSeccodes">
///	  Список кодов бумаг, разделенных символом «|», по которым будут заказаны
///	  сделки. Если в качестве значения указана пустая строка, то это означает,
///	  что заказано получение сделок по классу, указанному в параметре
///	  lpstrClassCode
///	</param>
  TLibarySubscribeTrades = function(lpstrClassCode: LPSTR;
    lpstrSeccodes: LPSTR): LongInt; stdcall;

///	<summary>
///	  Вызов функции инициирует процесс получения заявок по инструментам, список
///	  которых был сформирован предварительным вызовом функции
///	  TRANS2QUIK_SUBSCRIBE_ORDERS
///	</summary>
///	<param name="pfnOrderStatusCallback">
///	  Функции обратного вызова для обработки полученной информации по заявке
///	</param>
  TLibaryStartOrders = procedure(pfnOrderStatusCallback: TOrderStatusCallback); stdcall;

{ Вызов функции инициирует процесс получения сделок по инструментам, список
    которых был сформирован предварительным вызовом функции
    TRANS2QUIK_SUBSCRIBE_TRADES }

///	<summary>
///	  Вызов функции инициирует процесс получения сделок по инструментам, список
///	  которых был сформирован предварительным вызовом функции
///	  TRANS2QUIK_SUBSCRIBE_TRADES
///	</summary>
///	<param name="pfnTradesStatusCallback">
///	  Функции обратного вызова для обработки полученной информации по сделке
///	</param>
  TLibaryStartTrades = procedure(pfnTradesStatusCallback:
    TTradesStatusCallback); stdcall;

///	<summary>
///	  Вызов функции прерывает работу функции TRANS2QUIK_START_ORDERS и
///	  производит очистку списка получаемых инструментов, сформированного
///	  функцией TRANS2QUIK_SUBSCRIBE_ORDERS
///	</summary>
  TLibaryUnSubscribeOrders = function(): LongInt; stdcall;

///	<summary>
///	  Вызов функции прерывает работу функции TRANS2QUIK_START_TRADES и
///	  производит очистку списка получаемых инструментов, сформированного
///	  функцией TRANS2QUIK_SUBSCRIBE_TRADES
///	</summary>
  TLibaryUnSubscribeTrades = function(): LongInt; stdcall;

///	<summary>
///	  Возвращает количество заявки
///	</summary>
///	<param name="nOrderDescriptor">
///	  Дескриптор заявки
///	</param>
  TLibaryOrderQty = function(nOrderDescriptor: LongInt): LongInt; stdcall;

var
  t2qConnect                      : TLibaryConnect;
  t2qDisConnect                   : TLibaryDisConnect;
  t2qIsQuikConnected              : TLibaryIsQuikConnected;
  t2qIsDllConnected               : TLibaryIsDllConnected;
  t2qSendSyncTransaction          : TLibarySendSyncTransaction;
  t2qSendAsyncTransaction         : TLibarySendAsyncTransaction;
  t2qSetConnectionStatusCallbak   : TLibarySetConnectionStatusCallbak;
  t2qSetTransactionsReplyCallback : TLibarySetTransactionsReplyCallback;
  t2qSubscribeOrders              : TLibarySubscribeOrders;
  t2qSubscribeTrades              : TLibarySubscribeTrades;
  t2qStartOrders                  : TLibaryStartOrders;
  t2qStartTrades                  : TLibaryStartTrades;
  t2qUnSubscribeOrders            : TLibaryUnSubscribeOrders;
  t2qUnSubscribeTrades            : TLibaryUnSubscribeTrades;
  t2qOrderQty                     : TLibaryOrderQty;

implementation

uses
  System.Classes, BTMemoryModule;

{$R trans2quik.res}

var
  LibaryModule: PBTMemoryModule = nil;
  LibarySize: Int64 = 0;
  LibaryData: Pointer;

procedure SetInitializationLibary;
var
  sErorr: String;
  rsMemoryStream: TResourceStream;
begin

  rsMemoryStream := TResourceStream.Create(HInstance,'trans2quik',RT_RCDATA);
  try
    LibarySize := rsMemoryStream.Size;
    LibaryData := GetMemory(LibarySize);

    rsMemoryStream.Position := 0;
    rsMemoryStream.Read(LibaryData^,LibarySize);

    LibaryModule := BTMemoryLoadLibary(LibaryData,LibarySize);
    if Assigned(LibaryModule) then begin
      try
        @t2qConnect :=
          BTMemoryGetProcAddress(LibaryModule,'_TRANS2QUIK_CONNECT@16');
        @t2qDisConnect :=
          BTMemoryGetProcAddress(LibaryModule,'_TRANS2QUIK_DISCONNECT@12');
        @t2qIsQuikConnected :=
          BTMemoryGetProcAddress(LibaryModule,'_TRANS2QUIK_IS_QUIK_CONNECTED@12');
        @t2qIsDllConnected :=
          BTMemoryGetProcAddress(LibaryModule,'_TRANS2QUIK_IS_DLL_CONNECTED@12');
        @t2qSendSyncTransaction :=
          BTMemoryGetProcAddress(LibaryModule,'_TRANS2QUIK_SEND_SYNC_TRANSACTION@36');
        @t2qSendAsyncTransaction :=
          BTMemoryGetProcAddress(LibaryModule,'_TRANS2QUIK_SEND_ASYNC_TRANSACTION@16');
        @t2qSetConnectionStatusCallbak :=
          BTMemoryGetProcAddress(LibaryModule,'_TRANS2QUIK_SET_CONNECTION_STATUS_CALLBACK@16');
        @t2qSetTransactionsReplyCallback :=
          BTMemoryGetProcAddress(LibaryModule,'_TRANS2QUIK_SET_TRANSACTIONS_REPLY_CALLBACK@16');
        @t2qSubscribeOrders :=
          BTMemoryGetProcAddress(LibaryModule,'_TRANS2QUIK_SUBSCRIBE_ORDERS@8');
        @t2qSubscribeTrades :=
          BTMemoryGetProcAddress(LibaryModule,'_TRANS2QUIK_SUBSCRIBE_TRADES@8');
        @t2qStartOrders :=
          BTMemoryGetProcAddress(LibaryModule,'_TRANS2QUIK_START_ORDERS@4');
        @t2qStartTrades :=
          BTMemoryGetProcAddress(LibaryModule,'_TRANS2QUIK_START_TRADES@4');
        @t2qUnSubscribeOrders :=
          BTMemoryGetProcAddress(LibaryModule,'_TRANS2QUIK_UNSUBSCRIBE_ORDERS@0');
        @t2qUnSubscribeTrades :=
          BTMemoryGetProcAddress(LibaryModule,'_TRANS2QUIK_UNSUBSCRIBE_TRADES@0');
        @t2qOrderQty :=
          BTMemoryGetProcAddress(LibaryModule,'_TRANS2QUIK_ORDER_QTY@4');
      except
        sErorr := BTMemoryGetLastError;
        raise Exception.Create('Error Message'#13#10 +
                               'SetInitializationLibary'#13#10 +
                               sErorr);
      end;
    end else begin
      sErorr := BTMemoryGetLastError;
      raise Exception.Create('Error Message'#13#10 +
                             'SetInitializationLibary'#13#10 +
                             sErorr);
    end;

  finally
    rsMemoryStream.Free;
  end;
end;

procedure SetFinalizationLibary;
begin
  if LibarySize > 0 then FreeMemory(LibaryData);
  if LibaryModule <> nil then BTMemoryFreeLibrary(LibaryModule);
end;

initialization
  SetInitializationLibary;

finalization
  SetFinalizationLibary;

end.
