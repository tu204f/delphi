unit Lb.Bybit.SysUtils;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,
  System.JSON,
  Lb.Bybit.Encryption;

const
{$DEFINE TEST}
  BYBIT_HOST =
{$IFDEF TEST}
  'https://api-testnet.bybit.com';
{$ELSE}
  // Тесторый сервер: https://testnet.bybit.com
  'https://api.bybit.com';
{$ENDIF}

const
  ERROR_CODE        = 10000;
  ERROR_CODE_HTTP   = ERROR_CODE + 1;
  // Код ошибка парсинга
  ERROR_CODE_PARSER = ERROR_CODE + 2;

type
  TTypeCategory = (
    tcSpot,
    tcLinear,  // Бессрочный USDT и контракт USDC, включая USDC perp, фьючерсы USDC
    tcInverse, // Обратный контракт, включая обратного преступника, обратные фьючерсы
    tcOption
  );


  TTypeSide = (
    tsBuy,
    tsSell
  );

  TTypeOrder = (
    Market,
    Limit
  );

  TTypeOrderFilter = (
    Order,
    tpslOrder,
    StopOrder
  );

  TTypeTriggerBy = (
    LastPrice,
    IndexPrice,
    MarkPrice
  );

  ///<summary>Выбор действующего времени (GTC, IOC, FOK)</summary>
  TTypeTimeInForce = (
    ///<summary>
    /// Действует до отмены (GTC): Ордер будет оставаться действительным
    /// до тех пор, пока он не будет полностью исполнен или отменен вручную
    /// трейдером. GTC подходит для трейдеров, которые готовы ждать завершения
    /// всех контрактов по указанной цене и могут гибко отменять незаключенные
    /// контракты в любое время.
    ///</summary>
    GTC, // GoodTillCancel
    ///<summary>
    /// Немедленно или Отменить (IOC): Заказ должен быть выполнен немедленно
    /// по предельной цене заказа или выше. Если заказ не может быть выполнен
    /// немедленно, незаполненные контракты будут аннулированы. IOC обычно
    /// используется для предотвращения исполнения крупных ордеров по цене,
    /// отличающейся от идеальной. С этим набором контракты, которые не
    /// торгуются по указанной цене, будут аннулированы.
    ///</summary>
    IOC, // ImmediateOrCancel
    ///<summary>
    /// Заполнить или уничтожить (FOK): Заказ должен быть немедленно выполнен
    /// по цене заказа или выше, в противном случае он будет полностью
    /// отменен, а частично заполненные контракты не будут допущены.
    /// Эта стратегия исполнения чаще используется трейдерами-скальперами
    /// или дейтрейдерами, ищущими краткосрочные рыночные возможности.
    ///</summary>
    FOK, // FillOrKill
    PostOnly
  );

  TTypeSmpType = (
    Default,
    CancelMaker,
    CancelTaker,
    CancelBoth
  );

  ///<summary>TP/SL mode</summary>
  TTypeTpSlMode = (
    ///<summary>
    /// вся позиция для TP/SL. Тогда тип ордера tp или
    /// slOrderType должен быть рыночным
    ///</summary>
    Full,
    ///<summary>
    /// частичное расположение tp/sl. Поддерживается порядок ограничения TP/SL.
    /// Примечание: При создании ограничения tp/sl требуется режим TPSL, и
    /// он должен быть частичным
    ///</summary>
    Partial
  );

  TTypeStatus = (
    tsPreLaunch,
    tsTrading,
    tsSettling, // The unique status for USDC Perpetual 8-hour settlement
    tsDelivering,
    tsClosed
  );

  TTypeInterval = (
    ti_1, ti_3, ti_5, ti_15, ti_30, ti_60, ti_120, ti_240, ti_360, ti_720, //  minute
    ti_D, // day
    ti_W, // week
    ti_M  // month
  );

type
  THeader = TNameValuePair;
  THeaderList = class(TList<THeader>)
  private
    function GetValues(AName: String): String;
    procedure SetValues(AName: String; const AValue: String);
  protected
    function GetIndexOfName(const AName: String): Integer;
  public
    property Values[AName: String]: String read GetValues write SetValues;
  end;

  ///<summary>Событие</summary>
  TEventMessage = procedure(ASender: TObject; AMessage: String) of object;
  TEventOnStatus = procedure(ASender: TObject; Active: Boolean) of object;

  TParam = TPair<String,String>;
  TParamList = class(TList<TParam>)
  public
    function IndexOfKey(const ANameKey: String): Integer;
    procedure SetParam(const AKey, AValue: String);
  end;

(******************************************************************************)
(* Базовый класс, для всех запросов
(******************************************************************************)
  TBybitHttpClientAPI = class;
  TBytiyResponse = class;
  TBybitModule = class;
  TTypeHttp = (thNull, thGet, thPost);

  ///<summary>
  /// Отвечает за отправление запроса на сервер Bybit
  ///</summary>
  ///<remarks>
  /// Запросы все отправляются в потоке и с получением ответа от сервера
  /// Также можно задавать интервал запроса
  ///</remarks>
  TBybitHttpClient = class(TObject)
  private
    FActive: Boolean;
    FSource: TStrings;
    FValueMessage: String;
    FStatusCode: Integer;
    FInterval: Integer;        // Интервал обновления
    FTask: ITask;              // Задача - указатель на поток
    FBybitModule: TBybitModule;
    FResponse: TBytiyResponse;
  protected
    FOnEventMessage: TNotifyEvent;
    FOnEventException: TNotifyEvent;
    FOnEventBeginLoading: TNotifyEvent;
    FOnEventEndLoading: TNotifyEvent;
    procedure DoEventParser; virtual;
    procedure DoEventBeginLoading; virtual;
    procedure DoEventEndLoading; virtual;
    procedure DoEventMessage(const AMessage: String); virtual;
    procedure DoEventException(const AStatusCode: Integer; const AMessage: String);
  protected
    // Запрос выполняются в отделом потоке с ожидание ответа
    procedure SetThreading;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>Отправляем один запрос</summary>
    procedure Selected;
    ///<summary>Зациклить запрос</summary>
    procedure Start(const AInterval: Integer = 0);
    ///<summary>Остановить цикл</summary>
    procedure Stop;
    ///<summary>Параметры запроса</summary>
    property BybitModule: TBybitModule read FBybitModule;
    property Interval: Integer read FInterval;
    property ValueMessage: String read FValueMessage;
    property StatusCode: Integer read FStatusCode;
    property Source: TStrings read FSource;
    property Response: TBytiyResponse read FResponse;
    property Active: Boolean read FActive;
  public
    ///<summary>Событие ответа от сервера</summary>
    property OnEventMessage: TNotifyEvent write FOnEventMessage;
    property OnEventException: TNotifyEvent write FOnEventException;
    property OnEventBeginLoading: TNotifyEvent write FOnEventBeginLoading;
    property OnEventEndLoading: TNotifyEvent write FOnEventEndLoading;
  end;

  ///<summary>Ответ сервер</summary>
  TBytiyResponse = class(TObject)
  private
    FretCode: Integer;
    FretMsg: String;
    FretTime: Double;
    FResultObject: TJSONObject;
    FExtInfoObject: TJSONObject;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetParserValue(const AValue: String);
  public
    property ResultObject : TJSONObject read FResultObject;
    property ExtInfoObject: TJSONObject read FExtInfoObject;
    property RetCode: Integer read FretCode;
    property RetMsg: String read FretMsg;
    property RetTime: Double read FretTime;
  end;

  ///<summary>Структура запрашиваемого модуля</summary>
  TBybitModule = class(TObject)
  private
    FTypeHttp: TTypeHttp;
    FHost: String;
    FModule: String;
    FParams: TParamList;
    FHeaders: THeaderList;
    function GetQuery: String;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetURL: String;
    property TypeHttp: TTypeHttp read FTypeHttp write FTypeHttp;
    property Host: String read FHost write FHost;
    property Module: String read FModule write FModule;
    property Params: TParamList read FParams;
    property Headers: THeaderList read FHeaders;
    property Query: String read GetQuery;
  end;

(******************************************************************************)
(* Отрабатываем запрос — в не потока                                          *)
(******************************************************************************)
  TBybitHttpClientAPI = class(TObject)
  private
    FResponseValue: String;
    FSource: TStrings;
    FBybitModule: TBybitModule;
    FClient: TNetHTTPClient;
  private
    FStatusCode: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Selected;
    property BybitModule: TBybitModule write FBybitModule;
    property Source: TStrings read FSource;
    property ResponseValue: String read FResponseValue;
    property StatusCode: Integer read FStatusCode;
  end;

function GetStrToTypeCategory(ACategory: TTypeCategory): String;
function GetStrToTypeSide(const ASide: TTypeSide): String;
function GetStrToTypeOrder(const ATypeOrder: TTypeOrder): String;
function GetStrToTypeOrderFilter(const AOrderFilter: TTypeOrderFilter): String;
function GetStrToTypeTriggerBy(const ATriggerBy: TTypeTriggerBy): String;
function GetStrToTypeTimeInForce(const ATimeInForce: TTypeTimeInForce): String;
function GetStrToTypeSmpType(const ASmpType: TTypeSmpType): String;
function GetStrToTypeTpSlMode(const ATpSlMode: TTypeTpSlMode): String;
function GetStrToTypeStatus(AStatus: TTypeStatus): String;
function GetStrToTypeInterval(AInterval: TTypeInterval): String;

type
  ///<summary>Для всех Json – объектов</summary>
  TCustonObjectJson = class(TObject)
  private
    FObjectJson: TJSONObject;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetObjectJson(const AObjectJson: TJSONObject); virtual;
  end;

implementation

uses
  System.IniFiles,
  System.RTTI;

function GetStrToTypeCategory(ACategory: TTypeCategory): String;
begin
  case ACategory of
    tcSpot:    Result := 'spot';
    tcLinear:  Result := 'linear';
    tcInverse: Result := 'inverse';
    tcOption:  Result := 'option';
  else
    Result := '';
  end;
end;

function GetStrToTypeSide(const ASide: TTypeSide): String;
begin
  case ASide of
    tsBuy: Result := 'Buy';
    tsSell: Result := 'Sell';
  else
    Result := '';
  end;
end;


function GetStrToTypeOrder(const ATypeOrder: TTypeOrder): String;
begin
  case ATypeOrder of
    Market: Result := 'Market';
    Limit: Result := 'Limit';
  else
    Result := '';
  end;
end;

function GetStrToTypeOrderFilter(const AOrderFilter: TTypeOrderFilter): String;
begin
  case AOrderFilter of
    Order: Result := 'Order';
    tpslOrder: Result := 'tpslOrder';
    StopOrder: Result := 'StopOrder';
  else
    Result := '';
  end;
end;

function GetStrToTypeTriggerBy(const ATriggerBy: TTypeTriggerBy): String;
begin
  case ATriggerBy of
    LastPrice: Result := 'LastPrice';
    IndexPrice: Result := 'IndexPrice';
    MarkPrice: Result := 'MarkPrice';
  else
    Result := '';
  end;
end;

function GetStrToTypeTimeInForce(const ATimeInForce: TTypeTimeInForce): String;
begin
  case ATimeInForce of
    GTC     : Result := 'GTC';
    IOC     : Result := 'IOC';
    FOK     : Result := 'FOK';
    PostOnly: Result := 'PostOnly';
  else
    raise Exception.Create('Error Message: кроче все КЮ');
  end;
end;

function GetStrToTypeSmpType(const ASmpType: TTypeSmpType): String;
begin
  case ASmpType of
    Default: Result := 'default';
    CancelMaker: Result := 'CancelMaker';
    CancelTaker: Result := 'CancelTaker';
    CancelBoth: Result := 'CancelBoth';
  end;
end;

function GetStrToTypeTpSlMode(const ATpSlMode: TTypeTpSlMode): String;
begin
  case ATpSlMode of
    Full: Result := 'Full';
    Partial: Result := 'Partial';
  end;
end;

function GetStrToTypeStatus(AStatus: TTypeStatus): String;
begin
  case AStatus of
    tsPreLaunch:  Result := 'PreLaunch';
    tsTrading:    Result := 'Trading';
    tsSettling:   Result := 'Settling';
    tsDelivering: Result := 'Delivering';
    tsClosed:     Result := 'Closed';
  else
    Result := '';
  end;
end;

function GetStrToTypeInterval(AInterval: TTypeInterval): String;
begin
  case AInterval of
    ti_1  : Result := '1';
    ti_3  : Result := '3';
    ti_5  : Result := '5';
    ti_15 : Result := '15';
    ti_30 : Result := '30';
    ti_60 : Result := '60';
    ti_120: Result := '120';
    ti_240: Result := '240';
    ti_360: Result := '360';
    ti_720: Result := '720';
    ti_D  : Result := 'D';
    ti_W  : Result := 'W';
    ti_M  : Result := 'M';
  else
    Result := '';
  end;
end;

{ THeaderList }

function THeaderList.GetValues(AName: String): String;
var
  xIndex: Integer;
begin
  Result := '';
  xIndex := GetIndexOfName(AName);
  if xIndex >= 0 then
    Result := Self.Items[xIndex].Value;
end;

procedure THeaderList.SetValues(AName: String; const AValue: String);
var
  xIndex: Integer;
  xHeader: THeader;
begin
  xIndex := GetIndexOfName(AName);
  if xIndex >= 0 then
  begin
    xHeader := Self.Items[xIndex];
    xHeader.Value := AValue;
  end
  else
  begin
    xHeader := THeader.Create(AName,AValue);
    Self.Add(xHeader);
  end;
end;

function THeaderList.GetIndexOfName(const AName: String): Integer;
var
  i: Integer;
  xHeader: THeader;
begin
  Result := -1;
  if Self.Count > 0 then
    for i := 0 to Self.Count - 1 do
    begin
      xHeader := Self.Items[i];
      if SameText(xHeader.Name,AName) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

{ TParamList }

function TParamList.IndexOfKey(const ANameKey: String): Integer;
var
  i: Integer;
  xParam: TParam;
begin
  Result := -1;
  for i := 0 to Self.Count - 1 do
  begin
    xParam := Items[i];
    if SameText(xParam.Key,ANameKey) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TParamList.SetParam(const AKey, AValue: String);
var
  xParam: TParam;
  xIndexKey: Integer;
begin
  xIndexKey := IndexOfKey(AKey);
  if xIndexKey >= 0 then
  begin
    xParam := Self.Items[xIndexKey];
    xParam.Value := AValue;
    Self.Items[xIndexKey] := xParam;
  end
  else
  begin
    xParam := TParam.Create(AKey,AValue);
    Self.Add(xParam);
  end;
end;

{ TBybitHttpClient }

constructor TBybitHttpClient.Create;
begin
  FBybitModule := TBybitModule.Create;
  FResponse    := TBytiyResponse.Create;
  FTask := nil;
  FValueMessage := '';
  FInterval := 0;
  FSource  := TStringList.Create;
end;

destructor TBybitHttpClient.Destroy;
begin
  if Assigned(FTask) then
  begin
    FTask.Cancel;
    FTask := nil;
  end;
  FreeAndNil(FSource);
  FreeAndNil(FResponse);
  FreeAndNil(FBybitModule);
  inherited;
end;

procedure TBybitHttpClient.DoEventMessage(const AMessage: String);
begin
  if FInterval = 0 then
    FTask := nil;

  FValueMessage := AMessage;
  if not FValueMessage.IsEmpty then
    FResponse.SetParserValue(FValueMessage);

  if Assigned(FOnEventMessage) then
    FOnEventMessage(Self);
end;

procedure TBybitHttpClient.DoEventException(const AStatusCode: Integer; const AMessage: String);
begin
  // Генерируем перехватываем исключение
  FTask := nil;
  FStatusCode := AStatusCode;
  FValueMessage := AMessage;
  if Assigned(FOnEventException) then
    FOnEventException(Self);
end;

procedure TBybitHttpClient.DoEventParser;
begin
end;

procedure TBybitHttpClient.DoEventBeginLoading;
begin
  if Assigned(FOnEventBeginLoading) then
    FOnEventBeginLoading(Self);
end;

procedure TBybitHttpClient.DoEventEndLoading;
begin
  try
    DoEventParser;
  except
    FStatusCode := ERROR_CODE_PARSER;
  end;
  if FActive then
    FActive := False;
  if Assigned(FOnEventEndLoading) then
    FOnEventEndLoading(Self);
end;

procedure TBybitHttpClient.SetThreading;
begin
  if Assigned(FTask) then
    raise Exception.Create('Error Message: Задание уже запущенно');

  FTask := TTask.Create(
    procedure()
    var
      xHttpClientAPI: TBybitHttpClientAPI;
    begin
      while True do
      begin
        TThread.Synchronize(nil,DoEventBeginLoading);
        xHttpClientAPI := TBybitHttpClientAPI.Create;
        try
          xHttpClientAPI.BybitModule := FBybitModule;
          xHttpClientAPI.Selected;
          FStatusCode := xHttpClientAPI.StatusCode;
          if FStatusCode = 200 then
          begin
            FValueMessage := xHttpClientAPI.ResponseValue;
            DoEventMessage(FValueMessage);
          end
          else
          begin
            FValueMessage := '';
            DoEventException(
              xHttpClientAPI.StatusCode,
              xHttpClientAPI.ResponseValue
            );
          end;
        finally
          FreeAndNil(xHttpClientAPI);
        end;
        TThread.Synchronize(nil,DoEventEndLoading);
        if FInterval = 0 then
          Break
        else
          Sleep(FInterval);
        if FTask.Status = TTaskStatus.Canceled then
          Break;
      end;
    end
  );
  FTask.Start;
end;

procedure TBybitHttpClient.Selected;
begin
  if not FActive then
  begin
    // Отправляем запрос на обновление данных
    FInterval := 0;
    SetThreading;
    FActive := True;
  end;
end;

procedure TBybitHttpClient.Start(const AInterval: Integer);
begin
  if not FActive then
  begin
    FInterval := AInterval;
    SetThreading;
  end;
end;

procedure TBybitHttpClient.Stop;
begin
  if FActive then
    if Assigned(FTask) then
    begin
      FInterval := 0;
      FTask.Cancel;
      FTask := nil;
    end;
end;

{ TBytiyResponse }

constructor TBytiyResponse.Create;
begin
  FResultObject  := nil;
  FExtInfoObject := nil;
end;

destructor TBytiyResponse.Destroy;
begin

  inherited;
end;

procedure TBytiyResponse.SetParserValue(const AValue: String);
var
  xJson: TJSONObject;
begin
(*******************************************************************************
  Response Example
  {
    "retCode": 0,
    "retMsg": "OK",
    "result": {
        "orderId": "1321003749386327552",
        "orderLinkId": "spot-test-postonly"
    },
    "retExtInfo": {},
    "time": 1672211918471
  }
*******************************************************************************)
  try
    xJson          := TJSONObject.ParseJSONValue(AValue) as TJSONObject;
    FretCode       := xJson.Values['retCode'].Value.ToInteger;
    FretMsg        := xJson.Values['retMsg'].Value;
    FResultObject  := TJSONObject(xJson.Values['result']);
    FExtInfoObject := TJSONObject(xJson.Values['retExtInfo']);
    FretTime       := xJson.Values['time'].Value.ToDouble;
  except
    raise EAbort.Create('Error Message: Парсинг Json – объекта');
  end;
end;

{ TBybitModule }

constructor TBybitModule.Create;
begin
  FTypeHttp := TTypeHttp.thNull;
  FHost     := BYBIT_HOST;
  FModule   := '';
  FParams   := TParamList.Create;
  FHeaders  := THeaderList.Create;
end;

destructor TBybitModule.Destroy;
begin
  FreeAndNil(FHeaders);
  FreeAndNil(FParams);
  inherited;
end;

function TBybitModule.GetQuery: String;
var
  xS: String;
  i, iCount: Integer;
  xParam: TParam;
begin
  xS := '';
  iCount := FParams.Count;
  if iCount > 0 then
  begin
    for i := 0 to iCount - 2 do
    begin
      xParam := FParams[i];
      xS := xS + xParam.Key + '=' + xParam.Value + '&';
    end;
    xParam := FParams[iCount - 1];
    xS := xS + xParam.Key + '=' + xParam.Value;
  end;
  Result := xS;
end;

function TBybitModule.GetURL: String;
var
  xS: String;
  iCount: Integer;
begin
  xS := FHost + FModule;
  iCount := FParams.Count;
  if iCount > 0 then
    xS := xS + '?' + GetQuery;
  Result := xS;
end;

{ TBybitHttpClientAPI }

constructor TBybitHttpClientAPI.Create;
begin
  FBybitModule := nil;
  FClient := TNetHTTPClient.Create(nil);
  FSource := TStringList.Create;
end;

destructor TBybitHttpClientAPI.Destroy;
begin
  FreeAndNil(FSource);
  FreeAndNil(FClient);
  inherited;
end;

procedure TBybitHttpClientAPI.Selected;

  function _Headers: TNetHeaders;
  var
    i, iCount: Integer;
    xHeader: THeader;
    xHeaders: TNetHeaders;
  begin
    SetLength(xHeaders,FBybitModule.Headers.Count);
    iCount := FBybitModule.Headers.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xHeader :=  FBybitModule.Headers.Items[i];
        xHeaders[i] := xHeader;
      end;
    Result := xHeaders;
  end;

var
  xURL: String;
  xHeaders: TNetHeaders;
  xResponse: IHTTPResponse;
begin
  if not Assigned(FBybitModule) then
    raise Exception.Create('Error Message: Параметры модуля не определены');
  FClient.UserAgent := 'Client Bybit';
  xHeaders  := _Headers;
  xURL := FBybitModule.GetURL;
  try
    case FBybitModule.TypeHttp of
      thGet: xResponse := FClient.Get(
        xURL,
        nil,
        xHeaders
      );
      thPost: begin
        xResponse := FClient.Post(
          xURL,
          FSource,
          nil,
          TEncoding.UTF8,
          xHeaders
        );
      end;
    end;
    FStatusCode := xResponse.StatusCode;
    if FStatusCode = 200 then
      FResponseValue := xResponse.ContentAsString(TEncoding.UTF8)
    else
      FResponseValue := 'Ошибка запроса';
  except
    FStatusCode := ERROR_CODE_HTTP;
    FResponseValue := 'Error Connection. Ошибка соединение';
  end;
end;

{ TCustonObjectJson }

constructor TCustonObjectJson.Create;
begin

end;

destructor TCustonObjectJson.Destroy;
begin

  inherited;
end;

procedure TCustonObjectJson.SetObjectJson(const AObjectJson: TJSONObject);
begin
  {todo: автоматическиое присвоение свойство}
  FObjectJson := AObjectJson;
end;

end.
