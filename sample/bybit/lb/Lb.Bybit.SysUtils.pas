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
  System.JSON;

const
//{$DEFINE TEST}
  BYBIT_HOST =
{$IFDEF TEST}
  'https://api-testnet.bybit.com';
{$ELSE}
  'https://api.bybit.com';
{$ENDIF}

type
  TTypeCategory = (
    tcSpot,
    tcLinear,  // Бессрочный USDT и контракт USDC, включая USDC perp, фьючерсы USDC
    tcInverse, // Обратный контракт, включая обратного преступника, обратные фьючерсы
    tcOption
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
  TBytiyResponse = class;
  TBytiyModule = class;
  TTypeHttp = (thNull, thGet, thPost);
  ///<summary>Отвечает за отправление запроса на сервер Bybit</summary>
  ///<remarks>
  ///Запросы все отправляются в потоке и с получением ответа от сервера
  ///Также можно задавать интервал запроса
  ///</remarks>
  TBybitHttpClient = class(TObject)
  private
    FSource: TStrings;
    FValueMessage: String;
    FStatusCode: Integer;
    FIntervalSleep: Integer;
    FTask: ITask;
    FModuleParam: TBytiyModule;
    FResponse: TBytiyResponse;
  protected
    FOnEventMessage: TNotifyEvent;
    FOnEventException: TNotifyEvent;
    procedure DoEventMessage(const AMessage: String); virtual;
    procedure DoEventException(const AStatusCode: Integer; const AMessage: String);
    // Запрос выполняются в отделом потоке с ожидание ответа
    procedure SetTaskRun(const AURL: String);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Selected(const AInterval: Integer = 0);
    procedure Stop;
    property ModuleParam: TBytiyModule read FModuleParam;
    property IntervalSleep: Integer read FIntervalSleep write FIntervalSleep;
    property ValueMessage: String read FValueMessage;
    property StatusCode: Integer read FStatusCode;
    property Source: TStrings read FSource;
    property Response: TBytiyResponse read FResponse;
  public
    ///<summary>Событие ответа от сервера</summary>
    property OnEventMessage: TNotifyEvent write FOnEventMessage;
    property OnEventException: TNotifyEvent write FOnEventException;
  end;

  ///<summary>Ответ сервер</summary>
  TBytiyResponse = class(TObject)
  private
    FretCode: Integer;
    FretMsg: String;
    FretTime: Double;
    FResultObject: TJSONObject;
    FExtInfoObject: TJSONObject;
    procedure SetValueMessage(const Value: String);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property ValueMessage: String write SetValueMessage;
    property ResultObject: TJSONObject read FResultObject;
    property ExtInfoObject: TJSONObject read FExtInfoObject;
    property RetCode: Integer read FretCode;
    property RetMsg: String read FretMsg;
    property RetTime: Double read FretTime;
  end;

  ///<summary>Структура запрашиваемого модуля</summary>
  TBytiyModule = class(TObject)
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
(* Базовый класс, для всех запросов                                           *)
(******************************************************************************)

  ///<summary>Bybit – объект, Для периодический запросов</summary>
  TGetBybitObject = class(TObject)
  private
    FActive: Boolean;
    FActiveUpData: Boolean;
    FIntervalSleep: Integer;
    FURL: String;
    FHeaders: THeaderList;
    FParams: TParamList;
  private
    FretCode: Integer;
    FretMsg: String;
    FretTime: Double;
  private
    FHost: String;
    FModule: String;
    function GetURL: String;
  private
    FResultObject: TJSONObject;
    FExtInfoObject: TJSONObject;
  protected
    FOnEventMessage: TEventMessage;
    FOnStatus: TEventOnStatus;
    procedure DoEventMessage(const AMessage: String);
    procedure DoActive(const Active: Boolean);
    procedure Get(AURL: String);
    procedure SetResultObject(const AObjectJson: TJSONObject); virtual;
    procedure SetExtInfoObject(const AObjectJson: TJSONObject); virtual;
    procedure SetParserResult(const AObjectJson: TJSONObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Start(const ActiveUpData: Boolean = False);
    procedure Stop;
    property Host: String read FHost write FHost;
    property Module: String read FModule write FModule;
    property Params: TParamList read FParams;
    property URL: String read FURL write FURL;
    property IntervalSleep: Integer read FIntervalSleep write FIntervalSleep;
    ///<summary>Активное обновление</summary>
    property Active: Boolean read FActive;
    property Headers: THeaderList read FHeaders;
    property ResultObject: TJSONObject read FResultObject;
    property ExtInfoObject: TJSONObject read FExtInfoObject;
    property OnEventMessage: TEventMessage write FOnEventMessage;
    property OnStatus: TEventOnStatus write FOnStatus;
  public
    property RetCode: Integer read FretCode;
    property RetMsg: String read FretMsg;
    property RetTime: Double read FretTime;
  end;

  ///<summary></summary>
  TPostBybitObject = class(TObject)
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

function GetStrToTypeCategory(ACategory: TTypeCategory): String;
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
  FModuleParam := TBytiyModule.Create;
  FResponse    := TBytiyResponse.Create;
  FTask := nil;
  FValueMessage := '';
  FIntervalSleep := 0;
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
  FreeAndNil(FModuleParam);
  inherited;
end;

procedure TBybitHttpClient.DoEventMessage(const AMessage: String);
begin
  if FIntervalSleep = 0 then
    FTask := nil;
  FValueMessage := AMessage;
  if not FValueMessage.IsEmpty then
    FResponse.ValueMessage := FValueMessage;
  if Assigned(FOnEventMessage) then
    FOnEventMessage(Self);
end;

procedure TBybitHttpClient.DoEventException(const AStatusCode: Integer; const AMessage: String);
begin
  FTask := nil;
  FStatusCode := AStatusCode;
  FValueMessage := AMessage;
  if Assigned(FOnEventException) then
    FOnEventException(Self);
end;

procedure TBybitHttpClient.Selected(const AInterval: Integer);
begin
  FIntervalSleep := AInterval;
  SetTaskRun(
    FModuleParam.GetURL
  );
end;

procedure TBybitHttpClient.SetTaskRun(const AURL: String);
begin
  if Assigned(FTask) then
    raise Exception.Create('Error Message: Задание уже запущенно');

  FTask := TTask.Create(
    procedure()

      function _Headers: TNetHeaders;
      var
        i, iCount: Integer;
        xHeader: THeader;
        xHeaders: TNetHeaders;
      begin
        SetLength(xHeaders,FModuleParam.Headers.Count);
        iCount := FModuleParam.Headers.Count;
        if iCount > 0 then
          for i := 0 to iCount - 1 do
          begin
            xHeader :=  FModuleParam.Headers.Items[i];
            xHeaders[i] := xHeader;
          end;
        Result := xHeaders;
      end;

    var
      xValue: String;
      xHeaders : TNetHeaders;
      xClient  : TNetHTTPClient;
      xResponse: IHTTPResponse;
      xStatusCode: Integer;
    begin
      while True do
      begin
        if FTask.Status = TTaskStatus.Canceled then
          Exit;

        xClient := TNetHTTPClient.Create(nil);
        try
          xClient.UserAgent := 'Client Bybit';
          xHeaders  := _Headers;

          //xClient.CustHeaders.Value[]

//          with xClient.CustHeaders do
//          begin
//            Value['X-BAPI-API-KEY']     := 'IYokQRNi1KjdlQ34vT';
//            Value['X-BAPI-TIMESTAMP']   := '17019855110000';
//            Value['X-BAPI-RECV-WINDOW'] := xEncryption.RecvWindow;
//            Value['X-BAPI-SIGN-TYPE']   := '2';
//            Value['X-BAPI-SIGN']        := xEncryption.Signature;
//          end;

          case FModuleParam.TypeHttp of
            thGet : xResponse := xClient.Get(
              AURL,
              nil,
              xHeaders
            );
            thPost: begin
              // Пост запрос, который выполняется только один раз
              FIntervalSleep := 0;
              xResponse := xClient.Post(
                AURL,
                FSource,
                nil,
                TEncoding.UTF8,
                xHeaders
              );
            end;
          end;

          xStatusCode := xResponse.StatusCode;
          if xStatusCode = 200 then
          begin
            xValue := xResponse.ContentAsString(TEncoding.UTF8);
            TThread.Synchronize(nil,
              procedure()
              begin
                DoEventMessage(xValue);
              end
            );
          end;
        finally
          FreeAndNil(xClient);
        end;

        if xStatusCode <> 200 then
        begin
          // Нужно прикратить делать запросы
          TThread.Synchronize(nil,
            procedure()
            begin
              DoEventException(
                xStatusCode,
                'Проблемма за проссе');
            end
          );
          Break;
        end;

        // Перезапускам запрос, на сервер
        // Периодичность получение данных
        if FIntervalSleep > 0 then
          Sleep(FIntervalSleep)
        else
          Break;
      end;
    end
  );
  FTask.Start;
end;

procedure TBybitHttpClient.Stop;
begin
  if Assigned(FTask) then
  begin
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

procedure TBytiyResponse.SetValueMessage(const Value: String);
var
  xJson, xResultOb, xExtInfoOb: TJSONObject;
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
    xJson          := TJSONObject.ParseJSONValue(Value) as TJSONObject;
    FretCode       := xJson.Values['retCode'].Value.ToInteger;
    FretMsg        := xJson.Values['retMsg'].Value;
    FResultObject  := TJSONObject(xJson.Values['result']);
    FExtInfoObject := TJSONObject(xJson.Values['retExtInfo']);
    FretTime       := xJson.Values['time'].Value.ToDouble;
  except
    raise Exception.Create('Error Message: Парсинг Json – объекта');
  end;
end;

{ TBytiyModule }

constructor TBytiyModule.Create;
begin
  FTypeHttp := TTypeHttp.thNull;
  FHost     := BYBIT_HOST;
  FModule   := '';
  FParams   := TParamList.Create;
  FHeaders  := THeaderList.Create;
end;

destructor TBytiyModule.Destroy;
begin
  FreeAndNil(FHeaders);
  FreeAndNil(FParams);
  inherited;
end;

function TBytiyModule.GetQuery: String;
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

function TBytiyModule.GetURL: String;
var
  xS: String;
  i, iCount: Integer;
  xParam: TParam;
begin
  xS := FHost + FModule;
  iCount := FParams.Count;
  if iCount > 0 then
    xS := xS + '?' + GetQuery;
  Result := xS;
end;

{ TGetBybitObject }

constructor TGetBybitObject.Create;
begin
  FIntervalSleep := 1000;
  FParams := TParamList.Create;
  FURL := '';
  FHost := BYBIT_HOST;
  FHeaders := THeaderList.Create;
end;

destructor TGetBybitObject.Destroy;
begin
  FreeAndNil(FHeaders);
  FreeAndNil(FParams);
  inherited;
end;

procedure TGetBybitObject.Get(AURL: String);
var
  xTask: ITask;
begin
  // Поместить в поток
  xTask := TTask.Create(
    procedure()

      function _Headers: TNetHeaders;
      var
        i, iCount: Integer;
        xHeader: THeader;
        xHeaders: TNetHeaders;
      begin
        SetLength(xHeaders,FHeaders.Count);
        iCount := FHeaders.Count;
        if iCount > 0 then
          for i := 0 to iCount - 1 do
          begin
            xHeader :=  FHeaders.Items[i];
            xHeaders[i] := xHeader;
          end;

        Result := xHeaders;
      end;

    var
      xValue: String;
      xHeaders: TNetHeaders;
      xClient: TNetHTTPClient;
      xResponse: IHTTPResponse;
    begin
      while True do
      begin

        xClient := TNetHTTPClient.Create(nil);
        try
          xClient.UserAgent := 'Client Bybit';
          xHeaders := _Headers;
          xResponse := xClient.Get(AURL,nil,xHeaders);
          xValue := xResponse.ContentAsString(TEncoding.UTF8);
          TThread.Synchronize(nil,
            procedure()
            begin
              DoEventMessage(xValue);
            end
          );
        finally
          FreeAndNil(xClient);
        end;

        // Периодичность получение данных
        Sleep(FIntervalSleep);

        // Получить данные только один раз
        if not FActiveUpData then
          Break;

        // Продолаем работу
        if not FActive then
          Break;
      end;


      TThread.Synchronize(nil,
        procedure()
        begin
          if FActive then
            FActive := False;
          DoActive(FActive);
        end
      );

    end
  );
  xTask.Start;
end;

function TGetBybitObject.GetURL: String;
var
  xS: String;
  i, iCount: Integer;
  xParam: TParam;
begin
  xS := FHost + FModule;
  iCount := FParams.Count;
  if iCount > 0 then
  begin
    xS := xS + '?';
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


procedure TGetBybitObject.Start(const ActiveUpData: Boolean);
begin
  FActiveUpData := ActiveUpData;
  if not FActive then
  begin
    DoActive(True);
    if FURL.IsEmpty then
    begin
      FURL := GetURL;
      Self.Get(FURL);
    end;
  end;
end;

procedure TGetBybitObject.Stop;
begin
  if Self.Active then
    FActive := False;
end;

procedure TGetBybitObject.DoActive(const Active: Boolean);
begin
  FActive := Active;
  if Assigned(FOnStatus) then
    FOnStatus(Self,FActive);
end;

procedure TGetBybitObject.DoEventMessage(const AMessage: String);
var
  xJson: TJSONObject;
begin
  if Assigned(FOnEventMessage) then
  begin
    xJson := TJSONObject.ParseJSONValue(AMessage) as TJSONObject;
    SetParserResult(xJson);
    FOnEventMessage(Self,AMessage);
  end;
end;

procedure TGetBybitObject.SetResultObject(const AObjectJson: TJSONObject);
begin
  FResultObject := AObjectJson;
end;

procedure TGetBybitObject.SetExtInfoObject(const AObjectJson: TJSONObject);
begin
  FExtInfoObject := AObjectJson;
end;

procedure TGetBybitObject.SetParserResult(const AObjectJson: TJSONObject);
var
  xResultOb, xExtInfoOb: TJSONObject;
begin
  try
    FretCode    := AObjectJson.Values['retCode'].Value.ToInteger;
    FretMsg     := AObjectJson.Values['retMsg'].Value;
    xResultOb   := TJSONObject(AObjectJson.Values['result']);
    xExtInfoOb  := TJSONObject(AObjectJson.Values['retExtInfo']);
    FretTime    := AObjectJson.Values['time'].Value.ToDouble;
    if Assigned(xResultOb) then
      SetResultObject(xResultOb);
    if Assigned(xExtInfoOb) then
      SetExtInfoObject(xExtInfoOb);
  except
    {todo: сделка}
  end;
end;

{ TPostBybitObject }

constructor TPostBybitObject.Create;
begin

end;

destructor TPostBybitObject.Destroy;
begin

  inherited;
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
