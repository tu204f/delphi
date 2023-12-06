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

{$IFDEF TEST}
  BYBIT_HOST = 'https://api-testnet.bybit.com';
{$ELSE}
  BYBIT_HOST = 'https://api.bybit.com';
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

  ///<summary>Bybit – объект</summary>
  TBybitObject = class(TObject)
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

{ TBybitObject }

constructor TBybitObject.Create;
begin
  FIntervalSleep := 1000;
  FParams := TParamList.Create;
  FURL := '';
  FHost := BYBIT_HOST;
  FHeaders := THeaderList.Create;
end;

destructor TBybitObject.Destroy;
begin
  FreeAndNil(FHeaders);
  FreeAndNil(FParams);
  inherited;
end;

procedure TBybitObject.Get(AURL: String);
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

function TBybitObject.GetURL: String;
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


procedure TBybitObject.Start(const ActiveUpData: Boolean);
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

procedure TBybitObject.Stop;
begin
  if Self.Active then
    FActive := False;
end;

procedure TBybitObject.DoActive(const Active: Boolean);
begin
  FActive := Active;
  if Assigned(FOnStatus) then
    FOnStatus(Self,FActive);
end;

procedure TBybitObject.DoEventMessage(const AMessage: String);
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

procedure TBybitObject.SetResultObject(const AObjectJson: TJSONObject);
begin
  FResultObject := AObjectJson;
end;

procedure TBybitObject.SetExtInfoObject(const AObjectJson: TJSONObject);
begin
  FExtInfoObject := AObjectJson;
end;

procedure TBybitObject.SetParserResult(const AObjectJson: TJSONObject);
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
