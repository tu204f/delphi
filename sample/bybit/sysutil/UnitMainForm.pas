unit UnitMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Edit,
  Lb.Bybit.PlaceOrder,
  Lb.Bybit.Position,
  Lb.Bybit.ServerTime,
  Lb.Bybit.SysUtils;

//https://testnet.bybit.com/trade/usdt/BTCUSDT

type
  TMainForm = class(TForm)
    ButtonSelected: TButton;
    MemoResult: TMemo;
    ButtonSelectedOffCrypt: TButton;
    ButtonSelectedOnCrypt: TButton;
    ButtonOrder: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonSelectedClick(Sender: TObject);
    procedure ButtonSelectedOffCryptClick(Sender: TObject);
    procedure ButtonSelectedOnCryptClick(Sender: TObject);
    procedure ButtonOrderSpotClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    BybitServerTime: TBybitServerTime;
    BybitObject: TBybitHttpClient;
    function GetCreateBybitObject: TBybitHttpClient;
    procedure BybitObjectEventMessage(ASender: TObject);
  protected
    procedure BybitServerTimeOnEventBeginLoading(Sender: TObject);
    procedure BybitServerTimeOnEventEndLoading(Sender: TObject);
  public
    procedure SetLog(S: String = '');
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.Hash,
  System.DateUtils,
  Lb.Bybit.Encryption,
  Lb.Bybit.InstrumentsInfo;

{ TMainForm }

procedure TMainForm.SetLog(S: String);
begin
  MemoResult.Lines.Add(S);
end;

function TMainForm.GetCreateBybitObject: TBybitHttpClient;
var
  xBybitObject: TBybitHttpClient;
begin
  xBybitObject := TBybitHttpClient.Create;
  //xBybitObject := TBybitPosition.Create;
  Result := xBybitObject;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Self.Caption := 'Запрос на сервер Bybit';
  BybitObject := GetCreateBybitObject;
  if Assigned(BybitObject) then
  begin
    BybitObject.OnEventMessage := BybitObjectEventMessage;
    BybitObject.OnEventException := BybitObjectEventMessage;
  end;

  BybitServerTime := TBybitServerTime.Create;
  BybitServerTime.Selected(100);
  BybitServerTime.OnEventBeginLoading := BybitServerTimeOnEventBeginLoading;
  BybitServerTime.OnEventEndLoading := BybitServerTimeOnEventEndLoading;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(BybitServerTime);
  if Assigned(BybitObject) then
    FreeAndNil(BybitObject);
end;

procedure TMainForm.ButtonSelectedOffCryptClick(Sender: TObject);
var
  xS: String;
  HttpClientAPI: TBybitHttpClientAPI;
begin
  HttpClientAPI := TBybitHttpClientAPI.Create;
  try
    HttpClientAPI.BytiyModule.TypeHttp := TTypeHttp.thGet;
    HttpClientAPI.BytiyModule.Module := '/v5/market/time';
    HttpClientAPI.Selected;
    xS :=
      HttpClientAPI.StatusCode.ToString + ' ' +
      Copy(HttpClientAPI.ResponseValue,1,100);
    SetLog(xS);
  finally
    FreeAndNil(HttpClientAPI);
  end;
end;

procedure TMainForm.ButtonSelectedOnCryptClick(Sender: TObject);
var
  xS: String;
  xHttpClientAPI: TBybitHttpClientAPI;
  xEncryption: TEncryption;
begin
  xEncryption := TEncryption.Create;

  xHttpClientAPI := TBybitHttpClientAPI.Create;
  try
    xEncryption.ApiKey    := 'IYokQRNi1KjdlQ34vT';
    xEncryption.ApiSecret := 'cRQVjujmbZOAc4yIeoPTR2izhTbQPlkPgsGN';
    xEncryption.Timestamp := BybitServerTime.TimeSecond + '000';

    with xHttpClientAPI.BytiyModule do
    begin
      TypeHttp := TTypeHttp.thGet;
      Host := BYBIT_HOST;
      Module := '/v5/market/time';

      // Параметр
      with Params do
      begin
        SetParam('category',GetStrToTypeCategory(TTypeCategory.tcLinear));
        SetParam('symbol','BTCUSDT');
      end;
      xEncryption.QueryBody := Query;

      // Значение работы
      with Headers do
      begin
        Values['X-BAPI-API-KEY']     := xEncryption.ApiKey;
        Values['X-BAPI-TIMESTAMP']   := xEncryption.Timestamp;
        Values['X-BAPI-RECV-WINDOW'] := xEncryption.RecvWindow;
        Values['X-BAPI-SIGN-TYPE']   := '2';
        Values['X-BAPI-SIGN']        := xEncryption.Signature;
      end;
    end;

    xHttpClientAPI.Selected;
    xS :=
      xHttpClientAPI.StatusCode.ToString + ' ' +
      Copy(xHttpClientAPI.ResponseValue,1,300);
    SetLog(xS);

  finally
    FreeAndNil(xHttpClientAPI);
    FreeAndNil(xEncryption);
  end;
end;


procedure TMainForm.ButtonOrderSpotClick(Sender: TObject);
var
  xPlaceOrder: TObjectPlaceOrder;
begin
(*
  // Spot PostOnly normal order
  {
    "category":"spot",
    "symbol":"BTCUSDT",
    "side":"Buy",
    "orderType":"Limit",
    "qty":"0.1",
    "price":"15600",
    "timeInForce":"PostOnly",
    "orderLinkId":"spot-test-01",
    "isLeverage":0,
    "orderFilter":"Order"
  }
*)
  xPlaceOrder := TObjectPlaceOrder.Create;
  try
    xPlaceOrder.Category := TTypeCategory.tcSpot;
    xPlaceOrder.Symbol := 'BTCUSDT';
    xPlaceOrder.Side := TTypeSide.tsBuy;
    xPlaceOrder.OrderType := TTypeOrder.Limit;
    xPlaceOrder.Qty := 0.1;
    xPlaceOrder.Price := 15600;
    xPlaceOrder.TimeInForce := TTypeTimeInForce.PostOnly;
    xPlaceOrder.OrderLinkId := 'spot-test-01';
    xPlaceOrder.IsLeverage := 0;
    xPlaceOrder.OrderFilter := TTypeOrderFilter.Order;
    SetLog(xPlaceOrder.Value);
    SetLog('****');
  finally
    FreeAndNil(xPlaceOrder);
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  xPlaceOrder: TObjectPlaceOrder;
begin
(*
  // Spot TP/SL order
  {
    "category":"spot",
    "symbol":"BTCUSDT",
    "side":"Buy",
    "orderType":"Limit",
    "qty":"0.1",
    "price":"15600",
    "triggerPrice": "15000",
    "timeInForce":"Limit",
    "orderLinkId":"spot-test-02",
    "isLeverage":0,
    "orderFilter":"tpslOrder"
  }
*)
  xPlaceOrder := TObjectPlaceOrder.Create;
  try
    xPlaceOrder.Category := TTypeCategory.tcSpot;
    xPlaceOrder.Symbol := 'BTCUSDT';
    xPlaceOrder.Side := TTypeSide.tsBuy;
    xPlaceOrder.OrderType := TTypeOrder.Limit;
    xPlaceOrder.Qty := 0.1;
    xPlaceOrder.Price := 15600;
    xPlaceOrder.TriggerPrice := 15600;
    xPlaceOrder.TimeInForce := TTypeTimeInForce.PostOnly;
    xPlaceOrder.OrderLinkId := 'spot-test-02';
    xPlaceOrder.IsLeverage := 0;
    xPlaceOrder.OrderFilter := TTypeOrderFilter.tpslOrder;
    SetLog(xPlaceOrder.Value);
    SetLog('****');
  finally
    FreeAndNil(xPlaceOrder);
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  xPlaceOrder: TObjectPlaceOrder;
begin
(*
  // Spot margin normal order (UTA)
  {
    "category":"spot",
    "symbol":"BTCUSDT",
    "side":"Buy",
    "orderType":"Limit",
    "qty":"0.1",
    "price":"15600",
    "timeInForce":"Limit",
    "orderLinkId":"spot-test-limit",
    "isLeverage":1,
    "orderFilter":"Order"
  }
*)
  xPlaceOrder := TObjectPlaceOrder.Create;
  try
    xPlaceOrder.Category := TTypeCategory.tcSpot;
    xPlaceOrder.Symbol := 'BTCUSDT';
    xPlaceOrder.Side := TTypeSide.tsBuy;
    xPlaceOrder.OrderType := TTypeOrder.Limit;
    xPlaceOrder.Qty := 0.1;
    xPlaceOrder.Price := 15600;
    xPlaceOrder.TimeInForce := TTypeTimeInForce.PostOnly;
    xPlaceOrder.OrderLinkId := 'spot-test-limit';
    xPlaceOrder.IsLeverage := 1;
    xPlaceOrder.OrderFilter := TTypeOrderFilter.Order;
    SetLog(xPlaceOrder.Value);
    SetLog('****');
  finally
    FreeAndNil(xPlaceOrder);
  end;
end;

procedure TMainForm.Button3Click(Sender: TObject);
var
  xPlaceOrder: TObjectPlaceOrder;
begin
(*
  // Spot Market Buy order, qty is quote currency
  {
    "category":"spot",
    "symbol":"BTCUSDT",
    "side":"Buy",
    "orderType":"Market",
    "qty":"200",
    "timeInForce":"IOC",
    "orderLinkId":"spot-test-04",
    "isLeverage":0,
    "orderFilter":"Order"
  }
*)
  xPlaceOrder := TObjectPlaceOrder.Create;
  try
    xPlaceOrder.Category := TTypeCategory.tcSpot;
    xPlaceOrder.Symbol := 'BTCUSDT';
    xPlaceOrder.Side := TTypeSide.tsBuy;
    xPlaceOrder.OrderType := TTypeOrder.Market;
    xPlaceOrder.Qty := 200;
    xPlaceOrder.TimeInForce := TTypeTimeInForce.IOC;
    xPlaceOrder.OrderLinkId := 'spot-test-04';
    xPlaceOrder.IsLeverage := 0;
    xPlaceOrder.OrderFilter := TTypeOrderFilter.Order;
    SetLog(xPlaceOrder.Value);
    SetLog('****');
  finally
    FreeAndNil(xPlaceOrder);
  end;
end;

procedure TMainForm.ButtonSelectedClick(Sender: TObject);
var
  xEncryption: TEncryption;
begin
  MemoResult.Lines.Add(BybitServerTime.TimeSecond + ' :: ' + BybitServerTime.TimeNano);

  xEncryption := TEncryption.Create;

  xEncryption.ApiKey    := 'IYokQRNi1KjdlQ34vT';
  xEncryption.ApiSecret := 'cRQVjujmbZOAc4yIeoPTR2izhTbQPlkPgsGN';
  xEncryption.Timestamp := BybitServerTime.TimeSecond + '000';

  if Assigned(BybitObject) then
  begin
    with BybitObject do
    begin
      ModuleParam.TypeHttp := TTypeHttp.thGet;
      ModuleParam.Module := '/v5/position/list';

      with ModuleParam.Params do
      begin
        SetParam('category',GetStrToTypeCategory(TTypeCategory.tcLinear));
        SetParam('symbol','BTCUSDT');
        // SetParam('interval',GetStrToTypeInterval(TTypeInterval.ti_5));
        // SetParam('limit',IntToStr(1000));
        // SetParam('status',GetStrToTypeStatus(TTypeStatus.tsTrading));
        // baseCoin
        // limit
        // cursor
      end;
      xEncryption.QueryBody := ModuleParam.Query;

      with ModuleParam.Headers do
      begin
        Values['X-BAPI-API-KEY']     := xEncryption.ApiKey;
        Values['X-BAPI-TIMESTAMP']   := xEncryption.Timestamp;
        Values['X-BAPI-RECV-WINDOW'] := xEncryption.RecvWindow;
        Values['X-BAPI-SIGN-TYPE']   := '2';
        Values['X-BAPI-SIGN']        := xEncryption.Signature;
      end;
    end;
    BybitObject.Selected(0);
  end;

  FreeAndNil(xEncryption);
end;

procedure TMainForm.BybitObjectEventMessage(ASender: TObject);
begin
  MemoResult.Lines.Add(BybitObject.StatusCode.ToString);
  MemoResult.Lines.Add(BybitObject.ValueMessage);
//    TBybitServerTime(BybitObject).TimeSecond + ' ' +
//    TBybitServerTime(BybitObject).TimeNano + ' ' +
//    DateTimeToStr(
//      TBybitServerTime(BybitObject).DateTimeServer
//    );
end;

procedure TMainForm.BybitServerTimeOnEventBeginLoading(Sender: TObject);
begin

end;

procedure TMainForm.BybitServerTimeOnEventEndLoading(Sender: TObject);
begin

end;

end.
