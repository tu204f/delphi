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
  FMX.Memo.Types,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.StdCtrls,
  Lb.Bybit.SysUtils,
  Lb.Bybit.ServerTime,
  Lb.Bybit.Encryption,
  Lb.Bybit.PlaceOrder;

type
  TMainForm = class(TForm)
    MemoResult: TMemo;
    ButtonOrder1: TButton;
    ButtonOrder2: TButton;
    ButtonOrder3: TButton;
    ButtonOrder4: TButton;
    ButtonOrder: TButton;
    ButtonOnThread: TButton;
    Button1: TButton;
    procedure ButtonOrder1Click(Sender: TObject);
    procedure ButtonOrder2Click(Sender: TObject);
    procedure ButtonOrder3Click(Sender: TObject);
    procedure ButtonOrder4Click(Sender: TObject);
    procedure ButtonOrderClick(Sender: TObject);
    procedure ButtonOnThreadClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    BybitPlaceOrder: TBybitPlaceOrder;
    procedure BybitPlaceOrderOnEventEndLoading(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetLog(S: String = '');
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.Hash,
  System.DateUtils;

function GetNow: Int64;
var
  lDate: TDateTime;
begin
  lDate := TTimeZone.Local.ToUniversalTime(Now);
  Result := Abs(DateTimeToMilliseconds(UnixDateDelta) - DateTimeToMilliseconds(lDate));
end;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  BybitPlaceOrder := TBybitPlaceOrder.Create;
  BybitPlaceOrder.OnEventEndLoading := BybitPlaceOrderOnEventEndLoading;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(BybitPlaceOrder);
  inherited;
end;

procedure TMainForm.SetLog(S: String);
begin
  MemoResult.Lines.Add(S);
end;


procedure TMainForm.ButtonOrder1Click(Sender: TObject);
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

procedure TMainForm.ButtonOrder2Click(Sender: TObject);
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

procedure TMainForm.ButtonOrder3Click(Sender: TObject);
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

procedure TMainForm.ButtonOrder4Click(Sender: TObject);
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

procedure AddOrder(APlaceOrder: TObjectPlaceOrder);
begin
(*
  // Spot PostOnly normal order
  {
    "category":"linear",
    "symbol": "BTCUSDT",
    "side": "Buy",
    "positionIdx": 0,
    "orderType": "Limit",
    "qty": "0.001",
    "price": "45100",
    "timeInForce": "GTC",
    "orderLinkId": "test-01"
  }
*)
  APlaceOrder.Category := TTypeCategory.tcLinear;
  APlaceOrder.Symbol := 'BTCUSDT';
  APlaceOrder.Side := TTypeSide.tsBuy;
  APlaceOrder.PositionIdx := 0;
  APlaceOrder.OrderType := TTypeOrder.Limit;
  APlaceOrder.Qty := 0.001;
  APlaceOrder.Price := 44800;
  APlaceOrder.TimeInForce := TTypeTimeInForce.GTC;
  APlaceOrder.OrderLinkId := 'test-' + Random(60000).ToString;
end;

procedure TMainForm.ButtonOrderClick(Sender: TObject);
begin
  AddOrder(BybitPlaceOrder.AddPlaceOrder);
  BybitPlaceOrder.Selected;
end;

procedure TMainForm.BybitPlaceOrderOnEventEndLoading(Sender: TObject);
begin

end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  xValue: Int64;
  lDate: TDateTime;
begin
//  xValue := DateTimeToUnix(Now,True);
//  SetLog(IntToStr(xValue));

(*
  function SecondsBetween(const ANow, AThen: TDateTime): Int64;
  begin
    Result := Abs(DateTimeToMilliseconds(ANow) - DateTimeToMilliseconds(AThen))
      div (MSecsPerSec);
  end;

  function DateTimeToUnix(const AValue: TDateTime; AInputIsUTC: Boolean): Int64;
  var
    LDate: TDateTime;
   begin
    if AInputIsUTC then
      LDate := AValue
    else
      LDate := TTimeZone.Local.ToUniversalTime(AValue);
    Result := SecondsBetween(UnixDateDelta, LDate);
    if LDate < UnixDateDelta then
       Result := -Result;
   end;
*)

  lDate := Now;
  xValue := Abs(DateTimeToMilliseconds(UnixDateDelta) - DateTimeToMilliseconds(lDate));
  SetLog(IntToStr(xValue));
end;

procedure TMainForm.ButtonOnThreadClick(Sender: TObject);
var
  xValue: String;
  xModule: TBybitModule;
  xEncryption: TEncryption;
  xClientAPI: TBybitHttpClientAPI;
  xPlaceOrder: TObjectPlaceOrder;
  xSignature: String;
begin
  xModule := TBybitModule.Create;
  xEncryption:= TEncryption.Create;
  xClientAPI := TBybitHttpClientAPI.Create;
  xPlaceOrder := TObjectPlaceOrder.Create;
  try
    xEncryption.ApiKey    := 'xN7ITgfUFq8XoPSuiE';
    xEncryption.ApiSecret := 'MXcY548mw6hfp8YVoP6KO4XL8xFOYcMA1w7Z';
    xEncryption.Timestamp := GetNow.ToString;

    // Заполняем заявку
    AddOrder(xPlaceOrder);

    xValue := xPlaceOrder.Value;
    SetLog(xValue);

    xEncryption.QueryBody := xValue;
    xSignature := xEncryption.Signature;
    SetLog(xSignature);

    with xModule do
    begin
      TypeHttp := TTypeHttp.thPost;
      Host := BYBIT_HOST;
      Module := '/v5/order/create';

      // Значение работы
      with Headers do
      begin
        Values['X-BAPI-API-KEY']     := xEncryption.ApiKey;
        Values['X-BAPI-SIGN']        := xSignature;
        Values['X-BAPI-SIGN-TYPE']   := '2';
        Values['X-BAPI-TIMESTAMP']   := xEncryption.Timestamp;
        Values['X-BAPI-RECV-WINDOW'] := xEncryption.RecvWindow;
      end;
    end;

    xClientAPI.BybitModule := xModule;
    xClientAPI.Source := xValue;
    xClientAPI.Selected;
    MemoResult.Lines.Add(xClientAPI.ResponseValue);
  finally
    FreeAndNil(xPlaceOrder);
    FreeAndNil(xClientAPI);
    FreeAndNil(xEncryption);
    FreeAndNil(xModule);
  end;
end;


end.
