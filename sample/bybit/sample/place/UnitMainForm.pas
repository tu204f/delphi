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
  Lb.Bybit.PlaceOrder;

type
  TMainForm = class(TForm)
    MemoResult: TMemo;
    ButtonOrder1: TButton;
    ButtonOrder2: TButton;
    ButtonOrder3: TButton;
    ButtonOrder4: TButton;
    procedure ButtonOrder1Click(Sender: TObject);
    procedure ButtonOrder2Click(Sender: TObject);
    procedure ButtonOrder3Click(Sender: TObject);
    procedure ButtonOrder4Click(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetLog(S: String = '');
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TMainForm.Destroy;
begin

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


end.
