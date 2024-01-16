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
  Lb.Bybit.Trade,
  Lb.Bybit.RealTime;

type
  TMainForm = class(TForm)
    MemoResult: TMemo;
    ButtonPlaceOrder: TButton;
    ButtonCancelOrder1: TButton;
    ButtonCancelOrder2: TButton;
    ButtonAmendOrder1: TButton;
    ButtonOrders: TButton;
    ButtonOnThread: TButton;
    ButtonAmendOrder2: TButton;
    procedure ButtonPlaceOrderClick(Sender: TObject);
    procedure ButtonCancelOrder1Click(Sender: TObject);
    procedure ButtonCancelOrder2Click(Sender: TObject);
    procedure ButtonAmendOrder1Click(Sender: TObject);
    procedure ButtonAmendOrder2Click(Sender: TObject);
    procedure ButtonOrdersClick(Sender: TObject);
  private
    BybitRealTime: TBybitRealTime;
    OrderResponse: TOrderResponse;

    procedure RealTimeEventMessage(Sender: TObject);
    procedure RealTimeEventException(Sender: TObject);
    procedure RealTimeEventBeginLoading(Sender: TObject);
    procedure RealTimeEventEndLoading(Sender: TObject);

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

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  BybitRealTime := TBybitRealTime.Create;
  BybitRealTime.OnEventMessage := RealTimeEventMessage;
  BybitRealTime.OnEventException := RealTimeEventException;
  BybitRealTime.OnEventBeginLoading := RealTimeEventBeginLoading;
  BybitRealTime.OnEventEndLoading := RealTimeEventEndLoading;

  OrderResponse := TOrderResponse.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(OrderResponse);
  FreeAndNil(BybitRealTime);
  inherited;
end;

procedure TMainForm.SetLog(S: String);
begin
  MemoResult.Lines.Add(S);
end;

procedure TMainForm.ButtonPlaceOrderClick(Sender: TObject);
var
  xS: String;
  xPlaceOrder: TParamOrder;
begin
  xPlaceOrder := TParamOrder.Create;
  try
    xPlaceOrder.TypeProc    := TParamOrder.TTypeProc.Place;
    xPlaceOrder.Category    := TTypeCategory.tcLinear;
    xPlaceOrder.Symbol      := 'BTCUSDT';
    xPlaceOrder.Side        := TTypeSide.tsBuy;
    xPlaceOrder.PositionIdx := 0;
    xPlaceOrder.OrderType   := TTypeOrder.Limit;
    xPlaceOrder.Qty         := 0.001;
    xPlaceOrder.Price       := 43596;
    xPlaceOrder.timeInForce := TTypeTimeInForce.GTC;
    xPlaceOrder.OrderLinkId := 'test' + Random(65000).ToString;
    xS := SelectedOrder(
      'xN7ITgfUFq8XoPSuiE',
      'MXcY548mw6hfp8YVoP6KO4XL8xFOYcMA1w7Z',
       xPlaceOrder,
       OrderResponse
    );
    SetLog(xS);
  finally
    FreeAndNil(xPlaceOrder);
  end;
end;

procedure TMainForm.ButtonCancelOrder1Click(Sender: TObject);
var
  xS: String;
  xPlaceOrder: TParamOrder;
begin
  xPlaceOrder := TParamOrder.Create;
  try
    xPlaceOrder.TypeProc    := TParamOrder.TTypeProc.Cancel;
    xPlaceOrder.Category    := TTypeCategory.tcLinear;
    xPlaceOrder.Symbol      := 'BTCUSDT';
    xPlaceOrder.OrderID     := OrderResponse.OrderID;
    xS := SelectedOrder(
      'xN7ITgfUFq8XoPSuiE',
      'MXcY548mw6hfp8YVoP6KO4XL8xFOYcMA1w7Z',
       xPlaceOrder,
       OrderResponse
    );
    SetLog(xS);
  finally
    FreeAndNil(xPlaceOrder);
  end;
end;

procedure TMainForm.ButtonCancelOrder2Click(Sender: TObject);
var
  xS: String;
  xPlaceOrder: TParamOrder;
begin
  xPlaceOrder := TParamOrder.Create;
  try
    xPlaceOrder.TypeProc    := TParamOrder.TTypeProc.Cancel;
    xPlaceOrder.Category    := TTypeCategory.tcLinear;
    xPlaceOrder.Symbol      := 'BTCUSDT';
    xPlaceOrder.OrderLinkID := OrderResponse.OrderLinkID;
    xS := SelectedOrder(
      'xN7ITgfUFq8XoPSuiE',
      'MXcY548mw6hfp8YVoP6KO4XL8xFOYcMA1w7Z',
       xPlaceOrder,
       OrderResponse
    );
    SetLog(xS);
  finally
    FreeAndNil(xPlaceOrder);
  end;
end;


procedure TMainForm.ButtonAmendOrder1Click(Sender: TObject);
var
  xS: String;
  xPlaceOrder: TParamOrder;
begin
  xPlaceOrder := TParamOrder.Create;
  try
    xPlaceOrder.TypeProc    := TParamOrder.TTypeProc.Amend;
    xPlaceOrder.OrderID     := OrderResponse.OrderID;
    xPlaceOrder.Category    := TTypeCategory.tcLinear;
    xPlaceOrder.Symbol      := 'BTCUSDT';
    xPlaceOrder.Price       := 43455;
    xPlaceOrder.Qty         := 0.002;
    xS := SelectedOrder(
      'xN7ITgfUFq8XoPSuiE',
      'MXcY548mw6hfp8YVoP6KO4XL8xFOYcMA1w7Z',
       xPlaceOrder,
       OrderResponse
    );
    SetLog(xS);
  finally
    FreeAndNil(xPlaceOrder);
  end;
end;


procedure TMainForm.ButtonAmendOrder2Click(Sender: TObject);
var
  xS: String;
  xPlaceOrder: TParamOrder;
begin
  xPlaceOrder := TParamOrder.Create;
  try
    xPlaceOrder.TypeProc    := TParamOrder.TTypeProc.Amend;
    xPlaceOrder.OrderLinkID := OrderResponse.OrderLinkID;
    xPlaceOrder.Category    := TTypeCategory.tcLinear;
    xPlaceOrder.Symbol      := 'BTCUSDT';
    xPlaceOrder.Price       := 43455;
    xPlaceOrder.Qty         := 0.002;
    xS := SelectedOrder(
      'xN7ITgfUFq8XoPSuiE',
      'MXcY548mw6hfp8YVoP6KO4XL8xFOYcMA1w7Z',
       xPlaceOrder,
       OrderResponse
    );
    SetLog(xS);
  finally
    FreeAndNil(xPlaceOrder);
  end;
end;

procedure TMainForm.RealTimeEventBeginLoading(Sender: TObject);
begin
  SetLog('RealTimeEventBeginLoading:');
end;

procedure TMainForm.RealTimeEventException(Sender: TObject);
begin
  SetLog('RealTimeEventException:');
  SetLog('>> ' + BybitRealTime.Response.RetCode.ToString);
  SetLog('>> ' + BybitRealTime.Response.RetMsg);
end;

procedure TMainForm.RealTimeEventMessage(Sender: TObject);
begin
  //SetLog(BybitRealTime.ValueMessage);
end;

procedure TMainForm.ButtonOrdersClick(Sender: TObject);
begin
  BybitRealTime.OnEventEndLoading := RealTimeEventEndLoading;
  BybitRealTime.SetEncryption(
    'xN7ITgfUFq8XoPSuiE',
    'MXcY548mw6hfp8YVoP6KO4XL8xFOYcMA1w7Z'
  );
  BybitRealTime.Category := TTypeCategory.tcLinear;
  BybitRealTime.Symbol := 'BTCUSDT';
  BybitRealTime.Selected;
end;

procedure TMainForm.RealTimeEventEndLoading(Sender: TObject);
begin
  SetLog('RealTimeEventEndLoading:');
  SetLog('>> ' + BybitRealTime.RealTimeObjects.Count.ToString);
end;



end.
