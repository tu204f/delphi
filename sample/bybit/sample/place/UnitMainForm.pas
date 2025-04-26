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
  Lb.Bybit.RealTime, System.Rtti, FMX.Grid.Style, FMX.Grid;

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
    StrGrid: TStringGrid;
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

    procedure SetOrderLevel(const APrice: Double; ASide: TTypeSide);

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

{
    't0YI4Ou0TKOTd7WrkE',
    'dWcdTGIulDoKOiK4mggPQIkYwmMFGxvFVusp'
}

const
  API_KEY    = 't0YI4Ou0TKOTd7WrkE';
  API_SECRET = 'dWcdTGIulDoKOiK4mggPQIkYwmMFGxvFVusp';

procedure SetAddColumn(const AStrGrid: TStringGrid; const AHeader: String; const AWidth: Single = 80);
var
  xCol: TStringColumn;
begin
  xCol := TStringColumn.Create(nil);
  xCol.Parent := AStrGrid;
  xCol.Header := AHeader;
  xCol.Width  := AWidth;
end;


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


  SetAddColumn(StrGrid, 'OrderID');
  SetAddColumn(StrGrid, 'OrderLinkID');
  SetAddColumn(StrGrid, 'BlockTradeID');
  SetAddColumn(StrGrid, 'Symbol');
  SetAddColumn(StrGrid, 'Price');
  SetAddColumn(StrGrid, 'Qty');
  SetAddColumn(StrGrid, 'Side');
  SetAddColumn(StrGrid, 'IsLeverage');
  SetAddColumn(StrGrid, 'PositionIdx');
  SetAddColumn(StrGrid, 'OrderStatus');
  SetAddColumn(StrGrid, 'CreateType');
  SetAddColumn(StrGrid, 'CancelType');
  SetAddColumn(StrGrid, 'RejectReason');
  SetAddColumn(StrGrid, 'AvgPrice');
  SetAddColumn(StrGrid, 'LeavesQty');
  SetAddColumn(StrGrid, 'LeavesValue');
  SetAddColumn(StrGrid, 'CumExecQty');
  SetAddColumn(StrGrid, 'CumExecValue');
  SetAddColumn(StrGrid, 'CumExecFee');
  SetAddColumn(StrGrid, 'TimeInForce');
  SetAddColumn(StrGrid, 'OrderType');
  SetAddColumn(StrGrid, 'StopOrderType');
  SetAddColumn(StrGrid, 'OrderIv');
  SetAddColumn(StrGrid, 'MarketUnit');
  SetAddColumn(StrGrid, 'TriggerPrice');
  SetAddColumn(StrGrid, 'TakeProfit');
  SetAddColumn(StrGrid, 'StopLoss');
  SetAddColumn(StrGrid, 'TpSlMode');
  SetAddColumn(StrGrid, 'OcoTriggerType');
  SetAddColumn(StrGrid, 'TpLimitPrice');
  SetAddColumn(StrGrid, 'SlLimitPrice');
  SetAddColumn(StrGrid, 'TpTriggerBy');
  SetAddColumn(StrGrid, 'SlTriggerBy');
  SetAddColumn(StrGrid, 'TriggerDirection');
  SetAddColumn(StrGrid, 'TriggerBy');
  SetAddColumn(StrGrid, 'LastPriceOnCreated');
  SetAddColumn(StrGrid, 'ReduceOnly');
  SetAddColumn(StrGrid, 'CloseOnTrigger');
  SetAddColumn(StrGrid, 'PlaceType');
  SetAddColumn(StrGrid, 'SmpType');
  SetAddColumn(StrGrid, 'SmpGroup');
  SetAddColumn(StrGrid, 'SmpOrderID');
  SetAddColumn(StrGrid, 'CreatedTime');
  SetAddColumn(StrGrid, 'UpDatedTime');

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

procedure TMainForm.SetOrderLevel(const APrice: Double; ASide: TTypeSide);
var
  xS: String;
  xPlaceOrder: TParamOrder;
begin
  xPlaceOrder := TParamOrder.Create;
  try
    xPlaceOrder.TypeProc    := TParamOrder.TTypeProc.Place;
    xPlaceOrder.Category    := TTypeCategory.tcLinear;
    xPlaceOrder.Symbol      := 'ETHUSDT';
    xPlaceOrder.Side        := ASide;
    xPlaceOrder.PositionIdx := 0;
    xPlaceOrder.OrderType   := TTypeOrder.Limit;
    xPlaceOrder.Qty         := 1;
    xPlaceOrder.Price       := APrice;
    xPlaceOrder.timeInForce := TTypeTimeInForce.GTC;
    xPlaceOrder.OrderLinkId := 'test' + Random(65000).ToString;

//    xPlaceOrder.TakeProfit := xPlaceOrder.Price - 60;
//    xPlaceOrder.StopLoss   := xPlaceOrder.Price + 20;

    xS := SelectedOrder(
       API_KEY,
       API_SECRET,
       xPlaceOrder,
       OrderResponse
    );
    SetLog(xS);
  finally
    FreeAndNil(xPlaceOrder);
  end;
end;

procedure TMainForm.ButtonPlaceOrderClick(Sender: TObject);
begin
  SetOrderLevel(1820, TTypeSide.tsSell);
  SetOrderLevel(1810, TTypeSide.tsSell);
  SetOrderLevel(1800, TTypeSide.tsSell);
  SetOrderLevel(1790, TTypeSide.tsSell);
  SetOrderLevel(1780, TTypeSide.tsBuy);
  SetOrderLevel(1770, TTypeSide.tsBuy);
  SetOrderLevel(1760, TTypeSide.tsBuy);
  SetOrderLevel(1750, TTypeSide.tsBuy);
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
    xPlaceOrder.Symbol      := 'ETHUSDT';
    xPlaceOrder.OrderID     := OrderResponse.OrderID;
    xS := SelectedOrder(
       API_KEY,
       API_SECRET,
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
    xPlaceOrder.Symbol      := 'ETHUSDT';
    xPlaceOrder.OrderLinkID := OrderResponse.OrderLinkID;
    xS := SelectedOrder(
       API_KEY,
       API_SECRET,
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
    xPlaceOrder.Symbol      := 'ETHUSDT';
    xPlaceOrder.Price       := 1847;
    xPlaceOrder.Qty         := 0.02;
    xS := SelectedOrder(
       API_KEY,
       API_SECRET,
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
    xPlaceOrder.Symbol      := 'ETHUSDT';
    xPlaceOrder.Price       := 1848;
    xPlaceOrder.Qty         := 0.02;
    xS := SelectedOrder(
       API_KEY,
       API_SECRET,
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
    API_KEY,
    API_SECRET
  );
  BybitRealTime.Category := TTypeCategory.tcLinear;
  BybitRealTime.Symbol := 'ETHUSDT';
  BybitRealTime.Selected;
end;

procedure TMainForm.RealTimeEventEndLoading(Sender: TObject);

  function _GetStrToBool(const AValue: Boolean): String;
  begin
    if AValue then
      Result := 'True'
    else
      Result := 'False';
  end;

var
  i, iCount: Integer;
  xRealTimeObject: TRealTimeObject;
begin
  SetLog('RealTimeEventEndLoading:');
  SetLog('>> ' + BybitRealTime.RealTimeObjects.Count.ToString);


  iCount := BybitRealTime.RealTimeObjects.Count;
  StrGrid.RowCount := iCount;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xRealTimeObject := BybitRealTime.RealTimeObjects.Items[i];
      with xRealTimeObject do
      begin
        StrGrid.Cells[0,i] := OrderID;
        StrGrid.Cells[1,i] := OrderLinkID;
        StrGrid.Cells[2,i] := BlockTradeID;
        StrGrid.Cells[3,i] := Symbol;
        StrGrid.Cells[4,i] := Price;
        StrGrid.Cells[5,i] := Qty;
        StrGrid.Cells[6,i] := Side;
        StrGrid.Cells[7,i] := IsLeverage;
        StrGrid.Cells[8,i] := PositionIdx.ToString;
        StrGrid.Cells[9,i] := OrderStatus;
        StrGrid.Cells[10,i] := CreateType;
        StrGrid.Cells[11,i] := CancelType;
        StrGrid.Cells[12,i] := RejectReason;
        StrGrid.Cells[13,i] := AvgPrice;
        StrGrid.Cells[14,i] := LeavesQty;
        StrGrid.Cells[15,i] := LeavesValue;
        StrGrid.Cells[16,i] := CumExecQty;
        StrGrid.Cells[17,i] := CumExecValue;
        StrGrid.Cells[18,i] := CumExecFee;
        StrGrid.Cells[19,i] := TimeInForce;
        StrGrid.Cells[20,i] := OrderType;
        StrGrid.Cells[21,i] := StopOrderType;
        StrGrid.Cells[22,i] := OrderIv;
        StrGrid.Cells[23,i] := MarketUnit;
        StrGrid.Cells[24,i] := TriggerPrice;
        StrGrid.Cells[25,i] := TakeProfit;
        StrGrid.Cells[26,i] := StopLoss;
        StrGrid.Cells[27,i] := TpSlMode;
        StrGrid.Cells[28,i] := OcoTriggerType;
        StrGrid.Cells[29,i] := TpLimitPrice;
        StrGrid.Cells[30,i] := SlLimitPrice;
        StrGrid.Cells[31,i] := TpTriggerBy;
        StrGrid.Cells[32,i] := SlTriggerBy;
        StrGrid.Cells[33,i] := TriggerDirection.ToString;
        StrGrid.Cells[34,i] := TriggerBy;
        StrGrid.Cells[35,i] := LastPriceOnCreated;
        StrGrid.Cells[36,i] := _GetStrToBool(ReduceOnly);
        StrGrid.Cells[37,i] := _GetStrToBool(CloseOnTrigger);
        StrGrid.Cells[38,i] := PlaceType;
        StrGrid.Cells[39,i] := SmpType;
        StrGrid.Cells[40,i] := SmpGroup.ToString;
        StrGrid.Cells[41,i] := SmpOrderID;
        StrGrid.Cells[42,i] := CreatedTime;
        StrGrid.Cells[43,i] := UpDatedTime;
      end;
    end;

end;

end.
