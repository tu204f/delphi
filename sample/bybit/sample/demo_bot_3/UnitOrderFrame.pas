unit UnitOrderFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  Lb.Bybit.SysUtils,
  Lb.Bybit.RealTime,
  Lb.Bybit.Trade,
  UnitIndicatorFrame,
  UnitValueUpDataFrame,
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.Objects,
  FMX.Layouts;

type
  ///<summary>Ордер</summary>
  TOrderFrame = class(TFrame)
    Rectangle: TRectangle;
    LayoutQuantity: TLayout;
    LayoutRSI: TLayout;
    LayoutSlip: TLayout;
    LayoutClient: TLayout;
    Button1: TButton;
    Timer1: TTimer;
    CheckBoxOrder: TCheckBox;
    CheckBoxAuto: TCheckBox;
    procedure Button1Click(Sender: TObject);
  public const
    {todo: Получать эти параметры}
    STEP_PRICE    = 0.01;
    STEP_QUANTITY = 0.001;
    STEP_RSI      = 1;
    STEP_SLIP     = 1;
  private
    ValueQuantity: TValueUpDataFrame;
    ValueRSI: TValueUpDataFrame;
    ValueSlip: TValueUpDataFrame;
  private
    FSide: TTypeSide;
    procedure SetSide(const Value: TTypeSide);
  protected
    BybitRealTime: TBybitRealTime;
    OrderResponse: TOrderResponse;
    procedure SetOperationTrade;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetUpData;
    property Side: TTypeSide write SetSide;
  end;

implementation

{$R *.fmx}

const
  API_KEY    = '3bI64e0kw4KuihyRPu';
  API_SECRET = 'jvwTC14ESSTjIpvXaRbDGW8xd1KoqD3H3cWY';

{ TOrderFrame }

constructor TOrderFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ValueQuantity := TValueUpDataFrame.Create(nil);
  ValueQuantity.Parent := LayoutQuantity;
  ValueQuantity.Align := TAlignLayout.Client;
  ValueQuantity.Step := STEP_QUANTITY;
  ValueQuantity.Title := 'Количество:';
  ValueQuantity.Value := 0.001;

  ValueRSI := TValueUpDataFrame.Create(nil);
  ValueRSI.Parent := LayoutRSI;
  ValueRSI.Align := TAlignLayout.Client;
  ValueRSI.Step := STEP_RSI;
  ValueRSI.Title := 'RSI:';
  ValueRSI.Value := 50;

  ValueSlip := TValueUpDataFrame.Create(nil);
  ValueSlip.Parent := LayoutSlip;
  ValueSlip.Align := TAlignLayout.Client;
  ValueSlip.Step := STEP_SLIP;
  ValueSlip.Title := 'Slip:';
  ValueSlip.Value := 10;
end;

destructor TOrderFrame.Destroy;
begin
  FreeAndNil(ValueSlip);
  FreeAndNil(ValueRSI);
  FreeAndNil(ValueQuantity);
  inherited;
end;

procedure TOrderFrame.SetOperationTrade;

  function _GetPrice: Double;
  begin
    Result := 0;
//    case FSide of
//      tsBuy: begin
//        Result :=
//          Indicator.CurrentCandel.Close +
//          ValueSlip.Value * 0.01;
//      end;
//      tsSell: begin
//        Result :=
//          Indicator.CurrentCandel.Close -
//          ValueSlip.Value * 0.01;
//      end;
//    end;
  end;


var
  xS: String;
  xPlaceOrder: TParamOrder;
begin
  // Инструмент отслеживания
  // Передача ключей программе

  xPlaceOrder := TParamOrder.Create;
  try
    xPlaceOrder.TypeProc    := TParamOrder.TTypeProc.Place;

    {todo: сохранение данных}
    xPlaceOrder.Category    := TTypeCategory.tcLinear;
    xPlaceOrder.Symbol      := 'BTCUSDT';
    xPlaceOrder.Side        := FSide;
    xPlaceOrder.PositionIdx := 0;
    xPlaceOrder.OrderType   := TTypeOrder.Limit;
    xPlaceOrder.Qty         := ValueQuantity.Value;

    //xPlaceOrder.Price       := Indicator.CurrentCandel.Close;

    xPlaceOrder.timeInForce := TTypeTimeInForce.GTC;
    xPlaceOrder.OrderLinkId := 'test' + Random(65000).ToString;
    //xS :=
    SelectedOrder(
       API_KEY,
       API_SECRET,
       xPlaceOrder,
       OrderResponse
    );
    //ShowMessage(xS);
  finally
    FreeAndNil(xPlaceOrder);
  end;
end;

procedure TOrderFrame.SetSide(const Value: TTypeSide);
begin
  FSide := Value;
  case FSide of
    tsBuy: begin
      Rectangle.Fill.Color := TAlphaColorRec.Green;
    end;
    tsSell: begin
      Rectangle.Fill.Color := TAlphaColorRec.Red;
    end;
  end;
end;

procedure TOrderFrame.SetUpData;
//var
//  xRSI: Double;
begin
  if not CheckBoxOrder.IsChecked then
    Exit;

//  xRSI := Indicator.RSI.Current.Value;
//  case FSide of
//    tsBuy:
//      if ValueRSI.Value < xRSI then
//      begin
//        Self.SetOperationTrade;
//        CheckBoxOrder.IsChecked := False;
//      end;
//    tsSell:
//      if ValueRSI.Value > xRSI then
//      begin
//        Self.SetOperationTrade;
//        CheckBoxOrder.IsChecked := False;
//      end;
//  end;
end;

procedure TOrderFrame.Button1Click(Sender: TObject);
begin
  Self.SetOperationTrade;
end;


end.
