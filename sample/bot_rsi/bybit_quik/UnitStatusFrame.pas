unit UnitStatusFrame;

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
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.Edit,
  Lb.SysUtils,
  Lb.Bybit.Trade,
  Lb.Bybit.SysUtils,
  Lb.HistoryIndicator,
  Lb.Bybit.Position;

type
  ///<summary>
  /// Статус информации
  ///</summary>
  ///<remarks>
  /// Здесь управления — осуществляется чтение
  ///</remarks>
  TStatusFrame = class(TFrame)
    Rectangle1: TRectangle;
    EditQty: TEdit;
    EditValueRSI: TEdit;
    Timer: TTimer;
    ButtonSell: TButton;
    ButtonBuy: TButton;
    EditMsgOrder: TEdit;
    procedure TimerTimer(Sender: TObject);
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonSellClick(Sender: TObject);
  private
    FOnParams: TNotifyEvent;
    FHistoryIndicator: THistoryIndicator;
    FInstrumentPrice: TInstrumentPrice;
    FBybitPosition: TBybitPosition;
    procedure EventResponse(ASander: TObject; ATypeObject: TTypeObject);
    procedure EventBybitPositionEndLoading(Sender: TObject);
    function GetIsActive: Boolean;
  protected
    procedure DoParams;
    property HistoryIndicator: THistoryIndicator read FHistoryIndicator;
    property InstrumentPrice: TInstrumentPrice read FInstrumentPrice;
  public
    SumVal: Double;
    SumQty: Double;
  public
    ValueRSI: Double;
    Bid, Ask: Double;
    Qty: Double;
    Side: TTypeSide;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure SetOperationTrade(ASide: TTypeSide; APrice: Double; AQty: Double);
    property IsActive: Boolean read GetIsActive;
    property OnParams: TNotifyEvent write FOnParams;
  end;

implementation

{$R *.fmx}

{ TStatusFrame }


constructor TStatusFrame.Create(AOwner: TComponent);
begin
  inherited;
  FHistoryIndicator := THistoryIndicator.Create;
  FHistoryIndicator.OnResponse := EventResponse;

  FInstrumentPrice := TInstrumentPrice.Create;
  FInstrumentPrice.OnResponse := EventResponse;

  FBybitPosition := TBybitPosition.Create;
  FBybitPosition.OnEventEndLoading := EventBybitPositionEndLoading;
end;

destructor TStatusFrame.Destroy;
begin
  FreeAndNil(FBybitPosition);
  FreeAndNil(FInstrumentPrice);
  FreeAndNil(FHistoryIndicator);
  inherited;
end;

procedure TStatusFrame.Start;
begin
  if not Timer.Enabled then
  begin
    SumVal := 0;
    SumQty := 0;

    Timer.Enabled := True;

    HistoryIndicator.Symbol   := ParamApplication.Symble;
    HistoryIndicator.Category := ParamApplication.Category;
    HistoryIndicator.Interval := ParamApplication.Interval;
    HistoryIndicator.UpDate;

    InstrumentPrice.Symbol   := ParamApplication.Symble;
    InstrumentPrice.Category := ParamApplication.Category;
    InstrumentPrice.Limit    := 10;

    FBybitPosition.Symbol := ParamApplication.Symble;
    FBybitPosition.Category := ParamApplication.Category;
    FBybitPosition.SetEncryption(
      ParamApplication.ApiKey,
      ParamApplication.ApiSecret
    );
  end;
end;

procedure TStatusFrame.Stop;
begin
  if Timer.Enabled then
    Timer.Enabled := False;
end;


procedure TStatusFrame.TimerTimer(Sender: TObject);
begin
  HistoryIndicator.UpDate;
  InstrumentPrice.UpDate;
  FBybitPosition.Selected;
end;

procedure TStatusFrame.EventBybitPositionEndLoading(Sender: TObject);
var
  xF: TFormatSettings;
  xS: String;
begin
  Qty := 0;
  if FBybitPosition.PositionObjects.Count > 0 then
  begin
    xS := FBybitPosition.PositionObjects[0].Size;
    if not xS.IsEmpty then
    begin
      xF := FormatSettings;
      xF.DecimalSeparator := '.';
      Qty := StrToFloatDef(xS,0,xF);
    end;
    if Qty > 0 then
      Side := GetTypeSideToStr(FBybitPosition.PositionObjects[0].Side);
  end;
  if Qty > 0 then
    EditQty.Text := '(' + GetStrToTypeSide(Side) + ')' + FloatToStr(Qty)
  else
    EditQty.Text := '';
end;

procedure TStatusFrame.EventResponse(ASander: TObject; ATypeObject: TTypeObject);
begin
  case ATypeObject of
    tobHistoryIndicator: begin
      ValueRSI := HistoryIndicator.RSI.Current.AvgValue;
    end;
    tobInstrumentPrice: begin
      Bid := InstrumentPrice.Bid;
      Ask := InstrumentPrice.Ask;
    end;
  end;

  EditValueRSI.Text := 'RSI:' + FloatToStr(ValueRSI) +
  ' Price: ' + FloatToStr(Ask) + '/' + FloatToStr(Bid);

  if (ValueRSI > 0) and (Ask > 0) and (Bid > 0) then
    DoParams;
end;

function TStatusFrame.GetIsActive: Boolean;
begin
  Result := Timer.Enabled;
end;

procedure TStatusFrame.DoParams;
begin
  if Assigned(FOnParams) then
    FOnParams(Self);
end;

procedure TStatusFrame.SetOperationTrade(ASide: TTypeSide; APrice: Double; AQty: Double);
var
  xPlaceOrder: TParamOrder;
  xResponse: TOrderResponse;
begin
  try
    // Инструмент отслеживания
    // Передача ключей программе
    xPlaceOrder := TParamOrder.Create;
    try
      xPlaceOrder.TypeProc    := TParamOrder.TTypeProc.Place;

      {todo: сохранение данных}
      xPlaceOrder.Category    := TTypeCategory.tcLinear;
      xPlaceOrder.Symbol      := ParamApplication.Symble;
      xPlaceOrder.Side        := ASide;
      xPlaceOrder.PositionIdx := 0;
      xPlaceOrder.OrderType   := TTypeOrder.Limit;
      xPlaceOrder.Qty         := AQty;
      xPlaceOrder.Price       := APrice;
      xPlaceOrder.timeInForce := TTypeTimeInForce.GTC;
      xPlaceOrder.OrderLinkId := 'test' + Random(65000).ToString;

      xResponse := TOrderResponse.Create;
      try
        SelectedOrder(
           ParamApplication.ApiKey,
           ParamApplication.ApiSecret,
           xPlaceOrder,
           xResponse // Возрат сообщение ос делке
        );
        EditMsgOrder.Text := xResponse.RetMsg;
      finally
        FreeAndNil(xResponse);
      end;

    finally
      FreeAndNil(xPlaceOrder);
    end;
  except
    on E: Exception do
      raise Exception.Create('Error Message:' + E.Message);
  end;
end;

procedure TStatusFrame.ButtonBuyClick(Sender: TObject);
begin
  SetOperationTrade(TTypeSide.tsBuy,Ask,0.01);
end;

procedure TStatusFrame.ButtonSellClick(Sender: TObject);
begin
  SetOperationTrade(TTypeSide.tsSell,Bid,0.01);
end;

end.
