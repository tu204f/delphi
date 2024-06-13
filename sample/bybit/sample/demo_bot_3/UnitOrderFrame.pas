unit UnitOrderFrame;

interface

{$I demo_bot.inc}

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
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.Objects,
  FMX.Layouts,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  UnitEditFrame,
  UnitLineEditFrame;

type
  TParam = record
    Symbol: Double;
    Side: TTypeSide;
    Step: Double;
    Quantity: Double;
    ValueRSI: Double;
    ActiveValueRSI: Double;
  end;

  ///<summary>Ордер</summary>
  TOrderFrame = class(TFrame)
    Rectangle: TRectangle;
    LayoutClient: TLayout;
    ButtonOrder: TButton;
    Timer1: TTimer;
    Memo1: TMemo;
    TextPrice: TText;
    LayoutPrice: TLayout;
    GridPanelOrder: TGridPanelLayout;
    LayoutLongRSI: TLayout;
    LayoutMediumRSI: TLayout;
    LayoutShortSRI: TLayout;
    procedure ButtonOrderClick(Sender: TObject);
  public const
    {todo: Получать эти параметры}
    //STEP_PRICE    = 0.01;
    STEP_QUANTITY = 0.01;
    STEP_RSI      = 1;
    STEP_SLIP     = 1;
  private
    FValueRSI: Double;
  private
    FSymbol: String;
    FSide: TTypeSide;
    FPrice: Double;
    procedure SetSide(const Value: TTypeSide);
    procedure SetPrice(const Value: Double);
    procedure SetSymbol(const Value: String);
    procedure SetStepQuantity(const Value: Double);
    procedure SetQuantity(const Value: Double);
  protected
    BybitRealTime: TBybitRealTime;
    OrderResponse: TOrderResponse;
    procedure SetOperationTrade;
    procedure EventOperationTrade(Sender: TObject);
  public
    QtyValueFrame: TValueFrame;
    LongLineEditFrame: TLineEditFrame;
    MediumLineEditFrame: TLineEditFrame;
    ShortLineEditFrame: TLineEditFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetUpData(const AValueRSI: Double);
    {todo: Вывести настраивые панели}
    property Symbol: String write SetSymbol;
    property Side: TTypeSide write SetSide;
    property Price: Double write SetPrice;
    property StepQuantity: Double write SetStepQuantity;
    property Quantity: Double write SetQuantity;
  public
    procedure SetSave;
    procedure SetLoad;
  end;

implementation

{$R *.fmx}

uses
  Lb.Logger,
  Lb.VirtualTrade;

const
  {todo: лист вывести в настройки}
  API_KEY    = 'UAbp8xH59wGkdCtP8N';
  API_SECRET = 'RUVaS6CDSoRI2Mpck23pNwLfwlsdyPLIhaBW';

{ TOrderFrame }


constructor TOrderFrame.Create(AOwner: TComponent);

  function _GetCreateLineEditFrame(const ATitle: String; const ALayout: TLayout): TLineEditFrame;
  var
    xLine: TLineEditFrame;
  begin
    xLine := TLineEditFrame.Create(nil);
    xLine.Parent := ALayout;
    xLine.Align := TAlignLayout.Client;
    xLine.TextTitle.Text := ATitle;
    xLine.OnOperationTrade := EventOperationTrade;
    Result := xLine;
  end;

begin
  inherited Create(AOwner);
  OrderResponse := TOrderResponse.Create;

  QtyValueFrame := TValueFrame.Create(nil);
  QtyValueFrame.Parent := LayoutPrice;
  QtyValueFrame.Align := TAlignLayout.Client;
  QtyValueFrame.MaxValue := 100;
  QtyValueFrame.MinValue := 0;
  QtyValueFrame.Step := STEP_QUANTITY;

  LongLineEditFrame := _GetCreateLineEditFrame('Длинное значение',LayoutLongRSI);
  MediumLineEditFrame := _GetCreateLineEditFrame('Средние значение',LayoutMediumRSI);
  ShortLineEditFrame := _GetCreateLineEditFrame('Короткое значение',LayoutShortSRI);
end;

destructor TOrderFrame.Destroy;
begin
  FreeAndNil(ShortLineEditFrame);
  FreeAndNil(MediumLineEditFrame);
  FreeAndNil(LongLineEditFrame);

  FreeAndNil(QtyValueFrame);
  FreeAndNil(OrderResponse);
  inherited;
end;

procedure TOrderFrame.SetLoad;
begin
  LongLineEditFrame.SetLoad;
  MediumLineEditFrame.SetLoad;
  ShortLineEditFrame.SetLoad;
end;

procedure TOrderFrame.SetSave;
begin
  LongLineEditFrame.SetSave;
  MediumLineEditFrame.SetSave;
  ShortLineEditFrame.SetSave;
end;


procedure TOrderFrame.EventOperationTrade(Sender: TObject);
begin
  SetOperationTrade;
end;

procedure TOrderFrame.SetOperationTrade;
var
  xPlaceOrder: TParamOrder;
begin
  try
    // Инструмент отслеживания
    // Передача ключей программе
    xPlaceOrder := TParamOrder.Create;
    try
      xPlaceOrder.TypeProc    := TParamOrder.TTypeProc.Place;

      {todo: сохранение данных}
      xPlaceOrder.Category    := TTypeCategory.tcLinear;
      xPlaceOrder.Symbol      := FSymbol;
      xPlaceOrder.Side        := FSide;
      xPlaceOrder.PositionIdx := 0;
      xPlaceOrder.OrderType   := TTypeOrder.Limit;
      xPlaceOrder.Qty         := QtyValueFrame.Value;
      xPlaceOrder.Price       := FPrice;
      xPlaceOrder.timeInForce := TTypeTimeInForce.GTC;
      xPlaceOrder.OrderLinkId := 'test' + Random(65000).ToString;

      {$IFDEF VERTUAL_TRADE}
      SetVirtualOrderSelectedOrder(xPlaceOrder,0,0,0);
      {$ELSE}
      SelectedOrder(
          API_KEY,
          API_SECRET,
         xPlaceOrder,
         OrderResponse
      );
      {$ENDIF}

      Memo1.Lines.Clear;
      Memo1.Lines.Add(OrderResponse.Value);

    finally
      FreeAndNil(xPlaceOrder);
    end;
  except
    on E: Exception do
      Memo1.Lines.Add(E.Message);
  end
end;


procedure TOrderFrame.SetSide(const Value: TTypeSide);
begin
  FSide := Value;
  LongLineEditFrame.NameSetting := 'long_' + GetStrToTypeSide(FSide);
  MediumLineEditFrame.NameSetting := 'medium_' + GetStrToTypeSide(FSide);
  ShortLineEditFrame.NameSetting := 'short' + GetStrToTypeSide(FSide);
  case FSide of
    tsBuy: begin
      TextPrice.TextSettings.FontColor := TAlphaColorRec.Green;
    end;
    tsSell: begin
      TextPrice.TextSettings.FontColor := TAlphaColorRec.Red;
    end;
  end;
end;

procedure TOrderFrame.SetPrice(const Value: Double);
begin
  FPrice := Value;
  TextPrice.Text := FSymbol + ':' + FloatToStr(Value);
end;

procedure TOrderFrame.SetStepQuantity(const Value: Double);
begin
  QtyValueFrame.Step := Value;
end;

procedure TOrderFrame.SetQuantity(const Value: Double);
begin
  QtyValueFrame.Value := Value;
end;

procedure TOrderFrame.SetSymbol(const Value: String);
begin
  FSymbol := Value;
end;

procedure TOrderFrame.SetUpData(const AValueRSI: Double);
begin
  {$IFDEF DEBUG_TRADE}
  TLogger.LogText('*',80);
  TLogger.LogTree(0,'TOrderFrame.SetUpData:');
  TLogger.LogTreeText(3,'RSI:' + AValueRSI.ToString);
  {$ENDIF}
  TextPrice.Text := FSymbol + ':' + FPrice.ToString;
  FValueRSI := AValueRSI;

  {$IFDEF DEBUG_TRADE}
  LongLineEditFrame.SetUpData(FSide,AValueRSI);
  {$ELSE}
  LongLineEditFrame.SetUpData(FSide,AValueRSI);
  MediumLineEditFrame.SetUpData(FSide,AValueRSI);
  ShortLineEditFrame.SetUpData(FSide,AValueRSI);
  {$ENDIF}
end;

procedure TOrderFrame.ButtonOrderClick(Sender: TObject);
begin
  Self.SetOperationTrade;
end;

end.
