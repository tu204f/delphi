unit UnitOrderFrame;

interface

{$I demo_bot.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
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
  UnitLineEditFrame,
  Lb.VirtualTrade;

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
    GridPanelLine: TGridPanelLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    Layout5: TLayout;
    Layout6: TLayout;
    Text1: TText;
    Text2: TText;
    LayoutClose: TLayout;
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
  protected
    BybitRealTime: TBybitRealTime;
    OrderResponse: TOrderResponse;
    procedure SetOperationTrade(ASide: TTypeSide; AQty: Double;  ATypeLine: TTypeLine; ATypeStr: TTypeStr);
    procedure EventOperationTrade(Sender: TObject; ASide: TTypeSide; AQty: Double);
  public

    LineEditFrame1: TLineEditFrame;
    LineEditFrame2: TLineEditFrame;
    LineEditFrame3: TLineEditFrame;
    LineEditFrame4: TLineEditFrame;
    LineEditFrame5: TLineEditFrame;
    LineEditFrame6: TLineEditFrame;

    LineEditFrameClose: TLineEditFrame;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetUpData(const AValueRSI: Double);
    {todo: Вывести настраивые панели}
    property Symbol: String write SetSymbol;
    property Side: TTypeSide write SetSide;
    property Price: Double write SetPrice;
  public
    procedure SetSave;
    procedure SetLoad;
  end;

implementation

{$R *.fmx}

uses
  Lb.Logger;


const
  {todo: лист вывести в настройки}
  API_KEY    = 'V3yApfAKyszSN1bQVV';
  API_SECRET = 'Zkxlw9NYM3GPm5wdygkKt0fV9KciZbFLge7X';

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

  LineEditFrame1 := _GetCreateLineEditFrame('Уровень: 1',Layout1);
  LineEditFrame2 := _GetCreateLineEditFrame('Уровень: 2',Layout2);
  LineEditFrame3 := _GetCreateLineEditFrame('Уровень: 3',Layout3);
  LineEditFrame4 := _GetCreateLineEditFrame('Уровень: 4',Layout4);
  LineEditFrame5 := _GetCreateLineEditFrame('Уровень: 5',Layout5);
  LineEditFrame6 := _GetCreateLineEditFrame('Уровень: 6',Layout6);
  LineEditFrameClose := _GetCreateLineEditFrame('Закрытие',LayoutClose);

end;

destructor TOrderFrame.Destroy;
begin
  FreeAndNil(LineEditFrameClose);
  FreeAndNil(LineEditFrame6);
  FreeAndNil(LineEditFrame5);
  FreeAndNil(LineEditFrame4);
  FreeAndNil(LineEditFrame3);
  FreeAndNil(LineEditFrame2);
  FreeAndNil(LineEditFrame1);

  FreeAndNil(OrderResponse);
  inherited;
end;

procedure TOrderFrame.SetLoad;
begin
  LineEditFrame1.SetLoad;
  LineEditFrame2.SetLoad;
  LineEditFrame3.SetLoad;
  LineEditFrame4.SetLoad;
  LineEditFrame5.SetLoad;
  LineEditFrame6.SetLoad;
  LineEditFrameClose.SetLoad;
end;

procedure TOrderFrame.SetSave;
begin
  LineEditFrame1.SetSave;
  LineEditFrame2.SetSave;
  LineEditFrame3.SetSave;
  LineEditFrame4.SetSave;
  LineEditFrame5.SetSave;
  LineEditFrame6.SetSave;
  LineEditFrameClose.SetSave;
end;

procedure TOrderFrame.EventOperationTrade(Sender: TObject; ASide: TTypeSide; AQty: Double);
begin
  if LineEditFrame1 = Sender then SetOperationTrade(ASide, AQty, TTypeLine.tlLine1,TTypeStr(FSide));
  if LineEditFrame2 = Sender then SetOperationTrade(ASide, AQty, TTypeLine.tlLine2,TTypeStr(FSide));
  if LineEditFrame3 = Sender then SetOperationTrade(ASide, AQty, TTypeLine.tlLine3,TTypeStr(FSide));
  if LineEditFrame4 = Sender then SetOperationTrade(ASide, AQty, TTypeLine.tlLine4,TTypeStr(FSide));
  if LineEditFrame5 = Sender then SetOperationTrade(ASide, AQty, TTypeLine.tlLine5,TTypeStr(FSide));
  if LineEditFrame6 = Sender then SetOperationTrade(ASide, AQty, TTypeLine.tlLine6,TTypeStr(FSide));
  if LineEditFrameClose = Sender then SetOperationTrade(ASide, AQty, TTypeLine.tlCloseLine,TTypeStr(FSide));
end;

procedure TOrderFrame.SetOperationTrade(ASide: TTypeSide; AQty: Double; ATypeLine: TTypeLine; ATypeStr: TTypeStr);
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
      xPlaceOrder.Side        := ASide;
      xPlaceOrder.PositionIdx := 0;
      xPlaceOrder.OrderType   := TTypeOrder.Limit;
      xPlaceOrder.Qty         := AQty;
      xPlaceOrder.Price       := FPrice;
      xPlaceOrder.timeInForce := TTypeTimeInForce.GTC;
      xPlaceOrder.OrderLinkId := 'test' + Random(65000).ToString;

      // Если сработал уровень то отправляем торговый приказ
      {$IFDEF VERTUAL_TRADE}
      SetVirtualOrderSelectedOrder(
        xPlaceOrder,
        FValueRSI,
        ATypeLine,
        ATypeStr
      );
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

  procedure _NameSetting(ALineEditFrame: TLineEditFrame; AName: String);
  begin
    ALineEditFrame.NameSetting := AName;
  end;

begin
  FSide := Value;

  _NameSetting(LineEditFrame1,'line_1' + GetStrToTypeSide(FSide));
  _NameSetting(LineEditFrame2,'line_2' + GetStrToTypeSide(FSide));
  _NameSetting(LineEditFrame3,'line_3' + GetStrToTypeSide(FSide));
  _NameSetting(LineEditFrame4,'line_4' + GetStrToTypeSide(FSide));
  _NameSetting(LineEditFrame5,'line_5' + GetStrToTypeSide(FSide));
  _NameSetting(LineEditFrame6,'line_6' + GetStrToTypeSide(FSide));
  _NameSetting(LineEditFrameClose,'line_close' + GetStrToTypeSide(FSide));

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

  LineEditFrame1.SetUpData(FSide,AValueRSI);
  LineEditFrame2.SetUpData(FSide,AValueRSI);
  LineEditFrame3.SetUpData(FSide,AValueRSI);
  LineEditFrame4.SetUpData(FSide,AValueRSI);
  LineEditFrame5.SetUpData(FSide,AValueRSI);
  LineEditFrame6.SetUpData(FSide,AValueRSI);
  LineEditFrameClose.SetUpData(GetCrossTypeSide(FSide),AValueRSI);
end;

procedure TOrderFrame.ButtonOrderClick(Sender: TObject);
begin
  Self.SetOperationTrade(FSide,0.01,TTypeLine.tlNull,FSide);
end;



end.
