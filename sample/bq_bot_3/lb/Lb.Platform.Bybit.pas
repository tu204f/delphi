unit Lb.Platform.Bybit;

interface

{$I debug.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  Lb.SysUtils,
  Lb.Platform,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline,
  Lb.Bybit.OrderBook;

type
  ///<summary>
  /// ���������� �������� Bybit
  ///</summary>
  TPlatfomBybit = class(TTradingPlatform)
  private
    FApiKey: String;
    FApiSecret: String;
  private
    FCountSelected: Integer;
    FBybitKline: TBybitKline;
    FBybitOrderBook: TBybitOrderBook;
    procedure BybitKlineOnEventEndLoading(ASender: TObject);
    procedure BybitOrderBookOnEventEndLoading(ASender: TObject);
    procedure BybitOnEventException(ASender: TObject);
    procedure BybitKlineOnNewCandel(ASender: TObject);
    procedure SetInterval(const Value: TTypeInterval);
  protected
    procedure DoSelected; override;
    procedure DoMsgInfo(S: String); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    ///<summary>
    /// ���� ������������� ������ ��������� ������
    ///</summary>
    function SendTrade(const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell): String; override;

    property Interval: TTypeInterval write SetInterval;
  public
    property ApiKey: String read FApiKey write FApiKey;
    property ApiSecret: String read FApiSecret write FApiSecret;
  end;

implementation


uses
{$IFDEF DEBUG}
  Lb.Logger,
{$ENDIF}
  Lb.Bybit.Trade;

{ TPlatfomBybit }

constructor TPlatfomBybit.Create;
begin
  inherited;

//  if not BybitHostTest then
//    BybitHostTest := True;

  FCountSelected := 0;

  FBybitKline := TBybitKline.Create;
  FBybitKline.OnEventEndLoading := BybitKlineOnEventEndLoading;
  FBybitKline.OnEventException := BybitOnEventException;
  FBybitKline.OnNewCandel := BybitKlineOnNewCandel;

  FBybitOrderBook := TBybitOrderBook.Create;
  FBybitOrderBook.OnEventEndLoading := BybitOrderBookOnEventEndLoading;
  FBybitOrderBook.OnEventException := BybitOnEventException;
end;

destructor TPlatfomBybit.Destroy;
begin
  FreeAndNil(FBybitKline);
  inherited;
end;

procedure TPlatfomBybit.DoMsgInfo(S: String);
var
  xS: String;
begin
  xS := 'bybit_info ' + S;
  inherited DoMsgInfo(xS);
end;

procedure TPlatfomBybit.DoSelected;
begin
  if FCountSelected > 0 then
    Exit;

  FBybitKline.Symbol := Symbol;

  {todo: ��������� ��� ��������� � ���������}
  FBybitKline.Category := TTypeCategory.tcLinear;
  //FBybitKline.Interval := TTypeInterval.ti_60;
  FBybitKline.Limit    := 5000;
  FBybitKline.Selected;

  FBybitOrderBook.Category := TTypeCategory.tcLinear;
  FBybitOrderBook.Symbol := Symbol;
  FBybitOrderBook.Selected;

  FCountSelected := 2;
  inherited DoSelected;
end;

procedure TPlatfomBybit.BybitKlineOnEventEndLoading(ASender: TObject);

  function _ToCandel(ACandelObject: TCandelObject): TCandel;
  var
    xCandel: TCandel;
  begin
    xCandel.Time  := ACandelObject.DateTimeUnix;
    xCandel.Open  := ACandelObject.Open;
    xCandel.High  := ACandelObject.High;
    xCandel.Low   := ACandelObject.Low;
    xCandel.Close := ACandelObject.Close;
    xCandel.Vol   := ACandelObject.Vol;
    Result := xCandel;
  end;

begin
{$IFDEF DBG_BYBIT_CANDEL_OBJECT}
  TLogger.Log('TPlatfomBybit.BybitKlineOnEventEndLoading');
{$ENDIF}
  FStateMarket.Candels.Clear;
  for var xCandelObject in FBybitKline.CandelObjects do
  begin
{$IFDEF DBG_BYBIT_CANDEL_OBJECT}
    TLogger.LogTree(3,
      DateTimeToStr(xCandelObject.DateTime) + ' ' +
      xCandelObject.Close.ToString
    );
{$ENDIF}
    FStateMarket.Candels.Add(
      _ToCandel(xCandelObject)
    );
  end;
  Dec(FCountSelected);
  if FCountSelected = 0 then
    DoStateMarke;
{$IFDEF DBG_BYBIT_CANDEL_OBJECT}
  TLogger.Log('**********************************************');
{$ENDIF}
end;

procedure TPlatfomBybit.BybitOrderBookOnEventEndLoading(ASender: TObject);
var
  xBid, xAsk: Double;
begin
  xBid := FBybitOrderBook.OrderBook.Bid;
  xAsk := FBybitOrderBook.OrderBook.Ask;

  for var xOrderMarket in FBybitOrderBook.OrderBook.Bids do
  begin
    if xOrderMarket.SumQuantity >= FStateMarket.Qty then
    begin
      xBid := xOrderMarket.Price;
      Break;
    end;
  end;

  for var xOrderMarket in FBybitOrderBook.OrderBook.Asks do
  begin
    if xOrderMarket.SumQuantity >= FStateMarket.Qty then
    begin
      xAsk := xOrderMarket.Price;
      Break;
    end;
  end;

  Dec(FCountSelected);
  if (FCountSelected = 0) and (xBid > 0) and (xAsk > 0) then
  begin
    FStateMarket.SetPrice(xAsk,xBid);
    DoStateMarke;
  end;
end;


procedure TPlatfomBybit.BybitOnEventException(ASender: TObject);
begin
  raise Exception.Create(TBybitHttpClient(ASender).ValueMessage);
end;

function TPlatfomBybit.SendTrade(const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell): String;

  function _CreateOrderLinkId: String;
  var
    xS: String;
  begin
    xS := Random(65000).ToString;
    Result := xS;
  end;

  function _ToTypeSide(ASide: TTypeBuySell): TTypeSide;
  begin
    case ASide of
      TTypeBuySell.tsBuy: Result := TTypeSide.tsBuy;
      TTypeBuySell.tsSell: Result := TTypeSide.tsSell;
    else
      Result := TTypeSide.tsNull;
    end;
  end;

var
  xPlaceOrder: TParamOrder;
  xResponse: TOrderResponse;
  xLinkId: String;
begin
  xLinkId := _CreateOrderLinkId;
  Result := xLinkId;
  try
    // ���������� ������������
    // �������� ������ ���������
    xPlaceOrder := TParamOrder.Create;
    try
      xPlaceOrder.TypeProc    := TParamOrder.TTypeProc.Place;

      {todo: ���������� ������}
      xPlaceOrder.Category    := TTypeCategory.tcLinear;
      xPlaceOrder.Symbol      := Symbol;

      xPlaceOrder.Side        := _ToTypeSide(ASide);

      xPlaceOrder.PositionIdx := 0;
      xPlaceOrder.OrderType   := TTypeOrder.Market;
      xPlaceOrder.Qty         := AQty;
      xPlaceOrder.Price       := APrice;
      xPlaceOrder.timeInForce := TTypeTimeInForce.GTC;

      xPlaceOrder.OrderLinkId := xLinkId;

      xResponse := TOrderResponse.Create;
      try
        // �������� �������� ��������
        SelectedOrder(
           FApiKey,
           FApiSecret,
           xPlaceOrder,
           xResponse // ������ ��������� �� �����
        );
        DoMsgInfo(xResponse.Value + ' ' + xResponse.RetMsg);
      finally
        FreeAndNil(xResponse);
      end;
    finally
      FreeAndNil(xPlaceOrder);
    end;
  except
    on E: Exception do
      raise Exception.Create('Error Message: LinkID: [' + xLinkId + '].' + E.Message);
  end;
end;

procedure TPlatfomBybit.SetInterval(const Value: TTypeInterval);
begin
  FBybitKline.Interval := Value;
end;

procedure TPlatfomBybit.BybitKlineOnNewCandel(ASender: TObject);
begin
  DoNewCandel;
end;

end.
