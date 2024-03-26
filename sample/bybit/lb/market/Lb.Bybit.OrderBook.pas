(******************************************************************************)
(* Get Orderbook                                                              *)
(* Запрос данных о глубине книги заказов.                                     *)
(* Графики возвращаются группами на основе запрошенного интервала.            *)
(******************************************************************************)
unit Lb.Bybit.OrderBook;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.JSON,
  Lb.Bybit.SysUtils;

type
  TOrderMarket = record
    jsonPrice: String;
    jsonQuantity: String;
  private
    FSumQuantity: Double;
    function GetPrice: Double;
    function GetQuantity: Double;
  public
    constructor Create(const AJsonPrice, AJsonQuantity: String);
    procedure SetSumQuantity(const ASumQuantity: Double);
    function ToString: String;
    property Price: Double read GetPrice;
    property Quantity: Double read GetQuantity;
    property SumQuantity: Double read FSumQuantity;
  end;
  TOrderMarketList = TList<TOrderMarket>;

  TOrderRow = record
    Price: Double;       // Цена
    Quantity: Double;    // Количество
    SumQuantity: Double; // Сумма
    BuySell: TTypeSide;  // Напровление
    MyQuantity: Double;  // Свое количество
  public
    constructor Create(const APrice, AQuantity, ASumQuantity, AMyQuantity: Double; const ABuySell: TTypeSide);
    function ToString: String;
  end;
  TOrderRowList = TList<TOrderRow>;


  ///<summary>Стакан</summary>
  TOrderBook = class(TObject)
  private
    FBids: TOrderMarketList;
    FAsks: TOrderMarketList;
    FOrderRows: TOrderRowList;
    function GetBid: Double;
    function GetBidQuantity: Double;
    function GetAsk: Double;
    function GetAskQuantity: Double;
    function GetSpred: Double;
  protected
    procedure SetParser(const AValueBid, AValueAsk: TJSONValue);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Bids: TOrderMarketList read FBids;
    property Asks: TOrderMarketList read FAsks;
    property OrderRows: TOrderRowList read FOrderRows;
  public
    property Bid: Double read GetBid;
    property BidQuantity: Double read GetBidQuantity;
    property Ask: Double read GetAsk;
    property AskQuantity: Double read GetAskQuantity;
    property Spred: Double read GetSpred;
  end;

  ///<summary>Получаем котировальный стакан - bybit </summary>
  TBybitOrderBook = class(TBybitHttpClient)
  private
    FCategory: TTypeCategory;
    FSymbol: String;
    FLimit: Integer;
    procedure SetCategory(const Value: TTypeCategory);
    procedure SetLimit(const Value: Integer);
    procedure SetSymbol(const Value: String);
  private
    FOrderBook: TOrderBook;
  protected
    procedure DoEventParser; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  public {Request Parameters}
    ///<summary>Имя символа</summary>
    property Symbol: String read FSymbol write SetSymbol;
    ///<summary>Тип продукта. spot, linear, inverse, option</summary>
    property Category: TTypeCategory read FCategory write SetCategory;
    ///<summary>Limit size for each bid and ask</summary>
    property Limit: Integer read FLimit write SetLimit;
  public
    property OrderBook: TOrderBook read FOrderBook;
  end;

implementation

{ TOrderMarket }

constructor TOrderMarket.Create(const AJsonPrice, AJsonQuantity: String);
begin
  jsonPrice := AJsonPrice;
  jsonQuantity := AJsonQuantity;
end;

function TOrderMarket.ToString: String;
begin
  Result := 'JS:' + jsonPrice + ' ' + jsonQuantity + 'R: ' + SumQuantity.ToString;
end;

function TOrderMarket.GetPrice: Double;
begin
  Result := GetStrToFloat(jsonPrice);
end;

function TOrderMarket.GetQuantity: Double;
begin
  Result := GetStrToFloat(jsonQuantity);
end;

procedure TOrderMarket.SetSumQuantity(const ASumQuantity: Double);
begin
  FSumQuantity := ASumQuantity;
end;

{ TOrderRow }

constructor TOrderRow.Create(const APrice, AQuantity, ASumQuantity, AMyQuantity: Double;
  const ABuySell: TTypeSide);
begin
  Price     := APrice;
  Quantity  := AQuantity;
  BuySell   := ABuySell;
  MyQuantity:= AMyQuantity;
  SumQuantity := ASumQuantity;
end;

function TOrderRow.ToString: String;
begin
  Result :=
    Price.ToString + ' ' +
    Quantity.ToString + ' ' +
    GetStrToTypeSide(BuySell) + ' ' +
    MyQuantity.ToString + ' ' +
    SumQuantity.ToString;
end;

{ TOrderBook }

constructor TOrderBook.Create;
begin
  FBids := TOrderMarketList.Create;
  FAsks := TOrderMarketList.Create;
  FOrderRows := TOrderRowList.Create;
end;

destructor TOrderBook.Destroy;
begin
  FreeAndNil(FOrderRows);
  FreeAndNil(FAsks);
  FreeAndNil(FBids);
  inherited;
end;

procedure TOrderBook.SetParser(const AValueBid, AValueAsk: TJSONValue);

  function _GetMarketOrder(AJsonMarket: TJSONArray): TOrderMarket;
  begin
    Result := TOrderMarket.Create(
      GetStrToJson(AJsonMarket.Items[0]),
      GetStrToJson(AJsonMarket.Items[1])
    );
  end;

  procedure _SetMarketOrders(AMarketOrders: TOrderMarketList; AJsonMarkets: TJSONArray);
  var
    xValue: TJSONValue;
    xSumQuantity: Double;
    xMarketOrder: TOrderMarket;
  begin
    xSumQuantity := 0;
    AMarketOrders.Clear;
    for xValue in AJsonMarkets do
    begin
      xMarketOrder := _GetMarketOrder(TJSONArray(xValue));
      xSumQuantity := xSumQuantity + xMarketOrder.Quantity;
      xMarketOrder.SetSumQuantity(xSumQuantity);
      AMarketOrders.Add(xMarketOrder);
    end;
  end;

  procedure _OrderRows;
  var
    xOrderRow: TOrderRow;
    xOrderMarket: TOrderMarket;
  begin
    FOrderRows.Clear;

    // Заявка на продажу
    for var i := FAsks.Count - 1 downto 0 do
    begin
      xOrderMarket := FAsks[i];
      xOrderRow := TOrderRow.Create(
        xOrderMarket.Price,
        xOrderMarket.Quantity,
        xOrderMarket.SumQuantity,
        0,
        TTypeSide.tsSell
      );
      FOrderRows.Add(xOrderRow);
    end;

    // Заявка на покупку
    for var i := 0 to FBids.Count - 1 do
    begin
      xOrderMarket := FBids[i];
      xOrderRow := TOrderRow.Create(
        xOrderMarket.Price,
        xOrderMarket.Quantity,
        xOrderMarket.SumQuantity,
        0,
        TTypeSide.tsBuy
      );
      FOrderRows.Add(xOrderRow);
    end;

  end;


begin
  _SetMarketOrders(FBids,TJSONArray(AValueBid));
  _SetMarketOrders(FAsks,TJSONArray(AValueAsk));
  _OrderRows;
end;

function TOrderBook.GetAsk: Double;
begin
  Result := 0;
  if FAsks.Count > 0 then
    Result := FAsks[0].Price;
end;

function TOrderBook.GetAskQuantity: Double;
begin
  Result := 0;
  if FAsks.Count > 0 then
    Result := FAsks[0].Quantity;
end;

function TOrderBook.GetBid: Double;
begin
  Result := 0;
  if FBids.Count > 0 then
    Result := FBids[0].Price;
end;

function TOrderBook.GetBidQuantity: Double;
begin
  Result := 0;
  if FBids.Count > 0 then
    Result := FBids[0].Quantity;
end;

function TOrderBook.GetSpred: Double;

  function _Round(const AValue: Double): Double;
  begin
    Result := RoundTo(AValue,-10)
  end;

var
  xSpred: Double;
begin
  xSpred := Self.Ask -  Self.Bid;
  Result := _Round(xSpred);
end;

{ TBybitOrderBook }

constructor TBybitOrderBook.Create;
begin
  inherited;
  BybitModule.TypeHttp := TTypeHttp.thGet;
  BybitModule.Module := '/v5/market/orderbook';

  FOrderBook := TOrderBook.Create;
end;

destructor TBybitOrderBook.Destroy;
begin
  FreeAndNil(FOrderBook);
  inherited;
end;

procedure TBybitOrderBook.DoEventParser;
var
  xBid, xAsk: TJSONValue;
begin
  inherited;
  xBid := Response.ResultObject.Values['b'];
  xAsk := Response.ResultObject.Values['a'];
  try
    FOrderBook.SetParser(xBid,xAsk);
  except
    on E: Exception do begin
      raise Exception.Create('Error Message: TBybitOrderBook.DoEventParser');
    end;
  end;
end;

procedure TBybitOrderBook.SetCategory(const Value: TTypeCategory);
begin
  FCategory := Value;
  BybitModule.Params.SetParam('category',GetStrToTypeCategory(FCategory));
end;


procedure TBybitOrderBook.SetSymbol(const Value: String);
begin
  FSymbol := Value;
  BybitModule.Params.SetParam('symbol',FSymbol);
end;

procedure TBybitOrderBook.SetLimit(const Value: Integer);
begin
  FLimit := Value;
  BybitModule.Params.SetParam('limit',FLimit.ToString);
end;



end.
