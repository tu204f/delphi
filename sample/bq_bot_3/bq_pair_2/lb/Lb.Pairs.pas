unit Lb.Pairs;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Lb.Bybit.Tickers;

type
  ///<summary>
  /// Финансовый инструмент
  ///</summary>
  TSecurity = class(TObject)
  private
    FSymbol: String;
    FBybitTickers: TBybitTickers;
  private
    FTickerValue: TTickerValue;
    function GetAskPrice: Double;
    function GetBidPrice: Double;
    function GetLast: Double;
  public
    constructor Create(ASymbol: String; ABybitTickers: TBybitTickers); virtual;
    destructor Destroy; override;
    procedure SetUpData;
    ///<summary>Символ инструмента</summary>
    property Symbol: String read FSymbol write FSymbol;
    property Last: Double read GetLast;
    ///<summary>Цена продавца</summary>
    property AskPrice: Double read GetAskPrice;
    ///<summary>Цена покупателя</summary>
    property BidPrice: Double read GetBidPrice;
  end;

  ///<summary>
  /// Тип торговой операции, который совершаем
  ///</summary>
  TTypePair = (
    tpNull,
    tpBuy,
    tpSell,
    tpBuySell
  );

  TValueList = TList<Double>;

  TMaxValueList = class(TValueList)
  public
    function MaxValue: Double;
  end;

  TMinValueList = class(TValueList)
  public
    function MinValue: Double;
  end;


  ///<summary>
  /// Пара инструментов
  ///</summary>
  TPair = class(TObject)
  private
    FSecurityA: TSecurity;
    FSecurityB: TSecurity;
  private
    FTypePair: TTypePair;
    FQtyA, FQtyB: Double;
    FRate, FBuyValue, FSellValue: Double;
    FTradeBuy, FTradeSell: Double;
  private
    FMaxValues: TMaxValueList;
    FMinValues: TMinValueList;
    function GetMaxValue: Double;
    function GetMinValue: Double;
  public
    constructor Create(ASymbolA, ASymbolB: String; ABybitTickers: TBybitTickers); virtual;
    destructor Destroy; override;
    procedure SetUpData;
    property Rate: Double read FRate;
    property BuyValue: Double read FBuyValue;
    property SellValue: Double read FSellValue;
    property QtyA: Double read FQtyA write FQtyA;
    property QtyB: Double read FQtyB write FQtyB;
    property TypePair: TTypePair read FTypePair write FTypePair;
  public
    procedure SetOperationBuy;
    procedure SetOperationSell;
    property TradeBuy: Double read FTradeBuy write FTradeBuy;
    property TradeSell: Double read FTradeSell write FTradeSell;
  public
    property SecurityA: TSecurity read FSecurityA;
    property SecurityB: TSecurity read FSecurityB;
    property MaxValue: Double read GetMaxValue;
    property MinValue: Double read GetMinValue;
  end;
  TPairList = TObjectList<TPair>;

  ///<summary>
  /// Список пар инструментов
  ///</summary>
  TSecurityPairs = class(TObject)
  private
    FPairs: TPairList;
    FBybitTickers: TBybitTickers;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetUpData;
    procedure Clear;
    function GetAddPair(const ASymbolA, ASymbolB: String; const AQtyA, AQtyB: Double): TPair;
    property Pairs: TPairList read FPairs write FPairs;
    property BybitTickers: TBybitTickers read FBybitTickers write FBybitTickers;
  end;

function GetTypePairToStr(const ATypePair: TTypePair): String;

implementation

uses
  Lb.SysUtils,
  Lb.Trading.Pair;

var
  Global_ApiKey: String = 't0YI4Ou0TKOTd7WrkE';
  Global_ApiSecret: String = 'dWcdTGIulDoKOiK4mggPQIkYwmMFGxvFVusp';

function GetRound(const AValue: Double): Double;
begin
  Result := Trunc(AValue * 1000)/1000;
end;


function GetTypePairToStr(const ATypePair: TTypePair): String;
begin
  case ATypePair of
    tpNull: Result := 'null';
    tpBuy: Result := 'buy';
    tpSell: Result := 'sell';
    tpBuySell: Result := 'buy_sell' ;
  else
    Result := 'error.null';
  end;
end;

{ TSecurity }

constructor TSecurity.Create(ASymbol: String; ABybitTickers: TBybitTickers);
begin
  FSymbol := ASymbol;
  FBybitTickers := ABybitTickers;
end;

destructor TSecurity.Destroy;
begin

  inherited;
end;


procedure TSecurity.SetUpData;
var
  xIndex: Integer;
begin
  if not Assigned(FBybitTickers) then
    Exit;
  if FSymbol.IsEmpty then
    Exit;
  xIndex := Self.FBybitTickers.TickerValues.IndexOfSymbol(FSymbol);
  if xIndex >= 0 then
    FTickerValue := FBybitTickers.TickerValues[xIndex];
end;

function TSecurity.GetAskPrice: Double;
begin
  Result := FTickerValue.ask1Price;
end;

function TSecurity.GetBidPrice: Double;
begin
  Result := FTickerValue.bid1Price;
end;

function TSecurity.GetLast: Double;
begin
  Result := FTickerValue.lastPrice;
end;

{ TMaxValueList }

function TMaxValueList.MaxValue: Double;
var
  xValue: Double;
  i: Integer;
begin
  xValue := 0;
  for i := 0 to Count - 1 do
  begin
    if xValue < Self.Items[i] then
      xValue := Self.Items[i];
  end;
  Result := xValue;
end;

{ TMinValueList }

function TMinValueList.MinValue: Double;
var
  xValue: Double;
  i: Integer;
begin
  if Count > 0 then
  begin
    xValue := 999999;
    for i := 0 to Count - 1 do
    begin
      if xValue > Self.Items[i] then
        xValue := Self.Items[i];
    end;
  end
  else
    xValue := 0;
  Result := xValue;
end;

{ TPair }

constructor TPair.Create(ASymbolA, ASymbolB: String; ABybitTickers: TBybitTickers);
begin
  FSecurityA := TSecurity.Create(ASymbolA, ABybitTickers);
  FSecurityB := TSecurity.Create(ASymbolB, ABybitTickers);
  FQtyA := 0.01;
  FQtyB := 0.01;
  FBuyValue := 0;
  FSellValue := 0;
  FTradeBuy := 5;
  FTradeSell := 10;
  FTypePair := TTypePair.tpNull;

  FMaxValues := TMaxValueList.Create;
  FMinValues := TMinValueList.Create;
end;

destructor TPair.Destroy;
begin
  FreeAndNil(FMinValues);
  FreeAndNil(FMaxValues);
  FreeAndNil(FSecurityA);
  FreeAndNil(FSecurityB);
  inherited;
end;

function TPair.GetMaxValue: Double;
begin
  Result := FMaxValues.MaxValue;
end;

function TPair.GetMinValue: Double;
begin
  Result := FMinValues.MinValue;
end;

procedure TPair.SetUpData;
begin
  FSecurityA.SetUpData;
  FSecurityB.SetUpData;

  FRate       := FSecurityA.Last * FQtyA - FSecurityB.Last * FQtyB;
  FSellValue  := FSecurityA.BidPrice * FQtyA - FSecurityB.AskPrice * FQtyB;
  FBuyValue   := FSecurityA.AskPrice * FQtyA - FSecurityB.BidPrice * FQtyB;

  FRate := GetRound(FRate);
  FBuyValue := GetRound(FBuyValue);
  FSellValue := GetRound(FSellValue);

  if FSellValue <> 0 then
    FMaxValues.Add(FSellValue);
  if FBuyValue <> 0 then
    FMinValues.Add(FBuyValue);

  case FTypePair of
    tpBuy: begin
      if FBuyValue <= FTradeBuy then
        SetOperationBuy;
    end;
    tpSell: begin
      if FSellValue >= FTradeSell then
        SetOperationSell;
    end;
    tpBuySell: begin
      if FBuyValue <= FTradeBuy then
        SetOperationBuy;
      if FSellValue >= FTradeSell then
        SetOperationSell;
    end;
  end;

  if FMaxValues.Count > 1000 then
    FMaxValues.Delete(0);

  if FMinValues.Count > 1000 then
    FMinValues.Delete(0);

end;

procedure TPair.SetOperationSell;
var
  xParamTrading: TParamTrading;
begin
  // Продать
  xParamTrading.Symbol    := FSecurityA.Symbol;
  xParamTrading.ApiKey    := Global_ApiKey;
  xParamTrading.ApiSecret := Global_ApiSecret;
  xParamTrading.Time      := GetNewDateTime;
  xParamTrading.Price     := FSecurityA.BidPrice;
  xParamTrading.Qty       := FQtyA;
  xParamTrading.Side      := TTypeBuySell.tsSell;
  xParamTrading.OnMsgInfo := nil;
  SendTrade(xParamTrading);

  // Купить
  xParamTrading.Symbol    := FSecurityB.Symbol;
  xParamTrading.ApiKey    := Global_ApiKey;
  xParamTrading.ApiSecret := Global_ApiSecret;
  xParamTrading.Time      := GetNewDateTime;
  xParamTrading.Price     := FSecurityB.AskPrice;
  xParamTrading.Qty       := FQtyB;
  xParamTrading.Side      := TTypeBuySell.tsBuy;
  xParamTrading.OnMsgInfo := nil;
  SendTrade(xParamTrading);
end;

procedure TPair.SetOperationBuy;
var
  xParamTrading: TParamTrading;
begin
  // Купить
  xParamTrading.Symbol    := FSecurityA.Symbol;
  xParamTrading.ApiKey    := Global_ApiKey;
  xParamTrading.ApiSecret := Global_ApiSecret;
  xParamTrading.Time      := GetNewDateTime;
  xParamTrading.Price     := FSecurityA.BidPrice;
  xParamTrading.Qty       := FQtyA;
  xParamTrading.Side      := TTypeBuySell.tsBuy;
  xParamTrading.OnMsgInfo := nil;
  SendTrade(xParamTrading);

  // Продать
  xParamTrading.Symbol    := FSecurityB.Symbol;
  xParamTrading.ApiKey    := Global_ApiKey;
  xParamTrading.ApiSecret := Global_ApiSecret;
  xParamTrading.Time      := GetNewDateTime;
  xParamTrading.Price     := FSecurityB.AskPrice;
  xParamTrading.Qty       := FQtyB;
  xParamTrading.Side      := TTypeBuySell.tsSell;
  xParamTrading.OnMsgInfo := nil;
  SendTrade(xParamTrading);
end;


{ TSecurityPairs }

constructor TSecurityPairs.Create;
begin
  FBybitTickers := nil;
  FPairs := TPairList.Create;
end;

destructor TSecurityPairs.Destroy;
begin
  FreeAndNil(FPairs);
  inherited;
end;

procedure TSecurityPairs.Clear;
begin
  FPairs.Clear;
end;

function TSecurityPairs.GetAddPair(const ASymbolA, ASymbolB: String; const AQtyA, AQtyB: Double): TPair;
var
  xPair: TPair;
begin
  if not Assigned(FBybitTickers) then
  begin
    Result := nil;
    Exit;
  end;

  xPair := TPair.Create(ASymbolA, ASymbolB, FBybitTickers);
  xPair.QtyA := AQtyA;
  xPair.QtyB := AQtyB;

  FPairs.Add(xPair);
  Result := xPair;
  xPair.SetUpData;
end;

procedure TSecurityPairs.SetUpData;
begin
  for var xP in FPairs do
    xP.SetUpData;
end;



end.
