unit Lb.VirtualTrade;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline,
  Lb.Bybit.Trade;

type
  ///<summary>����������� ������</summary>
  TVirtualTrade = record
  public
    Symbol: String;      // ������������ �����������
    Side: TTypeSide;     // �����������
    Qty: Double;         // ����������
  public
    LongRSI: Double;     // ������ �������� RSI
    MediumRSI: Double;   // ������� �������� RSI
    ShortSRI: Double;    // �������� �������� RSI
  private
    FPrice: Double;
    FMaxPrice: Double;
    FMinPrice: Double;
    function GetValue: Double;
    procedure SetPrice(const Value: Double);
    function GetProfitMax: Double;
    function GetProfitMin: Double;
  public
    procedure Default;
    property MaxPrice: Double write FMaxPrice;
    property MinPrice: Double write FMinPrice;
    ///<summary>�������� ������</summary>
    property Price: Double read FPrice write SetPrice;
    ///<summary>����� �������� ������</summary>
    property Value: Double read GetValue;
    property ProfitMax: Double read GetProfitMax;
    property ProfitMin: Double read GetProfitMin;
  end;

  ///<summary>������ ����������� ������</summary>
  TVirtualTradeList = TList<TVirtualTrade>;

procedure SetVirtualOrderSelectedOrder(
  const APlaceOrder: TParamOrder;
  const ALongRSI, AMediumRSI, AShortSRI: Double
);

function GetVirtualTrades: TVirtualTradeList;

implementation

uses
  System.DateUtils;

var
  localVirtualTrades: TVirtualTradeList = nil;

function GetVirtualTrades: TVirtualTradeList;
begin
  if not Assigned(localVirtualTrades) then
    localVirtualTrades := TVirtualTradeList.Create;
  Result := localVirtualTrades;
end;

procedure SetVirtualOrderSelectedOrder(
  const APlaceOrder: TParamOrder;
  const ALongRSI, AMediumRSI, AShortSRI: Double
  );
var
  xVirtualTrade: TVirtualTrade;
begin
  with xVirtualTrade do
  begin
    Default;
    Symbol := APlaceOrder.Symbol; // ������������ �����������
    Side := APlaceOrder.Side;     // �����������
    Price := APlaceOrder.Price;   // ����
    Qty := APlaceOrder.Qty;       // ����������
    LongRSI := ALongRSI;          // ������ �������� RSI
    MediumRSI := AMediumRSI;      // ������� �������� RSI
    ShortSRI := AShortSRI;        // �������� �������� RSI
  end;
  GetVirtualTrades.Add(xVirtualTrade);
end;

{ TVirtualTrade }

procedure TVirtualTrade.Default;
begin
  Symbol := '';            // ������������ �����������
  Side := TTypeSide.tsBuy; // �����������
  Qty := 0;                // ����������
  LongRSI := 0;            // ������ �������� RSI
  MediumRSI := 0;          // ������� �������� RSI
  ShortSRI := 0;           // �������� �������� RSI
  FPrice := 0;             // ����
  MaxPrice := 0;
  MinPrice := 0;
end;

function TVirtualTrade.GetValue: Double;
begin
  Result := Qty * Price;
end;

procedure TVirtualTrade.SetPrice(const Value: Double);
begin
  FPrice := Value;

  if FMaxPrice = 0 then
    FMaxPrice := FPrice
  else
    if FMaxPrice < FPrice then
      FMaxPrice := FPrice;

  if FMinPrice = 0 then
    FMinPrice := FPrice
  else
    if FMinPrice > FPrice then
      FMinPrice := FPrice;
end;


function TVirtualTrade.GetProfitMax: Double;
begin
  Result := 0;
  if FMaxPrice > 0 then
  begin
    case Side of
      tsBuy: Result := FMaxPrice - FPrice;
      tsSell: Result := FPrice - FMinPrice;
    end;
  end;
end;

function TVirtualTrade.GetProfitMin: Double;
begin
  Result := 0;
  if FMinPrice > 0 then
  begin
    case Side of
      tsBuy: Result := FPrice - FMinPrice;
      tsSell: Result := FMinPrice - Price;
    end;
  end;
end;


initialization

finalization
  FreeAndNil(localVirtualTrades);

end.
