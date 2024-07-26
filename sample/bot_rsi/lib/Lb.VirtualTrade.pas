unit Lb.VirtualTrade;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.IniFiles,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Trade;

type
  TVirtualTrades = class(TObject)
  public type
    TParamTrade = record
      Time: TDateTime;
      Symbol: String;
      Side: TTypeSide;
      Qty: Double;
      Price: Double;
      OrderLinkId: String;
    private
      function GetToStr: String;
    public
      property ToStr: String read GetToStr;
    end;
    TParamTradeList = TList<TParamTrade>;
  private
    FSide: TTypeSide;
    FQty: Double;
    FParamTrades: TParamTradeList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TParamTrade;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure AddOrder(APlaceOrder: TParamOrder);
    property Items[Index: Integer]: TParamTrade read GetItems;
    property Count: Integer read GetCount;
    property Qty: Double read FQty;
    property Side: TTypeSide read FSide;
    procedure Save(const AFileName: String);
  end;

///<summary>
/// Для вертуальных операция, повторяем структура основной стратегии
///</summary>
function Virtual_SelectedOrder(const ApiKey, ApiSecret: String; APlaceOrder: TParamOrder; AOrderResponse: TOrderResponse): String;

///<summary>
/// Списко вертуальный сделок
///</summary>
function GetVirtualTrades: TVirtualTrades;

implementation

var
  localVirtualTrades: TVirtualTrades = nil;

function GetVirtualTrades: TVirtualTrades;
begin
  if not Assigned(localVirtualTrades) then
    localVirtualTrades := TVirtualTrades.Create;
  Result := localVirtualTrades;
end;

function Virtual_SelectedOrder(const ApiKey, ApiSecret: String; APlaceOrder: TParamOrder; AOrderResponse: TOrderResponse): String;
begin
  Result := 'OK';
  GetVirtualTrades.AddOrder(APlaceOrder);
end;

{ TVirtualTrades.TParamTrade }

function TVirtualTrades.TParamTrade.GetToStr: String;
var
  xS: String;
  xF: TFormatSettings;
begin
  xF := FormatSettings;
  xF.DecimalSeparator := '.';
  xS := '';
  xS := xS + DateTimeToStr(Time,xF) + ';';
  xS := xS + Symbol + ';';
  xS := xS + GetStrToTypeSide(Side) + ';';
  xS := xS + FloatToStr(Qty,xF) + ';';
  xS := xS + FloatToStr(Price,xF) + ';';
  xS := xS + OrderLinkId + ';';
  Result := xS;
end;

{ TVirtualTrades }

constructor TVirtualTrades.Create;
begin
  FQty := 0;
  FParamTrades := TParamTradeList.Create;
end;

destructor TVirtualTrades.Destroy;
begin
  FreeAndNil(FParamTrades);
  inherited;
end;

procedure TVirtualTrades.Clear;
begin
  FParamTrades.Clear;
end;

procedure TVirtualTrades.AddOrder(APlaceOrder: TParamOrder);
var
  xParamTrade: TParamTrade;
begin
  if FQty = 0 then
  begin
    FQty  := APlaceOrder.Qty;
    FSide := APlaceOrder.Side;
  end
  else
  begin
    case APlaceOrder.Side of
      tsBuy : begin
        case FSide of
          tsBuy: FQty := FQty + APlaceOrder.Qty;
          tsSell: FQty := FQty - APlaceOrder.Qty;
        end;
      end;
      tsSell: begin
        case FSide of
          tsBuy: FQty := FQty - APlaceOrder.Qty;
          tsSell: FQty := FQty + APlaceOrder.Qty;
        end;
      end;
    end;
  end;

  if FQty < 0 then
  begin
    FQty  := Abs(FQty);
    FSide := APlaceOrder.Side;
  end;

  xParamTrade.Time        := Date + Time;
  xParamTrade.Symbol      := APlaceOrder.Symbol;
  xParamTrade.Side        := APlaceOrder.Side;
  xParamTrade.Qty         := APlaceOrder.Qty;
  xParamTrade.Price       := APlaceOrder.Price;
  xParamTrade.OrderLinkId := APlaceOrder.OrderLinkID;
  FParamTrades.Add(xParamTrade);

  Save('history_trade.csv');
end;

function TVirtualTrades.GetCount: Integer;
begin
  Result := FParamTrades.Count;
end;

function TVirtualTrades.GetItems(Index: Integer): TParamTrade;
begin
  Result := FParamTrades.Items[Index];
end;

procedure TVirtualTrades.Save(const AFileName: String);
var
  xStr: TStringList;
begin
  xStr := TStringList.Create;
  try
    for var xParamTrade in FParamTrades do
      xStr.Add(xParamTrade.ToStr);
    xStr.SaveToFile(AFileName);
  finally
    FreeAndNil(xStr);
  end;
end;



initialization


finalization
  FreeAndNil(localVirtualTrades);

end.
