unit Lb.Platfom.Bybit;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  Lb.SysUtils,
  Lb.Platfom,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline,
  Lb.Bybit.OrderBook;

type
  ///<summary>
  /// Соединение сервером Bybit
  ///</summary>
  TPlatfomBybit = class(TTradingPlatform)
  private
    FCountSelected: Integer;
    FBybitKline: TBybitKline;
    FBybitOrderBook: TBybitOrderBook;
    procedure BybitKlineOnEventEndLoading(ASender: TObject);
    procedure BybitOrderBookOnEventEndLoading(ASender: TObject);
  protected
    procedure DoSelected; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TPlatfomBybit }

constructor TPlatfomBybit.Create;
begin
  inherited;
  FCountSelected := 0;

  FBybitKline := TBybitKline.Create;
  FBybitKline.OnEventEndLoading := BybitKlineOnEventEndLoading;

  FBybitOrderBook := TBybitOrderBook.Create;
  FBybitOrderBook.OnEventEndLoading := BybitOrderBookOnEventEndLoading;
end;

destructor TPlatfomBybit.Destroy;
begin
  FreeAndNil(FBybitKline);
  inherited;
end;

procedure TPlatfomBybit.DoSelected;
begin
  if FCountSelected > 0 then
    Exit;

  FBybitKline.Symbol := Symbel;

  {todo: Перевести эти параметры в найстроки}
  FBybitKline.Category := TTypeCategory.tcLinear;
  FBybitKline.Interval := TTypeInterval.ti_5;
  FBybitKline.Limit    := 100;
  FBybitKline.Selected;

  FBybitOrderBook.Category := TTypeCategory.tcLinear;
  FBybitOrderBook.Symbol := Symbel;
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
  FStateMarket.Candels.Clear;
  for var xCandelObject in FBybitKline.CandelObjects do
  begin
    FStateMarket.Candels.Add(
      _ToCandel(xCandelObject)
    );
  end;

  Dec(FCountSelected);
  if FCountSelected = 0 then
    DoStateMarke;
end;

procedure TPlatfomBybit.BybitOrderBookOnEventEndLoading(ASender: TObject);
var
  xBid, xAsk: Double;
begin
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
  FStateMarket.SetPrice(xAsk,xBid);

  Dec(FCountSelected);
  if FCountSelected = 0 then
    DoStateMarke;
end;


end.
