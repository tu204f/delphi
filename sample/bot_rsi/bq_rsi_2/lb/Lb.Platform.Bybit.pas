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
    FCountSelected: Integer;
    FBybitKline: TBybitKline;
    FBybitOrderBook: TBybitOrderBook;
    procedure BybitKlineOnEventEndLoading(ASender: TObject);
    procedure BybitOrderBookOnEventEndLoading(ASender: TObject);
    procedure BybitOnEventException(ASender: TObject);
  protected
    procedure DoSelected; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{$IFDEF DEBUG}
uses
  Lb.Logger;
{$ENDIF}

{ TPlatfomBybit }

constructor TPlatfomBybit.Create;
begin
  inherited;
  FCountSelected := 0;

  FBybitKline := TBybitKline.Create;
  FBybitKline.OnEventEndLoading := BybitKlineOnEventEndLoading;
  FBybitKline.OnEventException := BybitOnEventException;

  FBybitOrderBook := TBybitOrderBook.Create;
  FBybitOrderBook.OnEventEndLoading := BybitOrderBookOnEventEndLoading;
  FBybitOrderBook.OnEventException := BybitOnEventException;
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

  {todo: ��������� ��� ��������� � ���������}
  FBybitKline.Category := TTypeCategory.tcLinear;
  FBybitKline.Interval := TTypeInterval.ti_1;
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
  xBid := 0;
  xAsk := 0;

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

procedure TPlatfomBybit.BybitOnEventException(ASender: TObject);
begin
  raise Exception.Create(TBybitHttpClient(ASender).ValueMessage);
end;

end.
