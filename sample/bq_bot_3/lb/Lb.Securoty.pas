unit Lb.Securoty;

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
    procedure BybitTickersOnEventEndLoading(Sender: TObject);
  private
    FOnChangeTicker: TNotifyEvent;
    FTickerValue: TTickerValue;
    function GetAskPrice: Double;
    function GetBidPrice: Double;
    function GetLast: Double;
  protected
    procedure DoChangeTicker;
  public
    constructor Create(ASymbol: String; ABybitTickers: TBybitTickers); virtual;
    destructor Destroy; override;
    procedure SetUpData;
    ///<summary>Символ инструмента</summary>
    property Symbol: String read FSymbol write FSymbol;
    ///<summary>Последние цены</summary>
    property Last: Double read GetLast;
    ///<summary>Цена продавца</summary>
    property AskPrice: Double read GetAskPrice;
    ///<summary>Цена покупателя</summary>
    property BidPrice: Double read GetBidPrice;
  public
    property OnChangeTicker: TNotifyEvent write FOnChangeTicker;
  end;

implementation

{ TSecurity }

constructor TSecurity.Create(ASymbol: String; ABybitTickers: TBybitTickers);
begin
  FSymbol := ASymbol;
  FBybitTickers := ABybitTickers;
  FBybitTickers.OnEventEndLoading := BybitTickersOnEventEndLoading;
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

procedure TSecurity.BybitTickersOnEventEndLoading(Sender: TObject);
begin
  Self.DoChangeTicker;
end;

procedure TSecurity.DoChangeTicker;
begin

end;

end.
