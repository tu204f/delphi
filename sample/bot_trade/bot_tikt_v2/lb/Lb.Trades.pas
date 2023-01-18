unit Lb.Trades;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;


type
(******************************************************************************)
(*  аждое начало блока расматривать как отрытие позиции                       *)
(******************************************************************************)
  TStatusTrade = (stOpen, stClose);

  TTrade = class(TObject)
  private
    FOpenPrice: Double;
    FClosePrice: Double;
    FQuantity: Integer;
    FBuySell: Char;
    FProfit: Double;
    FMinProfit: Double;
    FMaxProfit: Double;
    FStatus: TStatusTrade;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetUpData(const APrice: Double);
    property OpenPrice: Double read FOpenPrice write FOpenPrice;
    property Quantity: Integer read FQuantity write FQuantity;
    property BuySell: Char read FBuySell write FBuySell;
    property ClosePrice: Double read FClosePrice;
    property Profit: Double read FProfit;
    property Status: TStatusTrade read FStatus write FStatus;
    property MinProfit: Double read FMinProfit;
    property MaxProfit: Double read FMaxProfit;
  end;
  TTradeList = TObjectList<TTrade>;

  TTrades = class(TObject)
  private
    FTrades: TTradeList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure OpenTrade(APrice: Double; AQuantity: Integer; ABuySell: Char);
    procedure SetUpData(const APrice: Double);
    property Items: TTradeList read FTrades;
  end;

implementation

{ TTrade }

constructor TTrade.Create;
begin
  FOpenPrice := 0;
  FClosePrice := 0;
  FQuantity := 0;
  FBuySell := #0;
  FProfit := 0;
  FMinProfit := 0;
  FMaxProfit := 0;
end;

destructor TTrade.Destroy;
begin

  inherited;
end;

procedure TTrade.SetUpData(const APrice: Double);
begin
  FClosePrice := APrice;
  case FBuySell of
    'B': FProfit := APrice - FOpenPrice;
    'S': FProfit := FOpenPrice - APrice;
  else
    raise Exception.Create('Error Message: Ќаправление сделки не определена');
  end;

  if (FMinProfit = 0) or (FMaxProfit = 0) then
  begin
    FMinProfit := FProfit;
    FMaxProfit := FProfit;
  end
  else
  begin
    if FMinProfit > FProfit then
      FMinProfit := FProfit;
    if FMaxProfit < FProfit then
      FMaxProfit := FProfit;
  end;

end;

{ TTrades }

constructor TTrades.Create;
begin
  FTrades := TTradeList.Create;
end;

destructor TTrades.Destroy;
begin
  FreeAndNil(FTrades);
  inherited;
end;

procedure TTrades.OpenTrade(APrice: Double; AQuantity: Integer; ABuySell: Char);

  procedure _SetCloseTrade;
  begin
    for var xTrade in FTrades do
      xTrade.Status := TStatusTrade.stClose;
  end;

var
//  xCount: Integer;
  xTrade: TTrade;
begin
  // ѕроверить условие назакрытие позиции
//  xCount := FTrades.Count;
//  if xCount > 0 then
//  begin
//    xTrade := FTrades[xCount - 1];
//    if xTrade.Profit < 0 then
//      _SetCloseTrade;
//  end;

  xTrade := TTrade.Create;
  xTrade.OpenPrice := APrice;
  xTrade.Quantity := AQuantity;
  xTrade.BuySell := ABuySell;
  FTrades.Add(xTrade);
end;

procedure TTrades.SetUpData(const APrice: Double);
begin
  for var xTrade in FTrades do
    if xTrade.Status = TStatusTrade.stOpen then
      xTrade.SetUpData(APrice);
end;

end.
