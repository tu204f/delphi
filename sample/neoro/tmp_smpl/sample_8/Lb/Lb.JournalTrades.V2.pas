unit Lb.JournalTrades.V2;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils.Candel,
  Lb.Block;

type
  ///<summary>Управление цены</summary>
  TControlPrice = class(TObject)
  private
    FBuySell: Char;
    FPriceOpen: Double;
    FPriceStep: Double;
    FPriceTop: Double;
    FPriceBottom: Double;
    procedure SetPriceStep(const Value: Double);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>Направление сделки</summary>
    property BuySell: Char read FBuySell write FBuySell;
    ///<summary>Цена открытие сделки</summary>
    property PriceOpen: Double read FPriceOpen write SetPriceStep;
    ///<summary>Верхняя цена</summary>
    property PriceTop: Double read FPriceTop;
    ///<summary>Нижняя граница</summary>
    property PriceBottom: Double read FPriceBottom;
  end;

  ///<summary>Упрвление сделками</summary>
  TControlTrade = class(TObject)
  public type
    ///<summary>Сделка</summary>
    TTrade = class(TObject)
    private
      FPrice: Double;
      FQuantity: Integer;
      FBuySell: Char;
    public
      constructor Create(APrice: Double; AQuantity: Integer; ABuySell: Char); virtual;
      property Price: Double read FPrice write FPrice;
      property Quantity: Integer read FQuantity write FQuantity;
      property BuySell: Char read FBuySell write FBuySell;
    end;
    ///<summary>Список сделок</summary>
    TTradeList = TObjectList<TTrade>;
  private
    FTrades: TTradeList;
    FPrice: Double;
    FQuantity: Integer;
    FBuySell: Char;
    function GetIsActive: Boolean;
  protected
    procedure SetControlTrade;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure OpenTrade(APrice: Double; AQuantity: Integer; ABuySell: Char);
    property Trades: TTradeList read FTrades;
    property IsActive: Boolean read GetIsActive;
  public
    ///<summary>Цена открытие позиции</summary>
    property Price: Double read FPrice;
    ///<summary>Клочество открытой позции</summary>
    property Quantity: Integer read FQuantity;
    ///<summary>Напровлнеие позиции</summary>
    property BuySell: Char read FBuySell;
  end;

  ///<summary>Список сделка</summary>
  TControlTradeList = TObjectList<TControlTrade>;

implementation

{ TControlPrice }

constructor TControlPrice.Create;
begin
  FPriceStep := 0;
  FPriceOpen := 0;
  FBuySell := #0;
end;

destructor TControlPrice.Destroy;
begin

  inherited;
end;

procedure TControlPrice.SetPriceStep(const Value: Double);
begin
  FPriceOpen := Value;
  FPriceTop := FPriceOpen + FPriceStep;
  FPriceBottom := FPriceOpen - FPriceStep;
end;

{ TControlTrade.TTrade }

constructor TControlTrade.TTrade.Create(APrice: Double; AQuantity: Integer;
  ABuySell: Char);
begin
  FPrice := APrice;
  FQuantity := AQuantity;
  FBuySell := ABuySell;
end;

{ TControlTrade }

constructor TControlTrade.Create;
begin
  FTrades := TTradeList.Create;
  FPrice := 0;
  FQuantity := 0;
  FBuySell := #0;
end;

destructor TControlTrade.Destroy;
begin
  FreeAndNil(FTrades);
  inherited;
end;

procedure TControlTrade.Clear;
begin
  FTrades.Clear;
end;

function TControlTrade.GetIsActive: Boolean;
begin
  Result := FTrades.Count > 0;
end;

procedure TControlTrade.OpenTrade(APrice: Double; AQuantity: Integer; ABuySell: Char);
var
  xTrade: TTrade;
begin
  xTrade := TTrade.Create(APrice,AQuantity,ABuySell);
  FTrades.Add(xTrade);
  SetControlTrade;
end;

procedure TControlTrade.SetControlTrade;
var
  xValue: Double;
  xTrade: TTrade;
  i, iCount: Integer;
begin
  xValue := 0;

  FPrice := 0;
  FQuantity := 0;
  FBuySell := #0;

  iCount := FTrades.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xTrade := FTrades[i];

      if FBuySell = #0 then
        FBuySell := xTrade.BuySell;

      if FBuySell <> xTrade.BuySell then
        raise Exception.Create('Error Message: Направление открытие сделки не совпадает');

      xValue := xValue + xTrade.Price * xTrade.Quantity;
      FQuantity := FQuantity + xTrade.Quantity;
      FPrice := xValue/FQuantity;
    end;
end;



end.
