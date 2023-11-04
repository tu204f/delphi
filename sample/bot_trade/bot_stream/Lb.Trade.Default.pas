unit Lb.Trade.Default;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  TTypeBuySell = (tNull, tBuy, tSell);

  TTrade = record
    Price: Double;          // Цена
    Quantity: Integer;      // Количество
    BuySell: TTypeBuySell;  // Напровление операции
  private
    function GetValue: Double;  
  public
    constructor Create(ATrade: TTrade); overload;
    constructor Create(APrice: Double; AQuantity: Integer; ABuySell: TTypeBuySell); overload;
    property Value: Double read GetValue;
  end;
  TTradeList = TList<TTrade>;

  TTrades = class(TObject)
  private
    FTrade: TTrade;
    FTrades: TTradeList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetOperationTrade(const ATrade: TTrade); overload;
    procedure SetOperationTrade(APrice: Double; AQuantity: Integer; ABuySell: TTypeBuySell); overload;
    procedure SetUpData(const APrice: Double);
  end;

implementation

{ TTrade }

constructor TTrade.Create(ATrade: TTrade);
begin
  Price := ATrade.Price;
  Quantity := ATrade.Quantity;
  BuySell := ATrade.BuySell;
end;

constructor TTrade.Create(APrice: Double; AQuantity: Integer; ABuySell: TTypeBuySell);
begin
  Self.Price := APrice;
  Self.Quantity := AQuantity;
  Self.BuySell := ABuySell;
end;

function TTrade.GetValue: Double;
begin
  Result := Price * Abs(Quantity);
end;

{ TTrades }

constructor TTrades.Create;
begin
  FTrade.BuySell := TTypeBuySell.tNull;
  FTrades := TTradeList.Create;
end;

destructor TTrades.Destroy;
begin
  FreeAndNil(FTrades);
  inherited;
end;

procedure TTrades.SetOperationTrade(const ATrade: TTrade);
begin
  case FTrade.BuySell of
    tNull: begin
      FTrade := TTrade.Create(ATrade);
    end;
    tBuy: begin
      var xQ := FTrade.Quantity + ATrade.Quantity;
      if xQ <> 0 then
      begin
        FTrade.Price := (FTrade.Value + ATrade.Value) / Abs(xQ);
        FTrade.Quantity := xQ;
      end;
    end;
    tSell: begin
      var xQ := FTrade.Quantity - ATrade.Quantity;
      if xQ <> 0 then
      begin
        FTrade.Price := (FTrade.Value - ATrade.Value) / Abs(xQ);
        FTrade.Quantity := xQ;
      end;
    end;
  end;
  FTrades.Add(ATrade);
end;

procedure TTrades.SetOperationTrade(APrice: Double; AQuantity: Integer; ABuySell: TTypeBuySell);
begin
  SetOperationTrade(TTrade.Create(APrice, AQuantity, ABuySell));
end;

procedure TTrades.SetUpData(const APrice: Double);
begin
  // Объект 
end;

end.
