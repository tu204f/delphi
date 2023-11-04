unit Lb.OpTrade;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  ///<summary>Напровление сделки</summary>
  TBuySell = (
    bsNull,  // Не определено управление
    bsBuy,   // Покупка
    bsSell   // Продажа
  );

  ///<summary>Сделка</summary>
  TTrade = class(TObject)
  private
    FTime: TDateTime;
    FPrice: Double;
    FBuySell: TBuySell;
    FQuantity: Integer;
    function GetValue: Double;
  public
    constructor Create; overload;
    constructor Create(ATime: TDateTime; APrice: Double; AQuantity: Integer; ABuySell: TBuySell); overload;
    destructor Destroy; override;
    property Time: TDateTime read FTime write FTime;
    property Price: Double read FPrice write FPrice;
    property BuySell: TBuySell read FBuySell write FBuySell;
    property Quantity: Integer read FQuantity write FQuantity;
    property Value: Double read GetValue;
  end;

  ///<summary>Список сделок</summary>
  TTradeList = TObjectList<TTrade>;

  ///<summary>Список сделок</summary>
  TTrades = class(TObject)
  private
    FItems: TTradeList;
    function GetCount: Integer;
  protected
    FPosition: Integer;
    FProfit: Double;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure OpenTrade(ATime: TDateTime; APrice: Double; AQuantity: Integer; ABuySell: TBuySell); overload;
    procedure OpenTrade(ATrade: TTrade); overload;
    procedure SetResultProfit(const APrice: Double = 0);

    property Position: Integer read FPosition;
    property Profit: Double read FProfit;

    property Items: TTradeList read FItems;
    property Count: Integer read GetCount;
  end;

implementation

{ TTrade }

constructor TTrade.Create;
begin
  FTime := 0;
  FPrice := 0;
  FBuySell := TBuySell.bsNull;
  FQuantity := 0;
end;

constructor TTrade.Create(ATime: TDateTime; APrice: Double; AQuantity: Integer; ABuySell: TBuySell);
begin
  FTime := ATime;
  FPrice := APrice;
  FBuySell := ABuySell;
  FQuantity := AQuantity;
end;

destructor TTrade.Destroy;
begin

  inherited;
end;

function TTrade.GetValue: Double;
begin
  Result := FPrice * FQuantity;
end;

{ TTrades }

constructor TTrades.Create;
begin
  FItems := TTradeList.Create;
end;

destructor TTrades.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TTrades.Clear;
begin
  FItems.Clear;
end;

function TTrades.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TTrades.SetResultProfit(const APrice: Double);
var
  xTrade: TTrade;
begin
  FProfit := 0;
  FPosition := 0;

  if FItems.Count > 0 then
    for xTrade in FItems do
    begin
      case xTrade.BuySell of
        bsBuy: begin
          FPosition := FPosition + xTrade.Quantity;
          FProfit   := FProfit - xTrade.Value;
        end;
        bsSell: begin
          FPosition := FPosition - xTrade.Quantity;
          FProfit   := FProfit + xTrade.Value;
        end
      else
        raise Exception.Create('Error Message: Напровление сделки должно быть определ');
      end;
    end;
  if APrice > 0 then
    FProfit := FProfit - FPosition * APrice;
end;

procedure TTrades.OpenTrade(ATrade: TTrade);
begin
  FItems.Add(ATrade);
  SetResultProfit(ATrade.Price);
end;

procedure TTrades.OpenTrade(ATime: TDateTime; APrice: Double; AQuantity: Integer; ABuySell: TBuySell);
begin
  OpenTrade(TTrade.Create(ATime,APrice,AQuantity,ABuySell));
end;

end.
