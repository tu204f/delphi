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
  ///<summary>Сделка</summary>
  TTrade = class(TObject)
  private
    FPrice: Double;
    FQuantity: Integer;
    FBuySell: Char;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Price: Double read FPrice write FPrice;
    property Quantity: Integer read FQuantity write FQuantity;
    property BuySell: Char read FBuySell write FBuySell;
  end;

  ///<summary>Список сделок</summary>
  TTradeList = TObjectList<TTrade>;

  TControlTrade = class(TObject)
  private
    FTrades: TTradeList;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Open(const APrice: Double; const AQuantity: Integer; const ABuySell: Char);
    procedure UpData(const APrice: Double);

    property Trades: TTradeList read FTrades;
  end;

implementation

{ TTrade }

constructor TTrade.Create;
begin

end;

destructor TTrade.Destroy;
begin

  inherited;
end;

{ TControlTrade }

constructor TControlTrade.Create;
begin
  FTrades := TTradeList.Create;
end;

destructor TControlTrade.Destroy;
begin
  FreeAndNil(FTrades);
  inherited;
end;

end.
