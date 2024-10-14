unit Lb.Bot.Position;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils;

type
  ///<summary>
  /// Заявку
  ///</summary>
  TBOrder = class(TObject)
    OrderNo: UInt64;
    Price: Double;
    Qty: Double;
    Side: TQBTypeSide;
  public
    constructor Create;
  end;

  ///<summary>
  /// Список заявок
  ///</summary>
  TBOrderList = TObjectList<TBOrder>;

type
  ///<summary>
  /// Сделка
  ///</summary>
  TBTrade = class(TObject)
    OrderNo: UInt64;
    TradeNo: UInt64;
    Price: Double;
    Qty: Double;
    Side: TQBTypeSide;
  public
    constructor Create;
  end;

  ///<summary>
  /// Список сделок
  ///</summary>
  TBTradeList = TObjectList<TBTrade>;

type
  ///<summary>
  /// Позиция - по оперциям
  ///</summary>
  TPosition = class(TObject)
  private
    FHistoryOrders: TBOrderList;
    FHistotyTrades: TBTradeList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TBOrder }

constructor TBOrder.Create;
begin
  OrderNo := 0;
  Price := 0;
  Qty := 0;
  Side := TQBTypeSide.tsNull;
end;

{ TBTrade }

constructor TBTrade.Create;
begin
  OrderNo := 0;
  TradeNo := 0;
  Price := 0;
  Qty := 0;
  Side := TQBTypeSide.tsNull;
end;

{ TPosition }

constructor TPosition.Create;
begin
  FHistoryOrders := TBOrderList.Create;
  FHistotyTrades := TBTradeList.Create;
end;

destructor TPosition.Destroy;
begin
  FreeAndNil(FHistotyTrades);
  FreeAndNil(FHistoryOrders);
  inherited;
end;

end.
