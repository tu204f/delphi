unit Quik.Operation;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  ///<summary>Торговая операция</summary>
  TOperation = class(TObject)
  public type

    ///<summary>Заявка</summary>
    TOrder = class(TObject)
      OrderID: Int64;
      Price: Double;
      Quantity: Integer;
      Balance: Integer;
    end;

    ///<summary>Сделка</summary>
    TTrade = class(TObject)
      OrderID: Int64;
      TradeID: Int64;
      Price: Double;
      Quantity: Integer;
    end;
    TTradeList = TList<TTrade>;

  private
    FOrder: TOrder;
    FTrades: TTradeList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Order: TOrder read FOrder;
    property Trades: TTradeList read FTrades;
  end;

  TTradePosition = class(TObject)
  private
    FOpenOperation: TOperation;
    FCloseOperation: TOperation;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>Открытие позиции</summary>
    procedure OpenPosition(APrice: Double; AQuantity: Integer);
    ///<summary>Закрытие позиции</summary>
    procedure ClosePosition(APrice: Double);
    property OpenOperation: TOperation read FOpenOperation;
    property CloseOperation: TOperation read FCloseOperation;
  end;

implementation

{ TOperation }

constructor TOperation.Create;
begin
  FOrder := TOrder.Create;
  FTrades := TTradeList.Create;
end;

destructor TOperation.Destroy;
begin
  FreeAndNil(FTrades);
  FreeAndNil(FOrder);
  inherited;
end;


{ TTradePosition }

constructor TTradePosition.Create;
begin
  FOpenOperation := TOperation.Create;
  FCloseOperation := TOperation.Create;
end;

destructor TTradePosition.Destroy;
begin
  FreeAndNil(FCloseOperation);
  FreeAndNil(FOpenOperation);
  inherited;
end;

procedure TTradePosition.OpenPosition(APrice: Double; AQuantity: Integer);
begin

end;

procedure TTradePosition.ClosePosition(APrice: Double);
begin

end;

end.
