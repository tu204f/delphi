unit Lb.SysUtils;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections;

type
  TTypeSide = (
    tsNull, {добавлено новое значение}
    tsBuy,
    tsSell
  );

  TDoubleList = TList<Double>;

type
  ///<summary>
  /// Свеча
  ///</summary>
  TCandel = record
    Time: Integer;  // Дата и время
    Open: Double;   // Цена открытие
    High: Double;   // Максимальная цена
    Low: Double;    // Минимальная цена
    Close: Double;  // Закрытие цены
    Vol: Double;    // Объем который прошол
  end;

  ///<summary>
  /// Массив свячей
  ///</summary>
  TCandelList = TList<TCandel>;

  ///<summary>
  /// Состояние рынка
  ///</summary>
  TStateMarket = class(TObject)
  private
    FAsk: Double;
    FBid: Double;
    FQty: Double;
    FCandels: TCandelList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetPrice(const AAsk, ABid: Double);
    ///<summary>
    /// Объем для определениые цена
    ///</summary>
    property Qty: Double read FQty write FQty;
    ///<summary>
    /// Цена продавца - который может взять Qty
    ///</summary>
    property Ask: Double read FAsk;
    ///<summary>
    /// Цена покупателя - который может взять Qty
    ///</summary>
    property Bid: Double read FBid;
    property Candels: TCandelList read FCandels;
  end;

implementation

{ TStateMarket }

constructor TStateMarket.Create;
begin
  FAsk := 0;
  FBid := 0;
  FCandels := TCandelList.Create;
end;

destructor TStateMarket.Destroy;
begin
  FreeAndNil(FCandels);
  inherited;
end;

procedure TStateMarket.SetPrice(const AAsk, ABid: Double);
begin
  FAsk := AAsk;
  FBid := ABid;
end;

end.
