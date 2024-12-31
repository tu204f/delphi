(******************************************************************************)
(* Журнал сделок торговых операций                                            *)
(******************************************************************************)
unit Lb.Journal.Trading.V2;

interface

{$I debug.inc}

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  Lb.SysUtils;

type
  TJournalPosition = class;

  ///<summary>Сделка</summary>
  TJournalTrade = class(TObject)
  private
    FTime: TDateTime;
    FPrice: Double;
    FQty: Double;
    FSide: TTypeBuySell;
    FCandels: TCandelList;
    function GetValue: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Time: TDateTime read FTime write FTime;
    property Price: Double read FPrice write FPrice;
    property Qty: Double read FQty write FQty;
    property Side: TTypeBuySell read FSide write FSide;
    property Value: Double read GetValue;
    property Candels: TCandelList read FCandels;
  end;

  ///<summary>Список сделок</summary>
  TJournalTradeList = TObjectList<TJournalTrade>;

  ///<summary>Позиция которую открыли</summary>
  ///<remarks>
  /// Позиция считается открытой пока есть хоть одна не нулевая позиция
  ///</remarks>
  TJournalPosition = class(TObject)
  private
    FQty: Double;
    FTrades: TJournalTradeList;
    function GetIsActive: Boolean;
  protected
    ///<summary>Обновить информацию по сделке</summary>
    procedure SetUpDate;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>Прошла сделка</summary>
    function AddTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell): TJournalTrade;
    ///<summary>Список сделок</summary>
    property Trades: TJournalTradeList read FTrades;
    property IsActive: Boolean read GetIsActive;
  end;


implementation

{ TJournalTrade }

constructor TJournalTrade.Create;
begin
  FCandels := TCandelList.Create;
end;

destructor TJournalTrade.Destroy;
begin
  FreeAndNil(FCandels);
  inherited;
end;

function TJournalTrade.GetValue: Double;
begin
  Result := FPrice * FQty;
end;

{ TJournalPosition }

constructor TJournalPosition.Create;
begin
  FTrades := TJournalTradeList.Create;
end;

destructor TJournalPosition.Destroy;
begin
  FreeAndNil(FTrades);
  inherited;
end;

function TJournalPosition.AddTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell): TJournalTrade;
var
  xTrade: TJournalTrade;
begin
  xTrade := TJournalTrade.Create;

  xTrade.Time := ATime;
  xTrade.Price := APrice;
  xTrade.Qty := AQty;
  xTrade.Side := ASide;

  FTrades.Add(xTrade);
  Result := xTrade;

  SetUpDate;
end;

procedure TJournalPosition.SetUpDate;
begin
  FQty := 0;
  for var xTrade in FTrades do
  begin
    FQty := FQty + xTrade.Qty;
  end;
  FQty := GetRound(FQty);
end;

function TJournalPosition.GetIsActive: Boolean;
begin
  Result := (FQty > 0);
end;

end.
