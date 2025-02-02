(******************************************************************************)
(* Журнал сделок торговых операций                                            *)
(******************************************************************************)
unit Lb.Journal.Trading.v2;

interface

{$I debug.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  Lb.SysUtils;

type
  TJournalTrade = class;
  TJournalPosition = class;
  TJournalManager = class;

  ///<summary>Когда позиция открывает новую сделку</summary>
  TEventOnNewTrade = procedure(ASander: TObject; ATrade: TJournalTrade) of object;
  TEventOnOpen = procedure(ASander: TObject) of object;
  TEventOnClose = procedure(ASander: TObject) of object;

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
    property Time: TDateTime      read FTime  write FTime;
    property Price: Double        read FPrice write FPrice;
    property Qty: Double          read FQty   write FQty;
    property Side: TTypeBuySell   read FSide  write FSide;
    property Value: Double        read GetValue;
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
    FID: Integer;
    FIsActive: Boolean;
    FManager: TJournalManager;
    FOnOpen: TEventOnOpen;
    FOnClose: TEventOnClose;
  private
    FOpenTime: TDateTime;
    FOpenPrice: Double;
    FCloseTime:  TDateTime;
    FClosePrice: Double;
    FSide: TTypeBuySell;
    FQty: Double;
    FProfit: Double;
    FTypeTrade: TTypeTrade;
  private
    FTriling: Double;
    FStopLoss: Double;
    FTakeProfit: Double;
  protected
    procedure DoOpen;
    procedure DoClose;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetUpData(const APrice: Double = 0);
  public
    property TypeTrade: TTypeTrade read FTypeTrade write FTypeTrade;
    property IsActive: Boolean read FIsActive write FIsActive;
    property OpenTime: TDateTime read FOpenTime write FOpenTime;
    property OpenPrice: Double read FOpenPrice write FOpenPrice;
    property CloseTime:  TDateTime read FCloseTime write FCloseTime;
    property ClosePrice: Double read FClosePrice write FClosePrice;
    property Side: TTypeBuySell read FSide write FSide;
    property Qty: Double read FQty write FQty;
    property Profit: Double read FProfit write FProfit;
    property Triling: Double read FTriling write FTriling;
    property StopLoss: Double read FStopLoss write FStopLoss;
    property TakeProfit: Double read FTakeProfit write FTakeProfit;
  public
    property OnOpen: TEventOnOpen write FOnOpen;
    property OnClose: TEventOnClose write FOnClose;
    property Manager: TJournalManager read FManager write FManager;
    property ID: Integer read FID write FID;
  end;

  ///<summary>Список позиций</summary>
  TJournalPositionList = TObjectList<TJournalPosition>;

  ///<summary>Менеджер позиций</summary>
  TJournalManager = class(TObject)
  private
    FPositions: TJournalPositionList;
    function GetProfit: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    ///<summary>Создание журнала позиции</summary>
    function GetCreateJournalPosition: TJournalPosition;

    ///<summary>Список позиций</summary>
    property Positions: TJournalPositionList read FPositions;
    ///<summary>Сумарный профит повсем позициям</summary>
    property Profit: Double read GetProfit;
  end;

implementation

uses
{$IFDEF DEBUG}
  Lb.Logger,
{$ENDIF}
  System.Math;

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
  FManager := nil;
  FOpenTime := 0;
  FOpenPrice := 0;
  FCloseTime := 0;
  FClosePrice := 0;
  FSide := TTypeBuySell.tsNull;
  FQty := 0;
  FProfit := 0;
  FTypeTrade := TTypeTrade.ttNull;
  FStopLoss := 0;
  FTakeProfit := 0;
end;

destructor TJournalPosition.Destroy;
begin

  inherited;
end;

procedure TJournalPosition.DoOpen;
begin
  if Assigned(FOnOpen) then
    FOnOpen(Self);
end;


procedure TJournalPosition.DoClose;
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TJournalPosition.SetUpData(const APrice: Double);

  procedure _CalcProfit(const APrice: Double);
  begin
    case FTypeTrade of
      ttOpen: begin
        case FSide of
          tsBuy: FProfit := (APrice - FOpenPrice) * FQty;
          tsSell: FProfit := (FOpenPrice - APrice) * FQty;
        end;
      end;
      ttClose: begin
        case FSide of
          tsBuy: FProfit := (FClosePrice - FOpenPrice) * FQty;
          tsSell: FProfit := (FOpenPrice - FClosePrice) * FQty;
        end;
      end;
    end;
    FProfit := GetRound(FProfit);
  end;

  procedure _CalcTrelingStopLoss(const APrice: Double);
  var
    xStopLoss: Double;
  begin
    if FTypeTrade = TTypeTrade.ttOpen then
    begin
      case FSide of
        tsBuy: begin
          xStopLoss := APrice - FTriling;
          if xStopLoss > FStopLoss then
            FStopLoss := xStopLoss;
        end;
        tsSell: begin
          xStopLoss := APrice - FTriling;
          if FStopLoss = 0  then
            FStopLoss := xStopLoss;
          if xStopLoss < FStopLoss then
            FStopLoss := xStopLoss;
        end;
      end;
    end;
  end;

  procedure _ActiveStopLoss(APrice: Double);
  begin
    if FTypeTrade = TTypeTrade.ttOpen then
    begin
      case FSide of
        tsBuy: begin
          if APrice < FStopLoss then
          begin
            CloseTime := GetNewDateTime;
            ClosePrice := APrice;
            IsActive := False;
            TypeTrade := TTypeTrade.ttClose;
          end;
        end;
        tsSell: begin
          if APrice > FStopLoss then
          begin
            CloseTime := GetNewDateTime;
            ClosePrice := APrice;
            IsActive := False;
            TypeTrade := TTypeTrade.ttClose;
          end;
        end;
      end;
    end;
  end;

begin
  _CalcProfit(APrice);
  _CalcTrelingStopLoss(APrice);
  _ActiveStopLoss(APrice);
end;

{ TJournalManager }

constructor TJournalManager.Create;
begin
  FPositions := TJournalPositionList.Create;
end;

destructor TJournalManager.Destroy;
begin
  FPositions.Clear;
  FreeAndNil(FPositions);
  inherited;
end;

function TJournalManager.GetCreateJournalPosition: TJournalPosition;
var
  xJournalPosition: TJournalPosition;
begin
  xJournalPosition := TJournalPosition.Create;
  xJournalPosition.Manager := Self;
  xJournalPosition.ID := FPositions.Count;
  FPositions.Add(xJournalPosition);
  Result := xJournalPosition;
end;


function TJournalManager.GetProfit: Double;
var
  xSum: Double;
begin
  xSum := 0;
//  for var xP in FPositions do
//    xSum := xSum + xP.Profit;
  Result := xSum;
end;

end.
