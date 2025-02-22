unit Lb.Bot;

interface

{$I debug.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils,
  Lb.Platform,
  Lb.Journal.Trading.v2;

type
  /// <summary>
  /// Определяет напровление торговли
  /// </summary>
  TTypeBot = (tbNull, tbStart, tbOpenTrade, tbClose);

  /// <summary>
  /// Событие
  /// </summary>
  TEventOnSendTrade = procedure(ASender: TObject; const ATime: TDateTime;
    const APrice, AQty: Double; ASide: TTypeBuySell) of object;

  TBot = class(TObject)
  private
    FQty: Double;
    FIsActive: Boolean;
    FTriling: Double;
    FManager: TJournalManager;
    FAskPrice, FBidPrice: Double;
  protected
    procedure EventPositionOnClose(ASander: TObject);
    procedure SetOpenTrade(const ASide: TTypeBuySell; APrice: Double);
    procedure SetCloseTrade;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure SetUpData(const AAskPrice, ABidPrice: Double); virtual;
    property Qty: Double read FQty write FQty;
    property Triling: Double read FTriling write FTriling;
    property Manager: TJournalManager read FManager;
    property IsActive: Boolean read FIsActive;
  end;

  TBotList = TObjectList<TBot>;

  ///<summary>
  /// Для тестирование, с фиксирваным размером депозита
  ///</summary>
  TLevelBot  = class(TBot)
  private
    FProfit: Double;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetUpData(const AAskPrice, ABidPrice: Double); override;
    property Profit: Double read FProfit write FProfit;
  end;


  TLevelBotList = TObjectList<TLevelBot>;

implementation

{ TBot }

constructor TBot.Create;
begin
  FManager := TJournalManager.Create;
  FTriling := 15;
  FIsActive := False;
  FQty := 1;
end;

destructor TBot.Destroy;
begin
  FreeAndNil(FManager);
  inherited;
end;

procedure TBot.EventPositionOnClose(ASander: TObject);
var
  xPosition: TJournalPosition;
begin
  if not FIsActive then
    Exit;

  if not(ASander is TJournalPosition) then
    Exit;

  xPosition := TJournalPosition(ASander);
  case xPosition.Side of
    tsBuy:
      SetOpenTrade(TTypeBuySell.tsSell, FAskPrice);
    tsSell:
      SetOpenTrade(TTypeBuySell.tsSell, FBidPrice);
  end;
end;

procedure TBot.SetOpenTrade(const ASide: TTypeBuySell; APrice: Double);

  function _GetQty: Double;
  var
    xCount: Integer;
    xPosition: TJournalPosition;
  begin
    Result := FQty;
    xCount := FManager.Positions.Count;
    if xCount > 0 then
    begin
      xPosition := FManager.Positions[xCount - 1];
      if xPosition.Profit < 0 then
        Result := 2 * xPosition.Qty;
    end;
  end;

var
  xPosition: TJournalPosition;
begin
  xPosition := FManager.GetCreateJournalPosition;
  xPosition.OnClose := EventPositionOnClose;
  with xPosition do
  begin
    OpenTime := GetNewDateTime;
    OpenPrice := APrice;
    Qty := _GetQty;
    Side := ASide;
    IsActive := True;
    TypeTrade := TTypeTrade.ttOpen;
    Triling := FTriling;
    DoOpen;
  end;
end;

procedure TBot.SetCloseTrade;
var
  xPrice: Double;
  i, iCount: Integer;
  xPosition: TJournalPosition;
begin
  iCount := FManager.Positions.Count;
  if iCount > 0 then
    for i := iCount - 1 downto 0 do
    begin
      xPosition := FManager.Positions[i];
      if xPosition.TypeTrade = TTypeTrade.ttOpen then
      begin
        with xPosition do
        begin
          case Side of
            TTypeBuySell.tsBuy:
              xPrice := FBidPrice;
            TTypeBuySell.tsSell:
              xPrice := FAskPrice;
          else
            xPrice := 0;
          end;

          if xPrice <= 0 then
            Exit;

          CloseTime := GetNewDateTime;
          ClosePrice := xPrice;
          IsActive := False;
          TypeTrade := TTypeTrade.ttClose;
          DoClose;
        end;
      end;
    end;
end;

procedure TBot.SetUpData(const AAskPrice, ABidPrice: Double);
var
  xPosition: TJournalPosition;
  i, iCount: Integer;
begin
  FAskPrice := AAskPrice;
  FBidPrice := ABidPrice;
  iCount := FManager.Positions.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xPosition := FManager.Positions[i];
      if xPosition.TypeTrade = TTypeTrade.ttOpen then
      begin
        case xPosition.Side of
          tsBuy:
            xPosition.SetUpData(ABidPrice);
          tsSell:
            xPosition.SetUpData(AAskPrice);
        end;
      end;
    end;
end;

procedure TBot.Start;
begin
  if not FIsActive then
  begin
    FIsActive := True;
    { todo: Случайно выбираем открывать позицию }
    SetOpenTrade(TTypeBuySell.tsBuy, FAskPrice);
  end;
end;

procedure TBot.Stop;
begin
  if FIsActive then
  begin
    FIsActive := False;
    SetCloseTrade;
  end;
end;

{ TLevelBot }

constructor TLevelBot.Create;
begin
  inherited;
  FProfit := 100;
end;

destructor TLevelBot.Destroy;
begin

  inherited;
end;

procedure TLevelBot.SetUpData(const AAskPrice, ABidPrice: Double);
begin
  inherited SetUpData(AAskPrice, ABidPrice);
  if (FProfit - FManager.Profit) < 0 then
    Stop;
end;

end.
