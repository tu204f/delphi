unit Lb.SysUtils;

interface

{$IFDEF DEBUG}
//  {$DEFINE DB_LOG}
{$ENDIF}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.ReadPrice;

type
  TTypeСriterion = (tcOpen, tcClose);
  TTypeSide = (tsBuy,tsSell);
  TTypePosition = (tpNull, tpOpen, tpClose);

  TTrader = class;
  TPositionTrade = class;

  ///<summary>
  /// Событие трейдера на открытие позиции
  ///</summary>
  TEventOperationTrade = procedure(
    ATrader: TTrader;               // Указатель на трейдора
    ATypeСriterion: TTypeСriterion; // Напровление позиции
    ATypeSide: TTypeSide;           // Напровление следки
    AQty: Double                    // Количество
  ) of object;

  ///<summary>
  /// Критерий отрытие позиции
  ///</summary>
  TСriterion = class(TObject)
  public
    RSI: Double;                   // Значение индикатора
    ReActiveRSI: Double;           // Значение индикатора, для активации кретерия
    Qty: Double;                   // Количество
    IsActive: Boolean;             // Состояние критерия
    TypeСriterion: TTypeСriterion; // Типа критерия
  public
    constructor Create; virtual;
    procedure Default;
  end;
  TСriterionList = TObjectList<TСriterion>;

  ///<summary>Трейдер</summary>
  TTrader = class(TObject)
  private
    FID: Integer;
    FSide: TTypeSide;
    FСriterions: TСriterionList;
  private
    FOnOperationTrade: TEventOperationTrade;
  protected
    procedure DoEventOperationTrade(ATypeСriterion: TTypeСriterion; ATypeSide: TTypeSide; AQty: Double);  virtual;
    procedure DoOperationTrade(AСriterion: TСriterion);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetUpDate(const AValueRSI: Double);
    ///<summary>Количество критериев</summary>
    procedure CreateСriterion(const ACount: Integer = 1);
    ///<summary>Список критериев</summary>
    property Сriterions: TСriterionList read FСriterions;
    ///<summary>Направление заявки</summary>
    property Side: TTypeSide read FSide write FSide;
    ///<summary>Номер трейдера</summary>
    property ID: Integer read FID write FID;
    ///<summary>Событие трейдера</summary>
    property OnOperationTrade: TEventOperationTrade write FOnOperationTrade;
  end;
  TTraderList = TObjectList<TTrader>;

  ///<summary>Сделка - торговля</summary>
  TTrade = record
    Date : TDateTime;
    Time : TDateTime;
    Price: Double;
    Qty  : Double;
    Side : TTypeSide;
  private
    function GetValue: Double;
  public
    property Value: Double read GetValue;
  end;
  TTradeList = TList<TTrade>;

  ///<summary>Позиция последки</summary>
  TPositionTrade = class(TObject)
  private
    FTrader: TTrader;
    FCloseTrade: TTrade;
    FTrades: TTradeList;
    FTypePosition: TTypePosition;
    function GetSide: TTypeSide;
    function GetQty: Double;
    function GetValue: Double;
    function GetPrice: Double;
    function GetProfit: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>Открытие позиции</summary>
    procedure SetOpenTrade(ADate, ATime: TDateTime; APrice, AQty: Double; ASide: TTypeSide);
    ///<summary>Закрытия позиции</summary>
    procedure SetCloseTrade(ADate, ATime: TDateTime; APrice, AQty: Double; ASide: TTypeSide);
    ///<summary>Тип позиции</summary>
    property TypePosition: TTypePosition read FTypePosition;
    ///<summary>Кто совершил торговую операцию</summary>
    property Trader: TTrader read FTrader write FTrader;
    ///<summary>Список проведенных сделок</summary>
    property Trades: TTradeList read FTrades;
    ///<summary>Сделка закрытие позиции</summary>
    property CloseTrade: TTrade read FCloseTrade;
  public
    property Price: Double read GetPrice;
    property Value: Double read GetValue;
    property Qty  : Double read GetQty;
    property Side : TTypeSide read GetSide;
    property Profit: Double read GetProfit;
  end;
  TPositionTradeList = TObjectList<TPositionTrade>;

  ///<summary>Рабочий трейдер</summary>
  TWorkTrader = class(TTrader)
  private
    FCandel: TCandel;
    FActivePosition: TPositionTrade;
    FPositionTrades: TPositionTradeList;
    function GetProfit: Double;
  protected
    procedure DoEventOperationTrade(ATypeСriterion: TTypeСriterion; ASide: TTypeSide; AQty: Double); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetUpDateCandel(ACandel: TCandel; AValueRSI: Double);
    property ActivePosition: TPositionTrade read FActivePosition;
    property PositionTrades: TPositionTradeList read FPositionTrades;
    ///<summary>Полученная прибыль по закрытым позициям</summary>
    property Profit: Double read GetProfit;
  end;
  TWorkTraderList = TObjectList<TWorkTrader>;


function GetTypeSideToStr(const ASide: TTypeSide): String;

implementation

uses
  UnitMainForm;

function GetTypeSideToStr(const ASide: TTypeSide): String;
begin
  Result := '';
  case ASide of
    tsBuy: Result := 'Buy';
    tsSell: Result := 'Sell';
  end;
end;

{ TСriterion }

constructor TСriterion.Create;
begin
  Default;
end;

procedure TСriterion.Default;
begin
  RSI := 0;
  ReActiveRSI := 0;
  Qty := 0;
  IsActive := False;
end;

{ TTrader }

constructor TTrader.Create;
begin
  FСriterions := TСriterionList.Create;
end;

destructor TTrader.Destroy;
begin
  FСriterions.Clear;
  FreeAndNil(FСriterions);
  inherited;
end;

procedure TTrader.DoEventOperationTrade(ATypeСriterion: TTypeСriterion; ATypeSide: TTypeSide; AQty: Double);
begin
  if Assigned(FOnOperationTrade) then
    FOnOperationTrade(Self,ATypeСriterion,FSide,AQty);
end;

procedure TTrader.DoOperationTrade(AСriterion: TСriterion);

  function _CrossSide(ASide: TTypeSide): TTypeSide;
  begin
    if ASide = tsBuy then
      Result := tsSell
    else
      Result := tsBuy;
  end;

begin
  case AСriterion.TypeСriterion of
    tcOpen: begin
      DoEventOperationTrade(AСriterion.TypeСriterion,FSide,AСriterion.Qty);
    end;
    tcClose: begin
      DoEventOperationTrade(AСriterion.TypeСriterion,_CrossSide(FSide),AСriterion.Qty);
    end;
  end;
end;

procedure TTrader.CreateСriterion(const ACount: Integer);
var
  xСriterion: TСriterion;
  i: Integer;
begin
  // Последний кретерий на закрытие
  FСriterions.Clear;
  for i := 0 to ACount do
  begin
    xСriterion := TСriterion.Create;
    xСriterion.Default;
    if i = (ACount - 1) then
      xСriterion.TypeСriterion := TTypeСriterion.tcOpen
    else
      xСriterion.TypeСriterion := TTypeСriterion.tcClose;
    FСriterions.Add(xСriterion);
  end;
end;

procedure TTrader.SetUpDate(const AValueRSI: Double);

  procedure _ActiveСriterion(AСriterion: TСriterion);
  begin
    // Производим активацию критерия
  end;

  procedure _ReActiveСriterion(AСriterion: TСriterion);
  begin
    // Производим реактивацию критерия
  end;

var
  xСriterion: TСriterion;
  i, iCount: Integer;
begin
  {$IFDEF DB_LOG}
  Log('TTrader.SetUpDateCandel:');
  {$ENDIF}
  iCount := FСriterions.Count;
  if iCount > 0 then
  begin
    {$IFDEF DB_LOG}
    Log(' >>' + iCount.ToString);
    {$ENDIF}
    for i := 0 to iCount - 1 do
    begin
      xСriterion := FСriterions[i];
      if xСriterion.IsActive then
      begin
        case FSide of
          tsBuy: if xСriterion.RSI > AValueRSI then
          begin
            xСriterion.IsActive := False;
            DoOperationTrade(xСriterion);
          end;
          tsSell: if xСriterion.RSI < AValueRSI then
          begin
            xСriterion.IsActive := False;
            DoOperationTrade(xСriterion);
          end;
        end;
      end
      else
      begin
        case FSide of
          tsBuy: if xСriterion.ReActiveRSI < AValueRSI then
            xСriterion.IsActive := True;
          tsSell: if xСriterion.ReActiveRSI > AValueRSI then
            xСriterion.IsActive := True;
        end;
      end;
    end;
  end;
end;

{ TTrade }

function TTrade.GetValue: Double;
begin
  Result := Price * Qty;
end;

{ TPositionTrade }

constructor TPositionTrade.Create;
begin
  FTrades := TTradeList.Create;
  FTypePosition := TTypePosition.tpNull;
end;

destructor TPositionTrade.Destroy;
begin
  FreeAndNil(FTrades);
  inherited;
end;

function TPositionTrade.GetSide: TTypeSide;
begin
  if FTrades.Count > 0 then
    Result := FTrades[0].Side
  else
    raise Exception.Create('Error Message: Нет возможности определить направление позиции');
end;

function TPositionTrade.GetPrice: Double;
begin
  Result := Self.Value/Self.Qty;
end;


function TPositionTrade.GetQty: Double;
var
  xQty: Double;
begin
  if FTrades.Count > 0 then
  begin
    xQty := 0;
    for var xTrade in FTrades do
      xQty := xQty + xTrade.Qty;
    Result := xQty;
  end
  else
    raise Exception.Create('Error Message: Нет возможности определить размер позиции');
end;

function TPositionTrade.GetValue: Double;
var
  xValue: Double;
begin
  if FTrades.Count > 0 then
  begin
    xValue := 0;
    for var xTrade in FTrades do
      xValue := xValue + xTrade.Value;
    Result := xValue;
  end
  else
    raise Exception.Create('Error Message: Нет возможности определить объем позиции');
end;

function TPositionTrade.GetProfit: Double;
begin
  Result := 0;
  if FTypePosition = TTypePosition.tpClose then
  begin
    case Side of
      tsBuy: Result := FCloseTrade.Value - Self.Value;
      tsSell: Result := Self.Value - FCloseTrade.Value;
    end;
  end;
end;

procedure TPositionTrade.SetOpenTrade(ADate, ATime: TDateTime; APrice, AQty: Double; ASide: TTypeSide);

  function _GetQty: Double;
  begin
    Result := 0;
    if FTrades.Count > 0 then
      Result := FTrades[FTrades.Count - 1].Qty;
  end;

var
  xTrade: TTrade;
begin
  if FTrades.Count < 3 then
  begin
    with xTrade do
    begin
      Date := ADate;
      Time := ATime;
      Price:= APrice;
      Qty  := _GetQty + AQty;
      Side := ASide;
    end;
    FTrades.Add(xTrade);
    FTypePosition := TTypePosition.tpOpen;
  end;
end;

procedure TPositionTrade.SetCloseTrade(ADate, ATime: TDateTime; APrice, AQty: Double; ASide: TTypeSide);
begin
  if FTypePosition = TTypePosition.tpOpen then
  begin
    with FCloseTrade do
    begin
      Date := ADate;
      Time := ATime;
      Price:= APrice;
      Qty  := AQty;
      Side := ASide;
    end;
    FTypePosition := TTypePosition.tpClose;
  end;
end;

{ TWorkTrader }

constructor TWorkTrader.Create;
begin
  inherited;
  FActivePosition := nil;
  FPositionTrades := TPositionTradeList.Create;
end;

destructor TWorkTrader.Destroy;
begin
  FPositionTrades.Clear;
  FreeAndNil(FPositionTrades);
  inherited;
end;

procedure TWorkTrader.DoEventOperationTrade(ATypeСriterion: TTypeСriterion;
  ASide: TTypeSide; AQty: Double);
begin
  inherited;
  if Assigned(FActivePosition) then
  begin
    case ATypeСriterion of
      tcOpen: FActivePosition.SetOpenTrade(FCandel.Date, FCandel.Time, FCandel.Close, AQty, ASide);
      tcClose: begin
        FActivePosition.SetCloseTrade(FCandel.Date, FCandel.Time, FCandel.Close, FActivePosition.Qty, ASide);
        FActivePosition := nil;
      end;
    end;
  end
  else
    if ATypeСriterion = tcOpen then
    begin
      FActivePosition := TPositionTrade.Create;
      FActivePosition.Trader := TTrader(Self);
      FActivePosition.SetOpenTrade(FCandel.Date, FCandel.Time, FCandel.Close, AQty, ASide);
      FPositionTrades.Add(FActivePosition);
    end;
end;

procedure TWorkTrader.SetUpDateCandel(ACandel: TCandel; AValueRSI: Double);
begin
  {$IFDEF DB_LOG}
  Log('TWorkTrader.SetUpDateCandel:');
  {$ENDIF}
  FCandel := ACandel;
  SetUpDate(AValueRSI);
end;

function TWorkTrader.GetProfit: Double;
var
  xSum: Double;
  i, iCount: Integer;
  xPositionTrade: TPositionTrade;
begin
  xSum := 0;
  iCount := FPositionTrades.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xPositionTrade := FPositionTrades[i];
      xSum := xSum + xPositionTrade.Profit;
    end;
  Result := xSum;
end;

end.
