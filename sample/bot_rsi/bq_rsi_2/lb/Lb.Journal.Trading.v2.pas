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

const
  FEE_RATES_TAKER = 0.1;
  FEE_RATES_MAKER = 0.036;

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
    function GetFeeRatesMaker: Double;
    function GetFeeRatesTaker: Double;
    function GetProfitFeeRatesMaker: Double;
    function GetProfitFeeRatesTaker: Double;
  private
    FRSI: Double;
    FMaRSI: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetUpData(const APrice: Double = 0);
    procedure DoOpen;
    procedure DoClose;
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
    property FeeRatesTaker: Double read GetFeeRatesTaker;
    property FeeRatesMaker: Double read GetFeeRatesMaker;
    property ProfitFeeRatesTaker: Double read GetProfitFeeRatesTaker;
    property ProfitFeeRatesMaker: Double read GetProfitFeeRatesMaker;
  public
    property OnOpen: TEventOnOpen write FOnOpen;
    property OnClose: TEventOnClose write FOnClose;
    property Manager: TJournalManager read FManager write FManager;
    property ID: Integer read FID write FID;
  public
    property RSI: Double read FRSI write FRSI;
    property MaRSI: Double read FMaRSI write FMaRSI;
  end;

  ///<summary>Список позиций</summary>
  TJournalPositionList = TObjectList<TJournalPosition>;

  ///<summary>Менеджер позиций</summary>
  TJournalManager = class(TObject)
  private
    FPositions: TJournalPositionList;
    function GetProfit: Double;
    function GetProfitFeeRatesMaker: Double;
    function GetProfitFeeRatesTaker: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>Создание журнала позиции</summary>
    function GetCreateJournalPosition: TJournalPosition;
    function GetSumCountIsActive: Integer;
    ///<summary>Список позиций</summary>
    property Positions: TJournalPositionList read FPositions;

    ///<summary>Сумарный профит повсем позициям</summary>
    property Profit: Double read GetProfit;
    property ProfitFeeRatesTaker: Double read GetProfitFeeRatesTaker;
    property ProfitFeeRatesMaker: Double read GetProfitFeeRatesMaker;

  end;

implementation

uses
  System.Math;

var
  localID: Integer = 0;
  LogCS: TCriticalSection;


procedure PositionText(S: String);
var
  F: TextFile;
  xPath: String;
begin
  LogCS.Enter;
  try
    xPath := ExtractFilePath(ParamStr(0)) + 'position.csv';
    AssignFile(f,xPath);
    if FileExists(xPath) then
      Append(F)
    else
      Rewrite(F);
    WriteLn(F, S);
    CloseFile(F);
  finally
    LogCS.Leave;
  end;
end;

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

  function _Add(S: String): String;
  begin
    Result := S + ';';
  end;

var
  xS: String;
begin
  if Self.Profit > 0 then
  begin
    Inc(localID);

    xS := '';
    xS := xS + _Add(localID.ToString);
    xS := xS + _Add(DateTimeToStr(Self.OpenTime));
    xS := xS + _Add(FloatToStr(Self.OpenPrice));

    if Self.ClosePrice = 0 then
    begin
      xS := xS + _Add('');
      xS := xS + _Add('');
    end else
    begin
      xS := xS + _Add(DateTimeToStr(Self.CloseTime));
      xS := xS + _Add(FloatToStr(Self.ClosePrice));
    end;

    xS := xS + _Add(FloatToStr(Self.Qty));
    xS := xS + _Add(GetStrToSide(Self.Side));
    xS := xS + _Add(FloatToStr(Self.StopLoss));
    xS := xS + _Add(FloatToStr(Self.TakeProfit));
    xS := xS + _Add(FloatToStr(Self.Profit));
    xS := xS + _Add(GetStrToTypeTrade(Self.TypeTrade));

    xS := xS + _Add(FloatToStr(Self.RSI));
    xS := xS + _Add(FloatToStr(Self.MaRSI));

    PositionText(xS);
  end;

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
        if FClosePrice <= 0 then
          raise Exception.Create('Error Message: Цена закрытие не может быть нулейо ');
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
          xStopLoss := APrice + FTriling;
          if FStopLoss = 0  then
            FStopLoss := xStopLoss;
          if xStopLoss < FStopLoss then
            FStopLoss := xStopLoss;
          if FStopLoss <= 0 then
            raise Exception.Create('Error Message: Стоп лосс не может быть нулевым');
        end;
      end;
    end;
  end;

  procedure _ActiveTakeProfit(APrice: Double);
  begin
    if FTakeProfit <= 0 then
      Exit;

    if FTypeTrade = TTypeTrade.ttOpen then
    begin
      case FSide of
        tsBuy: begin
          if APrice > FTakeProfit then
          begin
            CloseTime := GetNewDateTime;
            ClosePrice := APrice;
            IsActive := False;
            TypeTrade := TTypeTrade.ttClose;
            DoClose;
          end;
        end;
        tsSell: begin
          if APrice < FTakeProfit then
          begin
            CloseTime := GetNewDateTime;
            ClosePrice := APrice;
            IsActive := False;
            TypeTrade := TTypeTrade.ttClose;
            DoClose;
          end;
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
            DoClose;
          end;
        end;
        tsSell: begin
          if APrice > FStopLoss then
          begin
            CloseTime := GetNewDateTime;
            ClosePrice := APrice;
            IsActive := False;
            TypeTrade := TTypeTrade.ttClose;
            DoClose;
          end;
        end;
      end;
    end;
  end;

begin
  if FOpenPrice <= 0 then
    raise Exception.Create('Error Message: Нет цены открытие');

  _CalcProfit(APrice);
  _CalcTrelingStopLoss(APrice);
  _ActiveTakeProfit(APrice);
  _ActiveStopLoss(APrice);
end;


function GetFeeRates(const APrice, AQty, AFeeRates: Double): Double;
begin
  Result := APrice * AQty * AFeeRates / 100;
end;

function TJournalPosition.GetFeeRatesMaker: Double;
begin
  Result :=
    GetFeeRates(FOpenPrice,FQty,FEE_RATES_MAKER) +
    GetFeeRates(FClosePrice,FQty,FEE_RATES_MAKER);
end;

function TJournalPosition.GetFeeRatesTaker: Double;
begin
  Result :=
    GetFeeRates(FOpenPrice,FQty,FEE_RATES_TAKER) +
    GetFeeRates(FClosePrice,FQty,FEE_RATES_TAKER);
end;

function TJournalPosition.GetProfitFeeRatesMaker: Double;
begin
  Result := Self.Profit - Self.FeeRatesMaker;
end;

function TJournalPosition.GetProfitFeeRatesTaker: Double;
begin
  Result := Self.Profit - Self.FeeRatesTaker;
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
  for var xP in FPositions do
    xSum := xSum + xP.Profit;
  Result := xSum;
end;

function TJournalManager.GetProfitFeeRatesMaker: Double;
begin
  var xSum := 0.0;
  for var xP in FPositions do
    xSum := xSum + xP.GetProfitFeeRatesMaker;
  Result := xSum;
end;

function TJournalManager.GetProfitFeeRatesTaker: Double;
begin
  var xSum := 0.0;
  for var xP in FPositions do
    xSum := xSum + xP.GetProfitFeeRatesTaker;
  Result := xSum;
end;

function TJournalManager.GetSumCountIsActive: Integer;
var
  xCnt: Integer;
  i, iCount: Integer;
  xPosition: TJournalPosition;
begin
  xCnt := 0;
  iCount := FPositions.Count;
  if iCount > 0 then
  begin
    for i := iCount - 1 downto 0 do
    begin
      xPosition := FPositions[i];
      if xPosition.IsActive then
        Inc(xCnt);
    end;
  end;
  Result := xCnt;
end;

initialization
  localID := 0;
  LogCS := TCriticalSection.Create;

finalization
  FreeAndNil(LogCS);

end.
