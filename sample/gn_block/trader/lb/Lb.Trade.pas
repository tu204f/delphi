unit Lb.Trade;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  TInputParam = record
    Date: TDateTime;
    Time: TDateTime;
    Price: Double;
    BuySell: Char;
    ValueRSI: Double;
  end;


  ///<summary>
  /// Сделка
  ///</summary>
  TTrade = class(TObject)
  private
    FPrice: Double;
    FMaxProfit: Double;
    FMinProfit: Double;
    FProfit: Double;
    FBuySell: Char;
    FCountCandel: Integer;
    FTime: TDateTime;
    FDate: TDateTime;
    FValueRSI: Double;
  public
    constructor Create(AInputParam: TInputParam);
    destructor Destroy; override;
    procedure SetUpDate(const AHigh, ALow, AClose: Double);
    property Date: TDateTime read FDate;
    property Time: TDateTime read FTime;
    property Price: Double read FPrice;
    property BuySell: Char read FBuySell;
    property MaxProfit: Double read FMaxProfit;
    property MinProfit: Double read FMinProfit;
    property Profit: Double read FProfit;
    property ValueRSI: Double read FValueRSI;
    property CountCandel: Integer read FCountCandel;
  public
    function ToString: string; override;
  end;

  ///<summary>
  /// Список сделок
  ///</summary>
  TTradeList = TObjectList<TTrade>;

  ///<summary>
  /// Дата дня
  ///</summary>
  TDateTrade = class(TObject)
  private
    FDate: TDateTime;
    FCurrentTrade: TTrade;
    FTrades: TTradeList;
    function GetIsCurrentTrade: Boolean;
  protected
    FProfit: Double;
    FCountTakeProfit: Integer;
    FCountStopLoss: Integer;
    FCountNull: Integer;
  public
    constructor Create(const ADate: TDateTime);
    destructor Destroy; override;
    ///<summary>Добавить сделку</summary>
    procedure AddTrade(AInputParam: TInputParam);
    ///<summary>Обновление значение сделки</sumamry>
    procedure SetUpDate(const AHigh, ALow, AClose: Double);
    ///<summary>
    /// Дата - сделки
    ///</summary>
    property Date: TDateTime read FDate;
    property Trades: TTradeList read FTrades;
    property IsCurrentTrade: Boolean read GetIsCurrentTrade;
  public
    procedure SetUpDateTrade;
    property Profit: Double read FProfit;
    property CountTakeProfit: Integer read FCountTakeProfit;
    property CountStopLoss: Integer read FCountStopLoss;
    property CountNull: Integer read FCountNull;
  public
    function ToString: string; override;
  end;

  ///<summary>
  /// Список датах
  ///</summary>
  TDateTradeList = TObjectList<TDateTrade>;

implementation

{ TTrade }

constructor TTrade.Create(AInputParam: TInputParam);
begin
  FDate := AInputParam.Date;
  FTime := AInputParam.Time;
  FPrice :=  AInputParam.Price;
  FMaxProfit := 0;
  FMinProfit := 0;
  FBuySell := AInputParam.BuySell;
  FCountCandel := 0;
  FValueRSI := AInputParam.ValueRSI;
end;

destructor TTrade.Destroy;
begin

  inherited;
end;

procedure TTrade.SetUpDate(const AHigh, ALow, AClose: Double);
var
  xMaxProfit, xMinProfit: Double;
begin
  xMaxProfit := 0;
  xMinProfit := 0;
  case FBuySell of
    'B': begin
      xMaxProfit := AHigh - FPrice;
      xMinProfit := ALow - FPrice;
      FProfit    := AClose - FPrice;
    end;
    'S': begin
      xMinProfit := FPrice - AHigh;
      xMaxProfit := FPrice - ALow;
      FProfit    := FPrice - AClose;
    end;
  end;
  if xMaxProfit > FMaxProfit then
    FMaxProfit := xMaxProfit;
  if xMinProfit < FMinProfit then
    FMinProfit := xMinProfit;
  Inc(FCountCandel);
end;

function TTrade.ToString: String;
var
  xS: String;
begin
  xS :=
    DateToStr(Date) + ';' +
    TimeToStr(Time) + ';' +
    Price.ToString + ';' +
    BuySell + ';' +
    Profit.ToString + ';' +
    MaxProfit.ToString + ';' +
    MinProfit.ToString + ';' +
    ValueRSI.ToString + ';' +
    CountCandel.ToString;
  Result := xS;
end;

{ TDateTrade }

constructor TDateTrade.Create(const ADate: TDateTime);
begin
  FDate := ADate;
  FCurrentTrade := nil;
  FTrades := TTradeList.Create;
end;

destructor TDateTrade.Destroy;
begin
  FreeAndNil(FTrades);
  inherited;
end;

function TDateTrade.GetIsCurrentTrade: Boolean;
begin
  Result := Assigned(FCurrentTrade);
end;

procedure TDateTrade.AddTrade(AInputParam: TInputParam);
begin
  FCurrentTrade := TTrade.Create(AInputParam);
  FTrades.Add(FCurrentTrade);
end;

procedure TDateTrade.SetUpDate(const AHigh, ALow, AClose: Double);
begin
  if Assigned(FCurrentTrade) then
    FCurrentTrade.SetUpDate(AHigh, ALow, AClose);
end;

procedure TDateTrade.SetUpDateTrade;
var
  xTrade: TTrade;
  i, iCount: Integer;
begin
  FProfit := 0;
  FCountTakeProfit := 0;
  FCountStopLoss := 0;
  FCountNull := 0;

  iCount := FTrades.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xTrade := FTrades[i];
      FProfit := FProfit + xTrade.Profit;
      if xTrade.Profit > 0 then
        Inc(FCountTakeProfit)
      else if xTrade.Profit < 0 then
        Inc(FCountStopLoss)
      else
        Inc(FCountNull);
    end;
end;

function TDateTrade.ToString: string;
var
  xS: String;
begin
  xS :=
    DateToStr(Date) + '; ' +
    'Profit = ' + Profit.ToString + '; ' +
    CountTakeProfit.ToString + '; ' +
    CountStopLoss.ToString;
  Result := xS;
end;

end.
