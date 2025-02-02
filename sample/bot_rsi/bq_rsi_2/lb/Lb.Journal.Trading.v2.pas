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
    FTrades: TJournalTradeList;
    FManager: TJournalManager;
    FOnNewPositionTrade: TEventOnNewTrade;
    FOnOpen: TEventOnOpen;
    FOnClose: TEventOnClose;
  protected
    procedure DoOpen;
    procedure DoClose;
  public
    constructor Create; virtual;
    destructor Destroy; override;


    ///<summary>Список сделок</summary>
    property Trades: TJournalTradeList read FTrades;

    property OnOpen: TEventOnOpen write FOnOpen;
    property OnClose: TEventOnClose write FOnClose;
    ///<summary>Менеджер - работы</summary>
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


//procedure SaveJournalPosition(AJournalPosition: TJournalPosition; AJournal: TStrings);
//
//  procedure _Trades(ATrades: TJournalTradeList);
//  var
//    xS: String;
//    xTrade: TJournalTrade;
//    xF: TFormatSettings;
//    i, iCount: Integer;
//  begin
//    xF := FormatSettings;
//    xF.DecimalSeparator := '.';
//
//    AJournal.Add('## Список сделок');
//    AJournal.Add('[trades]');
//    AJournal.Add('ID;Time;Price;Qty;Side;');
//
//    iCount := ATrades.Count;
//    for i := 0 to iCount - 1 do
//    begin
//      xTrade := ATrades[i];
//      xS :=
//        IntToStr(i) + ';' +
//        DateTimeToStr(xTrade.Time) + ';' +
//        FloatToStr(xTrade.Price,xF) + ';' +
//        FloatToStr(xTrade.Qty,xF) + ';' +
//        GetStrToSide(xTrade.Side) + ';';
//      AJournal.Add(xS);
//    end;
//  end;
//
//  procedure _TradeCandes(ATrades: TJournalTradeList);
//  var
//    xS: String;
//    xTrade: TJournalTrade;
//    xF: TFormatSettings;
//    i, iCount: Integer;
//    j, jCount: Integer;
//    xCandel: TCandel;
//  begin
//    xF := FormatSettings;
//    xF.DecimalSeparator := '.';
//
//    AJournal.Add('## Список сделок');
//    AJournal.Add('[candels]');
//    AJournal.Add('TradeID;CandelID;Time;Open;High;Low;Close;Vol;');
//
//    iCount := ATrades.Count;
//    for i := 0 to iCount - 1 do
//    begin
//      xTrade := ATrades[i];
//      jCount := xTrade.Candels.Count;
//      for j := 0 to jCount - 1 do
//      begin
//        xCandel := xTrade.Candels[j];
//        xS :=
//          i.ToString + ';' +
//          j.ToString + ';' +
//          IntToStr(xCandel.Time) + ';' +
//          FloatToStr(xCandel.Open,xF) + ';' +
//          FloatToStr(xCandel.High,xF) + ';' +
//          FloatToStr(xCandel.Low,xF) + ';' +
//          FloatToStr(xCandel.Close,xF) + ';' +
//          FloatToStr(xCandel.Vol,xF) + ';';
//        AJournal.Add(xS);
//      end;
//    end;
//  end;
//
//  procedure _Profits(AConditionParams: TJournalPosition.TConditionParamList);
//  var
//    xS: String;
//    xF: TFormatSettings;
//    i, iCount: Integer;
//  begin
//    xF := FormatSettings;
//    xF.DecimalSeparator := '.';
//
//    AJournal.Add('## динамика профита');
//    AJournal.Add('[profits]');
//    AJournal.Add('ID;Value;');
//
//    iCount := AConditionParams.Count;
//    for i := 0 to iCount - 1 do
//    begin
//      var xValue := AConditionParams[i];
//      xS :=
//        i.ToString + ';' +
//        FloatToStr(xValue.Profit,xF) + ';' +
//        FloatToStr(xValue.RSI,xF) + ';' +
//        FloatToStr(xValue.AveragRSI,xF) + ';' +
//        FloatToStr(xValue.ART,xF);
//      AJournal.Add(xS);
//    end;
//  end;
//
//var
//  xF: TFormatSettings;
//begin
//  if not Assigned(AJournal) then
//    Exit;
//
//  xF := FormatSettings;
//  xF.DecimalSeparator := '.';
//  AJournal.Add('## Позиция по сделкам');
//  AJournal.Add('[journal]');
//  AJournal.Add('id;is_active;price;qty;side;profit;max_profit;min_profit;');
//  var xS :=
//    '0;' +
//    BoolToStr(AJournalPosition.IsActive) + ';' +
//    FloatToStr(AJournalPosition.Price,xF) + ';' +
//    FloatToStr(AJournalPosition.Price,xF) + ';' +
//    GetStrToSide(AJournalPosition.Side) + ';' +
//    FloatToStr(AJournalPosition.Profit,xF) + ';' +
//    FloatToStr(AJournalPosition.MaxProfit,xF) + ';' +
//    FloatToStr(AJournalPosition.MinProfit,xF) + ';';
//  AJournal.Add(xS);
//
//  _Trades(AJournalPosition.Trades);
//  _TradeCandes(AJournalPosition.Trades);
//  _Profits(AJournalPosition.ConditionParams);
//end;


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
  FTrades := TJournalTradeList.Create;
end;

destructor TJournalPosition.Destroy;
begin
  FreeAndNil(FTrades);
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

//procedure TJournalPosition.OpenTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);
//
//  procedure _CreateJournalTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);
//  var
//    xTrade: TJournalTrade;
//  begin
//    xTrade := TJournalTrade.Create;
//    xTrade.Time := ATime;
//    xTrade.Price := APrice;
//    xTrade.Qty := AQty;
//    xTrade.Side := ASide;
//    xTrade.Candels.CopyCandels(ACandels);
//
//    FTrades.Add(xTrade);
//    //SetUpDate;
//  end;
//
//begin
//  if Self.Qty = 0 then
//  begin
//    _CreateJournalTrade(ATime, APrice, AQty, ASide, ACandels);
//    DoOpen;
//  end
//  else
//  begin
//    if Self.Side = ASide then
//    begin
//      _CreateJournalTrade(ATime, APrice, AQty, ASide, ACandels);
//    end
//    else
//    begin
//      if Self.Qty >= AQty then
//      begin
//        _CreateJournalTrade(ATime, APrice, AQty, ASide, ACandels);
//        if GetRound(Self.Qty - AQty) < 0 then
//          DoClose;
//      end
//      else
//      begin
//        var xQty := GetRound(AQty - Self.Qty);
//        _CreateJournalTrade(ATime, APrice, Self.Qty, ASide, ACandels);
//        DoClose;
//        //DoNewPositionTrade(ATime, APrice, xQty, ASide, ACandels);
//      end
//    end;
//  end;
//end;


//procedure TJournalPosition.SetUpDateValue(const APrice, AValueRIS, AValueAveragRSI, AValueART: Double);
//
//  function _ProfitSumm: Double;
//  var
//    xSum: Double;
//  begin
//    xSum := 0;
//    for var xTrade in FTrades do
//    begin
//      case xTrade.Side of
//        tsBuy: xSum := xSum - xTrade.Value;
//        tsSell: xSum := xSum + xTrade.Value;
//      end;
//    end;
//    Result := xSum;
//  end;
//
//var
//  xProfit: Double;
//  xParam: TConditionParam;
//begin
//  if FTrades.Count > 0 then
//  begin
//    if FQty = 0 then
//    begin
//      xProfit := _ProfitSumm;
//    end
//    else if FPrice > 0 then
//    begin
//      case Self.Side of
//        tsBuy: xProfit := _ProfitSumm + APrice * Qty;
//        tsSell: xProfit := _ProfitSumm - APrice * Qty;
//      else
//        xProfit := 0;
//      end;
//    end
//    else
//      xProfit := 0;
//  end
//  else
//    xProfit := 0;
//
//  if not (xProfit = 0) then
//    xProfit := GetRound(xProfit);
//
//  if FMaxProfit < xProfit then
//    FMaxProfit := xProfit;
//
//  if FMinProfit > xProfit then
//    FMinProfit := xProfit;
//
//  if not SameValue(FProfit,xProfit,0.01) then
//  begin
//
//    xParam.Profit := xProfit;
//    xParam.RSI := AValueRIS;
//    xParam.AveragRSI := AValueAveragRSI;
//    xParam.ART := AValueART;
//
//    FConditionParams.Add(xParam);
//    FProfit := xProfit;
//  end;
//end;

//procedure TJournalPosition.CloseTrade(ATime: TDateTime; APrice: Double; ACandels: TCandelList);
//begin
//  Self.OpenTrade(
//    ATime,
//    APrice,
//    Self.Qty,
//    GetCrossSide(Self.Side),
//    ACandels
//  );
//  DoClose;
//end;

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
