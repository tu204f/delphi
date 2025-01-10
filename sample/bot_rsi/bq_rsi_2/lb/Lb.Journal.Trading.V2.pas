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
  TJournalManager = class;

  ///<summary>Когда позиция открывает новую сделку</summary>
  TEventOnNewPositionTrade = procedure(ASander: TObject; ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList) of object;

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
    FID: Integer;
    FPrice: Double;
    FQty: Double;
    FTrades: TJournalTradeList;
    function GetIsActive: Boolean;
    function GetQty: Double;
    function GetSide: TTypeBuySell;
  private
    FProfit: Double;
    FMaxProfit: Double;
    FMinProfit: Double;
    FProfits: TDoubleList;
  private
    FManager: TJournalManager;
    FOnNewPositionTrade: TEventOnNewPositionTrade;
    FOnOpen: TEventOnOpen;
    FOnClose: TEventOnClose;
  protected
    ///<summary>Обновить информацию по сделке</summary>
    procedure SetUpDate;
    procedure DoOpen;
    procedure DoClose;
    procedure DoNewPositionTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    ///<summary>Прошла сделка</summary>
    procedure OpenTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);

    ///<summary>Закрытие позиции</summary>
    procedure CloseTrade(ATime: TDateTime; APrice: Double; ACandels: TCandelList);

    ///<summary>Обновление параметров</summary>
    procedure SetUpDateValue(const APrice, AValueRIS, AValueAveragRSI, AValueART: Double);

    ///<summary>Список сделок</summary>
    property Trades: TJournalTradeList read FTrades;
    property IsActive: Boolean read GetIsActive;
    property Price: Double read FPrice;
    property Qty: Double read GetQty;
    property Side: TTypeBuySell read GetSide;
    property OnNewPositionTrade: TEventOnNewPositionTrade write FOnNewPositionTrade;
    property OnOpen: TEventOnOpen write FOnOpen;
    property OnClose: TEventOnClose write FOnClose;
    property Manager: TJournalManager read FManager write FManager;
    property ID: Integer read FID write FID;
  public
    property Profit: Double read FProfit;
    property MaxProfit: Double read FMaxProfit;
    property MinProfit: Double read FMinProfit;
    property Profits: TDoubleList read FProfits;
  end;

  ///<summary>Список позиций</summary>
  TJournalPositionList = TObjectList<TJournalPosition>;

  ///<summary>Менеджер позиций</summary>
  TJournalManager = class(TObject)
  private
    FPositions: TJournalPositionList;
    function GetCurrentPosition: TJournalPosition;
    function GetIsCurrentPosition: Boolean;
    function GetProfit: Double;
  protected
    function GetCreateJournalPosition: TJournalPosition;
    procedure DoNewTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    ///<summary>Прошла сделка</summary>
    procedure OpenTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);

    ///<summary>Закрытие позиции</summary>
    procedure CloseTrade(ATime: TDateTime; APrice: Double; ACandels: TCandelList);

    ///<summary>Закрытие позиции</summary>
    procedure ReverseTrade(ATime: TDateTime; APrice: Double; ACandels: TCandelList);

    ///<summary>Проверяет существование позиции</summary>
    property IsCurrentPosition: Boolean read GetIsCurrentPosition;
    ///<summary>Текущие позиция</summary>
    property CurrentPosition: TJournalPosition read GetCurrentPosition;
    ///<summary>Список позиций</summary>
    property Positions: TJournalPositionList read FPositions;
    ///<summary>Сумарный профит повсем позициям</summary>
    property Profit: Double read GetProfit;
  end;

implementation

{$IFDEF DEBUG}
uses
  Lb.Logger;
{$ENDIF}

procedure SaveJournalPosition(AJournalPosition: TJournalPosition; AJournal: TStrings);

  procedure _Trades(ATrades: TJournalTradeList);
  var
    xS: String;
    xTrade: TJournalTrade;
    xF: TFormatSettings;
    i, iCount: Integer;
  begin
    xF := FormatSettings;
    xF.DecimalSeparator := '.';

    AJournal.Add('## Список сделок');
    AJournal.Add('[trades]');
    AJournal.Add('ID;Time;Price;Qty;Side;');

    iCount := ATrades.Count;
    for i := 0 to iCount - 1 do
    begin
      xTrade := ATrades[i];
      xS :=
        IntToStr(i) + ';' +
        DateTimeToStr(xTrade.Time) + ';' +
        FloatToStr(xTrade.Price,xF) + ';' +
        FloatToStr(xTrade.Qty,xF) + ';' +
        GetStrToSide(xTrade.Side) + ';';
      AJournal.Add(xS);
    end;
  end;

  procedure _TradeCandes(ATrades: TJournalTradeList);
  var
    xS: String;
    xTrade: TJournalTrade;
    xF: TFormatSettings;
    i, iCount: Integer;
    j, jCount: Integer;
    xCandel: TCandel;
  begin
    xF := FormatSettings;
    xF.DecimalSeparator := '.';

    AJournal.Add('## Список сделок');
    AJournal.Add('[candels]');
    AJournal.Add('TradeID;CandelID;Time;Open;High;Low;Close;Vol;');

    iCount := ATrades.Count;
    for i := 0 to iCount - 1 do
    begin
      xTrade := ATrades[i];
      jCount := xTrade.Candels.Count;
      for j := 0 to jCount - 1 do
      begin
        xCandel := xTrade.Candels[j];
        xS :=
          i.ToString + ';' +
          j.ToString + ';' +
          IntToStr(xCandel.Time) + ';' +
          FloatToStr(xCandel.Open,xF) + ';' +
          FloatToStr(xCandel.High,xF) + ';' +
          FloatToStr(xCandel.Low,xF) + ';' +
          FloatToStr(xCandel.Close,xF) + ';' +
          FloatToStr(xCandel.Vol,xF) + ';';
        AJournal.Add(xS);
      end;
    end;
  end;

  procedure _Profits(AProfits: TDoubleList);
  var
    xS: String;
    xF: TFormatSettings;
    i, iCount: Integer;
  begin
    xF := FormatSettings;
    xF.DecimalSeparator := '.';

    AJournal.Add('## динамика профита');
    AJournal.Add('[profits]');
    AJournal.Add('ID;Value;');

    iCount := AProfits.Count;
    for i := 0 to iCount - 1 do
    begin
      var xValue := AProfits[i];
      xS :=
        i.ToString + ';' +
        FloatToStr(xValue,xF) + ';';
      AJournal.Add(xS);
    end;
  end;

var
  xF: TFormatSettings;
begin
  if not Assigned(AJournal) then
    Exit;

  xF := FormatSettings;
  xF.DecimalSeparator := '.';
  AJournal.Add('## Позиция по сделкам');
  AJournal.Add('[journal]');
  AJournal.Add('id;is_active;price;qty;side;profit;max_profit;min_profit;');
  var xS :=
    '0;' +
    BoolToStr(AJournalPosition.IsActive) + ';' +
    FloatToStr(AJournalPosition.Price,xF) + ';' +
    FloatToStr(AJournalPosition.Price,xF) + ';' +
    GetStrToSide(AJournalPosition.Side) + ';' +
    FloatToStr(AJournalPosition.Profit,xF) + ';' +
    FloatToStr(AJournalPosition.MaxProfit,xF) + ';' +
    FloatToStr(AJournalPosition.MinProfit,xF) + ';';
  AJournal.Add(xS);

  _Trades(AJournalPosition.Trades);
  _TradeCandes(AJournalPosition.Trades);
  _Profits(AJournalPosition.Profits);
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
  FTrades := TJournalTradeList.Create;

  FMaxProfit := 0;
  FMinProfit := 0;
  FProfits:= TDoubleList.Create;
end;

destructor TJournalPosition.Destroy;
begin
  FreeAndNil(FProfits);
  FreeAndNil(FTrades);
  inherited;
end;

procedure TJournalPosition.DoNewPositionTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);
begin
  if Assigned(FOnNewPositionTrade) then
    FOnNewPositionTrade(Self, ATime, APrice, AQty, ASide, ACandels);
  if Assigned(FManager) then
    FManager.DoNewTrade(ATime, APrice, AQty, ASide, ACandels)
end;

procedure TJournalPosition.DoOpen;
begin
  if Assigned(FOnOpen) then
    FOnOpen(Self);
end;

procedure TJournalPosition.DoClose;
var
  xFN: string;
  xStr: TStrings;
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
  {todo: сохранить информция о позии}

  xStr := TStringList.Create;
  try
    SaveJournalPosition(Self,xStr);
    xFN := ExtractFilePath(ParamStr(0)) + 'data' + PathDelim + 'posiotn_' + FID.ToString + '.txt';
    xStr.SaveToFile(xFN,TEncoding.ANSI);
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TJournalPosition.OpenTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);

  procedure _CreateJournalTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);
  var
    xTrade: TJournalTrade;
  begin
    xTrade := TJournalTrade.Create;
    xTrade.Time := ATime;
    xTrade.Price := APrice;
    xTrade.Qty := AQty;
    xTrade.Side := ASide;
    xTrade.Candels.CopyCandels(ACandels);

    FTrades.Add(xTrade);
    SetUpDate;
  end;

begin
  if Self.Qty = 0 then
  begin
    _CreateJournalTrade(ATime, APrice, AQty, ASide, ACandels);
    DoOpen;
  end
  else
  begin
    if Self.Side = ASide then
    begin
      _CreateJournalTrade(ATime, APrice, AQty, ASide, ACandels);
    end
    else
    begin
      if Self.Qty >= AQty then
      begin
        _CreateJournalTrade(ATime, APrice, AQty, ASide, ACandels);
        if GetRound(Self.Qty - AQty) < 0 then
          DoClose;
      end
      else
      begin
        var xQty := GetRound(AQty - Self.Qty);
        _CreateJournalTrade(ATime, APrice, Self.Qty, ASide, ACandels);
        DoClose;
        DoNewPositionTrade(ATime, APrice, xQty, ASide, ACandels);
      end
    end;
  end;
end;

procedure TJournalPosition.SetUpDate;

  procedure _CalcQty;
  begin
    FQty := 0;
    for var xTrade in FTrades do
    begin
      case xTrade.Side of
        tsBuy: FQty := FQty + xTrade.Qty;
        tsSell: FQty := FQty - xTrade.Qty;
      end;
    end;
    FQty := GetRound(FQty);
  end;

  procedure _CalcPrice;
  var
    xSumValue: Double;
    xQty: Double;
    xSide: TTypeBuySell;
    i, iCount: Integer;
    xJournalTrade: TJournalTrade;
  begin
    FPrice := 0;
    if IsActive then
    begin
      iCount := FTrades.Count;
      if iCount > 0 then
      begin
        xSumValue := 0;
        xQty := FQty;
        xSide := GetSide;
        for i := (iCount - 1) downto 0 do
        begin
          xJournalTrade := FTrades[i];
          if xJournalTrade.Side = xSide then
          begin
            var tmpQty := xQty - xJournalTrade.Qty;

            if tmpQty > 0 then
            begin
              xQty := tmpQty;
              xSumValue := xSumValue + xJournalTrade.Value;
            end;

            if tmpQty = 0 then
            begin
              xSumValue := xSumValue + xJournalTrade.Value;
              Break;
            end;

            if tmpQty < 0 then
            begin
              xSumValue := xSumValue + xJournalTrade.Price * xQty;
              Break;
            end;

          end;
        end;
        FPrice := xSumValue/FQty;
      end;
    end;
  end;

begin
  _CalcQty;
  _CalcPrice;
end;

function TJournalPosition.GetIsActive: Boolean;
begin
  Result := not (FQty = 0);
end;


function TJournalPosition.GetQty: Double;
begin
  Result := Abs(FQty);
end;

function TJournalPosition.GetSide: TTypeBuySell;
begin
  if FQty > 0 then
    Result := TTypeBuySell.tsBuy
  else if FQty < 0 then
    Result := TTypeBuySell.tsSell
  else
    Result := TTypeBuySell.tsNull;
end;

procedure TJournalPosition.SetUpDateValue(const APrice, AValueRIS, AValueAveragRSI, AValueART: Double);

  function _ProfitSumm: Double;
  var
    xSum: Double;
  begin
    xSum := 0;
    for var xTrade in FTrades do
    begin
      case xTrade.Side of
        tsBuy: xSum := xSum - xTrade.Value;
        tsSell: xSum := xSum + xTrade.Value;
      end;
    end;
    Result := xSum;
  end;

var
  xProfit: Double;
begin
  if FTrades.Count > 0 then
  begin
    if FQty = 0 then
    begin
      xProfit := _ProfitSumm;
    end
    else if FPrice > 0 then
    begin
      case Self.Side of
        tsBuy: xProfit := _ProfitSumm + APrice * Qty;
        tsSell: xProfit := _ProfitSumm - APrice * Qty;
      else
        xProfit := 0;
      end;
    end
    else
      xProfit := 0;
  end
  else
    xProfit := 0;

  if not (xProfit = 0) then
    xProfit := GetRound(xProfit);

  if FMaxProfit < xProfit then
    FMaxProfit := xProfit;

  if FMinProfit > xProfit then
    FMinProfit := xProfit;

  if not SameValue(FProfit,xProfit,0.01) then
  begin
    FProfits.Add(xProfit);
    FProfit := xProfit;
  end;
end;

procedure TJournalPosition.CloseTrade(ATime: TDateTime; APrice: Double; ACandels: TCandelList);
begin
  Self.OpenTrade(
    ATime,
    APrice,
    Self.Qty,
    GetCrossSide(Self.Side),
    ACandels
  );
  DoClose;
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
{$IFDEF DBG_JOURNAL_MANAGER}
  TLogger.LogTree(0,'TJournalManager.GetCreateJournalPosition:');
  TLogger.LogTreeText(3,'>> Создание Жернал сделок в позиции');
{$ENDIF}
  xJournalPosition := TJournalPosition.Create;
  xJournalPosition.Manager := Self;
  xJournalPosition.ID := FPositions.Count;
  FPositions.Add(xJournalPosition);
  Result := xJournalPosition;
end;

function TJournalManager.GetCurrentPosition: TJournalPosition;
var
  iCount: Integer;
  xPosition: TJournalPosition;
begin
  Result := nil;
  iCount := FPositions.Count;
  if iCount > 0 then
  begin
    xPosition := FPositions[iCount - 1];
    Result := xPosition;
  end;
end;

function TJournalManager.GetIsCurrentPosition: Boolean;
var
  xJournalPosition: TJournalPosition;
begin
  Result := False;
  xJournalPosition := GetCurrentPosition;
  if Assigned(xJournalPosition) then
    Result := xJournalPosition.IsActive;
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

procedure TJournalManager.OpenTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);
var
  xJournalPosition: TJournalPosition;
begin
{$IFDEF DBG_JOURNAL_MANAGER}
  TLogger.LogTree(0,'TJournalManager.OpenTrade:');
  TLogger.LogTreeText(3,'>> Открытие позиция');
  TLogger.LogTreeText(3,'>> Time := ' + DateTimeToStr(ATime));
  TLogger.LogTreeText(3,'>> Price := ' + FloatToStr(APrice));
  TLogger.LogTreeText(3,'>> Qty := ' + FloatToStr(AQty));
  TLogger.LogTreeText(3,'>> Side := ' + GetStrToSide(ASide));
{$ENDIF}
  if IsCurrentPosition then
  begin
    Self.CurrentPosition.OpenTrade(ATime, APrice, AQty, ASide, ACandels)
  end
  else
  begin
    xJournalPosition := GetCreateJournalPosition;
    xJournalPosition.OpenTrade(ATime, APrice, AQty, ASide, ACandels);
  end;
end;

procedure TJournalManager.ReverseTrade(ATime: TDateTime; APrice: Double; ACandels: TCandelList);
var
  xQty: Double;
  xSide: TTypeBuySell;
begin
{$IFDEF DBG_JOURNAL_MANAGER}
  TLogger.LogTree(0,'TJournalManager.ReverseTrade:');
  TLogger.LogTreeText(3,'>> Реверс позиции');
  TLogger.LogTreeText(3,'>> Time := ' + DateTimeToStr(ATime));
  TLogger.LogTreeText(3,'>> Price := ' + FloatToStr(APrice));
{$ENDIF}
  if IsCurrentPosition then
  begin
    xQty := CurrentPosition.Qty;
    xSide := CurrentPosition.Side;
    Self.CurrentPosition.OpenTrade(
      ATime,
      APrice,
      2 * xQty,
      GetCrossSide(xSide),
      ACandels
    );
  end;
end;

procedure TJournalManager.CloseTrade(ATime: TDateTime; APrice: Double; ACandels: TCandelList);
begin
{$IFDEF DBG_JOURNAL_MANAGER}
  TLogger.LogTree(0,'TJournalManager.CloseTrade:');
  TLogger.LogTreeText(3,'>> Закрытие позиции');
  TLogger.LogTreeText(3,'>> Time := ' + DateTimeToStr(ATime));
  TLogger.LogTreeText(3,'>> Price := ' + FloatToStr(APrice));
{$ENDIF}
  if IsCurrentPosition then
    Self.CurrentPosition.CloseTrade(ATime, APrice, ACandels)
end;

procedure TJournalManager.DoNewTrade(ATime: TDateTime; APrice, AQty: Double; ASide: TTypeBuySell; ACandels: TCandelList);
var
  xJournalPosition: TJournalPosition;
begin
{$IFDEF DBG_JOURNAL_MANAGER}
  TLogger.LogTree(0,'TJournalManager.DoNewTrade:');
  TLogger.LogTreeText(3,'>> Продолжаем открытие позиции');
  TLogger.LogTreeText(3,'>> Time := ' + DateTimeToStr(ATime));
  TLogger.LogTreeText(3,'>> Price := ' + FloatToStr(APrice));
  TLogger.LogTreeText(3,'>> Qty := ' + FloatToStr(AQty));
  TLogger.LogTreeText(3,'>> Side := ' + GetStrToSide(ASide));
{$ENDIF}
  xJournalPosition := GetCreateJournalPosition;
  xJournalPosition.OpenTrade(ATime, APrice, AQty, ASide, ACandels);
end;

end.
