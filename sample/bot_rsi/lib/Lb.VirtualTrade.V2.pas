unit Lb.VirtualTrade.V2;

interface

//{$DEFINE TEST_VT}

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.Generics.Defaults,
{$IFDEF TEST_VT}
  System.IniFiles;
{$ELSE}
  System.IniFiles,
  Lb.SysUtils;
{$ENDIF}

type
{$IFDEF TEST_VT}
  TQBTypeSide = (
    tsNull,
    tsBuy,
    tsSell
  );
{$ENDIF}

  TTypeTrade = (
    ttOpen,
    ttClose
  );

  TParamPositions = class;

  ///<summary>
  /// Параметры открытой операции
  ///</summary>
  TParamTrade = record
    Date: TDateTime;
    Time: TDateTime;
    Symbol: String;
    Side: TQBTypeSide;
    Qty: Double;
    Price: Double;
    OrderLinkId: String;
    TypeTrade: TTypeTrade;
  private
    function GetToStr: String;
    function GetValue: Double;
  public
    procedure SetDefault;
    property ToStr: String read GetToStr;
    property Value: Double read GetValue;
  end;
  TParamTradeList = TList<TParamTrade>;

  ///<summary>
  /// Параметры сделки
  ///</summary>
  TParamPosition = class(TObject)
  private
    FSide: TQBTypeSide;
    FQty: Double;
  private
    FPositions: TParamPositions;
    FOpenTrades: TParamTradeList;
    FHistoryTrades: TParamTradeList;
    function GetPrice: Double;
    function GetQty: Double;
    function GetProfit: Double;
  protected
    procedure DoOpenPosition;
    procedure DoClosePosition(AParamTrade: TParamTrade);
  public
    constructor Create(APositions: TParamPositions); virtual;
    destructor Destroy; override;
    procedure AddTrade(AParamTrade: TParamTrade);
    property OpenTrades: TParamTradeList read FOpenTrades;
    property HistoryTrades: TParamTradeList read FHistoryTrades;
  public
    function GetProfitUpData(const AAsk, ABid: Double): Double;
    property Side: TQBTypeSide read FSide;
    property Qty: Double read GetQty;
    property Price: Double read GetPrice;
    property Profit: Double read GetProfit;
  end;
  TParamPositionList = TObjectList<TParamPosition>;

  TEventOnChangePosition =  procedure(ASender: TObject) of object;
  TEventOnOpenPosition =  procedure(ASender: TObject) of object;
  TEventOnClosePosition = procedure(ASender: TObject; AParamTrade: TParamTrade) of object;

  ///<summary>
  /// Общая позиция по инструменту
  ///</summary>
  TParamPositions = class(TObject)
  private
    FOnOpenPosition: TEventOnOpenPosition;
    FOnClosePosition: TEventOnClosePosition;
    FOnChangePosition: TEventOnChangePosition;
    FCurrentPosition: TParamPosition;
    FPositions: TParamPositionList;
    function GetCount: Integer;
  protected
    procedure DoOpenPosition;
    procedure DoClosePosition(AParamTrade: TParamTrade);
    procedure DoChangePosition;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure AddTrade(AParamTrade: TParamTrade);
    property Positions: TParamPositionList read FPositions;
    property Count: Integer read GetCount;
    property OnOpenPosition: TEventOnOpenPosition write FOnOpenPosition;
    property OnClosePosition: TEventOnClosePosition write FOnClosePosition;
    property OnChangePosition: TEventOnChangePosition write FOnChangePosition;
    property CurrentPosition: TParamPosition read FCurrentPosition;
  public
    procedure Save(const AFileName: String);
  end;

{$IFDEF TEST_VT}
function GetStrToTypeSide(const ASide: TQBTypeSide): String;
{$ENDIF}

implementation

{$IFDEF TEST_VT}
function GetStrToTypeSide(const ASide: TQBTypeSide): String;
begin
  case ASide of
    tsBuy: Result := 'Buy';
    tsSell: Result := 'Sell';
  else
    Result := 'not TTypeSide';
  end;
end;

function GetCrossTypeSide(const ASide: TQBTypeSide): TQBTypeSide;
begin
  case ASide of
    tsBuy: Result := TQBTypeSide.tsSell;
    tsSell: Result := TQBTypeSide.tsBuy;
  else
    raise Exception.Create('Error Message: GetCrossTypeSide');
  end;
end;
{$ENDIF}

function _RoundTo(const AValue: Double): Double;
const
  SZ = 1000000;
begin
  Result := Round(AValue * SZ)/SZ;
end;


{ TParamTrade }

function TParamTrade.GetToStr: String;
var
  xS: String;
  xF: TFormatSettings;
begin
  xF := FormatSettings;
  xF.DecimalSeparator := '.';
  xS := '';
  xS := xS + DateToStr(Date,xF) + '; ';
  xS := xS + TimeToStr(Time,xF) + '; ';
  xS := xS + Symbol + '; ';
  xS := xS + GetStrToTypeSide(Side) + '; ';
  xS := xS + FloatToStr(Qty,xF) + '; ';
  xS := xS + FloatToStr(Price,xF) + '; ';
  xS := xS + OrderLinkId + '; ';
  Result := xS;
end;

function TParamTrade.GetValue: Double;
begin
  Result := Price * Qty;
end;

procedure TParamTrade.SetDefault;
begin
  Time := 0;
  Symbol := '';
  Side := TQBTypeSide.tsNull;
  Qty := 0;
  Price := 0;
  OrderLinkId := '';
  TypeTrade := TTypeTrade.ttOpen;
end;

{ TParamPosition }

constructor TParamPosition.Create(APositions: TParamPositions);
begin
  FPositions := APositions;
  FOpenTrades := TParamTradeList.Create;
  FHistoryTrades := TParamTradeList.Create;
end;

destructor TParamPosition.Destroy;
begin
  FreeAndNil(FHistoryTrades);
  FreeAndNil(FOpenTrades);
  inherited;
end;

procedure TParamPosition.DoOpenPosition;
begin
  // Событие открытие позции
  if Assigned(FPositions) then
    FPositions.DoOpenPosition;
end;

procedure TParamPosition.DoClosePosition(AParamTrade: TParamTrade);
begin
  // Событие закрытие позиции
  if Assigned(FPositions) then
    FPositions.DoClosePosition(AParamTrade);
end;

function TParamPosition.GetPrice: Double;
var
  xValue: Double;
  xQty: Double;
  i, iCount: Integer;
  xParamTrade: TParamTrade;
begin
  xValue := 0;
  iCount := FOpenTrades.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xParamTrade := FOpenTrades.Items[i];
      xQty := xQty + xParamTrade.Qty;
      xValue := xValue + xParamTrade.Value;
    end;
  if xQty > 0 then
    Result := xValue/xQty
  else
    Result := 0;
end;

function TParamPosition.GetProfit: Double;
var
  xValue: Double;
  i, iCount: Integer;
  xParamTrade: TParamTrade;
begin
  Result := 0;
  iCount := FHistoryTrades.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xParamTrade := FHistoryTrades.Items[i];
      case xParamTrade.Side of
        TQBTypeSide.tsBuy: xValue := xValue - xParamTrade.Value;
        TQBTypeSide.tsSell: xValue := xValue + xParamTrade.Value;
      end;
    end;
  Result := _RoundTo(xValue);
end;

function TParamPosition.GetProfitUpData(const AAsk, ABid: Double): Double;
var
  xValue: Double;
  i, iCount: Integer;
  xParamTrade: TParamTrade;
begin
  Result := 0;
  iCount := FHistoryTrades.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xParamTrade := FHistoryTrades.Items[i];
      case xParamTrade.Side of
        TQBTypeSide.tsBuy: xValue := xValue - xParamTrade.Value;
        TQBTypeSide.tsSell: xValue := xValue + xParamTrade.Value;
      end;
    end;

  case FSide of
    TQBTypeSide.tsBuy: xValue := xValue + ABid *  FQty;
    TQBTypeSide.tsSell: xValue := xValue - AAsk *  FQty;
  end;
  Result := _RoundTo(xValue);
end;

function TParamPosition.GetQty: Double;
var
  xQty: Double;
  i, iCount: Integer;
  xParamTrade: TParamTrade;
begin
  xQty := 0;
  iCount := FOpenTrades.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xParamTrade := FOpenTrades.Items[i];
      xQty := xQty + xParamTrade.Qty;
    end;
  Result := xQty;
end;

procedure TParamPosition.AddTrade(AParamTrade: TParamTrade);

  procedure _HistoryTrade(AQty: Double);
  var
    iCount: Integer;
    xTrade: TParamTrade;
  begin
    iCount := FHistoryTrades.Count;
    if iCount > 0 then
    begin
      xTrade := FHistoryTrades[iCount - 1];
      xTrade.Qty := AQty;
      FHistoryTrades[iCount - 1] := xTrade;
    end;
  end;

  procedure _Calc(AParamTrade: TParamTrade);
  var
    xQty: Double;
    i, iCount: Integer;
    xParamTrade, xTrade: TParamTrade;
  begin
    xTrade := AParamTrade;
    xQty := AParamTrade.Qty;
    while xQty > 0 do
    begin

      if FOpenTrades.Count = 0 then
      begin
        xTrade.Qty := xQty;
        DoClosePosition(xTrade);
        Break;
      end;

      xParamTrade := FOpenTrades[0];
      if xQty >= xParamTrade.Qty then
      begin
        // Удалить первую запись
        xQty := _RoundTo(xQty - xParamTrade.Qty);
        if (FOpenTrades.Count = 1) and (xQty > 0) then
          _HistoryTrade(xParamTrade.Qty);
        FOpenTrades.Delete(0);
      end
      else
      begin
        // Изменить первую запись
        xQty := _RoundTo(xParamTrade.Qty - xQty);
        xParamTrade.Qty := xQty;
        FOpenTrades[0] := xParamTrade;
        Break;
      end;
    end;
    if xQty = 0 then
    begin
      FillChar(xTrade,SizeOf(xTrade),0);
      DoClosePosition(xTrade);
    end;
  end;

var
  xQty: Double;
  xParamTrade: TParamTrade;
begin
  FHistoryTrades.Add(AParamTrade);

  if FOpenTrades.Count = 0 then
  begin
    FSide := AParamTrade.Side;
    FOpenTrades.Add(AParamTrade);
    DoOpenPosition;
  end
  else
  begin
    if FSide = AParamTrade.Side then
      FOpenTrades.Add(AParamTrade)
    else
      _Calc(AParamTrade);
  end;
end;



{ TParamPositions }

constructor TParamPositions.Create;
begin
  FCurrentPosition := nil;
  FPositions := TParamPositionList.Create;
end;

destructor TParamPositions.Destroy;
begin
  FreeAndNil(FPositions);
  inherited;
end;

procedure TParamPositions.DoChangePosition;
begin
  if Assigned(FOnChangePosition) then
    FOnChangePosition(Self);
  Save('result_trade.csv');
end;

procedure TParamPositions.DoOpenPosition;
begin
  DoChangePosition;
end;

procedure TParamPositions.DoClosePosition(AParamTrade: TParamTrade);
begin
  if Assigned(FCurrentPosition) then
    FCurrentPosition := nil;
  DoChangePosition;
  if not(AParamTrade.Side = TQBTypeSide.tsNull) then
    AddTrade(AParamTrade);
end;

function TParamPositions.GetCount: Integer;
begin
  Result := FPositions.Count;
end;


procedure TParamPositions.Clear;
begin
  FPositions.Clear;
end;

procedure TParamPositions.AddTrade(AParamTrade: TParamTrade);
begin
  if not Assigned(FCurrentPosition) then
  begin
    FCurrentPosition := TParamPosition.Create(Self);
    FPositions.Add(FCurrentPosition);
  end;
  FCurrentPosition.AddTrade(AParamTrade);
end;


procedure TParamPositions.Save(const AFileName: String);
var
  xStr: TStringList;
begin
  xStr := TStringList.Create;
  try
    for var xPostionTrade in FPositions do
    begin
      xStr.Add('');
      for var xParamTrade in xPostionTrade.HistoryTrades do
        xStr.Add(xParamTrade.ToStr);
    end;
    xStr.SaveToFile(AFileName);
  finally
    FreeAndNil(xStr);
  end;
end;

end.
