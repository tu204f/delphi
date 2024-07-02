unit Lb.HistoryIndicator;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline,
  Lb.Bybit.OrderBook,
  Lb.Indicator;

type
  ///<summary>
  /// Тип объекта
  ///</summary>
  TTypeObject = (
    tobNullObject,
    tobHistoryIndicator,
    tobInstrumentPrice
  );

  ///<summary>
  /// Ответ сервера
  ///</summary>
  TEventResponse = procedure(ASander: TObject; ATypeObject: TTypeObject) of object;

  ///<summary>
  /// Базовый объект стратегии
  ///</summary>
  TBasicObjectStrategy = class(TObject)
  private
    FTypeObject: TTypeObject;
    FOnResponse: TEventResponse;
  protected
    procedure DoEventResponse;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function UpDate: Boolean; virtual;
    property OnResponse: TEventResponse write FOnResponse;
  end;

  ///<summary>
  /// Исторические данные, с индикатором RSI
  ///</summary>
  THistoryIndicator = class(TBasicObjectStrategy)
  private
    FValueRSI: TRSI;
    FCurrentCandel: TCandelObject;
    FBybitKline: TBybitKline;
    procedure BybitKlineOnEventEndLoading(Sender: TObject);
  private
    FSymbol: String;
    FCategory: TTypeCategory;
    FInterval: TTypeInterval;
    FLimit: Integer;
    function GetCandels: TCandelObjectList;
  public
    constructor Create; override;
    destructor Destroy; override;
    ///<summary>Обновление данных</summary>
    function UpDate: Boolean; override;
  public
    property RSI: TRSI read FValueRSI;
    property Candels: TCandelObjectList read GetCandels;
    property CurrentCandel: TCandelObject read FCurrentCandel write FCurrentCandel;
  public
    property Symbol: String read FSymbol write FSymbol;
    property Category: TTypeCategory read FCategory write FCategory;
    property Interval: TTypeInterval read FInterval write FInterval;
  public
    property BybitKline: TBybitKline read FBybitKline;
  end;

  ///<summary>
  /// Цены инструмента, получаются из котировального стакана
  ///</summary>
  ///<remarks>
  /// Получать значение работы, на основание OrderBook
  ///</remarks>
  TInstrumentPrice = class(TBasicObjectStrategy)
  private
    FSymbol: String;
    FCategory: TTypeCategory;
    FLimit: Integer;
    FBybitOrderBook: TBybitOrderBook;
    procedure OrderBookOnEventEndLoading(Sender: TObject);
  private
    FSpred: Double;
    FBid: Double;
    FBidQuantity: Double;
    FAsk: Double;
    FAskQuantity: Double;
  public
    constructor Create; override;
    destructor Destroy; override;
    ///<summary>Обновление данных</summary>
    function UpDate: Boolean; override;
  public
    property Symbol: String read FSymbol write FSymbol;
    property Category: TTypeCategory read FCategory write FCategory;
    property Limit: Integer read FLimit write FLimit;
  public
    property Spred: Double read FSpred;
    property Bid: Double read FBid;
    property BidQuantity: Double read FBidQuantity;
    property Ask: Double read FAsk;
    property AskQuantity: Double read FAskQuantity;
  end;

  ///<summary>В ведем историю изменение, сохраняем знамение свечей</summary>
  TLoadHistory = class(TObject)
  private
    FSymbol: String;
    FFileName: String;
    FValues: TStrings;
    FStartTime: String;
    procedure SetSymbol(const Value: String);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>Наименование</summary>
    property Symbol: String read FSymbol write SetSymbol;
    procedure SetCurrentCandel(const ACandel: TCandelObject);
  end;

implementation

{ TBasicObjectStrategy }

constructor TBasicObjectStrategy.Create;
begin
  FTypeObject := tobNullObject;
end;

destructor TBasicObjectStrategy.Destroy;
begin
  inherited;
end;

function TBasicObjectStrategy.UpDate: Boolean;
begin
  Result := True;
  DoEventResponse;
end;

procedure TBasicObjectStrategy.DoEventResponse;
begin
  if Assigned(FOnResponse) then
    FOnResponse(Self,FTypeObject);
end;


{ THistoryIndicator }

constructor THistoryIndicator.Create;

  function _GetRSI(const APeriod, AvgPeriod: Integer): TRSI;
  var
    xRSI: TRSI;
  begin
    xRSI := TRSI.Create;
    xRSI.Period := APeriod;
    xRSI.AvgPeriod := AvgPeriod;
    Result := xRSI;
  end;

begin
  inherited Create;

  FTypeObject := tobHistoryIndicator;

  FSymbol := '';
  FCategory := TTypeCategory.tcLinear;
  FInterval := TTypeInterval.ti_5;

  FCurrentCandel := nil;

  FLimit := 150;
  FValueRSI := _GetRSI(7,3);

  FBybitKline := TBybitKline.Create;
  FBybitKline.OnEventEndLoading := BybitKlineOnEventEndLoading;
end;

destructor THistoryIndicator.Destroy;
begin
  FreeAndNil(FBybitKline);
  FreeAndNil(FValueRSI);
  inherited Destroy;
end;

function THistoryIndicator.GetCandels: TCandelObjectList;
begin
  Result := FBybitKline.CandelObjects;
end;

procedure THistoryIndicator.BybitKlineOnEventEndLoading(Sender: TObject);
var
  iCount: Integer;
begin
  iCount := FBybitKline.CandelObjects.Count;
  if iCount > 0 then
  begin
    FValueRSI.SetCandels(FBybitKline.CandelObjects);
    FCurrentCandel := FBybitKline.CandelObjects[0];
  end
  else
    FCurrentCandel := nil;
end;

function THistoryIndicator.UpDate: Boolean;
begin
  Result := inherited UpDate;
  try
    FBybitKline.Category := FCategory;
    FBybitKline.Symbol   := FSymbol;
    FBybitKline.Interval := FInterval;
    FBybitKline.Limit    := FLimit;
    FBybitKline.Selected;
  except
    Result := False;
  end;
end;

{ TInstrumentPrice }

constructor TInstrumentPrice.Create;
begin
  inherited;
  FTypeObject := tobInstrumentPrice;

  FBybitOrderBook := TBybitOrderBook.Create;
  FBybitOrderBook.OnEventEndLoading := OrderBookOnEventEndLoading;

  FSpred := 0;
  FBid := 0;
  FBidQuantity := 0;
  FAsk := 0;
  FAskQuantity := 0;
end;

destructor TInstrumentPrice.Destroy;
begin
  FreeAndNil(FBybitOrderBook);
  inherited;
end;

function TInstrumentPrice.UpDate: Boolean;
begin
  Result := inherited UpDate;
  try
    FBybitOrderBook.Category := FCategory;
    FBybitOrderBook.Symbol := FSymbol;
    FBybitOrderBook.Limit := FLimit;
    FBybitOrderBook.Selected;
  except
    Result := False;
  end;
end;

procedure TInstrumentPrice.OrderBookOnEventEndLoading(Sender: TObject);
begin
  FSpred       := FBybitOrderBook.OrderBook.Spred;
  FBid         := FBybitOrderBook.OrderBook.Bid;
  FBidQuantity := FBybitOrderBook.OrderBook.BidQuantity;
  FAsk         := FBybitOrderBook.OrderBook.Ask;
  FAskQuantity := FBybitOrderBook.OrderBook.AskQuantity;
end;

{ TLoadHistory }

constructor TLoadHistory.Create;
begin
  FValues := TStringList.Create;
  FStartTime := '';
end;

destructor TLoadHistory.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

procedure TLoadHistory.SetSymbol(const Value: String);
begin
  FSymbol := Value;
  FFileName := ExtractFilePath(ParamStr(0)) + 'data\';
  FFileName := FFileName + FSymbol + '.txt';
  FValues.LoadFromFile(FFileName);
end;

procedure TLoadHistory.SetCurrentCandel(const ACandel: TCandelObject);
var
  xS: String;
begin
  if not SameText(FStartTime,ACandel.startTime) then
  else
  begin
    xS := ACandel.ToString;
    FValues.Add(xS);
  end;
  FValues.SaveToFile(FFileName);
end;

end.
