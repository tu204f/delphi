unit Lb.SysUtils;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections;

type
  ///<summary>Состояние сделки</summary>
  TTypeTrade = (
    ttNull,
    ttOpen,
    ttClose
  );

  ///<summary>Напровление сделки</summary>
  TTypeDirection = (
    tdNull,
    tdLong,
    tdShort
  );

  TTypeBuySell = (
    tsNull, {добавлено новое значение}
    tsBuy,
    tsSell
  );

  TDoubleList = TList<Double>;

type
  ///<summary>
  /// Свеча
  ///</summary>
  TCandel = record
    Time: Int64;    // Дата и время
    Open: Double;   // Цена открытие
    High: Double;   // Максимальная цена
    Low: Double;    // Минимальная цена
    Close: Double;  // Закрытие цены
    Vol: Double;    // Объем который прошол
  public
    function GetToStr: String;
  end;

  ///<summary>
  /// Массив свячей
  ///</summary>
  TCandelList = class(TList<TCandel>)
  public
    procedure CopyCandels(const ACandels: TCandelList);
  end;

  ///<summary>
  /// Состояние рынка
  ///</summary>
  TStateMarket = class(TObject)
  private
    FAsk: Double;
    FBid: Double;
    FQty: Double;
    FCandels: TCandelList;
    FFirstCandelTime: Int64;
    FIsNewCandel: Boolean;
    FOnNewCandel: TNotifyEvent;
    function GetLast: Double;
    function GetServerTime: TDateTime;
  protected
    procedure DoNewCandel;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetPrice(const AAsk, ABid: Double);
    ///<summary>
    /// Объем для определениые цена
    ///</summary>
    property Qty: Double read FQty write FQty;
    ///<summary>
    /// Цена продавца - который может взять Qty
    ///</summary>
    property Ask: Double read FAsk;
    ///<summary>
    /// Цена покупателя - который может взять Qty
    ///</summary>
    property Bid: Double read FBid;
    ///<summary>
    /// Последния цена
    ///</summary>
    property Last: Double read GetLast;
    ///<summary>
    /// Показать сервисный время, наданный момоент показывает время (свечи)
    ///</summary>
    property ServerTime: TDateTime read GetServerTime;
  public
    procedure SetUpDataCandels;
    property Candels: TCandelList read FCandels;
    property OnNewCandel: TNotifyEvent write FOnNewCandel;
  end;

  ///<summary>
  /// Позиция
  ///</summary>
  TPositionMarket = class(TObject)
    OpenTime: Int64;
    CloseTime: Int64;
    Side: TTypeBuySell;
    OpenPrice: Double;
    ClosePrice: Double;
    MovingPrice: Double;
    Qty: Double;
    Value: Double;
  end;

function GetCrossSide(ASide: TTypeBuySell): TTypeBuySell;
function GetStrToSide(ASide: TTypeBuySell): String;
function GetSiseToStr(AValue: String): TTypeBuySell;
function GetStrToTypeTrade(const ATypeTrade: TTypeTrade): String;
function GetTypeTradeToStr(const AValue: String): TTypeTrade;

///<summary>
/// Текущая дата и время
///</summary>
function GetNewDateTime: TDateTime;

function GetTimeToInt64(const AValue: TDateTime): Int64;
function GetInt64ToTime(const AValue: Int64): TDateTime;

function GetRound(const AValue: Double): Double; inline;

implementation

const
  ///<summary>
  /// Количество миллисекунд в секунде
  ///</summary>
  SEC_COUNT_MSEC  = 1000;
  ///<summary>
  /// Количество миллисекунд в минуте
  ///</summary>
  MIN_COUNT_MSEC  = 60 * SEC_COUNT_MSEC;
  ///<summary>
  /// Количество миллисекунд в одном часе
  ///</summary>
  HOUR_COUNT_MSEC = 60 * MIN_COUNT_MSEC;

function GetStrToTypeTrade(const ATypeTrade: TTypeTrade): String;
begin
  case ATypeTrade of
    ttNull: Result := 'null';
    ttOpen: Result := 'open';
    ttClose: Result := 'close';
  end;
end;

function GetTypeTradeToStr(const AValue: String): TTypeTrade;
var
  xC: Char;
begin
  if AValue.IsEmpty then
  begin
    Result := TTypeTrade.ttNull;
    Exit;
  end;
  xC := AValue[1];
  case xC of
    'o': Result := TTypeTrade.ttOpen;
    'c': Result := TTypeTrade.ttClose;
  else
    Result := TTypeTrade.ttNull;
  end;
end;

function GetStrToSide(ASide: TTypeBuySell): String;
begin
  case ASide of
    tsNull: Result := 'N';
    tsBuy: Result := 'B';
    tsSell: Result := 'S';
  end;
end;

function GetSiseToStr(AValue: String): TTypeBuySell;
var
  xC: Char;
begin
  xC := #0;
  if AValue.Length > 0 then
    xC := AValue[1];
  case xC of
    'B': Result := TTypeBuySell.tsBuy;
    'S': Result := TTypeBuySell.tsSell;
  else
    Result :=  TTypeBuySell.tsNull;
  end;
end;

function GetCrossSide(ASide: TTypeBuySell): TTypeBuySell;
begin
  case ASide of
    tsBuy: Result := TTypeBuySell.tsSell;
    tsSell: Result := TTypeBuySell.tsBuy;
  else
    Result := ASide;
  end;
end;

function GetNewDateTime: TDateTime;
begin
  Result := System.SysUtils.Date + System.SysUtils.Time;
end;

function GetTimeToInt64(const AValue: TDateTime): Int64;
var
  xHour, xMin, xSec, xMSec: Word;
begin
  DecodeTime(AValue, xHour, xMin, xSec, xMSec);
  Result :=
    xHour * HOUR_COUNT_MSEC +
    xMin  * MIN_COUNT_MSEC +
    xSec  * SEC_COUNT_MSEC +
    xMSec;
end;

function GetInt64ToTime(const AValue: Int64): TDateTime;

  function _Hour(AValue: Int64): Word;
  var
    xResult: Integer;
  begin
    xResult := Trunc(AValue / HOUR_COUNT_MSEC);
    if xResult > 24 then
      xResult := Trunc(24 * Frac(xResult/24));
    Result := xResult;
  end;

  function _Min(AValue: Int64): Word;
  begin
    Result := Trunc(AValue / MIN_COUNT_MSEC);
  end;

  function _Sec(AValue: Int64): Word;
  begin
    Result := Trunc(AValue / SEC_COUNT_MSEC);
  end;

var
  xValue: Int64;
  xHour, xMin, xSec, xMSec: Word;
begin
  {todo: Есть уже заложденый баг}
  xHour  := _Hour(AValue);
  xValue := AValue - xHour * HOUR_COUNT_MSEC;
  xMin   := _Min(xValue);
  xValue := xValue - xMin * MIN_COUNT_MSEC;
  xSec   := _Sec(xValue);
  xMSec  := xValue - xSec * SEC_COUNT_MSEC;

  Result := EncodeTime(xHour, xMin, xSec, xMSec);
end;

function GetRound(const AValue: Double): Double;
begin
  Result := Trunc(AValue * 1000)/1000;
end;

{ TCandel }

function TCandel.GetToStr: String;
begin
  Result :=
    Time.ToString + ';' +
    Open.ToString + ';' +
    High.ToString + ';' +
    Low.ToString + ';' +
    Close.ToString + ';' +
    Vol.ToString;
end;

{ TCandelList }

procedure TCandelList.CopyCandels(const ACandels: TCandelList);
begin
  Self.Clear;
  if Assigned(ACandels) then
  begin
    for var xC in ACandels do
      Self.Add(xC);
  end;
end;

{ TStateMarket }

constructor TStateMarket.Create;
begin
  FAsk := 0;
  FBid := 0;
  FCandels := TCandelList.Create;
  FFirstCandelTime := 0;
end;

destructor TStateMarket.Destroy;
begin
  FreeAndNil(FCandels);
  inherited;
end;

procedure TStateMarket.DoNewCandel;
begin
  if Assigned(FOnNewCandel) then
    FOnNewCandel(Self);
end;

function TStateMarket.GetLast: Double;
begin
  Result := 0;
  if FCandels.Count > 0 then
    Result := FCandels[0].Close;
end;

procedure TStateMarket.SetPrice(const AAsk, ABid: Double);
begin
  {todo: тех случает, такое возможно когда нет ликвидности на рынке}
  if AAsk <= 0 then
    raise Exception.Create('Error Message: Цена продавца не может быть нулевой');

  if ABid <= 0 then
    raise Exception.Create('Error Message: Цена покупателя не может быть нулевой');

  FAsk := AAsk;
  FBid := ABid;
end;

procedure TStateMarket.SetUpDataCandels;
var
  xFirstCandelTime: Int64;
begin
  FIsNewCandel := False;
  if FCandels.Count > 0 then
  begin
    xFirstCandelTime := FCandels[0].Time;
    if FFirstCandelTime = 0 then
    begin
      FFirstCandelTime := xFirstCandelTime;
    end
    else
    begin
      FIsNewCandel := FFirstCandelTime <> xFirstCandelTime;
      if FIsNewCandel then
      begin
        FFirstCandelTime := xFirstCandelTime;
        DoNewCandel;
      end;
    end;
  end;
end;

function TStateMarket.GetServerTime: TDateTime;
var
  xCandel: TCandel;
begin
  if FCandels.Count > 0 then
  begin
    xCandel := FCandels[0];
    Result := UnixToDateTime(xCandel.Time);
  end
  else
    Result := 0;
end;

end.
