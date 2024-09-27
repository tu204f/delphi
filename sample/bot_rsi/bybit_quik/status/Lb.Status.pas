unit Lb.Status;

interface

{$i platform.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  Lb.VirtualTrade.V2,
  Lb.SysUtils;

type
  TEventInfoMsg = procedure(Sender: TObject; AMsg: String) of object;

  ///<summary>
  /// Совершать торговые операции
  ///</summary>
  TParamStatus = record
    Side: TQBTypeSide; // Направление
    Price: Double;     // Цена
    Qty: Double;       // Количество
    Line: TTypeLine;   // Какой уровень
  public
    constructor Create(ASide: TQBTypeSide; APrice: Double; AQty: Double; ALine: TTypeLine);
    function ToStr: String;
  end;

  ///<summary>
  /// Параметры позиции
  ///</summary>
  TParamPosition = class(TObject)
    Qty: Double;       // Количество
    Side: TQBTypeSide; // Напровление
    Price: Double;     // Средняя цена - позиции
  private
    FProfit: Double;
    FMaxProfit: Double;
    FMinProfit: Double;
    procedure SetProfit(const Value: Double);
  public
    constructor Create; virtual;
    procedure DefualtValue;
    property Profit: Double read FProfit write SetProfit;
  end;

  ///<summary>
  /// Состояние рынка, для принятие решение
  ///</summary>
  TCustomStatus = class(TObject)
  private
    FFastRSI: Double;
    FSlowRSI: Double;
    FBid: Double;
    FAsk: Double;
    FMinStep: Double;
    FIsNewCandel: Boolean;
  private
    FPosition: TParamPosition;
  private
    FTimer: TTimer;
    FOnParams: TNotifyEvent;
    FOnUpDate: TNotifyEvent;
    FOnInfoMsg: TEventInfoMsg;
    FOnNewCandel: TNotifyEvent;
    function GetIsActive: Boolean;
    procedure TimerTimer(Sender: TObject);
  protected
    FTypePlatform: TTypePlatform;
    procedure DoParams; virtual;
    procedure DoInfoMsg(const S: String); virtual;
    procedure DoStart; virtual;
    procedure DoStop; virtual;
    procedure DoUpDate; virtual;
    ///<summary>Запросить данные</summary>
    procedure DoSelected; virtual;
    procedure DoNewCandel; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Start; virtual;
    procedure Stop; virtual;
    ///<summary>Отправить заявку: TParamStatus(Side,Price,Qty. Доп.парам. Line)</summary>
    function GetOperationTrade(AParamStatus: TParamStatus): String; virtual;
    ///<summary>Активный таймер — для запроса состояние рынка</summary>
    property IsActive: Boolean read GetIsActive;
    property OnParams: TNotifyEvent write FOnParams;
    property OnUpDate: TNotifyEvent write FOnUpDate;
    property OnInfoMsg: TEventInfoMsg write FOnInfoMsg;
    property OnNewCandel: TNotifyEvent write FOnNewCandel;
    ///<summary>Позиция по бумагам</summary>
    property Position: TParamPosition read FPosition;
  public
    property TypePlatform: TTypePlatform read FTypePlatform;
    property FastRSI: Double read FFastRSI write FFastRSI;
    property SlowRSI: Double read FSlowRSI write FSlowRSI;
    property Bid: Double read FBid write FBid;
    property Ask: Double read FAsk write FAsk;
    property MinStep: Double read FMinStep write FMinStep;
    property IsNewCandel: Boolean read FIsNewCandel write FIsNewCandel;
  end;

///<summary>
/// Генерирование уникальный номер заявки
///</summary>
function CreateOrderLinkId(ASide: TQBTypeSide; ALine: TTypeLine): String;

function GetParamPositions: TParamPositions;

implementation

uses
  Lb.Logger;

function CreateOrderLinkId(ASide: TQBTypeSide; ALine: TTypeLine): String;
var
  xS: String;
begin
  xS :=
    GetStrToTypeLine(ALine) + '_' +
    GetStrToTypeSide(ASide) + '_' +
    Random(65000).ToString;
  Result := xS;
end;

var
  localParamPositions: TParamPositions = nil;

function Vir_SelectedOrder(
  ASymbol: String;     // Торговый символ
  ASide: TQBTypeSide;  // Напровление торгового оперций
  AQty: Double;        // Количество
  APrice: Double;      // Цена
  AOrderLinkId: String // Напровление объекта
): String;
var
  xParamTrade: TParamTrade;
begin
  if not Assigned(localParamPositions) then
  begin
    Result := 'Error.Ok';
    Exit;
  end;

  try
    xParamTrade.Date := Date;
    xParamTrade.Time := Time;
    xParamTrade.Symbol := ASymbol;
    xParamTrade.Side := ASide;
    xParamTrade.Qty := AQty;
    xParamTrade.Price := APrice;
    xParamTrade.OrderLinkId := AOrderLinkId;
    localParamPositions.AddTrade(xParamTrade);
    Result := 'OK.';
  except
    Result := 'Error.';
  end;
end;

function GetParamPositions: TParamPositions;
begin
  if not Assigned(localParamPositions) then
    localParamPositions := TParamPositions.Create;
  Result := localParamPositions;
end;

{ TParamStatus }

constructor TParamStatus.Create(ASide: TQBTypeSide; APrice, AQty: Double;
  ALine: TTypeLine);
begin
  Side  := ASide;
  Price := APrice;
  Qty   := AQty;
  Line  := ALine;
end;

function TParamStatus.ToStr: String;
var
  xS: String;
begin
  xS :=
    'Side:' + GetStrToTypeSide(Side) +
    ' Price:' + Price.ToString +
    ' Qty:' + Qty.ToString +
    ' Line:' + GetStrToTypeLine(Line);
  Result := xS;
end;

{ TParamPosition }

constructor TParamPosition.Create;
begin

end;

procedure TParamPosition.DefualtValue;
begin
  Qty   := 0;
  Side  := TQBTypeSide.tsNull;
  Price := 0;
  FProfit := 0;
  FMaxProfit := 0;
  FMinProfit := 9999999;
end;

procedure TParamPosition.SetProfit(const Value: Double);
begin
  FProfit := Value;
  if FProfit > FMaxProfit then
    FMaxProfit := FProfit;
  if FProfit < FMinProfit then
    FMinProfit := FProfit;
end;

{ TCustomStatus }

constructor TCustomStatus.Create;
begin
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer  := TimerTimer;
  FTimer.Enabled  := False;
  FTimer.Interval := 1000;

  FPosition := TParamPosition.Create;
end;

destructor TCustomStatus.Destroy;
begin
  FreeAndNil(FPosition);
  FreeAndNil(FTimer);
  inherited;
end;

procedure TCustomStatus.TimerTimer(Sender: TObject);
begin
  try
    DoSelected;
  except
    on E:Exception do
    begin
      DoInfoMsg(E.Message);
      Stop;
    end;
  end;
end;

procedure TCustomStatus.Start;
begin
  if not IsActive then
  begin
    FTimer.Enabled := True;
    DoStart;
  end;
end;

procedure TCustomStatus.Stop;
begin
  if IsActive then
  begin
    DoStop;
    FTimer.Enabled := False;
  end;
end;

function TCustomStatus.GetIsActive: Boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TCustomStatus.DoInfoMsg(const S: String);
begin
  if Assigned(FOnInfoMsg) then
  begin
    if ParamApplication.IsLogTrade then
      TLogger.Log(S);
    FOnInfoMsg(Self,S);
  end;
end;

procedure TCustomStatus.DoNewCandel;
begin
  FIsNewCandel := True;
  if Assigned(FOnNewCandel) then
    FOnNewCandel(Self);
end;

procedure TCustomStatus.DoParams;
begin
  if Assigned(FOnParams) then
    FOnParams(Self);
end;

procedure TCustomStatus.DoSelected;
begin

end;

procedure TCustomStatus.DoStart;
begin

end;

procedure TCustomStatus.DoStop;
begin

end;

procedure TCustomStatus.DoUpDate;
begin
  if Assigned(FOnUpDate) then
    FOnUpDate(Self);
end;

function TCustomStatus.GetOperationTrade(AParamStatus: TParamStatus): String;

  function _Symble: String;
  begin
    case ParamApplication.TypePlatform of
      TTypePlatform.tpBybit: Result := ParamApplication.Symble;
      TTypePlatform.tpQuik: Result := ParamApplication.SecCode;
    else
      Result := 'NonPlatfotm'
    end;
  end;

var
  xOrderLinkId: String;
begin
{$IFDEF DBG_LEVEL_ORDER_TRADE}
  TLogger.LogTree(0,'TCustomStatus.GetOperationTrade:');
  TLogger.LogTreeText(3,AParamStatus.ToStr);
  TLogger.LogTreeText(3,'>> OPERATION_TRADE');
{$ENDIF}
  xOrderLinkId := CreateOrderLinkId(AParamStatus.Side,AParamStatus.Line);
  Result := xOrderLinkId;

  if ParamApplication.IsLogTrade then
  begin
    Vir_SelectedOrder(
      _Symble,             // Торговый символ
      AParamStatus.Side,   // Напровление торгового оперций
      AParamStatus.Qty,    // Количество
      AParamStatus.Price,  // Цена
      xOrderLinkId         // Напровление объекта
    );
  end;

  if ParamApplication.IsVirtualChecked then
  begin
    if Assigned(localParamPositions.CurrentPosition) then
    begin
      Position.Side := localParamPositions.CurrentPosition.Side;
      Position.Qty := localParamPositions.CurrentPosition.Qty;
      Position.Profit := localParamPositions.CurrentPosition.GetProfitUpData(FAsk,FBid);
    end
    else
    begin
      Position.Side := TQBTypeSide.tsNull;
      Position.Qty := 0;
      Position.Profit := 0;
    end;
  end;

end;




end.
