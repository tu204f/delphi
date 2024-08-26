unit Lb.Status;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  Lb.VirtualTrade,
  Lb.SysUtils;

type

  TEventInfoMsg = procedure(Sender: TObject; AMsg: String) of object;

  TCustomStatus = class(TObject)
  private
    FFastRSI: Double;
    FSlowRSI: Double;
    FBid: Double;
    FAsk: Double;
    FQty: Double;
    FSide: TQBTypeSide;
    FMinStep: Double;
  private
    FTimer: TTimer;
    FOnParams: TNotifyEvent;
    FOnUpDate: TNotifyEvent;
    FOnInfoMsg: TEventInfoMsg;
    function GetIsActive: Boolean;
    procedure TimerTimer(Sender: TObject);
  protected
    procedure DoParams; virtual;
    procedure DoInfoMsg(const S: String); virtual;
    procedure DoStart; virtual;
    procedure DoStop; virtual;
    procedure DoUpDate; virtual;
    ///<summary>Запросить данные</summary>
    procedure DoSelected; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Start; virtual;
    procedure Stop; virtual;
    function GetOperationTrade(ASide: TQBTypeSide;
      APrice: Double; AQty: Double; ALine: TTypeLine): String; virtual;
    property IsActive: Boolean read GetIsActive;
    property OnParams: TNotifyEvent write FOnParams;
    property OnUpDate: TNotifyEvent write FOnUpDate;
    property OnInfoMsg: TEventInfoMsg write FOnInfoMsg;
  public
    property FastRSI: Double read FFastRSI write FFastRSI;
    property SlowRSI: Double read FSlowRSI write FSlowRSI;
    property Bid: Double read FBid write FBid;
    property Ask: Double read FAsk write FAsk;
    property Qty: Double read FQty write FQty;
    property Side: TQBTypeSide read FSide write FSide;
    property MinStep: Double read FMinStep write FMinStep;
  end;

///<summary>
/// Генерирование уникальный номер заявки
///</summary>
function CreateOrderLinkId(ASide: TQBTypeSide; ALine: TTypeLine): String;

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

{ TCustomStatus }

constructor TCustomStatus.Create;
begin
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer  := TimerTimer;
  FTimer.Enabled  := False;
  FTimer.Interval := 1000;
end;

destructor TCustomStatus.Destroy;
begin
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

function TCustomStatus.GetOperationTrade(ASide: TQBTypeSide; APrice,
  AQty: Double; ALine: TTypeLine): String;

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
  xOrderLinkId := CreateOrderLinkId(ASide,ALine);
  Result := xOrderLinkId;

  if ParamApplication.IsLogTrade then
  begin
    Virtual_SelectedOrder(
      _Symble,      // Торговый символ
      ASide,        // Напровление торгового оперций
      AQty,         // Количество
      APrice,       // Цена
      xOrderLinkId  // Напровление объекта
    );
  end;

  if ParamApplication.IsVirtualChecked then
  begin
    FSide := GetVirtualTrades.Side;
    FQty := GetVirtualTrades.Qty;
  end;

end;

end.
