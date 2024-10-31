unit Lb.Platform;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  Lb.SysUtils,
  Lb.Buffer.Trading;

type
  TEventOnStart        = procedure(ASender: TObject) of object;
  TEventOnStop         = procedure(ASender: TObject) of object;
  TEventOnSelected     = procedure(ASender: TObject) of object;
  TEventOnMsgInfo      = procedure(ASender: TObject; AMsg: String) of object;
  TEventOnStateMarket  = procedure(ASender: TObject; AStateMarket: TStateMarket) of object;

  ///<summary>
  /// Базовый объект для работы с платформой
  ///</summary>
  TCustomTradingPlatform = class(TObject)
  private
    FTimer: TTimer;
    function GetIsActive: Boolean;
    procedure TimerTimer(Sender: TObject);
  private
    FEventOnStart: TEventOnStart;
    FEventOnStop: TEventOnStop;
    FEventOnSelected: TEventOnSelected;
    FEventOnMsgInfo: TEventOnMsgInfo;
  protected
    procedure DoStart; virtual;
    procedure DoStop; virtual;
    procedure DoSelected; virtual;
    procedure DoMsgInfo(S: String); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Start; virtual;
    procedure Stop; virtual;
    ///<summary>Активный таймер — для запроса состояние рынка</summary>
    property IsActive: Boolean read GetIsActive;
  public
    property OnStart: TEventOnStart write FEventOnStart;
    property OnStop: TEventOnStop write FEventOnStop;
    property OnSelected: TEventOnSelected write FEventOnSelected;
    property OnMsgInfo: TEventOnMsgInfo write FEventOnMsgInfo;
  end;

  ///<summary>
  /// Платформа
  ///</summary>
  TTradingPlatform = class(TCustomTradingPlatform)
  private
    FSymbel: String;
    FOnStateMarket: TEventOnStateMarket;
  private
    FPeriod: Integer;
    FValueRSI: Double;
    FValueATR: Double;
  protected
    FStateMarket: TStateMarket;
    procedure DoStateMarke; virtual;
    procedure DoSelected; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    ///<summary>Платформа операций</summary>
    property Symbel: String read FSymbel write FSymbel;
    ///<summary>Состояние рынка</summary>
    property StateMarket: TStateMarket read FStateMarket;
    property OnStateMarket: TEventOnStateMarket write FOnStateMarket;
  public
    ///<summary>
    /// Есть потенциальная ошибка зависание заявки
    ///</summary>
    procedure SendTrade(const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell); virtual;
  public
    property Period: Integer read FPeriod write FPeriod;
    property ValueRSI: Double read FValueRSI;
    property ValueATR: Double read FValueATR;
  end;


implementation

uses
  Lb.Bot;

{ TCustomTradingPlatform }

constructor TCustomTradingPlatform.Create;
begin
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer  := TimerTimer;
  FTimer.Enabled  := False;
  FTimer.Interval := 1000;
end;

destructor TCustomTradingPlatform.Destroy;
begin
  FreeAndNil(FTimer);
  inherited;
end;

function TCustomTradingPlatform.GetIsActive: Boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TCustomTradingPlatform.Start;
begin
  if not IsActive then
  begin
    FTimer.Enabled := True;
    DoStart;
  end;
end;

procedure TCustomTradingPlatform.Stop;
begin
  if IsActive then
  begin
    DoStop;
    FTimer.Enabled := False;
  end;
end;

procedure TCustomTradingPlatform.TimerTimer(Sender: TObject);
begin
  try
    DoSelected;
  except
    on E: Exception do
    begin
      DoStop;
      DoMsgInfo(E.Message);
    end;
  end;
end;

procedure TCustomTradingPlatform.DoStart;
begin
  if Assigned(FEventOnStart) then
    FEventOnStart(Self);
end;

procedure TCustomTradingPlatform.DoStop;
begin
  if Assigned(FEventOnStop) then
    FEventOnStop(Self);
end;

procedure TCustomTradingPlatform.DoSelected;
begin
  if Assigned(FEventOnSelected) then
    FEventOnSelected(Self);
end;

procedure TCustomTradingPlatform.DoMsgInfo(S: String);
begin
  if Assigned(FEventOnMsgInfo) then
    FEventOnMsgInfo(Self,S);
end;

{ TTradingPlatform }

constructor TTradingPlatform.Create;
begin
  inherited Create;
  FPeriod := 14;
  FStateMarket := TStateMarket.Create;
end;

destructor TTradingPlatform.Destroy;
begin
  FreeAndNil(FStateMarket);
  inherited Destroy;
end;

procedure TTradingPlatform.DoSelected;
begin
  inherited;

  FValueATR := GetATR(FPeriod,StateMarket.Candels);
  FValueATR := Round(FValueATR * 100)/100;

  FValueRSI := GetRSI(FPeriod,StateMarket.Candels);
  FValueRSI := Round(FValueRSI * 100)/100;
end;

procedure TTradingPlatform.DoStateMarke;
begin
  if Assigned(FOnStateMarket) then
    FOnStateMarket(Self,FStateMarket);
end;

procedure TTradingPlatform.SendTrade(const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell);
begin
  //
end;

end.
