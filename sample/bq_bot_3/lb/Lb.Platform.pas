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
  Lb.Indicator;

type
  TEventOnStart        = procedure(ASender: TObject) of object;
  TEventOnStop         = procedure(ASender: TObject) of object;
  TEventOnSelected     = procedure(ASender: TObject) of object;
  TEventOnMsgInfo      = procedure(ASender: TObject; AMsg: String) of object;
  TEventOnStateMarket  = procedure(ASender: TObject; AStateMarket: TStateMarket) of object;

  ///<summary>
  /// ������� ������ ��� ������ � ����������
  ///</summary>
  TCustomTradingPlatform = class(TObject)
  private
    FTimer: TTimer;
    function GetIsActive: Boolean;
    procedure TimerTimer(Sender: TObject);
  private
    FEventOnNewCandel: TNotifyEvent;
    FEventOnStart: TEventOnStart;
    FEventOnStop: TEventOnStop;
    FEventOnSelected: TEventOnSelected;
    FEventOnMsgInfo: TEventOnMsgInfo;
  protected
    procedure DoStart; virtual;
    procedure DoStop; virtual;
    procedure DoSelected; virtual;
    procedure DoMsgInfo(S: String); virtual;
    procedure DoNewCandel; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Start; virtual;
    procedure Stop; virtual;
    ///<summary>�������� ������ � ��� ������� ��������� �����</summary>
    property IsActive: Boolean read GetIsActive;
  public
    property OnStart: TEventOnStart write FEventOnStart;
    property OnStop: TEventOnStop write FEventOnStop;
    property OnSelected: TEventOnSelected write FEventOnSelected;
    property OnMsgInfo: TEventOnMsgInfo write FEventOnMsgInfo;
    property OnNewCandel: TNotifyEvent write FEventOnNewCandel;
  end;

  ///<summary>
  /// ���������
  ///</summary>
  TTradingPlatform = class(TCustomTradingPlatform)
  private
    FSymbol: String;
    FOnStateMarket: TEventOnStateMarket;
  private
    FValueVolatility: TValueVolatility;
  protected
    FStateMarket: TStateMarket;
    procedure DoStateMarke; virtual;
    procedure DoSelected; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    ///<summary>��������� ��������</summary>
    property Symbol: String read FSymbol write FSymbol;
    ///<summary>��������� �����</summary>
    property StateMarket: TStateMarket read FStateMarket;
    ///<summary>���������� ������� ��������� �����</summary>
    property OnStateMarket: TEventOnStateMarket write FOnStateMarket;
  public
    ///<summary>
    /// ���� ������������� ������ ��������� ������
    ///</summary>
    function SendTrade(const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell): String; virtual;
  public {�������� �������� }
    property ValueVolatility: TValueVolatility read FValueVolatility;
  end;

implementation

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

procedure TCustomTradingPlatform.DoNewCandel;
begin
  if Assigned(FEventOnNewCandel) then
    FEventOnNewCandel(Self);
end;

{ TTradingPlatform }

constructor TTradingPlatform.Create;
begin
  inherited Create;
  FStateMarket := TStateMarket.Create;
  FValueVolatility := TValueVolatility.Create;
end;

destructor TTradingPlatform.Destroy;
begin
  FreeAndNil(FValueVolatility);
  FreeAndNil(FStateMarket);
  inherited Destroy;
end;

procedure TTradingPlatform.DoSelected;
begin
  inherited;

end;

procedure TTradingPlatform.DoStateMarke;
begin
  FValueVolatility.SetCandels(FStateMarket.Candels);
  if Assigned(FOnStateMarket) then
    FOnStateMarket(Self,FStateMarket);
end;

function TTradingPlatform.SendTrade(const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell): String;
begin
  //
end;

end.
