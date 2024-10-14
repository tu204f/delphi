unit Lb.Platfom;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  Lb.SysUtils;

type
  TEventOnStart    = procedure(ASender: TObject) of object;
  TEventOnStop     = procedure(ASender: TObject) of object;
  TEventOnSelected = procedure(ASender: TObject) of object;
  TEventOnMsgInfo  = procedure(ASender: TObject; AMsg: String) of object;

  ///<summary>
  /// Базовый объект для работы с платформой
  ///</summary>
  TCustomPlatform = class(TObject)
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
  TPlatform = class(TCustomPlatform)
  private
    FSymbel: String;
    FAsk: Double;
    FBid: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<symmary>Цена продавца</summary>
    property Ask: Double read FAsk;
    ///<summary>Цена покупателя</summary>
    property Bid: Double read FBid;
    ///<summary>Платформа операций</summary>
    property Symbel: String read FSymbel write FSymbel;
  end;

implementation

{ TCustomPlatform }

constructor TCustomPlatform.Create;
begin
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer  := TimerTimer;
  FTimer.Enabled  := False;
  FTimer.Interval := 1000;
end;

destructor TCustomPlatform.Destroy;
begin
  FreeAndNil(FTimer);
  inherited;
end;

function TCustomPlatform.GetIsActive: Boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TCustomPlatform.Start;
begin
  if not IsActive then
  begin
    FTimer.Enabled := True;
    DoStart;
  end;
end;

procedure TCustomPlatform.Stop;
begin
  if IsActive then
  begin
    DoStop;
    FTimer.Enabled := False;
  end;
end;

procedure TCustomPlatform.TimerTimer(Sender: TObject);
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

procedure TCustomPlatform.DoStart;
begin
  if Assigned(FEventOnStart) then
    FEventOnStart(Self);
end;

procedure TCustomPlatform.DoStop;
begin
  if Assigned(FEventOnStop) then
    FEventOnStop(Self);
end;

procedure TCustomPlatform.DoSelected;
begin
  if Assigned(FEventOnSelected) then
    FEventOnSelected(Self);
end;

procedure TCustomPlatform.DoMsgInfo(S: String);
begin
  if Assigned(FEventOnMsgInfo) then
    FEventOnMsgInfo(Self,S);
end;

{ TPlatform }

constructor TPlatform.Create;
begin

end;

destructor TPlatform.Destroy;
begin

  inherited;
end;

end.
