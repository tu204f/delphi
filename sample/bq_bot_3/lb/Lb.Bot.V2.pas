unit Lb.Bot.V2;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils;

type
  ///<summary>Бот заработой</summary>
  TWorkBot = class(TObject)
  public const
    VALUE_WR = 50;
  private
    FOnLongOpen: TNotifyEvent;
    FOnShortOpen: TNotifyEvent;
    FLongTypeTrade, FShortTypeTrade: TTypeTrade;
  protected
    procedure DoLongOpen;
    procedure DoShortOpen;
    procedure SetLongTrade(AFastValueMa, ASlowValueMa: Double);
    procedure SetShortTrade(AFastValueMa, ASlowValueMa: Double);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SerDefault;
    procedure SetUpDataParam(const AValueWR, AFastValueMa, ASlowValueMa: Double);
    property OnLongOpen: TNotifyEvent write FOnLongOpen;
    property OnShortOpen: TNotifyEvent write FOnShortOpen;
  end;

  TWorkBotV2 = class(TObject)
  private
    FOnLongOpen: TNotifyEvent;
    FOnShortOpen: TNotifyEvent;
  protected
    procedure DoLongOpen;
    procedure DoShortOpen;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetUpDataParam(ACandel: TCandel; ADeviation: Double);
    property OnLongOpen: TNotifyEvent write FOnLongOpen;
    property OnShortOpen: TNotifyEvent write FOnShortOpen;
  end;



implementation

{ TWorkBot }

constructor TWorkBot.Create;
begin
  FLongTypeTrade := TTypeTrade.ttNull;
  FShortTypeTrade := TTypeTrade.ttNull;
end;

destructor TWorkBot.Destroy;
begin

  inherited;
end;

procedure TWorkBot.DoLongOpen;
begin
  if Assigned(FOnLongOpen) then
    FOnLongOpen(Self);
end;

procedure TWorkBot.DoShortOpen;
begin
  if Assigned(FOnShortOpen) then
    FOnShortOpen(Self);
end;

procedure TWorkBot.SerDefault;
begin
  FLongTypeTrade := TTypeTrade.ttNull;
  FShortTypeTrade := TTypeTrade.ttNull;
end;

procedure TWorkBot.SetLongTrade(AFastValueMa, ASlowValueMa: Double);
begin
  if AFastValueMa > ASlowValueMa then
  begin
    if (FLongTypeTrade in [TTypeTrade.ttNull,TTypeTrade.ttClose]) then
    begin
      FLongTypeTrade := TTypeTrade.ttOpen;
      DoLongOpen;
    end;
  end
  else
    FLongTypeTrade := TTypeTrade.ttClose;
end;

procedure TWorkBot.SetShortTrade(AFastValueMa, ASlowValueMa: Double);
begin
  if AFastValueMa < ASlowValueMa then
  begin
    if (FShortTypeTrade in [TTypeTrade.ttNull,TTypeTrade.ttClose]) then
    begin
      FShortTypeTrade := TTypeTrade.ttOpen;
      DoShortOpen;
    end;
  end
  else
    FShortTypeTrade := TTypeTrade.ttClose;
end;

procedure TWorkBot.SetUpDataParam(const AValueWR, AFastValueMa, ASlowValueMa: Double);

  function _UpValueWR(AValueWR, AFastValueMa, ASlowValueMa: Double): Boolean;
  begin
    Result :=
      (AValueWR < TWorkBot.VALUE_WR) and
      (AFastValueMa < TWorkBot.VALUE_WR) and
      (ASlowValueMa < TWorkBot.VALUE_WR);
  end;

  function _DownValueWR(AValueWR, AFastValueMa, ASlowValueMa: Double): Boolean;
  begin
    Result :=
      (AValueWR > TWorkBot.VALUE_WR) and
      (AFastValueMa > TWorkBot.VALUE_WR) and
      (ASlowValueMa > TWorkBot.VALUE_WR);
  end;

begin
  if _UpValueWR(AValueWR, AFastValueMa, ASlowValueMa) then
    SetLongTrade(AFastValueMa, ASlowValueMa)
  else if _DownValueWR(AValueWR, AFastValueMa, ASlowValueMa) then
    SetShortTrade(AFastValueMa, ASlowValueMa);
end;

{ TWorkBotV2 }

constructor TWorkBotV2.Create;
begin

end;

destructor TWorkBotV2.Destroy;
begin

  inherited;
end;

procedure TWorkBotV2.DoLongOpen;
begin
  if Assigned(FOnLongOpen) then
    FOnLongOpen(Self);
end;

procedure TWorkBotV2.DoShortOpen;
begin
  if Assigned(FOnShortOpen) then
    FOnShortOpen(Self);
end;

procedure TWorkBotV2.SetUpDataParam(ACandel: TCandel; ADeviation: Double);
var
  xDeltaBody: Double;
begin
  xDeltaBody := Abs(ACandel.Close - ACandel.Open);
  if xDeltaBody > ADeviation then
  begin
    if ACandel.Open < ACandel.Close then
      DoLongOpen
    else if ACandel.Open > ACandel.Close then
      DoShortOpen;
  end;
end;

end.
