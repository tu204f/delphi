unit Lb.Bot.V3;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils,
  Lb.Breakdown;

type
  TWorkBotDeviation = class;

  ///<summary>Ñòàòóñ ïğîáèòèÿ<summary>
  TWorkBotStatus = (
    wbsNull,  // Ñòàòóñ íå îïğåäåëåí
    wbsHigh,  // Ïğîáèëè âåğõíåé ãğàíèöó âîëàòèëüíîñòè
    wbsLow    // Ïğîáèëè íèæíèé ãğàíèöû âîëàòèëüíîñòè
  );

  IEventWorkBot = interface
    procedure EventNewCandel;
    procedure EventParamValue(const AWorkBot: TWorkBotDeviation);
    procedure EventÑrossingLevel(const AWorkBotStatus: TWorkBotStatus);
  end;
  TOnEventÑrossingLevel = procedure(Sender: TObject; const AWorkBotStatus: TWorkBotStatus) of object;

  ///<summary>Ğåàëèçàöè ñòğàòåãèè</summary>
  TWorkBotDeviation = class(TObject)
  public type
    TParam = record
      Candel: TCandel;
      Deviation: Double;
      Rate: Double;
    end;
  private
    FCountParamValue: Integer;
    FPriceHigh: Double;
    FPriceLow: Double;
    FParam: TWorkBotDeviation.TParam;
    FEventWorkBot: IEventWorkBot;
    FWorkBotStatus: TWorkBotStatus;
    function GetCandel: TCandel;
    function GetDeviation: Double;
  protected
    FOnÑrossingHigh: TNotifyEvent;
    FOnÑrossingLow: TNotifyEvent;
    FOnÑrossingLevel: TOnEventÑrossingLevel;
    procedure DoÑrossingHigh;
    procedure DoÑrossingLow;
    procedure DoÑrossingLevel(const AWorkBotStatus: TWorkBotStatus);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetNewCandel;
    procedure SetParamValue(const AParam: TWorkBotDeviation.TParam);
    property CountParamValue: Integer read FCountParamValue;
    property PriceHigh: Double read FPriceHigh;
    property PriceLow: Double read FPriceLow;

    property OnÑrossingHigh: TNotifyEvent write FOnÑrossingHigh;
    property OnÑrossingLow: TNotifyEvent write FOnÑrossingLow;
    property OnÑrossingLevel: TOnEventÑrossingLevel write FOnÑrossingLevel;
  public
    property Candel: TCandel read GetCandel;
    property Deviation: Double read GetDeviation;
    property EventWorkBot: IEventWorkBot write FEventWorkBot;
    property WorkBotStatus: TWorkBotStatus read FWorkBotStatus;
  end;

implementation

uses
  Lb.Logger,
  System.DateUtils;

{ TWorkBotDeviation }

constructor TWorkBotDeviation.Create;
begin
  FCountParamValue := 0;
  FEventWorkBot := nil;
  FWorkBotStatus := TWorkBotStatus.wbsNull;
end;

destructor TWorkBotDeviation.Destroy;
begin

  inherited;
end;

procedure TWorkBotDeviation.SetNewCandel;
begin
  FCountParamValue := 0;
  if Assigned(FEventWorkBot) then
    FEventWorkBot.EventNewCandel;
end;

procedure TWorkBotDeviation.SetParamValue(const AParam: TWorkBotDeviation.TParam);
begin
  Inc(FCountParamValue);
  FParam := AParam;
  with AParam do
  begin
    FPriceHigh := Candel.Open + Deviation * Rate;
    FPriceLow  := Candel.Open - Deviation * Rate;
    if FPriceHigh < Candel.High then
      DoÑrossingHigh
    else if FPriceLow > Candel.Low then
      DoÑrossingLow;
  end;
  if Assigned(FEventWorkBot) then
    FEventWorkBot.EventParamValue(Self);
end;

procedure TWorkBotDeviation.DoÑrossingLevel(const AWorkBotStatus: TWorkBotStatus);
begin
  {$IFDEF DEBUG}
  TLogger.LogTree(0,'TWorkBotDeviation.DoÑrossingLevel: Ïåğåñå÷åíèå îäíîãî èç óğîâíåé');
  {$ENDIF}
  if Assigned(FOnÑrossingLevel) then
    FOnÑrossingLevel(Self,AWorkBotStatus);
end;

procedure TWorkBotDeviation.DoÑrossingHigh;
begin
  if Assigned(FOnÑrossingHigh) then
    FOnÑrossingHigh(Self);

  if (FWorkBotStatus in [TWorkBotStatus.wbsNull,TWorkBotStatus.wbsLow]) then
  begin
    FWorkBotStatus := TWorkBotStatus.wbsHigh;

    if Assigned(FEventWorkBot) then
      FEventWorkBot.EventÑrossingLevel(FWorkBotStatus);

    DoÑrossingLevel(FWorkBotStatus);
  end;
end;

procedure TWorkBotDeviation.DoÑrossingLow;
begin
  if Assigned(FOnÑrossingLow) then
    FOnÑrossingLow(Self);

  if (FWorkBotStatus in [TWorkBotStatus.wbsNull,TWorkBotStatus.wbsHigh]) then
  begin
    FWorkBotStatus := TWorkBotStatus.wbsLow;

    if Assigned(FEventWorkBot) then
      FEventWorkBot.EventÑrossingLevel(FWorkBotStatus);

    DoÑrossingLevel(FWorkBotStatus);
  end;
end;

function TWorkBotDeviation.GetCandel: TCandel;
begin
  Result := FParam.Candel;
end;

function TWorkBotDeviation.GetDeviation: Double;
begin
  Result := FParam.Deviation;
end;

end.
