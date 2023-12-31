unit Lb.Bot.Tiket;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.SysUtils;

type
  TTiketBot = class(TObject)
  private
    FOpenPrice: Double;
    FIsPosition: Boolean;
    FMode: TMode;
    FHealthPoints: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure SetMode(const ACandel: TCandel); virtual;
    procedure SetOpenPosition(const ALast: Double); virtual;
    procedure SetUpPosition(const ALast: Double); virtual;
    procedure SetClosePosition(const ALast: Double); virtual;
    property IsPosition: Boolean read FIsPosition;
    property OpenPrice: Double read FOpenPrice;
    property Mode: TMode read FMode;
    property HealthPoints: Double read FHealthPoints;
  end;

  ///<summary>С фиксированным убытком</summary>
  TStopLostTiketBot = class(TTiketBot)
  private
    FStopLoss: Double;
    FCountStop: Integer;
    FTmpCountStop: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetMode(const ACandel: TCandel); override;
    procedure SetOpenPosition(const ALast: Double); override;
    procedure SetUpPosition(const ALast: Double); override;
    property StopLoss: Double read FStopLoss write FStopLoss;
    property CountStop: Integer read FCountStop write FCountStop;
  end;

  ///<summary>Изменяемым значением стопа</summary>
  TTrelingStopTiketBot = class(TStopLostTiketBot)
  private
    FFixProfit: Double;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetOpenPosition(const ALast: Double); override;
    procedure SetUpPosition(const ALast: Double); override;
  end;

  TTakeProfitTiketBot = class(TStopLostTiketBot)
  private
    FTakeProfit: Double;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetUpPosition(const ALast: Double); override;
    property TakeProfit: Double read FTakeProfit write FTakeProfit;
  end;

implementation

{ TTiketBot }

constructor TTiketBot.Create;
begin
  Clear;
end;

destructor TTiketBot.Destroy;
begin

  inherited;
end;

procedure TTiketBot.Clear;
begin
  FIsPosition := False;
  FMode := tmNull;
  FHealthPoints := 0;
end;

procedure TTiketBot.SetMode(const ACandel: TCandel);
begin
  {todo: организовать стретегию входа}
  FMode := GetRandomMode;
end;

procedure TTiketBot.SetOpenPosition(const ALast: Double);
begin
  // открываем позицию
  if not FIsPosition then
  begin
    FOpenPrice := ALast;
    FIsPosition := True;
  end;
end;

procedure TTiketBot.SetUpPosition(const ALast: Double);
begin
  // Обновляем условия позиции

end;

procedure TTiketBot.SetClosePosition(const ALast: Double);
var
  xHP: Double;
begin
  // Принудительное закрытие
  xHP := 0;
  if FIsPosition then
  begin
    case FMode of
      tmBuy: xHP := ALast - FOpenPrice;
      tmSell: xHP := FOpenPrice - ALast;
    end;
    FHealthPoints := FHealthPoints + xHP;
    FIsPosition := False;
  end;
end;


{ TStopLostTiketBot }

constructor TStopLostTiketBot.Create;
begin
  inherited;
  FStopLoss := -20;
  FTmpCountStop := 0;
end;

destructor TStopLostTiketBot.Destroy;
begin

  inherited;
end;

procedure TStopLostTiketBot.SetMode(const ACandel: TCandel);
begin
  FMode := tmNull;
  if FTmpCountStop > 0 then
    Dec(FTmpCountStop)
  else
    inherited SetMode(ACandel);
end;

procedure TStopLostTiketBot.SetOpenPosition(const ALast: Double);
begin
  inherited SetOpenPosition(ALast);
  if FIsPosition then
    FTmpCountStop := 0;
end;

procedure TStopLostTiketBot.SetUpPosition(const ALast: Double);
var
  xProfit: Double;
begin
  xProfit := 0;
  if FIsPosition then
  begin
    case FMode of
      tmBuy : xProfit := ALast - FOpenPrice;
      tmSell: xProfit := FOpenPrice - ALast;
    end;
    if xProfit < FStopLoss then
    begin
      FTmpCountStop := FCountStop;
      SetClosePosition(ALast)
    end;
  end;
end;

{ TTrelingStopTiketBot }

constructor TTrelingStopTiketBot.Create;
begin
  inherited;
end;

destructor TTrelingStopTiketBot.Destroy;
begin
  inherited;
end;

procedure TTrelingStopTiketBot.SetOpenPosition(const ALast: Double);
begin
  inherited SetOpenPosition(ALast);
  if FIsPosition then
    FFixProfit := FStopLoss;
end;

procedure TTrelingStopTiketBot.SetUpPosition(const ALast: Double);
var
  xProfit: Double;
  xFixProfit: Double;
begin
  xProfit := 0;
  if FIsPosition then
  begin
    case FMode of
      tmBuy : xProfit := ALast - FOpenPrice;
      tmSell: xProfit := FOpenPrice - ALast;
    end;

    xFixProfit := xProfit + FStopLoss;
    if xFixProfit > FFixProfit then
      FFixProfit := xFixProfit;

    if xProfit < FFixProfit then
    begin
      if FFixProfit < 0 then
        FTmpCountStop := FCountStop;
      SetClosePosition(ALast);
    end;

    if xProfit < FStopLoss then
    begin
      FTmpCountStop := FCountStop;
      SetClosePosition(ALast)
    end;

  end;
end;

{ TTakeProfitTiketBot }

constructor TTakeProfitTiketBot.Create;
begin
  inherited;

end;

destructor TTakeProfitTiketBot.Destroy;
begin

  inherited;
end;

procedure TTakeProfitTiketBot.SetUpPosition(const ALast: Double);
var
  xProfit: Double;
begin
  xProfit := 0;
  if FIsPosition then
  begin
    case FMode of
      tmBuy : xProfit := ALast - FOpenPrice;
      tmSell: xProfit := FOpenPrice - ALast;
    end;
    if xProfit > FTakeProfit then
    begin
      SetClosePosition(ALast)
    end else if xProfit < FStopLoss then
    begin
      FTmpCountStop := FCountStop;
      SetClosePosition(ALast)
    end;
  end;
end;

end.
