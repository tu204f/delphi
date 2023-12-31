unit Lb.Bot;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.SysUtils;

type
  TBot = class(TObject)
  private
    FHealthPoints: Double;
  protected
    FMode: TMode;
    function GetMode: TMode; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure SetCurrentCandel(const ACandel: TCandel); virtual;
    property Mode: TMode read FMode;
    property HealthPoints: Double read FHealthPoints write FHealthPoints;
  end;

  ///<summary>Ѕот где ограничиваем убыток</summary>
  TStopLossBot = class(TBot)
  private
    FStopLoss: Double;
    FCountStop: Integer;
  protected
    FTmpCountStop: Integer;
    function GetMode: TMode; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetCurrentCandel(const ACandel: TCandel); override;
    ///<summary> оличество свечей которой нужно пропустить</summary>
    property CountStop: Integer read FCountStop write FCountStop;
    ///<summary>‘иксируем размер убытка</summary>
    property StopLoss: Double read FStopLoss write FStopLoss;
  end;

  ///<summary>Ѕот с фиксирование пробили, профитна€ сделка проходит первой</summary>
  TTakeProfit = class(TStopLossBot)
  private
    FTakeProfit: Double;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetCurrentCandel(const ACandel: TCandel); override;
    property TakeProfit: Double read FTakeProfit write FTakeProfit;
  end;

implementation

{ TBot }

procedure TBot.Clear;
begin
  FHealthPoints := 0;
  FMode := tmNull;
end;

constructor TBot.Create;
begin
  FHealthPoints := 0;
  FMode := TMode.tmNull;
end;

destructor TBot.Destroy;
begin

  inherited;
end;

function TBot.GetMode: TMode;
begin
  // «десь модуль принимает решение
  Result := GetRandomMode;
end;

procedure TBot.SetCurrentCandel(const ACandel: TCandel);
var
  xHP: Double;
begin
  // ѕередали боту св€чу, на основание которой примениет решение что ему делать
  xHP := 0;
  case FMode of
    tmBuy : xHP := ACandel.Close - ACandel.Open;
    tmSell: xHP := ACandel.Open  - ACandel.Close;
  end;
  FHealthPoints := FHealthPoints + xHP;
  FMode := GetMode;
end;

{ TStopLossBot }

constructor TStopLossBot.Create;
begin
  inherited;
  FTmpCountStop := 0;
  FCountStop := 5;
end;

destructor TStopLossBot.Destroy;
begin

  inherited;
end;

function TStopLossBot.GetMode: TMode;
begin
  if FTmpCountStop > 0 then
  begin
    Result := TMode.tmNull;
    Dec(FTmpCountStop);
  end
  else
    Result := inherited GetMode;
end;

procedure TStopLossBot.SetCurrentCandel(const ACandel: TCandel);

  function _HP_Buy(const ACandel: TCandel): Double;
  var
    xStop: Double;
  begin
    xStop := ACandel.Low - ACandel.Open;
    if xStop < FStopLoss then
    begin
      FTmpCountStop := FCountStop;
      Result := FStopLoss;
    end
    else
      Result := ACandel.Close - ACandel.Open;
  end;

  function _HP_Sell(const ACandel: TCandel): Double;
  var
    xStop: Double;
  begin
    xStop := ACandel.Open - ACandel.High;
    if xStop < FStopLoss then
    begin
      FTmpCountStop := FCountStop;
      Result := FStopLoss;
    end
    else
      Result := ACandel.Open - ACandel.Close;
  end;

var
  xHP: Double;
begin
  // ѕередали боту св€чу, на основание которой примениет решение что ему делать
  xHP := 0;
  case FMode of
    tmBuy : xHP := _HP_Buy(ACandel);
    tmSell: xHP := _HP_Sell(ACandel);
  end;
  FHealthPoints := FHealthPoints + xHP;
  FMode := GetMode;
end;

{ TTakeProfit }

constructor TTakeProfit.Create;
begin
  inherited;
  FTakeProfit := 20;
end;

destructor TTakeProfit.Destroy;
begin

  inherited;
end;

procedure TTakeProfit.SetCurrentCandel(const ACandel: TCandel);

  function _HP_Buy(const ACandel: TCandel): Double;
  var
    xProfit: Double;
    xStop: Double;
  begin
    xProfit := ACandel.High - ACandel.Open;
    if xProfit > FTakeProfit then
    begin
      Result := xProfit;
    end else
    begin
      xStop := ACandel.Low - ACandel.Open;
      if xStop < FStopLoss then
      begin
        FTmpCountStop := FCountStop;
        Result := FStopLoss;
      end
      else
        Result := ACandel.Close - ACandel.Open;
    end;
  end;

  function _HP_Sell(const ACandel: TCandel): Double;
  var
    xProfit: Double;
    xStop: Double;
  begin
    xProfit := ACandel.Open - ACandel.Low;
    if xProfit > FTakeProfit then
    begin
      Result := xProfit;
    end else
    begin
      xStop := ACandel.Open - ACandel.High;
      if xStop < FStopLoss then
      begin
        FTmpCountStop := FCountStop;
        Result := FStopLoss;
      end
      else
        Result := ACandel.Open - ACandel.Close;
    end;
  end;

var
  xHP: Double;
begin
  // ѕередали боту св€чу, на основание которой примениет решение что ему делать
  xHP := 0;
  case FMode of
    tmBuy : xHP := _HP_Buy(ACandel);
    tmSell: xHP := _HP_Sell(ACandel);
  end;
  FHealthPoints := FHealthPoints + xHP;
  FMode := GetMode;
end;

end.
