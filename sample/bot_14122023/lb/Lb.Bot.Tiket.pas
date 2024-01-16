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
  TOnEventPosition = procedure(const ASander: TObject; AMode: TMode; APrice: Double) of object;

  ///<summary>Тиковый бот</summary>
  TTiketBot = class(TObject)
  private
    FOpenPrice: Double;
    FIsPosition: Boolean;
    FMode: TMode;
    FHealthPoints: Double;
  protected
    FOnOpenPosition: TOnEventPosition;
    FOnClosePosition: TOnEventPosition;
    procedure DoOpenPosition(AMode: TMode; APrice: Double); virtual;
    procedure DoClosePosition(AMode: TMode; APrice: Double);  virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>Значение по умолчанию</summary>
    procedure Default;
    ///<summary>Принимаем решение</summary>
    procedure SetMode; virtual;
  public {Процедуры управление позиций}
    procedure SetOpenPosition(const ALast: Double); virtual;
    procedure SetUpPosition(const ALast: Double); virtual;
    procedure SetClosePosition(const ALast: Double); virtual;
  public
    ///<summary>Позиция принятого решения</summary>
    property IsPosition: Boolean read FIsPosition;
    ///<summary>Цена открытие</summary>
    property OpenPrice: Double read FOpenPrice;
    ///<summary>Направление принятого решения</summary>
    property Mode: TMode read FMode;
    ///<summary>Состояние уровня жизни</summary>
    property HealthPoints: Double read FHealthPoints;
  public
    property OnOpenPosition: TOnEventPosition write FOnOpenPosition;
    property OnClosePosition: TOnEventPosition write FOnClosePosition;
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
    procedure SetMode; override;
    procedure SetRvsMode(AMode: TMode);
  public
    procedure SetOpenPosition(const ALast: Double); override;
    procedure SetUpPosition(const ALast: Double); override;
  public
    ///<summary>Размер получаемого убытка, </summary>
    ///<remarks>Значение устанавливается отрицательным</remarks>
    property StopLoss: Double write FStopLoss;
    ///<summary>После полученного убытка, количество свечей ожидания<summary>
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

  ///<summary>C фиксированной приболею</summary>
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
  Default;
end;

destructor TTiketBot.Destroy;
begin

  inherited;
end;

procedure TTiketBot.DoOpenPosition(AMode: TMode; APrice: Double);
begin
  if Assigned(FOnOpenPosition) then
    FOnOpenPosition(Self,AMode,APrice);
end;

procedure TTiketBot.DoClosePosition(AMode: TMode; APrice: Double);
begin
  if Assigned(FOnClosePosition) then
    FOnClosePosition(Self,AMode,APrice);
end;

procedure TTiketBot.Default;
begin
  FIsPosition := False;
  FMode := tmNull;
  FHealthPoints := 0;
end;

procedure TTiketBot.SetMode;
begin
  {todo: организовать стретегию входа}
  FMode := GetRandomMode;
end;

procedure TTiketBot.SetOpenPosition(const ALast: Double);
begin
  // открываем позицию
  if (FMode = tmBuy) or (FMode = tmSell) then
  begin
    if not FIsPosition then
    begin
      FOpenPrice := ALast;
      FIsPosition := True;
      DoOpenPosition(FMode,FOpenPrice);
    end;
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
    DoClosePosition(FMode,ALast);
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


procedure TStopLostTiketBot.SetMode;
begin
  FMode := tmNull;
  if FTmpCountStop > 0 then
    Dec(FTmpCountStop)
  else
    inherited SetMode;
end;

procedure TStopLostTiketBot.SetRvsMode(AMode: TMode);
begin
  FMode := AMode;
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
    // При достижение придельного значение убытка производим фиксации
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

    // Производи фисацию, результата при росте
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
