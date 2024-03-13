unit Lb.Bot.Tiket;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.Mode;

type
  TOnEventPosition = procedure(const ASander: TObject; AMode: TMode; APrice: Double) of object;

  ///<summary>Тиковый бот</summary>
  TTiketBot = class(TObject)
  private
    FOpenTime: Int64;
    FCloseTime: Int64;
    FOpenPrice: Double;
    FClosePrice: Double;
    FIsPosition: Boolean;
    FMode: TMode;
    FHealthPoints: Double;
  protected
    FInfoData: String;
    FOnOpenPosition: TOnEventPosition;
    FOnClosePosition: TOnEventPosition;
    procedure DoOpenPosition(AMode: TMode; APrice: Double); virtual;
    procedure DoClosePosition(AMode: TMode; APrice: Double);  virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property InfoData: String read FInfoData write FInfoData;
    ///<summary>Значение по умолчанию</summary>
    procedure Default;
    ///<summary>Принимаем решение</summary>
    procedure SetMode; virtual;
  public {Процедуры управление позиций}
    procedure SetOpenPosition(const ATime: Int64; const ALast: Double); virtual;
    procedure SetUpPosition(const ATime: Int64; const ALast: Double); virtual;
    procedure SetClosePosition(const ATime: Int64; const ALast: Double); virtual;
  public
    ///<summary>Позиция принятого решения</summary>
    property IsPosition: Boolean read FIsPosition;
    ///<summary>Цена открытие</summary>
    property OpenPrice: Double read FOpenPrice;
    property ClosePrice: Double read FClosePrice;

    property OpenTime: Int64 read FOpenTime;
    property CloseTime: Int64 read FCloseTime;

    ///<summary>Направление принятого решения</summary>
    property Mode: TMode read FMode write FMode;
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
    procedure SetOpenPosition(const ATime: Int64; const ALast: Double); override;
    procedure SetUpPosition(const ATime: Int64; const ALast: Double); override;
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
    procedure SetOpenPosition(const ATime: Int64; const ALast: Double); override;
    procedure SetUpPosition(const ATime: Int64; const ALast: Double); override;
  end;

  ///<summary>C фиксированной приболею</summary>
  TTakeProfitTiketBot = class(TStopLostTiketBot)
  private
    FTakeProfit: Double;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetUpPosition(const ATime: Int64; const ALast: Double); override;
    property TakeProfit: Double read FTakeProfit write FTakeProfit;
  end;

  ///<summary>Использования парного бота, когда один  бот работает в лонг, другой в шорт</summary>
  TPairBot = class(TObject)
  private
    FStopLoss: Double;
    FCountStop: Integer;
    FTakeProfit: Double;
  private
    FObverse: TTakeProfitTiketBot;
    FReverse: TTakeProfitTiketBot;
    procedure SetCountStop(const Value: Integer);
    procedure SetStopLoss(const Value: Double);
    procedure SetTakeProfit(const Value: Double);
    function GetIsPosition: Boolean;
    function GetHealthPoints: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Default;
    procedure SetOpenPosition(const ATime: Int64; const ALast: Double);
    procedure SetUpPosition(const ATime: Int64; const ALast: Double);
    procedure SetClosePosition(const ATime: Int64; const ALast: Double);
    procedure SetMode;
    property StopLoss: Double read FStopLoss write SetStopLoss;
    property CountStop: Integer read FCountStop write SetCountStop;
    property TakeProfit: Double read FTakeProfit write SetTakeProfit;
    property IsPosition: Boolean read GetIsPosition;
    property HealthPoints: Double read GetHealthPoints;
  public
    property Obverse: TTakeProfitTiketBot read FObverse;
    property Reverse: TTakeProfitTiketBot read FReverse;
  end;

type
  ///<summary>Проведение анализа объекта</summary>
  TTradeBot = class(TObject)
  public type
    TStatusPosition = (spNull = 0, spOpen, spClose);
    TStatusBot = (sbNull = 0, sbObverse, sbReverse);
    TPosition = class(TObject)
    private
      FID: Integer;
      FOpenTime: Int64;
      FCloseTime: Int64;
      FOpen: Double;
      FClose: Double;
      FMode: TMode;
      FQuantity: Double;
      FStatusBot: TStatusBot;
      FStatus: TStatusPosition;
      function GetProfit: Double;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      property ID: Integer read FID write FID;
      property OpenTime: Int64 read FOpenTime write FOpenTime;
      property CloseTime: Int64 read FCloseTime write FCloseTime;
      property Open: Double read FOpen write FOpen;
      property Close: Double read FClose write FClose;
      property Mode: TMode read FMode write FMode;
      property Quantity: Double read FQuantity write FQuantity;
      property StatusBot: TStatusBot read FStatusBot write FStatusBot;
      property Status: TStatusPosition read FStatus write FStatus;
      property Profit: Double read GetProfit;
    public
      function GetUpData(const APrice: Double): Double;
    end;
    TPositionList = TObjectList<TPosition>;
  private
    FPositions: TPositionList;
  protected
    function GetCreatePosition: TPosition;
    function GetIndexOfID(const AID: Integer): Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetOpenPosition(
      AOpenTime: Int64;
      AOpen: Double;
      AMode: TMode;
      AQuantity: Double;
      AStatusBot: TStatusBot
    ): Integer;

    procedure SetClosePosition(
      AID: Integer;
      ACloseTime: Int64;
      AClose: Double
    );

    function GetProfit(const APrice: Double): Double;

    property Positions: TPositionList read FPositions;
  end;


implementation

uses
  System.DateUtils;

function GetRandomMode: TMode;
var
  xMode: Integer;
begin
  // Здесь модуль принимает решение
  xMode := Random(3);
  Result := TMode(xMode);
end;



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

procedure TTiketBot.SetOpenPosition(const ATime: Int64; const ALast: Double);
begin
  // открываем позицию
  if (FMode = tmBuy) or (FMode = tmSell) then
  begin
    if not FIsPosition then
    begin
      FOpenTime := ATime;
      FOpenPrice := ALast;
      FIsPosition := True;
      DoOpenPosition(FMode,FOpenPrice);
    end;
  end;
end;

procedure TTiketBot.SetUpPosition(const ATime: Int64; const ALast: Double);
begin
  // Обновляем условия позиции
end;

procedure TTiketBot.SetClosePosition(const ATime: Int64; const ALast: Double);
var
  xHP: Double;
begin
  // Принудительное закрытие
  xHP := 0;
  FCloseTime := ATime;
  FClosePrice := ALast;
  if FIsPosition then
  begin
    case FMode of
      tmBuy : xHP := FClosePrice - FOpenPrice;
      tmSell: xHP := FOpenPrice  - FClosePrice;
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

procedure TStopLostTiketBot.SetOpenPosition(const ATime: Int64; const ALast: Double);
begin
  inherited SetOpenPosition(ATime,ALast);
  if FIsPosition then
    FTmpCountStop := 0;
end;



procedure TStopLostTiketBot.SetUpPosition(const ATime: Int64; const ALast: Double);
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
      SetClosePosition(ATime,ALast)
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

procedure TTrelingStopTiketBot.SetOpenPosition(const ATime: Int64; const ALast: Double);
begin
  inherited SetOpenPosition(ATime,ALast);
  if FIsPosition then
    FFixProfit := FStopLoss;
end;

procedure TTrelingStopTiketBot.SetUpPosition(const ATime: Int64; const ALast: Double);
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
      SetClosePosition(ATime, ALast);
    end;

    if xProfit < FStopLoss then
    begin
      FTmpCountStop := FCountStop;
      SetClosePosition(ATime, ALast);
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

procedure TTakeProfitTiketBot.SetUpPosition(const ATime: Int64; const ALast: Double);
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
      SetClosePosition(ATime, ALast)
    end else if xProfit < FStopLoss then
    begin
      FTmpCountStop := FCountStop;
      SetClosePosition(ATime, ALast)
    end;
  end;
end;

{ TPairBot }

constructor TPairBot.Create;
begin
  FObverse := TTakeProfitTiketBot.Create;
  FObverse.InfoData := 'obverse';
  FReverse := TTakeProfitTiketBot.Create;
  FReverse.InfoData := 'reverse'
end;

destructor TPairBot.Destroy;
begin
  FreeAndNil(FReverse);
  FreeAndNil(FReverse);
  inherited;
end;

procedure TPairBot.Default;
begin
  FObverse.Default;
  FReverse.Default;
end;

function TPairBot.GetIsPosition: Boolean;
begin
  Result := FObverse.IsPosition and FReverse.IsPosition;
end;

procedure TPairBot.SetCountStop(const Value: Integer);
begin
  FCountStop := Value;
  FObverse.CountStop := FCountStop;
  FReverse.CountStop := FCountStop;
end;

procedure TPairBot.SetMode;
begin
  FObverse.SetMode;
  case FObverse.Mode of
    tmBuy: FReverse.Mode := tmSell;
    tmSell: FReverse.Mode := tmBuy;
    tmNull: FReverse.Mode := tmNull;
  end;
end;

procedure TPairBot.SetStopLoss(const Value: Double);
begin
  FStopLoss := Value;
  FObverse.StopLoss := FStopLoss;
  FReverse.StopLoss := FStopLoss;
end;

procedure TPairBot.SetTakeProfit(const Value: Double);
begin
  FTakeProfit := Value;
  FObverse.TakeProfit := FTakeProfit;
  FReverse.TakeProfit := FTakeProfit;
end;


procedure TPairBot.SetOpenPosition(const ATime: Int64; const ALast: Double);
begin
  if not Self.IsPosition then
  begin
    FObverse.SetOpenPosition(ATime, ALast);
    FReverse.SetOpenPosition(ATime, ALast);
  end;
end;

procedure TPairBot.SetClosePosition(const ATime: Int64; const ALast: Double);
begin
  if Self.IsPosition then
  begin
    FObverse.SetClosePosition(ATime, ALast);
    FReverse.SetClosePosition(ATime, ALast);
  end;
end;

procedure TPairBot.SetUpPosition(const ATime: Int64; const ALast: Double);
begin
  FObverse.SetUpPosition(ATime, ALast);
  FReverse.SetUpPosition(ATime, ALast);
end;

function TPairBot.GetHealthPoints: Double;
begin
  Result :=
    FObverse.HealthPoints +
    FReverse.HealthPoints;
end;

{ TTradeBot.TPosition }

constructor TTradeBot.TPosition.Create;
begin

end;

destructor TTradeBot.TPosition.Destroy;
begin

  inherited;
end;

function TTradeBot.TPosition.GetProfit: Double;
begin
  Result := 0;
  if Status = TTradeBot.TStatusPosition.spClose then
  begin
    case FMode of

      tmBuy : Result := (FClose - FOpen) * FQuantity;
      tmSell: Result := (FOpen  - FClose) * FQuantity;
    end;
  end;
end;

function TTradeBot.TPosition.GetUpData(const APrice: Double): Double;

  function GetProfit(const AOpen, AClose: Double): Double;
  begin
    Result := 0;
    case FMode of
      tmBuy: Result := AClose - AOpen;
      tmSell: Result := AOpen - AClose;
    end;
  end;

begin
  Result := 0;
  case FStatus of
    spOpen: begin
      Result := GetProfit(FOpen, APrice) * FQuantity;
    end;
    spClose: begin
      Result := GetProfit(FOpen,FClose) * FQuantity;
    end;
  end;
end;

{ TTradeBot }

constructor TTradeBot.Create;
begin
  FPositions := TPositionList.Create;
end;

destructor TTradeBot.Destroy;
begin
  FreeAndNil(FPositions);
  inherited;
end;

function TTradeBot.GetCreatePosition: TPosition;
var
  xPosition: TPosition;
begin
  xPosition := TPosition.Create;
  xPosition.ID := FPositions.Add(xPosition);
  Result := xPosition;
end;

function TTradeBot.GetIndexOfID(const AID: Integer): Integer;
var
  i, iCount: Integer;
  xPosition: TPosition;
begin
  Result := -1;
  iCount := FPositions.Count;
  if iCount > 0 then
    for i := iCount - 1 downto 0 do
    begin
      xPosition := FPositions[i];
      if xPosition.ID = AID then
      begin
        Result := i;
        Break;
      end;
    end;
end;


function TTradeBot.GetOpenPosition(AOpenTime: Int64; AOpen: Double;
  AMode: TMode; AQuantity: Double; AStatusBot: TStatusBot): Integer;
var
  xPosition: TPosition;
begin
  xPosition := GetCreatePosition;
  xPosition.OpenTime  := AOpenTime;
  xPosition.Open      := AOpen;
  xPosition.Mode      := AMode;
  xPosition.Quantity  := AQuantity;
  xPosition.StatusBot := AStatusBot;
  xPosition.Status    := TStatusPosition.spOpen;
  Result := xPosition.FID;
end;

procedure TTradeBot.SetClosePosition(AID: Integer; ACloseTime: Int64; AClose: Double);
var
  xIndex: Integer;
  xPosition: TPosition;
begin
  xIndex := GetIndexOfID(AID);
  if xIndex >= 0 then
  begin
    xPosition := FPositions[xIndex];
    xPosition.CloseTime := ACloseTime;
    xPosition.Close     := AClose;
    xPosition.Status    := TStatusPosition.spClose;
  end
  else
    raise Exception.Create('Error Message: Ошибка закрытие позиции');
end;

function TTradeBot.GetProfit(const APrice: Double): Double;
var
  xProfit: Double;
  i, iCount: Integer;
  xPosition: TPosition;
begin
  xProfit := 0;
  iCount := FPositions.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xPosition := FPositions[i];
      xProfit := xProfit + xPosition.GetUpData(APrice);
    end;
  Result := xProfit;
end;

end.
