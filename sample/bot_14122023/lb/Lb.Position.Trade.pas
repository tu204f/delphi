unit Lb.Position.Trade;

interface

{$I tiket.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.Mode,
  Lb.SysUtils;

type
  ///<summary>Направление принимаемого решение</summary>
  ///TMode       = (tmNull = 0, tmBuy,  tmSell);
  TTypeStatus = (tsNull = 0, tsOpen, tsClose);

  ///<summary>Событие закрытие позиции</summary>
  TEventClosePosition = procedure(Sender: Tobject; APrice: Double; AMode: TMode) of object;

  TTypeStopLoss = (slNorm,slTrailingStop);

  ///<summary>Объект позиции</summary>
  TTradePostion = class(TObject)
  private
    FOpenTime: {$IFDEF RUB} TDateTime {$ELSE} Int64 {$ENDIF};
    FCloseTime: {$IFDEF RUB} TDateTime {$ELSE} Int64 {$ENDIF};
    FOpen: Double;       // Цена открытие
    FClose: Double;      // Цена закрытия
    FQuantity: Double;   // Объем
    FMode: TMode;
    FStatus: TTypeStatus;
    FTakeProfit: Double;
    FStopLoss: Double;
    FStopProfit: Double;
    FIsTakeProfit: Boolean;
    FIsStopLoss: Boolean;
    FTypeStopLoss: TTypeStopLoss;
  private
    FProfit: Double;
    FProfitQuantity: Double;
    FMaxProfit: Double;
    FMinProfit: Double;
    FCommissionValue: Double;
  private
    FID: Integer;
    FCommission: Double;
    FOnOpen: TNotifyEvent;
    FOnClose: TNotifyEvent;
    function Get_CloseTime: TDateTime;
    function Get_OpenTime: TDateTime;
    function GetStopLoss: Double;
    function GetPriceSL: Double;
    function GetPriceTP: Double;
  protected
    procedure DoOpen;  virtual;
    procedure DoClose; virtual;
  public
    constructor Create(const AID: Integer); virtual;
    destructor Destroy; override;

    procedure SetOpen(
      const ATime: {$IFDEF RUB} TDateTime {$ELSE} Int64 {$ENDIF};
      const APrice: Double;
      const AQuantity: Double;
      const AMode: TMode;
      const ATakeProfit: Double = 0;
      const AStopLoss: Double = 0
    );

    procedure SetClose(
      const ATime: {$IFDEF RUB} TDateTime {$ELSE} Int64 {$ENDIF};
      const APrice: Double
    );

    procedure SetUpData(
      const ATime: {$IFDEF RUB} TDateTime {$ELSE} Int64 {$ENDIF};
      const APrice: Double
    );

    property Commission: Double read FCommission write FCommission;
    property ID: Integer read FID;
    property OpenTime: {$IFDEF RUB} TDateTime {$ELSE} Int64 {$ENDIF} read FOpenTime;
    property CloseTime: {$IFDEF RUB} TDateTime {$ELSE} Int64 {$ENDIF} read FCloseTime;
    property Open: Double read FOpen;
    property Close: Double read FClose;
    property Quantity: Double read FQuantity;
    property Mode: TMode read FMode;
    property Status: TTypeStatus read FStatus;
    property Profit: Double read FProfit;
    property ProfitQuantity: Double read FProfitQuantity;
    property MaxProfit: Double read FMaxProfit;
    property MinProfit: Double read FMinProfit;
    property CommissionValue: Double read FCommissionValue;
    property TakeProfit: Double read FTakeProfit;
    property StopLoss: Double read GetStopLoss;
    property StopProfit: Double read FStopProfit;
    property PriceTP: Double read GetPriceTP;
    property PriceSL: Double read GetPriceSL;
    property IsTakeProfit: Boolean read FIsTakeProfit;
    property IsStopLoss: Boolean read FIsStopLoss;

    property TypeStopLoss: TTypeStopLoss read FTypeStopLoss write FTypeStopLoss;
  public
    property OnOpen: TNotifyEvent write FOnOpen;
    property OnClose: TNotifyEvent write FOnClose;
  public {Дополнительная информация для класса }
    property _OpenTime: TDateTime read Get_OpenTime;
    property _CloseTime: TDateTime read Get_CloseTime;
  end;
  TTradePostionList = TObjectList<TTradePostion>;

  ///<summary>Список позиции</summary>
  TTradePostions = class(TObject)
  private
    FPostions: TTradePostionList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetOpen(
        const AOpenTime: {$IFDEF RUB} TDateTime {$ELSE} Int64 {$ENDIF};
        const AOpen: Double;
        const AQuantity: Double;
        const AMode: TMode;
        const ATakeProfit: Double = 0;
        const AStopLoss: Double = 0
    ): TTradePostion;
    procedure SetClose(
      const AID: Integer;
      const ACloseTime: {$IFDEF RUB} TDateTime {$ELSE} Int64 {$ENDIF};
      const AClose: Double
    );
    procedure SetUpData(
      const ATime : {$IFDEF RUB} TDateTime {$ELSE} Int64 {$ENDIF};
      const APrice: Double
    );
    function IndexOfID(const AID: Integer): Integer;
    function GetProfit(const APrice: Double): Double;
    property Items: TTradePostionList read FPostions;
  end;

function GetStrToMode(const AMode: TMode): String;
function GetStrToStaus(const AStatus: TTypeStatus): String;

implementation

uses
  System.DateUtils;

function GetStrToMode(const AMode: TMode): String;
begin
  case AMode of
    tmNull: Result := 'Null';
    tmBuy: Result := 'Buy';
    tmSell: Result := 'Sell';
  end;
end;

function GetStrToStaus(const AStatus: TTypeStatus): String;
begin
  case AStatus of
    tsNull: Result := 'Null';
    tsOpen: Result := 'Open';
    tsClose: Result := 'Close';
  end;
end;

{ TTradePostion }

constructor TTradePostion.Create(const AID: Integer);
begin
  FTypeStopLoss := TTypeStopLoss.slNorm;
  FStopProfit := 0;
  FID := AID;
  FCommission := 0;
  FMaxProfit := 0;
  FMinProfit := 0;
  FIsTakeProfit := False;
  FIsStopLoss := False;
end;

destructor TTradePostion.Destroy;
begin

  inherited;
end;

procedure TTradePostion.DoOpen;
begin
  if Assigned(FOnOpen) then
    FOnOpen(Self);
end;

procedure TTradePostion.DoClose;
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TTradePostion.SetOpen(
  const ATime: {$IFDEF RUB} TDateTime {$ELSE} Int64 {$ENDIF};
  const APrice, AQuantity: Double; const AMode: TMode;
  const ATakeProfit: Double = 0;
  const AStopLoss: Double = 0);
begin
  FOpenTime   := ATime;
  FCloseTime  := 0;
  FOpen       := APrice;
  FClose      := 0;
  FQuantity   := AQuantity;
  FMode       := AMode;
  FStatus     := TTypeStatus.tsOpen;
  FTakeProfit := ATakeProfit;
  FStopLoss   := AStopLoss;
  FStopProfit := -1 * AStopLoss;
  DoOpen;
end;

procedure TTradePostion.SetClose(
  const ATime: {$IFDEF RUB} TDateTime {$ELSE} Int64 {$ENDIF};
  const APrice: Double);
begin
  FCloseTime:= ATime;
  FClose    := APrice;
  FStatus   := TTypeStatus.tsClose;
  DoClose;
end;

procedure TTradePostion.SetUpData(
  const ATime: {$IFDEF RUB} TDateTime {$ELSE} Int64 {$ENDIF};
  const APrice: Double
);

  function _Round(const AValue: Double): Double;
  begin
    Result := Trunc(AValue * 1000)/1000;
  end;

  function _IsClosePrice(const AClosePrice, ACurrentPrice: Double): Double;
  begin
    if ACurrentPrice <= 0 then
    begin
      Result := AClosePrice;
    end
    else
    begin
      case Status of
        tsOpen: Result := ACurrentPrice;
        tsClose: Result := AClosePrice;
      else
        raise Exception.Create('Error Message: Состояние опции не определено');
      end;
    end;
  end;

  procedure _Commission(AClosePrice: Double);
  {$IFNDEF RUB}
  var
    xValue: Double;
  {$ENDIF}
  begin
    {$IFDEF RUB}
    FProfitQuantity  := FProfitQuantity - 2 * FCommission * FQuantity;
    FCommissionValue := 2 * FCommission * FQuantity;
    {$ELSE}
    xValue := FOpen * FQuantity + AClosePrice * FQuantity;
    FCommissionValue := xValue * (FCommission/100);
    FProfitQuantity := FProfitQuantity - FCommissionValue;
    {$ENDIF}
  end;


  procedure _StopLoss;
  var
    xProfit: Double;
  begin
    case FTypeStopLoss of
      TTypeStopLoss.slNorm: begin
        if FStopLoss > 0 then
        begin
          FIsStopLoss := FProfit <= (-1 * FStopLoss);
          if FIsStopLoss then
            SetClose(ATime,APrice);
        end
        else
          FIsStopLoss := False;
      end;
      TTypeStopLoss.slTrailingStop: begin
        xProfit := FProfit - FStopLoss;
        if FStopProfit < xProfit then
          FStopProfit := xProfit;
        FIsStopLoss := FProfit <= FStopProfit;
        if FIsStopLoss then
          SetClose(ATime,APrice);
      end;
    end;
  end;

var
  xClosePrice: Double;
begin
  xClosePrice := _IsClosePrice(FClose, APrice);
  case FMode of
    tmBuy: begin
      FProfit := (xClosePrice - FOpen);
      FProfitQuantity := FProfit * FQuantity;
    end;
    tmSell: begin
      FProfit := (FOpen - xClosePrice);
      FProfitQuantity := FProfit * FQuantity;
    end;
  else
    raise Exception.Create('Error Message: Направление позиции не определенно');
  end;

  FProfit := _Round(FProfit);
  FProfitQuantity := _Round(FProfitQuantity);

  // Расчет комисии
  if FCommission > 0 then
    _Commission(xClosePrice);

  // Передельные заначения

  if FMaxProfit < FProfit then
    FMaxProfit := FProfit;

  if FMinProfit = 0 then
    FMinProfit := FProfit
  else
  begin
    if FMinProfit > FProfit then
      FMinProfit := FProfit;
  end;

  // ##
  // Закрытие по стоп лоссу
   _StopLoss;


  // ##
  if FTakeProfit > 0 then
  begin
    FIsTakeProfit := FProfit >= FTakeProfit;
    if IsTakeProfit then
      SetClose(ATime,APrice);
  end
  else
    FIsTakeProfit := False;
end;

function TTradePostion.Get_OpenTime: TDateTime;
begin
  Result := UnixToDateTime(Trunc(OpenTime/1000));
end;


function TTradePostion.Get_CloseTime: TDateTime;
begin
  Result := UnixToDateTime(Trunc(CloseTime/1000));
end;

function TTradePostion.GetStopLoss: Double;
begin
  Result := 0;
  if FStopLoss > 0 then
    Result := -1 * FStopLoss;
end;

function TTradePostion.GetPriceTP: Double;
begin
  Result := FOpen + FTakeProfit;
end;

function TTradePostion.GetPriceSL: Double;
begin
  Result := FOpen - FStopLoss;
end;

{ TTradePostions }

constructor TTradePostions.Create;
begin
  FPostions := TTradePostionList.Create;
end;

destructor TTradePostions.Destroy;
begin
  FreeAndNil(FPostions);
  inherited;
end;

function TTradePostions.IndexOfID(const AID: Integer): Integer;
var
  i, iCount: Integer;
  xPostion: TTradePostion;
begin
  Result := -1;
  iCount := FPostions.Count;
  if iCount > 0 then
    for i := iCount - 1 downto 0 do
    begin
      xPostion := FPostions[i];
      if xPostion.ID = AID then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function TTradePostions.GetOpen(
    const AOpenTime: {$IFDEF RUB} TDateTime {$ELSE} Int64 {$ENDIF};
    const AOpen: Double;
    const AQuantity: Double;
    const AMode: TMode;
    const ATakeProfit: Double = 0;
    const AStopLoss: Double = 0
  ): TTradePostion;
var
  xPostion: TTradePostion;
begin
  xPostion := TTradePostion.Create(FPostions.Count);
  FPostions.Add(xPostion);
  xPostion.SetOpen(
    AOpenTime,
    AOpen,
    AQuantity,
    AMode,
    ATakeProfit,
    AStopLoss
  );
  Result := xPostion;
end;

procedure TTradePostions.SetClose(
  const AID: Integer;
  const ACloseTime: {$IFDEF RUB} TDateTime {$ELSE} Int64 {$ENDIF};
  const AClose: Double
);
var
  xInd: Integer;
  xPostion: TTradePostion;
begin
  xInd := IndexOfID(AID);
  if xInd >= 0 then
  begin
    xPostion := FPostions[xInd];
    xPostion.SetClose(ACloseTime,AClose);
  end;
end;

procedure TTradePostions.SetUpData(
  const ATime: {$IFDEF RUB} TDateTime {$ELSE} Int64 {$ENDIF};
  const APrice: Double
);
var
  xPostion: TTradePostion;
  i, iCount: Integer;
begin
  iCount := FPostions.Count;
  if iCount > 0 then
    for i := iCount - 1 downto 0 do
    begin
      xPostion := FPostions[i];
      if xPostion.Status = tsOpen then
        xPostion.SetUpData(ATime,APrice);
    end;
end;

function TTradePostions.GetProfit(const APrice: Double): Double;
var
  xSum: Double;
  i, iCount: Integer;
  xPostion: TTradePostion;
begin
  xSum := 0;
  iCount := FPostions.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xPostion := FPostions[i];
      xSum := xSum + xPostion.ProfitQuantity;
    end;
  Result :=  xSum;
end;

end.
