unit Lb.Trader;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils.Candel;

const
  ///<summary>Размер буфера свечей</summary>
  SIZE_BUFFER_CANDEL = 100;

type
  TTypeLine = (tlNull,tlSupport, tlResistance);
  TEventIntersection = procedure(Sender: TObject; ATypeLine: TTypeLine; APrice: Double) of object;

  ///<summary>Объект, который отвечает за открытие позиции</summary>
  TLevelSR = class(TObject)
  private
    FTypeLine: TTypeLine;
    FBufferCandels: TCandels;
    function GetPotential: Double;
  protected {придельные значение}
    FMaxPeriod, FMinPeriod,  FStep: Integer;
  protected {определяем уровень для открытия}
    FSupport: Double;
    FResistance: Double;
    FPeriodSupport: Integer;
    FPeriodResistance: Integer;
    FOnEventIntersection: TEventIntersection;
  protected
    ///<summary>Рассчитать придельные значение уровней поддержки и сопротивление</summary>
    procedure DoLevelSupportResistance;
    procedure DoEventIntersection(const ATypeLine: TTypeLine; const APrice: Double);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddBufferCandel(const ACandel: TCandel);
    ///<summary>У каждого трейдора свой набор объекта</summary>
    property BufferCandels: TCandels read FBufferCandels;
  public
    property MaxPeriod: Integer read FMaxPeriod write FMaxPeriod;
    property MinPeriod: Integer read FMinPeriod write FMinPeriod;
    property Step: Integer read FStep write FStep;
    property Support: Double read FSupport;
    property Resistance: Double read FResistance;
    property PeriodSupport: Integer read FPeriodSupport;
    property PeriodResistance: Integer read FPeriodResistance;
    property Potential: Double read GetPotential;
    property OnEventIntersection: TEventIntersection write FOnEventIntersection;
  end;

  TTrades = class(TObject)
  public const
    SIZE_PROFIT = 2;

    ST_ACTIVE = 1;
    ST_CLOSE_SL  = 2;
    ST_CLOSE_TP  = 3;

  public type
    TTrade = record
      Date: TDateTime;
      Time: TDateTime;
      IndexInc: Integer;

      Potential: Double;
      Price: Double;
      Quantity: Integer;
      BuySell: Char;

      SL: Double;
      TP: Double;

      Close_IndexInc: Integer;
      Close_Date: TDateTime;
      Close_Time: TDateTime;
      Close_Potential: Double;

      Status: Integer;

    private
      function GetProfit: Double;
    public
      property Profit: Double read GetProfit;

    end;
    TTradeList = TList<TTrade>;

  private
    FTradeClose: TTradeList;
    FItems: TTradeList;
    function GetCount: Integer;

  protected
    FPosiotion: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;

    procedure Operation(
      ADate: TDateTime;
      ATime: TDateTime;
      AIndexInc: Integer;
      APrice: Double;
      AQuantity: Integer;
      ABuySell: Char;
      APotential: Double
    );
    procedure UpDate(
      ADate: TDateTime;
      ATime: TDateTime;
      AIndexInc: Integer;
      APrice: Double;
      APotential: Double
    );
    property Items: TTradeList read FItems;
    property TradeClose: TTradeList read FTradeClose;
    property Count: Integer read GetCount;
    property Posiotion: Integer read FPosiotion;
  end;


  ///<summary>Список сделок</summary>
  TLevelSRList = TObjectList<TLevelSR>;

implementation

///<summary>Вычисляем уровень поддержки</summary>
function GetPeriodSupportValue(const APeriod: Integer; ABufferCandels: TCandels): Double;
var
  i, iCount: Integer;
  xCandel: TCandel;
  xCurIndex, xInd: Integer;
  xSupport: Double;
begin
  xSupport := 0;
  iCount := ABufferCandels.Count;
  if iCount > APeriod then
    iCount := APeriod;
  if iCount > 0 then
  begin
    xCurIndex := ABufferCandels.Count - 3;
    xCandel := ABufferCandels.Items[xCurIndex];
    xSupport := xCandel.Low;
    for i := 1 to iCount - 1 do
    begin

      xInd := xCurIndex - i;
      if xInd >= ABufferCandels.Count then
        Break;

      xCandel := ABufferCandels.Items[xInd];
      if xSupport > xCandel.Low then
        xSupport := xCandel.Low;
    end;
  end;
  Result := xSupport;
end;

///<summary>Вычисляем уровень поддержки</summary>
function GetPeriodResistanceValue(const APeriod: Integer; ABufferCandels: TCandels): Double;
var
  i, iCount: Integer;
  xCandel: TCandel;
  xCurIndex, xInd: Integer;
  xResistance: Double;
begin
  xResistance := 0;
  iCount := ABufferCandels.Count;
  if iCount > APeriod then
    iCount := APeriod;
  if iCount > 0 then
  begin
    xCurIndex := ABufferCandels.Count - 3;
    xCandel := ABufferCandels.Items[xCurIndex];
    xResistance := xCandel.High;
    for i := 1 to iCount - 1 do
    begin
      xInd := xCurIndex - i;
      if xInd >= ABufferCandels.Count then
        Break;

      xCandel := ABufferCandels.Items[xInd];
      if xResistance < xCandel.High then
        xResistance := xCandel.High;
    end;
  end;
  Result := xResistance;
end;

{ TLevelSR }

constructor TLevelSR.Create;
begin
  FTypeLine := tlNull;

  FBufferCandels := TCandels.Create;

  FMaxPeriod := 30;
  FMinPeriod := 10;
  FStep := 3;

  FPeriodSupport := FMinPeriod;
  FPeriodResistance := FMinPeriod;
end;

destructor TLevelSR.Destroy;
begin
  FreeAndNil(FBufferCandels);
  inherited;
end;

procedure TLevelSR.DoEventIntersection(const ATypeLine: TTypeLine; const APrice: Double);
begin
  if FTypeLine <> ATypeLine then
    if Assigned(FOnEventIntersection) then
    begin
      FOnEventIntersection(Self,ATypeLine,APrice);
      FTypeLine := ATypeLine;
    end;
end;

procedure TLevelSR.DoLevelSupportResistance;

  procedure _Support(const AValue: Double);
  begin
    // поддержка
    if AValue < FSupport then
    begin
      FPeriodSupport := FPeriodSupport - FStep;
      if FPeriodSupport < FMinPeriod then
        FPeriodSupport := FMinPeriod;
    end
    else
    begin
      FPeriodSupport := FPeriodSupport + 1;
      if FPeriodSupport >= FMaxPeriod then
        FPeriodSupport := FMaxPeriod;
    end;
    FSupport := AValue;
  end;


  procedure _Resistance(const AValue: Double);
  begin
    // Сопративление
    if AValue > FResistance then
    begin
      FPeriodResistance := FPeriodResistance - FStep;
      if FPeriodResistance < FMinPeriod then
        FPeriodResistance := FMinPeriod;
    end
    else
    begin
      FPeriodResistance := FPeriodResistance + 1;
      if FPeriodResistance >= FMaxPeriod then
        FPeriodResistance := FMaxPeriod;
    end;
    FResistance := AValue;
  end;

var
  xSupport: Double;
  xResistance: Double;
begin
  // Уровень поддержки сопративление для открытие позиции
  xSupport := GetPeriodSupportValue(FPeriodSupport, FBufferCandels);
  xResistance := GetPeriodResistanceValue(FPeriodResistance, FBufferCandels);

  _Support(xSupport);
  _Resistance(xResistance);
end;

function TLevelSR.GetPotential: Double;
begin
  Result := FResistance - FSupport;
end;

procedure TLevelSR.AddBufferCandel(const ACandel: TCandel);
begin
  FBufferCandels.Add(ACandel);
  if FBufferCandels.Count > SIZE_BUFFER_CANDEL then
    FBufferCandels.Delete(0);
  if FBufferCandels.Count > FMaxPeriod then
  begin
    DoLevelSupportResistance;
    if ACandel.High > FResistance then
      DoEventIntersection(TTypeLine.tlResistance,FResistance);
    if ACandel.Low < FSupport then
      DoEventIntersection(TTypeLine.tlSupport,FSupport);
  end;
end;

{ TTrades }

constructor TTrades.Create;
begin
  FItems := TTradeList.Create;
  FTradeClose := TTradeList.Create;
  FPosiotion := 0;
end;

destructor TTrades.Destroy;
begin
  FreeAndNil(FTradeClose);
  FreeAndNil(FItems);
  inherited;
end;

procedure TTrades.Clear;
begin
  FItems.Clear;
end;

function TTrades.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TTrades.Operation(
  ADate: TDateTime;
  ATime: TDateTime;
  AIndexInc: Integer;
  APrice: Double;
  AQuantity: Integer;
  ABuySell: Char;
  APotential: Double
);
var
  xTrade: TTrade;
begin
  xTrade.Date := ADate;
  xTrade.Time := ATime;
  xTrade.IndexInc := AIndexInc;
  xTrade.Potential := APotential;
  xTrade.Price := APrice;
  xTrade.Quantity := AQuantity;
  xTrade.BuySell := ABuySell;

  case ABuySell of
    'B': begin
      xTrade.SL := APrice - APotential;
      xTrade.TP := APrice + Trunc(SIZE_PROFIT * APotential);
    end;
    'S': begin
      xTrade.SL := APrice + APotential;
      xTrade.TP := APrice - Trunc(SIZE_PROFIT * APotential);
    end;
  end;

  // Активная торговая оперция
  xTrade.Status := ST_ACTIVE;


  if ABuySell = 'B' then
    FPosiotion := FPosiotion + AQuantity
  else
    FPosiotion := FPosiotion - AQuantity;

  FItems.Add(xTrade);


end;

procedure TTrades.UpDate(
  ADate: TDateTime;
  ATime: TDateTime;
  AIndexInc: Integer;
  APrice: Double;
  APotential: Double
);

  function _ReversSB(const AValue: Char): Char;
  begin
    if AValue = 'B' then
      Result := 'S'
    else
      Result := 'B';
  end;

var
  xTrade: TTrade;
  i, iCount: Integer;
begin
  iCount := FItems.Count;
  if iCount > 0 then
    for i := iCount - 1 downto 0 do
    begin
      xTrade := FItems[i];
      if xTrade.Status = ST_ACTIVE then
      begin
        case xTrade.BuySell of
          'B': begin
            if xTrade.SL >= APrice then
              xTrade.Status := ST_CLOSE_SL
            else if xTrade.TP <= APrice then
              xTrade.Status := ST_CLOSE_TP
          end;
          'S': begin
            if xTrade.SL <= APrice then
              xTrade.Status := ST_CLOSE_SL
            else if xTrade.TP >= APrice then
              xTrade.Status := ST_CLOSE_TP
          end;
        end;
      end;
      if xTrade.Status in [ST_CLOSE_SL, ST_CLOSE_TP] then
      begin
        FPosiotion := 0;

        xTrade.Close_IndexInc := AIndexInc;
        xTrade.Close_Date := ADate;
        xTrade.Close_Time := ATime;
        xTrade.Close_Potential := APotential;

        FItems.Delete(i);
        FTradeClose.Add(xTrade);
      end;
    end;
end;

{ TTrades.TTrade }

function TTrades.TTrade.GetProfit: Double;
begin

  Result := 0;
  case BuySell of
    'B': begin
      if Status = TTrades.ST_CLOSE_TP then
        Result := Quantity * (TP - Price)
      else
        Result := Quantity * (SL - Price)
    end;
    'S': begin
      if Status = TTrades.ST_CLOSE_TP then
        Result := Quantity * (Price - TP)
      else
        Result := Quantity * (Price - SL)
    end;
  end;
end;

end.
