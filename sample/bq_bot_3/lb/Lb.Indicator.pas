unit Lb.Indicator;

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
  ///<summary>
  /// Базовый индикатор
  ///</summary>
  TCustomIndicator = class(TObject)
  private
    FCandels: TCandelList;
  public
    procedure SetCandels(ACandels: TCandelList); virtual; abstract;
  end;

  {todo: Не правильно точнее, не коректно расчивается}
  ///<summary>Объект для расчета значение RSI</summary>
  TValueRSI = class(TCustomIndicator)
  private
    FValuesU: TDoubleList;
    FValuesD: TDoubleList;
    FValueMaU: TDoubleList;
    FValueMaD: TDoubleList;
    FValueRSI: TDoubleList;
    FValueMaRSI: TDoubleList;
    FValueMa2RSI: TDoubleList;
  public
    FPeriodRSI: Integer;
    FPeriodSmoothingRSI: Integer;
    FPeriodTrendRSI: Integer;
    function GetRSI: Double;
    function GetSmoothingRSI: Double;
    function GetTrendRSI: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetCandels(ACandels: TCandelList); override;
    property ValueRSI: TDoubleList read FValueRSI;
    property ValueMaRSI: TDoubleList read FValueMaRSI;
    property ValueMa2RSI: TDoubleList read FValueMa2RSI;
    ///<summary>
    /// Период для расчета RSI
    ///</summary>
    property PeriodRSI: Integer read FPeriodRSI write FPeriodRSI;
    ///<summary>
    /// Период сглаживания
    ///</summary>
    property PeriodSmoothingRSI: Integer read FPeriodSmoothingRSI write FPeriodSmoothingRSI;
    ///<summary>
    /// Период сглаживания
    ///</summary>
    property PeriodTrendRSI: Integer read FPeriodTrendRSI write FPeriodTrendRSI;
  public
    property RSI: Double read GetRSI;
    property SmoothingRSI: Double read GetSmoothingRSI;
    property TrendRSI: Double read GetTrendRSI;
  end;

  ///<summary>Процентный коридор Вильемса<summary>
  TValuesW = class(TCustomIndicator)
  private
    FValuesWR: TDoubleList;
    FValuesMA1: TDoubleList;
    FValuesMA2: TDoubleList;
    FPeriodWR: Integer;
    FPeriodMA1: Integer;
    FPeriodMA2: Integer;
    function GetValueWR: Double;
    function GetValueMA1: Double;
    function GetValueMA2: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetCandels(ACandels: TCandelList); override;

    property ValuesWR: TDoubleList read FValuesWR;
    property ValuesMA1: TDoubleList read FValuesMA1;
    property ValuesMA2: TDoubleList read FValuesMA2;

    property PeriodWR: Integer read FPeriodWR write FPeriodWR;
    property PeriodMA1: Integer read FPeriodMA1 write FPeriodMA1;
    property PeriodMA2: Integer read FPeriodMA2 write FPeriodMA2;
  public
    property ValueWR: Double read GetValueWR;
    property FastValueMa: Double read GetValueMA1;
    property SlowValueMa: Double read GetValueMA2;
  end;

  TValueATR = class(TCustomIndicator)
  private
    FValueTR: TDoubleList;
    FValues: TDoubleList;
    FPeriod: Integer;
    function GetATR: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetCandels(ACandels: TCandelList); override;
    property ValueTR: TDoubleList read FValueTR;
    property Values: TDoubleList read FValues;
    property Period: Integer read FPeriod write FPeriod;
  public
    property ATR: Double read GetATR;
  end;

  TValueMomentum = class(TCustomIndicator)
  private
    FValues: TDoubleList;
    FValuesMA: TDoubleList;
    FPeriod: Integer;
    FPeriodMA: Integer;
    function GetMomentum: Double;
    function GetMomentumMA: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetCandels(ACandels: TCandelList); override;
    property Period: Integer read FPeriod write FPeriod;
    property Values: TDoubleList read FValues;
    property ValuesMA: TDoubleList read FValuesMA;
  public
    property Momentum: Double read GetMomentum;
    property MomentumMA: Double read GetMomentumMA;
  end;

  ///<summary>
  /// Вычисляем валатильность инструмента
  ///</summary>
  TValueVolatility = class(TCustomIndicator)
  private
    FDeviationValue: Double;
    FDeviationValueQuard: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetCandels(ACandels: TCandelList); override;
    property DeviationValue: Double read FDeviationValue;
    property DeviationValueQuard: Double read FDeviationValueQuard;
  end;


function GetSMA(const AValue: TDoubleList): Double;
function GetATR(const APeriod: Integer; ACandels: TCandelList): Double;
function GetRSI(const APeriod: Integer; ACandels: TCandelList): Double;
procedure SetAveragRSI(const APeriodRSI, APeriodSMA: Integer; const ACandels: TCandelList; var AValueRSI, AValueAveragRSI: Double);

implementation

uses
{$IFDEF DEBUG}
  Lb.Logger,
{$ENDIF}
  System.Math;

function GetMovingAverag(const AValue: TDoubleList): Double;
var
  xSum: Double;
  i, iCount: Integer;
begin
  Result := 0;
  iCount := AValue.Count;
  if iCount > 0 then
  begin
    xSum := 0;
    for i := 0 to iCount - 1 do
      xSum := xSum + AValue[i];
    Result := xSum/iCount;
  end;
end;

function GetValueSMA(const APeriod, AIndex: Integer; const AValues: TDoubleList): Double;
var
  xValue: Double;
  xValues: TDoubleList;
  i, Count, xInd: Integer;
begin
  Count := AValues.Count;
  if Count > 0 then
  begin
    xValues := TDoubleList.Create;
    try
      for i := 0 to APeriod - 1 do
      begin
        xInd := i + AIndex;
        if (xInd >= 0) and (xInd < AValues.Count) then
        begin
          xValue := AValues[xInd];
          xValues.Add(xValue);
        end
        else
          Break;
      end;
      xValue := GetMovingAverag(xValues);
      xValue := GetRound(xValue);
      Result := xValue;
    finally
      FreeAndNil(xValues);
    end;
  end
  else
    Result := 0;
end;

(******************************************************************************)
(* Процедуры которые помогают оценить состояние рынка                         *)

function GetSMA(const AValue: TDoubleList): Double;
var
  xSum: Double;
  i, iCount: Integer;
begin
  Result := 0;
  iCount := AValue.Count;
  if iCount > 0 then
  begin
    xSum := 0;
    for i := 0 to iCount - 1 do
      xSum := xSum + AValue[i];
    Result := xSum/iCount;
  end;
end;

///<summary>Определяем валатиность рынка</summary>
function GetATR(const APeriod: Integer; ACandels: TCandelList): Double;

  function _MAX(const AValue1, AValue2, AValue3: Double): Double;
  var
    xValue: Double;
  begin
    xValue := AValue1;
    if xValue < AValue2 then
      xValue := AValue2;
    if xValue < AValue3 then
      xValue := AValue3;
    Result := xValue;
  end;

var
  xDelta: Double;
  xCandel1, xCandel2: TCandel;
  xTR: TDoubleList;
begin
  Result := 0;
  if APeriod > ACandels.Count then
    Exit;

  if APeriod > 0 then
  begin
    xTR := TDoubleList.Create;
    try
      for var i := 0 to APeriod - 1 do
      begin
        if i > 0 then
        begin
          xCandel1 := ACandels[i - 1];
          xCandel2 := ACandels[i];
          xDelta := _MAX(
            xCandel2.High - xCandel2.Low,
            xCandel2.High - xCandel1.Close,
            xCandel1.Close - xCandel2.Low
          );
          xTR.Add(xDelta);
        end
        else
        begin
          xCandel1 := ACandels[i];
          xDelta := xCandel1.High - xCandel1.Low;
          xTR.Add(xDelta);
        end;
      end;
      Result := GetSMA(xTR);
    finally
      FreeAndNil(xTR);
    end;
  end;
end;

///<summary>Расчет индикатора</summary>
function GetRSI(const APeriod: Integer; ACandels: TCandelList): Double;
var
  xDelta: Double;
  xCandel1, xCandel2: TCandel;
  xU, xD: TDoubleList;
  xMaU, xMaD, xRS: Double;
begin
  Result := 0;
  if APeriod >= ACandels.Count then
    Exit;

  if APeriod > 0 then
  begin
    xU := TDoubleList.Create;
    xD := TDoubleList.Create;
    try
      for var i := 1 to APeriod do
      begin
        xCandel1 := ACandels[i - 1];
        xCandel2 := ACandels[i];
        xDelta := xCandel1.Close - xCandel2.Close;
        if xDelta > 0 then
        begin
          xU.Add(xDelta);
          xD.Add(0);
        end
        else
        begin
          xU.Add(0);
          xD.Add(Abs(xDelta));
        end;
      end;
      xMaU := GetSMA(xU);
      xMaD := GetSMA(xD);
      xRS  := xMaU/xMaD;
      Result := 100 - 100/(1 + xRS);
    finally
      FreeAndNil(xD);
      FreeAndNil(xU);
    end;
  end;
end;

procedure SetAveragRSI(const APeriodRSI, APeriodSMA: Integer; const ACandels: TCandelList; var AValueRSI, AValueAveragRSI: Double);
var
  xCandels: TCandelList;
  i, j: Integer;
  xValues: TDoubleList;
begin
  AValueRSI := 0;
  AValueAveragRSI := 0;

  xCandels := TCandelList.Create;
  xValues  := TDoubleList.Create;
  try
    if ACandels.Count >= (APeriodRSI + APeriodSMA) then
    begin
      for j := 0 to APeriodSMA - 1 do
      begin
        xCandels.Clear;
        for i := 0 to APeriodRSI do
        begin
          var xCandel := ACandels[j + i];
          xCandels.Add(xCandel);
        end;
        var xValueRSI := GetRSI(APeriodRSI,xCandels);
        xValues.Add(xValueRSI);
      end;
      AValueRSI := GetRound(xValues[0]);
      AValueAveragRSI := GetRound(GetSMA(xValues));
    end;
  finally
    FreeAndNil(xValues);
    FreeAndNil(xCandels);
  end;
end;

{ TValueRSI }

constructor TValueRSI.Create;
begin
  FCandels := nil;

  FPeriodRSI := 14;
  FPeriodSmoothingRSI := 3;
  FPeriodTrendRSI := 14;

  FValuesU := TDoubleList.Create;
  FValuesD := TDoubleList.Create;
  FValueMaU := TDoubleList.Create;
  FValueMaD := TDoubleList.Create;
  FValueRSI := TDoubleList.Create;
  FValueMaRSI := TDoubleList.Create;
  FValueMa2RSI := TDoubleList.Create;
end;

destructor TValueRSI.Destroy;
begin
  FreeAndNil(FValueMa2RSI);
  FreeAndNil(FValueMaRSI);
  FreeAndNil(FValueRSI);
  FreeAndNil(FValueMaD);
  FreeAndNil(FValueMaU);
  FreeAndNil(FValuesD);
  FreeAndNil(FValuesU);
  inherited;
end;

procedure TValueRSI.SetCandels(ACandels: TCandelList);

  procedure _ValueUD;
  var
    xDelta: Double;
    i, Count: Integer;
    xCandel: TCandel;
  begin
    FValuesU.Clear;
    FValuesD.Clear;
    Count := FCandels.Count;
    for i := 0 to Count - 1 do
    begin
      xCandel := FCandels[i];
      xDelta := xCandel.Open - xCandel.Close;
      if xDelta > 0 then
      begin
        FValuesU.Add(xDelta);
        FValuesD.Add(0);
      end
      else
      begin
        FValuesU.Add(0);
        FValuesD.Add(-1 * xDelta);
      end;
    end;
  end;

  procedure _ValueMovingAverag;
  var
    i, Count: Integer;
  begin
    FValueMaU.Clear;
    FValueMaD.Clear;
    Count := FCandels.Count;
    if Count > 0 then
    begin
      for i := 0 to Count - 1 do
      begin
        FValueMaU.Add(GetValueSMA(FPeriodSmoothingRSI,i,FValuesU));
        FValueMaD.Add(GetValueSMA(FPeriodSmoothingRSI,i,FValuesD));
      end;
    end;
  end;

  procedure _ValueRSI;
  var
    i, Count: Integer;
    xMaU, xMaD, xRS: Double;
  begin
    FValueRSI.Clear;
    Count := FCandels.Count;
    if Count > 0 then
    begin
      for i := 0 to Count - 1 do
      begin
        xMaU := FValueMaU[i];
        xMaD := FValueMaD[i];
        if (xMaU > 0) and (xMaD > 0) then
          xRS := xMaU/xMaD
        else
          xRS := 0;
        xRS := 100 - 100/(1 + xRS);
        xRS := GetRound(xRS);
        FValueRSI.Add(xRS);
      end;
    end;
  end;

  procedure _ValueSmoothingRSI;
  var
    i, Count: Integer;
  begin
    FValueMaRSI.Clear;
    Count := FCandels.Count;
    if Count > 0 then
      for i := 0 to Count - 1 do
        FValueMaRSI.Add(GetValueSMA(FPeriodSmoothingRSI,i,FValueRSI));
  end;

  procedure _ValueTrandRSI;
  var
    i, Count: Integer;
  begin
    FValueMa2RSI.Clear;
    Count := FCandels.Count;
    if Count > 0 then
      for i := 0 to Count - 1 do
        FValueMa2RSI.Add(GetValueSMA(FPeriodTrendRSI,i,FValueMaRSI));
  end;

begin
  FCandels := ACandels;
  if Assigned(FCandels) then
  begin
    _ValueUD;
    _ValueMovingAverag;
    _ValueRSI;
    _ValueSmoothingRSI;
    _ValueTrandRSI;
  end;
end;

function TValueRSI.GetRSI: Double;
begin
  Result := 0;
  if FValueRSI.Count > 0 then
    Result := GetRound(FValueRSI[0]);
end;

function TValueRSI.GetSmoothingRSI: Double;
begin
  Result := 0;
  if FValueMaRSI.Count > 0 then
    Result := GetRound(FValueMaRSI[0]);
end;

function TValueRSI.GetTrendRSI: Double;
begin
  Result := 0;
  if FValueMa2RSI.Count > 0 then
    Result := GetRound(FValueMa2RSI[0]);
end;

{ TValuesW }

constructor TValuesW.Create;
begin
  FValuesWR := TDoubleList.Create;
  FPeriodWR := 14;

  FValuesMA1 := TDoubleList.Create;
  FPeriodMA1 := 9;

  FValuesMA2 := TDoubleList.Create;
  FPeriodMA2 := 26;
end;

destructor TValuesW.Destroy;
begin
  FreeAndNil(FValuesMA2);
  FreeAndNil(FValuesMA1);
  FreeAndNil(FValuesWR);
  inherited;
end;


procedure TValuesW.SetCandels(ACandels: TCandelList);

  procedure _SetValueMaxAndMinPrice(const AIndex: Integer; var AValueMax, AValueMin: Double);
  var
    xCandel: TCandel;
    i, xIndex: Integer;
  begin
    xCandel := FCandels[AIndex];
    AValueMax := xCandel.High;
    AValueMin := xCandel.Low;
    for i := 0 to FPeriodWR - 1 do
    begin
      xIndex := AIndex + i;
      if (xIndex >= 0) and (xIndex < FCandels.Count) then
      begin
        xCandel := FCandels[xIndex];
        if AValueMax < xCandel.High then
          AValueMax := xCandel.High;
        if AValueMin > xCandel.Low then
          AValueMin := xCandel.Low;
      end;
    end;
  end;

  procedure _SetValuesWR;
  var
    xValue: Double;
    xCandel: TCandel;
    i, iCount: Integer;
    xValueMax, xValueMin: Double;
  begin
    iCount := FCandels.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xCandel := FCandels[i];
        _SetValueMaxAndMinPrice(i,xValueMax,xValueMin);
        if (xValueMax > xValueMin) and (xValueMax > 0) and (xValueMin > 0) then
          xValue := (xCandel.Close - xValueMin)/(xValueMax - xValueMin)
        else
          xValue := 0;
        xValue := 100 * GetRound(xValue);
        FValuesWR.Add(xValue);
      end;
  end;

  procedure _ValuesMA(ASource: TDoubleList; ADest: TDoubleList; APeriod: Integer);
  var
    i, Count: Integer;
  begin
    ADest.Clear;
    Count := FCandels.Count;
    if Count > 0 then
      for i := 0 to Count - 1 do
        ADest.Add(GetValueSMA(APeriod,i,ASource));
  end;

begin
  FCandels := ACandels;
  FValuesWR.Clear;
  _SetValuesWR;
  _ValuesMA(FValuesWR,FValuesMA1,FPeriodMA1);
  _ValuesMA(FValuesMA1,FValuesMA2,FPeriodMA2);
end;

function TValuesW.GetValueWR: Double;
begin
  Result := 0;
  if FValuesWR.Count > 0 then
    Result := FValuesWR[0];
end;

function TValuesW.GetValueMA1: Double;
begin
  Result := 0;
  if FValuesMA1.Count > 0 then
    Result := FValuesMA1[0];
end;

function TValuesW.GetValueMA2: Double;
begin
  Result := 0;
  if FValuesMA2.Count > 0 then
    Result := FValuesMA2[0];
end;

{ TValueATR }

constructor TValueATR.Create;
begin
  FCandels := nil;

  FPeriod := 14;

  FValueTR := TDoubleList.Create;
  FValues := TDoubleList.Create;
end;

destructor TValueATR.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FValueTR);
  inherited;
end;

procedure TValueATR.SetCandels(ACandels: TCandelList);

  function _MAX(const AValue1, AValue2, AValue3: Double): Double;
  var
    xValue: Double;
  begin
    xValue := AValue1;
    if xValue < AValue2 then
      xValue := AValue2;
    if xValue < AValue3 then
      xValue := AValue3;
    Result := xValue;
  end;

  procedure _ValueTR;
  var
    xTR: Double;
    i, Count: Integer;
    xCandel1, xCandel2: TCandel;
  begin
    FValueTR.Clear;
    Count := FCandels.Count;
    if Count > 0 then
    begin
      for i := 0 to Count - 2 do
      begin
        xCandel1 := ACandels[i];
        xCandel2 := ACandels[i + 1];
        xTR := _MAX(
          xCandel2.High - xCandel2.Low,
          xCandel2.High - xCandel1.Close,
          xCandel1.Close - xCandel2.Low
        );
        FValueTR.Add(xTR);
      end;
      FValueTR.Add(0);
    end;
  end;

  procedure _ValueMovingAverag;
  var
    i, Count: Integer;
  begin
    FValues.Clear;
    Count := FCandels.Count;
    if Count > 0 then
      for i := 0 to Count - 1 do
        FValues.Add(GetValueSMA(FPeriod,i,FValueTR))
  end;

begin
  FCandels := ACandels;
  if Assigned(FCandels) then
  begin
    _ValueTR;
    _ValueMovingAverag;
  end;
end;

function TValueATR.GetATR: Double;
begin
  Result := 0;
  if FValues.Count > 0 then
    Result := GetRound(FValues[0]);
end;


{ TValueMomentum }

constructor TValueMomentum.Create;
begin
  FPeriod := 14;
  FPeriodMA := 3;
  FValues := TDoubleList.Create;
  FValuesMA := TDoubleList.Create;
end;

destructor TValueMomentum.Destroy;
begin
  FreeAndNil(FValuesMA);
  FreeAndNil(FValues);
  inherited;
end;

procedure TValueMomentum.SetCandels(ACandels: TCandelList);

  procedure _Momentum;
  var
    xValue: Double;
    xC1, xC2: TCandel;
    i, iCount, xIndex: Integer;
  begin
    FValues.Clear;
    iCount := FCandels.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xIndex := i + (FPeriod - 1);
        if xIndex < (iCount - 1) then
        begin
          xC1 := FCandels[i];
          xC2 := FCandels[xIndex];
          xValue := GetRound(xC1.Close - xC2.Close);
          FValues.Add(xValue);
        end
        else
          FValues.Add(0);
      end;
  end;

  procedure _ValueMovingAverag;
  var
    i, Count: Integer;
  begin
    FValuesMA.Clear;
    Count := FCandels.Count;
    if Count > 0 then
      for i := 0 to Count - 1 do
        FValuesMA.Add(GetValueSMA(FPeriodMA,i,FValues))
  end;

begin
  FCandels := ACandels;
  if Assigned(FCandels) then
  begin
    _Momentum;
    _ValueMovingAverag;
  end;
end;

function TValueMomentum.GetMomentum: Double;
begin
  Result := 0;
  if FCandels.Count > 0 then
    Result := FValues[0];
end;

function TValueMomentum.GetMomentumMA: Double;
begin
  Result := 0;
  if FCandels.Count > 0 then
    Result := FValuesMA[0];
end;


{ TValueVolatility }

constructor TValueVolatility.Create;
begin
  FDeviationValue := 0;
  FDeviationValueQuard := 0;
end;

destructor TValueVolatility.Destroy;
begin

  inherited;
end;

procedure TValueVolatility.SetCandels(ACandels: TCandelList);

  procedure _Deviation;
  var
    xDelta: Double;
    xSum, xSumQuard: Double;
    xCandel: TCandel;
    i, iCount: Integer;
  begin
    xSum := 0;
    xSumQuard := 0;
    iCount := FCandels.Count;
    if iCount > 0 then
    begin
      for i := 0 to iCount - 1 do
      begin
        xCandel := FCandels[i];
        xDelta := xCandel.High - xCandel.Low;
        xSum := xSum + xDelta;
        xSumQuard := xSumQuard + Power(xDelta,2);
      end;
      FDeviationValue := GetRound(xSum/iCount);
      FDeviationValueQuard := GetRound(xSumQuard/(iCount * (iCount - 1)));
    end;
  end;

begin
  FCandels := ACandels;
  _Deviation;
end;

end.
