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

  ///<summary>Объект для расчета значение RSI</summary>
  TValueRSI = class(TCustomIndicator)
  private
    FValuesU: TDoubleList;
    FValuesD: TDoubleList;
    FValueMaU: TDoubleList;
    FValueMaD: TDoubleList;
    FValueRSI: TDoubleList;
    FValueMaRSI: TDoubleList;
    FValueTR: TDoubleList;
    FValueATR: TDoubleList;
    FPeriod: Integer;
    FPeriodMovingAverag: Integer;
    FPeriodATR: Integer;
    function GetRSI: Double;
    function GetMovingAveragRSI: Double;
    function GetATR: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetCandels(ACandels: TCandelList); override;
    property ValueRSI: TDoubleList read FValueRSI;
    property ValueMaRSI: TDoubleList read FValueMaRSI;
    property ValueATR: TDoubleList read FValueATR;
    property Period: Integer read FPeriod write FPeriod;
    property PeriodMovingAverag: Integer read FPeriodMovingAverag write FPeriodMovingAverag;
    property PeriodATR: Integer read FPeriodATR write FPeriodATR;
  public
    property RSI: Double read GetRSI;
    property MovingAveragRSI: Double read GetMovingAveragRSI;
    property ATR: Double read GetATR;
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



function GetSMA(const AValue: TDoubleList): Double;
function GetATR(const APeriod: Integer; ACandels: TCandelList): Double;
function GetRSI(const APeriod: Integer; ACandels: TCandelList): Double;
procedure SetAveragRSI(const APeriodRSI, APeriodSMA: Integer; const ACandels: TCandelList; var AValueRSI, AValueAveragRSI: Double);

implementation

{$IFDEF DEBUG}
uses
  Lb.Logger;
{$ENDIF}

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

  FPeriod := 14;
  FPeriodMovingAverag := 9;
  FPeriodATR := 9;

  FValuesU := TDoubleList.Create;
  FValuesD := TDoubleList.Create;
  FValueMaU := TDoubleList.Create;
  FValueMaD := TDoubleList.Create;
  FValueRSI := TDoubleList.Create;
  FValueMaRSI := TDoubleList.Create;
  FValueTR := TDoubleList.Create;
  FValueATR := TDoubleList.Create;
end;

destructor TValueRSI.Destroy;
begin
  FreeAndNil(FValueATR);
  FreeAndNil(FValueTR);
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
    xCandel1, xCandel2: TCandel;
  begin
    FValuesU.Clear;
    FValuesD.Clear;
    Count := FCandels.Count;
    for i := 0 to Count - 2 do
    begin
      xCandel1 := FCandels[i];
      xCandel2 := FCandels[i + 1];
      xDelta := xCandel1.Close - xCandel2.Close;
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
    FValuesU.Add(0);
    FValuesD.Add(0);
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
        FValueMaU.Add(GetValueSMA(FPeriod,i,FValuesU));
        FValueMaD.Add(GetValueSMA(FPeriod,i,FValuesD));
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

  procedure _ValueMovingAveragRSI;
  var
    i, Count: Integer;
  begin
    FValueMaRSI.Clear;
    Count := FCandels.Count;
    if Count > 0 then
      for i := 0 to Count - 1 do
        FValueMaRSI.Add(GetValueSMA(FPeriodMovingAverag,i,FValueRSI));
  end;

  procedure _ValueTR;
  var
    xTR: Double;
    i, Count: Integer;
  begin
    FValueTR.Clear;
    Count := FCandels.Count;
    if Count > 0 then
    begin
      for i := 1 to Count - 1 do
      begin
        xTR := FValueRSI[i - 1] - FValueRSI[i];
        FValueTR.Add(xTR);
      end;
      FValueTR.Add(0);
    end;
  end;

  procedure _ValueMovingAveragRSI_ART;
  var
    i, Count: Integer;
  begin
    FValueATR.Clear;
    Count := FCandels.Count;
    if Count > 0 then
      for i := 0 to Count - 1 do
        FValueATR.Add(GetValueSMA(FPeriodATR,i,FValueTR));
  end;

begin
  FCandels := ACandels;
  if Assigned(FCandels) then
  begin
    _ValueUD;
    _ValueMovingAverag;
    _ValueRSI;
    _ValueMovingAveragRSI;
    _ValueTR;
    _ValueMovingAveragRSI_ART;
  end;
end;

function TValueRSI.GetRSI: Double;
begin
  Result := 0;
  if FValueRSI.Count > 0 then
    Result := GetRound(FValueRSI[0]);
end;

function TValueRSI.GetMovingAveragRSI: Double;
begin
  Result := 0;
  if FValueMaRSI.Count > 0 then
    Result := GetRound(FValueMaRSI[0]);
end;

function TValueRSI.GetATR: Double;
begin
  Result := 0;
  if FValueMaRSI.Count > 0 then
    Result := GetRound(FValueATR[0]);
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

end.
