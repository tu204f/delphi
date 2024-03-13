unit Lb.Indicator;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline;

type
  ///<summary>Базовый класс</summary>
  TCustomIndicator = class(TObject)
  private
    FPeriod: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetCandels(ACandelObjects: TCandelObjectList); virtual;
    property Period: Integer read FPeriod write FPeriod;
  end;

  ///<summary>Производим расчет осциллятор RSI</summary>
  TRSI = class(TCustomIndicator)
  private
    FGainValues: TValueList;
    FLosValues: TValueList;
    FAvgGainValues: TValueList;
    FAvgLosValues: TValueList;
    FRS: TValueList;
    FRSI: TValueList;
    FAvgRSI: TValueList;
  private
    FAvgPeriod: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetCandels(ACandelObjects: TCandelObjectList); override;
    property AvgPeriod: Integer read FAvgPeriod write FAvgPeriod;
    property GainValues: TValueList read FGainValues;
    property LosValues: TValueList read FLosValues;
    property AvgGainValues: TValueList read FAvgGainValues;
    property AvgLosValues: TValueList read FAvgLosValues;
    property RS: TValueList read FRS;
    property Values: TValueList read FRSI;
    property AvgValues: TValueList read FAvgRSI;
  end;

  ///<summary>средний дневной диапазон</summary>
  TADR = class(TCustomIndicator)
  private
    FValues: TValueList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetCandels(ACandelObjects: TCandelObjectList); override;
    property Values: TValueList read FValues;
  end;

  ///<summary>Индекс Относительной Бодрости</summary>
  TRVI = class(TCustomIndicator)
  private
    FValues: TValueList;
    FK: TValueList;
    FD: TValueList;
  private
    FPeriodSignal: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetCandels(ACandelObjects: TCandelObjectList); override;
    property PeriodSignal: Integer read FPeriodSignal write FPeriodSignal;
    property K: TValueList read FK;
    property D: TValueList read FD;
  end;

  ///<summary>Стохастический осциллятор</summary>
  TStochastic = class(TCustomIndicator)
  private
    FValues: TValueList;
    FK: TValueList;
    FD: TValueList;
  private
    FPeriodK: Integer;
    FPeriodD: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetCandels(ACandelObjects: TCandelObjectList); override;
    property K: TValueList read FK;
    property D: TValueList read FD;
    property PeriodK: Integer read FPeriodK write FPeriodK;
    property PeriodD: Integer read FPeriodD write FPeriodD;
  end;

implementation

function GetRound(const AValue: Double): Double;
begin
  Result := Round(100 * AValue)/100;
end;

function AvgValue(AIndex, APeriod: Integer; AValues: TValueList): Double;
var
  xSum: Double;
  i: Integer;
begin
  Result := 0;
  if APeriod > 0 then
  begin
    xSum := 0;
    for i := 0 to APeriod - 1 do
      xSum := xSum + AValues[AIndex + i];
    Result := xSum/APeriod;
  end;
end;

procedure AvgExpValue(AIndex, APeriod: Integer; AValues, AvgValues: TValueList);
var
  xEndIndex: Integer;
begin
  if APeriod > 0 then
  begin
    xEndIndex := AIndex + APeriod;
    if xEndIndex > (AValues.Count - 1) then
    begin
      AvgValues.Items[AIndex] := 0;
    end
    else if (xEndIndex = (AValues.Count - 1)) then
    begin
      var xValue := AvgValue(AIndex,APeriod,AValues);
      AvgValues.Items[AIndex] := GetRound(xValue);
    end
    else if (xEndIndex < (AValues.Count - 1)) then
    begin
      var xValue     := AvgValues.Items[AIndex + 1];
      var xCurAValue := AValues.Items[AIndex];
      var xAvgValue  := (xValue * (APeriod - 1) + xCurAValue)/APeriod;
      AvgValues.Items[AIndex] := GetRound(xAvgValue);
    end;
  end;
end;

procedure Avg(APeriod: Integer; AValues, AvgValues: TValueList);
var
  i, iCount: Integer;
begin
  iCount := AValues.Count;
  if iCount > 0 then
    for i := iCount - 1 downto 0 do
      AvgExpValue(i, APeriod, AValues, AvgValues);
end;

{ TCustomIndicator }

constructor TCustomIndicator.Create;
begin

end;

destructor TCustomIndicator.Destroy;
begin

  inherited;
end;

procedure TCustomIndicator.SetCandels(ACandelObjects: TCandelObjectList);
begin
  if FPeriod = 0 then
    raise Exception.Create('Error Message: Период не может принимать нулевое значение');
end;

{ TRSI }

constructor TRSI.Create;
begin
  inherited Create;
  FPeriod       := 14;
  FAvgPeriod    := 3;
  FGainValues   := TValueList.Create;
  FLosValues    := TValueList.Create;
  FAvgGainValues:= TValueList.Create;
  FAvgLosValues := TValueList.Create;
  FRS           := TValueList.Create;
  FRSI          := TValueList.Create;
  FAvgRSI       := TValueList.Create;
end;

destructor TRSI.Destroy;
begin
  FreeAndNil(FAvgRSI);
  FreeAndNil(FRSI);
  FreeAndNil(FRS);
  FreeAndNil(FAvgLosValues);
  FreeAndNil(FAvgGainValues);
  FreeAndNil(FLosValues);
  FreeAndNil(FGainValues);
  inherited;
end;

procedure TRSI.SetCandels(ACandelObjects: TCandelObjectList);

  procedure _GainLos;
  var
    xValue: Double;
    xCandel_1, xCandel_2: TCandelObject;
    i, iCount: Integer;
  begin
    iCount := ACandelObjects.Count;
    if iCount > 0 then
    begin
      for i := 0 to iCount - 2 do
      begin
        xCandel_1 := ACandelObjects[i];
        xCandel_2 := ACandelObjects[i + 1];
        // --
        if xCandel_1.Close > xCandel_2.Close then
        begin
          xValue := xCandel_1.Close -  xCandel_2.Close;
          FGainValues.Add(xValue);
          FLosValues.Add(0);
        end
        else
        begin
          xValue := xCandel_2.Close -  xCandel_1.Close;
          FGainValues.Add(0);
          FLosValues.Add(xValue);
        end;
        // --
        FAvgGainValues.Add(0);
        FAvgLosValues.Add(0);
        FRS.Add(0);
        FRSI.Add(0);
        FAvgRSI.Add(0);
      end;
      FGainValues.Add(0);
      FLosValues.Add(0);
      FAvgGainValues.Add(0);
      FAvgLosValues.Add(0);
      FRS.Add(0);
      FRSI.Add(0);
      FAvgRSI.Add(0);
    end;
  end;



  procedure _RS;
  var
    xG, xL: Double;
    i, iCount: Integer;
  begin
    iCount := FAvgGainValues.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xG := FAvgGainValues[i];
        xL := FAvgLosValues[i];
        if xL = 0 then
          FRS.Items[i] := 0
        else
          FRS.Items[i] := xG/xL;
      end;
  end;

  procedure _RSI;
  var
    xV: Double;
    i, iCount: Integer;
  begin
    iCount := FRS.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xV := FRS.Items[i];
        FRSI.Items[i] := GetRound(100 - 100/(1 + xV));
      end;
  end;

begin
  inherited;
  
  if FAvgPeriod = 0 then
    raise Exception.Create('Error Message: Период сглаживания не может принимать нулевое значения');
  
  FAvgRSI.Clear;
  FRSI.Clear;
  FRS.Clear;
  FAvgLosValues.Clear;
  FAvgGainValues.Clear;
  FLosValues.Clear;
  FGainValues.Clear;

  _GainLos;
  Avg(FPeriod, FGainValues,FAvgGainValues);
  Avg(FPeriod, FLosValues, FAvgLosValues);
  _RS;
  _RSI;
  Avg(FAvgPeriod,FRSI,FAvgRSI);
end;

{ TADR }

constructor TADR.Create;
begin
  inherited;
  FPeriod := 5;
  FValues := TValueList.Create;
end;

destructor TADR.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

procedure TADR.SetCandels(ACandelObjects: TCandelObjectList);
var
  xCandel: TCandelObject;
  i, j, iCount: Integer;
begin
  inherited;
  FValues.Clear;
  iCount := ACandelObjects.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      if (i + FPeriod) >= iCount then
      begin
        FValues.Add(0);
      end
      else
      begin
        var xSum := 0.0;
        for j := 0 to FPeriod - 1 do
        begin
          xCandel := ACandelObjects[i + j];
          xSum := xSum + (xCandel.High - xCandel.Low) 
        end;
        FValues.Add(GetRound(xSum/FPeriod));
      end;
    end;
end;

{ TRVI }

constructor TRVI.Create;
begin
  inherited;
  FValues := TValueList.Create;
  FK := TValueList.Create;
  FD := TValueList.Create;
  FPeriod := 14;
  FPeriodSignal := 3;
end;

destructor TRVI.Destroy;
begin
  FreeAndNil(FD);
  FreeAndNil(FK);
  FreeAndNil(FValues);
  inherited;
end;

procedure TRVI.SetCandels(ACandelObjects: TCandelObjectList);
var
  xCandel: TCandelObject;
  i, iCount: Integer;
begin
  inherited;
  FValues.Clear;
  iCount := ACandelObjects.Count;
  if iCount > 0 then
  begin
    for i := 0 to iCount - 1 do
    begin
      xCandel := ACandelObjects.Items[i];
      var xValue := (xCandel.Close - xCandel.Open)/(xCandel.High - xCandel.Low);
      FValues.Add(xValue);
      FK.Add(0);
      FD.Add(0);
    end;
  end;
  Avg(FPeriod,FValues,FK);
  Avg(FPeriodSignal,FK,FD);
end;

{ TStochastic }

constructor TStochastic.Create;
begin
  inherited;
  FPeriod  := 14;
  FPeriodK := 1;
  FPeriodD := 3;
  FValues := TValueList.Create;
  FK := TValueList.Create;
  FD := TValueList.Create;
end;

destructor TStochastic.Destroy;
begin
  FreeAndNil(FD);
  FreeAndNil(FK);
  FreeAndNil(FValues);
  inherited;
end;

procedure TStochastic.SetCandels(ACandelObjects: TCandelObjectList);

  procedure _MaxAndMinValue(const AIndex: Integer; var AMax, AMin, AValue: Double);
  var
    xInd: Integer;
    xCandel: TCandelObject;
  begin
    xCandel := ACandelObjects.Items[AIndex];
    AMax := xCandel.High;
    AMin := xCandel.Low;
    AValue := xCandel.Close;
    for var i := 0 to FPeriod - 1 do
    begin
      xInd := AIndex + i;
      if (xInd >= 0) and (xInd < ACandelObjects.Count) then
      begin
        xCandel := ACandelObjects.Items[xInd];
        if xCandel.High > AMax then
          AMax := xCandel.High;
        if xCandel.Low < AMin then
          AMin := xCandel.Low;
      end
      else
        Break;
    end;
  end;

var
  i, iCount: Integer;
  xMax, xMin, xValue, xStoh: Double;
begin
  inherited;
  FValues.Clear;
  iCount := ACandelObjects.Count;
  if iCount > 0 then
  begin
    for i := 0 to iCount - 1 do
    begin
      _MaxAndMinValue(i,xMax, xMin, xValue);
      xStoh := GetRound(100 * (xValue - xMin)/(xMax - xMin));
      FValues.Add(xStoh);
      FK.Add(0);
      FD.Add(0);
    end;
  end;
  Avg(FPeriodK, FValues, FK);
  Avg(FPeriodD, FK, FD);
end;

end.
