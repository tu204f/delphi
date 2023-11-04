unit Lb.Block;

interface

{$IFDEF DEBUG}
//  {$DEFINE DBL}
{$ENDIF}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils.Candel;

type
  TBlock = class;
  TBlockList = TObjectList<TBlock>;

  ///<summary>Базовый объект</summary>
  TBlock = class(TObject)
  public const
    BLOCK_SIZE = 45;
  private
    FCandels: TCandels;
    function GetCandelLast: TCandel;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFileName(const AFileName: String);
    procedure SetParserBlock(const ASources: TStrings);
    property Candels: TCandels read FCandels;
    ///<summary>Последние свеча в блоке</summary>
    property CandelLast: TCandel read GetCandelLast;
  end;

  TTypeDecision = (tdBuy = 0, tdWait, tdSell);

  TBlockParam = class(TBlock)
  public type
    TSP = record
      MaxValue: Double;
      MinValue: Double;
    private
      function GetDelta: Double;
    public
      property Delta: Double read GetDelta;
    end;
  private
    FUneCandels: TCandels;
  private
    FCountB: Integer;
    FCountR: Integer;
    function GetIsDecision: TTypeDecision;
  protected
    MaxValue, MinValue: Double;
    procedure SetUneCandels;
    procedure SetMaxAndMinValue;
    procedure SetMaxAndMinValueLong(var AMaxValue, AMinValue: Double);
    procedure SetMaxAndMinValueShort(var AMaxValue, AMinValue: Double);
  public
    constructor Create; override;
    destructor Destroy; override;
    ///<summary>Соответствуют значение 1/2</summary>
    property IsDecision: TTypeDecision read GetIsDecision;
    property CountB: Integer read FCountB write FCountB;
    property CountR: Integer read FCountR write FCountR;
  public {Приведение в единичные параметры}
    property UneCandels: TCandels read FUneCandels;
  end;


  TUneBlock = class(TBlock)
  private
    FUneCandels: TCandels;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetUneCandels;
    property UneCandels: TCandels read FUneCandels;
  end;


  TDoubleList = TList<Double>;

  ///<summary>В какой кондиции находится сейчас финансовый инструмент</summary>
  ///<remarks>
  /// Расчета индикаторо CCI, для принятие решение
  /// Расчет валатиности рынка, для определение степени риска
  ///</remarks>
  TBlockСondition = class(TBlock)
  private
    FPerionCCI: Integer;
    FTypicalPrices: TDoubleList;
    FTypicalPriceSMA: TDoubleList;
    FTypicalPriceSMA_D: TDoubleList;
  protected
    procedure CalculationTypicalPrice;
    procedure CalculationTypicalPriceSMA;
    procedure CalculationTypicalPriceD;
  public
    constructor Create; override;
    destructor Destroy; override;
    function CCI(const APriod: Integer): Double;
  end;


implementation

uses
{$IFDEF DBL}
  Lb.Logger,
{$ENDIF}
  System.Threading;

{ TBlock }

constructor TBlock.Create;
begin
  FCandels := TCandels.Create;
end;

destructor TBlock.Destroy;
begin
  FreeAndNil(FCandels);
  inherited;
end;

function TBlock.GetCandelLast: TCandel;
var
  xIndex: Integer;
begin
  xIndex := FCandels.Count - 1;
  Result := FCandels.Items[xIndex];
end;

procedure TBlock.LoadFileName(const AFileName: String);
var
  xSources: TStrings;
begin
  xSources := TStringList.Create;
  try
    xSources.LoadFromFile(AFileName);
    SetParserBlock(xSources);
  finally
    FreeAndNil(xSources);
  end;
end;

procedure TBlock.SetParserBlock(const ASources: TStrings);
var
  xCandel: TCandel;
  i, iCount: Integer;
  xS: String;
begin
  FCandels.Clear;
  iCount := ASources.Count;
  if iCount > 0 then
  begin
    for i := 0 to iCount - 1 do
    begin
      xS := ASources[i];
      xCandel.Create(xS);
      FCandels.Add(xCandel);
    end;
  end;
end;

{ TBlockParam.TSP }

function TBlockParam.TSP.GetDelta: Double;
begin
  Result := MaxValue - MinValue;
end;

{ TBlockParam }

constructor TBlockParam.Create;
begin
  inherited;
  FCountR := 5;
  FCountB := 34;
  FUneCandels := TCandels.Create;
end;

destructor TBlockParam.Destroy;
begin
  FreeAndNil(FUneCandels);
  inherited;
end;

procedure TBlockParam.SetMaxAndMinValue;
var
  i: Integer;
  xCandel: TCandel;
begin
  xCandel := FCandels[0];
  MaxValue := xCandel.High;
  MinValue := xCandel.Low;
  for i := 1 to FCountB - 1 do
  begin
    xCandel := FCandels[0];
    if MaxValue < xCandel.High then
      MaxValue := xCandel.High;
    if MinValue > xCandel.Low then
      MinValue := xCandel.Low;
  end;
end;

procedure TBlockParam.SetMaxAndMinValueLong(var AMaxValue, AMinValue: Double);
var
  i, xInd: Integer;
  xCandel: TCandel;
begin
  xInd := FCountB - 1;
  xCandel := FCandels[xInd];
  AMaxValue := xCandel.High;
  AMinValue := MinValue;
  for i := 1 to FCountR - 1 do
  begin
    xCandel := FCandels[xInd + i];
    if AMaxValue < xCandel.High then
      AMaxValue := xCandel.High;
    if AMinValue > xCandel.Low then
      AMinValue := xCandel.Low;
  end;
end;

procedure TBlockParam.SetMaxAndMinValueShort(var AMaxValue, AMinValue: Double);
var
  i, xInd: Integer;
  xCandel: TCandel;
begin
  xInd := FCountB - 1;
  xCandel := FCandels[xInd];
  AMaxValue := MaxValue;
  AMinValue := xCandel.Low;
  for i := 1 to FCountR - 1 do
  begin
    xCandel := FCandels[xInd + i];
    if AMaxValue < xCandel.High then
      AMaxValue := xCandel.High;
    if AMinValue > xCandel.Low then
      AMinValue := xCandel.Low;
  end;
end;


function TBlockParam.GetIsDecision: TTypeDecision;
var
  xDelta: Double;
  xLongSP, xShortSP: TSP;
begin
  SetMaxAndMinValue;
  SetMaxAndMinValueLong(xLongSP.MaxValue,xLongSP.MinValue);
  SetMaxAndMinValueShort(xShortSP.MaxValue,xShortSP.MinValue);
  xDelta := 2 * (MaxValue - MinValue);
  // Соновное движение должно привышать более чем в два раза
  if (xLongSP.MinValue = MinValue) and (xLongSP.Delta > xDelta) then
  begin
    // Структура подобна Long
    Result := TTypeDecision.tdBuy;
  end else
  if (xShortSP.MaxValue = MaxValue) and (xShortSP.Delta > xDelta) then
  begin
    // Структура подoбна Short
    Result := TTypeDecision.tdSell;
  end else
  begin
    // Подобна боковик
    Result := TTypeDecision.tdWait;
  end;
  SetUneCandels;
end;

procedure TBlockParam.SetUneCandels;

  procedure _SetMaxAndMinValue(var AMaxValue, AMinValue: Double);
  var
    i: Integer;
    xCandel: TCandel;
  begin
    xCandel := FCandels[0];
    AMaxValue := xCandel.Vol;
    AMinValue := xCandel.Vol;
    for i := 1 to FCountB - 1 do
    begin
      xCandel := FCandels[i];
      if AMaxValue < xCandel.Vol then
        AMaxValue := xCandel.Vol;
      if AMinValue > xCandel.Vol then
        AMinValue := xCandel.Vol;
    end;
  end;

var
  xCandel, xUneCandel: TCandel;
  i, iCount: Integer;
  xMaxVol, xMinVol: Double;
begin
  FUneCandels.Clear;

  _SetMaxAndMinValue(xMaxVol, xMinVol);

  for i := 0 to FCountB - 1 do
  begin
    xCandel := FCandels[i];
    xUneCandel.Open  := (xCandel.Open  - MinValue)/(MaxValue - MinValue);
    xUneCandel.High  := (xCandel.High  - MinValue)/(MaxValue - MinValue);
    xUneCandel.Low   := (xCandel.Low   - MinValue)/(MaxValue - MinValue);
    xUneCandel.Close := (xCandel.Close - MinValue)/(MaxValue - MinValue);
    xUneCandel.Vol   := (xCandel.Vol   - xMinVol)/(xMaxVol - xMinVol);
    FUneCandels.Add(xUneCandel);
  end;
end;

{ TUneBlock }

constructor TUneBlock.Create;
begin
  inherited Create;
  FUneCandels := TCandels.Create;
end;

destructor TUneBlock.Destroy;
begin
  FreeAndNil(FUneCandels);
  inherited;
end;

procedure TUneBlock.SetUneCandels;

  procedure _SetMaxAndMinValue(var AMaxValue, AMinValue: Double);
  var
    i: Integer;
    xCandel: TCandel;
  begin
    xCandel := FCandels[0];
    AMaxValue := xCandel.High;
    AMinValue := xCandel.Low;
    for i := 1 to FCandels.Count - 1 do
    begin
      xCandel := FCandels[0];
      if AMaxValue < xCandel.High then
        AMaxValue := xCandel.High;
      if AMinValue > xCandel.Low then
        AMinValue := xCandel.Low;
    end;
  end;

  procedure _SetMaxAndMinVol(var AMaxVol, AMinVol: Double);
  var
    i: Integer;
    xCandel: TCandel;
  begin
    xCandel := FCandels[0];
    AMaxVol := xCandel.Vol;
    AMinVol := xCandel.Vol;
    for i := 1 to FCandels.Count - 1 do
    begin
      xCandel := FCandels[i];
      if AMaxVol < xCandel.Vol then
        AMaxVol := xCandel.Vol;
      if AMinVol > xCandel.Vol then
        AMinVol := xCandel.Vol;
    end;
  end;

var
  xCandel, xUneCandel: TCandel;
  xMaxValue, xMinValue: Double;
  xMaxVol, xMinVol: Double;
  i, iCount: Integer;
begin
  FUneCandels.Clear;

  _SetMaxAndMinValue(xMaxValue, xMinValue);
  _SetMaxAndMinVol(xMaxVol, xMinVol);


  for i := 0 to FCandels.Count - 1 do
  begin
    xCandel := FCandels[i];
    xUneCandel.Open  := (xCandel.Open  - xMinValue)/(xMaxValue - xMinValue);
    xUneCandel.High  := (xCandel.High  - xMinValue)/(xMaxValue - xMinValue);
    xUneCandel.Low   := (xCandel.Low   - xMinValue)/(xMaxValue - xMinValue);
    xUneCandel.Close := (xCandel.Close - xMinValue)/(xMaxValue - xMinValue);
    xUneCandel.Vol   := (xCandel.Vol   - xMinVol)/(xMaxVol - xMinVol);
    FUneCandels.Add(xUneCandel);
  end;

end;

{ TBlockСondition }

constructor TBlockСondition.Create;
begin
  inherited;
  FTypicalPrices     := TDoubleList.Create;
  FTypicalPriceSMA   := TDoubleList.Create;
  FTypicalPriceSMA_D := TDoubleList.Create;
end;

destructor TBlockСondition.Destroy;
begin
  FreeAndNil(FTypicalPriceSMA_D);
  FreeAndNil(FTypicalPriceSMA);
  FreeAndNil(FTypicalPrices);
  inherited;
end;

procedure TBlockСondition.CalculationTypicalPrice;
var
  xCandel: TCandel;
  i, iCount: Integer;
  xTP: Double;
begin
  {$IFDEF DBL}
  TLogger.LogTree(0,'TBlockСondition.CalculationTypicalPrice:');
  {$ENDIF}
  // Находим типичную цену
  FTypicalPrices.Clear;
  iCount := FCandels.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xCandel := FCandels[i];
      xTP := (xCandel.High + xCandel.Low + xCandel.Close)/3;
      {$IFDEF DBL}
      TLogger.LogTreeText(3,'>> xTP[' + i.ToString + ']:' + xTP.ToString);
      {$ENDIF}
      FTypicalPrices.Add(xTP);
    end;
  {$IFDEF DBL}
  TLogger.LogTree(0,'Проверка: FTypicalPrices.Count := ' + FTypicalPrices.Count.ToString);
  {$ENDIF}
end;

procedure TBlockСondition.CalculationTypicalPriceSMA;

  function _SMA(const AIndex: Integer): Double;
  var
    i, xInd: Integer;
    xCandel: TCandel;
    xSum: Double;
    xIncSum: Integer;
  begin
    Result := 0;
    xSum := 0;
    xIncSum := 0;
    for i := 0 to FPerionCCI - 1 do
    begin
      xInd := AIndex - i;
      if (xInd >= 0) and (xInd < FTypicalPrices.Count) then
      begin
        xSum := xSum + FTypicalPrices[xInd];
        Inc(xIncSum);
      end
      else
        Break;
    end;
    if xIncSum > 0 then
      Result := xSum/xIncSum;
  end;

var
  xValue: Double;
  i, iCount: Integer;
begin
  {$IFDEF DBL}
  TLogger.LogTree(0,'TBlockСondition.CalculationTypicalPriceSMA:');
  {$ENDIF}
  // SMA типичных цен
  FTypicalPriceSMA.Clear;
  iCount := FCandels.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xValue := _SMA(i);
      {$IFDEF DBL}
      TLogger.LogTreeText(3,'>> xValue[' + i.ToString + ']:' + xValue.ToString);
      {$ENDIF}
      FTypicalPriceSMA.Add(xValue);
    end;
  {$IFDEF DBL}
  TLogger.LogTree(0,'Проверка: FTypicalPriceSMA.Count := ' + FTypicalPriceSMA.Count.ToString);
  {$ENDIF}
end;

procedure TBlockСondition.CalculationTypicalPriceD;

  function _D(const AIndex: Integer): Double;
  var
    i, xInd: Integer;
    xCandel: TCandel;
    xSum: Double;
    xIncSum: Integer;
  begin
    Result := 0;
    xSum := 0;
    xIncSum := 0;
    for i := 0 to FPerionCCI - 1 do
    begin
      xInd := AIndex - i;
      if (xInd >= 0) and (xInd < FTypicalPrices.Count) then
      begin
        xSum := xSum + Abs(FTypicalPrices[xInd] - FTypicalPriceSMA[xInd]);
        Inc(xIncSum);
      end
      else
        Break;
    end;
    if xIncSum > 0 then
      Result := xSum/xIncSum;
  end;

var
  xValue: Double;
  i, iCount: Integer;
begin
  {$IFDEF DBL}
  TLogger.LogTree(0,'TBlockСondition.CalculationTypicalPriceD:');
  {$ENDIF}
  // Средние значение откланения
  FTypicalPriceSMA_D.Clear;
  iCount := FCandels.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xValue := _D(i);
      {$IFDEF DBL}
      TLogger.LogTreeText(3,'>> xValue[' + i.ToString + ']:'  + xValue.ToString);
      {$ENDIF}
      FTypicalPriceSMA_D.Add(xValue);
    end;
  {$IFDEF DBL}
  TLogger.LogTree(0,'Проверка: FTypicalPriceSMA_D.Count := ' + FTypicalPriceSMA_D.Count.ToString);
  {$ENDIF}
end;

function TBlockСondition.CCI(const APriod: Integer): Double;
var
  xInd: Integer;
  xValueD: Double;
  xValueM: Double;
  xTP, xSMA: Double;
begin
  {$IFDEF DBL}
  TLogger.LogText('#',80);
  TLogger.LogTree(0,'TBlockСondition.CalculationTypicalPriceD:');
  {$ENDIF}
  // Потребуется оптимизация подведеляемую память
  FPerionCCI := APriod;
  // -------------------------
  CalculationTypicalPrice;
  CalculationTypicalPriceSMA;
  CalculationTypicalPriceD;
  // -------------------------
  xInd := FCandels.Count - 1;

  {$IFDEF DBL}
  TLogger.LogTreeText(3,'Index := ' + xInd.ToString);
  {$ENDIF}

  xTP := FTypicalPrices[xInd];
  xSMA := FTypicalPriceSMA[xInd];

  {$IFDEF DBL}
  TLogger.LogTreeText(3,'TP: ' + xTP.ToString + ' SAM: ' + xSMA.ToString);
  {$ENDIF}

  xValueD := xTP  - xSMA;
  xValueM := 0.015 * FTypicalPriceSMA_D[xInd];

  Result := xValueD/xValueM;
end;

end.
