unit Lb.Block;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.NeuronNet,
  Lb.SysUtils.Candel;

type
  TBlock = class;
  TBlockList = TObjectList<TBlock>;

  ///<summary>Базовый объект</summary>
  TBlock = class(TObject)
  private
    FCandels: TCandels;
  private
    procedure SetConvert(ACandels: TCandelList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFileName(const AFileName: String);
    procedure SetParserBlock(const ASources: TStrings);
    property Candels: TCandels read FCandels;
  end;

  ///<summary>Блок, где определенно будущие</summary>
  TBlockF = class(TBlock)
  public type
    TTypePotential = (tpNull,tpUp, tpDonw);
  private
    FTypePotential: TTypePotential;
    FPotential: Double;
    FCountF: Integer;
    FPriceF: Double;
    FSourceCandels: TCandels;
    procedure SetCountF(const Value: Integer);
  public
    constructor Create; override;
    destructor Destroy; override;
    ///<summary>Сколько свячий нужно пропустить</summary>
    property CountF: Integer read FCountF write SetCountF;
    property PriceF: Double read FPriceF;
    property SourceCandels: TCandels read FSourceCandels;
  end;

type
  TIntegerList = TList<Integer>;

  ///<summary>Объект обучения нейроносети на основание блоков</summary>
  TNeuronNetThread = class(TObject)
  public type
    TStatus = class(TObject)
      CountLearn: Integer;   // Количество эпох
      CountBlocks: Integer;  // Количество блоков
    end;

  public const
    SIZE_BLOCK = 160;
    SIZE_F_BLOCK = 10;
  private
    FEtalons: TIntegerList;
    FNeuronNet: TNeuronNet;
    FOnBeginLearn: TNotifyEvent;
    FOnEndLearn: TNotifyEvent;
    FBlocks: TBlockList;
    FCountLearn: Integer;

    FFileName: String;
    FStatus: TStatus;
  protected
    FActive: Boolean;
    procedure SetExecution;
    procedure DoBeginLearn;
    procedure DoEndLearn;

    procedure NeuronNet_Anticipation(ABlock: TBlock);
    procedure NeuronNet_Etalon(ABlock: TBlock);
    procedure NeuronNet_SetBlock(ABlock: TBlock);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure StartLearn;
    procedure StopLearn;
    procedure CompileNetWork(const ACounts: TArrInteger);
    ///<summary>Данные на основание которых происходит обучение</summary>
    property Blocks: TBlockList write FBlocks;
    ///<summary>Нейронная сеть которую нужно обучить</summary>
    property NeuronNet: TNeuronNet read FNeuronNet;
    property IsActive: Boolean read FActive;
    property OnBeginLearn: TNotifyEvent write FOnBeginLearn;
    property OnEndLearn: TNotifyEvent write FOnEndLearn;
    ///<summary>Количество эпох - обучения</summary>
    property CountLearn: Integer read FCountLearn write FCountLearn;
    ///<summary>Состояние обучения</summary>
    property Status: TStatus read FStatus;
    ///<summary>Имя файла</summary>
    property FileName: String write FFileName;
  end;

implementation

uses
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
var
  xCandels: TCandelList;
begin
  xCandels := TCandelList.Create;
  try
    iCount := ASources.Count;
    if iCount > 0 then
    begin

      for i := 0 to iCount - 1 do
      begin
        xS := ASources[i];
        xCandel.Create(xS);
        xCandels.Add(xCandel);
      end;

    end;
    SetConvert(xCandels);
  finally
    FreeAndNil(xCandels);
  end;
end;

procedure TBlock.SetConvert(ACandels: TCandelList);

  procedure _SetMaxAndMinPrice(var AMaxPrice, AMinPrice: Double);
  var
    i, iCount: Integer;
    xCandel: TCandel;
  begin
    // Предельное значение цены
    iCount := ACandels.Count;
    if iCount > 0 then
    begin
      xCandel := ACandels[0];
      AMaxPrice := xCandel.High;
      AMinPrice := xCandel.Low;
      for i := 1 to iCount - 1 do
      begin
        xCandel := ACandels[i];
        if xCandel.High > AMaxPrice then
          AMaxPrice := xCandel.High;
        if xCandel.Low < AMinPrice then
          AMinPrice := xCandel.Low;
      end;
    end;
  end;

  procedure _SetMaxAndMinVol(var AMaxVol, AMinVol: Double);
  var
    i, iCount: Integer;
    xCandel: TCandel;
  begin
    // Предельная значение объема
    iCount := ACandels.Count;
    if iCount > 0 then
    begin
      xCandel := ACandels[0];
      AMaxVol := xCandel.Vol;
      AMinVol := xCandel.Vol;
      for i := 1 to iCount - 1 do
      begin
        xCandel := ACandels[i];
        if xCandel.Vol > AMaxVol then
          AMaxVol := xCandel.Vol;
        if xCandel.Vol < AMinVol then
          AMinVol := xCandel.Vol;
      end;
    end;
  end;

  function _GetPrice(APrice, AMaxPrice, AMinPrice: Double): Double;
  begin
    if (AMaxPrice > 0) and (AMinPrice > 0) then
      Result := (APrice - AMinPrice)/(AMaxPrice - AMinPrice)
    else
      Result := 0;
  end;

  function _GetVol(AVol, AMaxVol, AMinVol: Double): Double;
  begin
    if (AMaxVol > 0) and (AMinVol > 0) then
      Result := (AVol - AMinVol)/(AMaxVol - AMinVol)
    else
      Result := 0;
  end;

var
  xUneCandel, xCandel: TCandel;
  xMaxPrice, xMinPrice: Double;
  xMaxVol, xMinVol: Double;
begin
  FCandels.Clear;

  _SetMaxAndMinPrice(xMaxPrice, xMinPrice);
  _SetMaxAndMinVol(xMaxVol, xMinVol);

  for xCandel in ACandels do
  begin
    xUneCandel.Open  := _GetPrice(xCandel.Open, xMaxPrice, xMinPrice);
    xUneCandel.High  := _GetPrice(xCandel.High, xMaxPrice, xMinPrice);
    xUneCandel.Low   := _GetPrice(xCandel.Low,  xMaxPrice, xMinPrice);
    xUneCandel.Close := _GetPrice(xCandel.Close,xMaxPrice, xMinPrice);
    xUneCandel.Vol   := _GetVol(xCandel.Vol, xMaxVol, xMinVol);
    FCandels.Add(xUneCandel);
  end;
end;

{ TBlockF }

constructor TBlockF.Create;
begin
  inherited;
  FSourceCandels := TCandels.Create;
  FTypePotential := TTypePotential.tpNull;
end;

destructor TBlockF.Destroy;
begin
  FreeAndNil(FSourceCandels);
  inherited;
end;

procedure TBlockF.SetCountF(const Value: Integer);
var
  xCandel: TCandel;
begin
  FCountF := Value;
  FSourceCandels.Clear;
  for var i := 0 to FCandels.Count - FCountF - 1 do
  begin
    xCandel := FCandels[i];
    FSourceCandels.Add(xCandel);
  end;
  FPriceF := xCandel.Close;
  xCandel := FCandels[FCandels.Count - 1];
  FPotential :=  xCandel.Close - FPriceF;
  if FPotential > 0 then
    FTypePotential := TTypePotential.tpUp
  else if FPotential < 0 then
    FTypePotential := TTypePotential.tpDonw
  else
    FTypePotential := TTypePotential.tpNull;
end;

{ TNeuronNetThread }

constructor TNeuronNetThread.Create;
begin
  FFileName := '';
  FActive := False;
  FCountLearn := 10000;

  FStatus  := TStatus.Create;
  FEtalons := TIntegerList.Create;
  FNeuronNet := TNeuronNet.Create;
end;

destructor TNeuronNetThread.Destroy;
begin
  FreeAndNil(FStatus);
  FreeAndNil(FEtalons);
  FreeAndNil(FNeuronNet);
  inherited;
end;

procedure TNeuronNetThread.DoBeginLearn;
begin
  if Assigned(FOnBeginLearn) then
    FOnBeginLearn(Self);
end;

procedure TNeuronNetThread.DoEndLearn;
begin
  if Assigned(FOnEndLearn) then
    FOnEndLearn(Self);

  FNeuronNet.Save(FFileName);
end;


procedure TNeuronNetThread.CompileNetWork(const ACounts: TArrInteger);
begin
  FNeuronNet.CompileNetWork(ACounts);
end;

procedure TNeuronNetThread.NeuronNet_Anticipation(ABlock: TBlock);

  procedure _SetMaxMinValue(const AValue: Integer; var AMaxValue, AMinValue: Integer);
  begin
    if AValue > AMaxValue then
      AMaxValue := AValue;
    if AValue < AMinValue then
      AMinValue := AValue;
  end;

  procedure _EtalonNull;
  begin
    FEtalons.Clear;
    for var i := 0 to SIZE_F_BLOCK - 1 do
      FEtalons.Add(0);
  end;

var
  xLast, xHigh, xLow, xValue: Integer;
  xIndex: Integer;
  xCount: Integer;
var
  xInd: Integer;
  xMaxValue, xMinValue: Integer;

begin
  // Вычисляем эталонное заначение
  xMaxValue := 0;
  xMinValue := 100;

  _EtalonNull;

  xIndex := ABlock.Candels.Count - SIZE_F_BLOCK;
  xCount := ABlock.Candels.Count;

  xLast := Trunc(100 * ABlock.Candels[xIndex].Close);
  for var i := xIndex to xCount - 1 do
  begin
    xHigh := Trunc(100 * ABlock.Candels[i].High);
    xLow  := Trunc(100 * ABlock.Candels[i].Low);
    xValue := (xHigh - xLast);
    _SetMaxMinValue(xValue,xMaxValue, xMinValue);
    xValue := (xLow - xLast);
    _SetMaxMinValue(xValue,xMaxValue, xMinValue);
  end;

  for var i := 0 to 4 do
  begin
    xInd := 4 - i;
    // --------------------------------
    if (10 * i) <= xMaxValue then
      FEtalons[xInd] := 1;
    // --------------------------------
    xInd := i + 5;
    if (-10 * i) >= xMinValue then
      FEtalons[xInd] := 1;
  end;

end;

procedure TNeuronNetThread.NeuronNet_Etalon(ABlock: TBlock);
begin
  NeuronNet.OutputNeurons.EtalonNullValue;
  NeuronNet_Anticipation(ABlock);

  for var i := 0 to FEtalons.Count - 1 do
    NeuronNet.OutputNeurons.Errors[i] := FEtalons[i];
end;

procedure TNeuronNetThread.NeuronNet_SetBlock(ABlock: TBlock);
var
  xCandel: TCandel;
  xInd, i: Integer;
begin
  // Запольняем входными данными и вычисляем
  xInd := 0;
  for i := 0 to ABlock.Candels.Count - SIZE_F_BLOCK - 1 do
  begin
    xCandel := ABlock.Candels[i];

    NeuronNet.InputNeurons.Values[xInd] := xCandel.Open;
    Inc(xInd);

    NeuronNet.InputNeurons.Values[xInd] := xCandel.High;
    Inc(xInd);

    NeuronNet.InputNeurons.Values[xInd] := xCandel.Low;
    Inc(xInd);

    NeuronNet.InputNeurons.Values[xInd] := xCandel.Close;
    Inc(xInd);

    NeuronNet.InputNeurons.Values[xInd] := xCandel.Vol;
    Inc(xInd);

  end;
  NeuronNet.InputNeurons.Values[xInd] := 1;
  NeuronNet.Calculate;
end;

procedure TNeuronNetThread.SetExecution;
var
  xTask: ITask;
begin
  xTask := TTask.Create(
    procedure()
    var
      xIndexBlock: Integer;
      i, j: Integer;
      xBlock: TBlock;
    begin
      Randomize;
      TThread.Synchronize(nil,DoBeginLearn);
      // Перебираем эпози обучения
      for i := 0 to FCountLearn - 1 do
      begin
        for j := 0 to FBlocks.Count - 1 do
        begin
          FStatus.CountLearn := i;
          FStatus.CountBlocks := j;

          // Случайным обрахом выбираем блок
          xIndexBlock := Random(FBlocks.Count);;
          xBlock := FBlocks[xIndexBlock];
          // Формируем эталонное занчение
          NeuronNet_Etalon(xBlock);
          // Задаем начальное заначение
          NeuronNet_SetBlock(xBlock);
          // Вычисляем ошибку
          FNeuronNet.CalculateError;
          // Обучение
          FNeuronNet.CalculateLearn(0.1);
        end;
        if not FActive then
          Break;
      end;
      TThread.Synchronize(nil,DoEndLearn);
    end
  );
  xTask.Start;
end;

procedure TNeuronNetThread.StartLearn;
begin
  if not FActive then
  begin
    SetExecution;
    FActive := True;
  end;
end;

procedure TNeuronNetThread.StopLearn;
begin
  if FActive then
    FActive := False;
end;

end.
