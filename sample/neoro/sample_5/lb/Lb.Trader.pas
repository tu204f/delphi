unit Lb.Trader;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Contnrs,
  System.Generics.Collections,
  Lb.NeuronNet,
  Lb.NeuronNet.ActivationFunction;

type
  {todo: мехнимз логирования}

  ///<summary>Транзакция — сделок</summary>
  TTransaction = class(TObject)
  private
    FProfit: Double;
    FBuySell: Char;
    FQuantity: Integer;
    FProfitResult: Double;
    FProfitLimit: Double;
    FCountBar: Integer;
    FWaitingСlose: Integer;
  protected
    procedure SetProfit;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>Здесь фиксируем оперцию</summary>
    ///<remarks>Эта событие приходит каждые новый такт, или новый бар</remarks>
    procedure Operation(const APrice: Double; const AQuantity: Integer; const ABuySell: Char);
    ///<summary>Будет долго ждать умрем</summary>
    procedure OperationWait;
    ///<summary>Проверяем возможность совешать трензакции</summary>
    function IsActive: Boolean;
  public

    ///<summary>Количество баров после, открытие позци, закрываем полюбому не зависемо не отчего</summary>
    property CountBar: Integer read FCountBar;
    ///<summary>Сколько нужно подождать до закрытия позиции</summary>
    property WaitingСlose: Integer read FWaitingСlose write FWaitingСlose;

    ///<summary>
    /// Доход с одной сделки
    ///</summary>
    property Profit: Double read FProfit write FProfit;

    ///<summary>
    /// Направление сделки
    ///</summary>
    property BuySell: Char read FBuySell write FBuySell;

    ///<summary>
    /// Размер открываемой позиции
    ///</summary>
    property Quantity: Integer read FQuantity write FQuantity;

    ///<summary>
    /// Общий результат по все сделкам
    ///</summary>
    property ProfitResult: Double read FProfitResult write FProfitResult;

    ///<summary>
    /// Лимитный профит, ограничивает рост и падение
    /// Нужно для упрвления риски по все оперция, то есть максимальная
    /// Просадка, которая может - что не убить весть полученую приболь
    ///</summary>
    property ProfitLimit: Double read FProfitLimit write FProfitLimit;
  end;

  ///<summary>Принятое решение</summary>
  TTypeDecision = (tdBuy = 0, tdWait, tdSell);

  ///<summary>Участник торгов</summary>
  TTraderMan = class(TObject)
  private
    FID: Integer;
    FTypeDecision: TTypeDecision;
    FNeuronNet: TNeuronNet;
    FTransaction: TTransaction;
    FAge: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Calculate(const AValues: array of Double);
    property NeuronNet: TNeuronNet read FNeuronNet;
    property TypeDecision: TTypeDecision read FTypeDecision;
    ///<summary>Трейдер совершает торговые оперции</summary>
    property Transaction: TTransaction read FTransaction;
    property ID: Integer read FID write FID;
    property Age: Integer read FAge write FAge;
  end;

  ///<summary>Список трейдеров</summary>
  TTraderManList = TObjectList<TTraderMan>;

implementation

{ TTransaction }

constructor TTransaction.Create;
begin
  FProfit := 0;
  FProfitResult := 0;
  FProfitLimit := 0;
  FCountBar := 0;
  FWaitingСlose := 5;
end;

destructor TTransaction.Destroy;
begin
  inherited;
end;


function TTransaction.IsActive: Boolean;
begin
  Result := FProfitLimit > -2000;
end;

procedure TTransaction.Operation(const APrice: Double; const AQuantity: Integer; const ABuySell: Char);

  procedure _SetCloseOperation(const APrice: Double);
  begin
    case FBuySell of
      'B': FProfit := FProfit - APrice * FQuantity;
      'S': FProfit := APrice * FQuantity - FProfit;
    end;
    FBuySell := #0;
    FQuantity := 0;
    SetProfit;
  end;

begin
  // Проверяем возможность совершение транзакций
  if not IsActive then
    Exit;

  // -------------------------------------------
  if FBuySell = ABuySell then
  begin

    if FCountBar >= FWaitingСlose then
    begin
      _SetCloseOperation(APrice);
    end;
    Inc(FCountBar);
    {todo: Дать возможность нарашивать позицию}
  end
  else
  begin
    if FQuantity = 0 then
    begin
      FCountBar := 0;
      FQuantity := AQuantity;
      FBuySell := ABuySell;
      FProfit := AQuantity * APrice;
    end
    else
    begin
      _SetCloseOperation(APrice);
    end;
  end;
end;

procedure TTransaction.OperationWait;
begin
  if IsActive then
    FProfitLimit := FProfitLimit - 1;
end;

procedure TTransaction.SetProfit;
begin
  FProfitResult := FProfitResult + FProfit;
  FProfitLimit  := FProfitLimit + FProfit;
  if FProfitLimit > 500 then
    FProfitLimit := 500;
end;

{ TTraderMan }

constructor TTraderMan.Create;
begin
  FNeuronNet := TNeuronNet.Create;
  FNeuronNet.TypeFuction := TTypeFuction.tfSigma;

  // Пять входящий нейронов (Можно использовать не индикатор а массив свячей)
  // Один одинарный нейрон
  // 3 первых нейронов, определяют направление tdBuy = 0, tdWait, tdSell
  // 5 Последних нейронов — определяю количество свечей ожиданий до закрытие

  FNeuronNet.CompileNetWork([601,100,100,100,8]);
  FTransaction := TTransaction.Create;
  FAge := 0;
end;

destructor TTraderMan.Destroy;
begin
  FreeAndNil(FTransaction);
  FreeAndNil(FNeuronNet);
  inherited;
end;

procedure TTraderMan.Calculate(const AValues: array of Double);

  function _IndexOfValueMax(AValues: array of Double): Integer;
  var
    i, xLength, xInd: Integer;
    xValueMax: Double;
  begin
    // Нормализация
    xLength := Length(AValues);

    xInd := 0;
    xValueMax := 0;
    for i := 0 to xLength - 1 do
    begin
      if AValues[i] > xValueMax then
      begin
        xValueMax := AValues[i];
        xInd := i;
      end;
    end;

    Result := xInd;
  end;

  function _TypeDecision(AValues: array of Double): Integer;
  begin
    Result := _IndexOfValueMax(AValues)
  end;


  function _WaitingСlose(AValues: array of Double): Integer;
  begin
    Result := _IndexOfValueMax(AValues) + 1;
  end;

var
  xIndex: Integer;
begin
  FNeuronNet.InputNeurons.Values[0] := AValues[0];
  FNeuronNet.InputNeurons.Values[1] := AValues[1];
  FNeuronNet.InputNeurons.Values[2] := AValues[2];
  FNeuronNet.InputNeurons.Values[3] := AValues[3];
  FNeuronNet.InputNeurons.Values[4] := AValues[4];
  FNeuronNet.InputNeurons.Values[5] := 1;

  // Производим вычисление
  FNeuronNet.Calculate;

  // Решение ожиданий
  xIndex := _TypeDecision([
    FNeuronNet.OutputNeurons.Values[0],
    FNeuronNet.OutputNeurons.Values[1],
    FNeuronNet.OutputNeurons.Values[2]
  ]);
  FTypeDecision := TTypeDecision(xIndex);

  // Количество свечей ожиданий
  FTransaction.WaitingСlose := _WaitingСlose([
    FNeuronNet.OutputNeurons.Values[3],
    FNeuronNet.OutputNeurons.Values[4],
    FNeuronNet.OutputNeurons.Values[5],
    FNeuronNet.OutputNeurons.Values[6],
    FNeuronNet.OutputNeurons.Values[7]
  ]);

end;


end.
