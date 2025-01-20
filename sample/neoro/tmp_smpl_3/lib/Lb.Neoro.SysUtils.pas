unit Lb.Neoro.SysUtils;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Contnrs,
  System.Generics.Collections;

type
  TDoubleList = array of Double;
  TDoubleMarix = array of array of Double;

  ///<summary>Значение нейров</summary>
  TNeuron = class(TObject)
  private
    FCount: Integer;
    FValues: TDoubleList;
    FErrors: TDoubleList;
    function GetValues(Index: Integer): Double;
    procedure SetValues(Index: Integer; const Value: Double);
    function GetErrors(Index: Integer): Double;
    procedure SetErrors(Index: Integer; const Value: Double);
  public
    constructor Create(const ACount: Integer); virtual;
    destructor Destroy; override;
    property Values[Index: Integer]: Double read GetValues write SetValues;
    property Errors[Index: Integer]: Double read GetErrors write SetErrors;
    property Count: Integer read FCount;
  end;
  TNeuronList = TObjectList<TNeuron>;

  ///<summary>Веса<summary>
  ///<remarks>
  /// InPut - это строки
  /// OutPut - это количество строки
  ///</remarks>
  TWeight = class(TObject)
  private
    FRowCount: Integer;
    FColCount: Integer;
    FValues: TDoubleMarix;
    function GetValues(Col, Row: Integer): Double;
    procedure SetValues(Col, Row: Integer; const Value: Double);
  public
    constructor Create(const AInputNeorons, AOutputNeorons: TNeuron); overload;
    constructor Create(const AInputCount, AOutputCount: Integer); overload;
    destructor Destroy; override;
    procedure DefaultValue;
    property Values[Col, Row: Integer]: Double read GetValues write SetValues;
    property RowCount: Integer read FRowCount;
    property ColCount: Integer read FColCount;
  end;
  TWeightList = TObjectList<TWeight>;

  ///<summary>Нейроная сеть</summary>
  TNeuronNet = class(TObject)
  private
    FNeurons: TNeuronList;
    FWeights: TWeightList;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear;
    ///<summary>Создание Нейронной сетки</summary>
    procedure CreatingNeuralNetwork(const ACountValues: array of Integer);

    property Neurons: TNeuronList read FNeurons;
    property Weights: TWeightList read FWeights;
  public
    procedure Calculate;
    procedure CalculateError(const AEtalons: array of Double);
    procedure CalculateLearn(AValue: Double);
  end;

implementation


{ TNeoron }

constructor TNeuron.Create(const ACount: Integer);
begin
  FCount := ACount;
  SetLength(FValues,ACount);
  SetLength(FErrors,ACount);
end;

destructor TNeuron.Destroy;
begin

  inherited;
end;

function TNeuron.GetErrors(Index: Integer): Double;
begin
  Result := FErrors[Index];
end;

function TNeuron.GetValues(Index: Integer): Double;
begin
  Result := FValues[Index];
end;

procedure TNeuron.SetErrors(Index: Integer; const Value: Double);
begin
  FErrors[Index] := Value;
end;

procedure TNeuron.SetValues(Index: Integer; const Value: Double);
begin
  FValues[Index] := Value;
end;

{ TWeight }

constructor TWeight.Create(const AInputNeorons, AOutputNeorons: TNeuron);
begin
  FRowCount := AInputNeorons.Count;
  FColCount := AOutputNeorons.Count;
  if (FRowCount > 0) and (FColCount > 0) then
    SetLength(FValues,FRowCount,FColCount)
end;

constructor TWeight.Create(const AInputCount, AOutputCount: Integer);
begin
  FRowCount := AInputCount;
  FColCount := AOutputCount;
  if (FRowCount > 0) and (FColCount > 0) then
  begin
    FRowCount := FRowCount + 1;
    SetLength(FValues,FRowCount,FColCount);
  end;
end;

procedure TWeight.DefaultValue;
var
  xRow, xCol: Integer;
begin
  if (FRowCount > 0) and (FColCount > 0) then
  begin
    for xRow := 0 to FRowCount - 1 do
      for xCol := 0 to FColCount - 1 do
        FValues[xRow,xCol] :=  Trunc(10000 * Random)/10000;
  end;
end;

destructor TWeight.Destroy;
begin

  inherited;
end;

function TWeight.GetValues(Col, Row: Integer): Double;
begin
  Result := FValues[Row,COl];
end;

procedure TWeight.SetValues(Col, Row: Integer; const Value: Double);
begin
  FValues[Row,COl] := Value;
end;

{ TNeuronNet }

constructor TNeuronNet.Create;
begin
  FNeurons := TNeuronList.Create;
  FWeights := TWeightList.Create;
end;

destructor TNeuronNet.Destroy;
begin
  FreeAndNil(FWeights);
  FreeAndNil(FNeurons);
  inherited;
end;

procedure TNeuronNet.Clear;
begin
  FNeurons.Clear;
  FWeights.Clear;
end;

procedure TNeuronNet.CreatingNeuralNetwork(const ACountValues: array of Integer);

  procedure _CreateNeuron(const ACountValues: array of Integer);
  var
    xNeuron: TNeuron;
  begin
    var xL := Length(ACountValues);
    for var i := 0 to xL - 1 do
    begin
      xNeuron := TNeuron.Create(ACountValues[i]);
      FNeurons.Add(xNeuron);
    end;
  end;

  procedure _CreateWeight;
  var
    xWeight: TWeight;
    xInputNeuron, xOutputNeuron: TNeuron;
    i, iCount: Integer;
  begin
    iCount := FNeurons.Count;
    if iCount > 0 then
      for i := 1 to iCount - 1 do
      begin
        xInputNeuron := FNeurons[i - 1];
        xOutputNeuron := FNeurons[i];
        xWeight := TWeight.Create(xInputNeuron,xOutputNeuron);
        xWeight.DefaultValue;
        FWeights.Add(xWeight);
      end;
  end;


begin
  _CreateNeuron(ACountValues);
  _CreateWeight;
end;

procedure TNeuronNet.Calculate;

  function _ActivationFunction(const AValue: Double): Double;
  begin
    Result := 1 / (1 + Exp((-1 * AValue)));
  end;

  procedure _Calculate(AInputNeuron, AOutputNeuron: TNeuron; AWeight: TWeight);
  var
    xSum: Double;
    xRow, xRowCount: Integer;
    xCol, xColCount: Integer;
  begin
    xRowCount := AWeight.RowCount;
    xColCount := AWeight.ColCount;
    if (xRowCount > 0) and (xColCount > 0) then
    begin
      for xCol := 0 to xColCount - 1 do
      begin
        xSum := 0;
        for xRow := 0 to xRowCount - 1 do
        begin
          if xRow < xRowCount then
            xSum := xSum + AInputNeuron.Values[xRow] * AWeight.Values[xRow,xCol]
          else
            xSum := xSum + AWeight.Values[xRow,xCol]
        end;
        AOutputNeuron.Values[xCol] := _ActivationFunction(xSum);
      end;
    end;
  end;

var
  i, iCount: Integer;
  xWeight: TWeight;
  xInputNeuron, xOutputNeuron: TNeuron;
begin
  iCount := FNeurons.Count;
  if iCount > 0 then
  begin
    for i := 1 to iCount - 1 do
    begin
      xInputNeuron := FNeurons[i - 1];
      xOutputNeuron := FNeurons[i];
      xWeight := FWeights[i - 1];
      _Calculate(xInputNeuron,xOutputNeuron,xWeight);
    end;
  end;
end;

procedure TNeuronNet.CalculateError;
begin

end;

procedure TNeuronNet.CalculateLearn(AValue: Double);
begin

end;



end.
