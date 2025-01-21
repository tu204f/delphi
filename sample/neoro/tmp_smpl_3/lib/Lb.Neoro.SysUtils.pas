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
  TFunctionActive = function(const AValue: Double): Double;

  TDoubleList = array of Double;
  TDoubleMarix = array of array of Double;

  ///<summary>Значение нейров</summary>
  TNeuronsLayer = class(TObject)
  private
    FCount: Integer;
    FValues: TDoubleList;
    FErrors: TDoubleList;
    function GetValues(Index: Integer): Double;
    function GetErrors(Index: Integer): Double;
    procedure SetValues(Index: Integer; const Value: Double);
    procedure SetErrors(Index: Integer; const Value: Double);
  public
    constructor Create(const ACount: Integer); virtual;
    destructor Destroy; override;
    property Values[Index: Integer]: Double read GetValues write SetValues;
    property Errors[Index: Integer]: Double read GetErrors write SetErrors;
    property Count: Integer read FCount;
  end;
  TNeuronsLayerList = TObjectList<TNeuronsLayer>;

  ///<summary>Веса<summary>
  ///<remarks>
  /// InPut - это строки
  /// OutPut - это количество строки
  ///</remarks>
  TWeightsLayer = class(TObject)
  private
    FRowCount: Integer;
    FColCount: Integer;
    FValues: TDoubleMarix;
    function GetValues(Col, Row: Integer): Double;
    procedure SetValues(Col, Row: Integer; const Value: Double);
  private
    FInputNeurons: TNeuronsLayer;
    FOutputNeurons: TNeuronsLayer;
  public
    constructor Create(const AInputNeurons, AOutputNeurons: TNeuronsLayer); overload;
    constructor Create(const AInputCount, AOutputCount: Integer); overload;
    destructor Destroy; override;
    procedure DefaultValue;
    property Values[Col, Row: Integer]: Double read GetValues write SetValues;
    property RowCount: Integer read FRowCount;
    property ColCount: Integer read FColCount;
    property InputNeurons: TNeuronsLayer read FInputNeurons;
    property OutputNeurons: TNeuronsLayer read FOutputNeurons;
  end;
  TWeightsLayerList = TObjectList<TWeightsLayer>;

  ///<summary>Нейроная сеть</summary>
  TNeuronNet = class(TObject)
  private
    FNeuronsLayers: TNeuronsLayerList;
    FWeightsLayers: TWeightsLayerList;
    function GetInputNeurons: TNeuronsLayer;
    function GetOutputNeurons: TNeuronsLayer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure CreatingNeuralNetwork(const ACountValues: array of Integer);
    property NeuronsLayers: TNeuronsLayerList read FNeuronsLayers;
    property WeightsLayers: TWeightsLayerList read FWeightsLayers;
  public
    procedure Calculate;
    procedure CalculateError(const AEtalons: array of Double);
    procedure CalculateLearn(AValue: Double);
  public
    property InputNeurons: TNeuronsLayer read GetInputNeurons;
    property OutputNeurons: TNeuronsLayer read GetOutputNeurons;
  end;

implementation

{ TNeoron }

constructor TNeuronsLayer.Create(const ACount: Integer);
begin
  FCount := ACount;
  SetLength(FValues,ACount);
  SetLength(FErrors,ACount);
end;

destructor TNeuronsLayer.Destroy;
begin

  inherited;
end;

function TNeuronsLayer.GetErrors(Index: Integer): Double;
begin
  Result := FErrors[Index];
end;

function TNeuronsLayer.GetValues(Index: Integer): Double;
begin
  Result := FValues[Index];
end;

procedure TNeuronsLayer.SetErrors(Index: Integer; const Value: Double);
begin
  FErrors[Index] := Value;
end;

procedure TNeuronsLayer.SetValues(Index: Integer; const Value: Double);
begin
  FValues[Index] := Value;
end;

{ TWeight }

constructor TWeightsLayer.Create(const AInputNeurons, AOutputNeurons: TNeuronsLayer);
begin
  FInputNeurons  := AInputNeurons;
  FOutputNeurons := AOutputNeurons;
  FRowCount := AOutputNeurons.Count;
  FColCount := AInputNeurons.Count;
  if (FRowCount > 0) and (FColCount > 0) then
    SetLength(FValues,FRowCount,FColCount)
end;

constructor TWeightsLayer.Create(const AInputCount, AOutputCount: Integer);
begin
  FInputNeurons  := nil;
  FOutputNeurons := nil;
  FRowCount := AOutputCount;
  FColCount := AInputCount;
  if (FRowCount > 0) and (FColCount > 0) then
  begin
    FRowCount := FRowCount + 1;
    SetLength(FValues,FRowCount,FColCount);
  end;
end;

procedure TWeightsLayer.DefaultValue;
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

destructor TWeightsLayer.Destroy;
begin

  inherited;
end;

function TWeightsLayer.GetValues(Col, Row: Integer): Double;
begin
  Result := FValues[Row,Col];
end;

procedure TWeightsLayer.SetValues(Col, Row: Integer; const Value: Double);
begin
  FValues[Row,COl] := Value;
end;

{ TNeuronNet }

constructor TNeuronNet.Create;
begin
  FNeuronsLayers := TNeuronsLayerList.Create;
  FWeightsLayers := TWeightsLayerList.Create;
end;

destructor TNeuronNet.Destroy;
begin
  FreeAndNil(FWeightsLayers);
  FreeAndNil(FNeuronsLayers);
  inherited;
end;

function TNeuronNet.GetInputNeurons: TNeuronsLayer;
begin
  Result := FNeuronsLayers[0]
end;

function TNeuronNet.GetOutputNeurons: TNeuronsLayer;
begin
  Result := FNeuronsLayers[FNeuronsLayers.Count - 1];
end;

procedure TNeuronNet.Clear;
begin
  FNeuronsLayers.Clear;
  FWeightsLayers.Clear;
end;

procedure TNeuronNet.CreatingNeuralNetwork(const ACountValues: array of Integer);

  procedure _CreateNeuron(const ACountValues: array of Integer);
  var
    xNeuron: TNeuronsLayer;
  begin
    var xL := Length(ACountValues);
    for var i := 0 to xL - 1 do
    begin
      xNeuron := TNeuronsLayer.Create(ACountValues[i]);
      FNeuronsLayers.Add(xNeuron);
    end;
  end;

  procedure _CreateWeight;
  var
    xWeightsLayer: TWeightsLayer;
    xInputNeuron, xOutputNeuron: TNeuronsLayer;
    i, iCount: Integer;
  begin
    iCount := FNeuronsLayers.Count;
    if iCount > 0 then
      for i := 1 to iCount - 1 do
      begin
        xInputNeuron := FNeuronsLayers[i - 1];
        xOutputNeuron := FNeuronsLayers[i];
        xWeightsLayer := TWeightsLayer.Create(xInputNeuron,xOutputNeuron);
        xWeightsLayer.DefaultValue;
        FWeightsLayers.Add(xWeightsLayer);
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

  procedure _Calculate(AInputNeuron, AOutputNeuron: TNeuronsLayer; AWeightsLayer: TWeightsLayer);
  var
    xSum: Double;
    xRow, xRowCount: Integer;
    xCol, xColCount: Integer;
  begin
    xRowCount := AWeightsLayer.RowCount;
    xColCount := AWeightsLayer.ColCount;
    if (xRowCount > 0) and (xColCount > 0) then
    begin
      for xRow := 0 to xRowCount - 1 do
      begin
        xSum := 0;
        for xCol := 0 to xColCount - 1 do
          xSum := xSum + AInputNeuron.Values[xCol] * AWeightsLayer.Values[xCol,xRow];
        AOutputNeuron.Values[xRow] := _ActivationFunction(xSum);
      end;
    end;
  end;

var
  i, iCount: Integer;
  xWeight: TWeightsLayer;
  xInputNeuron, xOutputNeuron: TNeuronsLayer;
begin
  iCount := FNeuronsLayers.Count;
  if iCount > 0 then
  begin
    for i := 1 to iCount - 1 do
    begin
      xInputNeuron := FNeuronsLayers[i - 1];
      xOutputNeuron := FNeuronsLayers[i];
      xWeight := FWeightsLayers[i - 1];
      _Calculate(xInputNeuron,xOutputNeuron,xWeight);
    end;
  end;
end;

procedure TNeuronNet.CalculateError(const AEtalons: array of Double);

  function _DifSigma(const AValue: Double): Double;
  begin
    Result := AValue * (1 - AValue);
  end;

  procedure _CalculateErrorWeights(AWeight: TWeightsLayer);
  var
    r, c: Integer;
    xSum: Double;
    xW, xV: Double;
  begin
    for c := 0 to AWeight.InputNeurons.Count - 1 do
    begin
      xSum := 0;
      for r := 0 to AWeight.OutputNeurons.Count - 1 do
      begin
        xW := AWeight.Values[c,r];
        xV := AWeight.OutputNeurons.Errors[r];
        xSum := xSum + xW * xV;
      end;
      AWeight.InputNeurons.Errors[c] := xSum * _DifSigma(AWeight.InputNeurons.Values[c]);
    end;
  end;

var
  i: Integer;
  xNeuron: TNeuronsLayer;
  xWeight: TWeightsLayer;
begin
  if FNeuronsLayers.Count > 0 then
  begin
    xNeuron := FNeuronsLayers[FNeuronsLayers.Count - 1];
    if xNeuron.Count > 0 then
    begin
      for i := 0 to xNeuron.Count - 1 do
      begin
        xNeuron.Errors[i] := AEtalons[i] - xNeuron.Values[i];
        xNeuron.Errors[i] := xNeuron.Errors[i] * _DifSigma(xNeuron.Values[i]);
      end;
    end;

    for i := FWeightsLayers.Count - 1 downto 0 do
    begin
      xWeight := FWeightsLayers[i];
      _CalculateErrorWeights(xWeight);
    end;

  end;
end;

procedure TNeuronNet.CalculateLearn(AValue: Double);

  procedure _CalculateLearnWeights(AWeight: TWeightsLayer);
  var
    c, r: Integer;
    xW, xV, xE: Double;
    xDeltaW: Double;
  begin
    for c := 0 to AWeight.InputNeurons.Count - 1 do
    begin
      for r := 0 to AWeight.OutputNeurons.Count - 1 do
      begin
        xW := AWeight.Values[c,r];
        xE := AWeight.OutputNeurons.Errors[r];
        xV := AWeight.InputNeurons.Values[c];

        xDeltaW := AValue * xE * xV;
        AWeight.Values[c,r] := xW + xDeltaW;
      end;
    end;
  end;

var
  xWeight: TWeightsLayer;
begin
  if FWeightsLayers.Count > 0 then
    for var i := FWeightsLayers.Count - 1 downto 0 do
    begin
      xWeight := FWeightsLayers[i];
      _CalculateLearnWeights(xWeight);
    end;
end;

end.
