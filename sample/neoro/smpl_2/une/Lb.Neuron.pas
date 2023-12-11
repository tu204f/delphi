(******************************************************************************)
(* Нероная сеть с обучением                                                   *)
(******************************************************************************)
unit Lb.Neuron;

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
  TTypeLayer = (tlNull,tlNorm,tlReverse);

  TNeuron = class;
  TWeight = class;
  TNeuronLayer = class;

  TNeuronList = TObjectList<TNeuron>;
  TWeightList = TObjectList<TWeight>;
  TValueWeightList = TList<Double>;
  TNeuronLayerList = TObjectList<TNeuronLayer>;



  ///<summary>Связь</summary>
  TWeight = class(TObject)
  private
    FValue: Double;
    FInputNeuron: TNeuron;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property InputNeuron: TNeuron read FInputNeuron write FInputNeuron;
    property Value: Double read FValue write FValue;
  end;

  ///<summary>нейрон</summary>
  TNeuron = class(TObject)
  private
    FValue: Double;
    FWeights: TWeightList;
    function GetValue: Double;
    procedure SetValue(const Value: Double);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Weights: TWeightList read FWeights;
    property Value: Double read GetValue write SetValue;
  end;

  ///<summary>Слой - Массив нейронов</summary>
  TNeuronLayer = class(TNeuronList)
  private
    FTypeLayer: TTypeLayer;
  protected
    function GetCreateNeuron: TNeuron;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Compile(const ACount: Integer);
    property TypeLayer: TTypeLayer read FTypeLayer write FTypeLayer;
  end;

type
  ///<summary>нейронная сеть</summary>
  TNeuronNet = class(TObject)
  private
    FNeuronLayers: TNeuronLayerList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure CompileNetWork(const ACounts: array of Integer);
  end;

implementation

function Sigma(const AValue: Double): Double;
begin
  Result := 1 / (1 + Exp((-1 * AValue)));
end;

function DifSigma(const AValue: Double): Double;
begin
  Result := AValue * (1 - AValue);
end;

{ TWeight }

constructor TWeight.Create;
begin
  FInputNeuron := nil;
end;

destructor TWeight.Destroy;
begin

  inherited;
end;

{ TNeuron }

constructor TNeuron.Create;
begin
  FValue := 1;
  FWeights := TWeightList.Create(False);
end;

destructor TNeuron.Destroy;
begin
  FreeAndNil(FWeights);
  inherited;
end;

function TNeuron.GetValue: Double;
var
  xSum, xValue: Double;
  i, iCount: Integer;
begin
  xSum := 0;
  Result := FValue;
  iCount := FWeights.Count;
  if iCount > 0 then
  begin
    for i := 0 to iCount - 1 do
    begin
      xValue := FWeights[i].Value;
      xSum := xSum + xValue;
    end;
    Result := Sigma(xSum);
  end;
end;

procedure TNeuron.SetValue(const Value: Double);
begin
  FWeights.Clear;
  FValue := Value;
end;

{ TNeuronLayer }

constructor TNeuronLayer.Create;
begin

end;

destructor TNeuronLayer.Destroy;
begin

  inherited;
end;

function TNeuronLayer.GetCreateNeuron: TNeuron;
begin
  Result := TNeuron.Create;
end;

procedure TNeuronLayer.Compile(const ACount: Integer);
var
  xNeuron: TNeuron;
begin
  Clear;
  for var i := 0 to ACount - 1 do
  begin
    xNeuron := GetCreateNeuron;
    Self.Add(xNeuron);
  end;
  xNeuron := GetCreateNeuron;
  xNeuron.Value := 1;
  Self.Add(xNeuron);
end;

{ TNeuronNet }

constructor TNeuronNet.Create;
begin
  FNeuronLayers := TNeuronLayerList.Create;
end;

destructor TNeuronNet.Destroy;
begin
  FreeAndNil(FNeuronLayers);
  inherited;
end;

procedure TNeuronNet.CompileNetWork(const ACounts: array of Integer);

  procedure _CretaeNeuron;
  var
    xNeuronLayer: TNeuronLayer;
    i, xL, xH: Integer;
    xCountNeuron: Integer;
  begin
    xL := Low(ACounts);
    xH := High(ACounts);
    for i := xL to xH do
    begin
      xNeuronLayer := TNeuronLayer.Create;
      xNeuronLayer.TypeLayer := TTypeLayer.tlNorm;
      FNeuronLayers.Add(xNeuronLayer);
    end;
  end;

  procedure _InitWeight(const AInput, ALayer: TNeuronLayer);
  var
    i, iCount: Integer;
    j, jCount: Integer;
    xWeight: TWeight;
    xNeuron: TNeuron;
  begin
    iCount := ALayer.Count - 1;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xNeuron := ALayer.Items[i];
        // Перебираем все входящие нероны
        jCount := AInput.Count;
        if jCount > 0 then
          for j := 0 to jCount - 1 do
          begin
            xWeight := TWeight.Create;
            xWeight.Value := (Random(201) - 100) / 100;
            xWeight.InputNeuron := AInput[j];
            xNeuron.Weights.Add(xWeight);
          end;
      end;
  end;

  procedure _InitWeightNeuron;
  var
    xInput, xLayer: TNeuronLayer;
    i, iCount: Integer;
  begin
    iCount := FNeuronLayers.Count;
    if iCount > 0 then
      for i := 1 to iCount - 1 do
      begin
        xInput := FNeuronLayers[i - 1];
        xLayer := FNeuronLayers[i];
        _InitWeight(xInput,xLayer);
      end;
  end;

begin
  FNeuronLayers.Clear;
  _CretaeNeuron;
  _InitWeightNeuron;
end;

end.
