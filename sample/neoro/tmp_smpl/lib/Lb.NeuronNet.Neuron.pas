unit Lb.NeuronNet.Neuron;

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
  TEquations = function(const AValue: Double): Double;

  TDoubleList = class(TList<Double>)
  public
    procedure GetArrayValue(const AValues: TArray<Double>);
  end;

type
  TLink = class;
  TNeuron = class;
  TLayer = class;
  TNeuronNet = class;

  ///<summary>
  /// Объект связи
  ///</summary>
  TLink = class(TObject)
  private
    FNeuron: TNeuron;
    FParent: TNeuron;
    FWeight: Double;
  public
    constructor Create(const AParent, ANeuron: TNeuron); virtual;
    destructor Destroy; override;
    ///<summary>
    /// Связанные нейроны
    ///</summary>
    property Neuron: TNeuron read FNeuron;
    ///<summary>
    /// Принадлежность
    ///</summary>
    property Parent: TNeuron read FParent;
    ///<summary>
    /// Весь связи
    ///</summary>
    property Weight: Double read FWeight write FWeight;
  end;
  TLinkList = TObjectList<TLink>;

  ///<summary>
  /// Класс, который реализует нейрон
  ///</summary>
  TNeuron = class(TObject)
  private
    FValue: Double;
    FLinks: TLinkList;
    FErrorValue: Double;
    function GetValue: Double;
    procedure SetValue(const Value: Double);
  private
    FOutputLinks: TLinkList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddLinkNeuron(const ANeuron: TNeuron): TLink;
    property Links: TLinkList read FLinks;
    property Value: Double read GetValue write SetValue;
  public
    property OutputLinks: TLinkList read FOutputLinks;
    property ErrorValue: Double read FErrorValue write FErrorValue;
  end;
  TNeuronList = TObjectList<TNeuron>;

  ///<summary>
  /// Нейронный слой
  ///</summary>
  TLayer = class(TObject)
  private
    FID: Integer;
    FNeurons: TNeuronList;
    FNeuronNet: TNeuronNet;
  public
    constructor Create(ANeuronNet: TNeuronNet; ANeuronCount: Integer; AOutput: Boolean = True); virtual;
    destructor Destroy; override;
    procedure Clear;
    property ID: Integer read FID;
    property Neurons: TNeuronList read FNeurons;
  end;
  TLayerList = TObjectList<TLayer>;

  ///<summary>
  /// Нейронная сеть
  ///</summary>
  TNeuronNet = class(TObject)
  private
    FLayers: TLayerList;
  protected
    procedure CreateLayerLink(const AInput, ALayer: TLayer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function AddLayer(const ANeuronCount: Integer): TLayer;
    procedure OutputLayer(const ANeuronCount: Integer);
    property Layers: TLayerList read FLayers;
  public
    ///<summary>
    /// Вычисляем значение работы сети: Выходное значение AOutputValues
    ///</summary>
    procedure Calculate(const AOutputValues: TDoubleList);
    ///<summary>
    /// Вычисляем отклонение ошибки для обучение работы сети
    ///</summary>
    procedure CalculateError(const AStandardValues, AResultValues: TDoubleList);
    ///<summary>
    /// Обучение сети
    ///</summary>
    procedure CalculateLearn(AStepValue: Double);
  public
    procedure Calc(AInPut, AOutPut: TDoubleList);
    procedure CalcLearn(AStandard, AOutPut: TDoubleList; AStepValue: Double);
  end;

implementation

(******************************************************************************)
(* Тестовая функция активации - линейная                                      *)
(******************************************************************************)

function TestAct(const AValue: Double): Double;

  function fun_y(const x: Double): Double;
  begin
    Result := x;
  end;

var
  xR: Double;
begin
  xR := fun_y(AValue);
  if xR >= 1 then
    xR := 1
  else if xR <= -1 then
    xR := -1;
  Result := xR;
end;

function DifTestAct(const AValue: Double): Double;
var
  xV: Double;
begin
  xV := TestAct(AValue);
  Result := 1 - xV * xV;
end;


(******************************************************************************)
(* Тождественная                                                              *)
(******************************************************************************)

function Line(const AValue: Double): Double;
begin
  if (AValue >= 1) then
    Result := 1
  else if (AValue <= -1) then
    Result := -1
  else
    Result := AValue;
end;

function DifLine(const AValue: Double): Double;
var
  xV: Double;
begin
  xV := Line(AValue);
  Result := 1 - xV * xV;
end;

(******************************************************************************)
(* Сигмойд  от 0 до 1                                                         *)
(******************************************************************************)

function Sigma(const AValue: Double): Double;
begin
  Result := 1 / (1 + Exp((-1 * AValue)));
end;

function DifSigma(const AValue: Double): Double;
begin
  Result := AValue * (1 - AValue);
end;

(******************************************************************************)
(* Гиперболический тангес
(******************************************************************************)

function Th(const AValue: Double): Double;
begin
  Result := (Exp(AValue) - Exp(-1 * AValue))/(Exp(AValue) + Exp(-1 * AValue));
end;

function DifTh(const AValue: Double): Double;
var
  xV: Double;
begin
  xV := Th(AValue);
  Result := 1 - xV * xV;
end;


var
  localEquations: TEquations = Sigma;
  localDefEquations: TEquations = DifSigma;

function GetValueNeuron(ALinks: TLinkList): Double;
var
  xSum: Double;
  xLink: TLink;
  i, iCount: Integer;
begin
  xSum := 0;
  iCount := ALinks.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xLink := ALinks[i];
      xSum := xSum + xLink.Neuron.GetValue * xLink.Weight;
    end;
  Result := localEquations(xSum);
end;

procedure SetErrorStandard(AStandardValues, AValues: TDoubleList; ANeurons: TNeuronList);
var
  xS: String;
  i, iCount: Integer;
  xDeltaError, xValue: Double;
begin
  if AStandardValues.Count = ANeurons.Count then
  begin
    iCount := AStandardValues.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xValue := AValues[i];
        xDeltaError :=  AStandardValues[i] - xValue;
        ANeurons[i].ErrorValue := xDeltaError * localDefEquations(xValue);
      end;
  end
  else
  begin
    xS := 'Количество Эталонного значение не соответствует, ' +
          'выходного количеству нейронов.';
    raise Exception.Create(xS);
  end;
end;

procedure SetErrorStandardValue(ANeurons: TNeuronList);
var
  xSum: Double;
  xLink: TLink;
  xNeuron: TNeuron;
begin
  for xNeuron in ANeurons do
  begin
    xSum := 0;
    for xLink in xNeuron.OutputLinks do
      xSum := xSum + xLink.Weight * xLink.Parent.ErrorValue;
    xNeuron.ErrorValue := xSum * localDefEquations(xNeuron.Value);
  end;
end;

{ TDoubleList }

procedure TDoubleList.GetArrayValue(const AValues: TArray<Double>);
var
  xValue: Double;
begin
  Self.Clear;
  for xValue in AValues do
    Self.Add(xValue);
end;

{ TLink }

constructor TLink.Create(const AParent, ANeuron: TNeuron);

  function _RandonValueWeight: Double;
  begin
    Result := (Random(201) - 100) / 100;
  end;

begin
  FNeuron := ANeuron;
  FParent := AParent;
  FWeight := _RandonValueWeight;
end;

destructor TLink.Destroy;
begin

  inherited;
end;

{ TNeuron }

constructor TNeuron.Create;
begin
  FLinks := TLinkList.Create;
  FOutputLinks := TLinkList.Create(False);
end;

destructor TNeuron.Destroy;
begin
  FreeAndNil(FOutputLinks);
  FreeAndNil(FLinks);
  inherited;
end;

function TNeuron.GetValue: Double;
var
  iCount: Integer;
begin
  iCount := FLinks.Count;
  if iCount > 0 then
    Result := GetValueNeuron(FLinks)
  else
    Result := FValue;
end;

procedure TNeuron.SetValue(const Value: Double);
begin
  FValue := Value;
  FLinks.Clear;
end;

function TNeuron.AddLinkNeuron(const ANeuron: TNeuron): TLink;
var
  xLink: TLink;
begin
  xLink := TLink.Create(Self,ANeuron);
  Result := xLink;
  FLinks.Add(xLink);
end;

{ TLayer }

constructor TLayer.Create(ANeuronNet: TNeuronNet; ANeuronCount: Integer; AOutput: Boolean = True);
var
  xNeuron: TNeuron;
begin
  FNeuronNet := ANeuronNet;
  FID := FNeuronNet.Layers.Count;
  FNeurons := TNeuronList.Create;
  for var i := 0 to ANeuronCount - 1 do
  begin
    xNeuron := TNeuron.Create;
    FNeurons.Add(xNeuron);
  end;

  if AOutput then
  begin
    // Базовый нейрон
    if FNeurons.Count > 0 then
    begin
      xNeuron := TNeuron.Create;
      xNeuron.Value := 1;
      FNeurons.Add(xNeuron);
    end;
  end;
end;

destructor TLayer.Destroy;
begin
  FreeAndNil(FNeurons);
  inherited;
end;

procedure TLayer.Clear;
begin
  FNeurons.Clear;
end;

{ TNeuronNet }

constructor TNeuronNet.Create;
begin
  FLayers := TLayerList.Create;
end;

destructor TNeuronNet.Destroy;
begin
  FreeAndNil(FLayers);
  inherited;
end;

procedure TNeuronNet.Clear;
begin
  FLayers.Clear;
end;

procedure TNeuronNet.CreateLayerLink(const AInput, ALayer: TLayer);
var
  i, iCount: Integer;
  j, jCount: Integer;
  xLink: TLink;
  xInput: TNeuron;
  xNeuron: TNeuron;
begin
  iCount := ALayer.Neurons.Count;
  if iCount > 0 then
  begin
    if iCount > 1 then
      iCount := iCount - 1;
    for i := 0 to iCount - 1 do
    begin
      xNeuron := ALayer.Neurons[i];
      jCount := AInput.Neurons.Count;
      if jCount > 0 then
      begin
        for j := 0 to jCount - 1 do
        begin
          xInput := AInput.Neurons[j];
          if j = (jCount - 1) then
          begin
            xNeuron.AddLinkNeuron(xInput);
          end
          else
          begin
            xLink := xNeuron.AddLinkNeuron(xInput);
            xInput.OutputLinks.Add(xLink);
          end;
        end;
      end;
    end;
  end;
end;

function TNeuronNet.AddLayer(const ANeuronCount: Integer): TLayer;
var
  xLayer: TLayer;
begin
  xLayer := TLayer.Create(Self,ANeuronCount);
  Result := xLayer;
  if FLayers.Count > 0 then
    CreateLayerLink(FLayers[FLayers.Count - 1],xLayer);
  FLayers.Add(xLayer);
end;

procedure TNeuronNet.OutputLayer(const ANeuronCount: Integer);
var
  xLayer: TLayer;
begin
  xLayer := TLayer.Create(Self,ANeuronCount,False);
  if FLayers.Count > 0 then
    CreateLayerLink(FLayers[FLayers.Count - 1],xLayer);
  FLayers.Add(xLayer);
end;


procedure TNeuronNet.Calculate(const AOutputValues: TDoubleList);
var
  xLayer: TLayer;
  xNeuron: TNeuron;
begin
  AOutputValues.Clear;
  xLayer := FLayers.Items[FLayers.Count - 1];
  for xNeuron in xLayer.Neurons do
    AOutputValues.Add(xNeuron.Value);
end;

procedure TNeuronNet.CalculateError(const AStandardValues, AResultValues: TDoubleList);
var
  xLayer: TLayer;
  i, iCount: Integer;
begin
  iCount := FLayers.Count;
  if iCount > 0 then
    for i := iCount - 1 downto 0 do
    begin
      xLayer := FLayers.Items[i];
      if i = (iCount - 1) then
        SetErrorStandard(AStandardValues, AResultValues, xLayer.Neurons)
      else
        SetErrorStandardValue(xLayer.Neurons);
    end;
end;

procedure TNeuronNet.CalculateLearn(AStepValue: Double);

  procedure _Learn(const ALayer: TLayer; AStepValue: Double);
  var
    xLink: TLink;
    xNeuron: TNeuron;
  begin
    for xNeuron in ALayer.Neurons do
    begin
      for xLink in xNeuron.Links do
      begin
        xLink.Weight :=
          xLink.Weight +
          AStepValue * xLink.Neuron.Value * xLink.Parent.ErrorValue;
      end;
    end;
  end;

var
  xLayer: TLayer;
  i, iCount: Integer;
begin
  iCount := FLayers.Count;
  if iCount > 0 then
    for i := 1 to iCount - 1 do
    begin
      xLayer := FLayers[i];
      _Learn(xLayer,AStepValue);
    end;
end;

procedure TNeuronNet.Calc(AInPut, AOutPut: TDoubleList);
var
  xLayer: TLayer;
  i, iCount: Integer;
begin
  if FLayers.Count = 0 then
    raise Exception.Create('Error Message: Нейронная сеть на задана');

  xLayer := FLayers[0];
  if (xLayer.Neurons.Count >= AInPut.Count) then
  begin
    iCount := AInPut.Count;
    for i := 0 to iCount - 1 do
      xLayer.Neurons[i].Value := AInPut[i];
    Calculate(AOutPut);
  end
  else
  begin
    var xS := 'Количество входящих значение ' +
              'не соответствует входящих нейронов';
    raise Exception.Create('Error Message: ' + xS);
  end;
end;

procedure TNeuronNet.CalcLearn(AStandard, AOutPut: TDoubleList; AStepValue: Double);
begin
  CalculateError(AStandard,AOutPut);
  CalculateLearn(AStepValue);
end;

end.
