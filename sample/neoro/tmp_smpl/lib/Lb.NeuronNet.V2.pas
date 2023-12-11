unit Lb.NeuronNet.V2;

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
  TDoubleList = TList<Double>;

  TTypeNeuron = (
    tnNull,
    tnNeuron,
    tnBayes,  // ��� �������� ������ ������ ������� ��������� "1"
    tnInput,  // �������� ������
    tnOutput  // ���������
  );
  TTypeDirection = (tdDirect, tdReverse);

  TCustomNeuron = class(TObject)
  private
    FValue: Double;
    FTypeNeuron: TTypeNeuron;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>�������� ������� ��������� � �������</summary>
    property Value: Double read FValue write FValue;
    ///<summary>��� �������</summary>
    property TypeNeuron: TTypeNeuron read FTypeNeuron;
  end;

  ///<summary>�������� ������</summary>
  TBayesNeuron = class(TCustomNeuron)
  private
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  ///<summary>��� ��� �����</summary>
  TWeight = class(TObject)
  private
    FValue: Double;
    FNeuron: TCustomNeuron;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>�������� �����</summary>
    property Neuron: TCustomNeuron read FNeuron;
    ///<summary>�������� ����</summary>
    property Value: Double read FValue write FValue;
    ///<summary>��������� ��������</summary>
    function Calculate: Double;
  end;

  ///<summary>������ �����</summary>
  TWeightList = TObjectList<TWeight>;

  ///<summary>������</summary>
  TNeuron = class(TCustomNeuron)
  private
    FDirection: TTypeDirection;
    FWeights: TWeightList;
  protected
    function GetActive(const AValue: Double): Double;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Weights: TWeightList read FWeights;
    ///<summary>��������� ��������</summary>
    function Calculate: Double;
    procedure CalculateLearn(AValue: Double);
    ///<summary>����������� ����������</summary>
    property Direction: TTypeDirection read FDirection write FDirection;
  end;

  ///<summary>�������� ������</summary>
  TInputNeuron = class(TBayesNeuron)
  private
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  ///</summary>��������� ������</summary>
  TOutputNeoton = class(TNeuron)
  private
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  ///<summary>������ �������� ��������</summary>
  TInputNeuronList  = TObjectList<TInputNeuron>;
  ///<summayr>������ ��������� ��������</summary>
  TOutputNeotonList = TObjectList<TOutputNeoton>;
  ///<summary>������  ��������</summary>
  TNeuronList       = TObjectList<TNeuron>;

  {todo: ������� ��� ���� ������ � ��������. �������� ������������ ��� ��������}


implementation

{ TCustomNeuron }

constructor TCustomNeuron.Create;
begin
  FValue := 1;
  FTypeNeuron := TTypeNeuron.tnNull;
end;

destructor TCustomNeuron.Destroy;
begin

  inherited;
end;

{ TBayesNeuron }

constructor TBayesNeuron.Create;
begin
  inherited;
  FValue := 1;
  FTypeNeuron := TTypeNeuron.tnBayes;
end;

destructor TBayesNeuron.Destroy;
begin

  inherited;
end;

{ TWeight }

constructor TWeight.Create;
begin
  FNeuron := nil;
end;

destructor TWeight.Destroy;
begin

  inherited;
end;

function TWeight.Calculate: Double;
begin
  Result := 0;
  if Assigned(FNeuron) then
    Result := FNeuron.Value * FValue;
end;

{ TNeuron }

constructor TNeuron.Create;
begin
  inherited;
  FDirection := TTypeDirection.tdDirect;
  FWeights := TWeightList.Create;
  FTypeNeuron := TTypeNeuron.tnNeuron;
end;

destructor TNeuron.Destroy;
begin
  FreeAndNil(FWeights);
  inherited;
end;

function TNeuron.GetActive(const AValue: Double): Double;
begin
  // ������� ���������
  case FDirection of
    tdDirect: Result := 1 / (1 + Exp((-1 * AValue)));
    tdReverse: Result := AValue * (1 - AValue);
  else
    Result := 0;
  end;
end;

function TNeuron.Calculate: Double;
var
  xSum: Double;
  i, iCount: Integer;
begin
  xSum := 0;
  iCount := FWeights.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
      xSum := xSum + FWeights[i].Calculate;
  Result := GetActive(xSum);
end;

procedure TNeuron.CalculateLearn(AValue: Double);
begin

end;

{ TInputNeuron }

constructor TInputNeuron.Create;
begin
  inherited;
  FTypeNeuron := TTypeNeuron.tnInput;
end;

destructor TInputNeuron.Destroy;
begin

  inherited;
end;

{ TOutputNeoton }

constructor TOutputNeoton.Create;
begin
  inherited;
  FTypeNeuron := TTypeNeuron.tnOutput;
end;

destructor TOutputNeoton.Destroy;
begin

  inherited;
end;

end.
