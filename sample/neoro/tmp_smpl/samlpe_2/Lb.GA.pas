(******************************************************************************)
(* Алгоритм обучение нейронной сети                                           *)
(******************************************************************************)
unit Lb.GA;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Contnrs,
  System.Generics.Collections,
  Lb.NeuronNet;

type
  TBeing = class;
  TBeingClass = class of TBeing;

  TBeingList = TObjectList<TBeing>;

  ///<summary>Геном</summary>
  TDoubleList = TList<Double>;

  ///<summary>Особъ - жизень всех формах проявления</summary>
  TBeing = class(TObject)
  private
    FNeuronNet: TNeuronNet; // Нейроная есть
    FGenomes: TDoubleList;  // Геном
    FBalls: Integer;        // Балы, жизни
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>Сформировать геном существа </summary>
    procedure SetСompleteGenomes;
    ///<summary>Чтение генома в нейронную сеть</summary>
    ///<remarks>Записываем значение генов в Нейронную сеть</remarks>
    procedure SetReadingGenomes;
    ///<summary>Сеть</summary>
    property NeuronNet: TNeuronNet read FNeuronNet;
    ///<summary>Геном нейронной сети</summary>
    property Genomes: TDoubleList read FGenomes;
    ///<summary>Количество полученных балов</summary>
    procedure SetBalls(const AValueBall: Integer);
    ///<summary>Количество полученных балов</summary>
    property Balls: Integer read FBalls;
  end;

  TGeneticAlgorithm = class(TObject)
  private
    FBeingClass: TBeingClass;
    FBeings: TBeingList;
    function GetCountBeing: Integer;
    procedure SetCountBeing(const AValue: Integer);
  protected

  public
    constructor Create(const ABeingClass: TBeingClass); virtual;
    destructor Destroy; override;
    ///<summary>Размер популяции</summary>
    property CountBeing: Integer read GetCountBeing write SetCountBeing;
  end;

  ///<summary></summary>
  TBeingXOR = class(TBeing)
  private
    FError: Double;
    ///<summary>возвращает значение, ошибки</summary>
    function GetXOR(const A, B, C: Byte): Double;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Calculate;
    property Error: Double read FError;
  end;


implementation

uses
  MAth;

{ TBeing }

constructor TBeing.Create;
begin
  FNeuronNet := TNeuronNet.Create;
  FGenomes := TDoubleList.Create;
end;

destructor TBeing.Destroy;
begin
  FreeAndNil(FGenomes);
  FreeAndNil(FNeuronNet);
  inherited;
end;

procedure TBeing.SetBalls(const AValueBall: Integer);
begin
  FBalls := FBalls + AValueBall;
end;

procedure TBeing.SetReadingGenomes;

  function _WeightSetBuild(AWeights: TWeights; APosition: Integer): Integer;
  var
    xW: Double;
    i, iCount: Integer;
    j, jCount: Integer;
    xPosition: Integer;
  begin
    xPosition := APosition;
    iCount := AWeights.InputNeurons.Count;
    jCount := AWeights.OutputNeurons.Count;
    if (iCount > 0) and (jCount > 0) then
      for i := 0 to iCount - 1 do
        for j := 0 to jCount - 1 do
        begin
          xW := FGenomes[xPosition];
          AWeights.Cells[i,j] := xW;
          Inc(xPosition);
        end;
    Result := xPosition;
  end;

var
  xWeights: TWeights;
  i, iCount: Integer;
  xPosition: Integer;
begin
  i := 0;
  xPosition := 0;
  iCount := FNeuronNet.WeightsCount;
  while i < iCount do
  begin
    xWeights := FNeuronNet.Weights[i];
    xPosition := _WeightSetBuild(xWeights,xPosition);
    Inc(i);
  end;
end;

procedure TBeing.SetСompleteGenomes;

  procedure _WeightSetBuild(AWeights: TWeights);
  var
    xW: Double;
    i, iCount: Integer;
    j, jCount: Integer;
  begin
    iCount := AWeights.InputNeurons.Count;
    jCount := AWeights.OutputNeurons.Count;
    if (iCount > 0) and (jCount > 0) then
      for i := 0 to iCount - 1 do
        for j := 0 to jCount - 1 do
        begin
          xW := AWeights.Cells[i,j];
          FGenomes.Add(xW);
        end;
  end;

var
  xWeights: TWeights;
  i, iCount: Integer;
begin
  FGenomes.Clear;
  iCount := FNeuronNet.WeightsCount;
  for i := 0 to iCount - 1 do
  begin
    xWeights := FNeuronNet.Weights[i];
    _WeightSetBuild(xWeights);
  end;
end;

{ TGeneticAlgorithm }

constructor TGeneticAlgorithm.Create(const ABeingClass: TBeingClass);
begin
  FBeingClass := ABeingClass;
  FBeings := TBeingList.Create(True);
end;

destructor TGeneticAlgorithm.Destroy;
begin
  FreeAndNil(FBeings);
  inherited;
end;

function TGeneticAlgorithm.GetCountBeing: Integer;
begin
  Result := FBeings.Count;
end;

procedure TGeneticAlgorithm.SetCountBeing(const AValue: Integer);
var
  xBeing: TBeing;
begin
  FBeings.Clear;
  for var i := 0 to AValue - 1 do
  begin
    {todo: порождаем объект}
    xBeing := FBeingClass.Create;
    FBeings.Add(xBeing);
  end;
end;

{ TBeingXOR }

constructor TBeingXOR.Create;
begin
  inherited;

end;

destructor TBeingXOR.Destroy;
begin

  inherited;
end;

function TBeingXOR.GetXOR(const A, B, C: Byte): Double;
var
  xV: Double;
begin
  NeuronNet.InputNeurons.Values[0] := A;
  NeuronNet.InputNeurons.Values[1] := B;
  NeuronNet.Calculate;
  xV := NeuronNet.OutputNeurons.Values[0];
  Result := C - xV;
end;

procedure TBeingXOR.Calculate;
var
  xE: Double;
begin
  xE := Power(GetXOR(0,0,0),2) +
        Power(GetXOR(0,1,1),2) +
        Power(GetXOR(1,0,1),2) +
        Power(GetXOR(1,1,0),2);
  FError := Sqrt(xE);
end;

end.
