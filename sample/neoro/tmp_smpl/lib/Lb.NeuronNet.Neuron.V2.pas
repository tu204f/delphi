unit Lb.NeuronNet.Neuron.V2;

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
  ///<summary>Значение весов</summary>
  TWeightValues = array of array of Double;

  TDoubleList = class(TList<Double>)
  public
    procedure GetArrayValue(const AValues: TArray<Double>);
  end;


type
  ///<summary>
  /// Слой
  ///</summary>
  TLayer = class(TObject)
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

function GetWeights(const AInValues, AOutValues: TDoubleList): TWeightValues;

implementation

function GetWeights(const AInValues, AOutValues: TDoubleList): TWeightValues;
var
  xWeightValues: TWeightValues;
begin
  SetLength(xWeightValues,AInValues.Count,AOutValues.Count);
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

{ TLayer }

constructor TLayer.Create;
begin

end;

destructor TLayer.Destroy;
begin

  inherited;
end;

end.
