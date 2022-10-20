unit Lb.Neoron;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  TInputValue = record
    Value: Double;
    Weight: Double;
  public
    function GetMultiplication: Double;
  end;

  TInputValues = TList<TInputValue>;

  TTypeFunctionActive = (tfaSigmoid,tfaBipolar);

  TNeoron = class(TObject)
  private
    FFunctionActive: TTypeFunctionActive;
    FInputValues: TInputValues;
    function GetSum: Double;
    function GetOutputValue: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property OutputValue: Double read GetOutputValue;
    property InputValues: TInputValues read FInputValues;
    property FunctionActive: TTypeFunctionActive read FFunctionActive write FFunctionActive;
  end;

implementation

function GetSigmoid(const AValue: Double): Double;
var
  xValue: Double;
begin
  xValue := -1 * AValue;
  Result := 1/(1 + Exp(xValue));
end;

function GetBipolar(const AValue: Double): Double;
var
  xValue: Double;
begin
  xValue := 2 * AValue;
  Result := (Exp(xValue) - 1)/(Exp(xValue) + 1);
end;

{ TInputValue }

function TInputValue.GetMultiplication: Double;
begin
  Result := Value * Weight;
end;

{ TNeoron }

constructor TNeoron.Create;
begin
  FFunctionActive := tfaSigmoid;
  FInputValues := TInputValues.Create;
end;

destructor TNeoron.Destroy;
begin
  FreeAndNil(FInputValues);
  inherited;
end;

function TNeoron.GetOutputValue: Double;
var
  xValue: Double;
begin
  xValue := GetSum;
  case FFunctionActive of
    tfaSigmoid: Result := GetSigmoid(xValue);
    tfaBipolar: Result := GetBipolar(xValue);
  end;
end;

function TNeoron.GetSum: Double;
var
  xSum: Double;
  xInputValue: TInputValue;
begin
  xSum := 0;
  for xInputValue in FInputValues do
    xSum := xSum + xInputValue.GetMultiplication;
  Result := xSum;
end;

end.
