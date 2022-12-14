unit dnk.Operand;

interface

uses
  dnk.Calc;

type
  TOperandNumber = class(TOperand)
  private
    FValue: Double;
  protected
    function GetValue: Double; override;
  public
    constructor Create;  override;
  end;

  TOperandBracket = class(TOperand)
  private
    FOperation: TOperation;
  protected
    function GetValue: Double; override;
  public
    constructor Create;  override;
  end;

implementation

{ TOperandNumber }

constructor TOperandNumber.Create;
begin
  inherited;
  FTypeOperand := TTypeOperand.Number;
end;

function TOperandNumber.GetValue: Double;
begin
  Result := FValue;
end;

end.
