{todo: ���� ��� ��������� ��� � ��� ��� ���������}
unit Lb.NeuronNet.SysUtils;

interface

uses
  System.SysUtils,
  System.Classes;

type
  TLineMatrix = array of Double;

  ///<summary>����������� ���������� ��������</summary>
  ///<remarks>����������� (N - �������,M - �����)</remarks>
  TMatrix = record
    Values: array of array of Double;
  private
    FCountN, FCountM: Integer;
    function GetCells(ACol, ARow: Integer): Double;
    procedure SetCells(ACol, ARow: Integer; const Value: Double);
    function GetLineMatrix: TLineMatrix;
    procedure SetLineMatrix(const Value: TLineMatrix);
  public

    procedure RandomValue;

    procedure IsCheckMatrixSize;
    ///<summary>������������� ����������� �������</summary>
    procedure MatrixSize(const ACountN, ACountM: Integer);
    ///<summary>���������� ��������� �� ������ ��� ������� � ������ </summary>
    property Cells[ACol, ARow: Integer]: Double read GetCells write SetCells;
    ///<summary>������� �������� � � ������������� � ������� ����</summary>
    property LineMatrix: TLineMatrix read GetLineMatrix write SetLineMatrix;
    property CountN: Integer read FCountN;
    property CountM: Integer read FCountM;
  end;

implementation

function Multiplication(A, B: TMatrix): TMatrix;
var
  n, m: Integer;
begin
  if A.CountM = B.CountN then
  begin
    {todo: }
  end
  else
    raise Exception.Create('Error Message');
end;

{ TMatrix }

procedure TMatrix.MatrixSize(const ACountN, ACountM: Integer);
begin
  FCountN := ACountN;
  FCountM := ACountM;
  SetLength(Values, ACountN, ACountM);
end;

procedure TMatrix.IsCheckMatrixSize;
begin
  if (FCountN <= 0) and (FCountM <= 0) then
    Exception.Create('Error Message: �� ����������� ������� �������');
end;

function TMatrix.GetCells(ACol, ARow: Integer): Double;
begin
  IsCheckMatrixSize;
  Result := Values[ACol,ARow];
end;

procedure TMatrix.SetCells(ACol, ARow: Integer; const Value: Double);
begin
  IsCheckMatrixSize;
  Values[ACol,ARow] := Value;
end;

function TMatrix.GetLineMatrix: TLineMatrix;
var
  n, m, xInd: Integer;
  xLineCount: Integer;
  xLineMatrix: TLineMatrix;
begin
  IsCheckMatrixSize;
  xLineCount := FCountN * FCountM;
  SetLength(xLineMatrix, xLineCount);
  for n := 0 to FCountN - 1 do
    for m := 0 to FCountM - 1 do
    begin
      xInd := n * m + m;
      xLineMatrix[xInd] := Values[n,m];
    end;
end;

procedure TMatrix.SetLineMatrix(const Value: TLineMatrix);
var
  n, m, xInd: Integer;
  xLineCount: Integer;
  xLineMatrix: TLineMatrix;
begin
  IsCheckMatrixSize;
  xLineCount := Length(Value);
  if xLineCount <> (FCountN * FCountM) then
    raise Exception.Create('Error Message: �� ������������� ����������� ������� ');
  for n := 0 to FCountN - 1 do
    for m := 0 to FCountM - 1 do
    begin
      xInd := n * m + m;
      Values[n,m] := xLineMatrix[xInd];
    end;
end;

procedure TMatrix.RandomValue;
var
  xV: Double;
  n, m: Integer;
begin
  IsCheckMatrixSize;
  for n := 0 to FCountN - 1 do
    for m := 0 to FCountM - 1 do
    begin
      xV := (100 - Random(201))/200;
      xV := Trunc(100 * xV)/100;
      Values[n,m] := xV;
    end;
end;

end.
