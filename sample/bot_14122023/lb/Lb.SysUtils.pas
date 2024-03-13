unit Lb.SysUtils;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants;

type
  TCandel = record
    Open : Double;
    High : Double;
    Low  : Double;
    Close: Double;
    Vol  : Double;
  end;

  TTiket = record
    Time: Integer; // ��������
    Last: Double;
    Vol: Double;
  end;

  TParam = record
    T: Double;  // �������� 1 - ����, 0 - �������, 0,5 - �� ���� �� ����
    H: Double;  // ������ ����
    B: Double;  // ����
    L: Double;  // ����� ����
  public
    function GetSum: Double;
  end;

function GetParamToCandel(const ACandel: TCandel): TParam;
//function GetRandomMode: TMode;

implementation

//function GetRandomMode: TMode;
//var
//  xMode: Integer;
//begin
//  // ����� ������ ��������� �������
//  xMode := Random(3);
//  Result := TMode(xMode);
//end;


function GetParamToCandel(const ACandel: TCandel): TParam;
var
  xParam: TParam;
  xHL: Double;
begin
  xHL := ACandel.High - ACandel.Low;
  if xHL > 0 then
  begin
    if ACandel.Open < ACandel.Close then
    begin
      // ����� ������
      xParam.T := 1;
      xParam.H := (ACandel.High  - ACandel.Close)/xHL;
      xParam.B := (ACandel.Close - ACandel.Open)/xHL;
      xParam.L := (ACandel.Open  - ACandel.Low)/xHL;
    end else if ACandel.Open > ACandel.Close then
    begin
      // ������� �����
      xParam.T := 0;
      xParam.H := (ACandel.High  - ACandel.Open)/xHL;
      xParam.B := (ACandel.Open  - ACandel.Close)/xHL;
      xParam.L := (ACandel.Close - ACandel.Low)/xHL;
    end else
    begin
      // ���� �������� ������ ���� ��������
      xParam.T := 0.5;
      xParam.H := (ACandel.High - ACandel.Open)/xHL;
      xParam.B := 0;
      xParam.L := (ACandel.Open - ACandel.Low)/xHL;
    end;
    Result := xParam;
  end
  else
    raise Exception.Create('Error Message: ����� ������ ������');
end;

{ TParam }

function TParam.GetSum: Double;
begin
  // ��� ��������� ���������
  Result := H + B + L;
end;

end.
