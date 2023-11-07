unit BackProp;

// (c) BaseGroup Lab. 2000
//
// ��������� ����, ��������� �� �������� ���������
// ��������������� ������. ������� �������� XOR.

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, NeuralBaseComp, NeuralBaseTypes, ComCtrls, Spin;

type
  TForm1 = class(TForm)
    NeuralNetBP: TNeuralNetBP;
    btnTeach: TButton;
    Memo: TMemo;
    prbEpoch: TProgressBar;
    speEpochCount: TSpinEdit;
    Label3: TLabel;
    Label4: TLabel;
    sttError: TStaticText;
    procedure btnTeachClick(Sender: TObject);
    procedure NeuralNetBPEpochPassed(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

// ������������� ����
procedure TForm1.FormActivate(Sender: TObject);
var
  xInputVector: TVectorFloat;   // ������� ������
  xOutputVector: TVectorFloat;  // ������� ������
begin
  // �������� �������� � ��������
  NeuralNetBP.ResetPatterns;
  SetLength(xInputVector, 2);
  SetLength(xOutputVector, 1);

  // ���������� � ���� ������� ��� ��������
  // ������� ���� ������������� ������� XOR
  //
  //  1 �������� | 2 �������� | ���������
  //  -----------------------------------
  //      1           1            0
  //      0           1            1
  //      1           0            1
  //      0           0            0

  xInputVector[0] := 1;
  xInputVector[1] := 1;
  xOutputVector[0] := 0;
  NeuralNetBP.AddPattern(xInputVector, xOutputVector);

  xInputVector[0] := 1;
  xInputVector[1] := 0;
  xOutputVector[0] := 1;
  NeuralNetBP.AddPattern(xInputVector, xOutputVector);

  xInputVector[0] := 0;
  xInputVector[1] := 1;
  xOutputVector[0] := 1;
  NeuralNetBP.AddPattern(xInputVector, xOutputVector);

  xInputVector[0] := 0;
  xInputVector[1] := 0;
  xOutputVector[0] := 0;
  NeuralNetBP.AddPattern(xInputVector, xOutputVector);
end;

// �������� ����
procedure TForm1.btnTeachClick(Sender: TObject);
var
  xInputVector: TVectorFloat;   // ������� ������
begin
  NeuralNetBP.EpochCount := speEpochCount.Value;
  prbEpoch.Max := NeuralNetBP.EpochCount;
  prbEpoch.Position := 0;

  // ������ �������� �������� (offline)
  NeuralNetBP.TeachOffLine;

  SetLength(xInputVector, 2);
  Memo.Lines.Clear;

  // ����� ����������
  xInputVector[0] := 1;                 // ���� ���������� �� ����
  xInputVector[1] := 1;                 // ���� ���������� �� ����
  NeuralNetBP.Compute(xInputVector);    // ������ ���� ������
  Memo.Lines.Add(FloatToStr(xInputVector[0]) + ' XOR ' +
                 FloatToStr(xInputVector[1]) + ' = ' +
                 FloatToStr(NeuralNetBP.Output[0]));     // �������� ���������

  xInputVector[0] := 1;
  xInputVector[1] := 0;
  NeuralNetBP.Compute(xInputVector);    // ������ ���� ������
  Memo.Lines.Add(FloatToStr(xInputVector[0]) + ' XOR ' +
                 FloatToStr(xInputVector[1]) + ' = ' +
                 FloatToStr(NeuralNetBP.Output[0]));     // �������� ���������

  xInputVector[0] := 0;
  xInputVector[1] := 1;
  NeuralNetBP.Compute(xInputVector);    // ������ ���� ������
  Memo.Lines.Add(FloatToStr(xInputVector[0]) + ' XOR ' +
                 FloatToStr(xInputVector[1]) + ' = ' +
                 FloatToStr(NeuralNetBP.Output[0]));     // �������� ���������

  xInputVector[0] := 0;
  xInputVector[1] := 0;
  NeuralNetBP.Compute(xInputVector);    // ������ ���� ������
  Memo.Lines.Add(FloatToStr(xInputVector[0]) + ' XOR ' +
                 FloatToStr(xInputVector[1]) + ' = ' +
                 FloatToStr(NeuralNetBP.Output[0]));     // �������� ���������
end;

// ����������� �������� ��������
procedure TForm1.NeuralNetBPEpochPassed(Sender: TObject);
begin
  prbEpoch.Position := prbEpoch.Position + 1;
  sttError.Caption := FloatToStr(NeuralNetBP.TeachError);
  Application.ProcessMessages;
end;


end.
