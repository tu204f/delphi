unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  VclTee.TeeGDIPlus, VclTee.TeEngine, VclTee.Series, VclTee.TeeProcs,
  VclTee.Chart;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Label1: TLabel;
    Button2: TButton;
    Chart1: TChart;
    Series1: TLineSeries;
    Label2: TLabel;
    Button3: TButton;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const  //������� ���������
  CInp = 28 * 28 + 1;
  CHid = 20;
  CHid2 = 20;
  COut = 10;

type
  FMnistImg = array [1 .. 28, 1 .. 28] of Single;

  TNeuralNet = class
    InL: Array [1 .. CInp] of Single; //������� ����
    HL: Array [1 .. CHid] of Single; //������ ������� ����
    HL2: Array [1 .. CHid2] of Single; //������ ������� ����
    OutL: Array [1 .. COut] of Single; //�������� ����

    C1: Array [1 .. CInp, 1 .. CHid] of Single; // ����� 1-2 ����
    C2: Array [1 .. CHid, 1 .. CHid2] of Single; // ����� 2-3 ����
    C3: Array [1 .. CHid2, 1 .. COut] of Single; // ����� 3-4 ����

    Etalon: Array [1 .. COut] of Single; // ���������� ����� (������)

    ErrOut: Array [1 .. COut] of Single; //������ ��������� ����
    ErrHL: Array [1 .. CHid] of Single; //������ ������� �������� ����
    ErrHL2: Array [1 .. CHid2] of Single; //������ ������� �������� ����

    Procedure Calculate;
    Procedure RandomNN;
    Procedure FindError;
    Procedure SetImg(V: FMnistImg);
    Procedure Learn(H: Single);
    Procedure CleanEtalon;
    Function AvgErr: Single;
    Function GetMaxOut: Integer;
  end;

const  //����� ������� ������
  Names: array [0 .. 9] of string = ('T-shirt/top', 'Trouser', 'Pullover',
    'Dress', 'Coat', 'Sandal', 'Shirt', 'Sneaker', 'Bag', 'Ankle boot');

var
  Form1: TForm1;
  Imgs: array [1 .. 60000] of FMnistImg; //�������� ��� ��������
  Group: Array [1 .. 60000] of byte; //������
  ImgsTst: array [1 .. 10000] of FMnistImg; //�������� ��� ��������
  GroupTst: Array [1 .. 10000] of byte; //������
  NI: LongInt;
  GPos: Integer;
  NN: TNeuralNet;

implementation

{$R *.dfm}

function Sigma(X: Single): Single;//��������
begin
  Result := 1 / (1 + Exp(-X));
end;

function DifSigma(X: Single): Single; //����������� (������������) ��������
begin
  Result := X * (1 - X);
end;

Procedure TNeuralNet.FindError;//���������� ������ �����
var
  i, j: Integer;
  S: Single;
begin
  for i := 1 to COut do //������ ��������� ����
    ErrOut[i] := (Etalon[i] - OutL[i]) * DifSigma(OutL[i]);

  for i := 1 to CHid2 do //������ ������� �������� ����
  begin
    S := 0;
    for j := 1 to COut do
      S := S + C3[i, j] * ErrOut[j];
    ErrHL2[i] := S * DifSigma(HL2[i]);
  end;

  for i := 1 to CHid do //������ ������� �������� ����
  begin
    S := 0;
    for j := 1 to CHid2 do
      S := S + C2[i, j] * ErrHL2[j];
    ErrHL[i] := S * DifSigma(HL[i]);
  end;
end;

Procedure TNeuralNet.Learn(H: Single);//������������� �������������
var
  i, j: Integer;
begin
  for i := 1 to CInp do
    for j := 1 to CHid do
      C1[i, j] := C1[i, j] + H * InL[i] * ErrHL[j];
  for i := 1 to CHid do
    for j := 1 to CHid2 do
      C2[i, j] := C2[i, j] + H * HL[i] * ErrHL2[j];
  for i := 1 to CHid2 do
    for j := 1 to COut do
      C3[i, j] := C3[i, j] + H * HL2[i] * ErrOut[j];
end;

Procedure TNeuralNet.SetImg(V: FMnistImg);
var
  i, j: Integer;
begin
  for i := 1 to 28 do
    for j := 1 to 28 do
      InL[i + (j - 1) * 28] := V[i, j];
  InL[CInp] := 1;
end;

Procedure TNeuralNet.CleanEtalon;
var
  i: Integer;
begin
  for i := 1 to COut do
    Etalon[i] := 0;
end;

Procedure TNeuralNet.RandomNN;
var
  i, j: Integer;
begin
  for j := 1 to CHid do
  begin
    for i := 1 to CInp do
      C1[i, j] := (Random(201) - 100) / 100;
  end;
  for j := 1 to CHid2 do
  begin
    for i := 1 to CHid do
      C2[i, j] := (Random(201) - 100) / 100;
  end;
  for j := 1 to COut do
  begin
    for i := 1 to CHid2 do
      C3[i, j] := (Random(201) - 100) / 100;
  end;
end;

Function TNeuralNet.AvgErr;//������� ������
var
  i: Integer;
  S: Single;
begin
  S := 0;
  for i := 1 to COut do //��� �� ���� ������� ������� ��������������
    S := S + Abs(OutL[i] - Etalon[i]);
  Result := S / COut;
end;

Function TNeuralNet.GetMaxOut: Integer;//������ � ������������ �������
var
  i, N: Integer;
  Max: Single;
begin
  N := 1;
  Max := OutL[1];
  for i := 2 to COut do
    if OutL[i] > Max then //����� ���������, ��������
    begin
      N := i;
      Max := OutL[i];
    end;
  Result := N;
end;

Procedure TNeuralNet.Calculate; //������ ���������
var
  i, j: Integer;
  Sum: Single;
begin
  for j := 1 to CHid do
  begin
    Sum := 0;
    for i := 1 to CInp do
      Sum := Sum + C1[i, j] * InL[i];
    HL[j] := Sigma(Sum);
  end;
  for j := 1 to CHid2 do
  begin
    Sum := 0;
    for i := 1 to CHid do
      Sum := Sum + C2[i, j] * HL[i];
    HL2[j] := Sigma(Sum);
  end;
  for j := 1 to COut do
  begin
    Sum := 0;
    for i := 1 to CHid2 do
      Sum := Sum + C3[i, j] * HL2[i];
    OutL[j] := Sigma(Sum);
  end;
end;

Procedure DrawImg(X, Y: Integer; Img: FMnistImg);
var
  i, j: Integer;
  C: LongInt;
begin
  for j := 1 to 28 do
    for i := 1 to 28 do
    begin
      C := Round($FFFFFF * Img[i, j]);//��������� �������� ������
      Form1.Image1.Picture.Bitmap.Canvas.Pixels[X + i - 1, Y + j - 1] := C;
    end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  A: Single;
begin
  DrawImg(0, 0, ImgsTst[NI]);
  Label1.Caption := Names[GroupTst[NI]];
  NN.SetImg(ImgsTst[NI]);
  NN.Calculate;
  Label2.Caption := Names[NN.GetMaxOut - 1];
  if NN.GetMaxOut - 1 = GroupTst[NI] then
    Label2.Font.Color := clGreen
  else
    Label2.Font.Color := clRed;
  Inc(NI);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i, j: Integer;
  E: Single;
begin
  E := 0;
  for i := 1 to 10 do
    for j := 1 to 60000 do
    begin
      NN.SetImg(Imgs[j]);
      NN.CleanEtalon;
      NN.Etalon[Group[j] + 1] := 1;
      NN.Calculate;
      NN.FindError;
      NN.Learn(0.01);
      E := E + NN.AvgErr;
      if j mod 1000 = 0 then
      begin
        E := E / 1000;
        Inc(GPos);
        Chart1.Series[0].AddXY(GPos, E);
        Chart1.Repaint;
        E := 0;
      end;
    end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i: Integer;
  Right: Integer;
begin
  Right := 0;
  for i := 1 to 10000 do
  begin
    NN.SetImg(ImgsTst[i]);
    NN.Calculate;
    if NN.GetMaxOut - 1 = GroupTst[i] then
      Right := Right + 1;
  end;
  Label3.Caption := 'Acuracy: ' + FormatFloat('0.00', Right / 100) + '%';
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  S: String;
  F: File;
  i, j, k: LongInt;
  B: byte;
  Buf: array [1 .. 28, 1 .. 28] of byte;
begin
  Randomize;
  GPos := 0;
  NN := TNeuralNet.Create;
  NN.RandomNN;

  //����� ../ - ����� �� ���� ����� ����
  S := '../../train-images-idx3-ubyte'; //�������� ��������
  AssignFile(F, S);
  Reset(F, 1);
  for k := 1 to 16 do
    BlockRead(F, B, 1);

  for k := 1 to 60000 do
  begin
    BlockRead(F, Buf, SizeOf(Buf));
    for i := 1 to 28 do
      for j := 1 to 28 do
        Imgs[k][i, j] := Buf[j, i] / 255;
  end;
  CloseFile(F);

  //��, ��,... � ������� ������� DRY
  S := '../../t10k-images-idx3-ubyte';//��� ����� ��� ��������
  AssignFile(F, S);
  Reset(F, 1);
  for k := 1 to 16 do
    BlockRead(F, B, 1);
  for k := 1 to 10000 do
  begin
    BlockRead(F, Buf, SizeOf(Buf));
    for i := 1 to 28 do
      for j := 1 to 28 do
        ImgsTst[k][i, j] := Buf[j, i] / 255;
  end;
  CloseFile(F);
  S := '../../train-labels-idx1-ubyte';//�������� ����� ������� ������
  AssignFile(F, S);
  Reset(F, 1);
  for k := 1 to 8 do
    BlockRead(F, B, 1);
  BlockRead(F, Group, 60000);
  CloseFile(F);
  S := '../../t10k-labels-idx1-ubyte'; //��� ����� ��� ��������
  AssignFile(F, S);
  Reset(F, 1);
  for k := 1 to 8 do
    BlockRead(F, B, 1);
  BlockRead(F, GroupTst, 10000);
  CloseFile(F);
  //���������� ������ ����������� ��� TImage
  Image1.Picture.Bitmap.Width := 28;
  Image1.Picture.Bitmap.Height := 28;
  NI := 1;
end;

end.
