unit UnitMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, VCLTee.TeEngine,
  VCLTee.Series, Vcl.ExtCtrls, VCLTee.TeeProcs, VCLTee.Chart, Vcl.StdCtrls,
  Vcl.ComCtrls;

type
  TMainForm = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    Button2: TButton;
    Chart1: TChart;
    Series1: TLineSeries;
    Button3: TButton;
    ProgressBar1: TProgressBar;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Lb.NeuronNet;

type
  TMnistImg = array [1 .. 28, 1 .. 28] of Double;

const  //имена классов одежды
  Names: array [0 .. 9] of string =
    ('T-shirt/top', 'Trouser', 'Pullover', 'Dress', 'Coat', 'Sandal', 'Shirt',
    'Sneaker', 'Bag', 'Ankle boot');


type
  TNeuronNetTest = class(TNeuronNet)
  public
  end;


var
  NeuronNet: TNeuronNetTest = nil;

  Imgs    : array [1 .. 60000] of TMnistImg; // картинки для обучения
  Group   : array [1 .. 60000] of byte;      // ответы
  ImgsTst : array [1 .. 10000] of TMnistImg; // картинки для проверки
  GroupTst: array [1 .. 10000] of byte;      // ответы
  NI      : Integer = 1;

procedure LoadData;
var
  S: String;
  F: File;
  i, j, k: LongInt;
  B: byte;
  Buf: array [1 .. 28, 1 .. 28] of byte;
begin
  // ***************************************************************************
  //здесь ../ - выход на одну папку выше
  S := 'train-images-idx3-ubyte'; //загрузка картинок
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

  // ***************************************************************************
  //да, да,... я нарушил принцип DRY
  S := 't10k-images-idx3-ubyte';//это набор для проверки
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

  // ***************************************************************************
  S := 'train-labels-idx1-ubyte';//загрузка кодов классов одежды
  AssignFile(F, S);
  Reset(F, 1);
  for k := 1 to 8 do
    BlockRead(F, B, 1);
  BlockRead(F, Group, 60000);
  CloseFile(F);

  // ***************************************************************************
  S := 't10k-labels-idx1-ubyte'; //это набор для проверки
  AssignFile(F, S);
  Reset(F, 1);
  for k := 1 to 8 do
    BlockRead(F, B, 1);
  BlockRead(F, GroupTst, 10000);
  CloseFile(F);

end;


procedure TMainForm.FormCreate(Sender: TObject);
var
  xInputCount: Integer;
begin
  NeuronNet := TNeuronNetTest.Create;
  xInputCount := 28 * 28 + 1;
  NeuronNet.CompileNetWork([xInputCount,20,20,10]);

  LoadData;
  NI := 1;
  // ***************************************************************************
  //выставляет размер изображения для TImage
  Image1.Picture.Bitmap.Width := 28;
  Image1.Picture.Bitmap.Height := 28;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(NeuronNet);
end;

procedure DrawImg(X, Y: Integer; Img: TMnistImg);
var
  i, j: Integer;
  C: LongInt;
begin
  for j := 1 to 28 do
    for i := 1 to 28 do
    begin
      C := Round($FFFFFF * Img[i, j]);//получение градации серого
      MainForm.Image1.Picture.Bitmap.Canvas.Pixels[X + i - 1, Y + j - 1] := C;
    end;
end;

procedure TNeuralNet_SetImg(V: TMnistImg);
var
  xInd: Integer;
  i, j: Integer;
begin
  xInd := 0;
  for i := 1 to 28 do
    for j := 1 to 28 do
    begin
      NeuronNet.InputNeurons.Values[xInd] := V[i, j];
      Inc(xInd);
    end;

  NeuronNet.InputNeurons.Values[xInd] := 1;
  NeuronNet.Calculate;
end;

function OutputIndexNeuron: Integer;
var
  xV: Double;
  i, N: Integer;
  Max: Single;
begin
  N := 0;
  Max := 0.0;
  for i := 0 to NeuronNet.OutputNeurons.Count - 1 do
  begin
    xV := NeuronNet.OutputNeurons.Values[i];
    if xV > Max then //поиск максимума, классика
    begin
      N := i;
      Max := xV;
    end;
  end;
  Result := N;
end;


Function AvgErr: Double;//средняя ошибка
var
  i: Integer;
  xV, xE: Double;
  S: Single;
begin
  S := 0;
  for i := 0 to NeuronNet.OutputNeurons.Count - 1 do //тут по сути простое среднее арифметическое
  begin
    xV := NeuronNet.OutputNeurons.Values[i];
    xE := NeuronNet.OutputNeurons.Etalons[i];
    S := S + Abs(xV - xE);
  end;
  Result := S / NeuronNet.OutputNeurons.Count;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  xInd: Integer;
begin
  DrawImg(0, 0, ImgsTst[NI]);
  Label1.Caption := Names[GroupTst[NI]];
  TNeuralNet_SetImg(ImgsTst[NI]);

  xInd := OutputIndexNeuron;
  if xInd >= 0 then
  begin
    Label2.Caption := Names[xInd];
  end
  else
    Label2.Caption := 'error';


  if xInd = GroupTst[NI] then
    Label2.Font.Color := clGreen
  else
    Label2.Font.Color := clRed;
  Inc(NI);
end;

var
  GPos: Integer = 0;

procedure TMainForm.Button2Click(Sender: TObject);

  procedure _EtalonL(V: Integer);
  begin
    NeuronNet.OutputNeurons.EtalonNullValue;
    NeuronNet.OutputNeurons.Etalons[V] := 1;
  end;

var
  i, j: Integer;
  xP: Integer;
begin
  // Обучение
  i := 1;
  xP := 0;
  for i := 1 to 10 do
  begin
    for j := 1 to 60000 do
    begin
      TNeuralNet_SetImg(Imgs[j]);
      _EtalonL(Group[j]);

      NeuronNet.CalculateError;
      NeuronNet.CalculateLearn(0.01);

//      E := E + NeuronNet.AverageErrorValue;
//      if j mod 1000 = 0 then
//      begin
//        E := E / 1000;
//        Inc(GPos);
//        Chart1.Series[0].AddXY(GPos, E);
//        //Chart1.Repaint;
//        E := 0;
//      end;

      xP := xP + 1;
      ProgressBar1.Position := xP;
      Label4.Caption := 'i := ' + i.ToString + '; j := ' + j.ToString + '; xP := ' + xP.ToString;
      Application.ProcessMessages;
    end;
  end;
  ShowMessage('Готова');
end;

procedure TMainForm.Button3Click(Sender: TObject);
var
  i: Integer;
  Right: Integer;
begin
  Right := 0;
  for i := 1 to 10000 do
  begin
    TNeuralNet_SetImg(Imgs[i]);
    NeuronNet.Calculate;
    if OutputIndexNeuron = GroupTst[i] then
      Right := Right + 1;
  end;
  Label3.Caption := 'Acuracy: ' + FormatFloat('0.00', Right / 100) + '%';
end;

end.
