unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  VclTee.TeeGDIPlus, VclTee.TeEngine, VclTee.Series, VclTee.TeeProcs,
  VclTee.Chart, Data.DB,
  Lb.NeuronNet.Neuron,
  Lb.Source.DB;

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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    Index: Integer;
  public
    t10_SourceLabels: TSourceDataSetLabels;
    t10_SourceImages: TSourceDataSetImages;
    train_SourceLabels: TSourceDataSetLabels;
    train_SourceImages: TSourceDataSetImages;
  end;

const  //размеры нейросети
  CInp  = 28 * 28;
  CHid  = 20;
  CHid2 = 20;
  COut  = 10;

//type
//  FMnistImg = array [1 .. 28, 1 .. 28] of Single;
//
//  TNeuralNet = class
//    InL: Array [1 .. CInp] of Single; //входной слой
//    HL: Array [1 .. CHid] of Single; //первый скрытый слой
//    HL2: Array [1 .. CHid2] of Single; //второй скрытый слой
//    OutL: Array [1 .. COut] of Single; //выходной слой
//
//    C1: Array [1 .. CInp, 1 .. CHid] of Single; // связи 1-2 слоя
//    C2: Array [1 .. CHid, 1 .. CHid2] of Single; // связи 2-3 слоя
//    C3: Array [1 .. CHid2, 1 .. COut] of Single; // связи 3-4 слоя
//
//    Etalon: Array [1 .. COut] of Single; // правильный ответ (эталон)
//
//    ErrOut: Array [1 .. COut] of Single; //ошибка выходного слоя
//    ErrHL: Array [1 .. CHid] of Single; //ошибка первого скрытого слоя
//    ErrHL2: Array [1 .. CHid2] of Single; //ошибка второго скрытого слоя
//
//    Procedure Calculate;
//    Procedure RandomNN;
//    Procedure FindError;
//    Procedure SetImg(V: TMaskImg);
//    Procedure Learn(H: Single);
//    Procedure CleanEtalon;
//    Function AvgErr: Single;
//    Function GetMaxOut: Integer;
//  end;

const  //имена классов одежды
  Names: array [0 .. 9] of string = ('T-shirt/top', 'Trouser', 'Pullover',
    'Dress', 'Coat', 'Sandal', 'Shirt', 'Sneaker', 'Bag', 'Ankle boot');

var
  Form1: TForm1;
  NeuronNet: TNeuronNet = nil;

implementation

{$R *.dfm}

procedure SourceImageToValues(const ASourceImage: TSourceImage; AValues: TDoubleList);
begin
  AValues.Clear;
  for var xB in ASourceImage.Bytes do
    AValues.Add(xB/255);
end;

function GetIndexToValues(const AValues: TDoubleList): Integer;
var
  xInd: Integer;
  xValueMax: Double;
  i, iCount: Integer;
begin
  xInd := -1;
  xValueMax := 0;
  iCount := AValues.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      if xValueMax < AValues[i] then
      begin
        xValueMax := AValues[i];
        xInd := i;
      end;
    end;
  Result := xInd;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  xIndexSource: Integer;
  xSourceImage: TSourceImage;
  xIndex, xIndexOut: Integer;
var
  xInValues, xOutValues: TDoubleList;
begin
  xInValues  := TDoubleList.Create;
  xOutValues := TDoubleList.Create;
  try
    xIndex := Random(train_SourceImages.Count);

    xSourceImage := train_SourceImages.SourceImages[xIndex];
    SourceImageToBitmap(
      xSourceImage,
      Image1.Picture.Bitmap
    );

    xIndexSource := train_SourceLabels.Bytes[xIndex];
    Label1.Caption := Names[xIndexSource];

    SourceImageToValues(xSourceImage,xInValues);
    NeuronNet.Calc(xInValues,xOutValues);

    xIndexOut := GetIndexToValues(xOutValues);
    Label2.Caption := Names[xIndexOut];

    if xIndexOut = xIndexSource then
      Label2.Font.Color := clGreen
    else
      Label2.Font.Color := clRed;

  finally
    FreeAndNil(xOutValues);
    FreeAndNil(xInValues);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);

  procedure _SetStandard(AStandard: TDoubleList; AIndexSource: Integer);
  var
    i: Integer;
  begin
    AStandard.Clear;
    for i := 0 to 9 do
    begin
      if i = AIndexSource then
        AStandard.Add(AIndexSource)
      else
        AStandard.Add(0);
    end;
  end;

var
  i, j: Integer;
  xIndexSource: Integer;
  xSourceImage: TSourceImage;
var
  xInValues, xOutValues, xStandard: TDoubleList;
begin
  xInValues  := TDoubleList.Create;
  xOutValues := TDoubleList.Create;
  xStandard  := TDoubleList.Create;
  try

    for i := 1 to 10 do
      for j := 0 to train_SourceImages.Count - 1 do
      begin
        xSourceImage := train_SourceImages.SourceImages[j];
        xIndexSource := train_SourceLabels.Bytes[j];
        SourceImageToValues(xSourceImage,xInValues);

        _SetStandard(xStandard,xIndexSource);

        NeuronNet.Calc(xInValues,xOutValues);
        NeuronNet.CalcLearn(xStandard,xOutValues,0.01);
      end;
  finally
    FreeAndNil(xStandard);
    FreeAndNil(xOutValues);
    FreeAndNil(xInValues);
  end;

end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i: Integer;
  Right: Integer;
var
  xIndexSource: Integer;
  xSourceImage: TSourceImage;
var
  xIndexOut: Integer;
  xInValues, xOutValues: TDoubleList;
begin
  xInValues := TDoubleList.Create;
  xOutValues:= TDoubleList.Create;
  try
    Right := 0;
    for i := 0 to t10_SourceImages.Count - 1 do
    begin
      xSourceImage := t10_SourceImages.SourceImages[i];
      xIndexSource := t10_SourceLabels.Bytes[i];

      SourceImageToValues(xSourceImage,xInValues);
      NeuronNet.Calc(xInValues,xOutValues);


      xIndexOut := GetIndexToValues(xOutValues);
      if xIndexOut = xIndexSource then
        Label2.Font.Color := clGreen
      else
        Label2.Font.Color := clRed;

      if xIndexOut = xIndexSource then
        Right := Right + 1;
    end;
    Label3.Caption := 'Acuracy: ' + FormatFloat('0.00', Right / 100) + '%';
  finally
    FreeAndNil(xOutValues);
    FreeAndNil(xInValues);
  end;



end;


procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(NeuronNet);
  FreeAndNil(t10_SourceLabels);
  FreeAndNil(t10_SourceImages);
  FreeAndNil(train_SourceLabels);
  FreeAndNil(train_SourceImages);
end;

procedure TForm1.FormCreate(Sender: TObject);

  procedure _LoadCustomSourceDataSet(const AFileName: String; ASourceDataSet: TCustomSourceDataSet);
  var
    xStream: TFileStream;
  begin
    xStream := TFileStream.Create(AFileName,fmOpenRead);
    try
      ASourceDataSet.Sources := xStream;
    finally
      FreeAndNil(xStream);
    end;
  end;

begin
  Index := 0;

  t10_SourceLabels   := TSourceDataSetLabels.Create;
  t10_SourceImages   := TSourceDataSetImages.Create;
  train_SourceLabels := TSourceDataSetLabels.Create;
  train_SourceImages := TSourceDataSetImages.Create;

  _LoadCustomSourceDataSet('../../t10k-labels-idx1-ubyte',t10_SourceLabels);
  _LoadCustomSourceDataSet('../../t10k-images-idx3-ubyte',t10_SourceImages);
  _LoadCustomSourceDataSet('../../train-labels-idx1-ubyte',train_SourceLabels);
  _LoadCustomSourceDataSet('../../train-images-idx3-ubyte',train_SourceImages);

  Randomize;

  NeuronNet := TNeuronNet.Create;
  NeuronNet.AddLayer(CInp);
  NeuronNet.AddLayer(CHid);
  NeuronNet.AddLayer(CHid2);
  NeuronNet.OutputLayer(COut);

  Image1.Picture.Bitmap.Width := 28;
  Image1.Picture.Bitmap.Height := 28;
end;


end.
