unit UnitMainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Lb.NeuronNet.Neuron,
  Lb.NeuronNet.Files;

type
  TForm5 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo: TMemo;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;
  NeuronNet: TNeuronNet;

implementation

{$R *.dfm}

procedure TForm5.FormCreate(Sender: TObject);
begin
  NeuronNet := TNeuronNet.Create;
end;

procedure TForm5.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(NeuronNet);
end;

procedure TForm5.FormShow(Sender: TObject);
begin
  Memo.Lines.Clear;
  Self.Caption := 'Тестирование нейроной сети';
  Button1Click(nil);
end;

procedure neuroXOR(const A, B: Byte);
var
  xC: Double;
  xIn, xPut: TDoubleList;
begin
  xIn := TDoubleList.Create;
  xPut:= TDoubleList.Create;
  try
    xIn.GetArrayValue([A,B]);
    NeuronNet.Calc(xIn,xPut);
    xC := xPut[0];
  finally
    FreeAndNil(xPut);
    FreeAndNil(xIn);
  end;
  Form5.Memo.Lines.Add('A = ' + A.ToString + ' B = ' + B.ToString + '; Result = ' + xC.ToString + ']');
end;

procedure neuroLearnXOR(const A, B, C: Byte);
var
  xStandard, xIn, xPut: TDoubleList;
begin
  xStandard  := TDoubleList.Create;
  xIn  := TDoubleList.Create;
  xPut := TDoubleList.Create;
  try
    xIn.GetArrayValue([A,B]);
    NeuronNet.Calc(xIn,xPut);
    xStandard.GetArrayValue([C]);
    NeuronNet.CalcLearn(xStandard,xPut,0.1);
  finally
    FreeAndNil(xStandard);
    FreeAndNil(xIn);
    FreeAndNil(xPut);
  end;
end;

function GetValue(const ASide: Integer): Double;
var
  xV: Double;
begin
  Result := 0;
  xV := Random(5)/10;
  case ASide of
    0: Result := xV;
    1: Result := 1 - xV;
  end;
end;

procedure TForm5.Button1Click(Sender: TObject);
begin
  // Создание нейроной сети
  Memo.Lines.Add('Создание нейроно сети');
  NeuronNet.Clear;
  NeuronNet.AddLayer(2);
  NeuronNet.AddLayer(2);
  NeuronNet.OutputLayer(1);
end;

procedure TForm5.Button2Click(Sender: TObject);
begin
  Memo.Lines.Add('Результат нейроной сети');
  neuroXOR(0,0);
  neuroXOR(0,1);
  neuroXOR(1,0);
  neuroXOR(1,1);
end;

procedure TForm5.Button3Click(Sender: TObject);
const
  COUNT_NEURON = 1000;
begin
  Memo.Lines.Add('Обучение нейроной сети: количество: ' + COUNT_NEURON.ToString);
  for var i := 0 to COUNT_NEURON - 1 do
  begin
    neuroLearnXOR(0,0,0);
    neuroLearnXOR(0,1,1);
    neuroLearnXOR(1,0,1);
    neuroLearnXOR(1,1,0);
  end;
  neuroXOR(0,0);
  neuroXOR(0,1);
  neuroXOR(1,0);
  neuroXOR(1,1);
end;

procedure TForm5.Button4Click(Sender: TObject);
var
  xParam: TParamNeuronNet;
begin
  xParam := TParamNeuronNet.Create(NeuronNet);
  try
    xParam.Save('_neuron.txt');
  finally
    FreeAndNil(xParam);
  end;
end;

procedure TForm5.Button5Click(Sender: TObject);
var
  xParam: TParamNeuronNet;
begin
  xParam := TParamNeuronNet.Create(NeuronNet);
  try
    xParam.Load('neuron.txt');
  finally
    FreeAndNil(xParam);
  end;
end;

end.
