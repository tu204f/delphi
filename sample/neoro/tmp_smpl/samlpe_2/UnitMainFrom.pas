unit UnitMainFrom;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm4 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo: TMemo;
    Button3: TButton;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    procedure Log(S: String = '');
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

uses
  Lb.ActivationFunction,
  Lb.NeuronNet;

var
  NeuronNet: TNeuronNet = nil;


procedure _neuroXOR(ANeuronNet: TNeuronNet; const A, B: Byte);
var
  xV: Double;
begin
  ANeuronNet.InputNeurons.Values[0] := A;
  ANeuronNet.InputNeurons.Values[1] := B;
  ANeuronNet.InputNeurons.Values[2] := 1;
  ANeuronNet.Calculate;
  xV := ANeuronNet.OutputNeurons.Values[0];
  Form4.Log('A = ' + A.ToString + '; B = ' + B.ToString + '; Result = ' + xV.ToString);
end;

procedure neuroXOR(const A, B: Byte);
var
  xV: Double;
begin
  NeuronNet.InputNeurons.Values[0] := A;
  NeuronNet.InputNeurons.Values[1] := B;
  NeuronNet.InputNeurons.Values[2] := 1;
  NeuronNet.Calculate;
  xV := NeuronNet.OutputNeurons.Values[0];
  Form4.Log('A = ' + A.ToString + '; B = ' + B.ToString + '; Result = ' + xV.ToString);
end;

procedure neuroLearnXOR(const A, B, C: Byte);
begin
  // Обратная проходка
  NeuronNet.InputNeurons.Values[0] := A;
  NeuronNet.InputNeurons.Values[1] := B;
  NeuronNet.InputNeurons.Values[2] := 1;
  NeuronNet.Calculate;
  NeuronNet.OutputNeurons.Values[0];

  // Начало объекта
  NeuronNet.OutputNeurons.EtalonNullValue;
  NeuronNet.OutputNeurons.Etalons[0] := C;
  NeuronNet.CalculateError;
  NeuronNet.CalculateLearn(0.05);
end;

{ TForm4 }

procedure TForm4.FormCreate(Sender: TObject);
begin
  NeuronNet := TNeuronNet.Create;
  NeuronNet.TypeFuction := TTypeFuction.tfSigma;
  NeuronNet.CompileNetWork([3,3,1]);
end;

procedure TForm4.Log(S: String);
begin
  Memo.Lines.Add(S);
end;

procedure TForm4.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(NeuronNet);
end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  neuroXOR(0,0);
  neuroXOR(0,1);
  neuroXOR(1,0);
  neuroXOR(1,1);
end;

procedure TForm4.Button2Click(Sender: TObject);
begin
  Log('*************************************************');
  for var i := 0 to 100000 do
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

procedure TForm4.Button3Click(Sender: TObject);
begin
  NeuronNet.Save('xor.neuron');
end;

procedure TForm4.Button4Click(Sender: TObject);
begin
  NeuronNet.Load('xor.neuron');
end;



end.
