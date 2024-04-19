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
  Lb.NeuronNet.Neuron;

type
  TForm5 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo: TMemo;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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

procedure TForm5.Button1Click(Sender: TObject);

  procedure _LayerToStrings(const ALayer: TLayer; const ASources: TStrings);
  var
    i, iCount: Integer;
    xNeuron: TNeuron;
  begin
    ASources.Clear;
    iCount := ALayer.Neurons.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xNeuron := ALayer.Neurons[i];
        ASources.Add(Format('N[%d] = [%d]',
        [
          xNeuron.Links.Count,
          xNeuron.OutputLinks.Count
        ]))
      end;
  end;

begin
  //
  NeuronNet.AddLayer(2);
  NeuronNet.AddLayer(5);
  NeuronNet.AddLayer(3);
  NeuronNet.OutputLayer(1);
end;

procedure neuroXOR(const A, B: Byte);
var
  xV: Double;
  xIn, xPut: TDoubleList;
begin
  xIn := TDoubleList.Create;
  xPut:= TDoubleList.Create;
  try
    xIn.GetArrayValue([A,B]);
    NeuronNet.Calc(xIn,xPut);
    xV := xPut[0];
  finally
    FreeAndNil(xPut);
    FreeAndNil(xIn);
  end;
  Form5.Memo.Lines.Add('A = ' + A.ToString + '; B = ' + B.ToString + '; Result = ' + xV.ToString);
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

procedure TForm5.Button2Click(Sender: TObject);
begin
  neuroXOR(0,0);
  neuroXOR(0,1);
  neuroXOR(1,0);
  neuroXOR(1,1);
end;

procedure TForm5.Button3Click(Sender: TObject);
begin
  Memo.Lines.Add('***********************************');
  for var i := 0 to 9000 do
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

end.
