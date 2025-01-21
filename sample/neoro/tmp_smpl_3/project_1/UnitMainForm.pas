unit UnitMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Lb.Neoro.SysUtils, Vcl.Grids, Vcl.ExtCtrls;

type
  TForm4 = class(TForm)
    Button1: TButton;
    Memo: TMemo;
    Button2: TButton;
    StrGridN1: TStringGrid;
    StrGridW1: TStringGrid;
    StrGridN2: TStringGrid;
    StrGridW2: TStringGrid;
    StrGridN3: TStringGrid;
    Button3: TButton;
    Button4: TButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure SetShowGrid;
  public
    NeuronNet: TNeuronNet;
    function NeuronNetXOR(const AValueA, AValueB: Integer): Double;
    procedure LearnNeuronNetXOR(const AValueA, AValueB, AValueC: Integer);
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.FormCreate(Sender: TObject);
begin
  NeuronNet := TNeuronNet.Create;
  NeuronNet.CreatingNeuralNetwork([2,3,1]);
end;

procedure TForm4.FormShow(Sender: TObject);
begin
  SetShowGrid;
end;

procedure TForm4.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(NeuronNet);
end;

function TForm4.NeuronNetXOR(const AValueA, AValueB: Integer): Double;
begin
  with NeuronNet.InputNeurons do
  begin
    Values[0] := AValueA;
    Values[1] := AValueB;
  end;
  NeuronNet.Calculate;
  Result := NeuronNet.OutputNeurons.Values[0];
end;

procedure TForm4.LearnNeuronNetXOR(const AValueA, AValueB, AValueC: Integer);
begin
  with NeuronNet.InputNeurons do
  begin
    Values[0] := AValueA;
    Values[1] := AValueB;
  end;
  NeuronNet.Calculate;
  NeuronNet.CalculateError([AValueC]);
  NeuronNet.CalculateLearn(0.1);
end;

procedure TForm4.SetShowGrid;

  procedure _GridNeuronValue(AStrGrid: TStringGrid; ANeuron: TNeuronsLayer);
  begin
    AStrGrid.RowCount := ANeuron.Count + 1;
    for var i := 0 to ANeuron.Count - 1 do
      AStrGrid.Cells[0,i + 1] := ANeuron.Values[i].ToString;
  end;

  procedure _GridWeightValue(AStrGrid: TStringGrid; AWeight: TWeightsLayer);
  begin
    AStrGrid.RowCount := AWeight.RowCount + 1;
    AStrGrid.ColCount := AWeight.ColCount;

    for var c := 0 to AWeight.ColCount - 1 do
      for var r := 0 to AWeight.RowCount - 1 do
        AStrGrid.Cells[c,r + 1] := AWeight.Values[c,r].ToString;
  end;

begin
  _GridNeuronValue(StrGridN1,NeuronNet.NeuronsLayers[0]);
  _GridWeightValue(StrGridW1,NeuronNet.WeightsLayers[0]);
  _GridNeuronValue(StrGridN2,NeuronNet.NeuronsLayers[1]);
  _GridWeightValue(StrGridW2,NeuronNet.WeightsLayers[1]);
  _GridNeuronValue(StrGridN3,NeuronNet.NeuronsLayers[2]);
end;



procedure TForm4.Button1Click(Sender: TObject);
begin
  with Memo.Lines do
  begin
    Add('---------------------------------------');
    Add('0 | 0 | = ' + NeuronNetXOR(0,0).ToString);
    Add('0 | 1 | = ' + NeuronNetXOR(0,1).ToString);
    Add('1 | 0 | = ' + NeuronNetXOR(1,0).ToString);
    Add('1 | 1 | = ' + NeuronNetXOR(1,1).ToString);
  end;
end;

procedure TForm4.Button2Click(Sender: TObject);
begin
  for var i := 0 to 100000 do
  begin
    LearnNeuronNetXOR(0,0,0);
    LearnNeuronNetXOR(0,1,1);
    LearnNeuronNetXOR(1,0,1);
    LearnNeuronNetXOR(1,1,0);
  end;
  Button1Click(nil);
  SetShowGrid;
end;

procedure TForm4.Button3Click(Sender: TObject);
begin
  NeuronNetXOR(0,1);
  SetShowGrid;
end;

procedure TForm4.Button4Click(Sender: TObject);
begin
  Timer1.Enabled := not Timer1.Enabled;
end;

procedure TForm4.Timer1Timer(Sender: TObject);
begin
  LearnNeuronNetXOR(0,1,1);
  SetShowGrid;
end;

end.
