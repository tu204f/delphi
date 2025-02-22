program nr_xor;

uses
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {Form5},
  Lb.NeuronNet.Neuron in '..\lib\Lb.NeuronNet.Neuron.pas',
  Lb.NeuronNet.Files in '..\lib\Lb.NeuronNet.Files.pas',
  Lb.NeuronNet.Neuron.V2 in '..\lib\Lb.NeuronNet.Neuron.V2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
